/* continuations.c -- continuations, much stack hackery..
   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* notes:

   The basic idea is to copy the entire active stack into the
   continuation, together with a jmpbuf and the pointers into the stack
   stored lisp histories (lisp call stack, gc roots, blocked file
   operations, saved regexp data, etc..)

   When the continuation is activated, the stack is built up so that
   it's large enough to contain the saved stack in the continuation.
   The saved version is then copied over the current stack, and the
   jmpbuf is called

   Marking a continuation involves marking all the lisp histories, but
   remembering to relocate into the copied stack data

   Some of the ideas here were inspired by the SCM/Guile implementation
   of continuations.

   We also use continuation `barriers'. A barrier marks a (possibly
   saved) stack position, and can be either `open' or `closed'. There
   is a tree of barriers, branches of which may be stored in
   continuations, or on the current stack.

   When invoking a continuation it is forbidden to cross any closed
   barriers. Each barrier has two functions `in' and `out' associated
   with it, one of these may be invoked when a continuation is invoked
   and the barrier is crossed. These functions are normally used for
   setting and unsetting global state.

   Note that continuations only save and restore variable bindings
   (both lexical and dynamic). It doesn't make sense to save other
   dynamic state (i.e. catch/throw, unwind-protect, etc..), though it
   could be done using open barriers..

   Hopefully this will be reasonably portable, I _think_ it only
   depends on having a linear stack that completely encapsulates the
   current process state, and a setjmp/longjmp implementation..

   Continuations are also used to provide a basic threading
   implementation. Threads are local to each enclosing closed barrier
   (dynamic root). Each barrier has two thread queues, runnable and
   suspended. Each thread is just a (primitive) continuation, the
   lexical environment, and a forbid-preemption count. The dynamic root
   acts as a serialization point, it will only be crossed when the last
   thread has exited or been deleted.

   To avoid having to consider preemption throughout the interpreter,
   there are only (currently) two preemption points, in funcall and the
   bytecode interpreter. The rep_test_int_counter is used to decide
   when to try to preempt the current thread. In non-threaded mode
   (i.e. thread_invoke () hasn't been called in the current root),
   these are all no-ops. The rep_TEST_INT_SLOW macro is also allowed to
   preempt.

   Finally, here's an example of using threads:

   (defvar *counter* nil)

   (defun thread-fun (id)
     (let
	 ((*counter* (* id 1000)))
       (while t
	 (format standard-output "thread-%s: %8d\n" id *counter*)
	 (setq *counter* (1+ *counter*)))))

   (setq thread-1 (make-thread (lambda () (thread-fun 1)) "thread-1"))
   (setq thread-2 (make-thread (lambda () (thread-fun 2)) "thread-2"))

   [ the dynamic root is a serialization point, it won't be exited
   until _all_ threads it contains have exited / been deleted, or it's
   been thrown threw (which deletes all running threads)  ]

   The lisp debugger runs in it's own dynamic root, so debugging
   threads works for free!  */

#define _GNU_SOURCE
#undef DEBUG

/* AIX requires this to be the first thing in the file.  */
#include <config.h>
#ifdef __GNUC__
# define alloca __builtin_alloca
#else
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include "repint.h"
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <setjmp.h>
#include <limits.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#if defined (DEBUG)
# define DB(x) printf x
#else
# define DB(x)
#endif

/* Threads only preempted when this is zero. */
int rep_thread_lock = 0;

/* True when the current thread should be preempted soon */
rep_bool rep_pending_thread_yield;

#ifdef WITH_CONTINUATIONS

#if STACK_DIRECTION == 0
# error "stack growth direction unknown"
#elif STACK_DIRECTION > 0
# warning "upward growing stacks are untested"
#endif

#if STACK_DIRECTION < 0
  /* was address B1 put on the stack _before_ address B2? */
# define SP_OLDER_P(b1, b2) ((b1) > (b2))
  /* was address B1 put on the stack _after_ address B2? */
# define SP_NEWER_P(b1, b2) ((b1) < (b2))
#else
# define SP_OLDER_P(b1, b2) ((b1) < (b2))
# define SP_NEWER_P(b1, b2) ((b1) > (b2))
#endif

/* copied from guile 1.3.2 */
#if !defined (FLUSH_REGISTER_WINDOWS)
# if defined (sparc)
#  define FLUSH_REGISTER_WINDOWS asm ("ta 3")
# else
#  define FLUSH_REGISTER_WINDOWS
# endif
#endif

typedef struct rep_barrier_struct rep_barrier;
typedef struct rep_continuation_struct rep_continuation;
typedef struct rep_thread_struct rep_thread;

/* Continuations can only be invoked if there's no closed barriers
   between the current stack address and the address contained in the
   continuation. Open barriers are simply used for context switching
   globally-stored state

   Barriers also allow us to be selective about how much of the stack
   is saved for each continuation. Only the portion more recent than
   the most recent closed barrier is saved. */

struct rep_barrier_struct {
    rep_barrier *next;
    rep_barrier *root;		/* upwards closed barrier */
    char *point;
    void (*in)(void *data);
    void (*out)(void *data);
    void *data;
    rep_thread *active;
    rep_thread *head, *tail;
    rep_thread *susp_head, *susp_tail;
    short depth;
    unsigned int closed : 1;
    unsigned int targeted : 1;		/* may contain continuations */
};

/* List of all currently active barriers (on the current stack) */
static rep_barrier *barriers;

/* The outermost active closed barrier (the dynamic root in guile terms?) */
static rep_barrier *root_barrier;

/* Put in rep_throw_value when the enclosing closed barrier needs to exit */
static repv exit_barrier_cell;

/* The data saved for a continuation */
struct rep_continuation_struct {
    repv car;
    rep_continuation *next;

    jmp_buf jmpbuf;
    char *stack_copy, *stack_top, *stack_bottom;
    size_t stack_size, real_size;

    rep_barrier *barriers;
    rep_barrier *root;

    struct rep_Call *call_stack;
    repv special_bindings;
    rep_GC_root *gc_roots;
    rep_GC_n_roots *gc_n_roots;
    struct rep_saved_regexp_data *regexp_data;
    struct blocked_op *blocked_ops[op_MAX];
    repv throw_value;
    rep_bool single_step;
    int lisp_depth;
};

#define rep_CONTIN(v)	((rep_continuation *)rep_PTR(v))
#define rep_CONTINP(v)	rep_CELL16_TYPEP(v, continuation_type ())

#define CF_INVALID	(1 << rep_CELL16_TYPE_BITS)

#define CONTIN_MAX_SLOP 4096

/* returns the cell16 typecode allocated for continuation objects */
static int continuation_type (void);

/* list of all allocated continuations */
static rep_continuation *continuations;

struct rep_thread_struct {
    repv car;
    rep_thread *next_alloc;
    rep_thread *next, *pred;
    repv name;
    rep_continuation *cont;
    repv env, structure;
    int lock;
    struct timeval run_at;
    rep_bool (*poll)(rep_thread *t, void *arg);
    void *poll_arg;
    repv exit_val;
};

#define XTHREADP(v)	rep_CELL16_TYPEP(v, thread_type ())
#define THREADP(v)	(XTHREADP (v) && !(THREAD (v)->car & TF_EXITED))
#define THREAD(v)	((rep_thread *) rep_PTR (v))

#define TF_EXITED	(1 << (rep_CELL16_TYPE_BITS + 0))
#define TF_SUSPENDED	(1 << (rep_CELL16_TYPE_BITS + 1))

static int thread_type (void);
static rep_thread *threads;

#define TV_LATER_P(t1, t2)			\
    (((t1)->tv_sec > (t2)->tv_sec)		\
     || (((t1)->tv_sec == (t2)->tv_sec)		\
	 && ((t1)->tv_usec > (t2)->tv_usec)))

DEFSYM(continuation, "continuation");

/* used while longjmp'ing to save accessing a local variable */
static rep_continuation *invoked_continuation;
static repv invoked_continuation_ret;
static rep_barrier *invoked_continuation_ancestor;

/* Approx. number of extra bytes of stack per recursion */
#define STACK_GROWTH 512

static inline char *
fixup (char *addr, rep_continuation *c)
{
#if STACK_DIRECTION < 0
    if (addr < c->stack_bottom)
	return (addr - c->stack_top) + c->stack_copy;
    else
	return addr;
#else
    if (addr > c->stack_bottom)
	return (addr - c->stack_bottom) + c->stack_copy;
    else
	return addr;
#endif
}

#define FIXUP(t,c,addr) ((t) (fixup ((char *) (addr), (c))))

static void thread_delete (rep_thread *t);


/* barriers */

/* Create a barrier (closed if CLOSED is true, open otherwise), then
   call CALLBACK with ARG as its argument. The barrier will be in place
   for the duration of the call to CALLBACK.

   If either of IN or OUT aren't null pointers then they will be called
   when the barrier is crossed (while invoking a continuation). Closed
   barriers are never crossed. DATA is passed to both IN and OUT
   functions when they are called.

   The IN function is called when control passes from above barrier on
   the stack to below; OUT when control passes from below to above. */
repv
rep_call_with_barrier (repv (*callback)(repv), repv arg,
		       rep_bool closed, void (*in)(void *),
		       void (*out)(void *), void *data)
{
    repv ret;
    rep_barrier b;

    memset (&b, 0, sizeof (b));
    b.point = (char *) &b;
#if STACK_DIRECTION > 0
    b.point += sizeof (rep_barrier);	/* don't want to save barrier */
#endif
    b.root = root_barrier;
    b.in = in;
    b.out = out;
    b.data = data;
    b.closed = closed;
    b.depth = barriers ? barriers->depth + 1 : 1;

    b.next = barriers;
    barriers = &b;

    if (closed)
	root_barrier = &b;

    DB(("with-barrier[%s]: in  %p (%d)\n",
	closed ? "closed" : "open", &b, b.depth));

    ret = callback (arg);

    if (closed)
    {
	rep_thread *ptr;

    again:
	if (rep_throw_value == exit_barrier_cell)
	{
	    DB (("caught barrier exit throw\n"));
	    rep_throw_value = rep_CDR (exit_barrier_cell);
	    ret = (rep_throw_value == rep_NULL) ? Qnil : rep_NULL;
	    rep_CDR (exit_barrier_cell) = Qnil;
	}

	if (rep_throw_value == rep_NULL && b.active != 0)
	{
	    /* An active thread exited. Calling thread_delete () on the
	       active thread will call thread_invoke (). That will
	       exit if there are no more runnable threads. */
	    DB (("deleting active thread %p\n", b.active));
	    thread_delete (b.active);
	    goto again;
	}

	if (b.targeted)
	{
	    /* Invalidate any continuations that require this barrier */
	    rep_continuation *c;
	    for (c = continuations; c != 0; c = c->next)
	    {
		if (c->root == &b)
		    c->car |= CF_INVALID;
	    }
	}

	for (ptr = b.head; ptr != 0; ptr = ptr->next)
	    ptr->car |= TF_EXITED;
	for (ptr = b.susp_head; ptr != 0; ptr = ptr->next)
	    ptr->car |= TF_EXITED;
	if (b.active != 0)
	    b.active->car |= TF_EXITED;
    }

    DB(("with-barrier[%s]: out %p (%d)\n",
	closed ? "closed" : "open", &b, b.depth));

    barriers = b.next;
    root_barrier = b.root;
    return ret;
}

static rep_barrier *
get_dynamic_root (int depth)
{
    rep_barrier *root = root_barrier;
    while (depth-- > 0 && root != 0)
	root = root->root;
    return root;
}

/* Record all barriers from continuation C's outermost barrier into the
   array HIST, stopping at the first closed barrier encountered.
   Returns the total number of barrier placed in HIST. */
static int
trace_barriers (rep_continuation *c, rep_barrier **hist)
{
    int i;
    rep_barrier *ptr = FIXUP (rep_barrier *, c, c->barriers);
    for (i = 0; ptr != 0; ptr = FIXUP (rep_barrier *, c, ptr->next))
    {
	hist[i++] = ptr;
	if (ptr->closed)
	    break;
    }
    return i;
}

/* Find the most recent common ancestor of barrier CURRENT, and the
   list of barriers in DEST-HIST (containing DEST-DEPTH pointers).
   Returns a null pointer if no such barrier can be found. */
static rep_barrier *
common_ancestor (rep_barrier *current, rep_barrier **dest_hist, int dest_depth)
{
    rep_barrier *ptr;
    int first_dest = 0;

    for (ptr = current; ptr != 0; ptr = ptr->next)
    {
	int i;
	for (i = first_dest; i < dest_depth; i++)
	{
	    if (dest_hist[i]->point == ptr->point)
		return ptr;
	    else if (SP_NEWER_P (dest_hist[i]->point, ptr->point))
		first_dest = i + 1;
	}
	if (ptr->closed)
	    break;
    }

    return 0;
}


/* continuations */

/* save the original stack for continuation C */
static void
save_stack (rep_continuation *c)
{
    size_t size;

    FLUSH_REGISTER_WINDOWS;

    /* __builtin_frame_address doesn't give the right thing on athlon64 */

#if defined (__GNUC__) && !defined (BROKEN_ALPHA_GCC) && !defined (__x86_64)
    c->stack_top = __builtin_frame_address (0);
#else
    c->stack_top = (char *) &size;
#endif

#if STACK_DIRECTION < 0
    size = c->stack_bottom - c->stack_top;
#else
    size = c->stack_top - c->stack_bottom;
#endif

    if (c->stack_copy != 0)
    {
	if (c->stack_size < size || (c->stack_size - size) > CONTIN_MAX_SLOP)
	{
	    rep_free (c->stack_copy);
	    rep_data_after_gc -= c->stack_size;
	    c->stack_copy = 0;
	}
    }

    if (c->stack_copy == 0)
    {
	c->stack_size = size;
	c->stack_copy = rep_alloc (size);
	rep_data_after_gc += size;
    }

    c->real_size = size;
#if STACK_DIRECTION < 0
    memcpy (c->stack_copy, c->stack_top, c->real_size);
#else
    memcpy (c->stack_copy, c->stack_bottom, c->real_size);
#endif
}

/* Make sure that the current frame has enough space under it to
   hold the saved copy in C, then invoke the continuation */
static void
grow_stack_and_invoke (rep_continuation *c, char *water_mark)
{
    volatile char growth[STACK_GROWTH];

    /* if stack isn't large enough, recurse again */

#if STACK_DIRECTION < 0
    if (water_mark >= c->stack_top)
	grow_stack_and_invoke (c, (char *) growth + STACK_GROWTH);
#else
    if (water_mark <= c->stack_top)
	grow_stack_and_invoke (c, (char *) growth);
#endif

    FLUSH_REGISTER_WINDOWS;

    /* stack's big enough now, so reload the saved copy somewhere
       below the current position. */

#if STACK_DIRECTION < 0
    memcpy (c->stack_top, c->stack_copy, c->real_size);
#else
    memcpy (c->stack_bottom, c->stack_copy, c->real_size);
#endif

    longjmp (c->jmpbuf, 1);
}

static void
primitive_invoke_continuation (rep_continuation *c, repv ret)
{
    char water_mark;
    rep_barrier **dest_hist = 0, *dest_root = 0, *anc, *ptr;
    int depth;

    /* try to find a route from the current root barrier to the
       root barrier of the continuation, without crossing any
       closed barriers */

    dest_root = FIXUP (rep_barrier *, c, c->barriers);
    dest_hist = alloca (sizeof (rep_barrier *) * dest_root->depth);
    depth = trace_barriers (c, dest_hist);

    anc = common_ancestor (barriers, dest_hist, depth);
    if (anc == 0)
    {
	DEFSTRING (unreachable, "unreachable continuation");
	Fsignal (Qerror, rep_LIST_1 (rep_VAL (&unreachable)));
	return;
    }

    /* Handle any `out' barrier functions */
    for (ptr = barriers; ptr != anc; ptr = ptr->next)
    {
	DB (("invoke: outwards through %p (%d)\n", ptr, ptr->depth));
	if (ptr->out != 0)
	{
	    repv cont = rep_VAL (c);
	    rep_GC_root gc_cont, gc_ret;
	    rep_PUSHGC (gc_cont, cont);
	    rep_PUSHGC (gc_ret, ret);
	    ptr->out (ptr->data);
	    rep_POPGC; rep_POPGC;
	}
    }

    /* save the return value and recurse up the stack until there's
       room to invoke the continuation. Note that invoking this subr
       will already have restored the original environment since the
       call to Fmake_closure () will have saved its old state.. */

    invoked_continuation = c;
    invoked_continuation_ret = ret;
    invoked_continuation_ancestor = anc;

    DB (("invoke: calling continuation %p\n", c));
    grow_stack_and_invoke (c, &water_mark);
}

/* The continuations passed in from Fcall_cc () are actually closures
   around this subr. They have Qcontinuation bound to the primitive
   continuation object in their lexical environment */
DEFUN("primitive-invoke-continuation", Fprimitive_invoke_continuation,
      Sprimitive_invoke_continuation, (repv ret), rep_Subr1)
{
    repv cont = Fsymbol_value (Qcontinuation, Qnil);

    if (cont == rep_NULL || !rep_CONTINP(cont)
	|| (rep_CONTIN(cont)->car & CF_INVALID))
    {
	DEFSTRING (invalid, "invalid continuation");
	return Fsignal (Qerror, rep_LIST_1 (rep_VAL (&invalid)));
    }

    primitive_invoke_continuation (rep_CONTIN (cont), ret);
    return rep_NULL;
}

static repv
get_cont (repv arg)
{
    return Fsymbol_value (Qcontinuation, Qnil);
}

DEFUN("continuation-callable-p", Fcontinuation_callable_p,
      Scontinuation_callable_p, (repv cont), rep_Subr1) /*
::doc:rep.lang.interpreter#continuation-callable-p::
continuation-callable-p CONTINUATION

Returns `t' if the continuation object CONTINUATION from the current
execution point of the interpreter.
::end:: */
{
    rep_continuation *c;
    rep_barrier **dest_hist = 0, *dest_root = 0, *anc;
    int depth;

    rep_DECLARE1(cont, rep_FUNARGP);
    cont = rep_call_with_closure (cont, get_cont, Qnil);
    if (cont == rep_NULL)
	return rep_NULL;
    rep_DECLARE1(cont, rep_CONTINP);
    c = rep_CONTIN (cont);

    if (c->car & CF_INVALID)
	return Qnil;

    /* copied from above function */

    dest_root = FIXUP (rep_barrier *, c, c->barriers);
    dest_hist = alloca (sizeof (rep_barrier *) * dest_root->depth);
    depth = trace_barriers (c, dest_hist);

    anc = common_ancestor (barriers, dest_hist, depth);
    return anc == 0 ? Qnil : Qt;
}

static repv
primitive_call_cc (repv (*callback)(rep_continuation *, void *), void *data,
		   rep_continuation *c)
{
    struct rep_saved_regexp_data re_data;
    repv ret;

    if (root_barrier == 0)
    {
	DEFSTRING (no_root, "no dynamic root");
	return Fsignal (Qerror, rep_LIST_1 (rep_VAL (&no_root)));
    }

    if (c == 0)
    {
	c = rep_ALLOC_CELL (sizeof (rep_continuation));
	rep_data_after_gc += sizeof (rep_continuation);
	c->next = continuations;
	continuations = c;
	c->stack_copy = 0;
    }

    c->car = continuation_type ();
    
    if (setjmp (c->jmpbuf))
    {
	/* back from call/cc */
	rep_barrier *ancestor;

	/* fish out the continuation (variable `c' may have been lost) */
	c = invoked_continuation;
	invoked_continuation = 0;

	rep_lisp_depth = c->lisp_depth;
	rep_single_step_flag = c->single_step;
	rep_throw_value = c->throw_value;
	memcpy (rep_blocked_ops, c->blocked_ops, sizeof (rep_blocked_ops));
	rep_saved_matches = c->regexp_data;
	rep_gc_n_roots_stack = c->gc_n_roots;
	rep_gc_root_stack = c->gc_roots;
	rep_special_bindings = c->special_bindings;
	rep_call_stack = c->call_stack;
	root_barrier = c->root;
	barriers = c->barriers;

	ret = invoked_continuation_ret;
	invoked_continuation_ret = rep_NULL;

	ancestor = invoked_continuation_ancestor;
	invoked_continuation_ancestor = 0;

	/* handle any `in' barrier functions */
	if (barriers != 0)
	{
	    int count = barriers->depth - (ancestor ? ancestor->depth : 0);
	    rep_barrier **hist = alloca (sizeof (rep_barrier *) * count);
	    rep_barrier *ptr;
	    int i = 0;

	    for (ptr = barriers; ptr != ancestor; ptr = ptr->next)
		hist[i++] = ptr;
	    for (i = count - 1; i >= 0; i--)
	    {
		ptr = hist[i];
		DB (("invoke: inwards through %p (%d)\n", ptr, ptr->depth));
		if (ptr->in != 0)
		{
		    rep_GC_root gc_ret;
		    rep_PUSHGC (gc_ret, ret);
		    ptr->in (ptr->data);
		    rep_POPGC;
		}
	    }
	}

	rep_pop_regexp_data ();
    }
    else
    {
	/* into call/cc */

	rep_push_regexp_data (&re_data);

	c->barriers = barriers;
	c->root = root_barrier;
	root_barrier->targeted = 1;
	c->call_stack = rep_call_stack;
	c->special_bindings = rep_special_bindings;
	c->gc_roots = rep_gc_root_stack;
	c->gc_n_roots = rep_gc_n_roots_stack;
	c->regexp_data = rep_saved_matches;
	memcpy (c->blocked_ops, rep_blocked_ops, sizeof (c->blocked_ops));
	c->throw_value = rep_throw_value;
	c->single_step = rep_single_step_flag;
	c->lisp_depth = rep_lisp_depth;

	c->stack_bottom = c->root->point;
	save_stack (c);

	DB (("call/cc: saved %p; real_size=%lu (%u)\n",
	     c, (unsigned long) c->real_size, rep_stack_bottom - c->stack_top));

	ret = callback (c, data);

	rep_pop_regexp_data ();
    }

    return ret;
}

static repv
inner_call_cc (rep_continuation *c, void *data)
{
    repv proxy;
    proxy = Fmake_closure (rep_VAL(&Sprimitive_invoke_continuation), Qnil);
    rep_FUNARG(proxy)->env
	= rep_add_binding_to_env (rep_FUNARG(proxy)->env,
				  Qcontinuation, rep_VAL(c));
    return rep_call_lisp1 ((repv) data, proxy);
}

DEFUN("call/cc", Fcall_cc, Scall_cc, (repv fun), rep_Subr1) /*
::doc:rep.lang.interpreter#call/cc::
call/cc FUNCTION

Invoke FUNCTION with a single parameter, the continuation function of
the current state of the interpreter. Subsequently calling the
continuation function (with an optional single argument) will pass
control immediately back to the statement following the call to the
`call/cc' function (even if that stack frame has since been exited).
::end:: */
{
    return primitive_call_cc (inner_call_cc, (void *) fun, 0);
}


/* threads */

static inline void
thread_save_environ (rep_thread *t)
{
    t->env = rep_env;
    t->structure = rep_structure;
}

static inline void
thread_load_environ (rep_thread *t)
{
    rep_env = t->env;
    rep_structure = t->structure;
}

static void
enqueue_thread (rep_thread *t, rep_barrier *root)
{
    assert (!(t->car & TF_EXITED));
    if (!(t->car & TF_SUSPENDED))
    {
	t->pred = root->tail;
	if (t->pred != 0)
	    t->pred->next = t;
	if (root->head == 0)
	    root->head = t;
	root->tail = t;
    }
    else
    {
	rep_thread *ptr = root->susp_head;
	while (ptr != 0 && TV_LATER_P (&t->run_at, &ptr->run_at))
	    ptr = ptr->next;
	if (ptr != 0)
	{
	    t->pred = ptr->pred;
	    if (ptr->pred != 0)
		ptr->pred->next = t;
	    else
		root->susp_head = t;
	    ptr->pred = t;
	    t->next = ptr;
	}
	else
	{
	    t->pred = root->susp_tail;
	    if (t->pred != 0)
		t->pred->next = t;
	    if (root->susp_head == 0)
		root->susp_head = t;
	    root->susp_tail = t;
	}
    }
}

static void
unlink_thread (rep_thread *t)
{
    rep_barrier *root = t->cont->root;

    if (t->pred != 0)
	t->pred->next = t->next;
    if (t->next != 0)
	t->next->pred = t->pred;

    if (!(t->car & TF_SUSPENDED))
    {
	if (root->head == t)
	    root->head = t->next;
	if (root->tail == t)
	    root->tail = t->pred;
    }
    else
    {
	if (root->susp_head == t)
	    root->susp_head = t->next;
	if (root->susp_tail == t)
	    root->susp_tail = t->pred;
    }
    t->next = t->pred = 0;
}

static void
thread_wake (rep_thread *t)
{
    rep_barrier *root = t->cont->root;
    assert (t->car & TF_SUSPENDED);
    assert (!(t->car & TF_EXITED));

    unlink_thread (t);
    t->car &= ~TF_SUSPENDED;
    enqueue_thread (t, root);
}

static rep_bool
poll_threads (rep_barrier *root)
{
    rep_bool woke_any = rep_FALSE;
    rep_thread *t, *next;
    for (t = root->susp_head; t != 0; t = next)
    {
	next = t->next;
	if (t->poll && t->poll (t, t->poll_arg))
	{
	    thread_wake (t);
	    woke_any = rep_TRUE;
	}
    }
    return woke_any;
}

static repv
inner_thread_invoke (rep_continuation *c, void *data)
{
    rep_thread *t = data;
    t->cont = c;
    rep_thread_lock = root_barrier->head->lock;
    DB (("invoking thread %p\n", root_barrier->head));
    thread_load_environ (root_barrier->head);
    primitive_invoke_continuation (root_barrier->head->cont, Qnil);
    return rep_NULL;
}

static void
thread_invoke (void)
{
again:
    if (root_barrier == 0)
	return;

    if (root_barrier->head != 0)
    {
	rep_thread *active = root_barrier->active;
	assert (root_barrier->head != 0);
	root_barrier->active = root_barrier->head;
	if (active != 0)
	{
	    /* save the continuation of this thread,
	       then invoke the next thread */
	    active->lock = rep_thread_lock;
	    thread_save_environ (active);
	    primitive_call_cc (inner_thread_invoke, active, active->cont);
	}
	else
	{
	    rep_thread_lock = root_barrier->head->lock;
	    DB (("invoking thread %p\n", root_barrier->head));
	    thread_load_environ (root_barrier->head);
	    primitive_invoke_continuation (root_barrier->head->cont, Qnil);
	}
    }
    else
    {
	/* No thread to run. If no suspended threads return from the
	   root barrier. Else sleep.. */
	if (root_barrier->susp_head == 0)
	{
	    root_barrier->active = 0;
	    assert (rep_throw_value != exit_barrier_cell);
	    rep_CDR (exit_barrier_cell) = rep_throw_value;
	    rep_throw_value = exit_barrier_cell;
	    DB (("no more threads, throwing to root..\n"));
	    return;
	}
	else if (poll_threads (root_barrier))
	{
	    /* something woke */
	    goto again;
	}
	else
	{
	    rep_thread *b = root_barrier->susp_head;
	    struct timeval now;
	    gettimeofday (&now, 0);
	    DB (("no more threads, sleeping..\n"));
	    if (TV_LATER_P (&b->run_at, &now))
	    {
		struct timeval delta;
		delta.tv_sec = b->run_at.tv_sec - now.tv_sec;
		delta.tv_usec = b->run_at.tv_usec - now.tv_usec;
		while (delta.tv_usec < 0)
		{
		    delta.tv_usec += 1000000;
		    delta.tv_sec--;
		}
		rep_sleep_for (delta.tv_sec, delta.tv_usec / 1000);
	    }
	    DB (("..waking thread %p\n", b));
	    thread_wake (b);
	    goto again;
	}
    }
}

static void
thread_delete (rep_thread *t)
{
    rep_barrier *root = t->cont->root;
    rep_thread *active = root->head;

    unlink_thread (t);
    t->car |= TF_EXITED;
    if (active == t)
	thread_invoke ();
}

static repv
inner_make_thread (rep_continuation *c, void *data)
{
    rep_thread *t = data;
    t->cont = c;
    enqueue_thread (t, t->cont->root);
    return -1;
}

static rep_thread *
new_thread (repv name)
{
    rep_thread *t = rep_ALLOC_CELL (sizeof (rep_thread));
    rep_data_after_gc += sizeof (rep_thread);
    memset (t, 0, sizeof (rep_thread));
    t->car = thread_type ();
    t->name = name;
    t->poll = 0;
    t->poll_arg = 0;
    t->exit_val = rep_NULL;
    t->next_alloc = threads;
    threads = t;
    return t;
}

static void
ensure_default_thread (void)
{
    if (root_barrier->active == 0)
    {
	/* entering threaded execution. make the default thread */
	rep_thread *x = new_thread (Qnil);
	thread_save_environ (x);
	/* this continuation will never get called,
	   but it simplifies things.. */
	if (primitive_call_cc (inner_make_thread, x, 0) != -1)
	    abort ();
	root_barrier->active = x;
    }
}

static rep_thread *
make_thread (repv thunk, repv name, rep_bool suspended)
{
    repv ret;
    rep_GC_root gc_thunk;
    rep_thread *t;

    if (root_barrier == 0)
	return 0;

    t = new_thread (name);
    if (suspended)
	t->car |= TF_SUSPENDED;
    thread_save_environ (t);

    ensure_default_thread ();

    rep_PUSHGC (gc_thunk, thunk);
    ret = primitive_call_cc (inner_make_thread, t, 0);
    rep_POPGC;
    if (ret == -1)
	return t;
    else
    {
	ret = rep_call_lisp0 (thunk);
	t->car |= TF_EXITED;
	if (ret != rep_NULL)
	{
	    t->exit_val = ret;
	    thread_delete (t);
	    assert (rep_throw_value == exit_barrier_cell);
	}
	else
	{
	    /* exited with a throw, throw out of the dynamic root */
	    rep_CDR (exit_barrier_cell) = rep_throw_value;
	    rep_throw_value = exit_barrier_cell;
	}
	return 0;
    }
}

static rep_bool
thread_yield (void)
{
    struct timeval now;
    rep_thread *ptr, *next;
    rep_thread *old_head;

    if (root_barrier == 0)
	return rep_FALSE;

    old_head = root_barrier->head;
    rep_pending_thread_yield = rep_FALSE;
    if (root_barrier->head && root_barrier->head->next)
    {
	rep_thread *old = root_barrier->head;
	if (old->pred != 0)
	    old->pred->next = old->next;
	if (old->next != 0)
	    old->next->pred = old->pred;
	root_barrier->head = old->next;
	old->next = 0;
	old->pred = root_barrier->tail;
	old->pred->next = old;
	root_barrier->tail = old;
    }

    /* check suspend queue for threads that need waking */

    if (root_barrier->susp_head != 0)
	gettimeofday (&now, 0);
    for (ptr = root_barrier->susp_head; ptr != 0; ptr = next)
    {
	next = ptr->next;
	if (TV_LATER_P (&now, &ptr->run_at)
	    || (ptr->poll && ptr->poll (ptr, ptr->poll_arg)))
	{
	    thread_wake (ptr);
	}
    }

    if (root_barrier->head != old_head)
    {
	thread_invoke ();
	return rep_TRUE;
    }
    else
	return rep_FALSE;
}

static void
thread_suspend (rep_thread *t, unsigned long msecs,
		rep_bool (*poll)(rep_thread *t, void *arg), void *poll_arg)
{
    rep_barrier *root = t->cont->root;
    assert (!(t->car & TF_SUSPENDED));
    assert (!(t->car & TF_EXITED));

    unlink_thread (t);
    t->car |= TF_SUSPENDED;
    if (msecs == 0)
    {
	/* XXX assumes twos-complement representation.. but Solaris
	   XXX has a weird struct timeval.. */
	t->run_at.tv_sec = ~0UL >> 1;
	t->run_at.tv_usec = ~0UL >> 1;
    }
    else
    {
	gettimeofday (&t->run_at, 0);
	t->run_at.tv_sec += (msecs / 1000);
	t->run_at.tv_usec += (msecs % 1000) * 1000;
	if (t->run_at.tv_usec > 1000000)
	{
	    t->run_at.tv_sec += t->run_at.tv_usec / 1000000;
	    t->run_at.tv_usec = t->run_at.tv_usec % 1000000;
	}
    }
    t->poll = poll;
    t->poll_arg = poll_arg;
    t->exit_val = Qnil;
    enqueue_thread (t, root);
    if (root_barrier->active == t)
	thread_invoke ();
}

unsigned long
rep_max_sleep_for (void)
{
    rep_barrier *root = root_barrier;
    if (root == 0 || root->active == 0)
    {
	/* not using threads, sleep as long as you like..
	   XXX grr.. using ULONG_MAX doesn't work on solaris*/
	return UINT_MAX;
    }
    else if (root->head != 0 && root->head->next != 0)
    {
	/* other threads ready to run, don't sleep */
	return 0;
    }
    else if (root->susp_head != 0)
    {
	/* other threads sleeping, how long until the first wakes? */
	/* XXX ignores polling */
	struct timeval now;
	long msecs;
	gettimeofday (&now, 0);
	msecs = ((root->susp_head->run_at.tv_sec - now.tv_sec) * 1000
		 + (root->susp_head->run_at.tv_usec - now.tv_usec) / 1000);
	return MAX (msecs, 0);
    }
    else
    {
	/* whatever.. */
	return UINT_MAX;
    }
}


/* type hooks */

static void
mark_cont (repv obj)
{
    rep_GC_root *roots;
    rep_GC_n_roots *nroots;
    struct rep_Call *calls;
    struct rep_saved_regexp_data *matches;
    rep_barrier *barrier;

    rep_continuation *c = rep_CONTIN (obj);
    rep_MARKVAL (c->throw_value);
    rep_MARKVAL (c->special_bindings);

    for (barrier = c->barriers;
	 barrier != 0 && !SP_OLDER_P ((char *) barrier, c->stack_bottom);
	 barrier = FIXUP(rep_barrier *, c, barrier)->next)
    {
	rep_barrier *ptr = FIXUP (rep_barrier *, c, barrier);
	rep_thread *t;
	for (t = ptr->head; t != 0; t = t->next)
	    rep_MARKVAL (rep_VAL (t));
	for (t = ptr->susp_head; t != 0; t = t->next)
	    rep_MARKVAL (rep_VAL (t));
	rep_MARKVAL (rep_VAL (ptr->active));
    }
    for (roots = c->gc_roots;
	 roots != 0 && !SP_OLDER_P ((char *) roots, c->stack_bottom);
	 roots = FIXUP(rep_GC_root *, c, roots)->next)
    {
	repv *ptr = FIXUP(rep_GC_root *, c, roots)->ptr;
	rep_MARKVAL (*FIXUP(repv *, c, ptr));
    }
    for (nroots = c->gc_n_roots;
	 nroots != 0 && !SP_OLDER_P ((char *) roots, c->stack_bottom);
	 nroots = FIXUP(rep_GC_n_roots *, c, nroots)->next)
    {
	repv *ptr = FIXUP(repv *, c, FIXUP(rep_GC_n_roots *, c, nroots)->first);
	int n = FIXUP(rep_GC_n_roots *, c, nroots)->count, i;
	for (i = 0; i < n; i++)
	    rep_MARKVAL (ptr[i]);
    }
    for (calls = c->call_stack;
	 calls != 0 && !SP_OLDER_P ((char *) calls, c->stack_bottom);
	 calls = FIXUP(struct rep_Call *, c, calls)->next)
    {
	struct rep_Call *lc = FIXUP(struct rep_Call *, c, calls);
	rep_MARKVAL(lc->fun);
	rep_MARKVAL(lc->args);
	rep_MARKVAL(lc->current_form);
	rep_MARKVAL(lc->saved_env);
	rep_MARKVAL(lc->saved_structure);
    }
    for (matches = c->regexp_data;
	 matches != 0 && !SP_OLDER_P ((char *) matches, c->stack_bottom);
	 matches = FIXUP(struct rep_saved_regexp_data *, c, matches)->next)
    {
	struct rep_saved_regexp_data *sd
	    = FIXUP(struct rep_saved_regexp_data *, c, matches);
	assert (sd->type ==  rep_reg_obj || sd->type == rep_reg_string);
	if(sd->type == rep_reg_obj)
	{
	    int i;
	    for(i = 0; i < rep_NSUBEXP; i++)
	    {
		rep_MARKVAL(sd->matches.obj.startp[i]);
		rep_MARKVAL(sd->matches.obj.endp[i]);
	    }
	}
	rep_MARKVAL(sd->data);
    }
}

static void
mark_all (void)
{
    rep_barrier *ptr;
    for (ptr = barriers; ptr != 0; ptr = ptr->next)
    {
	rep_thread *t;
	for (t = ptr->head; t != 0; t = t->next)
	    rep_MARKVAL (rep_VAL (t));
	for (t = ptr->susp_head; t != 0; t = t->next)
	    rep_MARKVAL (rep_VAL (t));
	rep_MARKVAL (rep_VAL (ptr->active));
    }
}
	
static void
sweep_cont (void)
{
    rep_continuation *c = continuations;
    continuations = 0;
    while (c)
    {
	rep_continuation *next = c->next;
	if (!rep_GC_CELL_MARKEDP (rep_VAL (c)))
	{
	    rep_free (c->stack_copy);
	    rep_FREE_CELL (c);
	}
	else
	{
	    rep_GC_CLR_CELL (rep_VAL (c));
	    c->next = continuations;
	    continuations = c;
	}
	c = next;
    }
}

static void
print_cont (repv stream, repv obj)
{
    rep_stream_puts (stream, "#<continuation>", -1, rep_FALSE);
}

static int
continuation_type (void)
{
    static int type;

    if (type == 0)
    {
	type = rep_register_new_type ("continuation",
				      rep_ptr_cmp, print_cont, print_cont, 
				      sweep_cont, mark_cont, mark_all,
				      0, 0, 0, 0, 0, 0);
    }

    return type;
}

static void
mark_thread (repv obj)
{
    rep_MARKVAL (rep_VAL (THREAD (obj)->cont));
    rep_MARKVAL (THREAD (obj)->env);
    rep_MARKVAL (THREAD (obj)->structure);
    rep_MARKVAL (THREAD (obj)->name);
    rep_MARKVAL (THREAD (obj)->exit_val);
}

static void
sweep_thread (void)
{
    rep_thread *t = threads;
    threads = 0;
    while (t)
    {
	rep_thread *next = t->next_alloc;
	if (!rep_GC_CELL_MARKEDP (rep_VAL (t)))
	    rep_FREE_CELL (t);
	else
	{
	    rep_GC_CLR_CELL (rep_VAL (t));
	    t->next_alloc = threads;
	    threads = t;
	}
	t = next;
    }
}

static void
print_thread (repv stream, repv obj)
{
    rep_stream_puts (stream, "#<thread", -1, rep_FALSE);
    if (rep_STRINGP (THREAD (obj)->name))
    {
	rep_stream_putc (stream, ' ');
	rep_stream_puts (stream, rep_STR (THREAD (obj)->name), -1, rep_FALSE);
    }
    rep_stream_putc (stream, '>');
}

static int
thread_type (void)
{
    static int type;

    if (type == 0)
    {
	type = rep_register_new_type ("thread", rep_ptr_cmp,
				      print_thread, print_thread, 
				      sweep_thread, mark_thread,
				      0, 0, 0, 0, 0, 0, 0);
    }

    return type;
}

#else /* WITH_CONTINUATIONS */

repv
rep_call_with_barrier (repv (*callback)(repv), repv arg,
		       rep_bool closed, void (*in)(void *),
		       void (*out)(void *), void *data)
{
    return callback (arg);
}

DEFSTRING (ccc_missing, "call/cc was not included in this system");

static repv
call_cc_missing (void)
{
    return Fsignal (Qerror, rep_LIST_1 (rep_VAL (&ccc_missing)));
}


DEFUN ("call/cc", Fcall_cc, Scall_cc, (repv fun), rep_Subr1)
{
    return call_cc_missing ();
}

DEFUN("continuation-callable-p", Fcontinuation_callable_p,
      Scontinuation_callable_p, (repv cont), rep_Subr1)
{
    return rep_signal_arg_error (cont, 1);
}

unsigned long
rep_max_sleep_for (void)
{
    return UINT_MAX;
}

#endif /* !WITH_CONTINUATIONS */


/* misc lisp functions */

/* Bind one object, returning the handle to later unbind by. */
static repv
bind_object(repv obj)
{
    rep_type *t = rep_get_data_type(rep_TYPE(obj));
    if (t->bind != 0)
	return t->bind(obj);
    else
	return Qnil;
}

static void
unbind_object (repv handle)
{
    repv obj;
    rep_type *t;
    if (handle == Qnil)
	return;
    else if (rep_CONSP (handle))
	obj = rep_CAR (handle);
    else
	obj = handle;
    t = rep_get_data_type (rep_TYPE (obj));
    if (t->unbind != 0)
	t->unbind(handle);
}

static void
call_with_inwards (void *data_)
{
    repv *data = data_;
    if (data[0] != rep_NULL)
	data[1] = bind_object (data[0]);
    else
	data[1] = rep_NULL;
}

static void
call_with_outwards (void *data_)
{
    repv *data = data_;
    if (data[1] != rep_NULL)
    {
	unbind_object (data[1]);
	data[1] = rep_NULL;
    }
}

DEFUN("call-with-object", Fcall_with_object,
      Scall_with_object, (repv arg, repv thunk), rep_Subr2) /*
::doc:rep.lang.interpreter#call-with-object::
call-with-object ARG THUNK

Call the zero-parameter function THUNK, with object ARG temporarily
`bound' (a type-specific operation, usually to make ARG `active' in
some way). When THUNK returns ARG is unbound. The value returned by
THUNK is then returned.

If THUNK is ever left due to a continuation being invoked, ARG will be
unbound. If THUNK is subsequently reentered, ARG will be rebound.
::end:: */
{
    repv data[2];			/* { ARG, HANDLE } */
    data[0] = arg;
    data[1] = bind_object(data[0]);
    if (data[1] != rep_NULL)
    {
	repv ret;
	rep_GC_n_roots gc_data;
	rep_PUSHGCN (gc_data, data, 2);
	ret = rep_call_with_barrier (rep_call_lisp0, thunk,
				     rep_FALSE, call_with_inwards,
				     call_with_outwards, data);
	unbind_object (data[1]);
	rep_POPGCN;
	return ret;
    }
    else
	return rep_NULL;
}

DEFUN("call-with-dynamic-root", Fcall_with_dynamic_root,
      Scall_with_dynamic_root, (repv thunk), rep_Subr1) /*
::doc:rep.lang.interpreter#call-with-dynamic-root::
call-with-dynamic-root THUNK

Call the zero-parameter function THUNK, as the root of a new execution
environment. This means that the continuation of THUNK will always be
reached once, and once only. Any continuations above the new root may
not be invoked from inside the root.
::end:: */
{
    return rep_call_with_barrier (rep_call_lisp0, thunk, rep_TRUE, 0, 0, 0);
}

static void
call_in (void *data_)
{
    repv *data = data_;
    if (data[0] != Qnil)
	rep_call_lisp0 (data[0]);
}

static void
call_out (void *data_)
{
    repv *data = data_;
    if (data[1] != Qnil)
	rep_call_lisp0 (data[1]);
}

DEFUN("call-with-barrier", Fcall_with_barrier, Scall_with_barrier,
      (repv thunk, repv closed, repv in, repv out), rep_Subr4) /*
::doc:rep.lang.interpreter#call-with-barrier::
call-with-barrier THUNK CLOSED [IN-THUNK] [OUT-THUNK]

Call THUNK inside a new execution environment. If CLOSED is non-`nil'
then the new environment will be exited exactly once (i.e.
continuations may not pass through it).

Alternatively, if CLOSED is `nil' then the environment is said to be
`open' and continuations may cause control to flow into and out of the
new environment. As this happens one of IN-THUNK or OUT-THUNK will be
called (if defined).

The value of this function is the value returned by THUNK.
::end:: */
{
    repv thunks[2], ret;
    rep_GC_n_roots gc_thunks;
    thunks[0] = in;
    thunks[1] = out;
    rep_PUSHGCN (gc_thunks, thunks, 2);
    ret = rep_call_with_barrier (rep_call_lisp0, thunk,
				 closed == Qnil ? rep_FALSE : rep_TRUE,
				 call_in, call_out, thunks);
    rep_POPGCN;
    return ret;
}

DEFUN("make-thread", Fmake_thread, Smake_thread, (repv thunk, repv name), rep_Subr2) /*
::doc:rep.threads#make-thread::
make-thread THUNK [NAME]

Create and return an object representing a new thread of execution. The
new thread will begin by calling THUNK, a function with zero
parameters.
::end:: */
{
#ifdef WITH_CONTINUATIONS
    return rep_VAL (make_thread (thunk, name, rep_FALSE));
#else
    return call_cc_missing ();
#endif
}

DEFUN("make-suspended-thread", Fmake_suspended_thread, Smake_suspended_thread,
      (repv thunk, repv name), rep_Subr2) /*
::doc:rep.threads#make-suspended-thread::
make-suspended-thread THUNK [NAME]

Identical to `make-thread', except that the created thread will be
immediately put in the suspended state.
::end:: */
{
#ifdef WITH_CONTINUATIONS
    return rep_VAL (make_thread (thunk, name, rep_TRUE));
#else
    return call_cc_missing ();
#endif
}

DEFUN("thread-yield", Fthread_yield, Sthread_yield, (void), rep_Subr0) /*
::doc:rep.threads#thread-yield::
thread-yield

Pass control away from the current thread if other threads are waiting
to run.
::end:: */
{
#ifdef WITH_CONTINUATIONS
    return thread_yield () ? Qt : Qnil;
#else
    return Qnil;
#endif
}

DEFUN("thread-delete", Fthread_delete, Sthread_delete, (repv th), rep_Subr1) /*
::doc:rep.threads#thread-delete::
thread-delete [THREAD]

Mark THREAD (or the current thread), as being deleted. It will not be
switched to in the future. If the current thread is deleted, control
will be passed to the next runnable thread. Deleting the last runnable
thread results forces the containing dynamic root to be closed.
::end:: */
{
#ifdef WITH_CONTINUATIONS
    if (th == Qnil)
	th = Fcurrent_thread (Qnil);
    rep_DECLARE1 (th, THREADP);
    thread_delete (THREAD (th));
    return Qnil;
#else
    return rep_signal_arg_error (th, 1);
#endif
}

DEFUN("thread-suspend", Fthread_suspend,
      Sthread_suspend, (repv th, repv msecs), rep_Subr2) /*
::doc:rep.threads#thread-suspend::
thread-suspend [THREAD] [MSECS]

Mark THREAD (or the current thread) as being suspended. It will not be
selected until it has this status removed. Suspending the current
thread will pass control to the next runnable thread. If there are no
runnable threads, then sleep until the next thread becomes runnable.

Returns true if the timeout was reached.
::end:: */
{
#ifdef WITH_CONTINUATIONS
    long timeout;
    repv no_timeout;
    if (th == Qnil)
	th = Fcurrent_thread (Qnil);
    rep_DECLARE1 (th, THREADP);
    rep_DECLARE2_OPT (msecs, rep_NUMERICP);
    timeout = (msecs == Qnil) ? 1 : rep_get_long_int (msecs);
    thread_suspend (THREAD (th), timeout, 0, 0);
    no_timeout = THREAD (th)->exit_val;
    THREAD (th)->exit_val = rep_NULL;
    return no_timeout == Qnil ? Qt : Qnil;
#else
    return rep_signal_arg_error (th, 1);
#endif
}

#ifdef WITH_CONTINUATIONS
static rep_bool
thread_join_poller (rep_thread *t, void *arg)
{
    rep_thread *th = arg;
    return (th->car & TF_EXITED) ? rep_TRUE : rep_FALSE;
}
#endif

DEFUN("thread-join", Fthread_join,
      Sthread_join, (repv th, repv msecs, repv def), rep_Subr3) /*
::doc:rep.threads#thread-join::
thread-join THREAD [MSECS] [DEFAULT-VALUE]

Suspend the current thread until THREAD has exited, or MSECS
milliseconds have passed. If THREAD exits normally, return the value of
the last form it evaluated, else return DEFAULT-VALUE.

It is an error to call thread-join on a THREAD that is not a member of
current dynamic root.
::end:: */
{
#ifdef WITH_CONTINUATIONS
    repv self = Fcurrent_thread (Qnil);
    rep_DECLARE (1, th, XTHREADP (th) && th != self
		 && THREAD (th)->cont->root == root_barrier);
    if (THREADP (self))
    {
	rep_GC_root gc_th;
	rep_PUSHGC (gc_th, th);
	rep_DECLARE2_OPT (msecs, rep_NUMERICP);
	thread_suspend (THREAD (self),
			rep_get_long_int (msecs),
			thread_join_poller, THREAD (th));
	THREAD (self)->exit_val = rep_NULL;
	rep_POPGC;
	if ((THREAD (th)->car & TF_EXITED) && THREAD (th)->exit_val)
	    return THREAD (th)->exit_val;
    }
    return def;
#else
    return rep_signal_arg_error (th, 1);
#endif
}

DEFUN("thread-wake", Fthread_wake, Sthread_wake, (repv th), rep_Subr1) /*
::doc:rep.threads#thread-wake::
thread-wake [THREAD]

If THREAD (or the current thread) is currently suspended, mark it as
being runnable once more.
::end:: */
{
#ifdef WITH_CONTINUATIONS
    if (th == Qnil)
	th = Fcurrent_thread (Qnil);
    rep_DECLARE1 (th, THREADP);
    THREAD (th)->exit_val = Qt;		/* signals timeout not reached */
    thread_wake (THREAD (th));
    return Qnil;
#else
    return rep_signal_arg_error (th, 1);
#endif
}

DEFUN("threadp", Fthreadp, Sthreadp, (repv arg), rep_Subr1) /*
::doc:rep.threads#threadp::
threadp ARG

Return `t' if ARG is a thread object.
::end:: */
{
#ifdef WITH_CONTINUATIONS
    return XTHREADP (arg) ? Qt : Qnil;
#else
    return Qnil;
#endif
}

DEFUN("thread-suspended-p", Fthread_suspended_p,
      Sthread_suspended_p, (repv th), rep_Subr1) /*
::doc:rep.threads#thread-suspended-p::
thread-suspended-p THREAD

Return `t' if THREAD is currently suspended from running.
::end:: */
{
#ifdef WITH_CONTINUATIONS
    rep_DECLARE1 (th, THREADP);
    return (THREAD (th)->car & TF_SUSPENDED) ? Qt : Qnil;
#else
    return rep_signal_arg_error (th, 1);
#endif
}

DEFUN("thread-exited-p", Fthread_exited_p,
      Sthread_exited_p, (repv th), rep_Subr1) /*
::doc:rep.threads#thread-exited-p::
thread-exited-p THREAD

Return `t' if THREAD has exited.
::end:: */
{
#ifdef WITH_CONTINUATIONS
    rep_DECLARE1 (th, XTHREADP);
    return (THREAD (th)->car & TF_EXITED) ? Qt : Qnil;
#else
    return rep_signal_arg_error (th, 1);
#endif
}

DEFUN("current-thread", Fcurrent_thread,
      Scurrent_thread, (repv depth), rep_Subr1) /*
::doc:rep.threads#current-thread::
current-thread [DEPTH]

Return the currently executing thread.
::end:: */
{
#ifdef WITH_CONTINUATIONS
    rep_barrier *root;

    rep_DECLARE1_OPT (depth, rep_INTP);
    if (depth == Qnil)
	depth = rep_MAKE_INT (0);

    if (depth == rep_MAKE_INT (0))
	ensure_default_thread ();

    root = get_dynamic_root (rep_INT (depth));
    if (root == 0)
	return Qnil;
    else
	return (root->active) ? rep_VAL (root->active) : Qnil;
#else
    return Qnil;
#endif
}

DEFUN("all-threads", Fall_threads, Sall_threads, (repv depth), rep_Subr1) /*
::doc:rep.threads#all-threads::
all-threads [DEPTH]

Return a list of all threads.
::end:: */
{
#ifdef WITH_CONTINUATIONS
    rep_barrier *root;

    rep_DECLARE1_OPT (depth, rep_INTP);
    if (depth == Qnil)
	depth = rep_MAKE_INT (0);

    if (depth == rep_MAKE_INT (0))
	ensure_default_thread ();

    root = get_dynamic_root (rep_INT (depth));
    if (root == 0)
	return Qnil;
    else
    {
	repv out = Qnil;
	rep_thread *ptr;
	for (ptr = root->susp_tail; ptr != 0; ptr = ptr->pred)
	    out = Fcons (rep_VAL (ptr), out);
	for (ptr = root->tail; ptr != 0; ptr = ptr->pred)
	    out = Fcons (rep_VAL (ptr), out);
	return out;
    }
#else
    return Qnil;
#endif
}

DEFUN("thread-forbid", Fthread_forbid, Sthread_forbid, (void), rep_Subr0) /*
::doc:rep.threads#thread-forbid::
thread-forbid

Increment the thread preemption lock. When greather than zero all
preemption of threads is disabled. Returns `t' if preemption is blocked
as this function returns.
::end:: */
{
    rep_FORBID;
    return rep_PREEMPTABLE_P ? Qnil : Qt;
}

DEFUN("thread-permit", Fthread_permit, Sthread_permit, (void), rep_Subr0) /*
::doc:rep.threads#thread-permit::
thread-permit

Decrement the thread preemption lock. When greather than zero all
preemption of threads is disabled. Returns `t' if preemption is blocked
as this function returns.
::end:: */
{
    rep_PERMIT;
    return rep_PREEMPTABLE_P ? Qnil : Qt;
}

DEFUN("thread-name", Fthread_name, Sthread_name, (repv th), rep_Subr1) /*
::doc:rep.threads#thread-name:
thread-name THREAD

Return the name of the thread THREAD.
::end:: */
{
#ifdef WITH_CONTINUATIONS
    rep_DECLARE1 (th, XTHREADP);
    return THREAD (th)->name;
#else
    return rep_signal_arg_error (th, 1);
#endif
}


/* dl hooks */

void
rep_continuations_init (void)
{
    repv tem = rep_push_structure ("rep.lang.interpreter");

#ifdef WITH_CONTINUATIONS
    exit_barrier_cell = Fcons (Qnil, Qnil);
    rep_mark_static (&exit_barrier_cell);
    rep_INTERN(continuation);
    rep_ADD_INTERNAL_SUBR(Sprimitive_invoke_continuation);
#endif

    rep_ADD_SUBR(Scall_cc);
    rep_ADD_SUBR(Scontinuation_callable_p);
    rep_ADD_SUBR(Scall_with_object);
    rep_ADD_SUBR(Scall_with_dynamic_root);
    rep_ADD_SUBR(Scall_with_barrier);
    rep_pop_structure (tem);

    tem = rep_push_structure ("rep.threads");
    rep_ADD_SUBR(Smake_thread);
    rep_ADD_SUBR(Smake_suspended_thread);
    rep_ADD_SUBR(Sthread_yield);
    rep_ADD_SUBR(Sthread_delete);
    rep_ADD_SUBR(Sthread_suspend);
    rep_ADD_SUBR(Sthread_join);
    rep_ADD_SUBR(Sthread_wake);
    rep_ADD_SUBR(Sthreadp);
    rep_ADD_SUBR(Sthread_suspended_p);
    rep_ADD_SUBR(Sthread_exited_p);
    rep_ADD_SUBR(Scurrent_thread);
    rep_ADD_SUBR(Sall_threads);
    rep_ADD_SUBR(Sthread_forbid);
    rep_ADD_SUBR(Sthread_permit);
    rep_ADD_SUBR(Sthread_name);
    rep_pop_structure (tem);
}
