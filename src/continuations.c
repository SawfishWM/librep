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
   lexical environment, and a forbid-preemption count.

   To avoid having to consider preemption throughout the interpreter,
   there are only (currently) two preemption points, in funcall and the
   bytecode jmp instructions. The rep_test_int_counter is used to
   decide when to try to preempt the current thread. In non-threaded
   mode (i.e. thread_invoke () hasn't been called in the current root),
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

   (setq thread-1 (make-thread (lambda () (thread-fun 1)) 'thread-1))
   (setq thread-2 (make-thread (lambda () (thread-fun 2)) 'thread-2))

   (thread-delete)   */

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

#if defined (DEBUG)
# define DB(x) printf x
#else
# define DB(x)
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
    int closed : 1;
    int targeted : 1;		/* may contain continuations */
};

/* List of all currently active barriers (on the current stack) */
static rep_barrier *barriers;

/* The outermost active closed barrier (the dynamic root in guile terms?) */
static rep_barrier *root_barrier;

/* The data saved for a continuation */
struct rep_continuation_struct {
    repv car;
    rep_continuation *next;

    jmp_buf jmpbuf;
    char *stack_copy, *stack_top, *stack_bottom;
    size_t stack_size;

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

#define rep_CONTIN(v)		((rep_continuation *)rep_PTR(v))
#define rep_CONTINP(v)		rep_CELL16_TYPEP(v, rep_continuation_type)

#define CF_INVALID		(1 << rep_CELL16_TYPE_BITS)

/* the cell16 typecode allocated for continuation objects */
static int rep_continuation_type;

/* list of all allocated continuations */
static rep_continuation *continuations;

struct rep_thread_struct {
    repv car;
    rep_thread *next_alloc;
    rep_thread *next, *pred;
    rep_continuation *cont;
    repv env, special_env, fh_env;
    repv (*bytecode)(repv, repv, repv, repv);
    int lock;
    struct timeval run_at;
};

#define THREADP(v)		rep_CELL16_TYPEP(v, rep_thread_type)
#define THREAD(v)		((rep_thread *) rep_PTR (v))

#define TF_EXITED (1 << (rep_CELL16_TYPE_BITS + 0))
#define TF_SUSPENDED  (1 << (rep_CELL16_TYPE_BITS + 1))

static int rep_thread_type;
static rep_thread *threads;

/* Threads only preempted when this is zero. */
int rep_thread_lock = 0;

/* True when the current thread should be preempted soon */
rep_bool rep_pending_thread_yield;

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
    if (addr <= c->stack_bottom)
	return (addr - c->stack_top) + c->stack_copy;
    else
	return addr;
#else
    if (addr >= c->stack_bottom)
	return (addr - c->stack_bottom) + c->stack_copy;
    else
	return addr;
#endif
}

#define FIXUP(t,c,addr) ((t) (fixup ((char *) (addr), (c))))


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
#if STACK_DIRECTION < 0
    /* ensure all of barrier is saved on the stack */
    b.point += sizeof (rep_barrier);
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

    DB(("with-barrier[%s]: out %p (%d)\n",
	closed ? "closed" : "open", &b, b.depth));

    if (closed && b.targeted)
    {
	/* Invalidate any continuations that require this barrier */
	rep_continuation *c;
	for (c = continuations; c != 0; c = c->next)
	{
	    if (c->root == &b)
		c->car |= CF_INVALID;
	}
    }

    barriers = b.next;
    root_barrier = b.root;
    return ret;
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
	    else if (SP_OLDER_P (dest_hist[i]->point, ptr->point))
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
    FLUSH_REGISTER_WINDOWS;

#if STACK_DIRECTION < 0
    c->stack_size = c->stack_bottom - c->stack_top;
#else
    c->stack_size = c->stack_top - c->stack_bottom;
#endif
    c->stack_copy = rep_alloc (c->stack_size);
    rep_data_after_gc += c->stack_size;
#if STACK_DIRECTION < 0
    memcpy (c->stack_copy, c->stack_top, c->stack_size);
#else
    memcpy (c->stack_copy, c->stack_bottom, c->stack_size);
#endif
}

/* Make sure that the current frame has enough space under it to
   hold the saved copy in C, then invoke the continuation */
static void
grow_stack_and_invoke (rep_continuation *c, char *water_mark)
{
    volatile char growth[STACK_GROWTH];
    rep_barrier tem;

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
       below the current position. But preserve the current contents
       of the continuation's barrier. */

    tem = *c->root;
#if STACK_DIRECTION < 0
    memcpy (c->stack_top, c->stack_copy, c->stack_size);
#else
    memcpy (c->stack_bottom, c->stack_copy, c->stack_size);
#endif
    *c->root = tem;

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
	Fsignal (Qerror,
		 rep_LIST_1 (rep_string_dup ("unreachable continuation")));
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
	return Fsignal (Qerror, rep_LIST_1 (rep_string_dup
					    ("invalid continuation")));
    }

    primitive_invoke_continuation (rep_CONTIN (cont), ret);
    return rep_NULL;
}

DEFUN("continuation-callable-p", Fcontinuation_callable_p,
      Scontinuation_callable_p, (repv cont), rep_Subr1) /*
::doc:continuation-callable-p::
continuation-callable-p CONTINUATION

Returns `t' if the continuation object CONTINUATION from the current
execution point of the interpreter.
::end:: */
{
    rep_continuation *c;
    rep_barrier **dest_hist = 0, *dest_root = 0, *anc;
    int depth;

    rep_DECLARE1(cont, rep_FUNARGP);
    cont = rep_FUNARG(cont)->env;
    rep_DECLARE(1, cont, rep_CAAR (cont) == Qcontinuation);
    c = rep_CONTIN (rep_CDAR (cont));

    if (c->car & CF_INVALID)
	return Qnil;

    /* copied from above function */

    dest_root = FIXUP (rep_barrier *, c, c->barriers);
    dest_hist = alloca (sizeof (rep_barrier *) * dest_root->depth);
    depth = trace_barriers (c, dest_hist);

    anc = common_ancestor (barriers, dest_hist, depth);
    return anc == 0 ? Qnil : Qt;
}

/* Return the address further into the stack than any part of the frame
   of the calling function. */
static char *
get_stack_top (rep_continuation *c)
{
#if defined (__GNUC__) && !defined (BROKEN_ALPHA_GCC)
    return __builtin_frame_address (0);
#else
    int dummy;
    return (char *) &dummy;
#endif
}

static repv
primitive_call_cc (repv (*callback)(rep_continuation *, void *), void *data)
{
    rep_continuation *c;
    repv ret;

    if (root_barrier == 0)
	return Fsignal (Qerror, rep_LIST_1 (rep_string_dup ("No dynamic root")));

    c = rep_ALLOC_CELL (sizeof (rep_continuation));
    rep_data_after_gc += sizeof (rep_continuation);
    c->next = continuations;
    continuations = c;
    c->car = rep_continuation_type;

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
    }
    else
    {
	/* into call/cc */

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

	c->stack_top = get_stack_top (c);
	c->stack_bottom = c->root->point;
	save_stack (c);

	DB (("call/cc: saved %p; stack_size=%lu (%u)\n",
	     c, (u_long) c->stack_size, rep_stack_bottom - c->stack_top));

	ret = callback (c, data);
    }

    return ret;
}

static repv
inner_call_cc (rep_continuation *c, void *data)
{
    repv proxy;
    proxy = Fmake_closure (rep_VAL(&Sprimitive_invoke_continuation), Qnil);
    rep_FUNARG(proxy)->env = Fcons (Fcons (Qcontinuation, rep_VAL(c)),
				    rep_FUNARG(proxy)->env);
    return rep_call_lisp1 ((repv) data, proxy);
}

DEFUN("call/cc", Fcall_cc, Scall_cc, (repv fun), rep_Subr1) /*
::doc:call/cc::
call/cc FUNCTION

Invoke FUNCTION with a single parameter, the continuation function of
the current state of the interpreter. Subsequently calling the
continuation function (with an optional single argument) will pass
control immediately back to the statement following the call to the
`call/cc' function (even if that stack frame has since been exited).

Note that invoking continuation functions do not currently affect the
values of any dynamic bindings currently in effect.
::end:: */
{
    return primitive_call_cc (inner_call_cc, (void *) fun);
}


/* threads */

static inline void
thread_save_environ (rep_thread *t)
{
    t->env = rep_env;
    t->special_env = rep_special_env;
    t->fh_env = rep_fh_env;
    t->bytecode = rep_bytecode_interpreter;
}

static inline void
thread_load_environ (rep_thread *t)
{
    rep_env = t->env;
    rep_special_env = t->special_env;
    rep_fh_env = t->fh_env;
    rep_bytecode_interpreter = t->bytecode;
}

static void
append_thread (rep_thread *t, rep_barrier *root)
{
    assert (!(t->car & TF_SUSPENDED));
    t->pred = root->tail;
    if (t->pred != 0)
	t->pred->next = t;
    if (root->head == 0)
	root->head = t;
    root->tail = t;
}

static void
unlink_thread (rep_thread *t)
{
    if (t->pred != 0)
	t->pred->next = t->next;
    if (t->next != 0)
	t->next->pred = t->pred;

    if (!(t->car & TF_SUSPENDED))
    {
	if (root_barrier->head == t)
	    root_barrier->head = t->next;
	if (root_barrier->tail == t)
	    root_barrier->tail = t->pred;
    }
    else
    {
	if (root_barrier->susp_head == t)
	    root_barrier->susp_head = t->next;
	if (root_barrier->susp_tail == t)
	    root_barrier->susp_tail = t->pred;
    }
    t->next = t->pred = 0;
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
thread_invoke ()
{
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
	    primitive_call_cc (inner_thread_invoke, active);
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
	root_barrier->active = 0;
}

static void
thread_delete (rep_thread *t)
{
    rep_barrier *root = t->cont->root;
    rep_thread *active = root->head;

    unlink_thread (t);
    if (active == t)
	thread_invoke (active);
}

static repv
inner_make_thread (rep_continuation *c, void *data)
{
    rep_thread *t = data;
    t->cont = c;
    append_thread (t, t->cont->root);
    return -1;
}

static rep_thread *
new_thread (void)
{
    rep_thread *t = rep_ALLOC_CELL (sizeof (rep_thread));
    rep_data_after_gc += sizeof (rep_thread);
    memset (t, 0, sizeof (rep_thread));
    t->car = rep_thread_type;
    t->next_alloc = threads;
    threads = t;
    return t;
}

static rep_thread *
make_thread (repv thunk)
{
    repv ret;
    rep_thread *t = new_thread ();
    thread_save_environ (t);

    if (root_barrier->active == 0)
    {
	/* entering threaded execution. make the default thread */
	rep_thread *x = new_thread ();
	thread_save_environ (t);
	/* this continuation will never get called,
	   but it simplifies things.. */
	if (primitive_call_cc (inner_make_thread, x) != -1)
	    abort ();
	root_barrier->active = x;
    }

    ret = primitive_call_cc (inner_make_thread, t);
    if (ret == -1)
	return t;
    else
    {
	ret = rep_call_lisp0 (thunk);
	t->car |= TF_EXITED;
	thread_delete (t);
	thread_invoke ();
	fprintf (stderr, "last thread died, exiting..\n");
	exit (0);
    }
}

static void
thread_yield (void)
{
    struct timeval now;
    rep_thread *ptr, *next;
    rep_thread *old_head = root_barrier->head;

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

    /* check suspend queue for threads whose time is up */
    if (root_barrier->susp_head != 0)
	gettimeofday (&now, 0);
    for (ptr = root_barrier->susp_head; ptr != 0; ptr = next)
    {
	next = ptr->next;
	if (TV_LATER_P (&now, &ptr->run_at))
	{
	    unlink_thread (ptr);
	    ptr->car &= ~TF_SUSPENDED;
	    append_thread (ptr, root_barrier);
	}
    }

    if (root_barrier->head != old_head)
	thread_invoke ();
}

static void
thread_suspend (rep_thread *t, u_long msecs)
{
    rep_barrier *root = t->cont->root;
    assert (!(t->car & TF_SUSPENDED));
    assert (!(t->car & TF_EXITED));

    unlink_thread (t);
    t->pred = root->susp_tail;
    if (root->susp_tail != 0)
	root->susp_tail->next = t;
    else
	root->susp_head = t;
    root->susp_tail = t;
    t->next = 0;
    t->car |= TF_SUSPENDED;
    if (msecs == 0)
    {
	t->run_at.tv_sec = LONG_MAX;
	t->run_at.tv_usec = LONG_MAX;
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
    if (root_barrier->active == t)
	thread_invoke ();
}

static void
thread_wake (rep_thread *t)
{
    rep_barrier *root = t->cont->root;
    assert (t->car & TF_SUSPENDED);
    assert (!(t->car & TF_EXITED));

    unlink_thread (t);
    t->car &= ~TF_SUSPENDED;
    append_thread (t, root);
}

static inline int
thread_queue_length (void)
{
    if (root_barrier == 0 || root_barrier->head == 0)
	return 0;
    else
    {
	int len = 0;
	rep_thread *ptr;
	for (ptr = root_barrier->head->next; ptr != 0; ptr = ptr->next)
	    len++;
	return len;
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
	 barrier != 0 && SP_NEWER_P ((char *) barrier, c->stack_bottom);
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
	 roots != 0 && SP_NEWER_P ((char *) roots, c->stack_bottom);
	 roots = FIXUP(rep_GC_root *, c, roots)->next)
    {
	repv *ptr = FIXUP(rep_GC_root *, c, roots)->ptr;
	rep_MARKVAL (*FIXUP(repv *, c, ptr));
    }
    for (nroots = c->gc_n_roots;
	 nroots != 0 && SP_NEWER_P ((char *) roots, c->stack_bottom);
	 nroots = FIXUP(rep_GC_n_roots *, c, nroots)->next)
    {
	repv *ptr = FIXUP(repv *, c, FIXUP(rep_GC_n_roots *, c, nroots)->first);
	int n = FIXUP(rep_GC_n_roots *, c, nroots)->count, i;
	for (i = 0; i < n; i++)
	    rep_MARKVAL (ptr[i]);
    }
    for (calls = c->call_stack;
	 calls != 0 && SP_NEWER_P ((char *) calls, c->stack_bottom);
	 calls = FIXUP(struct rep_Call *, c, calls)->next)
    {
	struct rep_Call *lc = FIXUP(struct rep_Call *, c, calls);
	rep_MARKVAL(lc->fun);
	rep_MARKVAL(lc->args);
	rep_MARKVAL(lc->saved_env);
	rep_MARKVAL(lc->saved_special_env);
	rep_MARKVAL(lc->saved_fh_env);
    }
    for (matches = c->regexp_data;
	 matches != 0 && SP_NEWER_P ((char *) matches, c->stack_bottom);
	 matches = FIXUP(struct rep_saved_regexp_data *, c, matches)->next)
    {
	struct rep_saved_regexp_data *sd
	    = FIXUP(struct rep_saved_regexp_data *, c, matches);
	if(sd->type == rep_reg_obj)
	{
	    int i;
	    for(i = 0; i < NSUBEXP; i++)
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

static void
mark_thread (repv obj)
{
    rep_MARKVAL (rep_VAL (THREAD (obj)->cont));
    rep_MARKVAL (THREAD (obj)->env);
    rep_MARKVAL (THREAD (obj)->special_env);
    rep_MARKVAL (THREAD (obj)->fh_env);
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
    rep_stream_puts (stream, "#<thread>", -1, rep_FALSE);
}


/* misc lisp functions */

static void
call_with_inwards (void *data_)
{
    repv *data = data_;
    printf ("binding %d\n", data[0]);
    if (data[0] != rep_NULL)
	data[1] = rep_bind_object (data[0]);
    else
	data[1] = rep_NULL;
}

static void
call_with_outwards (void *data_)
{
    repv *data = data_;
    printf ("unbinding %d\n", data[1]);
    if (data[1] != rep_NULL)
    {
	rep_unbind_object (data[1]);
	data[1] = rep_NULL;
    }
}

DEFUN("call-with-object", Fcall_with_object,
      Scall_with_object, (repv arg, repv thunk), rep_Subr2) /*
::call-with-object::
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
    data[1] = rep_bind_object(data[0]);
    if (data[1] != rep_NULL)
    {
	repv ret;
	rep_GC_n_roots gc_data;
	rep_PUSHGCN (gc_data, data, 2);
	ret = rep_call_with_barrier (rep_call_lisp0, thunk,
				     rep_FALSE, call_with_inwards,
				     call_with_outwards, data);
	rep_unbind_object (data[1]);
	rep_POPGCN;
	return ret;
    }
    else
	return rep_NULL;
}

DEFUN("call-with-dynamic-root", Fcall_with_dynamic_root,
      Scall_with_dynamic_root, (repv thunk), rep_Subr1) /*
::doc:call-with-dynamic-root::
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
::doc:call-with-barrier::
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

DEFUN("make-thread", Fmake_thread, Smake_thread, (repv thunk), rep_Subr1) /*
::doc:make-thread::
make-thread THUNK

Create and return an object representing a new thread of execution. The
new thread will begin by calling THUNK, a function with zero
parameters.
::end:: */
{
    return rep_VAL (make_thread (thunk));
}

DEFUN("thread-yield", Fthread_yield, Sthread_yield, (void), rep_Subr0) /*
::doc:thread-yield::
thread-yield

Pass control away from the current thread if other threads are waiting
to run.
::end:: */
{
    thread_yield ();
    return Qnil;
}

DEFUN("thread-delete", Fthread_delete, Sthread_delete, (repv th), rep_Subr1) /*
::doc:thread-delete::
thread-delete [THREAD]

Mark THREAD (or the current thread), as being deleted. It will not be
switched to in the future. If the current thread is deleted, control
will be passed to the next runnable thread. Deleting the last runnable
thread results in undefined behaviour.
::end:: */
{
    if (th == Qnil)
	th = Fcurrent_thread ();
    rep_DECLARE1 (th, THREADP);
    thread_delete (THREAD (th));
    return Qnil;
}

DEFUN("thread-suspend", Fthread_suspend,
      Sthread_suspend, (repv th, repv msecs), rep_Subr2) /*
::doc:thread-suspend::
thread-suspend [THREAD] [MSECS]

Mark THREAD (or the current thread) as being suspended. It will not be
selected until it has this status removed. Suspending the current
thread will pass control to the next runnable thread. Suspending the
last runnable thread results in undefined behaviour.
::end:: */
{
    if (th == Qnil)
	th = Fcurrent_thread ();
    rep_DECLARE1 (th, THREADP);
    thread_suspend (THREAD (th), rep_INTP (msecs) ? rep_INT (msecs) : 0);
    return Qnil;
}

DEFUN("thread-wake", Fthread_wake, Sthread_wake, (repv th), rep_Subr1) /*
::doc:thread-wake::
thread-wake [THREAD]

If THREAD (or the current thread) is currently suspended, mark it as
being runnable once more.
::end:: */
{
    if (th == Qnil)
	th = Fcurrent_thread ();
    rep_DECLARE1 (th, THREADP);
    thread_wake (THREAD (th));
    return Qnil;
}

DEFUN("threadp", Fthreadp, Sthreadp, (repv arg), rep_Subr1) /*
::doc:threadp::
threadp ARG

Return `t' if ARG is a thread object.
::end:: */
{
    return THREADP (arg) ? Qt : Qnil;
}

DEFUN("thread-suspended-p", Fthread_suspended_p,
      Sthread_suspended_p, (repv th), rep_Subr1) /*
::doc:thread-suspended-p::
thread-suspended-p THREAD

Return `t' if THREAD is currently suspended from running.
::end:: */
{
    rep_DECLARE1 (th, THREADP);
    return (THREAD (th)->car & TF_SUSPENDED) ? Qt : Qnil;
}

DEFUN("current-thread", Fcurrent_thread, Scurrent_thread, (void), rep_Subr0) /*
::doc:current-thread::
current-thread

Return the currently executing thread, or `nil' if threaded execution
is not currently taking place.
::end:: */
{
    return ((root_barrier && root_barrier->active)
	    ? rep_VAL (root_barrier->active) : Qnil);
}

DEFUN("thread-queue-length", Fthread_queue_length,
      Sthread_queue_length, (void), rep_Subr0) /*
::doc:thread-queue-length::
thread-queue-length

Returns the number of threads waiting to run in the current execution
environment (exclusing any currently running threads).
::end:: */
{
    return rep_MAKE_INT (thread_queue_length ());
}

DEFUN("thread-forbid", Fthread_forbid, Sthread_forbid, (void), rep_Subr0) /*
::doc:thread-forbid::
thread-forbid

Increment the thread preemption lock. When greather than zero all
preemption of threads is disabled. Returns `t' if preemption is blocked
as this function returns.
::end:: */
{
    rep_thread_lock++;
    return rep_thread_lock > 0 ? Qt : Qnil;
}

DEFUN("thread-permit", Fthread_permit, Sthread_permit, (void), rep_Subr0) /*
::doc:thread-permit::
thread-permit

Decrement the thread preemption lock. When greather than zero all
preemption of threads is disabled. Returns `t' if preemption is blocked
as this function returns.
::end:: */
{
    rep_thread_lock--;
    return rep_thread_lock > 0 ? Qt : Qnil;
}


/* dl hooks */

void
rep_continuations_init (void)
{
    rep_continuation_type = rep_register_new_type ("continuation",
						   rep_ptr_cmp,
						   print_cont, print_cont, 
						   sweep_cont, mark_cont,
						   mark_all, 0, 0, 0, 0, 0, 0);
    rep_thread_type = rep_register_new_type ("thread", rep_ptr_cmp,
					     print_thread, print_thread, 
					     sweep_thread, mark_thread,
					     0, 0, 0, 0, 0, 0, 0);
    rep_INTERN(continuation);
    rep_ADD_SUBR(Scall_cc);
    rep_ADD_SUBR(Sprimitive_invoke_continuation);
    rep_ADD_SUBR(Scontinuation_callable_p);
    rep_ADD_SUBR(Scall_with_object);
    rep_ADD_SUBR(Scall_with_dynamic_root);
    rep_ADD_SUBR(Scall_with_barrier);
    rep_ADD_SUBR(Smake_thread);
    rep_ADD_SUBR(Sthread_yield);
    rep_ADD_SUBR(Sthread_delete);
    rep_ADD_SUBR(Sthread_suspend);
    rep_ADD_SUBR(Sthread_wake);
    rep_ADD_SUBR(Sthreadp);
    rep_ADD_SUBR(Sthread_suspended_p);
    rep_ADD_SUBR(Scurrent_thread);
    rep_ADD_SUBR(Sthread_queue_length);
    rep_ADD_SUBR(Sthread_forbid);
    rep_ADD_SUBR(Sthread_permit);
}
