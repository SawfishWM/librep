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

   Note that continuations only save and restore variables bindings
   (both lexical and dynamic). It doesn't make sense to save other
   dynamic state (i.e. catch/throw, unwind-protect, etc..), and I
   haven't worked out how to do it anyway...

   Hopefully this will be reasonably portable, I _think_ it only
   depends on having a linear stack that completely encapsulates the
   current process state, and a setjmp/longjmp implementation..   */

#define _GNU_SOURCE

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

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

#if STACK_DIRECTION == 0
# error "stack growth direction unknown"
#elif STACK_DIRECTION > 0
# warning "upward growing stacks are untested"
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

/* The data saved for a continuation */
typedef struct rep_continuation {
    repv car;
    struct rep_continuation *next;
    jmp_buf jmpbuf;
    char *stack_copy, *stack_top;
    size_t stack_size;
    struct rep_Call *call_stack;
    repv special_bindings;
    rep_GC_root *gc_roots;
    rep_GC_n_roots *gc_n_roots;
    struct rep_saved_regexp_data *regexp_data;
    struct blocked_op *blocked_ops[op_MAX];
    repv throw_value;
    rep_bool single_step;
    int lisp_depth;
    repv ret_value;
} rep_continuation;

#define rep_CONTIN(v)		((rep_continuation *)rep_PTR(v))
#define rep_CONTINP(v)		rep_CELL16_TYPEP(v, rep_continuation_type)

/* the cell16 typecode allocated for continuation objects */
static int rep_continuation_type;

/* list of all allocated continuations */
static rep_continuation *continuations;

DEFSYM(continuation, "continuation");
DEFSYM(callcc, "callcc");

/* used while longjmp'ing to save accessing a local variable */
static rep_continuation *invoked_continuation;

/* Approx. number of extra bytes of stack per recursion */
#define STACK_GROWTH 512

/* save the original stack for continuation C */
static void
save_stack (rep_continuation *c)
{
    FLUSH_REGISTER_WINDOWS;

#if STACK_DIRECTION < 0
    c->stack_size = rep_stack_bottom - c->stack_top;
#else
    c->stack_size = c->stack_top - rep_stack_bottom;
#endif
    c->stack_copy = rep_alloc (c->stack_size);
    rep_data_after_gc += c->stack_size;
#if STACK_DIRECTION < 0
    memcpy (c->stack_copy, c->stack_top, c->stack_size);
#else
    memcpy (c->stack_copy, rep_stack_bottom, c->stack_size);
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
       below the current position */

#if STACK_DIRECTION < 0
    memcpy (c->stack_top, c->stack_copy, c->stack_size);
#else
    memcpy (rep_stack_bottom, c->stack_copy, c->stack_size);
#endif

    longjmp (c->jmpbuf, 1);
}

/* The continuations passed in from Fcall_cc () are actually closures
   around this subr. They have Qcontinuation bound to the primitive
   continuation object in their lexical environment */
DEFUN("primitive-invoke-continuation", Fprimitive_invoke_continuation,
      Sprimitive_invoke_continuation, (repv ret), rep_Subr1)
{
    repv cont = Fsymbol_value (Qcontinuation, Qnil);
    char water_mark;

    if (cont == rep_NULL || !rep_CONTINP(cont))
    {
	return Fsignal (Qerror, rep_LIST_1 (rep_string_dup
					    ("bad continuation")));
    }

    /* save the return value and recurse up the stack until there's
       room to invoke the continuation. Note that invoking this subr
       will already have restored the original environment since the
       call to Fmake_closure () will have saved its old state.. */

    rep_CONTIN(cont)->ret_value = ret;
    invoked_continuation = rep_CONTIN(cont);

    DB (("invoke: calling continuation %p\n", rep_CONTIN(cont)));
    grow_stack_and_invoke (rep_CONTIN(cont), &water_mark);

    /* not reached */
    return Qnil;
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
    rep_continuation *c;
    repv ret;

    c = rep_ALLOC_CELL (sizeof (rep_continuation));
    rep_data_after_gc += sizeof (rep_continuation);
    c->next = continuations;
    continuations = c;
    c->car = rep_continuation_type;

    if (setjmp (c->jmpbuf))
    {
	/* back from call/cc */

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

	ret = c->ret_value;
	c->ret_value = Qnil;
    }
    else
    {
	/* into call/cc */
	repv proxy;

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

	save_stack (c);

	DB (("call/cc: saved %p; stack_size=%lu\n", c, c->stack_size));

	proxy = Fmake_closure (rep_VAL(&Sprimitive_invoke_continuation), Qnil);
	rep_FUNARG(proxy)->env = Fcons (Fcons (Qcontinuation, rep_VAL(c)),
					rep_FUNARG(proxy)->env);
	ret = rep_call_lisp1 (fun, proxy);
    }

    return ret;
}

#if STACK_DIRECTION < 0
static inline char *
fixup (char *addr, rep_continuation *c)
{
    if (c->stack_top > c->stack_copy)
	return (addr - c->stack_top) + c->stack_copy;
    else
	return (addr - c->stack_copy) + c->stack_top;
}
#else
static inline char *
fixup (char *addr, unsigned rep_PTR_SIZED_INT diff)
{
    if (c->stack_top > c->stack_copy)
	return (addr - rep_stack_bottom) + c->stack_copy;
    else
	return (addr - c->stack_copy) + rep_stack_bottom;
}
#endif

static void
mark (repv obj)
{
    rep_GC_root *roots;
    rep_GC_n_roots *nroots;
    struct rep_Call *calls;
    struct rep_saved_regexp_data *matches;

#define FIXUP(t,addr) ((t) (fixup ((char *) (addr), (c))))

    rep_continuation *c = rep_CONTIN (obj);
    rep_MARKVAL (c->throw_value);
    rep_MARKVAL (c->special_bindings);

    for (roots = c->gc_roots; roots != 0;
	 roots = FIXUP(rep_GC_root *, roots)->next)
    {
	repv *ptr = FIXUP(rep_GC_root *, roots)->ptr;
	rep_MARKVAL (*FIXUP(repv *, ptr));
    }
    for (nroots = c->gc_n_roots; nroots != 0;
	 nroots = FIXUP(rep_GC_n_roots *, nroots)->next)
    {
	repv *ptr = FIXUP(repv *, FIXUP(rep_GC_n_roots *, nroots)->first);
	int n = FIXUP(rep_GC_n_roots *, nroots)->count, i;
	for (i = 0; i < n; i++)
	    rep_MARKVAL (ptr[i]);
    }
    for (calls = c->call_stack; calls != 0;
	 calls = FIXUP(struct rep_Call *, calls)->next)
    {
	struct rep_Call *lc = FIXUP(struct rep_Call *, calls);
	rep_MARKVAL(lc->fun);
	rep_MARKVAL(lc->args);
	rep_MARKVAL(lc->saved_env);
	rep_MARKVAL(lc->saved_special_env);
	rep_MARKVAL(lc->saved_fh_env);
    }
    for (matches = c->regexp_data; matches != 0;
	 matches = FIXUP(struct rep_saved_regexp_data *, matches)->next)
    {
	struct rep_saved_regexp_data *sd
	    = FIXUP(struct rep_saved_regexp_data *, matches);
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

#undef FIXUP

static void
sweep (void)
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
print (repv stream, repv obj)
{
    rep_stream_puts (stream, "#<continuation>", -1, rep_FALSE);
}


/* dl hooks */

repv
rep_dl_init (void)
{
    rep_continuation_type = rep_register_new_type ("continuation",
						   rep_ptr_cmp, print, print, 
						   sweep, mark,
						   0, 0, 0, 0, 0, 0, 0);
    rep_INTERN(continuation);
    rep_INTERN(callcc);
    rep_ADD_SUBR(Scall_cc);
    rep_ADD_SUBR(Sprimitive_invoke_continuation);
    return Qcallcc;
}
