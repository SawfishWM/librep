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

/* todo:

   There's an obvious optimisation to make -- only copy the stack if
   it's actually required. The only way it can be required is if either
   the creating call/cc exits, or if invoke-primitive-continuation is
   called. Either way the original stack contents are still available
   for copying

   There are a few wrinkles though.. e.g. call/cc has to check that the
   continuation hasn't been collected before copying in the stack.  */

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

/* The data saved for a continuation. Note that currently no attempt
   is made to save/restore special bindings (variables or objects); I
   can't think of an efficient way, and I'm not even sure it's possible
   or desirable..? */
typedef struct rep_continuation {
    repv car;
    struct rep_continuation *next;
    jmp_buf jmpbuf;
    char *stack_copy, *stack_top;
    size_t stack_size;
    struct rep_Call *call_stack;
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

/* rep_init () will set this to an early stack pointer */
char *rep_stack_bottom;

/* the cell16 typecode allocated for continuation objects */
int rep_continuation_type;

/* list of all allocated continuations */
static rep_continuation *continuations;

DEFSYM(continuation, "continuation");

/* used while longjmp'ing to save accessing a local variable */
static rep_continuation *invoked_continuation;

/* Extra space (in bytes) to allow at the top of the stack. This
   seems to help on sparcs at least, flushing reg. windows stomps
   on some of the stack I guess (but how much of the stack? dan thinks
   each window has 24 registers, each 64 bits, so 192 bytes?) */
#define STACK_SLOP 256

/* Approx. number of extra bytes of stack per recursion */
#define STACK_GROWTH 512

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

    FLUSH_REGISTER_WINDOWS;		/* XXX necessary? */

    /* stack's big enough now, so reload the saved copy somewhere
       below the current position */

#if STACK_DIRECTION < 0
    memcpy (c->stack_top, c->stack_copy, c->stack_size);
#else
    memcpy (rep_stack_bottom, c->stack_copy, c->stack_size);
#endif

    /* save the continuation where it can be found, then
       longjmp back to Fcall_cc () */

    invoked_continuation = c;
    FLUSH_REGISTER_WINDOWS;		/* XXX necessary? */
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
    grow_stack_and_invoke (rep_CONTIN(cont), &water_mark);

    /* not reached */
    return Qnil;
}

/* save the original stack for continuation C */
static void
save_stack (rep_continuation *c)
{
    int dummy;
    char *stack_top = (char *) &dummy;

    FLUSH_REGISTER_WINDOWS;

    c->stack_top = stack_top;
#if STACK_DIRECTION < 0
    c->stack_size = rep_stack_bottom - stack_top;
#else
    c->stack_size = stack_top - rep_stack_bottom;
#endif
    c->stack_copy = rep_alloc (c->stack_size);
    rep_data_after_gc += c->stack_size;
#if STACK_DIRECTION < 0
    memcpy (c->stack_copy, stack_top, c->stack_size);
#else
    memcpy (c->stack_copy, rep_stack_bottom, c->stack_size);
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

    c = rep_ALLOC_CELL (sizeof (rep_continuation));
    rep_data_after_gc += sizeof (rep_continuation);
    c->next = continuations;
    continuations = c;
    c->car = rep_continuation_type;

    c->call_stack = rep_call_stack;
    c->gc_roots = rep_gc_root_stack;
    c->gc_n_roots = rep_gc_n_roots_stack;
    c->regexp_data = rep_saved_matches;
    memcpy (c->blocked_ops, rep_blocked_ops, sizeof (c->blocked_ops));
    c->throw_value = rep_throw_value;
    c->single_step = rep_single_step_flag;
    c->lisp_depth = rep_lisp_depth;

    if (setjmp (c->jmpbuf))
    {
	/* back from call/cc */
	repv ret;

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
	rep_call_stack = c->call_stack;

	ret = c->ret_value;
	c->ret_value = Qnil;
	return ret;
    }
    else
    {
	/* into call/cc */
	repv proxy;

	/* copy the stack contents, including this frame */
	save_stack (c);

	proxy = Fmake_closure (rep_VAL(&Sprimitive_invoke_continuation), Qnil);
	rep_FUNARG(proxy)->env = Fcons (Fcons (Qcontinuation, rep_VAL(c)),
					rep_FUNARG(proxy)->env);
	return rep_call_lisp1 (fun, proxy);
    }
}

#if STACK_DIRECTION < 0
static char *
fixup (char *addr, rep_continuation *c)
{
    if (c->stack_top > c->stack_copy)
	return (addr - c->stack_top) + c->stack_copy;
    else
	return (addr - c->stack_copy) + c->stack_top;
}
#else
static char *
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


/* continuations */

void
rep_continuations_init (void)
{
    rep_continuation_type = rep_register_new_type ("continuation",
						   rep_ptr_cmp, print, print, 
						   sweep, mark,
						   0, 0, 0, 0, 0, 0, 0);
    rep_INTERN(continuation);
    rep_ADD_SUBR(Scall_cc);
    rep_ADD_SUBR(Sprimitive_invoke_continuation);
}

void
rep_continuations_kill (void)
{
}
