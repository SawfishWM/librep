/* lispmach.c -- Interpreter for compiled Lisp forms
   Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

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

/* Define this to get a list of byte-code/frequency of use */
#undef BYTE_CODE_HISTOGRAM

#include "jade.h"
#include <lib/jade_protos.h>
#include "bytecodes.h"

#include <assert.h>

_PR void lispmach_init(void);
_PR void lispmach_kill(void);

_PR VALUE sym_bytecode_error;
DEFSYM(bytecode_error, "bytecode-error");
DEFSTRING(err_bytecode_error, "Invalid byte code version");
DEFSTRING(unknown_op, "Unknown lisp opcode");

#ifdef BYTE_CODE_HISTOGRAM
static u_long byte_code_usage[256];
#endif

/* Unbind one level of the BIND-STACK and return the new head of the stack.
   Each item in the BIND-STACK may be one of:
	(t . FORM)
		unwind-protect FORM to always evaluate
	(BUFFER . VIEW)
		install BUFFER as current in VIEW
	((SYM . OLD-VAL) ...)
		list of symbol bindings to undo with unbind_symbols()
	(VIEW . WINDOW)
		set VIEW as current in its window
	WINDOW
		set WINDOW as current.
	(PC . STACK-DEPTH)
		not unbound here; install exception handler at PC */
static inline VALUE
unbind_one_level(VALUE bind_stack)
{
    if(CONSP(bind_stack))
    {
	VALUE item = VCAR(bind_stack);
	if(CONSP(item))
	{
	    if(VCAR(item) == sym_t)
	    {
		/* unwind-protect protection forms. */
		cmd_eval(VCDR(item));
	    }
	    else if(BUFFERP(VCAR(item)) && VIEWP(VCDR(item)))
	    {
		/* (BUFFER . VIEW)
		   reinstall BUFFER in VIEW */
		swap_buffers(VVIEW(VCDR(item)), VTX(VCAR(item)));
	    }
	    else if(CONSP(VCAR(item)))
	    {
		/* A set of symbol bindings (let or let*). */
		unbind_symbols(item);
	    }
	    else if(VIEWP(VCAR(item)) && WINDOWP(VCDR(item)))
	    {
		/* (VIEW . WINDOW) */
		VW *vw = VVIEW(VCAR(item));
		WIN *win = VWIN(VCDR(item));
		if(vw->vw_Win && vw->vw_Win->w_Window != WINDOW_NIL)
		{
		    vw->vw_Win->w_CurrVW = vw;
		    curr_win = win;
		    curr_vw = curr_win->w_CurrVW;
		}
	    }
	}
	else if(WINDOWP(item))
	{
	    /* Reinstall WINDOW */
	    if(VWIN(item)->w_Window != WINDOW_NIL)
	    {
		curr_win = VWIN(item);
		curr_vw = curr_win->w_CurrVW;
	    }
	}
	return VCDR(bind_stack);
    }
    return sym_nil;
}

#define TOP	    (*stackp)
#define RET_POP	    (*stackp--)
#define POP	    (stackp--)
#define POPN(n)	    (stackp -= n)
#define PUSH(v)	    (*(++stackp) = (v))
#define STK_USE	    (stackp - (stackbase - 1))

#define FETCH	    (*pc++)
#define FETCH2	    ((FETCH << ARG_SHIFT) | FETCH)

/* These macros pop as many args as required then call the specified
   function properly. */

#define CALL_1(cmd)				\
    if((TOP = cmd (TOP)))			\
	break;					\
    goto error
    
#define CALL_2(cmd)				\
    tmp = RET_POP;				\
    if((TOP = cmd (TOP, tmp)))			\
	break;					\
    goto error

#define CALL_3(cmd)				\
    tmp = RET_POP;				\
    tmp2 = RET_POP;				\
    if((TOP = cmd (TOP, tmp2, tmp)))		\
	break;					\
    goto error

/* Output the case statement for an instruction OP, with an embedded
   argument. The code for the instruction should start at the following
   piece of code. */
#define CASE_OP_ARG(op)							\
	case op+7:							\
	    arg = FETCH2; goto CONCAT(op_, op);				\
	case op: case op+1: case op+2: case op+3: case op+4: case op+5:	\
	    arg = c - op; goto CONCAT(op_, op);				\
	case op+6:							\
	    arg = FETCH;						\
	CONCAT(op_, op):

_PR VALUE cmd_jade_byte_code(VALUE code, VALUE consts, VALUE stkreq);
DEFUN("jade-byte-code", cmd_jade_byte_code, subr_jade_byte_code, (VALUE code, VALUE consts, VALUE stkreq), V_Subr3, DOC_jade_byte_code) /*
::doc:jade_byte_code::
jade-byte-code CODE-STRING CONST-VEC MAX-STACK

Evaluates the string of byte codes CODE-STRING, the constants that it
references are contained in the vector CONST-VEC. MAX-STACK is a number
defining how much stack space is required to evaluate the code.

Do *not* attempt to call this function manually, the lisp file `compiler.jl'
contains a simple compiler which translates files of lisp forms into files
of byte code. See the functions `compile-file', `compile-directory' and
`compile-lisp-lib' for more details.
::end:: */
{
    VALUE *stackbase;
    register VALUE *stackp;
    /* This holds a list of sets of bindings, it can also hold the form of
       an unwind-protect that always gets eval'd (when the car is t).  */
    VALUE bindstack = sym_nil;
    register u_char *pc;
    u_char c;
    GC_root gc_code, gc_consts, gc_bindstack;
    /* The `gcv_N' field is only filled in with the stack-size when there's
       a chance of gc.	*/
    GC_n_roots gc_stackbase;

    DECLARE1(code, STRINGP);
    DECLARE2(consts, VECTORP);
    DECLARE3(stkreq, INTP);

    stackbase = alloca(sizeof(VALUE) * VINT(stkreq));

    stackp = stackbase - 1;
    PUSHGC(gc_code, code);
    PUSHGC(gc_consts, consts);
    PUSHGC(gc_bindstack, bindstack);
    PUSHGCN(gc_stackbase, stackbase, 0);

    pc = VSTR(code);
fetch:
    while((c = FETCH) != 0)
    {
#ifdef BYTE_CODE_HISTOGRAM
	byte_code_usage[c]++;
#endif
	switch(c)
	{
	    u_short arg;
	    VALUE tmp, tmp2;

	CASE_OP_ARG(OP_CALL)
#ifdef MINSTACK
	    if(STK_SIZE <= MINSTACK)
	    {
		STK_WARN("lisp-code");
		TOP = cmd_signal(sym_stack_error, sym_nil);
		goto quit;
	    }
#endif
	    /* args are still available above the top of the stack,
	       this just makes things a bit easier. */
	    POPN(arg);
	    tmp = TOP;
	    if(SYMBOLP(tmp))
	    {
		if(VSYM(tmp)->car & SF_DEBUG)
		    single_step_flag = TRUE;
		if(!(tmp = cmd_symbol_function(tmp, sym_nil)))
		    goto error;
	    }
	    gc_stackbase.count = STK_USE;
	    switch(VTYPE(tmp))
	    {
	    case V_Subr0:
		TOP = VSUBR0FUN(tmp)();
		break;
	    case V_Subr1:
		TOP = VSUBR1FUN(tmp)(arg >= 1 ? stackp[1] : sym_nil);
		break;
	    case V_Subr2:
		switch(arg)
		{
		case 0:
		    TOP = VSUBR2FUN(tmp)(sym_nil, sym_nil);
		    break;
		case 1:
		    TOP = VSUBR2FUN(tmp)(stackp[1], sym_nil);
		    break;
		default:
		    TOP = VSUBR2FUN(tmp)(stackp[1], stackp[2]);
		    break;
		}
		break;
	    case V_Subr3:
		switch(arg)
		{
		case 0:
		    TOP = VSUBR3FUN(tmp)(sym_nil, sym_nil, sym_nil);
		    break;
		case 1:
		    TOP = VSUBR3FUN(tmp)(stackp[1], sym_nil, sym_nil);
		    break;
		case 2:
		    TOP = VSUBR3FUN(tmp)(stackp[1], stackp[2], sym_nil);
		    break;
		default:
		    TOP = VSUBR3FUN(tmp)(stackp[1], stackp[2], stackp[3]);
		    break;
		}
		break;
	    case V_Subr4:
		switch(arg)
		{
		case 0:
		    TOP = VSUBR4FUN(tmp)(sym_nil, sym_nil,
					 sym_nil, sym_nil);
		    break;
		case 1:
		    TOP = VSUBR4FUN(tmp)(stackp[1], sym_nil,
					 sym_nil, sym_nil);
		    break;
		case 2:
		    TOP = VSUBR4FUN(tmp)(stackp[1], stackp[2],
					 sym_nil, sym_nil);
		    break;
		case 3:
		    TOP = VSUBR4FUN(tmp)(stackp[1], stackp[2],
					 stackp[3], sym_nil);
		    break;
		default:
		    TOP = VSUBR4FUN(tmp)(stackp[1], stackp[2],
					 stackp[3], stackp[4]);
		    break;
		}
		break;
	    case V_Subr5:
		switch(arg)
		{
		case 0:
		    TOP = VSUBR5FUN(tmp)(sym_nil, sym_nil, sym_nil,
					 sym_nil, sym_nil);
		    break;
		case 1:
		    TOP = VSUBR5FUN(tmp)(stackp[1], sym_nil, sym_nil,
					 sym_nil, sym_nil);
		    break;
		case 2:
		    TOP = VSUBR5FUN(tmp)(stackp[1], stackp[2], sym_nil,
					 sym_nil, sym_nil);
		    break;
		case 3:
		    TOP = VSUBR5FUN(tmp)(stackp[1], stackp[2], stackp[3],
					 sym_nil, sym_nil);
		    break;
		case 4:
		    TOP = VSUBR5FUN(tmp)(stackp[1], stackp[2], stackp[3],
					 stackp[4], sym_nil);
		default:
		    TOP = VSUBR5FUN(tmp)(stackp[1], stackp[2], stackp[3],
					 stackp[4], stackp[5]);
		    break;
		}
		break;
	    case V_SubrN:
		tmp2 = sym_nil;
		POPN(-arg); /* reclaim my args */
		while(arg--)
		    tmp2 = cmd_cons(RET_POP, tmp2);
		TOP = VSUBRNFUN(tmp)(tmp2);
		break;

	    case V_Cons:
		tmp2 = sym_nil;
		POPN(-arg);
		while(arg--)
		    tmp2 = cmd_cons(RET_POP, tmp2);
		if(VCAR(tmp) == sym_lambda)
		{
		    struct Lisp_Call lc;
		    lc.next = lisp_call_stack;
		    lc.fun = TOP;
		    lc.args = tmp2;
		    lc.args_evalled_p = sym_t;
		    lisp_call_stack = &lc;
		    TOP = eval_lambda(tmp, tmp2, FALSE);
		    lisp_call_stack = lc.next;
		}
		else if(VCAR(tmp) == sym_autoload)
		    /* I can't be bothered to go to all the hassle
		       of doing this here, it's going to be slow
		       anyway so just pass it to funcall.  */
		    TOP = funcall(TOP, tmp2, FALSE);
		else
		    goto invalid;
		break;

	    case V_Compiled:
		tmp2 = sym_nil;
		POPN(-arg);
		while(arg--)
		    tmp2 = cmd_cons(RET_POP, tmp2);
		if(!COMPILED_MACRO_P(tmp))
		{
		    VALUE bindings;
		    struct Lisp_Call lc;
		    lc.next = lisp_call_stack;
		    lc.fun = TOP;
		    lc.args = tmp2;
		    lc.args_evalled_p = sym_t;
		    lisp_call_stack = &lc;

		    bindings = bindlambdalist(COMPILED_LAMBDA(tmp),
					      tmp2, FALSE);
		    if(bindings != LISP_NULL)
		    {
			GC_root gc_bindings;
			PUSHGC(gc_bindings, bindings);
			TOP = cmd_jade_byte_code(COMPILED_CODE(tmp),
						 COMPILED_CONSTANTS(tmp),
						 MAKE_INT(COMPILED_STACK(tmp)));
			POPGC;
			unbind_symbols(bindings);
		    }
		    else
			goto error;
		    lisp_call_stack = lc.next;
		}
		else
		    goto invalid;
		break;
		    
	    default: invalid:
		cmd_signal(sym_invalid_function, LIST_1(TOP));
		goto error;
	    }
	    if(!TOP)
		goto error;
	    break;

	CASE_OP_ARG(OP_PUSH)
	    PUSH(VVECT(consts)->array[arg]);
	    break;

	CASE_OP_ARG(OP_REFQ)
	    if(PUSH(cmd_symbol_value(VVECT(consts)->array[arg],
				     sym_nil)))
	    {
		break;
	    }
	    goto error;

	CASE_OP_ARG(OP_SETQ)
	    if((TOP = cmd_set(VVECT(consts)->array[arg], TOP)))
		break;
	    goto error;

	CASE_OP_ARG(OP_LIST)
	    tmp = sym_nil;
	    while(arg--)
		tmp = cmd_cons(RET_POP, tmp);
	    PUSH(tmp);
	    break;

	CASE_OP_ARG(OP_BIND)
	    tmp = VVECT(consts)->array[arg];
	    if(SYMBOLP(tmp))
	    {
		VCAR(bindstack) = bind_symbol(VCAR(bindstack), tmp, RET_POP);
		break;
	    }
	    signal_arg_error(tmp, 1);
	    goto error;

	case OP_REF:
	    if((TOP = cmd_symbol_value(TOP, sym_nil)))
		break;
	    goto error;

	case OP_SET:
	    CALL_2(cmd_set);

	case OP_FREF:
	    if((TOP = cmd_symbol_function(TOP, sym_nil)))
		break;
	    goto error;

	case OP_FSET:
	    CALL_2(cmd_fset);

	case OP_INIT_BIND:
	    bindstack = cmd_cons(sym_nil, bindstack);
	    break;

	case OP_UNBIND:
	    gc_stackbase.count = STK_USE;
	    bindstack = unbind_one_level(bindstack);
	    break;

	case OP_DUP:
	    tmp = TOP;
	    PUSH(tmp);
	    break;

	case OP_SWAP:
	    tmp = TOP;
	    TOP = stackp[-1];
	    stackp[-1] = tmp;
	    break;

	case OP_POP:
	    POP;
	    break;

	case OP_NIL:
	    PUSH(sym_nil);
	    break;

	case OP_T:
	    PUSH(sym_t);
	    break;

	case OP_CONS:
	    CALL_2(cmd_cons);

	case OP_CAR:
	    tmp = TOP;
	    if(CONSP(tmp))
		TOP = VCAR(tmp);
	    else
		TOP = sym_nil;
	    break;

	case OP_CDR:
	    tmp = TOP;
	    if(CONSP(tmp))
		TOP = VCDR(tmp);
	    else
		TOP = sym_nil;
	    break;

	case OP_RPLACA:
	    CALL_2(cmd_rplaca);

	case OP_RPLACD:
	    CALL_2(cmd_rplacd);

	case OP_NTH:
	    CALL_2(cmd_nth);

	case OP_NTHCDR:
	    CALL_2(cmd_nthcdr);

	case OP_ASET:
	    CALL_3(cmd_aset);

	case OP_AREF:
	    CALL_2(cmd_aref);

	case OP_LENGTH:
	    CALL_1(cmd_length);

	case OP_EVAL:
	    gc_stackbase.count = STK_USE;
	    CALL_1(cmd_eval);

	case OP_ADD:
	    tmp = RET_POP;
	    if(INTP(tmp) && INTP(TOP))
	    {
		TOP = MAKE_INT(VINT(TOP) + VINT(tmp));
		break;
	    }
	    if(INTP(tmp))
		signal_arg_error(TOP, 2);
	    else
		signal_arg_error(tmp, 1);
	    goto error;

	case OP_NEG:
	    if(INTP(TOP))
	    {
		TOP = MAKE_INT(-VINT(TOP));
		break;
	    }
	    signal_arg_error(TOP, 1);
	    goto error;

	case OP_SUB:
	    tmp = RET_POP;
	    if(INTP(tmp) && INTP(TOP))
	    {
		TOP = MAKE_INT(VINT(TOP) - VINT(tmp));
		break;
	    }
	    if(INTP(tmp))
		signal_arg_error(TOP, 2);
	    else
		signal_arg_error(tmp, 1);
	    goto error;

	case OP_MUL:
	    tmp = RET_POP;
	    if(INTP(tmp) && INTP(TOP))
	    {
		TOP = MAKE_INT(VINT(TOP) * VINT(tmp));
		break;
	    }
	    if(INTP(tmp))
		signal_arg_error(TOP, 2);
	    else
		signal_arg_error(tmp, 1);
	    goto error;

	case OP_DIV:
	    tmp = RET_POP;
	    if(INTP(tmp) && INTP(TOP))
	    {
		TOP = MAKE_INT(VINT(TOP) / VINT(tmp));
		break;
	    }
	    if(INTP(tmp))
		signal_arg_error(TOP, 2);
	    else
		signal_arg_error(tmp, 1);
	    goto error;

	case OP_REM:
	    CALL_2(cmd_remainder);

	case OP_LNOT:
	    if(INTP(TOP))
	    {
		TOP = MAKE_INT(~VINT(TOP));
		break;
	    }
	    signal_arg_error(TOP, 1);
	    goto error;

	case OP_NOT:
	    if(TOP == sym_nil)
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_LOR:
	    tmp = RET_POP;
	    if(INTP(tmp) && INTP(TOP))
	    {
		TOP = MAKE_INT(VINT(TOP) | VINT(tmp));
		break;
	    }
	    if(INTP(tmp))
		signal_arg_error(TOP, 2);
	    else
		signal_arg_error(tmp, 1);
	    goto error;

	case OP_LXOR:
	    tmp = RET_POP;
	    if(INTP(tmp) && INTP(TOP))
	    {
		TOP = MAKE_INT(VINT(TOP) ^ VINT(tmp));
		break;
	    }
	    if(INTP(tmp))
		signal_arg_error(TOP, 2);
	    else
		signal_arg_error(tmp, 1);
	    goto error;

	case OP_LAND:
	    tmp = RET_POP;
	    if(INTP(tmp) && INTP(TOP))
	    {
		TOP = MAKE_INT(VINT(TOP) & VINT(tmp));
		break;
	    }
	    if(INTP(tmp))
		signal_arg_error(TOP, 2);
	    else
		signal_arg_error(tmp, 1);
	    goto error;

	case OP_EQUAL:
	    tmp = RET_POP;
	    if(!(VALUE_CMP(TOP, tmp)))
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_EQ:
	    tmp = RET_POP;
	    if(TOP == tmp)
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_NUM_EQ:
	    CALL_2(cmd_num_eq);

	case OP_NUM_NOTEQ:
	    CALL_2(cmd_num_noteq);

	case OP_GT:
	    tmp = RET_POP;
	    if(VALUE_CMP(TOP, tmp) > 0)
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_GE:
	    tmp = RET_POP;
	    if(VALUE_CMP(TOP, tmp) >= 0)
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_LT:
	    tmp = RET_POP;
	    if(VALUE_CMP(TOP, tmp) < 0)
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_LE:
	    tmp = RET_POP;
	    if(VALUE_CMP(TOP, tmp) <= 0)
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_INC:
	    if(INTP(TOP))
	    {
		TOP = MAKE_INT(VINT(TOP) + 1);
		break;
	    }
	    signal_arg_error(TOP, 1);
	    goto error;

	case OP_DEC:
	    if(INTP(TOP))
	    {
		TOP = MAKE_INT(VINT(TOP) - 1);
		break;
	    }
	    signal_arg_error(TOP, 1);
	    goto error;

	case OP_LSH:
	    CALL_2(cmd_lsh);

	case OP_ZEROP:
	    if(INTP(TOP) && (VINT(TOP) == 0))
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_NULL:
	    if(NILP(TOP))
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_ATOM:
	    if(!CONSP(TOP))
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_CONSP:
	    if(CONSP(TOP))
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_LISTP:
	    if(CONSP(TOP) || NILP(TOP))
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_NUMBERP:
	    if(INTP(TOP))
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_STRINGP:
	    if(STRINGP(TOP))
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_VECTORP:
	    if(VECTORP(TOP))
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_CATCH:
	    /* This takes two arguments, TAG and THROW-VALUE.
	       THROW-VALUE is the saved copy of throw_value,
	       if (car THROW-VALUE) == TAG we match, and we
	       leave two values on the stack, nil on top (to
	       pacify EJMP), (cdr THROW-VALUE) below that. */
	    tmp = RET_POP;		/* tag */
	    tmp2 = TOP;		/* throw_value */
	    if(CONSP(tmp2) && VCAR(tmp2) == tmp)
	    {
		TOP = VCDR(tmp2);	/* leave result at stk[1] */
		PUSH(sym_nil);		/* cancel error */
	    }
	    break;

	case OP_THROW:
	    tmp = RET_POP;
	    if(!throw_value)
		throw_value = cmd_cons(TOP, tmp);
	    /* This isn't really an error :-)  */
	    goto error;

	case OP_BINDERR:
	    /* Pop our single argument and cons it onto the bind-
	       stack in a pair with the current stack-pointer.
	       This installs an address in the code string as an
	       error handler. */
	    tmp = RET_POP;
	    bindstack = cmd_cons(cmd_cons(tmp, MAKE_INT(STK_USE)), bindstack);
	    break;

	case OP_FBOUNDP:
	    CALL_1(cmd_fboundp);

	case OP_BOUNDP:
	    CALL_1(cmd_boundp);

	case OP_SYMBOLP:
	    if(SYMBOLP(TOP))
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_GET:
	    CALL_2(cmd_get);

	case OP_PUT:
	    CALL_3(cmd_put);

	case OP_ERRORPRO:
	    /* This should be called with three values on the stack.
		1. conditions of the error handler
		2. throw_value of the exception
		3. symbol to bind the error data to (or nil)

	       This function pops (1) and tests it against the error
	       in (2). If they match it sets (2) to nil, and binds the
	       error data to the symbol in (3). */
	    tmp = RET_POP;
	    if(CONSP(TOP) && VCAR(TOP) == sym_error
	       && compare_error(VCDR(TOP), tmp))
	    {
		/* The handler matches the error. */
		tmp = VCDR(TOP);	/* the error data */
		tmp2 = stackp[-1];	/* the symbol to bind to */
		if(SYMBOLP(tmp2) && !NILP(tmp2))
		    bindstack = cmd_cons(bind_symbol(sym_nil, tmp2, tmp),
					 bindstack);
		else
		    /* Placeholder to allow simple unbinding */
		    bindstack = cmd_cons(sym_nil, bindstack);
		TOP = sym_nil;
	    }
	    break;

	case OP_SIGNAL:
	    gc_stackbase.count = STK_USE;
	    CALL_2(cmd_signal);

	case OP_REVERSE:
	    CALL_1(cmd_reverse);

	case OP_NREVERSE:
	    CALL_1(cmd_nreverse);

	case OP_ASSOC:
	    CALL_2(cmd_assoc);

	case OP_ASSQ:
	    CALL_2(cmd_assq);

	case OP_RASSOC:
	    CALL_2(cmd_rassoc);

	case OP_RASSQ:
	    CALL_2(cmd_rassq);

	case OP_LAST:
	    CALL_1(cmd_last);

	case OP_MAPCAR:
	    gc_stackbase.count = STK_USE;
	    CALL_2(cmd_mapcar);

	case OP_MAPC:
	    gc_stackbase.count = STK_USE;
	    CALL_2(cmd_mapc);

	case OP_MEMBER:
	    CALL_2(cmd_member);

	case OP_MEMQ:
	    CALL_2(cmd_memq);

	case OP_DELETE:
	    CALL_2(cmd_delete);

	case OP_DELQ:
	    CALL_2(cmd_delq);

	case OP_DELETE_IF:
	    gc_stackbase.count = STK_USE;
	    CALL_2(cmd_delete_if);

	case OP_DELETE_IF_NOT:
	    gc_stackbase.count = STK_USE;
	    CALL_2(cmd_delete_if_not);

	case OP_COPY_SEQUENCE:
	    CALL_1(cmd_copy_sequence);

	case OP_SEQUENCEP:
	    CALL_1(cmd_sequencep);

	case OP_FUNCTIONP:
	    CALL_1(cmd_functionp);

	case OP_SPECIAL_FORM_P:
	    CALL_1(cmd_special_form_p);

	case OP_SUBRP:
	    CALL_1(cmd_subrp);

	case OP_EQL:
	    tmp = RET_POP;
	    if(INTP(tmp) && INTP(TOP))
		TOP = (VINT(TOP) == VINT(tmp) ? sym_t : sym_nil);
	    else
		TOP = (TOP == tmp ? sym_t : sym_nil);
	    break;

	case OP_MAX:
	    tmp = RET_POP;
	    if(value_cmp(tmp, TOP) > 0)
		TOP = tmp;
	    break;

	case OP_MIN:
	    tmp = RET_POP;
	    if(value_cmp(tmp, TOP) < 0)
		TOP = tmp;
	    break;

	case OP_FILTER:
	    gc_stackbase.count = STK_USE;
	    CALL_2(cmd_filter);

	case OP_MACROP:
	    CALL_1(cmd_macrop);

	case OP_BYTECODEP:
	    CALL_1(cmd_bytecodep);

	case OP_SET_CURRENT_BUFFER:
	    CALL_2(cmd_set_current_buffer);

	case OP_BIND_BUFFER:
	    /* one arg: buffer. */
	    tmp = RET_POP;
	    if(!BUFFERP(tmp))
	    {
		signal_arg_error(tmp, 1);
		goto error;
	    }
	    tmp = VAL(swap_buffers(curr_vw, VTX(tmp)));
	    bindstack = cmd_cons(cmd_cons(tmp, VAL(curr_vw)), bindstack);
	    break;

	case OP_CURRENT_BUFFER:
	    CALL_1(cmd_current_buffer);

	case OP_BUFFERP:
	    if(BUFFERP(TOP))
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_MARKP:
	    if(MARKP(TOP))
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_WINDOWP:
	    if(WINDOWP(TOP))
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_BIND_WINDOW:
	    tmp = RET_POP;
	    if(!WINDOWP(tmp) || !VWIN(tmp)->w_Window)
	    {
		signal_arg_error(tmp, 1);
		goto error;
	    }
	    bindstack = cmd_cons(VAL(curr_win), bindstack);
	    curr_win = VWIN(tmp);
	    curr_vw = curr_win->w_CurrVW;
	    break;

	case OP_VIEWP:
	    if(VIEWP(TOP))
		TOP = sym_t;
	    else
		TOP = sym_nil;
	    break;

	case OP_BIND_VIEW:
	    tmp = RET_POP;
	    if(!VIEWP(tmp) || !VVIEW(tmp)->vw_Win
	       || !VVIEW(tmp)->vw_Win->w_Window)
	    {
		signal_arg_error(tmp, 1);
		goto error;
	    }
	    bindstack = cmd_cons(cmd_cons(VAL(VVIEW(tmp)->vw_Win->w_CurrVW),
					  VAL(curr_win)), bindstack);
	    curr_vw = VVIEW(tmp);
	    curr_win = VVIEW(tmp)->vw_Win;
	    curr_win->w_CurrVW = curr_vw;
	    break;

	case OP_CURRENT_VIEW:
	    CALL_1(cmd_current_view);
	    break;

	case OP_SWAP2:
	    tmp = TOP;
	    TOP = stackp[-1];
	    stackp[-1] = stackp[-2];
	    stackp[-2] = tmp;
	    break;

	case OP_MOD:
	    CALL_2(cmd_mod);

	case OP_POS:
	    CALL_2(cmd_pos);

	case OP_POSP:
	    TOP = (POSP(TOP) ? sym_t : sym_nil);
	    break;

	/* Jump instructions follow */

	case OP_EJMP:
	    /* Pop the stack; if it's nil jmp pc[0,1], otherwise
	       set throw_value=ARG and goto the error handler. */
	    tmp = RET_POP;
	    if(NILP(tmp))
		goto do_jmp;
	    throw_value = tmp;
	    goto error;

	case OP_JN:
	    if(NILP(RET_POP))
		goto do_jmp;
	    pc += 2;
	    break;

	case OP_JT:
	    if(!NILP(RET_POP))
		goto do_jmp;
	    pc += 2;
	    break;

	case OP_JPN:
	    if(NILP(TOP))
	    {
		POP;
		goto do_jmp;
	    }
	    pc += 2;
	    break;

	case OP_JPT:
	    if(!NILP(TOP))
	    {
		POP;
		goto do_jmp;
	    }
	    pc += 2;
	    break;

	case OP_JNP:
	    if(NILP(TOP))
		goto do_jmp;
	    POP;
	    pc += 2;
	    break;

	case OP_JTP:
	    if(NILP(TOP))
	    {
		POP;
		pc += 2;
		break;
	    }
	    /* FALL THROUGH */

	case OP_JMP:
	do_jmp:
	    pc = VSTR(code) + ((pc[0] << ARG_SHIFT) | pc[1]);

	    /* Test if an error occurred (or an interrupt) */
	    TEST_INT;
	    if(INT_P)
		goto error;
	    /* Test for gc time */
	    if(data_after_gc >= gc_threshold)
	    {
		gc_stackbase.count = STK_USE;
		cmd_garbage_collect(sym_t);
	    }
	    break;

	default:
	    cmd_signal(sym_error, LIST_1(VAL(&unknown_op)));
	error:
	    while(CONSP(bindstack))
	    {
		VALUE item = VCAR(bindstack);
		if(!CONSP(item) || !INTP(VCAR(item)) || !INTP(VCDR(item)))
		{
		    GC_root gc_throwval;
		    VALUE throwval = throw_value;
		    throw_value = LISP_NULL;
		    PUSHGC(gc_throwval, throwval);
		    bindstack = unbind_one_level(bindstack);
		    POPGC;
		    throw_value = throwval;
		}
		else if(throw_value != LISP_NULL)
		{
		    /* car is an exception-handler, (PC . SP)

		       When the code at PC is called, it will have
		       the current stack usage set to SP, and then
		       the value of throw_value pushed on top.

		       The handler can then use the EJMP instruction
		       to pass control back to the error: label, or
		       simply continue execution as normal.

		       Note how we remove the bindstack entry before
		       jumping :-) */

		    stackp = (stackbase - 1) + VINT(VCDR(item));
		    PUSH(throw_value);
		    throw_value = LISP_NULL;
		    pc = VSTR(code) + VINT(VCAR(item));
		    bindstack = VCDR(bindstack);
		    goto fetch;
		}
		else
		{
		    /* car is an exception handler, but throw_value isn't
		       set, so there's nothing to handle. Keep unwinding. */
#if 1
		    fprintf(stderr, "lispmach: ignoring exception handler (%ld . %ld) pc=%d",
			    VINT(VCAR(item)), VINT(VCDR(item)), pc - VSTR(code));
		    cmd_backtrace(cmd_stderr_file());
		    fputs("\n", stderr);
#endif
		    bindstack = VCDR(bindstack);
		}
	    }
	    TOP = LISP_NULL;
	    goto quit;
	}
#ifdef PARANOID
	if(stackp < (stackbase - 1))
	{
	    fprintf(stderr, "jade: stack underflow in lisp-code: aborting...\n");
	    abort();
	}
	if(stackp > (stackbase + VINT(stkreq)))
	{
	    fprintf(stderr, "jade: stack overflow in lisp-code: aborting...\n");
	    abort();
	}
#endif
    }

#ifdef PARANOID
    if(stackp != stackbase)
	fprintf(stderr, "jade: (stackp != stackbase) at end of lisp-code\n");
#endif
    
quit:
    /* only use this var to save declaring another */
    bindstack = TOP;

    POPGCN; POPGC; POPGC; POPGC;
    return bindstack;
}

_PR VALUE cmd_validate_byte_code(VALUE bc_major, VALUE bc_minor, VALUE e_major, VALUE e_minor);
DEFUN("validate-byte-code", cmd_validate_byte_code, subr_validate_byte_code, (VALUE bc_major, VALUE bc_minor, VALUE e_major, VALUE e_minor), V_Subr4, DOC_validate_byte_code) /*
::doc:validate_byte_code::
validate-byte-code BC-MAJOR BC-MINOR JADE-MAJOR JADE-MINOR

Check that byte codes from instruction set BC-MAJOR.BC-MINOR, compiled
by Jade version JADE-MAJOR.JADE-MINOR, may be executed. If not, an error
will be signalled.
::end:: */
{
    if(!INTP(bc_major) || !INTP(bc_minor)
       || !INTP(e_major) || !INTP(e_minor)
       || VINT(bc_major) != BYTECODE_MAJOR_VERSION
       || VINT(bc_minor) < BYTECODE_MINOR_VERSION
       || VINT(e_major) != MAJOR
       || VINT(e_minor) < MINOR)
	return cmd_signal(sym_bytecode_error, sym_nil);
    else
	return sym_t;
}

_PR VALUE cmd_make_byte_code_subr(VALUE args);
DEFUN("make-byte-code-subr", cmd_make_byte_code_subr, subr_make_byte_code_subr, (VALUE args), V_SubrN, DOC_make_byte_code_subr) /*
::doc:make_byte_code_subr::
make-byte-code-subr ARGS CODE CONSTANTS STACK [DOC] [INTERACTIVE] [MACROP]

Return an object that can be used as the function value of a symbol.
::end:: */
{
    int len = list_length(args);
    VALUE obj[6], vec;
    int used;

    if(len < COMPILED_MIN_SLOTS)
	return signal_missing_arg(len + 1);
    
    if(!LISTP(VCAR(args)))
	return signal_arg_error(VCAR(args), 1);
    obj[0] = VCAR(args); args = VCDR(args);
    if(!STRINGP(VCAR(args)))
	return signal_arg_error(VCAR(args), 2);
    obj[1] = VCAR(args); args = VCDR(args);
    if(!VECTORP(VCAR(args)))
	return signal_arg_error(VCAR(args), 3);
    obj[2] = VCAR(args); args = VCDR(args);
    if(!INTP(VCAR(args)))
	return signal_arg_error(VCAR(args), 4);
    obj[3] = VCAR(args); args = VCDR(args);
    used = 4;

    if(CONSP(args))
    {
	obj[used++] = VCAR(args); args = VCDR(args);
	if(CONSP(args))
	{
	    obj[used++] = VCAR(args); args = VCDR(args);
	    if(CONSP(args) && !NILP(VCAR(args)))
		obj[3] = MAKE_INT(VINT(obj[3]) | 0x10000);
	    if(NILP(obj[used - 1]))
		used--;
	}
	if(used == 5 && NILP(obj[used - 1]))
	    used--;
    }

    vec = cmd_make_vector(MAKE_INT(used), sym_nil);
    if(vec != LISP_NULL)
    {
	int i;
	VCOMPILED(vec)->car = ((VCOMPILED(vec)->car & ~CELL8_TYPE_MASK)
			       | V_Compiled);
	for(i = 0; i < used; i++)
	    VVECTI(vec, i) = obj[i];
    }
    return vec;
}

void
lispmach_init(void)
{
    ADD_SUBR(subr_jade_byte_code);
    ADD_SUBR(subr_validate_byte_code);
    ADD_SUBR(subr_make_byte_code_subr);
    INTERN(bytecode_error); ERROR(bytecode_error);
}

void
lispmach_kill(void)
{
#ifdef BYTE_CODE_HISTOGRAM
    int i;
    fprintf(stderr, "\nByte code usages:\n");
    for(i = 0; i < 256; i++)
	fprintf(stderr, "\t%3d %ld\n", i, byte_code_usage[i]);
#endif
}
