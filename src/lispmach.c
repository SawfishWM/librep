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

#include "repint.h"
#include "bytecodes.h"
#include <assert.h>

DEFSYM(bytecode_error, "bytecode-error");
DEFSTRING(err_bytecode_error, "Invalid byte code version");
DEFSTRING(unknown_op, "Unknown lisp opcode");

#ifdef BYTE_CODE_HISTOGRAM
static u_long byte_code_usage[256];
#endif


/* Bindings */

/* Unbind one level of the BIND-STACK and return the new head of the stack.
   Each item in the BIND-STACK may be one of:
	(t . FORM)
		unwind-protect FORM to always evaluate
	((SYM . OLD-VAL) ...)
		list of symbol bindings to undo with rep_unbind_symbols()
	(PC . STACK-DEPTH)
		not unbound here; install exception handler at PC */
inline void
rep_unbind_object(repv item)
{
    if(rep_CONSP(item))
    {
	if(rep_CAR(item) == Qt)
	{
	    /* unwind-protect protection forms. */
	    Feval(rep_CDR(item));
	}
	else if(rep_CONSP(rep_CAR(item)))
	{
	    /* A set of symbol bindings (let or let*). */
	    rep_unbind_symbols(item);
	}
	else
	{
	    rep_type *t = rep_get_data_type(rep_TYPE(rep_CAR(item)));
	    if (t->unbind != 0)
		t->unbind(item);
	}
    }
    else
    {
	rep_type *t = rep_get_data_type(rep_TYPE(item));
	if (t->unbind != 0)
	    t->unbind(item);
    }
}

/* Bind one object, returning the handle to later unbind by. */
inline repv
rep_bind_object(repv obj)
{
    rep_type *t = rep_get_data_type(rep_TYPE(obj));
    if (t->bind != 0)
	return t->bind(obj);
    else
	return Qnil;
}


/* Lisp VM. */

#define TOP	    (*stackp)
#define RET_POP	    (*stackp--)
#define POP	    (stackp--)
#define POPN(n)	    (stackp -= n)
#define PUSH(v)	    (*(++stackp) = (v))
#define STK_USE	    (stackp - (stackbase - 1))

#define FETCH	    (*pc++)
#define FETCH2(var) ((var) = (FETCH << ARG_SHIFT), (var) += FETCH)

/* These macros pop as many args as required then call the specified
   function properly. */

#define CALL_1(cmd)	\
    TOP = cmd (TOP);	\
    break;
    
#define CALL_2(cmd)		\
    tmp = RET_POP;		\
    TOP = cmd (TOP, tmp);	\
    break;

#define CALL_3(cmd)			\
    tmp = RET_POP;			\
    tmp2 = RET_POP;			\
    TOP = cmd (TOP, tmp2, tmp);		\
    break;

/* Output the case statement for an instruction OP, with an embedded
   argument. The code for the instruction should start at the following
   piece of code. */
#define CASE_OP_ARG(op)							\
	case op+7:							\
	    FETCH2(arg); goto rep_CONCAT(op_, op);			\
	case op: case op+1: case op+2: case op+3: case op+4: case op+5:	\
	    arg = c - op; goto rep_CONCAT(op_, op);			\
	case op+6:							\
	    arg = FETCH;						\
	rep_CONCAT(op_, op):

DEFUN("jade-byte-code", Fjade_byte_code, Sjade_byte_code, (repv code, repv consts, repv stkreq), rep_Subr3) /*
::doc:Sjade-byte-code::
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
    repv *stackbase;
    register repv *stackp;
    /* This holds a list of sets of bindings, it can also hold the form of
       an unwind-protect that always gets eval'd (when the car is t).  */
    repv bindstack = Qnil;
    register u_char *pc;
    u_char c;
    rep_GC_root gc_code, gc_consts, gc_bindstack;
    /* The `gcv_N' field is only filled in with the stack-size when there's
       a chance of gc.	*/
    rep_GC_n_roots gc_stackbase;

    rep_DECLARE1(code, rep_STRINGP);
    rep_DECLARE2(consts, rep_VECTORP);
    rep_DECLARE3(stkreq, rep_INTP);

    stackbase = alloca(sizeof(repv) * (rep_INT(stkreq) + 1));

    /* Make sure that even when the stack has no entries, the TOP
       element still != 0 (for the error-detection at label quit:) */
    *stackbase++ = Qt;

    stackp = stackbase - 1;
    rep_PUSHGC(gc_code, code);
    rep_PUSHGC(gc_consts, consts);
    rep_PUSHGC(gc_bindstack, bindstack);
    rep_PUSHGCN(gc_stackbase, stackbase, 0);

    pc = rep_STR(code);
fetch:
    while((c = FETCH) != 0)
    {
#ifdef BYTE_CODE_HISTOGRAM
	byte_code_usage[c]++;
#endif
	switch(c)
	{
	    u_short arg;
	    repv tmp, tmp2;

	CASE_OP_ARG(OP_CALL)
#ifdef MINSTACK
	    if(STK_SIZE <= MINSTACK)
	    {
		STK_WARN("lisp-code");
		TOP = Fsignal(Qstack_error, Qnil);
		goto quit;
	    }
#endif
	    /* args are still available above the top of the stack,
	       this just makes things a bit easier. */
	    POPN(arg);
	    tmp = TOP;
	    if(rep_SYMBOLP(tmp))
	    {
		if(rep_SYM(tmp)->car & rep_SF_DEBUG)
		    rep_single_step_flag = rep_TRUE;
		if(!(tmp = Fsymbol_function(tmp, Qnil)))
		    goto error;
	    }
	    gc_stackbase.count = STK_USE;
	    switch(rep_TYPE(tmp))
	    {
	    case rep_Subr0:
		TOP = rep_SUBR0FUN(tmp)();
		break;
	    case rep_Subr1:
		TOP = rep_SUBR1FUN(tmp)(arg >= 1 ? stackp[1] : Qnil);
		break;
	    case rep_Subr2:
		switch(arg)
		{
		case 0:
		    TOP = rep_SUBR2FUN(tmp)(Qnil, Qnil);
		    break;
		case 1:
		    TOP = rep_SUBR2FUN(tmp)(stackp[1], Qnil);
		    break;
		default:
		    TOP = rep_SUBR2FUN(tmp)(stackp[1], stackp[2]);
		    break;
		}
		break;
	    case rep_Subr3:
		switch(arg)
		{
		case 0:
		    TOP = rep_SUBR3FUN(tmp)(Qnil, Qnil, Qnil);
		    break;
		case 1:
		    TOP = rep_SUBR3FUN(tmp)(stackp[1], Qnil, Qnil);
		    break;
		case 2:
		    TOP = rep_SUBR3FUN(tmp)(stackp[1], stackp[2], Qnil);
		    break;
		default:
		    TOP = rep_SUBR3FUN(tmp)(stackp[1], stackp[2], stackp[3]);
		    break;
		}
		break;
	    case rep_Subr4:
		switch(arg)
		{
		case 0:
		    TOP = rep_SUBR4FUN(tmp)(Qnil, Qnil,
					 Qnil, Qnil);
		    break;
		case 1:
		    TOP = rep_SUBR4FUN(tmp)(stackp[1], Qnil,
					 Qnil, Qnil);
		    break;
		case 2:
		    TOP = rep_SUBR4FUN(tmp)(stackp[1], stackp[2],
					 Qnil, Qnil);
		    break;
		case 3:
		    TOP = rep_SUBR4FUN(tmp)(stackp[1], stackp[2],
					 stackp[3], Qnil);
		    break;
		default:
		    TOP = rep_SUBR4FUN(tmp)(stackp[1], stackp[2],
					 stackp[3], stackp[4]);
		    break;
		}
		break;
	    case rep_Subr5:
		switch(arg)
		{
		case 0:
		    TOP = rep_SUBR5FUN(tmp)(Qnil, Qnil, Qnil,
					 Qnil, Qnil);
		    break;
		case 1:
		    TOP = rep_SUBR5FUN(tmp)(stackp[1], Qnil, Qnil,
					 Qnil, Qnil);
		    break;
		case 2:
		    TOP = rep_SUBR5FUN(tmp)(stackp[1], stackp[2], Qnil,
					 Qnil, Qnil);
		    break;
		case 3:
		    TOP = rep_SUBR5FUN(tmp)(stackp[1], stackp[2], stackp[3],
					 Qnil, Qnil);
		    break;
		case 4:
		    TOP = rep_SUBR5FUN(tmp)(stackp[1], stackp[2], stackp[3],
					 stackp[4], Qnil);
		    break;
		default:
		    TOP = rep_SUBR5FUN(tmp)(stackp[1], stackp[2], stackp[3],
					 stackp[4], stackp[5]);
		    break;
		}
		break;
	    case rep_SubrN:
		tmp2 = Qnil;
		POPN(-arg); /* reclaim my args */
		while(arg--)
		    tmp2 = Fcons(RET_POP, tmp2);
		TOP = rep_SUBRNFUN(tmp)(tmp2);
		break;

	    case rep_Cons:
		tmp2 = Qnil;
		POPN(-arg);
		while(arg--)
		    tmp2 = Fcons(RET_POP, tmp2);
		if(rep_CAR(tmp) == Qlambda)
		{
		    struct rep_Call lc;
		    lc.next = rep_call_stack;
		    lc.fun = TOP;
		    lc.args = tmp2;
		    lc.args_evalled_p = Qt;
		    rep_call_stack = &lc;
		    TOP = rep_eval_lambda(tmp, tmp2, rep_FALSE);
		    rep_call_stack = lc.next;
		}
		else if(rep_CAR(tmp) == Qautoload)
		    /* I can't be bothered to go to all the hassle
		       of doing this here, it's going to be slow
		       anyway so just pass it to rep_funcall.  */
		    TOP = rep_funcall(TOP, tmp2, rep_FALSE);
		else
		    goto invalid;
		break;

	    case rep_Compiled:
		tmp2 = Qnil;
		POPN(-arg);
		while(arg--)
		    tmp2 = Fcons(RET_POP, tmp2);
		if(!rep_COMPILED_MACRO_P(tmp))
		{
		    repv bindings;
		    struct rep_Call lc;
		    lc.next = rep_call_stack;
		    lc.fun = TOP;
		    lc.args = tmp2;
		    lc.args_evalled_p = Qt;
		    rep_call_stack = &lc;

		    bindings = rep_bind_lambda_list(rep_COMPILED_LAMBDA(tmp),
					      tmp2, rep_FALSE);
		    if(bindings != rep_NULL)
		    {
			rep_GC_root gc_bindings;
			rep_PUSHGC(gc_bindings, bindings);
			TOP = Fjade_byte_code(rep_COMPILED_CODE(tmp),
						 rep_COMPILED_CONSTANTS(tmp),
						 rep_MAKE_INT(rep_COMPILED_STACK(tmp)));
			rep_POPGC;
			rep_unbind_symbols(bindings);
		    }
		    else
			goto error;
		    rep_call_stack = lc.next;
		}
		else
		    goto invalid;
		break;
		    
	    default: invalid:
		Fsignal(Qinvalid_function, rep_LIST_1(TOP));
		goto error;
	    }
	    break;

	CASE_OP_ARG(OP_PUSH)
	    PUSH(rep_VECT(consts)->array[arg]);
	    break;

	CASE_OP_ARG(OP_REFQ)
	    PUSH(Fsymbol_value(rep_VECT(consts)->array[arg], Qnil));
	    break;

	CASE_OP_ARG(OP_SETQ)
	    TOP = Fset(rep_VECT(consts)->array[arg], TOP);
	    break;

	CASE_OP_ARG(OP_LIST)
	    tmp = Qnil;
	    while(arg--)
		tmp = Fcons(RET_POP, tmp);
	    PUSH(tmp);
	    break;

	CASE_OP_ARG(OP_BIND)
	    tmp = rep_VECT(consts)->array[arg];
	    if(rep_SYMBOLP(tmp))
		rep_CAR(bindstack) = rep_bind_symbol(rep_CAR(bindstack),
						     tmp, RET_POP);
	    else
		rep_signal_arg_error(tmp, 1);
	    break;

	case OP_REF:
	    TOP = Fsymbol_value(TOP, Qnil);
	    break;

	case OP_SET:
	    CALL_2(Fset);

	case OP_FREF:
	    TOP = Fsymbol_function(TOP, Qnil);
	    break;

	case OP_FSET:
	    CALL_2(Ffset);

	case OP_INIT_BIND:
	    bindstack = Fcons(Qnil, bindstack);
	    break;

	case OP_UNBIND:
	    gc_stackbase.count = STK_USE;
	    rep_unbind_object(rep_CAR(bindstack));
	    bindstack = rep_CDR(bindstack);
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
	    PUSH(Qnil);
	    break;

	case OP_T:
	    PUSH(Qt);
	    break;

	case OP_CONS:
	    CALL_2(Fcons);

	case OP_CAR:
	    tmp = TOP;
	    if(rep_CONSP(tmp))
		TOP = rep_CAR(tmp);
	    else
		TOP = Qnil;
	    break;

	case OP_CDR:
	    tmp = TOP;
	    if(rep_CONSP(tmp))
		TOP = rep_CDR(tmp);
	    else
		TOP = Qnil;
	    break;

	case OP_RPLACA:
	    CALL_2(Frplaca);

	case OP_RPLACD:
	    CALL_2(Frplacd);

	case OP_NTH:
	    CALL_2(Fnth);

	case OP_NTHCDR:
	    CALL_2(Fnthcdr);

	case OP_ASET:
	    CALL_3(Faset);

	case OP_AREF:
	    CALL_2(Faref);

	case OP_LENGTH:
	    CALL_1(Flength);

	case OP_EVAL:
	    gc_stackbase.count = STK_USE;
	    CALL_1(Feval);

	case OP_ADD:
	    tmp = RET_POP;
	    if(rep_INTP(tmp) && rep_INTP(TOP))
		TOP = rep_MAKE_INT(rep_INT(TOP) + rep_INT(tmp));
	    else
	    {
		if(rep_INTP(tmp))
		    rep_signal_arg_error(TOP, 2);
		else
		    rep_signal_arg_error(tmp, 1);
	    }
	    break;

	case OP_NEG:
	    if(rep_INTP(TOP))
		TOP = rep_MAKE_INT(-rep_INT(TOP));
	    else
		rep_signal_arg_error(TOP, 1);
	    break;

	case OP_SUB:
	    tmp = RET_POP;
	    if(rep_INTP(tmp) && rep_INTP(TOP))
		TOP = rep_MAKE_INT(rep_INT(TOP) - rep_INT(tmp));
	    else
	    {
		if(rep_INTP(tmp))
		    rep_signal_arg_error(TOP, 2);
		else
		    rep_signal_arg_error(tmp, 1);
	    }
	    break;

	case OP_MUL:
	    tmp = RET_POP;
	    if(rep_INTP(tmp) && rep_INTP(TOP))
		TOP = rep_MAKE_INT(rep_INT(TOP) * rep_INT(tmp));
	    else
	    {
		if(rep_INTP(tmp))
		    rep_signal_arg_error(TOP, 2);
		else
		    rep_signal_arg_error(tmp, 1);
	    }
	    break;

	case OP_DIV:
	    tmp = RET_POP;
	    if(rep_INTP(tmp) && rep_INTP(TOP))
		TOP = rep_MAKE_INT(rep_INT(TOP) / rep_INT(tmp));
	    else
	    {
		if(rep_INTP(tmp))
		    rep_signal_arg_error(TOP, 2);
		else
		    rep_signal_arg_error(tmp, 1);
	    }
	    break;

	case OP_REM:
	    CALL_2(Fremainder);

	case OP_LNOT:
	    if(rep_INTP(TOP))
		TOP = rep_MAKE_INT(~rep_INT(TOP));
	    else
		rep_signal_arg_error(TOP, 1);
	    break;

	case OP_NOT:
	    if(TOP == Qnil)
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_LOR:
	    tmp = RET_POP;
	    if(rep_INTP(tmp) && rep_INTP(TOP))
		TOP = rep_MAKE_INT(rep_INT(TOP) | rep_INT(tmp));
	    else
	    {
		if(rep_INTP(tmp))
		    rep_signal_arg_error(TOP, 2);
		else
		    rep_signal_arg_error(tmp, 1);
	    }
	    break;

	case OP_LXOR:
	    tmp = RET_POP;
	    if(rep_INTP(tmp) && rep_INTP(TOP))
		TOP = rep_MAKE_INT(rep_INT(TOP) ^ rep_INT(tmp));
	    else
	    {
		if(rep_INTP(tmp))
		    rep_signal_arg_error(TOP, 2);
		else
		    rep_signal_arg_error(tmp, 1);
	    }
	    break;

	case OP_LAND:
	    tmp = RET_POP;
	    if(rep_INTP(tmp) && rep_INTP(TOP))
		TOP = rep_MAKE_INT(rep_INT(TOP) & rep_INT(tmp));
	    else
	    {
		if(rep_INTP(tmp))
		    rep_signal_arg_error(TOP, 2);
		else
		    rep_signal_arg_error(tmp, 1);
	    }
	    break;

	case OP_EQUAL:
	    tmp = RET_POP;
	    if(!(rep_value_cmp(TOP, tmp)))
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_EQ:
	    tmp = RET_POP;
	    if(TOP == tmp)
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_NUM_EQ:
	    CALL_2(Fnum_eq);

	case OP_NUM_NOTEQ:
	    CALL_2(Fnum_noteq);

	case OP_GT:
	    tmp = RET_POP;
	    if(rep_value_cmp(TOP, tmp) > 0)
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_GE:
	    tmp = RET_POP;
	    if(rep_value_cmp(TOP, tmp) >= 0)
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_LT:
	    tmp = RET_POP;
	    if(rep_value_cmp(TOP, tmp) < 0)
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_LE:
	    tmp = RET_POP;
	    if(rep_value_cmp(TOP, tmp) <= 0)
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_INC:
	    if(rep_INTP(TOP))
		TOP = rep_MAKE_INT(rep_INT(TOP) + 1);
	    else
		rep_signal_arg_error(TOP, 1);
	    break;

	case OP_DEC:
	    if(rep_INTP(TOP))
		TOP = rep_MAKE_INT(rep_INT(TOP) - 1);
	    else
		rep_signal_arg_error(TOP, 1);
	    break;

	case OP_LSH:
	    CALL_2(Flsh);

	case OP_ZEROP:
	    if(rep_INTP(TOP) && (rep_INT(TOP) == 0))
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_NULL:
	    if(rep_NILP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_ATOM:
	    if(!rep_CONSP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_CONSP:
	    if(rep_CONSP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_LISTP:
	    if(rep_CONSP(TOP) || rep_NILP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_NUMBERP:
	    if(rep_INTP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_STRINGP:
	    if(rep_STRINGP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_VECTORP:
	    if(rep_VECTORP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_CATCH:
	    /* This takes two arguments, TAG and THROW-repv.
	       THROW-repv is the saved copy of rep_throw_value,
	       if (car THROW-repv) == TAG we match, and we
	       leave two values on the stack, nil on top (to
	       pacify EJMP), (cdr THROW-repv) below that. */
	    tmp = RET_POP;		/* tag */
	    tmp2 = TOP;		/* rep_throw_value */
	    if(rep_CONSP(tmp2) && rep_CAR(tmp2) == tmp)
	    {
		TOP = rep_CDR(tmp2);	/* leave result at stk[1] */
		PUSH(Qnil);		/* cancel error */
	    }
	    break;

	case OP_THROW:
	    tmp = RET_POP;
	    if(!rep_throw_value)
		rep_throw_value = Fcons(TOP, tmp);
	    break;

	case OP_BINDERR:
	    /* Pop our single argument and cons it onto the bind-
	       stack in a pair with the current stack-pointer.
	       This installs an address in the code string as an
	       error handler. */
	    tmp = RET_POP;
	    bindstack = Fcons(Fcons(tmp, rep_MAKE_INT(STK_USE)), bindstack);
	    break;

	case OP_FBOUNDP:
	    CALL_1(Ffboundp);

	case OP_BOUNDP:
	    CALL_1(Fboundp);

	case OP_SYMBOLP:
	    if(rep_SYMBOLP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    break;

	case OP_GET:
	    CALL_2(Fget);

	case OP_PUT:
	    CALL_3(Fput);

	case OP_ERRORPRO:
	    /* This should be called with three values on the stack.
		1. conditions of the error handler
		2. rep_throw_value of the exception
		3. symbol to bind the error data to (or nil)

	       This function pops (1) and tests it against the error
	       in (2). If they match it sets (2) to nil, and binds the
	       error data to the symbol in (3). */
	    tmp = RET_POP;
	    if(rep_CONSP(TOP) && rep_CAR(TOP) == Qerror
	       && rep_compare_error(rep_CDR(TOP), tmp))
	    {
		/* The handler matches the error. */
		tmp = rep_CDR(TOP);	/* the error data */
		tmp2 = stackp[-1];	/* the symbol to bind to */
		if(rep_SYMBOLP(tmp2) && !rep_NILP(tmp2))
		    bindstack = Fcons(rep_bind_symbol(Qnil, tmp2, tmp),
					 bindstack);
		else
		    /* Placeholder to allow simple unbinding */
		    bindstack = Fcons(Qnil, bindstack);
		TOP = Qnil;
	    }
	    break;

	case OP_SIGNAL:
	    gc_stackbase.count = STK_USE;
	    CALL_2(Fsignal);

	case OP_REVERSE:
	    CALL_1(Freverse);

	case OP_NREVERSE:
	    CALL_1(Fnreverse);

	case OP_ASSOC:
	    CALL_2(Fassoc);

	case OP_ASSQ:
	    CALL_2(Fassq);

	case OP_RASSOC:
	    CALL_2(Frassoc);

	case OP_RASSQ:
	    CALL_2(Frassq);

	case OP_LAST:
	    CALL_1(Flast);

	case OP_MAPCAR:
	    gc_stackbase.count = STK_USE;
	    CALL_2(Fmapcar);

	case OP_MAPC:
	    gc_stackbase.count = STK_USE;
	    CALL_2(Fmapc);

	case OP_MEMBER:
	    CALL_2(Fmember);

	case OP_MEMQ:
	    CALL_2(Fmemq);

	case OP_DELETE:
	    CALL_2(Fdelete);

	case OP_DELQ:
	    CALL_2(Fdelq);

	case OP_DELETE_IF:
	    gc_stackbase.count = STK_USE;
	    CALL_2(Fdelete_if);

	case OP_DELETE_IF_NOT:
	    gc_stackbase.count = STK_USE;
	    CALL_2(Fdelete_if_not);

	case OP_COPY_SEQUENCE:
	    CALL_1(Fcopy_sequence);

	case OP_SEQUENCEP:
	    CALL_1(Fsequencep);

	case OP_FUNCTIONP:
	    CALL_1(Ffunctionp);

	case OP_SPECIAL_FORM_P:
	    CALL_1(Fspecial_form_p);

	case OP_SUBRP:
	    CALL_1(Fsubrp);

	case OP_EQL:
	    tmp = RET_POP;
	    if(rep_INTP(tmp) && rep_INTP(TOP))
		TOP = (rep_INT(TOP) == rep_INT(tmp) ? Qt : Qnil);
	    else
		TOP = (TOP == tmp ? Qt : Qnil);
	    break;

	case OP_MAX:
	    tmp = RET_POP;
	    if(rep_value_cmp(tmp, TOP) > 0)
		TOP = tmp;
	    break;

	case OP_MIN:
	    tmp = RET_POP;
	    if(rep_value_cmp(tmp, TOP) < 0)
		TOP = tmp;
	    break;

	case OP_FILTER:
	    gc_stackbase.count = STK_USE;
	    CALL_2(Ffilter);

	case OP_MACROP:
	    CALL_1(Fmacrop);

	case OP_BYTECODEP:
	    CALL_1(Fbytecodep);

	case OP_PUSHI0:
	    PUSH(rep_MAKE_INT(0));
	    break;

	case OP_PUSHI1:
	    PUSH(rep_MAKE_INT(1));
	    break;

	case OP_PUSHI2:
	    PUSH(rep_MAKE_INT(2));
	    break;

	case OP_PUSHIM1:
	    PUSH(rep_MAKE_INT(-1));
	    break;

	case OP_PUSHIM2:
	    PUSH(rep_MAKE_INT(-2));
	    break;

	case OP_PUSHI:
	    arg = FETCH;
	    if (arg < 128)
		PUSH(rep_MAKE_INT(arg));
	    else
		PUSH(rep_MAKE_INT(((short)arg) - 256));
	    break;

	case OP_PUSHIW:
	    FETCH2(arg);
	    if (arg < 32768)
		PUSH(rep_MAKE_INT(arg));
	    else
		PUSH(rep_MAKE_INT(((long)arg) - 65536));
	    break;

	case OP_BINDOBJ:
	    tmp = RET_POP;
	    bindstack = Fcons(rep_bind_object(tmp), bindstack);
	    break;

	case OP_SWAP2:
	    tmp = TOP;
	    TOP = stackp[-1];
	    stackp[-1] = stackp[-2];
	    stackp[-2] = tmp;
	    break;

	case OP_MOD:
	    CALL_2(Fmod);

	/* Jump instructions follow */

	case OP_EJMP:
	    /* Pop the stack; if it's nil jmp pc[0,1], otherwise
	       set rep_throw_value=ARG and goto the error handler. */
	    tmp = RET_POP;
	    if(rep_NILP(tmp))
		goto do_jmp;
	    rep_throw_value = tmp;
	    goto error;

	case OP_JN:
	    if(rep_NILP(RET_POP))
		goto do_jmp;
	    pc += 2;
	    break;

	case OP_JT:
	    if(!rep_NILP(RET_POP))
		goto do_jmp;
	    pc += 2;
	    break;

	case OP_JPN:
	    if(rep_NILP(TOP))
	    {
		POP;
		goto do_jmp;
	    }
	    pc += 2;
	    break;

	case OP_JPT:
	    if(!rep_NILP(TOP))
	    {
		POP;
		goto do_jmp;
	    }
	    pc += 2;
	    break;

	case OP_JNP:
	    if(rep_NILP(TOP))
		goto do_jmp;
	    POP;
	    pc += 2;
	    break;

	case OP_JTP:
	    if(rep_NILP(TOP))
	    {
		POP;
		pc += 2;
		break;
	    }
	    /* FALL THROUGH */

	case OP_JMP:
	do_jmp:
	    pc = rep_STR(code) + ((pc[0] << ARG_SHIFT) | pc[1]);

	    /* Test if an error occurred (or an interrupt) */
	    rep_TEST_INT;
	    if(rep_INTERRUPTP)
		goto error;
	    /* Test for gc time */
	    if(rep_data_after_gc >= rep_gc_threshold)
	    {
		gc_stackbase.count = STK_USE;
		Fgarbage_collect(Qt);
	    }
	    break;

	default:
	    Fsignal(Qerror, rep_LIST_1(rep_VAL(&unknown_op)));
	}

	if (rep_throw_value || !TOP)
	{
	    /* Some form of error occurred. Unbind the binding stack. */
	error:
	    while(rep_CONSP(bindstack))
	    {
		repv item = rep_CAR(bindstack);
		if(!rep_CONSP(item)
		   || !rep_INTP(rep_CAR(item)) || !rep_INTP(rep_CDR(item)))
		{
		    rep_GC_root gc_throwval;
		    repv throwval = rep_throw_value;
		    rep_throw_value = rep_NULL;
		    rep_PUSHGC(gc_throwval, throwval);
		    gc_stackbase.count = STK_USE;
		    rep_unbind_object(item);
		    bindstack = rep_CDR(bindstack);
		    rep_POPGC;
		    rep_throw_value = throwval;
		}
		else if(rep_throw_value != rep_NULL)
		{
		    /* car is an exception-handler, (PC . SP)

		       When the code at PC is called, it will have
		       the current stack usage set to SP, and then
		       the value of rep_throw_value pushed on top.

		       The handler can then use the EJMP instruction
		       to pass control back to the error: label, or
		       simply continue execution as normal.

		       Note how we remove the bindstack entry before
		       jumping :-) */

		    stackp = (stackbase - 1) + rep_INT(rep_CDR(item));
		    PUSH(rep_throw_value);
		    rep_throw_value = rep_NULL;
		    pc = rep_STR(code) + rep_INT(rep_CAR(item));
		    bindstack = rep_CDR(bindstack);
		    goto fetch;
		}
		else
		{
		    /* car is an exception handler, but rep_throw_value isn't
		       set, so there's nothing to handle. Keep unwinding. */
#if 1
		    fprintf(stderr, "lispmach: ignoring exception handler (%ld . %ld) pc=%d",
			    rep_INT(rep_CAR(item)),
			    rep_INT(rep_CDR(item)), pc - rep_STR(code));
		    Fbacktrace(Fstderr_file());
		    fputs("\n", stderr);
#endif
		    bindstack = rep_CDR(bindstack);
		}
	    }
	    TOP = rep_NULL;
	    goto quit;
	}
    }

quit:
    /* only use this var to save declaring another */
    bindstack = TOP;

    rep_POPGCN; rep_POPGC; rep_POPGC; rep_POPGC;
    return bindstack;
}

DEFUN("validate-byte-code", Fvalidate_byte_code, Svalidate_byte_code, (repv bc_major, repv bc_minor), rep_Subr2) /*
::doc:Svalidate-byte-code::
validate-byte-code BC-MAJOR BC-MINOR

Check that byte codes from instruction set BC-MAJOR.BC-MINOR, may be
executed. If not, an error will be signalled.
::end:: */
{
    if(!rep_INTP(bc_major) || !rep_INTP(bc_minor)
       || rep_INT(bc_major) != BYTECODE_MAJOR_VERSION
       || rep_INT(bc_minor) > BYTECODE_MINOR_VERSION)
	return Fsignal(Qbytecode_error, Qnil);
    else
	return Qt;
}

DEFUN("make-byte-code-subr", Fmake_byte_code_subr, Smake_byte_code_subr, (repv args), rep_SubrN) /*
::doc:Smake-byte-code-subr::
make-byte-code-subr ARGS CODE CONSTANTS STACK [rep_DOC] [INTERACTIVE] [MACROP]

Return an object that can be used as the function value of a symbol.
::end:: */
{
    int len = rep_list_length(args);
    repv obj[6], vec;
    int used;

    if(len < rep_COMPILED_MIN_SLOTS)
	return rep_signal_missing_arg(len + 1);
    
    if(!rep_LISTP(rep_CAR(args)))
	return rep_signal_arg_error(rep_CAR(args), 1);
    obj[0] = rep_CAR(args); args = rep_CDR(args);
    if(!rep_STRINGP(rep_CAR(args)))
	return rep_signal_arg_error(rep_CAR(args), 2);
    obj[1] = rep_CAR(args); args = rep_CDR(args);
    if(!rep_VECTORP(rep_CAR(args)))
	return rep_signal_arg_error(rep_CAR(args), 3);
    obj[2] = rep_CAR(args); args = rep_CDR(args);
    if(!rep_INTP(rep_CAR(args)))
	return rep_signal_arg_error(rep_CAR(args), 4);
    obj[3] = rep_CAR(args); args = rep_CDR(args);
    used = 4;

    if(rep_CONSP(args))
    {
	obj[used++] = rep_CAR(args); args = rep_CDR(args);
	if(rep_CONSP(args))
	{
	    obj[used++] = rep_CAR(args); args = rep_CDR(args);
	    if(rep_CONSP(args) && !rep_NILP(rep_CAR(args)))
		obj[3] = rep_MAKE_INT(rep_INT(obj[3]) | 0x10000);
	    if(rep_NILP(obj[used - 1]))
		used--;
	}
	if(used == 5 && rep_NILP(obj[used - 1]))
	    used--;
    }

    vec = Fmake_vector(rep_MAKE_INT(used), Qnil);
    if(vec != rep_NULL)
    {
	int i;
	rep_COMPILED(vec)->car = ((rep_COMPILED(vec)->car & ~rep_CELL8_TYPE_MASK)
			       | rep_Compiled);
	for(i = 0; i < used; i++)
	    rep_VECTI(vec, i) = obj[i];
    }
    return vec;
}

void
rep_lispmach_init(void)
{
    rep_ADD_SUBR(Sjade_byte_code);
    rep_ADD_SUBR(Svalidate_byte_code);
    rep_ADD_SUBR(Smake_byte_code_subr);
    rep_INTERN(bytecode_error); rep_ERROR(bytecode_error);
}

void
rep_lispmach_kill(void)
{
#ifdef BYTE_CODE_HISTOGRAM
    int i;
    fprintf(stderr, "\nByte code usages:\n");
    for(i = 0; i < 256; i++)
	fprintf(stderr, "\t%3d %ld\n", i, byte_code_usage[i]);
#endif
}
