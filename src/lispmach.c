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

#include "jade.h"
#include "jade_protos.h"
#include "bytecodes.h"

#ifdef HAVE_ALLOCA
# include <alloca.h>
#endif

_PR void lispmach_init(void);

/* Unbind one level of the BIND-STACK and return the new head of the stack.
   Each item in the BIND-STACK may be one of:
	(t . FORM)
		unwind-protect FORM to always evaluate
	(BUFFER . VIEW)
		install BUFFER as current in VIEW
	((SYM . OLD-VAL) ...)
		list of symbol bindings to undo with unbind_symbols()
	VIEW
		set VIEW as current in its window
	WINDOW
		set WINDOW as current. */
static INLINE VALUE
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
		swap_buffers_tmp(VVIEW(VCDR(item)), VTX(VCAR(item)));
	    }
	    else
	    {
		/* A set of symbol bindings (let or let*). */
		unbind_symbols(item);
	    }
	}
	else if(VIEWP(item))
	{
	    /* Reinstall VIEW */
	    if(VVIEW(item)->vw_Win)
	    {
		curr_vw = VVIEW(item);
		curr_win = curr_vw->vw_Win;
		curr_win->w_CurrVW = curr_vw;
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
    GCVAL gcv_code, gcv_consts, gcv_bindstack;
    /* The `gcv_N' field is only filled in with the stack-size when there's
       a chance of gc.	*/
    GCVALN gcv_stackbase;

    DECLARE1(code, STRINGP);
    DECLARE2(consts, VECTORP);
    DECLARE3(stkreq, NUMBERP);

#ifdef HAVE_ALLOCA
    stackbase = alloca(sizeof(VALUE) * VNUM(stkreq));
#else
    if(!(stackbase = str_alloc(sizeof(VALUE) * VNUM(stkreq))))
	return(NULL);
#endif

    stackp = stackbase - 1;
    PUSHGC(gcv_code, code);
    PUSHGC(gcv_consts, consts);
    PUSHGC(gcv_bindstack, bindstack);
    PUSHGCN(gcv_stackbase, stackbase, 0);

    pc = VSTR(code);
    while((c = *pc++) != 0)
    {
	if(c < OP_LAST_WITH_ARGS)
	{
	    register short arg;
	    switch(c & OP_ARG_MASK)
	    {
	    case OP_ARG_1BYTE:
		arg = *pc++;
		break;
	    case OP_ARG_2BYTE:
		arg = (pc[0] << ARG_SHIFT) | pc[1];
		pc += 2;
		break;
	    default:
		arg = c & OP_ARG_MASK;
	    }
	    switch(c & OP_OP_MASK)
	    {
		register VALUE tmp;
		VALUE tmp2;

	    case OP_CALL:
#ifdef MINSTACK
		if(STK_SIZE <= MINSTACK)
		{
		    STK_WARN("lisp-code");
		    TOP = cmd_signal(sym_stack_error, sym_nil);
		    goto quit;
		}
#endif
		/* args are still available above the top of the stack,
		   this just makes things a bit easier.	 */
		POPN(arg);
		tmp = TOP;
		if(SYMBOLP(tmp))
		{
		    if(VSYM(tmp)->sym_Flags & SF_DEBUG)
			single_step_flag = TRUE;
		    if(!(tmp = cmd_symbol_function(tmp, sym_nil)))
			goto error;
		}
		gcv_stackbase.gcv_N = STK_USE;
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
			struct LispCall lc;
			lc.lc_Next = lisp_call_stack;
			lc.lc_Fun = TOP;
			lc.lc_Args = tmp2;
			lc.lc_ArgsEvalledP = sym_t;
			lisp_call_stack = &lc;
			if(!(TOP = eval_lambda(tmp, tmp2, FALSE))
			   && throw_value
			   && (VCAR(throw_value) == sym_defun))
			{
			    TOP = VCDR(throw_value);
			    throw_value = NULL;
			}
			lisp_call_stack = lc.lc_Next;
		    }
		    else if(VCAR(tmp) == sym_autoload)
			/* I can't be bothered to go to all the hassle
			   of doing this here, it's going to be slow
			   anyway so just pass it to funcall.  */
			TOP = funcall(TOP, tmp2);
		    else
		    {
			cmd_signal(sym_invalid_function, LIST_1(TOP));
			goto error;
		    }
		    break;
		default:
		    cmd_signal(sym_invalid_function, LIST_1(TOP));
		    goto error;
		}
		if(!TOP)
		    goto error;
		break;

	    case OP_PUSH:
		PUSH(VVECT(consts)->vc_Array[arg]);
		break;

	    case OP_REFQ:
		if(PUSH(cmd_symbol_value(VVECT(consts)->vc_Array[arg],
					 sym_nil)))
		{
		    break;
		}
		goto error;

	    case OP_SETQ:
		if((TOP = cmd_set(VVECT(consts)->vc_Array[arg], TOP)))
		    break;
		goto error;

	    case OP_LIST:
		tmp = sym_nil;
		while(arg--)
		    tmp = cmd_cons(RET_POP, tmp);
		PUSH(tmp);
		break;

	    case OP_BIND:
		tmp = VVECT(consts)->vc_Array[arg];
		if(SYMBOLP(tmp))
		{
		    VCAR(bindstack) = bind_symbol(VCAR(bindstack), tmp,
						  RET_POP);
		    break;
		}
		goto error;
	    }
	}
	else
	{
	    switch(c)
	    {
		register VALUE tmp;
		VALUE tmp2;
		int i;

	    case OP_POP:
		POP;
		break;

	    case OP_REF:
		if((TOP = cmd_symbol_value(TOP, sym_nil)))
		    break;
		goto error;

	    case OP_SET:
		tmp = RET_POP;
		if((TOP = cmd_set(tmp, TOP)))
		    break;
		goto error;

	    case OP_FREF:
		if((TOP = cmd_symbol_function(TOP, sym_nil)))
		    break;
		goto error;

	    case OP_FSET:
		tmp = RET_POP;
		if(cmd_fset(tmp, RET_POP))
		    break;
		goto error;

	    case OP_INIT_BIND:
		bindstack = cmd_cons(sym_nil, bindstack);
		break;

	    case OP_UNBIND:
		gcv_stackbase.gcv_N = STK_USE;
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
		gcv_stackbase.gcv_N = STK_USE;
		CALL_1(cmd_eval);

	    case OP_ADD:
		tmp = RET_POP;
		if(NUMBERP(tmp) && NUMBERP(TOP))
		{
		    TOP = make_number(VNUM(TOP) + VNUM(tmp));
		    break;
		}
		goto error;

	    case OP_NEG:
		if(NUMBERP(TOP))
		{
		    TOP = make_number(-VNUM(TOP));
		    break;
		}
		goto error;

	    case OP_SUB:
		tmp = RET_POP;
		if(NUMBERP(tmp) && NUMBERP(TOP))
		{
		    TOP = make_number(VNUM(TOP) - VNUM(tmp));
		    break;
		}
		goto error;

	    case OP_MUL:
		tmp = RET_POP;
		if(NUMBERP(tmp) && NUMBERP(TOP))
		{
		    TOP = make_number(VNUM(TOP) * VNUM(tmp));
		    break;
		}
		goto error;

	    case OP_DIV:
		tmp = RET_POP;
		if(NUMBERP(tmp) && NUMBERP(TOP))
		{
		    TOP = make_number(VNUM(TOP) / VNUM(tmp));
		    break;
		}
		goto error;

	    case OP_MOD:
		tmp = RET_POP;
		if(NUMBERP(tmp) && NUMBERP(TOP))
		{
		    TOP = make_number(VNUM(TOP) % VNUM(tmp));
		    break;
		}
		goto error;

	    case OP_LNOT:
		if(NUMBERP(TOP))
		{
		    TOP = make_number(~VNUM(TOP));
		    break;
		}
		goto error;

	    case OP_NOT:
		if(TOP == sym_nil)
		    TOP = sym_t;
		else
		    TOP = sym_nil;
		break;

	    case OP_LOR:
		tmp = RET_POP;
		if(NUMBERP(tmp) && NUMBERP(TOP))
		{
		    TOP = make_number(VNUM(TOP) | VNUM(tmp));
		    break;
		}
		goto error;

	    case OP_LXOR:
		tmp = RET_POP;
		if(NUMBERP(tmp) && NUMBERP(TOP))
		{
		    TOP = make_number(VNUM(TOP) ^ VNUM(tmp));
		    break;
		}
		goto error;

	    case OP_LAND:
		tmp = RET_POP;
		if(NUMBERP(tmp) && NUMBERP(TOP))
		{
		    TOP = make_number(VNUM(TOP) & VNUM(tmp));
		    break;
		}
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
		if(NUMBERP(TOP))
		{
		    TOP = make_number(VNUM(TOP) + 1);
		    break;
		}
		goto error;

	    case OP_DEC:
		if(NUMBERP(TOP))
		{
		    TOP = make_number(VNUM(TOP) - 1);
		    break;
		}
		goto error;

	    case OP_LSH:
		CALL_2(cmd_lsh);

	    case OP_ZEROP:
		if(NUMBERP(TOP) && (VNUM(TOP) == 0))
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
		if(NUMBERP(TOP))
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

	    case OP_CATCH_KLUDGE:
		/* This is very crude.	*/
		tmp = RET_POP;
		tmp = cmd_cons(tmp, cmd_cons(TOP, sym_nil));
		gcv_stackbase.gcv_N = STK_USE;
		if((TOP = cmd_catch(tmp)))
		    break;
		goto error;

	    case OP_THROW:
		tmp = RET_POP;
		if(!throw_value)
		    throw_value = cmd_cons(TOP, tmp);
		/* This isn't really an error :-)  */
		goto error;

	    case OP_UNWIND_PRO:
		tmp = RET_POP;
		bindstack = cmd_cons(cmd_cons(sym_t, tmp), bindstack);
		break;

#if 0
	    case OP_UN_UNWIND_PRO:
		gcv_stackbase.gcv_N = STK_USE;
		/* there will only be one form (a lisp-code) */
		cmd_eval(VCDR(VCAR(bindstack)));
		bindstack = VCDR(bindstack);
		break;
#endif

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

	    case OP_ERROR_PRO:
		/* bit of a kludge, this just calls the special-form, it
		   takes an extra argument on top of the stack - the number
		   of arguments that it has been given.	 */
		i = VNUM(RET_POP);
		tmp = sym_nil;
		while(i--)
		    tmp = cmd_cons(RET_POP, tmp);
		gcv_stackbase.gcv_N = STK_USE;
		tmp = cmd_error_protect(tmp);
		if(tmp)
		{
		    PUSH(tmp);
		    break;
		}
		goto error;

	    case OP_SIGNAL:
		CALL_2(cmd_signal);

	    case OP_RETURN:
		if(!throw_value)
		    throw_value = cmd_cons(sym_defun, TOP);
		goto error;

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
		CALL_2(cmd_mapcar);

	    case OP_MAPC:
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
		CALL_2(cmd_delete_if);

	    case OP_DELETE_IF_NOT:
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
		if(NUMBERP(tmp) && NUMBERP(TOP))
		    TOP = (VNUM(TOP) == VNUM(tmp) ? sym_t : sym_nil);
		else
		    TOP = (TOP == tmp ? sym_t : sym_nil);
		break;

	    case OP_SET_CURRENT_BUFFER:
		CALL_2(cmd_set_current_buffer);

	    case OP_BIND_BUFFER:
		/* one arg: buffer. */
		tmp = RET_POP;
		if(!BUFFERP(tmp))
		    goto error;
		tmp = VAL(swap_buffers_tmp(curr_vw, VTX(tmp)));
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
		    goto error;
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
		    goto error;
		bindstack = cmd_cons(VAL(curr_vw), bindstack);
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
		if((data_after_gc >= gc_threshold) && !gc_inhibit)
		{
		    gcv_stackbase.gcv_N = STK_USE;
		    cmd_garbage_collect(sym_t);
		}
		break;

	    default:
		cmd_signal(sym_error,
			   LIST_1(MKSTR("Unknown lisp opcode")));
	    error:
		while(CONSP(bindstack))
		{
		    GCVAL gcv_throwval;
		    VALUE throwval = throw_value;
		    throw_value = NULL;
		    PUSHGC(gcv_throwval, throwval);

		    bindstack = unbind_one_level(bindstack);

		    POPGC;
		    throw_value = throwval;
		}
		TOP = NULL;
		goto quit;
	    }
	}
#ifdef PARANOID
	if(stackp < (stackbase - 1))
	{
	    fprintf(stderr, "jade: stack underflow in lisp-code: aborting...\n");
	    abort();
	}
	if(stackp > (stackbase + VNUM(stkreq)))
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
#ifndef HAVE_ALLOCA
    str_free(stackbase);
#endif
    POPGCN; POPGC; POPGC; POPGC;
    return(bindstack);
}

void
lispmach_init(void)
{
    ADD_SUBR(subr_jade_byte_code);
}
