/* lisp.c -- Core of the Lisp, reading and evaluating...
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
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include "jade_protos.h"

#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

_PR VALUE readl(VALUE, int *);

_PR VALUE eval_lambda(VALUE, VALUE, bool);
_PR VALUE load_autoload(VALUE, VALUE);
_PR VALUE funcall(VALUE, VALUE);
_PR VALUE eval_string(u_char *, bool);

_PR VALUE call_lisp0(VALUE);
_PR VALUE call_lisp1(VALUE, VALUE);
_PR VALUE call_lisp2(VALUE, VALUE, VALUE);

_PR void lisp_prin(VALUE, VALUE);
_PR void string_princ(VALUE, VALUE);
_PR void string_print(VALUE, VALUE);

_PR VALUE find_member_by_index(VALUE, int);
_PR VALUE move_down_list(VALUE, int);
_PR int list_length(VALUE);
_PR VALUE copy_list(VALUE);
_PR VALUE handle_var_int(VALUE, long *);

_PR void handle_error(VALUE, VALUE);
_PR VALUE signal_arg_error(VALUE, int);
_PR VALUE signal_missing_arg(int argnum);
_PR VALUE mem_error(void);

_PR void lisp_init(void);

_PR VALUE sym_debug_entry, sym_debug_exit, sym_debug_error_entry;
VALUE sym_debug_entry, sym_debug_exit, sym_debug_error_entry;

_PR VALUE sym_quote, sym_lambda, sym_macro, sym_autoload, sym_function;
VALUE sym_quote, sym_lambda, sym_macro, sym_autoload, sym_function;

_PR VALUE sym_standard_input, sym_standard_output, sym_defun;
VALUE sym_standard_input, sym_standard_output, sym_defun;

_PR VALUE sym_amp_optional, sym_amp_rest, sym_amp_aux;
VALUE sym_amp_optional, sym_amp_rest, sym_amp_aux;

/* When a `throw' happens a function stuffs a cons-cell in here with,
   (TAG . VALUE).
   An error is the above with TAG=sym_error and VALUE a list of relevant
   data. */
_PR VALUE throw_value;
VALUE throw_value;

/* This cons cell is used for interrupts. We don't know if it's safe to
   call cmd_cons() (maybe in gc?) so this is always valid.  */
_PR VALUE int_cell;
VALUE int_cell;

_PR VALUE sym_error, sym_error_message, sym_invalid_function;
_PR VALUE sym_void_function, sym_void_value, sym_bad_arg, sym_invalid_read_syntax;
_PR VALUE sym_end_of_stream, sym_invalid_lambda_list, sym_missing_arg;
_PR VALUE sym_invalid_macro, sym_invalid_autoload, sym_no_catcher;
_PR VALUE sym_buffer_read_only, sym_bad_event_desc, sym_file_error;
_PR VALUE sym_invalid_stream, sym_setting_constant, sym_process_error;
_PR VALUE sym_invalid_area, sym_no_memory, sym_user_interrupt, sym_arith_error;
_PR VALUE sym_window_error, sym_invalid_pos;

VALUE sym_error, sym_error_message, sym_invalid_function,
    sym_void_function, sym_void_value, sym_bad_arg, sym_invalid_read_syntax,
    sym_end_of_stream, sym_invalid_lambda_list, sym_missing_arg,
    sym_invalid_macro, sym_invalid_autoload, sym_no_catcher,
    sym_buffer_read_only, sym_bad_event_desc, sym_file_error,
    sym_invalid_stream, sym_setting_constant, sym_process_error,
    sym_invalid_area, sym_no_memory, sym_user_interrupt, sym_arith_error,
    sym_window_error, sym_invalid_pos;

#ifdef MINSTACK
_PR VALUE sym_stack_error;
VALUE sym_stack_error;
#endif

_PR VALUE debug_on_error, sym_error_info;
VALUE debug_on_error, sym_error_info;

_PR VALUE sym_print_escape_newlines, sym_print_length, sym_print_level;
VALUE sym_print_escape_newlines, sym_print_length, sym_print_level; /*
::doc:print_escape_newlines::
When non-nil `print' escapes newline, form-feed, and tab characters.
::end::
::doc:print_length::
The maximum number of list elements to print before abbreviating.
::end::
::doc:print_level::
The number of list levels to descend when printing before abbreviating.
::end:: */

/*
 * When TRUE cmd_eval() calls the "debug-entry" function
 */
_PR bool single_step_flag;
bool single_step_flag;

_PR struct LispCall *lisp_call_stack;
struct LispCall *lisp_call_stack;

static long lisp_depth, max_lisp_depth = 500;

/*
 * All of the read-related functions are now stream based. This will
 * probably add some (much?) overhead but I think it's worth it?
 *
 * The `c' variable which keeps coming up is the lookahead character,
 * since each read*() routine normally has to look at the next character
 * to see if it's what it wants. If not, this char is given to someone
 * else...
 */

static VALUE
read_list(VALUE strm, register int *c_p)
{
    VALUE result = sym_nil;
    VALUE last = NULL;
    *c_p = stream_getc(strm);
    while(1)
    {
	switch(*c_p)
	{
	case EOF:
	    return(cmd_signal(sym_end_of_stream, LIST_1(strm)));

	case ' ':
	case '\t':
	case '\n':
	case '\f':
	    *c_p = stream_getc(strm);
	    continue;

	case ';':
	    {
		register int c;
		while((c = stream_getc(strm)) != EOF && c != '\n' && c != '\f')
		    ;
		*c_p = stream_getc(strm);
		continue;
	    }

	case '.':
	    *c_p = stream_getc(strm);
	    if(last)
	    {
		if(!(VCDR(last) = readl(strm, c_p)))
		    return(NULL);
	    }
	    else
	    {
		return(cmd_signal(sym_invalid_read_syntax,
				  LIST_1(MKSTR("Nothing to dot second element of cons-cell to"))));
	    }

	case ')':
	case ']':
	    *c_p = stream_getc(strm);
	    return(result);
	    
	default:
	    {
		register VALUE this = cmd_cons(sym_nil, sym_nil);
		if(last)
		    VCDR(last) = this;
		else
		    result = this;
		if(!(VCAR(this) = readl(strm, c_p)))
		    return(NULL);
		last = this;
	    }
	}
    }
}

/*
 * could be number *or* symbol
 */
static VALUE
read_symbol(VALUE strm, int *c_p)
{
#define SYM_BUF_LEN 255
    VALUE result;
    u_char buff[SYM_BUF_LEN + 1];
    register u_char *buf = buff + 1;
    int c = *c_p;
    register int i = 0;
    bool couldbenum = TRUE;
    buff[0] = V_StaticString;
    while((c != EOF) && (i < SYM_BUF_LEN))
    {
	switch(c)
	{
	case ' ':
	case '\t':
	case '\n':
	case '\f':
	case '(':
	case ')':
	case '[':
	case ']':
	case '\'':
	case '"':
	case ';':
	    goto done;
	case '\\':
	    couldbenum = FALSE;
	    c = stream_getc(strm);
	    if(c == EOF)
		return(cmd_signal(sym_end_of_stream, LIST_1(strm)));
	    buf[i++] = c;
	    break;
	case '|':
	    couldbenum = FALSE;
	    c = stream_getc(strm);
	    while((c != EOF) && (c != '|') && (i < SYM_BUF_LEN))
	    {
		buf[i++] = c;
		c = stream_getc(strm);
	    }
	    if(c == EOF)
		return(cmd_signal(sym_end_of_stream, LIST_1(strm)));
	    break;
	default:
	    if(couldbenum)
	    {
		/*
		 * if c isn't a digit (decimal or hex) and c isn't a sign
		 * at the start of the string then it's not a number!
		 */
		if(!(isdigit(c) || ((i >= 2) && isxdigit(c)) || ((i == 1) && (toupper(c) == 'X'))))
		{
		    if(!((i == 0) && ((c == '+') || (c == '-'))))
			couldbenum = FALSE;
		}
	    }
	    buf[i++] = c;
	}
	c = stream_getc(strm);
    }
    if(i >= SYM_BUF_LEN)
    {
	/* Guess I'd better fix this! */
	return(cmd_signal(sym_error,
			  LIST_1(MKSTR("Internal buffer overrun"))));
    }
done:
    buf[i] = 0;
    if(couldbenum && ((i > 1) || isdigit(*buf)))
    {
	char *dummy;
	result = make_number(strtol(buf, &dummy, 0));
    }
    else
    {
	if(!(result = cmd_find_symbol(VAL(buff), sym_nil))
	   || (NILP(result) && strcmp(buf, "nil")))
	{
	    VALUE name;
	    if((name = string_dup(buf)) && (result = cmd_make_symbol(name)))
		result = cmd_intern_symbol(result, sym_nil);
	    else
		result = NULL;
	}
    }
    *c_p = c;
    return(result);
}

static VALUE
read_vector(VALUE strm, int *c_p)
{
    VALUE result;
    VALUE list = read_list(strm, c_p);
    if(list)
    {
	VALUE cur = list;
	int len;
	for(len = 0; CONSP(cur); len++)
	    cur = VCDR(cur);
	result = make_vector(len);
	if(result)
	{
	    int i;
	    cur = list;
	    for(i = 0; i < len; i++)
	    {
		VALUE nxt = VCDR(cur);
		VVECT(result)->vc_Array[i] =  VCAR(cur);
#if 1
		/* I think it's okay to put the cons cells back onto their
		   freelist. There can't be any references to them??  */
		cons_free(cur);
#endif
		cur = nxt;
	    }
	}
	else
	    result = NULL;
    }
    else
	result = NULL;
    return(result);
}

static VALUE
read_str(VALUE strm, int *c_p)
{
    VALUE result;
    int buflen = MAXBUCKETSIZE;	/* biggest block that will be cached */
    int c = stream_getc(strm);
    u_char *buf = str_alloc(buflen);
    register u_char *cur = buf;
    u_char *bufend = buf + buflen;
    if(buf)
    {
	while((c != EOF) && (c != '"'))
	{
	    if(cur == bufend)
	    {
		register int newbuflen = buflen * 2;
		register u_char *newbuf = str_alloc(newbuflen);
		if(newbuf)
		{
		    memcpy(newbuf, buf, cur - buf);
		    str_free(buf);
		    buf = newbuf;
		    cur = buf + buflen;
		    buflen = newbuflen;
		    bufend = buf + buflen;
		}
		else
		    return(mem_error());
	    }
	    if(c == '\\')
	    {
		c = stream_getc(strm);
		if(c == '\n')
		    /* escaped newline is ignored */
 		    c = stream_getc(strm);
		else
		    *cur++ = (u_char)stream_read_esc(strm, &c);
	    }
	    else
	    {
		*cur++ = c;
		c = stream_getc(strm);
	    }
	}
	if(c == EOF)
	    result = cmd_signal(sym_end_of_stream, LIST_1(strm));
	else
	{
	    *c_p = stream_getc(strm);
	    result = string_dupn(buf, cur - buf);
	}
	str_free(buf);
	return(result);
    }
    return(mem_error());
}

/*
 * Using the above readlisp*() functions this classifies each type
 * of expression and translates it into a lisp object (VALUE).
 * Returns NULL in case of error.
 */
VALUE
readl(VALUE strm, register int *c_p)
{
#ifdef MINSTACK
    if(STK_SIZE <= MINSTACK)
    {
	STK_WARN("read");
	return(cmd_signal(sym_stack_error, sym_nil));
    }
#endif
    while(1)
    {
	switch(*c_p)
	{
	case EOF:
	    return(sym_nil);

	case ' ':
	case '\t':
	case '\n':
	case '\f':
	    *c_p = stream_getc(strm);
	    continue;

	case ';':
	    {
		register int c;
		while((c = stream_getc(strm)) != EOF && c != '\n' && c != '\f')
		    ;
		*c_p = stream_getc(strm);
		continue;
	    }

	case '\(':
	    return(read_list(strm, c_p));

	case '\'':
	    {
		/*
		 * transmogrify 'X into (quote X)
		 */
		register VALUE form;
		form = cmd_cons(sym_quote, cmd_cons(sym_nil, sym_nil));
		if((*c_p = stream_getc(strm)) == EOF)
		    goto eof;
		else if((VCAR(VCDR(form)) = readl(strm, c_p)))
		    return(form);
		return(NULL);
	    }

	case '[':
	    return(read_vector(strm, c_p));

	case '"':
	    return(read_str(strm, c_p));

	case '?':
	    {
		register int c;
		switch(c = stream_getc(strm))
		{
		case EOF:
		    goto eof;
		case '\\':
		    if((*c_p = stream_getc(strm)) == EOF)
			goto eof;
		    else
			return(make_number(stream_read_esc(strm, c_p)));
		    break;
		default:
		    *c_p = stream_getc(strm);
		    return(make_number(c));
		}
	    }

	case '#':
	    switch(*c_p = stream_getc(strm))
	    {
		register VALUE form;
	    case EOF:
		goto eof;
	    case '\'':
		form = cmd_cons(sym_function, cmd_cons(sym_nil, sym_nil));
		if((*c_p = stream_getc(strm)) == EOF)
		    goto eof;
		if(!(VCAR(VCDR(form)) = readl(strm, c_p)))
		    return(NULL);
		return(form);
	    default:
		return(cmd_signal(sym_invalid_read_syntax, LIST_1(strm)));
	    }

	default:
	    return(read_symbol(strm, c_p));
	}
    }
    /* NOT REACHED */

eof:
    return(cmd_signal(sym_end_of_stream, LIST_1(strm)));
}

/*
 * Evaluates each element of `list' and builds them into a new list.
 */
static VALUE
eval_list(VALUE list)
{
    VALUE result = sym_nil;
    VALUE *last = &result;
    GCVAL gcv_result, gcv_list;
    PUSHGC(gcv_result, result);
    PUSHGC(gcv_list, list);
    while(CONSP(list))
    {
	VALUE tmp;
	if(!(tmp = cmd_eval(VCAR(list))))
	{
	    result = NULL;
	    break;
	}
	if(!(*last = cmd_cons(tmp, sym_nil)))
	{
	    result = NULL;
	    break;
	}
	list = VCDR(list);
	last = &VCDR(*last);
	TEST_INT;
	if(INT_P)
	{
	    result = NULL;
	    break;
	}
    }
    if(result && last && !NILP(list))
	*last = cmd_eval(list);
    POPGC; POPGC;
    return(result);
}

/*
 * format of lambda-lists is something like,
 *
 * [{required-symbols}] [&optional {optional-symbols}] [&rest symbol]
 * [&aux {auxiliary-symbols}]
 *
 * NB: auxiliary symbols are set to nil.
 */
static VALUE
bindlambdalist(VALUE lambdaList, VALUE argList, int evalArgs)
{
#define STATE_REQUIRED 1
#define STATE_OPTIONAL 2
#define STATE_REST     3
#define STATE_AUX      4
    VALUE boundlist = sym_nil;
    if(CONSP(lambdaList))
    {
	GCVAL gcv_boundlist;
	char state = STATE_REQUIRED;
	PUSHGC(gcv_boundlist, boundlist);
	while(CONSP(lambdaList) && SYMBOLP(VCAR(lambdaList)))
	{
	    VALUE argobj;
	    VALUE argspec = VCAR(lambdaList);
	    if(VSTR(VSYM(argspec)->sym_Name)[0] == '&')
	    {
		if(argspec == sym_amp_optional)
		{
		    if(state > STATE_OPTIONAL)
		    {
			cmd_signal(sym_invalid_lambda_list, LIST_1(lambdaList));
			goto error;
		    }
		    state = STATE_OPTIONAL;
		    lambdaList = VCDR(lambdaList);
		    continue;
		}
		else if(argspec == sym_amp_rest)
		{
		    if(state > STATE_REST)
		    {
			cmd_signal(sym_invalid_lambda_list, LIST_1(lambdaList));
			goto error;
		    }
		    state = STATE_REST;
		    lambdaList = VCDR(lambdaList);
		    continue;
		}
		else if(argspec == sym_amp_aux)
		{
		    state = STATE_AUX;
		    lambdaList = VCDR(lambdaList);
		    continue;
		}
	    }
	    switch(state)
	    {
	    case STATE_REQUIRED:
		if(!CONSP(argList))
		{
		    cmd_signal(sym_missing_arg, LIST_1(argspec));
		    goto error;
		}
		/* FALL THROUGH */
	    case STATE_OPTIONAL:
		if(CONSP(argList))
		{
		    if(evalArgs)
		    {
			if(!(argobj = cmd_eval(VCAR(argList))))
			    goto error;
		    }
		    else
			argobj = VCAR(argList);
		    argList = VCDR(argList);
		}
		else
		    argobj = sym_nil;
		boundlist = bind_symbol(boundlist, argspec, argobj);
		break;
	    case STATE_REST:
		if(evalArgs)
		{
		    if(!(argobj = eval_list(argList)))
			goto error;
		}
		else
		    argobj = argList;
		boundlist = bind_symbol(boundlist, argspec, argobj);
		state = STATE_AUX;
		break;
	    case STATE_AUX:
		boundlist = bind_symbol(boundlist, argspec, sym_nil);
	    }
	    lambdaList = VCDR(lambdaList);
	    TEST_INT;
	    if(INT_P)
		goto error;
	}
	POPGC;
    }
    return(boundlist);

error:
    POPGC;
    unbind_symbols(boundlist);
    return(NULL);
}

VALUE
eval_lambda(VALUE lambdaExp, VALUE argList, bool evalArgs)
{
    VALUE result = NULL;
    if(CONSP(VCDR(lambdaExp)))
    {
	VALUE boundlist;
	GCVAL gcv_lambdaExp, gcv_argList;
	PUSHGC(gcv_lambdaExp, lambdaExp);
	PUSHGC(gcv_argList, argList);
	lambdaExp = VCDR(lambdaExp);
	boundlist = bindlambdalist(VCAR(lambdaExp), argList, evalArgs);
	if(boundlist)
	{
	    GCVAL gcv_boundlist;
	    PUSHGC(gcv_boundlist, boundlist);
	    result = cmd_progn(VCDR(lambdaExp));
	    POPGC;
	    unbind_symbols(boundlist);
	}
	else
	    result = NULL;
	POPGC; POPGC;
    }
    return(result);
}

/* Autoloads a function, FUN is the symbol of the function, ALOAD-DEF is
   the `(autoload FILE-NAME ...)' object. This function may cause a gc.
   Returns the new function-value of FUN, or NULL for an error. */
VALUE
load_autoload(VALUE fun, VALUE aload_def)
{
    if(!SYMBOLP(fun))
    {
	/* Unless the function we're calling is a symbol don't bother.
	   (Because it wouldn't be possible to find the new definition.)  */
	return(cmd_signal(sym_invalid_autoload,
			  list_2(fun,
				 MKSTR("Can only autoload from symbols"))));
    }
    else
    {
	VALUE autoload = VCDR(aload_def);
	if(CONSP(autoload))
	{
	    /* trash the autoload defn, this way I make sure
	       that we don't keep trying to autoload a function
	       indefinitely.  */
	    GCVAL gcv_fun;
	    VALUE tmp;
	    u_char *old_msg;
	    u_long old_msg_len;
	    save_message(curr_win, &old_msg, &old_msg_len);
	    messagef("Loading %s...", VSTR(VCAR(autoload)));
	    refresh_message(curr_win);
	    cmd_flush_output();

	    PUSHGC(gcv_fun, fun);
	    VCAR(aload_def) = sym_nil;
	    tmp = cmd_load(VCAR(autoload), sym_t, sym_nil, sym_nil);
	    POPGC;

	    messagef("Loading %s...done.", VSTR(VCAR(autoload)));
	    refresh_message(curr_win);
	    cmd_flush_output();
	    restore_message(curr_win, old_msg, old_msg_len);

	    if(tmp && !NILP(tmp))
		return(cmd_symbol_function(fun, sym_nil));
	}
	return(cmd_signal(sym_invalid_autoload, LIST_1(fun)));
    }
}

static VALUE
eval(VALUE obj)
{
    VALUE result = NULL;
    GCVAL gcv_obj;
#ifdef MINSTACK
    if(STK_SIZE <= MINSTACK)
    {
	STK_WARN("eval");
	return(cmd_signal(sym_stack_error, sym_nil));
    }
#endif
    if(++lisp_depth > max_lisp_depth)
    {
	cmd_signal(sym_error, LIST_1(MKSTR("max-lisp-depth exceeded, possible infite recursion?")));
    }
    else if(obj)
    {
	switch(VTYPE(obj))
	{
	    VALUE funcobj, arglist;
	    int type;
	case V_Symbol:
	    result = cmd_symbol_value(obj, sym_nil);
	    break;

	case V_Cons:
again:
	    funcobj = VCAR(obj);
	    arglist = VCDR(obj);
	    if(SYMBOLP(funcobj))
	    {
		if(VSYM(funcobj)->sym_Flags & SF_DEBUG)
		    single_step_flag = TRUE;
		funcobj = cmd_symbol_function(funcobj, sym_nil);
		if(!funcobj)
		    goto end;
	    }
	    switch(type = VTYPE(funcobj))
	    {
		VALUE alist, car, args[5];
		GCVALN gcvn_args;
		int i, nargs;
	    case V_Subr0:
		result = VSUBR0FUN(funcobj)();
		break;

	    case V_SubrN:
		PUSHGC(gcv_obj, obj);
		alist = eval_list(arglist);
		if(alist)
		    result = VSUBRNFUN(funcobj)(alist);
		POPGC;
		break;

	    case V_Subr1:
		nargs = 1;
		args[0] = sym_nil;
		goto do_subr;

	    case V_Subr2:
		nargs = 2;
		args[0] = args[1] = sym_nil;
		goto do_subr;

	    case V_Subr3:
		nargs = 3;
		args[0] = args[1] = args[2] = sym_nil;
		goto do_subr;

	    case V_Subr4:
		nargs = 4;
		args[0] = args[1] = args[2] = args[3] = sym_nil;
		goto do_subr;

	    case V_Subr5:
		nargs = 5;
		args[0] = args[1] = args[2] = args[3] = args[4] = sym_nil;
do_subr:
		PUSHGCN(gcvn_args, args, nargs);
		PUSHGC(gcv_obj, obj);
		for(i = 0; i < nargs; i++)
		{
		    if(CONSP(arglist))
		    {
			if(!(args[i] = cmd_eval(VCAR(arglist))))
			{
			    POPGC; POPGCN;
			    goto end;
			}
			arglist = VCDR(arglist);
		    }
		    else
			break;
		}
		POPGC; POPGCN;
		switch(type)
		{
		case V_Subr1:
		    result = VSUBR1FUN(funcobj)(args[0]);
		    break;
		case V_Subr2:
		    result = VSUBR2FUN(funcobj)(args[0], args[1]);
		    break;
		case V_Subr3:
		    result = VSUBR3FUN(funcobj)(args[0], args[1], args[2]);
		    break;
		case V_Subr4:
		    result = VSUBR4FUN(funcobj)(args[0], args[1],
						args[2], args[3]);
		    break;
		case V_Subr5:
		    result = VSUBR5FUN(funcobj)(args[0], args[1], args[2],
						args[3], args[4]);
		    break;
		}
		break;

	    case V_SF:
		result = VSFFUN(funcobj)(arglist);
		break;

	    case V_Cons:
		car = VCAR(funcobj);
		if(car == sym_lambda)
		{
		    struct LispCall lc;
		    lc.lc_Next = lisp_call_stack;
		    lc.lc_Fun = VCAR(obj);
		    lc.lc_Args = arglist;
		    lc.lc_ArgsEvalledP = sym_nil;
		    lisp_call_stack = &lc;
		    if(!(result = eval_lambda(funcobj, arglist, TRUE))
		       && throw_value && (VCAR(throw_value) == sym_defun))
		    {
			result = VCDR(throw_value);
			throw_value = NULL;
		    }
		    lisp_call_stack = lc.lc_Next;
		}
		else if(car == sym_macro)
		{
		    funcobj = VCDR(funcobj);
		    if(CONSP(funcobj) && (VCAR(funcobj) == sym_lambda))
		    {
			VALUE form = eval_lambda(funcobj, arglist, FALSE);
			if(form)
			    result = cmd_eval(form);
		    }
		    else
			cmd_signal(sym_invalid_macro, LIST_1(VCAR(obj)));
		}
		else if(car == sym_autoload)
		{
		    PUSHGC(gcv_obj, obj);
		    result = load_autoload(VCAR(obj), funcobj);
		    POPGC;
		    if(result)
		    {
			result = NULL;
			goto again;
		    }
		}
		else
		    cmd_signal(sym_invalid_function, LIST_1(VCAR(obj)));
		break;

	    default:
		cmd_signal(sym_invalid_function, LIST_1(VCAR(obj)));
		break;
	    }
	    break;

	case V_Var:
	    if(!(result = VVARFUN(obj)(NULL)))
		cmd_signal(sym_void_value, LIST_1(obj));
	    break;

	default:
	    result = obj;
	    break;
	}
    }
    else
	cmd_signal(sym_error, LIST_1(MKSTR("Void object to `eval'")));
    /* In case I missed a non-local exit somewhere.  */
    if(result && throw_value)
	result = NULL;
end:
    lisp_depth--;
    return(result);
}

_PR VALUE cmd_eval(VALUE);
DEFUN("eval", cmd_eval, subr_eval, (VALUE obj), V_Subr1, DOC_eval) /*
::doc:eval::
eval FORM

Evaluates FORM and returns its value.
::end:: */
{
    static int DbDepth;
    bool newssflag = TRUE;
    VALUE result;

    TEST_INT;
    if(INT_P || !curr_vw)
	return(NULL);

    if((data_after_gc >= gc_threshold) && !gc_inhibit)
    {
	GCVAL gcv_obj;
	PUSHGC(gcv_obj, obj);
	cmd_garbage_collect(sym_t);
	POPGC;
    }

    if(!single_step_flag)
	return(eval(obj));

    DbDepth++;
    result = NULL;
    if(VSYM(sym_debug_entry)->sym_Function)
    {
	VALUE dbres;
	VALUE dbargs = cmd_cons(obj, cmd_cons(make_number(DbDepth), sym_nil));
	if(dbargs)
	{
	    GCVAL gcv_dbargs;
	    PUSHGC(gcv_dbargs, dbargs);
	    single_step_flag = FALSE;
	    if((dbres = funcall(sym_debug_entry, dbargs)) && CONSP(dbres))
	    {
		switch(VNUM(VCAR(dbres)))
		{
		case 1:
		    /* single step cdr and following stuff  */
		    single_step_flag = TRUE;
		    result = eval(VCDR(dbres));
		    single_step_flag = FALSE;
		    break;
		case 2:
		    /* run through cdr and step following  */
		    result = eval(VCDR(dbres));
		    break;
		case 3:
		    /* run cdr and following  */
		    result = eval(VCDR(dbres));
		    newssflag = FALSE;
		    break;
		case 4:
		    /* result = cdr  */
		    single_step_flag = TRUE;
		    result = VCDR(dbres);
		    single_step_flag = FALSE;
		    break;
		}
		if(result)
		{
		    if(VSYM(sym_debug_exit)->sym_Function)
		    {
			VCAR(dbargs) = result;
			if(!(dbres = funcall(sym_debug_exit, dbargs)))
			    result = NULL;
		    }
		}
	    }
	    POPGC;
	}
    }
    else
    {
	cmd_signal(sym_error, LIST_1(MKSTR("No debugger installed")));
	newssflag = FALSE;
	result = NULL;
    }
    DbDepth--;
    single_step_flag = newssflag;
    return(result);
}

VALUE
funcall(VALUE fun, VALUE arglist)
{
    int type;
    VALUE result = NULL, origfun = fun;
    GCVAL gcv_origfun, gcv_arglist;

    TEST_INT;
    if(INT_P || !curr_vw)
	return(NULL);

#ifdef MINSTACK
    if(STK_SIZE <= MINSTACK)
    {
	STK_WARN("funcall");
	return(cmd_signal(sym_stack_error, sym_nil));
    }
#endif

    if(++lisp_depth > max_lisp_depth)
    {
	lisp_depth--;
	return(cmd_signal(sym_error, LIST_1(MKSTR("max-lisp-depth exceeded, possible infite recursion?"))));
    }

    if((data_after_gc >= gc_threshold) && !gc_inhibit)
    {
	PUSHGC(gcv_origfun, origfun);
	PUSHGC(gcv_arglist, arglist);
	cmd_garbage_collect(sym_t);
	POPGC; POPGC;
    }

again:
    if(SYMBOLP(fun))
    {
	if(VSYM(fun)->sym_Flags & SF_DEBUG)
	    single_step_flag = TRUE;
	fun = cmd_symbol_function(fun, sym_nil);
	if(!fun)
	    goto end;
    }
    switch(type = VTYPE(fun))
    {
	int i, nargs;
	VALUE car, argv[5];
    case V_SubrN:
	result = VSUBRNFUN(fun)(arglist);
	break;

    case V_Subr0:
	result = VSUBR0FUN(fun)();
	break;

    case V_Subr1:
	nargs = 1;
	argv[0] = sym_nil;
	goto do_subr;

    case V_Subr2:
	nargs = 2;
	argv[0] = argv[1] = sym_nil;
	goto do_subr;

    case V_Subr3:
	nargs = 3;
	argv[0] = argv[1] = argv[2] = sym_nil;
	goto do_subr;

    case V_Subr4:
	nargs = 4;
	argv[0] = argv[1] = argv[2] = argv[3] = sym_nil;
	goto do_subr;

    case V_Subr5:
	nargs = 5;
	argv[0] = argv[1] = argv[2] = argv[3] = argv[4] = sym_nil;
do_subr:
	for(i = 0; i < nargs; i++)
	{
	    if(CONSP(arglist))
	    {
		argv[i] = VCAR(arglist);
		arglist = VCDR(arglist);
	    }
	    else
		break;
	}
	switch(type)
	{
	case V_Subr1:
	    result = VSUBR1FUN(fun)(argv[0]);
	    break;
	case V_Subr2:
	    result = VSUBR2FUN(fun)(argv[0], argv[1]);
	    break;
	case V_Subr3:
	    result = VSUBR3FUN(fun)(argv[0], argv[1], argv[2]);
	    break;
	case V_Subr4:
	    result = VSUBR4FUN(fun)(argv[0], argv[1], argv[2], argv[3]);
	    break;
	case V_Subr5:
	    result = VSUBR5FUN(fun)(argv[0], argv[1], argv[2],
				    argv[3], argv[4]);
	    break;
	}
	break;

    case V_Cons:
	car = VCAR(fun);
	if(car == sym_lambda)
	{
	    struct LispCall lc;
	    lc.lc_Next = lisp_call_stack;
	    lc.lc_Fun = origfun;
	    lc.lc_Args = arglist;
	    lc.lc_ArgsEvalledP = sym_t;
	    lisp_call_stack = &lc;
	    if(!(result = eval_lambda(fun, arglist, FALSE))
	       && throw_value && (VCAR(throw_value) == sym_defun))
	    {
		result = VCDR(throw_value);
		throw_value = NULL;
	    }
	    lisp_call_stack = lc.lc_Next;
	}
	else if(car == sym_autoload)
	{
	    PUSHGC(gcv_origfun, origfun);
	    PUSHGC(gcv_arglist, arglist);
	    car = load_autoload(origfun, fun);
	    POPGC; POPGC;
	    if(car)
	    {
		fun = origfun;
		goto again;
	    }
	}
	else
	    cmd_signal(sym_invalid_function, LIST_1(fun));
	break;
    default:
	cmd_signal(sym_invalid_function, LIST_1(fun));
    }
    /* In case I missed a non-local exit somewhere.  */
    if(result && throw_value)
	result = NULL;
end:
    lisp_depth--;
    return(result);
}

_PR VALUE cmd_funcall(VALUE);
DEFUN("funcall", cmd_funcall, subr_funcall, (VALUE args), V_SubrN, DOC_funcall) /*
::doc:funcall::
funcall FUNCTION ARGS...

Calls FUNCTION with arguments ARGS... and returns its result.
::end:: */
{
    if(!CONSP(args))
	return(cmd_signal(sym_bad_arg, list_2(sym_nil, make_number(1))));
    return(funcall(VCAR(args), VCDR(args)));
}

_PR VALUE cmd_progn(VALUE);
DEFUN("progn", cmd_progn, subr_progn, (VALUE args), V_SF, DOC_progn) /*
::doc:progn::
progn FORMS...

Eval's each of the FORMS in order returning the value of the last
one.
::end:: */
{
    VALUE result = sym_nil;
    GCVAL gcv_args;
    PUSHGC(gcv_args, args);
    while(CONSP(args))
    {
	result = cmd_eval(VCAR(args));
	args = VCDR(args);
	TEST_INT;
	if(!result || INT_P)
	    break;
    }
    if(result && !NILP(args))
	result = cmd_eval(args);
    POPGC;
    return(result);
}

VALUE
eval_string(u_char *str, bool isValString)
{
    VALUE res = sym_nil;
    VALUE stream = cmd_cons(make_number(0), sym_nil);
    if(stream)
    {
	VALUE obj;
	int c;
	GCVAL gcv_stream;
	if(isValString)
	    VCDR(stream) = VAL(STRING_HDR(str));
	else
	{
	    if(!(VCDR(stream) = string_dup(str)))
		return(NULL);
	}
	PUSHGC(gcv_stream, stream);
	obj = sym_nil;
	c = stream_getc(stream);
	while(res && (c != EOF) && (obj = readl(stream, &c)))
	{
	    res = cmd_eval(obj);
	    TEST_INT;
	    if(INT_P)
		res = NULL;
	}
	POPGC;
    }
    return(res);
}

VALUE
call_lisp0(VALUE function)
{
    return(funcall(function, sym_nil));
}

VALUE
call_lisp1(VALUE function, VALUE arg1)
{
    return(funcall(function, LIST_1(arg1)));
}

VALUE
call_lisp2(VALUE function, VALUE arg1, VALUE arg2)
{
    return(funcall(function, LIST_2(arg1, arg2)));
}

void
lisp_prin(VALUE strm, VALUE obj)
{
    static int print_level = 0;

    switch(VTYPE(obj))
    {
	u_char tbuf[40];
	int j;
	int print_length;
	VALUE tem;

    case V_Number:
	sprintf(tbuf, "%ld", VNUM(obj));
	stream_puts(strm, tbuf, -1, FALSE);
	break;

    case V_Cons:
	tem = cmd_symbol_value(sym_print_level, sym_t);
	if(tem && NUMBERP(tem) && print_level >= VNUM(tem))
	{
	    stream_puts(strm, "...", 3, FALSE);
	    return;
	}
	print_level++;
	stream_putc(strm, '(');
	tem = cmd_symbol_value(sym_print_length, sym_t);
	print_length = 0;
	while(CONSP(VCDR(obj)))
	{
	    if(tem && NUMBERP(tem) && print_length >= VNUM(tem))
	    {
		stream_puts(strm, "...", 3, FALSE);
		goto cons_out;
	    }
	    print_val(strm, VCAR(obj));
	    obj = VCDR(obj);
	    stream_putc(strm, ' ');
	    TEST_INT;
	    if(INT_P)
		goto cons_out;
	    print_length++;
	}
	if(tem && NUMBERP(tem) && print_length >= VNUM(tem))
	    stream_puts(strm, "...", 3, FALSE);
	else
	{
	    print_val(strm, VCAR(obj));
	    if(!NILP(VCDR(obj)))
	    {
		stream_puts(strm, " . ", -1, FALSE);
		print_val(strm, VCDR(obj));
	    }
	}
    cons_out:
	stream_putc(strm, ')');
	print_level--;
	break;

    case V_Vector:
	stream_putc(strm, '\[');
	for(j = 0; j < VVECT(obj)->vc_Size; j++)
	{
	    if(VVECT(obj)->vc_Array[j])
		print_val(strm, VVECT(obj)->vc_Array[j]);
	    else
		stream_puts(strm, "#<void>", -1, FALSE);
	    if(j != (VVECT(obj)->vc_Size - 1))
		stream_putc(strm, ' ');
	}
	stream_putc(strm, ']');
	break;

    case V_Subr0:
    case V_Subr1:
    case V_Subr2:
    case V_Subr3:
    case V_Subr4:
    case V_Subr5:
    case V_SubrN:
	sprintf(tbuf, "#<subr %s>", VSTR(VXSUBR(obj)->subr_Name));
	stream_puts(strm, tbuf, -1, FALSE);
	break;

    case V_SF:
	sprintf(tbuf, "#<special-form %s>", VSTR(VXSUBR(obj)->subr_Name));
	stream_puts(strm, tbuf, -1, FALSE);
	break;

    case V_Var:
	sprintf(tbuf, "#<var %s>", VSTR(VXSUBR(obj)->subr_Name));
	stream_puts(strm, tbuf, -1, FALSE);
	break;

#ifndef HAVE_SUBPROCESSES
    case V_Process:
	stream_puts(strm, "#<process>", -1, FALSE);
	break;
#endif

    case V_Void:
	stream_puts(strm, "#<void>", -1, FALSE);
	break;

    default:
	stream_puts(strm, "#<unknown object type>", -1, FALSE);
    }
}

void
string_princ(VALUE strm, VALUE obj)
{
    stream_puts(strm, VSTR(obj), -1, TRUE);
}

void
string_print(VALUE strm, VALUE obj)
{
    int len = STRING_LEN(obj);
    u_char *s = VSTR(obj);
    u_char c;
    VALUE escape_newlines = NULL;
    stream_putc(strm, '\"');
    while(len-- > 0)
    {
	switch(c = *s++)
	{
	case '\t':
	case '\n':
	case '\f':
	    if(!escape_newlines)
		escape_newlines = cmd_symbol_value(sym_print_escape_newlines,
						   sym_t);
	    if(VOIDP(escape_newlines) || escape_newlines == sym_nil)
		stream_putc(strm, (int)c);
	    else
		stream_puts(strm, (c == '\t' ? "\\t"
				   : ((c == '\n') ? "\\n" : "\\f")),
			    2, FALSE);
	    break;

	case '\\':
	    stream_puts(strm, "\\\\", 2, FALSE);
	    break;

	case '"':
	    stream_puts(strm, "\\\"", 2, FALSE);
	    break;

	default:
	    stream_putc(strm, (int)c);
	}
    }
    stream_putc(strm, '\"');
}

VALUE
find_member_by_index(VALUE list, int index)
{
    while((--index) && CONSP(list))
    {
	list = VCDR(list);
	TEST_INT;
	if(INT_P)
	    return(sym_nil);
    }
    if(CONSP(list))
	return(VCAR(list));
    return(sym_nil);
}

VALUE
move_down_list(VALUE list, int nodes)
{
    while((nodes--) && CONSP(list))
    {
	list = VCDR(list);
	TEST_INT;
	if(INT_P)
	    return(NULL);
    }
    return(list);
}

int
list_length(VALUE list)
{
    int i = 0;
    while(CONSP(list))
    {
	i++;
	list = VCDR(list);
	TEST_INT;
	if(INT_P)
	    return(i);
    }
    return(i);
}

VALUE
copy_list(VALUE list)
{
    VALUE result;
    VALUE *last = &result;
    while(CONSP(list))
    {
	if(!(*last = cmd_cons(VCAR(list), sym_nil)))
	    return(NULL);
	list = VCDR(list);
	last = &VCDR(*last);
	TEST_INT;
	if(INT_P)
	    return(NULL);
    }
    *last = list;
    return(result);
}

/*
 * Used for easy handling of `var' objects
 */
VALUE
handle_var_int(VALUE val, long *data)
{
    if(val)
    {
	if(NUMBERP(val))
	    *data = VNUM(val);
	return(NULL);
    }
    return(make_number(*data));
}

_PR VALUE cmd_break(void);
DEFUN("break", cmd_break, subr_break, (void), V_Subr0, DOC_break) /*
::doc:break::
break

The next form to be evaluated will be done so through the Lisp debugger.
::end:: */
{
    single_step_flag = TRUE;
    return(sym_t);
}

_PR VALUE cmd_step(VALUE);
DEFUN_INT("step", cmd_step, subr_step, (VALUE form), V_Subr1, DOC_step, "xForm to step through") /*
::doc:step::
step FORM

Use the Lisp debugger to evaluate FORM.
::end:: */
{
    VALUE res;
    bool oldssf = single_step_flag;
    single_step_flag = TRUE;
    res = cmd_eval(form);
    single_step_flag = oldssf;
    return(res);
}

_PR VALUE cmd_macroexpand(VALUE, VALUE);
DEFUN("macroexpand", cmd_macroexpand, subr_macroexpand, (VALUE form, VALUE env), V_Subr2, DOC_macroexpand) /*
::doc:macroexpand::
macroexpand FORM [ENVIRONMENT]

If FORM is a macro call, expand it until it isn't. If ENVIRONMENT is
specified it is an alist of `(MACRO-NAME . DEFINITION)'.
::end:: */
{
    VALUE car;
    GCVAL gcv_form, gcv_env, gcv_car;
    PUSHGC(gcv_form, form);
    PUSHGC(gcv_env, env);
    PUSHGC(gcv_car, car);
top:
    if(CONSP(form))
    {
	car = VCAR(form);
	if(SYMBOLP(car))
	{
	    VALUE tmp;
	    if(!NILP(env) && (tmp = cmd_assq(car, env)) && CONSP(tmp))
	    {
		car = VCDR(tmp);
		form = eval_lambda(car, VCDR(form), FALSE);
		if(form)
		    goto top;
	    }
	    else
	    {
		car = cmd_symbol_function(car, sym_t);
		if(VOIDP(car) || NILP(car))
		    goto end;
		if(CONSP(car) && (VCAR(car) == sym_macro)
		   && (VCAR(VCDR(car)) == sym_lambda))
		{
		    form = eval_lambda(VCDR(car), VCDR(form), FALSE);
		    if(form)
			goto top;
		}
	    }
	}
    }
end:
    POPGC; POPGC; POPGC;
    return(form);
}

_PR VALUE cmd_get_doc_string(VALUE idx);
DEFUN("get-doc-string", cmd_get_doc_string, subr_get_doc_string, (VALUE idx), V_Subr1, DOC_get_doc_string) /*
::doc:get_doc_string::
get-doc-string INDEX

Returns the document-string number INDEX.
::end:: */
{
    DECLARE1(idx, NUMBERP);
    return(cmd_read_file_from_to(MKSTR(DOC_FILE), idx, make_number((int)'\f')));
}

_PR VALUE cmd_add_doc_string(VALUE str);
DEFUN("add-doc-string", cmd_add_doc_string, subr_add_doc_string, (VALUE str), V_Subr1, DOC_add_doc_string) /*
::doc:add_doc_string::
add-doc-string STRING

Appends STRING to the end of the doc-file and returns the index position of
it's first character (a number).
::end:: */
{
    FILE *docs;
    DECLARE1(str, STRINGP);
    docs = fopen(DOC_FILE, "a");
    if(docs)
    {
	int len = STRING_LEN(str);
	VALUE idx = make_number(ftell(docs));
	if(fwrite(VSTR(str), 1, len, docs) != len)
	{
	    return(cmd_signal(sym_file_error,
			      LIST_1(MKSTR("Can't append to doc-file"))));
	}
	putc('\f', docs);
	fclose(docs);
	return(idx);
    }
    return(cmd_signal(sym_file_error,
		      list_2(MKSTR("Can't open doc-file"), MKSTR(DOC_FILE))));
}

_PR VALUE var_debug_on_error(VALUE val);
DEFUN("debug-on-error", var_debug_on_error, subr_debug_on_error, (VALUE val), V_Var, DOC_debug_on_error) /*
::doc:debug_on_error::
When an error is signalled this variable controls whether or not to enter the
Lisp debugger immediately. If the variable's value is t or a list of symbols
- one of which is the signalled error symbol - the debugger is entered.
See `signal'.
::end:: */
{
    if(val)
	debug_on_error = val;
    return(debug_on_error);
}

_PR VALUE cmd_signal(VALUE error, VALUE data);
DEFUN("signal", cmd_signal, subr_signal, (VALUE error, VALUE data), V_Subr2, DOC_signal) /*
::doc:signal::
signal ERROR-SYMBOL DATA

Signal that an error has happened. ERROR-SYMBOL is the name of a symbol
classifying the type of error, it should have a property `error-message' (a
string) with a short description of the error message.
DATA is a list of objects which are relevant to the error -- they will
be made available to any error-handler or printed by the default error
-handler.
::end:: */
{
    VALUE tmp, errlist;
    /* Can only have one error at once.	 */
    if(throw_value)
	return(NULL);
    DECLARE1(error, SYMBOLP);

    errlist = cmd_cons(error, data);

    if(((debug_on_error == sym_t)
	|| (CONSP(debug_on_error) && (tmp = cmd_memq(error, debug_on_error))
	    && !NILP(tmp)))
       && VSYM(sym_debug_error_entry)->sym_Function)
    {
	/* Enter debugger. */
	VALUE old_debug_on_error = debug_on_error;
	GCVAL gcv_odoe;
	bool oldssflag = single_step_flag;
	debug_on_error = sym_nil;
	single_step_flag = FALSE;
	PUSHGC(gcv_odoe, old_debug_on_error);
	tmp = funcall(sym_debug_error_entry, cmd_cons(errlist, sym_nil));
	POPGC;
	debug_on_error = old_debug_on_error;
	if(tmp && (tmp == sym_t))
	    single_step_flag = TRUE;
	else
	    single_step_flag = oldssflag;
    }
    throw_value = cmd_cons(sym_error, errlist);
    return(NULL);
}

_PR VALUE cmd_error_protect(VALUE args);
DEFUN("error-protect", cmd_error_protect, subr_error_protect, (VALUE args), V_SF, DOC_error_protect) /*
::doc:error_protect::
error-protect FORM HANDLERS...

Evaluates FORM with error-handlers in place, if no errors occur return the
value returned by FORM, else the value of whichever handler's body was
evaluated.
Each HANDLER is a list looking like `(ERROR-SYMBOL BODY...)'. If an error
of type ERROR-SYMBOL occurs BODY is evaluated with the symbol `error-info'
temporarily set to `(ERROR-SYMBOL . DATA)' (these were the arguments given to
the `signal' which caused the error).
::end:: */
{
    VALUE res;
    GCVAL gcv_args;
    if(!CONSP(args))
	return(cmd_signal(sym_bad_arg, list_2(sym_nil, make_number(1))));
    PUSHGC(gcv_args, args);
    if(!(res = cmd_eval(VCAR(args))) && throw_value
       && (VCAR(throw_value) == sym_error))
    {
	/* an error.  */
	VALUE errorsym = VCAR(VCDR(throw_value)), handlers = VCDR(args);
	while(CONSP(handlers) && CONSP(VCAR(handlers)))
	{
	    VALUE handler = VCAR(handlers);
	    if((VCAR(handler) == errorsym) || (VCAR(handler) == sym_error))
	    {
		VALUE bindlist = sym_nil;
		GCVAL gcv_bindlist;
		bindlist = bind_symbol(sym_nil, sym_error_info, VCDR(throw_value));
		throw_value = NULL;
		PUSHGC(gcv_bindlist, bindlist);
		res = cmd_progn(VCDR(handler));
		POPGC;
		unbind_symbols(bindlist);
		break;
	    }
	    handlers = VCDR(handlers);
	    TEST_INT;
	    if(INT_P)
	    {
		res = NULL;
		break;
	    }
	}
    }
    POPGC;
    return(res);
}

void
handle_error(VALUE error, VALUE data)
{
    VALUE errstr;
    cursor(curr_vw, CURS_OFF);
    cmd_beep();
    if(!(errstr = cmd_get(error, sym_error_message)) || !STRINGP(errstr))
	errstr = MKSTR("Unknown error");
    switch(list_length(data))
    {
    case 0:
	cmd_format(list_3(sym_t, MKSTR("%s"), errstr));
	break;
    case 1:
	cmd_format(list_4(sym_t, MKSTR("%s: %s"), errstr, VCAR(data)));
	break;
    case 2:
	cmd_format(list_5(sym_t, MKSTR("%s: %s, %s"), errstr,
			  VCAR(data), VCAR(VCDR(data))));
	break;
    case 3:
	cmd_format(cmd_cons(sym_t, list_5(MKSTR("%s: %s, %s, %s"), errstr,
			  VCAR(data), VCAR(VCDR(data)), VCAR(VCDR(VCDR(data))))));
	break;
    default:
	cmd_format(list_3(sym_t, MKSTR("%s: ..."), errstr));
    }
    refresh_world();
    cursor(curr_vw, CURS_ON);
}

VALUE
signal_arg_error(VALUE obj, int argNum)
{
    return(cmd_signal(sym_bad_arg, list_2(obj, make_number(argNum))));
}

VALUE
signal_missing_arg(int argnum)
{
    return cmd_signal(sym_missing_arg, LIST_1(make_number(argnum)));
}

VALUE
mem_error(void)
{
    return(cmd_signal(sym_no_memory, sym_nil));
}

_PR VALUE cmd_backtrace(VALUE strm);
DEFUN("backtrace", cmd_backtrace, subr_backtrace, (VALUE strm), V_Subr1, DOC_backtrace) /*
::doc:backtrace::
backtrace [STREAM]

Prints a backtrace of the current Lisp call stack to STREAM (or to
`standard-output').
The format is something like:
  FUNCTION (ARGLIST) ARGS-EVALLED-P
where ARGS-EVALLED-P is either `t' or `nil', depending on whether or not
ARGLIST had been evaluated or not before being put into the stack.
::end:: */
{
    struct LispCall *lc;
    if(NILP(strm)
       && !(strm = cmd_symbol_value(sym_standard_output, sym_nil)))
    {
	return(cmd_signal(sym_bad_arg, list_2(strm, make_number(1))));
    }
    lc = lisp_call_stack;
    while(lc)
    {
	stream_putc(strm, '\n');
	print_val(strm, lc->lc_Fun);
	stream_putc(strm, ' ');
	print_val(strm, lc->lc_Args);
	stream_putc(strm, ' ');
	print_val(strm, lc->lc_ArgsEvalledP);
	lc = lc->lc_Next;
    }
    return(sym_t);
}

_PR VALUE var_max_lisp_depth(VALUE val);
DEFUN("max-lisp-depth", var_max_lisp_depth, subr_max_lisp_depth, (VALUE val), V_Var, DOC_max_lisp_depth) /*
::doc:max_lisp_depth::
The maximum number of times that eval and funcall can be called recursively.
This is intended to stop infinite recursion, if the default value of 250 is
too small (you get errors in normal use) set it to something larger.
::end:: */
{
    return(handle_var_int(val, &max_lisp_depth));
}

void
lisp_init(void)
{
    INTERN(sym_quote, "quote");
    INTERN(sym_lambda, "lambda");
    INTERN(sym_macro, "macro");
    INTERN(sym_autoload, "autoload");
    INTERN(sym_function, "function");
    INTERN(sym_standard_input, "standard-input");
    INTERN(sym_standard_output, "standard-output");
    INTERN(sym_defun, "defun");
    INTERN(sym_debug_entry, "debug-entry");
    INTERN(sym_debug_exit, "debug-exit");
    INTERN(sym_debug_error_entry, "debug-error-entry");
    INTERN(sym_amp_optional, "&optional");
    INTERN(sym_amp_rest, "&rest");
    INTERN(sym_amp_aux, "&aux");
    mark_static(&throw_value);
    ADD_SUBR(subr_eval);
    ADD_SUBR(subr_funcall);
    ADD_SUBR(subr_progn);
    ADD_SUBR(subr_break);
    ADD_SUBR(subr_step);
    ADD_SUBR(subr_macroexpand);
    ADD_SUBR(subr_get_doc_string);
    ADD_SUBR(subr_add_doc_string);
    ADD_SUBR(subr_debug_on_error);
    ADD_SUBR(subr_signal);
    ADD_SUBR(subr_error_protect);
    ADD_SUBR(subr_backtrace);
    ADD_SUBR(subr_max_lisp_depth);

    /* Stuff for error-handling */
    debug_on_error = sym_nil;
    INTERN(sym_error_message, "error-message");
    INTERN(sym_error, "error");
    cmd_put(sym_error, sym_error_message, MKSTR("Error"));
    INTERN(sym_invalid_function, "invalid-function");
    cmd_put(sym_invalid_function, sym_error_message, MKSTR("Invalid function"));
    INTERN(sym_void_function, "void-function");
    cmd_put(sym_void_function, sym_error_message, MKSTR("Function value is void"));
    INTERN(sym_void_value, "void-value");
    cmd_put(sym_void_value, sym_error_message, MKSTR("Value as variable is void"));
    INTERN(sym_bad_arg, "bad-arg");
    cmd_put(sym_bad_arg, sym_error_message, MKSTR("Bad argument"));
    INTERN(sym_invalid_read_syntax, "invalid-read-syntax");
    cmd_put(sym_invalid_read_syntax, sym_error_message, MKSTR("Invalid read syntax"));
    INTERN(sym_end_of_stream, "end-of-stream");
    cmd_put(sym_end_of_stream, sym_error_message, MKSTR("Premature end of stream"));
    INTERN(sym_invalid_lambda_list, "invalid-lambda-list");
    cmd_put(sym_invalid_lambda_list, sym_error_message, MKSTR("Invalid lambda-list"));
    INTERN(sym_missing_arg, "missing-arg");
    cmd_put(sym_missing_arg, sym_error_message, MKSTR("Required argument missing"));
    INTERN(sym_invalid_macro, "invalid-macro");
    cmd_put(sym_invalid_macro, sym_error_message, MKSTR("Invalid macro definition"));
    INTERN(sym_invalid_autoload, "invalid-autoload");
    cmd_put(sym_invalid_autoload, sym_error_message, MKSTR("Invalid autoload definition"));
    INTERN(sym_no_catcher, "no-catcher");
    cmd_put(sym_no_catcher, sym_error_message, MKSTR("No catch'er for throw"));
    INTERN(sym_buffer_read_only, "buffer-read-only");
    cmd_put(sym_buffer_read_only, sym_error_message, MKSTR("Buffer is read-only"));
    INTERN(sym_bad_event_desc, "bad_event_desc");
    cmd_put(sym_bad_event_desc, sym_error_message, MKSTR("Invalid event description"));
    INTERN(sym_file_error, "file-error");
    cmd_put(sym_file_error, sym_error_message, MKSTR("File error"));
    INTERN(sym_invalid_stream, "invalid-stream");
    cmd_put(sym_invalid_stream, sym_error_message, MKSTR("Invalid stream"));
    INTERN(sym_setting_constant, "setting-constant");
    cmd_put(sym_setting_constant, sym_error_message, MKSTR("Attempt to set value of constant"));
    INTERN(sym_process_error, "process-error");
    cmd_put(sym_process_error, sym_error_message, MKSTR("Process error"));
    INTERN(sym_invalid_area, "invalid-area");
    cmd_put(sym_invalid_area, sym_error_message, MKSTR("Invalid area"));
#ifdef MINSTACK
    INTERN(sym_stack_error, "stack-error");
    cmd_put(sym_stack_error, sym_error_message, MKSTR("Stack overflow"));
#endif
    INTERN(sym_no_memory, "no-memory");
    cmd_put(sym_no_memory, sym_error_message, MKSTR("No free memory"));
    INTERN(sym_user_interrupt, "user-interrupt");
    cmd_put(sym_user_interrupt, sym_error_message, MKSTR("User interrupt!"));
    INTERN(sym_arith_error, "arith-error");
    cmd_put(sym_arith_error, sym_error_message, MKSTR("Arithmetic error"));
    INTERN(sym_window_error, "window-error");
    cmd_put(sym_window_error, sym_error_message, MKSTR("Window error"));
    INTERN(sym_invalid_pos, "invalid-pos");
    cmd_put(sym_invalid_pos, sym_error_message, MKSTR("Invalid position"));
    
    INTERN(sym_error_info, "error-info");

    int_cell = cmd_cons(sym_user_interrupt, sym_nil);
    mark_static(&int_cell);

    INTERN(sym_print_escape_newlines, "print-escape-newlines");
    INTERN(sym_print_length, "print-length");
    INTERN(sym_print_level, "print-level");
    DOC_VAR(sym_print_escape_newlines, DOC_print_escape_newlines);
    DOC_VAR(sym_print_length, DOC_print_length);
    DOC_VAR(sym_print_level, DOC_print_level);
    VSYM(sym_print_escape_newlines)->sym_Value = sym_nil;
    VSYM(sym_print_length)->sym_Value = sym_nil;
    VSYM(sym_print_level)->sym_Value = sym_nil;
}

