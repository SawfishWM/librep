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

#include "jade.h"
#include <lib/jade_protos.h>

#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

_PR VALUE readl(VALUE, int *);

_PR VALUE bindlambdalist(VALUE lambdaList, VALUE argList, bool eval_args);
_PR VALUE eval_lambda(VALUE, VALUE, bool eval_args);
_PR VALUE load_autoload(VALUE, VALUE, bool);
_PR VALUE funcall(VALUE fun, VALUE arglist, bool eval_args);

_PR VALUE call_lisp0(VALUE);
_PR VALUE call_lisp1(VALUE, VALUE);
_PR VALUE call_lisp2(VALUE, VALUE, VALUE);
_PR VALUE call_lisp3(VALUE, VALUE, VALUE, VALUE);
_PR VALUE call_lisp4(VALUE, VALUE, VALUE, VALUE, VALUE);

_PR void lisp_prin(VALUE, VALUE);
_PR void string_princ(VALUE, VALUE);
_PR void string_print(VALUE, VALUE);

_PR int list_length(VALUE);
_PR VALUE copy_list(VALUE);
_PR VALUE handle_var_int(VALUE, int *);
_PR VALUE handle_var_long_int(VALUE, long *);

_PR bool compare_error(VALUE error, VALUE handler);
_PR void handle_error(VALUE, VALUE);
_PR VALUE signal_arg_error(VALUE, int);
_PR VALUE signal_missing_arg(int argnum);
_PR VALUE mem_error(void);

_PR void lisp_init(void);

_PR VALUE sym_debug_entry, sym_debug_exit, sym_debug_error_entry;
DEFSYM(debug_entry, "debug-entry");
DEFSYM(debug_exit, "debug-exit");
DEFSYM(debug_error_entry, "debug-error-entry");

_PR VALUE sym_quote, sym_lambda, sym_macro, sym_autoload, sym_function;
DEFSYM(quote, "quote");
DEFSYM(backquote, "backquote");
DEFSYM(backquote_unquote, "backquote-unquote");
DEFSYM(backquote_splice, "backquote-splice");
DEFSYM(lambda, "lambda");
DEFSYM(macro, "macro");
DEFSYM(autoload, "autoload");
DEFSYM(function, "function");

_PR VALUE sym_standard_input, sym_standard_output;
DEFSYM(standard_input, "standard-input");
DEFSYM(standard_output, "standard-output");

_PR VALUE sym_amp_optional, sym_amp_rest, sym_amp_aux;
DEFSYM(amp_optional, "&optional");
DEFSYM(amp_rest, "&rest");
DEFSYM(amp_aux, "&aux");

/* When a `throw' happens a function stuffs a cons-cell in here with,
   (TAG . VALUE).
   An error is the above with TAG=sym_error and VALUE a list of relevant
   data. */
_PR volatile VALUE throw_value;
volatile VALUE throw_value;

/* This cons cell is used for interrupts. We don't know if it's safe to
   call cmd_cons() (maybe in gc?) so this is always valid.  */
_PR VALUE int_cell, term_cell;
VALUE int_cell, term_cell;

_PR VALUE sym_error, sym_error_message, sym_invalid_function;
_PR VALUE sym_void_function, sym_void_value, sym_bad_arg, sym_invalid_read_syntax;
_PR VALUE sym_end_of_stream, sym_invalid_lambda_list, sym_missing_arg;
_PR VALUE sym_invalid_macro, sym_invalid_autoload, sym_no_catcher;
_PR VALUE sym_buffer_read_only, sym_bad_event_desc, sym_file_error;
_PR VALUE sym_invalid_stream, sym_setting_constant, sym_process_error;
_PR VALUE sym_invalid_area, sym_no_memory, sym_user_interrupt, sym_arith_error;
_PR VALUE sym_window_error, sym_invalid_pos, sym_term_interrupt;

DEFSYM(error, "error");
DEFSTRING(err_error, "Error");
DEFSYM(error_message, "error-message");
DEFSYM(invalid_function, "invalid-function");
DEFSTRING(err_invalid_function, "Invalid function");
DEFSYM(void_function, "void-function");
DEFSTRING(err_void_function, "Function value is void");
DEFSYM(void_value, "void-value");
DEFSTRING(err_void_value, "Symbol value is void");
DEFSYM(bad_arg, "bad-arg");
DEFSTRING(err_bad_arg, "Bad argument");
DEFSYM(invalid_read_syntax, "invalid-read-syntax");
DEFSTRING(err_invalid_read_syntax, "Invalid read syntax");
DEFSYM(end_of_stream, "end-of-stream");
DEFSTRING(err_end_of_stream, "Premature end of stream");
DEFSYM(invalid_lambda_list, "invalid-lambda-list");
DEFSTRING(err_invalid_lambda_list, "Invalid lambda list");
DEFSYM(missing_arg, "missing-arg");
DEFSTRING(err_missing_arg, "Required argument missing");
DEFSYM(invalid_macro, "invalid-macro");
DEFSTRING(err_invalid_macro, "Invalid macro definition");
DEFSYM(invalid_autoload, "invalid-autoload");
DEFSTRING(err_invalid_autoload, "Invalid autoload definition");
DEFSYM(no_catcher, "no-catcher");
DEFSTRING(err_no_catcher, "No catcher for throw");
DEFSYM(buffer_read_only, "buffer-read-only");
DEFSTRING(err_buffer_read_only, "Buffer is read-only");
DEFSYM(bad_event_desc, "bad-event-desc");
DEFSTRING(err_bad_event_desc, "Invalid event description");
DEFSYM(file_error, "file-error");
DEFSTRING(err_file_error, "File error");
DEFSYM(invalid_stream, "invalid-stream");
DEFSTRING(err_invalid_stream, "Invalid stream");
DEFSYM(setting_constant, "setting-constant");
DEFSTRING(err_setting_constant, "Attempt to set value of constant");
DEFSYM(process_error, "process-error");
DEFSTRING(err_process_error, "Process error");
DEFSYM(invalid_area, "invalid-area");
DEFSTRING(err_invalid_area, "Invalid area");
DEFSYM(no_memory, "no-memory");
DEFSTRING(err_no_memory, "No free memory");
DEFSYM(user_interrupt, "user-interrupt");
DEFSTRING(err_user_interrupt, "User interrupt!");
DEFSYM(arith_error, "arith-error");
DEFSTRING(err_arith_error, "Arithmetic error");
DEFSYM(window_error, "window-error");
DEFSTRING(err_window_error, "Window error");
DEFSYM(invalid_pos, "invalid-pos");
DEFSTRING(err_invalid_pos, "Invalid position");
DEFSYM(term_interrupt, "term-interrupt");

#ifdef MINSTACK
_PR VALUE sym_stack_error;
DEFSYM(stack_error, "stack-error");
DEFSTRING(err_stack_error, "Out of stack space");
#endif

DEFSYM(debug_on_error, "debug-on-error");
DEFSYM(debug_macros, "debug-macros"); /*
::doc:debug_on_error::
When an error is signalled this variable controls whether or not to enter the
Lisp debugger immediately. If the variable's value is t or a list of symbols
- one of which is the signalled error symbol - the debugger is entered.
See `signal'.
::end::
::doc:debug_macros::
When nil, the debugger isn't entered while expanding macro definitions.
::end:: */

_PR VALUE sym_print_escape, sym_print_length, sym_print_level, sym_newlines;
DEFSYM(print_escape, "print-escape");
DEFSYM(print_length, "print-length");
DEFSYM(print_level, "print-level");
DEFSYM(newlines, "newlines"); /*
::doc:print_escape::
Defines which control characters `print' should quote. Acceptable values
are:
	nil		Only escape double-quote and backslash
	newlines	Escape double-quote, backslash, newline,
			 TAB, and formfeed.
	t		Escape all control codes (characters with a
			 value less than 32), and all characters with
			 a value greater than 126.
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

_PR struct Lisp_Call *lisp_call_stack;
struct Lisp_Call *lisp_call_stack;

static int lisp_depth, max_lisp_depth = 500;

/* Used to avoid costly interrupt checking too often */
_PR int lisp_test_int_counter;
int lisp_test_int_counter = 0;


/* Reading */

/* All of the read-related functions are now stream based. This will
   probably add some (much?) overhead but I think it's worth it?

   The `c' variable which keeps coming up is the lookahead character,
   since each read*() routine normally has to look at the next character
   to see if it's what it wants. If not, this char is given to someone
   else... */

DEFSTRING(nodot, "Nothing to dot second element of cons-cell to");

static VALUE
read_list(VALUE strm, register int *c_p)
{
    VALUE result = sym_nil;
    VALUE last = LISP_NULL;
    *c_p = stream_getc(strm);
    while(1)
    {
	switch(*c_p)
	{
	case EOF:
	    return cmd_signal(sym_end_of_stream, LIST_1(strm));

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
		    return LISP_NULL;
	    }
	    else
	    {
		return cmd_signal(sym_invalid_read_syntax,
				  LIST_1(VAL(&nodot)));
	    }

	case ')':
	case ']':
	    *c_p = stream_getc(strm);
	    return result;
	    
	default:
	    {
		register VALUE this = cmd_cons(sym_nil, sym_nil);
		if(last)
		    VCDR(last) = this;
		else
		    result = this;
		if(!(VCAR(this) = readl(strm, c_p)))
		    return LISP_NULL;
		last = this;
	    }
	}
    }
}

DEFSTRING(buf_overflow, "Internal buffer overflow");
DEFSTRING(int_overflow, "Integer limit exceeded");

/* Could be a symbol or a number */
static VALUE
read_symbol(VALUE strm, int *c_p)
{
#define SYM_BUF_LEN 240
    static VALUE buffer = LISP_NULL;

    VALUE result;
    u_char *buf;
    int c = *c_p;
    int i = 0;

    /* For parsing numbers, while radix != zero, it might still be
       an integer that we're reading. */
    int radix = -1, sign = 1;
    long value = 0;

    if(buffer == LISP_NULL)
    {
	buffer = make_string(SYM_BUF_LEN + 1);
	mark_static(&buffer);
    }

    buf = VSTR(buffer);

    while((c != EOF) && (i < SYM_BUF_LEN))
    {
	switch(c)
	{
	case ' ':  case '\t': case '\n': case '\f':
	case '(':  case ')':  case '[':  case ']':
	case '\'': case '"':  case ';':
	    goto done;

	case '\\':
	    radix = 0;
	    c = stream_getc(strm);
	    if(c == EOF)
		return cmd_signal(sym_end_of_stream, LIST_1(strm));
	    buf[i++] = c;
	    break;

	case '|':
	    radix = 0;
	    c = stream_getc(strm);
	    while((c != EOF) && (c != '|') && (i < SYM_BUF_LEN))
	    {
		buf[i++] = c;
		c = stream_getc(strm);
	    }
	    if(c == EOF)
		return cmd_signal(sym_end_of_stream, LIST_1(strm));
	    break;

	default:
	    if(radix != 0)
	    {
		/* It still may be a number that we're parsing */
		if(i == 0 && (c == '-' || c == '+'))
		    /* A leading sign */
		    sign = (c == '-') ? -1 : 1;
		else
		{
		    switch(radix)
		    {
		    case -1:
			/* Deduce the base next (or that we're not
			   looking at a number) */
			if(!isdigit(c))
			    radix = 0;
			else if(c == '0')
			    radix = 1;	/* octal or hex */
			else
			{
			    radix = 10;
			    value = c - '0';
			}
			break;

		    case 1:
			/* We had a leading zero last character. If
			   this char's an 'x' it's hexadecimal. */
			if(toupper(c) == 'X')
			    radix = 16;
			else if(isdigit(c))
			{
			    radix = 8;
			    value = (value * radix) + (c - '0');
			}
			else
			    radix = 0;
			break;

		    default:
			/* Now we're speculatively reading a number
			   of base radix. */
			if(radix <= 10)
			{
			    if(c >= '0' && c <= ('0' + radix - 1))
				value = value * radix + (c - '0');
			    else
				radix = 0;
			}
			else
			{
			    /* hex */
			    if(isxdigit(c))
			    {
				value = (value * 16
					 + ((c >= '0' && c <= '9')
					    ? c - '0'
					    : 10 + toupper(c) - 'A'));
			    }
			    else
				radix = 0;
			}
		    }
		}
	    }
	    buf[i++] = c;
	}
	c = stream_getc(strm);
    }
    if(i >= SYM_BUF_LEN)
    {
	/* Guess I'd better fix this! */
	return cmd_signal(sym_error, LIST_1(VAL(&buf_overflow)));
    }
done:
    if(radix > 0
       /* Ensure that we don't accept `0x' as hex zero */
       && ((radix != 16) || (i > 2)))
    {
	/* It was a number */
	value *= sign;
	if(value < LISP_MIN_INT || value > LISP_MAX_INT)
	    return cmd_signal(sym_arith_error, LIST_1(VAL(&int_overflow)));
	result = MAKE_INT(value);
    }
    else
    {
	buf[i] = 0;
	set_string_len(buffer, i);
	if(!(result = cmd_find_symbol(VAL(buffer), sym_nil))
	   || (NILP(result) && strcmp(buf, "nil")))
	{
	    VALUE name;
	    if((name = string_dup(buf)) && (result = cmd_make_symbol(name)))
		result = cmd_intern_symbol(result, sym_nil);
	    else
		result = LISP_NULL;
	}
    }
    *c_p = c;
    return result;
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
		VVECT(result)->array[i] =  VCAR(cur);
#if 1
		/* I think it's okay to put the cons cells back onto their
		   freelist. There can't be any references to them??  */
		cons_free(cur);
#endif
		cur = nxt;
	    }
	}
	else
	    result = LISP_NULL;
    }
    else
	result = LISP_NULL;
    return result;
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
		    return mem_error();
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
	return result;
    }
    return mem_error();
}

/* Using the above readlisp*() functions this classifies each type
   of expression and translates it into a lisp object (VALUE).
   Returns NULL in case of error. */
VALUE
readl(VALUE strm, register int *c_p)
{
#ifdef MINSTACK
    if(STK_SIZE <= MINSTACK)
    {
	STK_WARN("read");
	return cmd_signal(sym_stack_error, sym_nil);
    }
#endif
    while(1)
    {
	switch(*c_p)
	{
	    VALUE form;

	case EOF:
	    return sym_nil;

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
	    return read_list(strm, c_p);

	case '\'': case '`': 
	    /* 'X => (quote X)
	       `X => (backquote X) */
	    form = cmd_cons(*c_p == '\'' ? sym_quote : sym_backquote,
			    cmd_cons(sym_nil, sym_nil));
	    if((*c_p = stream_getc(strm)) == EOF)
		goto eof;
	    if((VCAR(VCDR(form)) = readl(strm, c_p)) != LISP_NULL)
		return form;
	    else
		return LISP_NULL;

	case ',':
	    /* ,@X => (backquote-splice X)
	       ,X  => (backquote-unquote X) */
	    form = cmd_cons(sym_backquote_unquote, cmd_cons(sym_nil, sym_nil));
	    switch((*c_p = stream_getc(strm)))
	    {
	    case EOF:
		goto eof;

	    case '@':
		VCAR(form) = sym_backquote_splice;
		if((*c_p = stream_getc(strm)) == EOF)
		    goto eof;
	    }
	    if((VCAR(VCDR(form)) = readl(strm, c_p)) != LISP_NULL)
		return form;
	    else
		return LISP_NULL;

	case '[':
	    return read_vector(strm, c_p);

	case '"':
	    return read_str(strm, c_p);

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
			return MAKE_INT(stream_read_esc(strm, c_p));
		    break;
		default:
		    *c_p = stream_getc(strm);
		    return MAKE_INT(c);
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
		    return LISP_NULL;
		return form;
	    case '[':
		{
		    VALUE vec = read_vector(strm, c_p);
		    if(vec != LISP_NULL)
		    {
			if(VVECT_LEN(vec) >= COMPILED_MIN_SLOTS)
			{
			    VCOMPILED(vec)->car = (VCOMPILED(vec)->car
						   & ~CELL8_TYPE_MASK)
						  | V_Compiled;
			    return vec;
			}
			goto error;
		    }
		    break;
		}
	    default: error:
		return cmd_signal(sym_invalid_read_syntax, LIST_1(strm));
	    }

	default:
	    return read_symbol(strm, c_p);
	}
    }
    /* NOT REACHED */

eof:
    return cmd_signal(sym_end_of_stream, LIST_1(strm));
}


/* Evaluating */

/* Evaluates each element of `list' and builds them into a new list. */
static VALUE
eval_list(VALUE list)
{
    VALUE result = sym_nil;
    VALUE *last = &result;
    GC_root gc_result, gc_list;
    PUSHGC(gc_result, result);
    PUSHGC(gc_list, list);
    while(CONSP(list))
    {
	VALUE tmp;
	if(!(tmp = cmd_eval(VCAR(list))))
	{
	    result = LISP_NULL;
	    break;
	}
	if(!(*last = cmd_cons(tmp, sym_nil)))
	{
	    result = LISP_NULL;
	    break;
	}
	list = VCDR(list);
	last = &VCDR(*last);
	TEST_INT;
	if(INT_P)
	{
	    result = LISP_NULL;
	    break;
	}
    }
    if(result && last && !NILP(list))
	*last = cmd_eval(list);
    POPGC; POPGC;
    return result;
}

/* format of lambda-lists is something like,

   [{required-symbols}] [&optional {optional-symbols}] [&rest symbol]
   [&aux {auxiliary-symbols}]

   NB: auxiliary symbols are set to nil. Also note that the lambdaList
   arg isn't protected from gc by this function; it's assumed that
   this is done by the caller. */
VALUE
bindlambdalist(VALUE lambdaList, VALUE argList, bool eval_args)
{
#define STATE_REQUIRED 1
#define STATE_OPTIONAL 2
#define STATE_REST     3
#define STATE_AUX      4

    VALUE *evalled_args;
    int evalled_nargs;
    VALUE boundlist = sym_nil;

    GC_root gc_arglist, gc_boundlist;
    GC_n_roots gc_evalled_args;

    PUSHGC(gc_arglist, argList);
    PUSHGC(gc_boundlist, boundlist);

    /* Evaluate arguments, and stick them in the evalled_args array */
    if(eval_args)
    {
	int i;
	evalled_nargs = list_length(argList);
	evalled_args = alloca(sizeof(VALUE) * evalled_nargs);
	PUSHGCN(gc_evalled_args, evalled_args, 0);
	for(i = 0; i < evalled_nargs; i++)
	{
	    if((evalled_args[i] = cmd_eval(VCAR(argList))) == LISP_NULL)
		goto error;
	    argList = VCDR(argList);
	    gc_evalled_args.count++;
	}
    }

    if(CONSP(lambdaList))
    {
	int state = STATE_REQUIRED;
	while(CONSP(lambdaList) && SYMBOLP(VCAR(lambdaList)))
	{
	    VALUE argobj;
	    VALUE argspec = VCAR(lambdaList);
	    if(VSTR(VSYM(argspec)->name)[0] == '&')
	    {
		if(argspec == sym_amp_optional)
		{
		    if(state > STATE_OPTIONAL)
		    {
			cmd_signal(sym_invalid_lambda_list,
				   LIST_1(lambdaList));
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
			cmd_signal(sym_invalid_lambda_list,
				   LIST_1(lambdaList));
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
	    case STATE_OPTIONAL:
		if(eval_args && evalled_nargs > 0)
		{
		    argobj = *evalled_args++;
		    evalled_nargs--;
		    gc_evalled_args.count--;
		}
		else if(!eval_args && CONSP(argList))
		{
		    argobj = VCAR(argList);
		    argList = VCDR(argList);
		}
		else if(state == STATE_OPTIONAL)
		    argobj = sym_nil;
		else
		{
		    cmd_signal(sym_missing_arg, LIST_1(argspec));
		    goto error;
		}
		boundlist = bind_symbol(boundlist, argspec, argobj);
		break;

	    case STATE_REST:
		if(eval_args)
		{
		    VALUE list = sym_nil;
		    VALUE *ptr = &list;
		    while(evalled_nargs > 0)
		    {
			*ptr = cmd_cons(*evalled_args++, sym_nil);
			ptr = &(VCDR(*ptr));
			evalled_nargs--;
			gc_evalled_args.count--;
		    }
		    argobj = list;
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
	    {
	    error:
		unbind_symbols(boundlist);
		boundlist = LISP_NULL;
		break;
	    }
	}
    }

    if(eval_args)
	POPGCN;
    POPGC; POPGC;

    return boundlist;
}

VALUE
eval_lambda(VALUE lambdaExp, VALUE argList, bool eval_args)
{
    VALUE result = LISP_NULL;
    if(CONSP(VCDR(lambdaExp)))
    {
	VALUE boundlist;
	GC_root gc_lambdaExp, gc_argList;
	PUSHGC(gc_lambdaExp, lambdaExp);
	PUSHGC(gc_argList, argList);
	lambdaExp = VCDR(lambdaExp);
	boundlist = bindlambdalist(VCAR(lambdaExp), argList, eval_args);
	if(boundlist)
	{
	    GC_root gc_boundlist;
	    PUSHGC(gc_boundlist, boundlist);
	    result = cmd_progn(VCDR(lambdaExp));
	    POPGC;
	    unbind_symbols(boundlist);
	}
	else
	    result = LISP_NULL;
	POPGC; POPGC;
    }
    return result;
}

/* Autoloads a function, FUN is the symbol of the function, ALOAD-DEF is
   the `(autoload FILE-NAME ...)' object. This function may cause a gc.
   Returns the new function-value of FUN, or NULL for an error.
   If IS-VARIABLE is true, then return the value-slot of FUN, but do
   everything else the same. */
DEFSTRING(invl_autoload, "Can only autoload from symbols");
VALUE
load_autoload(VALUE fun, VALUE aload_def, bool is_variable)
{
    if(!SYMBOLP(fun))
    {
	/* Unless the function we're calling is a symbol don't bother.
	   (Because it wouldn't be possible to find the new definition.)  */
	return cmd_signal(sym_invalid_autoload,
			  list_2(fun, VAL(&invl_autoload)));
    }
    else
    {
	VALUE autoload = VCDR(aload_def);
	if(CONSP(autoload) && STRINGP(VCAR(autoload)))
	{
	    VALUE file = VCAR(autoload);
	    GC_root gc_fun, gc_file;
	    VALUE tmp;

	    u_char *old_msg;
	    u_long old_msg_len;
	    save_message(curr_win, &old_msg, &old_msg_len);
	    messagef("Loading %s...", VSTR(file));
	    redisplay_message(curr_win);

	    PUSHGC(gc_fun, fun); PUSHGC(gc_file, file);
	    /* trash the autoload defn, so we don't keep trying to
	       autoload indefinitely.  */
	    VCAR(autoload) = sym_nil;
	    tmp = cmd_load(file, sym_t, sym_nil, sym_nil);
	    POPGC; POPGC;

	    messagef("Loading %s...done.", VSTR(file));
	    redisplay_message(curr_win);
	    restore_message(curr_win, old_msg, old_msg_len);

	    if(tmp && !NILP(tmp))
	    {
		return (!is_variable
			? cmd_symbol_function(fun, sym_nil)
			: cmd_symbol_value(fun, sym_nil));
	    }
	}
	return cmd_signal(sym_invalid_autoload, LIST_1(fun));
    }
}

DEFSTRING(max_depth, "max-lisp-depth exceeded, possible infinite recursion?");

/* Applies ARGLIST to FUN. If EVAL-ARGS is true, all arguments will be
   evaluated first. Note that both FUN and ARGLIST are gc-protected
   for the duration of this function. */
VALUE
funcall(VALUE fun, VALUE arglist, bool eval_args)
{
    int type;
    VALUE result = LISP_NULL;
    struct Lisp_Call lc;

    TEST_INT;
    if(INT_P || !curr_vw)
	return LISP_NULL;

#ifdef MINSTACK
    if(STK_SIZE <= MINSTACK)
    {
	STK_WARN("funcall");
	return cmd_signal(sym_stack_error, sym_nil);
    }
#endif

    if(++lisp_depth > max_lisp_depth)
    {
	lisp_depth--;
	return cmd_signal(sym_error, LIST_1(VAL(&max_depth)));
    }

    /* Note that by putting FUN and ARGLIST in the backtrace,
       they're automagically protected from gc. */
    lc.fun = fun;
    lc.args = arglist;
    lc.args_evalled_p = eval_args ? sym_nil : sym_t;
    lc.next = lisp_call_stack;
    lisp_call_stack = &lc;

    if((data_after_gc >= gc_threshold) && !gc_inhibit)
	cmd_garbage_collect(sym_t);

again:
    if(SYMBOLP(fun))
    {
	if(VSYM(fun)->car & SF_DEBUG)
	    single_step_flag = TRUE;
	fun = cmd_symbol_function(fun, sym_nil);
	if(!fun)
	    goto end;
    }
    switch(type = VTYPE(fun))
    {
	int i, nargs;
	VALUE car, argv[5];
	GC_n_roots gc_argv;

    case V_SubrN:
	if(eval_args)
	{
	    arglist = eval_list(arglist);
	    if(arglist == LISP_NULL)
		goto end;
	}
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
	/* FALL THROUGH */

    do_subr:
	if(eval_args)
	    PUSHGCN(gc_argv, argv, nargs);
	for(i = 0; i < nargs; i++)
	{
	    if(CONSP(arglist))
	    {
		if(!eval_args)
		    argv[i] = VCAR(arglist);
		else
		{
		    argv[i] = cmd_eval(VCAR(arglist));
		    if(argv[i] == LISP_NULL)
		    {
			POPGCN;
			goto end;
		    }
		}
		arglist = VCDR(arglist);
	    }
	    else
		break;
	}
	if(eval_args)
	    POPGCN;
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
	    /* eval_lambda() expanded inline. */
	    if(CONSP(VCDR(fun)))
	    {
		VALUE boundlist;
		fun = VCDR(fun);
		boundlist = bindlambdalist(VCAR(fun), arglist, eval_args);
		if(boundlist != LISP_NULL)
		{
		    GC_root gc_boundlist;
		    PUSHGC(gc_boundlist, boundlist);
		    result = cmd_progn(VCDR(fun));
		    POPGC;
		    unbind_symbols(boundlist);
		}
		else
		    result = LISP_NULL;
	    }
	}
	else if(car == sym_autoload)
	{
	    /* lc.fun contains the original function description,
	       i.e. usually a symbol */
	    car = load_autoload(lc.fun, fun, FALSE);
	    if(car)
	    {
		fun = lc.fun;
		goto again;
	    }
	}
	else
	    cmd_signal(sym_invalid_function, LIST_1(fun));
	break;

    case V_Compiled:
	{
	    VALUE boundlist;

	    if(COMPILED_MACRO_P(fun))
		goto invalid;

	    boundlist = bindlambdalist(COMPILED_LAMBDA(fun),
				       arglist, eval_args);
	    if(boundlist != LISP_NULL)
	    {
		GC_root gc_boundlist;
		PUSHGC(gc_boundlist, boundlist);
		result = cmd_jade_byte_code(COMPILED_CODE(fun),
					    COMPILED_CONSTANTS(fun),
					    MAKE_INT(COMPILED_STACK(fun)));
		POPGC;
		unbind_symbols(boundlist);
	    }
	    else
		result = LISP_NULL;
	    break;
	}

    default: invalid:
	cmd_signal(sym_invalid_function, LIST_1(fun));
    }

    /* In case I missed a non-local exit somewhere.  */
    if(throw_value != LISP_NULL)
	result = LISP_NULL;

end:
    lisp_call_stack = lc.next;
    lisp_depth--;
    return result;
}

_PR VALUE cmd_funcall(VALUE);
DEFUN("funcall", cmd_funcall, subr_funcall, (VALUE args), V_SubrN, DOC_funcall) /*
::doc:funcall::
funcall FUNCTION ARGS...

Calls FUNCTION with arguments ARGS... and returns the result.
::end:: */
{
    if(!CONSP(args))
	return signal_missing_arg(1);
    else
	return funcall(VCAR(args), VCDR(args), FALSE);
}

DEFSTRING(void_obj, "Void object to `eval'");

static VALUE
eval(VALUE obj)
{
    VALUE result = LISP_NULL;

#ifdef MINSTACK
    if(STK_SIZE <= MINSTACK)
    {
	STK_WARN("eval");
	return cmd_signal(sym_stack_error, sym_nil);
    }
#endif

    switch(VTYPE(obj))
    {
	VALUE funcobj;

    case V_Symbol:
	result = cmd_symbol_value(obj, sym_nil);
	break;

    case V_Cons:
	funcobj = VCAR(obj);
	if(SYMBOLP(funcobj))
	{
	    if(VSYM(funcobj)->car & SF_DEBUG)
		single_step_flag = TRUE;
	    funcobj = cmd_symbol_function(funcobj, sym_nil);
	    if(funcobj == LISP_NULL)
		goto end;
	}
	if(VCELL8_TYPEP(funcobj, V_SF))
	{
	    result = VSFFUN(funcobj)(VCDR(obj));
	}
	else if((CONSP(funcobj) && VCAR(funcobj) == sym_macro)
		|| (COMPILEDP(funcobj) && COMPILED_MACRO_P(funcobj)))
	{
	    /* A macro */
	    VALUE form;
	    if(single_step_flag
	       && (form = cmd_symbol_value(sym_debug_macros, sym_t))
	       && NILP(form))
	    {
		/* Debugging macros gets tedious; don't
		   bother when debug-macros is nil. */
		single_step_flag = FALSE;
		form = cmd_macroexpand(obj, sym_nil);
		single_step_flag = TRUE;
	    }
	    else
		form = cmd_macroexpand(obj, sym_nil);
	    if(form != LISP_NULL)
		result = cmd_eval(form);
	}
	else
	    result = funcall(VCAR(obj), VCDR(obj), TRUE);
	break;

    case V_Var:
	if(!(result = VVARFUN(obj)(LISP_NULL)))
	    cmd_signal(sym_void_value, LIST_1(obj));
	break;

    default:
	result = obj;
	break;
    }

    /* In case I missed a non-local exit somewhere.  */
    if(throw_value != LISP_NULL)
	result = LISP_NULL;

end:
    return result;
}

DEFSTRING(no_debug, "No debugger installed");

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
	return LISP_NULL;

    if((data_after_gc >= gc_threshold) && !gc_inhibit)
    {
	GC_root gc_obj;
	PUSHGC(gc_obj, obj);
	cmd_garbage_collect(sym_t);
	POPGC;
    }

    if(!single_step_flag)
	return eval(obj);

    DbDepth++;
    result = LISP_NULL;
    if(VSYM(sym_debug_entry)->function)
    {
	VALUE dbres;
	VALUE dbargs = cmd_cons(obj, cmd_cons(MAKE_INT(DbDepth), sym_nil));
	if(dbargs)
	{
	    GC_root gc_dbargs;
	    PUSHGC(gc_dbargs, dbargs);
	    single_step_flag = FALSE;
	    if((dbres = funcall(sym_debug_entry, dbargs, FALSE))
	       && CONSP(dbres))
	    {
		switch(VINT(VCAR(dbres)))
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
		    if(VSYM(sym_debug_exit)->function)
		    {
			VCAR(dbargs) = result;
			if(!(dbres = funcall(sym_debug_exit, dbargs, FALSE)))
			    result = LISP_NULL;
		    }
		}
	    }
	    POPGC;
	}
    }
    else
    {
	cmd_signal(sym_error, LIST_1(VAL(&no_debug)));
	newssflag = FALSE;
	result = LISP_NULL;
    }
    DbDepth--;
    single_step_flag = newssflag;
    return result;
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
    GC_root gc_args;
    PUSHGC(gc_args, args);
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
    return result;
}

VALUE
call_lisp0(VALUE function)
{
    return funcall(function, sym_nil, FALSE);
}

VALUE
call_lisp1(VALUE function, VALUE arg1)
{
    return funcall(function, LIST_1(arg1), FALSE);
}

VALUE
call_lisp2(VALUE function, VALUE arg1, VALUE arg2)
{
    return funcall(function, LIST_2(arg1, arg2), FALSE);
}

VALUE
call_lisp3(VALUE function, VALUE arg1, VALUE arg2, VALUE arg3)
{
    return funcall(function, LIST_3(arg1, arg2, arg3), FALSE);
}

VALUE
call_lisp4(VALUE function, VALUE arg1, VALUE arg2, VALUE arg3, VALUE arg4)
{
    return funcall(function, LIST_4(arg1, arg2, arg3, arg4), FALSE);
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

    case V_Int:
#ifdef HAVE_SNPRINTF
	snprintf(tbuf, sizeof(tbuf), "%ld", VINT(obj));
#else
	sprintf(tbuf, "%ld", VINT(obj));
#endif
	stream_puts(strm, tbuf, -1, FALSE);
	break;

    case V_Cons:
	tem = cmd_symbol_value(sym_print_level, sym_t);
	if(tem && INTP(tem) && print_level >= VINT(tem))
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
	    if(tem && INTP(tem) && print_length >= VINT(tem))
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
	if(tem && INTP(tem) && print_length >= VINT(tem))
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

    case V_Compiled:
	stream_putc(strm, '#');
	/* FALL THROUGH */
    case V_Vector:
	{
	    int len = VVECT_LEN(obj);
	    stream_putc(strm, '\[');
	    for(j = 0; j < len; j++)
	    {
		if(VVECT(obj)->array[j])
		    print_val(strm, VVECT(obj)->array[j]);
		else
		    stream_puts(strm, "#<void>", -1, FALSE);
		if(j != (len - 1))
		    stream_putc(strm, ' ');
	    }
	    stream_putc(strm, ']');
	    break;
	}

    case V_Subr0:
    case V_Subr1:
    case V_Subr2:
    case V_Subr3:
    case V_Subr4:
    case V_Subr5:
    case V_SubrN:
#ifdef HAVE_SNPRINTF
	snprintf(tbuf, sizeof(tbuf), "#<subr %s>", VSTR(VXSUBR(obj)->name));
#else
	sprintf(tbuf, "#<subr %s>", VSTR(VXSUBR(obj)->name));
#endif
	stream_puts(strm, tbuf, -1, FALSE);
	break;

    case V_SF:
#ifdef HAVE_SNPRINTF
	snprintf(tbuf, sizeof(tbuf),
		 "#<special-form %s>", VSTR(VXSUBR(obj)->name));
#else
	sprintf(tbuf, "#<special-form %s>", VSTR(VXSUBR(obj)->name));
#endif
	stream_puts(strm, tbuf, -1, FALSE);
	break;

    case V_Var:
#ifdef HAVE_SNPRINTF
	snprintf(tbuf, sizeof(tbuf), "#<var %s>", VSTR(VXSUBR(obj)->name));
#else
	sprintf(tbuf, "#<var %s>", VSTR(VXSUBR(obj)->name));
#endif
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
    stream_puts(strm, VPTR(obj), -1, TRUE);
}

void
string_print(VALUE strm, VALUE obj)
{
    int len = STRING_LEN(obj);
    u_char *s = VSTR(obj);
    u_char c;

    bool escape_all, escape_newlines;
    VALUE tem = cmd_symbol_value(sym_print_escape, sym_t);
    if(tem == sym_newlines)
	escape_all = FALSE, escape_newlines = TRUE;
    else if(tem == sym_t)
	escape_all = TRUE, escape_newlines = TRUE;
    else
	escape_all = FALSE, escape_newlines = FALSE;

    stream_putc(strm, '\"');
    while(len-- > 0)
    {
	c = *s++;
	if(escape_all && (c < 32 || c > 126))
	{
	    stream_putc(strm, '\\');
	    stream_putc(strm, '0' + c / 64);
	    stream_putc(strm, '0' + (c % 64) / 8);
	    stream_putc(strm, '0' + c % 8);
	}
	else
	{
	    switch(c)
	    {
	    case '\t':
	    case '\n':
	    case '\f':
		if(!escape_newlines)
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
    }
    stream_putc(strm, '\"');
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
	    return i;
    }
    return i;
}

VALUE
copy_list(VALUE list)
{
    VALUE result;
    VALUE *last = &result;
    while(CONSP(list))
    {
	if(!(*last = cmd_cons(VCAR(list), sym_nil)))
	    return LISP_NULL;
	list = VCDR(list);
	last = &VCDR(*last);
	TEST_INT;
	if(INT_P)
	    return LISP_NULL;
    }
    *last = list;
    return result;
}

/* Used for easy handling of `var' objects */
VALUE
handle_var_int(VALUE val, int *data)
{
    if(val)
    {
	if(INTP(val))
	    *data = VINT(val);
	return LISP_NULL;
    }
    else
	return MAKE_INT(*data);
}

/* Similar, but for variables containing greater than 24 bits of data,
   passed around as a cons cell containing two integers */
VALUE
handle_var_long_int(VALUE val, long *data)
{
    if(val)
    {
	if(LONG_INTP(val))
	    *data = VLONG_INT(val);
	return LISP_NULL;
    }
    else
	return MAKE_LONG_INT(*data);
}

_PR VALUE cmd_break(void);
DEFUN("break", cmd_break, subr_break, (void), V_Subr0, DOC_break) /*
::doc:break::
break

The next form to be evaluated will be done so through the Lisp debugger.
::end:: */
{
    single_step_flag = TRUE;
    return sym_t;
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
    return res;
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
    GC_root gc_form, gc_env, gc_car;
    PUSHGC(gc_form, form);
    PUSHGC(gc_env, env);
    PUSHGC(gc_car, car);
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
	    }
	    else
	    {
		car = cmd_symbol_function(car, sym_t);
		if(CONSP(car))
		{
		    if(VCAR(car) != sym_macro)
			goto end;
		    car = VCDR(car);
		}
	    }
	    if(VOIDP(car) || NILP(car))
		goto end;
	    if(CONSP(car) && VCAR(car) == sym_lambda)
	    {
		form = eval_lambda(car, VCDR(form), FALSE);
		if(form != LISP_NULL)
		    goto top;
	    }
	    else if(COMPILEDP(car) && COMPILED_MACRO_P(car))
	    {
		VALUE boundlist;
		struct Lisp_Call lc;

		lc.next = lisp_call_stack;
		lc.fun = VCAR(form);
		lc.args = VCDR(form);
		lc.args_evalled_p = TRUE;
		lisp_call_stack = &lc;
		boundlist = bindlambdalist(COMPILED_LAMBDA(car),
					   VCDR(form), FALSE);
		if(boundlist != LISP_NULL)
		{
		    GC_root gc_boundlist;
		    PUSHGC(gc_boundlist, boundlist);
		    form = cmd_jade_byte_code(COMPILED_CODE(car),
					      COMPILED_CONSTANTS(car),
					      MAKE_INT(COMPILED_STACK(car)));
		    POPGC;
		    unbind_symbols(boundlist);
		}
		else
		    form = LISP_NULL;
		lisp_call_stack = lc.next;

		if(form != LISP_NULL)
		    goto top;
	    }
	}
    }
end:
    POPGC; POPGC; POPGC;
    return form;
}

_PR VALUE cmd_signal(VALUE error, VALUE data);
DEFUN("signal", cmd_signal, subr_signal, (VALUE error, VALUE data), V_Subr2, DOC_signal) /*
::doc:signal::
signal ERROR-SYMBOL DATA

Signal that an error has happened. ERROR-SYMBOL is the name of a symbol
classifying the type of error, it should have a property `error-message'
(a string) with a short description of the error message.
DATA is a list of objects which are relevant to the error -- they will
be made available to any error-handler or printed by the default error
handler.
::end:: */
{
    VALUE tmp, errlist, on_error;
    /* Can only have one error at once.	 */
    if(throw_value)
	return LISP_NULL;
    DECLARE1(error, SYMBOLP);

    errlist = cmd_cons(error, data);
    on_error = cmd_symbol_value(sym_debug_on_error, sym_t);
    if(((on_error != LISP_NULL && on_error == sym_t)
	|| (CONSP(on_error) && (tmp = cmd_memq(error, on_error)) && !NILP(tmp)))
       && VSYM(sym_debug_error_entry)->function)
    {
	/* Enter debugger. */
	GC_root gc_on_error;
	bool oldssflag = single_step_flag;
	cmd_set(sym_debug_on_error, sym_nil);
	single_step_flag = FALSE;
	PUSHGC(gc_on_error, on_error);
	tmp = funcall(sym_debug_error_entry,
		      cmd_cons(errlist, sym_nil), FALSE);
	POPGC;
	cmd_set(sym_debug_on_error, on_error);
	if(tmp && (tmp == sym_t))
	    single_step_flag = TRUE;
	else
	    single_step_flag = oldssflag;
    }
    throw_value = cmd_cons(sym_error, errlist);
    return LISP_NULL;
}

/* For an error ERROR (the cdr of throw_value), if it matches the error
   handler HANDLER (the car of the handler list), return TRUE. */
bool
compare_error(VALUE error, VALUE handler)
{
    if(CONSP(error))
    {
	VALUE error_sym = VCAR(error);
	if(SYMBOLP(handler) && (error_sym == handler || handler == sym_error))
	    return TRUE;
	else if(CONSP(handler))
	{
	    handler = cmd_memq(error_sym, handler);
	    return handler != LISP_NULL && !NILP(handler);
	}
    }
    return FALSE;
}

_PR VALUE cmd_condition_case(VALUE args);
DEFUN("condition-case", cmd_condition_case, subr_condition_case, (VALUE args), V_SF, DOC_condition_case) /*
::doc:condition_case::
condition-case VAR FORM HANDLERS...

Evaluates FORM with error-handlers in place, if no errors occur return the
value returned by FORM, else the value of whichever handler's body was
evaluated.

Each HANDLER is a list of `(ERROR BODY...)'. ERROR defines which types of
errors the handler catches, either a symbol or a list of symbols. The
special symbol `error' matches all types of errors.

If VAR is non-nil it's a symbol whose values is bound to
`(ERROR-SYMBOL . DATA)' while the handler is evaluated (these are the
arguments given to `signal' when the error was raised).
::end:: */
{
    VALUE var, res = LISP_NULL;
    GC_root gc_args;
    if(!CONSP(args))
	return signal_missing_arg(1);
    PUSHGC(gc_args, args);
    var = VCAR(args);
    args = VCDR(args);
    if(!CONSP(args))
	return sym_nil;
    res = cmd_eval(VCAR(args));
    args = VCDR(args);
    if(res == LISP_NULL && throw_value != LISP_NULL
       && (VCAR(throw_value) == sym_error) && CONSP(VCDR(throw_value)))
    {
	/* an error.  */
	VALUE error = VCDR(throw_value);
	VALUE throw_val = throw_value;
	throw_value = LISP_NULL;
	while(CONSP(args) && CONSP(VCAR(args)))
	{
	    VALUE handler = VCAR(args);
	    if(compare_error(error, VCAR(handler)))
	    {
		VALUE bindlist = sym_nil;
		GC_root gc_bindlist;
		if(SYMBOLP(var) && !NILP(var))
		{
		    bindlist = bind_symbol(sym_nil, var, error);
		    PUSHGC(gc_bindlist, bindlist);
		}
		res = cmd_progn(VCDR(handler));
		if(SYMBOLP(var) && !NILP(var))
		{
		    POPGC;
		    unbind_symbols(bindlist);
		}
		break;
	    }
	    args = VCDR(args);
	    TEST_INT;
	    if(INT_P)
	    {
		res = LISP_NULL;
		break;
	    }
	    if(!CONSP(args))
		throw_value = throw_val; /* reinstall the error */
	}
    }
    POPGC;
    return res;
}

DEFSTRING(unknown_err, "Unknown error");
DEFSTRING(one_err_fmt, "%s");
DEFSTRING(two_err_fmt, "%s: %s");
DEFSTRING(three_err_fmt, "%s: %s, %s");
DEFSTRING(four_err_fmt, "%s: %s, %s, %s");
DEFSTRING(n_err_fmt, "%s: ...");

void
handle_error(VALUE error, VALUE data)
{
    VALUE errstr;
    cmd_beep();
    if(!(errstr = cmd_get(error, sym_error_message)) || !STRINGP(errstr))
	errstr = VAL(&unknown_err);
    switch(list_length(data))
    {

    case 0:
	cmd_format(list_3(sym_t, VAL(&one_err_fmt), errstr));
	break;
    case 1:
	cmd_format(list_4(sym_t, VAL(&two_err_fmt), errstr, VCAR(data)));
	break;
    case 2:
	cmd_format(list_5(sym_t, VAL(&three_err_fmt), errstr,
			  VCAR(data), VCAR(VCDR(data))));
	break;
    case 3:
	cmd_format(cmd_cons(sym_t, list_5(VAL(&four_err_fmt), errstr,
					  VCAR(data), VCAR(VCDR(data)),
					  VCAR(VCDR(VCDR(data))))));
	break;
    default:
	cmd_format(list_3(sym_t, VAL(&n_err_fmt), errstr));
    }
}

VALUE
signal_arg_error(VALUE obj, int argNum)
{
    return cmd_signal(sym_bad_arg, list_2(obj, MAKE_INT(argNum)));
}

VALUE
signal_missing_arg(int argnum)
{
    return cmd_signal(sym_missing_arg, LIST_1(MAKE_INT(argnum)));
}

VALUE
mem_error(void)
{
    return cmd_signal(sym_no_memory, sym_nil);
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
    struct Lisp_Call *lc;
    if(NILP(strm)
       && !(strm = cmd_symbol_value(sym_standard_output, sym_nil)))
    {
	return cmd_signal(sym_bad_arg, list_2(strm, MAKE_INT(1)));
    }
    lc = lisp_call_stack;
    while(lc)
    {
	stream_putc(strm, '\n');
	print_val(strm, lc->fun);
	stream_putc(strm, ' ');
	print_val(strm, lc->args);
	stream_putc(strm, ' ');
	print_val(strm, lc->args_evalled_p);
	lc = lc->next;
    }
    return sym_t;
}

_PR VALUE var_max_lisp_depth(VALUE val);
DEFUN("max-lisp-depth", var_max_lisp_depth, subr_max_lisp_depth, (VALUE val), V_Var, DOC_max_lisp_depth) /*
::doc:max_lisp_depth::
The maximum number of times that funcall can be called recursively.

This is intended to stop infinite recursion, if the default value of 250 is
too small (you get errors in normal use) set it to something larger.
::end:: */
{
    return handle_var_int(val, &max_lisp_depth);
}

void
lisp_init(void)
{
    INTERN(quote); INTERN(lambda); INTERN(macro);
    INTERN(backquote); INTERN(backquote_unquote); INTERN(backquote_splice);
    INTERN(autoload); INTERN(function);
    INTERN(standard_input); INTERN(standard_output);
    INTERN(debug_entry); INTERN(debug_exit); INTERN(debug_error_entry);
    INTERN(amp_optional); INTERN(amp_rest); INTERN(amp_aux);
    mark_static((VALUE *)&throw_value);
    ADD_SUBR(subr_eval);
    ADD_SUBR(subr_funcall);
    ADD_SUBR(subr_progn);
    ADD_SUBR(subr_break);
    ADD_SUBR_INT(subr_step);
    ADD_SUBR(subr_macroexpand);
    ADD_SUBR(subr_signal);
    ADD_SUBR(subr_condition_case);
    ADD_SUBR(subr_backtrace);
    ADD_SUBR(subr_max_lisp_depth);

    /* Stuff for error-handling */
    INTERN(error_message);
    INTERN(error); ERROR(error);
    INTERN(invalid_function); ERROR(invalid_function);
    INTERN(void_function); ERROR(void_function);
    INTERN(void_value); ERROR(void_value);
    INTERN(bad_arg); ERROR(bad_arg);
    INTERN(invalid_read_syntax); ERROR(invalid_read_syntax);
    INTERN(end_of_stream); ERROR(end_of_stream);
    INTERN(invalid_lambda_list); ERROR(invalid_lambda_list);
    INTERN(missing_arg); ERROR(missing_arg);
    INTERN(invalid_macro); ERROR(invalid_macro);
    INTERN(invalid_autoload); ERROR(invalid_autoload);
    INTERN(no_catcher); ERROR(no_catcher);
    INTERN(buffer_read_only); ERROR(buffer_read_only);
    INTERN(bad_event_desc); ERROR(bad_event_desc);
    INTERN(file_error); ERROR(file_error);
    INTERN(invalid_stream); ERROR(invalid_stream);
    INTERN(setting_constant); ERROR(setting_constant);
    INTERN(process_error); ERROR(process_error);
    INTERN(invalid_area); ERROR(invalid_area);
#ifdef MINSTACK
    INTERN(stack_error); ERROR(stack_error);
#endif
    INTERN(no_memory); ERROR(no_memory);
    INTERN(user_interrupt); ERROR(user_interrupt);
    INTERN(arith_error); ERROR(arith_error);
    INTERN(window_error); ERROR(window_error);
    INTERN(invalid_pos); ERROR(invalid_pos);
    INTERN(term_interrupt);

    INTERN(debug_on_error); DOC(debug_on_error);
    VSYM(sym_debug_on_error)->value = sym_nil;
    INTERN(debug_macros); DOC(debug_macros);
    VSYM(sym_debug_macros)->value = sym_nil;

    int_cell = cmd_cons(sym_user_interrupt, sym_nil);
    mark_static(&int_cell);
    term_cell = cmd_cons(sym_term_interrupt, sym_nil);
    mark_static(&term_cell);

    INTERN(print_escape); 
    INTERN(print_length);
    INTERN(print_level);
    DOC(print_escape);
    DOC(print_length);
    DOC(print_level);
    VSYM(sym_print_escape)->value = sym_nil;
    VSYM(sym_print_length)->value = sym_nil;
    VSYM(sym_print_level)->value = sym_nil;
    INTERN(newlines);
}
