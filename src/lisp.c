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
#include <ctype.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

DEFSYM(debug_entry, "debug-entry");
DEFSYM(debug_exit, "debug-exit");
DEFSYM(debug_error_entry, "debug-error-entry");

DEFSYM(quote, "quote");
DEFSYM(backquote, "backquote");
DEFSYM(backquote_unquote, "backquote-unquote");
DEFSYM(backquote_splice, "backquote-splice");
DEFSYM(lambda, "lambda");
DEFSYM(macro, "macro");
DEFSYM(autoload, "autoload");
DEFSYM(function, "function");

DEFSYM(standard_input, "standard-input");
DEFSYM(standard_output, "standard-output");

DEFSYM(amp_optional, "&optional");
DEFSYM(amp_rest, "&rest");
DEFSYM(amp_aux, "&aux");

/* When a `throw' happens a function stuffs a cons-cell in here with,
   (TAG . repv).
   An error is the above with TAG=Qerror and repv a list of relevant
   data. */
volatile repv rep_throw_value;

/* This cons cell is used for interrupts. We don't know if it's safe to
   call Fcons() (maybe in gc?) so this is always valid.  */
repv rep_int_cell, rep_term_cell;

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
DEFSYM(file_error, "file-error");
DEFSTRING(err_file_error, "File error");
DEFSYM(invalid_stream, "invalid-stream");
DEFSTRING(err_invalid_stream, "Invalid stream");
DEFSYM(setting_constant, "setting-constant");
DEFSTRING(err_setting_constant, "Attempt to set value of constant");
DEFSYM(process_error, "process-error");
DEFSTRING(err_process_error, "Process error");
DEFSYM(no_memory, "no-memory");
DEFSTRING(err_no_memory, "No free memory");
DEFSYM(user_interrupt, "user-interrupt");
DEFSTRING(err_user_interrupt, "User interrupt!");
DEFSYM(arith_error, "arith-error");
DEFSTRING(err_arith_error, "Arithmetic error");
DEFSYM(term_interrupt, "term-interrupt");

#ifdef MINSTACK
DEFSYM(stack_error, "stack-error");
DEFSTRING(err_stack_error, "Out of stack space");
#endif

DEFSYM(debug_on_error, "debug-on-error");
DEFSYM(debug_macros, "debug-macros"); /*
::doc:Vdebug-on-error::
When an error is signalled this variable controls whether or not to enter the
Lisp debugger immediately. If the variable's value is t or a list of symbols
- one of which is the signalled error symbol - the debugger is entered.
See `signal'.
::end::
::doc:Vdebug-macros::
When nil, the debugger isn't entered while expanding macro definitions.
::end:: */

DEFSYM(print_escape, "print-escape");
DEFSYM(print_length, "print-length");
DEFSYM(print_level, "print-level");
DEFSYM(newlines, "newlines"); /*
::doc:Vprint-escape::
Defines which control characters `print' should quote. Acceptable values
are:
	nil		Only escape double-quote and backslash
	newlines	Escape double-quote, backslash, newline,
			 TAB, and formfeed.
	t		Escape all control codes (characters with a
			 value less than 32), and all characters with
			 a value greater than 126.
::end::
::doc:Vprint-length::
The maximum number of list elements to print before abbreviating.
::end::
::doc:Vprint-level::
The number of list levels to descend when printing before abbreviating.
::end:: */

/* When rep_TRUE Feval() calls the "debug-entry" function */
rep_bool rep_single_step_flag;

struct rep_Call *rep_call_stack;

static int lisp_depth, max_lisp_depth = 500;

/* Used to avoid costly interrupt checking too often */
int rep_test_int_counter = 0;

/* Limit before calling test_int_fun() */
int rep_test_int_period = 1000;

/* Function to test asynchronously for interrupts. If it detects an
   interrupt, it should set `rep_throw_value' to `rep_int_cell' */
static void default_test_int (void) { }
void (*rep_test_int_fun)(void) = default_test_int;


/* Reading */

/* All of the read-related functions are now stream based. This will
   probably add some (much?) overhead but I think it's worth it?

   The `c' variable which keeps coming up is the lookahead character,
   since each read*() routine normally has to look at the next character
   to see if it's what it wants. If not, this char is given to someone
   else... */

DEFSTRING(nodot, "Nothing to dot second element of cons-cell to");

static repv
read_list(repv strm, register int *c_p)
{
    repv result = Qnil;
    repv last = rep_NULL;
    rep_GC_root gc_result;
    rep_PUSHGC(gc_result, result);
    *c_p = rep_stream_getc(strm);
    while(result != rep_NULL)
    {
	switch(*c_p)
	{
	case EOF:
	    result = Fsignal(Qend_of_stream, rep_LIST_1(strm));
	    break;

	case ' ':
	case '\t':
	case '\n':
	case '\f':
	    *c_p = rep_stream_getc(strm);
	    continue;

	case ';':
	    {
		register int c;
		while((c = rep_stream_getc(strm)) != EOF && c != '\n' && c != '\f')
		    ;
		*c_p = rep_stream_getc(strm);
		continue;
	    }

	case '.':
	    *c_p = rep_stream_getc(strm);
	    if(last)
	    {
		if(!(rep_CDR(last) = rep_readl(strm, c_p)))
		    result = rep_NULL;
	    }
	    else
		result = Fsignal(Qinvalid_read_syntax,
				    rep_LIST_1(rep_VAL(&nodot)));
	    break;

	case ')':
	case ']':
	    *c_p = rep_stream_getc(strm);
	    goto end;
	    
	default:
	    {
		register repv this = Fcons(Qnil, Qnil);
		if(last)
		    rep_CDR(last) = this;
		else
		    result = this;
		if(!(rep_CAR(this) = rep_readl(strm, c_p)))
		    result = rep_NULL;
		last = this;
	    }
	}
    }
end:
    rep_POPGC;
    return result;
}

DEFSTRING(buf_overflow, "Internal buffer overflow");
DEFSTRING(int_overflow, "Integer limit exceeded");

/* Could be a symbol or a number */
static repv
read_symbol(repv strm, int *c_p)
{
#define SYM_BUF_LEN 240
    static repv buffer = rep_NULL;

    repv result;
    u_char *buf;
    int c = *c_p;
    int i = 0;

    /* For parsing numbers, while radix != zero, it might still be
       an integer that we're reading. */
    int radix = -1, sign = 1;
    long value = 0;

    if(buffer == rep_NULL)
    {
	buffer = rep_make_string(SYM_BUF_LEN + 1);
	rep_mark_static(&buffer);
    }

    buf = rep_STR(buffer);

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
	    c = rep_stream_getc(strm);
	    if(c == EOF)
		return Fsignal(Qend_of_stream, rep_LIST_1(strm));
	    buf[i++] = c;
	    break;

	case '|':
	    radix = 0;
	    c = rep_stream_getc(strm);
	    while((c != EOF) && (c != '|') && (i < SYM_BUF_LEN))
	    {
		buf[i++] = c;
		c = rep_stream_getc(strm);
	    }
	    if(c == EOF)
		return Fsignal(Qend_of_stream, rep_LIST_1(strm));
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
	c = rep_stream_getc(strm);
    }
    if(i >= SYM_BUF_LEN)
    {
	/* Guess I'd better fix this! */
	return Fsignal(Qerror, rep_LIST_1(rep_VAL(&buf_overflow)));
    }
done:
    if(radix > 0
       /* Ensure that we don't accept `0x' as hex zero */
       && ((radix != 16) || (i > 2)))
    {
	/* It was a number */
	value *= sign;
	if(value < rep_LISP_MIN_INT || value > rep_LISP_MAX_INT)
	    return Fsignal(Qarith_error, rep_LIST_1(rep_VAL(&int_overflow)));
	result = rep_MAKE_INT(value);
    }
    else
    {
	buf[i] = 0;
	rep_set_string_len(buffer, i);
	if(!(result = Ffind_symbol(rep_VAL(buffer), Qnil))
	   || (rep_NILP(result) && strcmp(buf, "nil")))
	{
	    repv name;
	    if((name = rep_string_dup(buf)) && (result = Fmake_symbol(name)))
		result = Fintern_symbol(result, Qnil);
	    else
		result = rep_NULL;
	}
    }
    *c_p = c;
    return result;
}

static repv
read_vector(repv strm, int *c_p)
{
    repv result;
    repv list = read_list(strm, c_p);
    if(list)
    {
	repv cur = list;
	int len;
	for(len = 0; rep_CONSP(cur); len++)
	    cur = rep_CDR(cur);
	result = rep_make_vector(len);
	if(result)
	{
	    int i;
	    cur = list;
	    for(i = 0; i < len; i++)
	    {
		repv nxt = rep_CDR(cur);
		rep_VECT(result)->array[i] =  rep_CAR(cur);
#if 1
		/* I think it's okay to put the cons cells back onto their
		   freelist. There can't be any references to them??  */
		rep_cons_free(cur);
#endif
		cur = nxt;
	    }
	}
	else
	    result = rep_NULL;
    }
    else
	result = rep_NULL;
    return result;
}

static repv
read_str(repv strm, int *c_p)
{
    repv result;
    int buflen = 128;
    int c = rep_stream_getc(strm);
    u_char *buf = rep_alloc(buflen);
    register u_char *cur = buf;
    u_char *bufend = buf + buflen;
    if(buf)
    {
	while((c != EOF) && (c != '"'))
	{
	    if(cur == bufend)
	    {
		register int newbuflen = buflen * 2;
		register u_char *newbuf = rep_alloc(newbuflen);
		if(newbuf)
		{
		    memcpy(newbuf, buf, cur - buf);
		    rep_free(buf);
		    buf = newbuf;
		    cur = buf + buflen;
		    buflen = newbuflen;
		    bufend = buf + buflen;
		}
		else
		    return rep_mem_error();
	    }
	    if(c == '\\')
	    {
		c = rep_stream_getc(strm);
		if(c == '\n')
		    /* escaped newline is ignored */
 		    c = rep_stream_getc(strm);
		else
		    *cur++ = (u_char)rep_stream_read_esc(strm, &c);
	    }
	    else
	    {
		*cur++ = c;
		c = rep_stream_getc(strm);
	    }
	}
	if(c == EOF)
	    result = Fsignal(Qend_of_stream, rep_LIST_1(strm));
	else
	{
	    *c_p = rep_stream_getc(strm);
	    result = rep_string_dupn(buf, cur - buf);
	}
	rep_free(buf);
	return result;
    }
    return rep_mem_error();
}

/* Using the above readlisp*() functions this classifies each type
   of expression and translates it into a lisp object (repv).
   Returns NULL in case of error. */
repv
rep_readl(repv strm, register int *c_p)
{
#ifdef MINSTACK
    if(STK_SIZE <= MINSTACK)
    {
	STK_WARN("read");
	return Fsignal(Qstack_error, Qnil);
    }
#endif
    while(1)
    {
	switch(*c_p)
	{
	    repv form;
	    rep_GC_root gc_form;

	case EOF:
	    return Qnil;

	case ' ':
	case '\t':
	case '\n':
	case '\f':
	    *c_p = rep_stream_getc(strm);
	    continue;

	case ';':
	    {
		register int c;
		while((c = rep_stream_getc(strm)) != EOF && c != '\n' && c != '\f')
		    ;
		*c_p = rep_stream_getc(strm);
		continue;
	    }

	case '\(':
	    return read_list(strm, c_p);

	case '\'': case '`': 
	    /* 'X => (quote X)
	       `X => (backquote X) */
	    form = Fcons(*c_p == '\'' ? Qquote : Qbackquote,
			    Fcons(Qnil, Qnil));
	    rep_PUSHGC(gc_form, form);
	    if((*c_p = rep_stream_getc(strm)) == EOF)
	    {
		rep_POPGC;
		goto eof;
	    }
	    rep_CAR(rep_CDR(form)) = rep_readl(strm, c_p);
	    rep_POPGC;
	    if(rep_CAR(rep_CDR(form)) != rep_NULL)
		return form;
	    else
		return rep_NULL;

	case ',':
	    /* ,@X => (backquote-splice X)
	       ,X  => (backquote-unquote X) */
	    form = Fcons(Qbackquote_unquote, Fcons(Qnil, Qnil));
	    rep_PUSHGC(gc_form, form);
	    switch((*c_p = rep_stream_getc(strm)))
	    {
	    case EOF:
		rep_POPGC;
		goto eof;

	    case '@':
		rep_CAR(form) = Qbackquote_splice;
		if((*c_p = rep_stream_getc(strm)) == EOF)
		{
		    rep_POPGC;
		    goto eof;
		}
	    }
	    rep_CAR(rep_CDR(form)) = rep_readl(strm, c_p);
	    rep_POPGC;
	    if(rep_CAR(rep_CDR(form)) != rep_NULL)
		return form;
	    else
		return rep_NULL;

	case '[':
	    return read_vector(strm, c_p);

	case '"':
	    return read_str(strm, c_p);

	case '?':
	    {
		register int c;
		switch(c = rep_stream_getc(strm))
		{
		case EOF:
		    goto eof;
		case '\\':
		    if((*c_p = rep_stream_getc(strm)) == EOF)
			goto eof;
		    else
			return rep_MAKE_INT(rep_stream_read_esc(strm, c_p));
		    break;
		default:
		    *c_p = rep_stream_getc(strm);
		    return rep_MAKE_INT(c);
		}
	    }

	case '#':
	    switch(*c_p = rep_stream_getc(strm))
	    {
	    case EOF:
		goto eof;
	    case '\'':
		form = Fcons(Qfunction, Fcons(Qnil, Qnil));
		rep_PUSHGC(gc_form, form);
		if((*c_p = rep_stream_getc(strm)) == EOF)
		{
		    rep_POPGC;
		    goto eof;
		}
		rep_CAR(rep_CDR(form)) = rep_readl(strm, c_p);
		rep_POPGC;
		if(rep_CAR(rep_CDR(form)) == rep_NULL)
		    return rep_NULL;
		else
		    return form;
	    case '[':
		{
		    repv vec = read_vector(strm, c_p);
		    if(vec != rep_NULL)
		    {
			if(rep_VECT_LEN(vec) >= rep_COMPILED_MIN_SLOTS)
			{
			    rep_COMPILED(vec)->car = (rep_COMPILED(vec)->car
						   & ~rep_CELL8_TYPE_MASK)
						  | rep_Compiled;
			    return vec;
			}
			goto error;
		    }
		    break;
		}
	    case '!':
		if (rep_FILEP(strm))
		{
		    repv pos = Fseek_file (strm, Qnil, Qnil);
		    if (pos && rep_INTP(pos) && rep_INT(pos) == 2)
		    {
			/* #! at the start of the file. Skip until !# */
			register int c;
			while ((c = rep_stream_getc (strm)) != EOF)
			{
			    if (c == '!')
			    {
				c = rep_stream_getc (strm);
				if (c == EOF || c == '#')
				    break;
			    }
			}
			if (c != EOF)
			    c = rep_stream_getc (strm);
			*c_p = c;
			continue;
		    }
		}
		/* FALL THROUGH */
	    default: error:
		return Fsignal(Qinvalid_read_syntax, rep_LIST_1(strm));
	    }

	default:
	    return read_symbol(strm, c_p);
	}
    }
    /* NOT REACHED */

eof:
    return Fsignal(Qend_of_stream, rep_LIST_1(strm));
}


/* Evaluating */

/* Evaluates each element of `list' and builds them into a new list. */
static repv
eval_list(repv list)
{
    repv result = Qnil;
    repv *last = &result;
    rep_GC_root gc_result, gc_list;
    rep_PUSHGC(gc_result, result);
    rep_PUSHGC(gc_list, list);
    while(rep_CONSP(list))
    {
	repv tmp;
	if(!(tmp = Feval(rep_CAR(list))))
	{
	    result = rep_NULL;
	    break;
	}
	if(!(*last = Fcons(tmp, Qnil)))
	{
	    result = rep_NULL;
	    break;
	}
	list = rep_CDR(list);
	last = &rep_CDR(*last);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	{
	    result = rep_NULL;
	    break;
	}
    }
    if(result && last && !rep_NILP(list))
	*last = Feval(list);
    rep_POPGC; rep_POPGC;
    return result;
}

/* format of lambda-lists is something like,

   [{required-symbols}] [&optional {optional-symbols}] [&rest symbol]
   [&aux {auxiliary-symbols}]

   NB: auxiliary symbols are set to nil. Also note that the lambdaList
   arg isn't protected from gc by this function; it's assumed that
   this is done by the caller. */
repv
rep_bind_lambda_list(repv lambdaList, repv argList, rep_bool eval_args)
{
#define STATE_REQUIRED 1
#define STATE_OPTIONAL 2
#define STATE_REST     3
#define STATE_AUX      4

    repv *evalled_args;
    int evalled_nargs;
    repv boundlist = Qnil;

    rep_GC_root gc_arglist, gc_boundlist;
    rep_GC_n_roots gc_evalled_args;

    rep_PUSHGC(gc_arglist, argList);
    rep_PUSHGC(gc_boundlist, boundlist);

    /* Evaluate arguments, and stick them in the evalled_args array */
    if(eval_args)
    {
	int i;
	evalled_nargs = rep_list_length(argList);
	evalled_args = alloca(sizeof(repv) * evalled_nargs);
	rep_PUSHGCN(gc_evalled_args, evalled_args, 0);
	for(i = 0; i < evalled_nargs; i++)
	{
	    if((evalled_args[i] = Feval(rep_CAR(argList))) == rep_NULL)
		goto error;
	    argList = rep_CDR(argList);
	    gc_evalled_args.count++;
	}
    }

    if(rep_CONSP(lambdaList))
    {
	int state = STATE_REQUIRED;
	while(rep_CONSP(lambdaList) && rep_SYMBOLP(rep_CAR(lambdaList)))
	{
	    repv argobj;
	    repv argspec = rep_CAR(lambdaList);
	    if(rep_STR(rep_SYM(argspec)->name)[0] == '&')
	    {
		if(argspec == Qamp_optional)
		{
		    if(state > STATE_OPTIONAL)
		    {
			Fsignal(Qinvalid_lambda_list,
				   rep_LIST_1(lambdaList));
			goto error;
		    }
		    state = STATE_OPTIONAL;
		    lambdaList = rep_CDR(lambdaList);
		    continue;
		}
		else if(argspec == Qamp_rest)
		{
		    if(state > STATE_REST)
		    {
			Fsignal(Qinvalid_lambda_list,
				   rep_LIST_1(lambdaList));
			goto error;
		    }
		    state = STATE_REST;
		    lambdaList = rep_CDR(lambdaList);
		    continue;
		}
		else if(argspec == Qamp_aux)
		{
		    state = STATE_AUX;
		    lambdaList = rep_CDR(lambdaList);
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
		else if(!eval_args && rep_CONSP(argList))
		{
		    argobj = rep_CAR(argList);
		    argList = rep_CDR(argList);
		}
		else if(state == STATE_OPTIONAL)
		    argobj = Qnil;
		else
		{
		    Fsignal(Qmissing_arg, rep_LIST_1(argspec));
		    goto error;
		}
		boundlist = rep_bind_symbol(boundlist, argspec, argobj);
		break;

	    case STATE_REST:
		if(eval_args)
		{
		    repv list = Qnil;
		    repv *ptr = &list;
		    while(evalled_nargs > 0)
		    {
			*ptr = Fcons(*evalled_args++, Qnil);
			ptr = &(rep_CDR(*ptr));
			evalled_nargs--;
			gc_evalled_args.count--;
		    }
		    argobj = list;
		}
		else
		    argobj = argList;
		boundlist = rep_bind_symbol(boundlist, argspec, argobj);
		state = STATE_AUX;
		break;

	    case STATE_AUX:
		boundlist = rep_bind_symbol(boundlist, argspec, Qnil);
	    }

	    lambdaList = rep_CDR(lambdaList);

	    rep_TEST_INT;
	    if(rep_INTERRUPTP)
	    {
	    error:
		rep_unbind_symbols(boundlist);
		boundlist = rep_NULL;
		break;
	    }
	}
    }

    if(eval_args)
	rep_POPGCN;
    rep_POPGC; rep_POPGC;

    return boundlist;
}

repv
rep_eval_lambda(repv lambdaExp, repv argList, rep_bool eval_args)
{
    repv result = rep_NULL;
    if(rep_CONSP(rep_CDR(lambdaExp)))
    {
	repv boundlist;
	rep_GC_root gc_lambdaExp, gc_argList;
	rep_PUSHGC(gc_lambdaExp, lambdaExp);
	rep_PUSHGC(gc_argList, argList);
	lambdaExp = rep_CDR(lambdaExp);
	boundlist = rep_bind_lambda_list(rep_CAR(lambdaExp), argList, eval_args);
	if(boundlist)
	{
	    rep_GC_root gc_boundlist;
	    rep_PUSHGC(gc_boundlist, boundlist);
	    result = Fprogn(rep_CDR(lambdaExp));
	    rep_POPGC;
	    rep_unbind_symbols(boundlist);
	}
	else
	    result = rep_NULL;
	rep_POPGC; rep_POPGC;
    }
    return result;
}

/* Autoloads a function, FUN is the symbol of the function, ALOAD-DEF is
   the `(autoload FILE-NAME ...)' object. This function may cause a gc.
   Returns the new function-value of FUN, or NULL for an error.
   If IS-VARIABLE is true, then return the value-slot of FUN, but do
   everything else the same. */
DEFSTRING(invl_autoload, "Can only autoload from symbols");
repv
rep_load_autoload(repv fun, repv aload_def, rep_bool is_variable)
{
    if(!rep_SYMBOLP(fun))
    {
	/* Unless the function we're calling is a symbol don't bother.
	   (Because it wouldn't be possible to find the new definition.)  */
	return Fsignal(Qinvalid_autoload,
			  rep_list_2(fun, rep_VAL(&invl_autoload)));
    }
    else
    {
	repv autoload = rep_CDR(aload_def);
	if(rep_CONSP(autoload) && rep_STRINGP(rep_CAR(autoload)))
	{
	    repv file = rep_CAR(autoload);
	    rep_GC_root gc_fun, gc_file;
	    repv tmp;

	    u_char *old_msg;
	    u_long old_msg_len;
	    if (rep_message_fun != 0)
	    {
		(*rep_message_fun)(rep_save_message, &old_msg, &old_msg_len);
		(*rep_message_fun)(rep_messagef,
				   "Loading %s...", rep_STR(file));
		(*rep_message_fun)(rep_redisplay_message);
	    }
	    rep_PUSHGC(gc_fun, fun); rep_PUSHGC(gc_file, file);
	    /* trash the autoload defn, so we don't keep trying to
	       autoload indefinitely.  */
	    rep_CAR(autoload) = Qnil;
	    tmp = Fload(file, Qt, Qnil, Qnil);
	    rep_POPGC; rep_POPGC;

	    if (rep_message_fun != 0)
	    {
		(*rep_message_fun)(rep_messagef,
				   "Loading %s...done.", rep_STR(file));
		(*rep_message_fun)(rep_redisplay_message);
		(*rep_message_fun)(rep_restore_message, old_msg, old_msg_len);
	    }
	    if(tmp && !rep_NILP(tmp))
	    {
		return (!is_variable
			? Fsymbol_function(fun, Qnil)
			: Fsymbol_value(fun, Qnil));
	    }
	}
	return Fsignal(Qinvalid_autoload, rep_LIST_1(fun));
    }
}

DEFSTRING(max_depth, "max-lisp-depth exceeded, possible infinite recursion?");

/* Applies ARGLIST to FUN. If EVAL-ARGS is true, all arguments will be
   evaluated first. Note that both FUN and ARGLIST are gc-protected
   for the duration of this function. */
repv
rep_funcall(repv fun, repv arglist, rep_bool eval_args)
{
    int type;
    repv result = rep_NULL;
    struct rep_Call lc;

    rep_TEST_INT;
    if(rep_INTERRUPTP)
	return rep_NULL;

#ifdef MINSTACK
    if(STK_SIZE <= MINSTACK)
    {
	STK_WARN("rep_funcall");
	return Fsignal(Qstack_error, Qnil);
    }
#endif

    if(++lisp_depth > max_lisp_depth)
    {
	lisp_depth--;
	return Fsignal(Qerror, rep_LIST_1(rep_VAL(&max_depth)));
    }

    /* Note that by putting FUN and ARGLIST in the backtrace,
       they're automagically protected from gc. */
    lc.fun = fun;
    lc.args = arglist;
    lc.args_evalled_p = eval_args ? Qnil : Qt;
    lc.next = rep_call_stack;
    rep_call_stack = &lc;

    if(rep_data_after_gc >= rep_gc_threshold)
	Fgarbage_collect(Qt);

again:
    if(rep_SYMBOLP(fun))
    {
	if(rep_SYM(fun)->car & rep_SF_DEBUG)
	    rep_single_step_flag = rep_TRUE;
	fun = Fsymbol_function(fun, Qnil);
	if(!fun)
	    goto end;
    }
    switch(type = rep_TYPE(fun))
    {
	int i, nargs;
	repv car, argv[5];
	rep_GC_n_roots gc_argv;

    case rep_SubrN:
	if(eval_args)
	{
	    arglist = eval_list(arglist);
	    if(arglist == rep_NULL)
		goto end;
	}
	result = rep_SUBRNFUN(fun)(arglist);
	break;

    case rep_Subr0:
	result = rep_SUBR0FUN(fun)();
	break;

    case rep_Subr1:
	nargs = 1;
	argv[0] = Qnil;
	goto do_subr;

    case rep_Subr2:
	nargs = 2;
	argv[0] = argv[1] = Qnil;
	goto do_subr;

    case rep_Subr3:
	nargs = 3;
	argv[0] = argv[1] = argv[2] = Qnil;
	goto do_subr;

    case rep_Subr4:
	nargs = 4;
	argv[0] = argv[1] = argv[2] = argv[3] = Qnil;
	goto do_subr;

    case rep_Subr5:
	nargs = 5;
	argv[0] = argv[1] = argv[2] = argv[3] = argv[4] = Qnil;
	/* FALL THROUGH */

    do_subr:
	if(eval_args)
	    rep_PUSHGCN(gc_argv, argv, nargs);
	for(i = 0; i < nargs; i++)
	{
	    if(rep_CONSP(arglist))
	    {
		if(!eval_args)
		    argv[i] = rep_CAR(arglist);
		else
		{
		    argv[i] = Feval(rep_CAR(arglist));
		    if(argv[i] == rep_NULL)
		    {
			rep_POPGCN;
			goto end;
		    }
		}
		arglist = rep_CDR(arglist);
	    }
	    else
		break;
	}
	if(eval_args)
	    rep_POPGCN;
	switch(type)
	{
	case rep_Subr1:
	    result = rep_SUBR1FUN(fun)(argv[0]);
	    break;
	case rep_Subr2:
	    result = rep_SUBR2FUN(fun)(argv[0], argv[1]);
	    break;
	case rep_Subr3:
	    result = rep_SUBR3FUN(fun)(argv[0], argv[1], argv[2]);
	    break;
	case rep_Subr4:
	    result = rep_SUBR4FUN(fun)(argv[0], argv[1], argv[2], argv[3]);
	    break;
	case rep_Subr5:
	    result = rep_SUBR5FUN(fun)(argv[0], argv[1], argv[2],
				    argv[3], argv[4]);
	    break;
	}
	break;

    case rep_Cons:
	car = rep_CAR(fun);
	if(car == Qlambda)
	{
	    /* rep_eval_lambda() expanded inline. */
	    if(rep_CONSP(rep_CDR(fun)))
	    {
		repv boundlist;
		fun = rep_CDR(fun);
		boundlist = rep_bind_lambda_list(rep_CAR(fun), arglist, eval_args);
		if(boundlist != rep_NULL)
		{
		    rep_GC_root gc_boundlist;
		    rep_PUSHGC(gc_boundlist, boundlist);
		    result = Fprogn(rep_CDR(fun));
		    rep_POPGC;
		    rep_unbind_symbols(boundlist);
		}
		else
		    result = rep_NULL;
	    }
	}
	else if(car == Qmacro)
	{
	macro_hack:
	    /* A macro. This could occur if autoloading from
	       a macro definition. Try to accommodate.. */
	    if(!eval_args)
		/* Args already evaluated. Can't expand the macro */
		goto invalid;
	    result = Fmacroexpand(Fcons(lc.fun, arglist), Qnil);
	    if(result != rep_NULL)
		result = Feval(result);
	}
	else if(car == Qautoload)
	{
	    /* lc.fun contains the original function description,
	       i.e. usually a symbol */
	    car = rep_load_autoload(lc.fun, fun, rep_FALSE);
	    if(car)
	    {
		fun = lc.fun;
		goto again;
	    }
	}
	else
	    Fsignal(Qinvalid_function, rep_LIST_1(fun));
	break;

    case rep_Compiled:
	{
	    repv boundlist;

	    if(rep_COMPILED_MACRO_P(fun))
		goto macro_hack;

	    boundlist = rep_bind_lambda_list(rep_COMPILED_LAMBDA(fun),
				       arglist, eval_args);
	    if(boundlist != rep_NULL)
	    {
		rep_GC_root gc_boundlist;
		rep_PUSHGC(gc_boundlist, boundlist);
		result = Fjade_byte_code(rep_COMPILED_CODE(fun),
					    rep_COMPILED_CONSTANTS(fun),
					    rep_MAKE_INT(rep_COMPILED_STACK(fun)));
		rep_POPGC;
		rep_unbind_symbols(boundlist);
	    }
	    else
		result = rep_NULL;
	    break;
	}

    default: invalid:
	Fsignal(Qinvalid_function, rep_LIST_1(fun));
    }

    /* In case I missed a non-local exit somewhere.  */
    if(rep_throw_value != rep_NULL)
	result = rep_NULL;

end:
    rep_call_stack = lc.next;
    lisp_depth--;
    return result;
}

DEFUN("funcall", Ffuncall, Sfuncall, (repv args), rep_SubrN) /*
::doc:Sfuncall::
funcall FUNCTION ARGS...

Calls FUNCTION with arguments ARGS... and returns the result.
::end:: */
{
    if(!rep_CONSP(args))
	return rep_signal_missing_arg(1);
    else
	return rep_funcall(rep_CAR(args), rep_CDR(args), rep_FALSE);
}

DEFSTRING(void_obj, "Void object to `eval'");

static repv
eval(repv obj)
{
    repv result = rep_NULL;

#ifdef MINSTACK
    if(STK_SIZE <= MINSTACK)
    {
	STK_WARN("eval");
	return Fsignal(Qstack_error, Qnil);
    }
#endif

    switch(rep_TYPE(obj))
    {
	repv funcobj;

    case rep_Symbol:
	result = Fsymbol_value(obj, Qnil);
	break;

    case rep_Cons:
	funcobj = rep_CAR(obj);
	if(rep_SYMBOLP(funcobj))
	{
	    if(rep_SYM(funcobj)->car & rep_SF_DEBUG)
		rep_single_step_flag = rep_TRUE;
	    funcobj = Fsymbol_function(funcobj, Qnil);
	    if(funcobj == rep_NULL)
		goto end;
	}
	if(rep_CELL8_TYPEP(funcobj, rep_SF))
	{
	    result = rep_SFFUN(funcobj)(rep_CDR(obj));
	}
	else if((rep_CONSP(funcobj) && rep_CAR(funcobj) == Qmacro)
		|| (rep_COMPILEDP(funcobj) && rep_COMPILED_MACRO_P(funcobj)))
	{
	    /* A macro */
	    repv form;
	    if(rep_single_step_flag
	       && (form = Fsymbol_value(Qdebug_macros, Qt))
	       && rep_NILP(form))
	    {
		/* Debugging macros gets tedious; don't
		   bother when debug-macros is nil. */
		rep_single_step_flag = rep_FALSE;
		form = Fmacroexpand(obj, Qnil);
		rep_single_step_flag = rep_TRUE;
	    }
	    else
		form = Fmacroexpand(obj, Qnil);
	    if(form != rep_NULL)
		result = Feval(form);
	}
	else
	    result = rep_funcall(rep_CAR(obj), rep_CDR(obj), rep_TRUE);
	break;

    case rep_Var:
	if(!(result = rep_VARFUN(obj)(rep_NULL)))
	    Fsignal(Qvoid_value, rep_LIST_1(obj));
	break;

    default:
	result = obj;
	break;
    }

    /* In case I missed a non-local exit somewhere.  */
    if(rep_throw_value != rep_NULL)
	result = rep_NULL;

end:
    return result;
}

DEFSTRING(no_debug, "No debugger installed");

DEFUN("eval", Feval, Seval, (repv obj), rep_Subr1) /*
::doc:Seval::
eval FORM

Evaluates FORM and returns its value.
::end:: */
{
    static int DbDepth;
    rep_bool newssflag = rep_TRUE;
    repv result;

    rep_TEST_INT;
    if(rep_INTERRUPTP)
	return rep_NULL;

    if(rep_data_after_gc >= rep_gc_threshold)
    {
	rep_GC_root gc_obj;
	rep_PUSHGC(gc_obj, obj);
	Fgarbage_collect(Qt);
	rep_POPGC;
    }

    if(!rep_single_step_flag)
	return eval(obj);

    DbDepth++;
    result = rep_NULL;
    if(rep_SYM(Qdebug_entry)->function)
    {
	repv dbres;
	repv dbargs = Fcons(obj, Fcons(rep_MAKE_INT(DbDepth), Qnil));
	if(dbargs)
	{
	    rep_GC_root gc_dbargs;
	    struct rep_saved_regexp_data re_data;
	    rep_PUSHGC(gc_dbargs, dbargs);
	    rep_push_regexp_data(&re_data);
	    rep_single_step_flag = rep_FALSE;
	    dbres = rep_funcall(Qdebug_entry, dbargs, rep_FALSE);
	    rep_pop_regexp_data();
	    if (dbres != rep_NULL && rep_CONSP(dbres))
	    {
		switch(rep_INT(rep_CAR(dbres)))
		{
		case 1:
		    /* single step cdr and following stuff  */
		    rep_single_step_flag = rep_TRUE;
		    result = eval(rep_CDR(dbres));
		    rep_single_step_flag = rep_FALSE;
		    break;
		case 2:
		    /* run through cdr and step following  */
		    result = eval(rep_CDR(dbres));
		    break;
		case 3:
		    /* run cdr and following  */
		    result = eval(rep_CDR(dbres));
		    newssflag = rep_FALSE;
		    break;
		case 4:
		    /* result = cdr  */
		    rep_single_step_flag = rep_TRUE;
		    result = rep_CDR(dbres);
		    rep_single_step_flag = rep_FALSE;
		    break;
		}
		if(result)
		{
		    if(rep_SYM(Qdebug_exit)->function)
		    {
			rep_push_regexp_data(&re_data);
			rep_CAR(dbargs) = result;
			if(!(dbres = rep_funcall(Qdebug_exit, dbargs, rep_FALSE)))
			    result = rep_NULL;
			rep_pop_regexp_data();
		    }
		}
	    }
	    rep_POPGC;
	}
    }
    else
    {
	Fsignal(Qerror, rep_LIST_1(rep_VAL(&no_debug)));
	newssflag = rep_FALSE;
	result = rep_NULL;
    }
    DbDepth--;
    rep_single_step_flag = newssflag;
    return result;
}

DEFUN("progn", Fprogn, Sprogn, (repv args), rep_SF) /*
::doc:Sprogn::
progn FORMS...

Eval's each of the FORMS in order returning the value of the last
one.
::end:: */
{
    repv result = Qnil;
    rep_GC_root gc_args;
    rep_PUSHGC(gc_args, args);
    while(rep_CONSP(args))
    {
	result = Feval(rep_CAR(args));
	args = rep_CDR(args);
	rep_TEST_INT;
	if(!result || rep_INTERRUPTP)
	    break;
    }
    if(result && !rep_NILP(args))
	result = Feval(args);
    rep_POPGC;
    return result;
}

repv
rep_call_lisp0(repv function)
{
    return rep_funcall(function, Qnil, rep_FALSE);
}

repv
rep_call_lisp1(repv function, repv arg1)
{
    return rep_funcall(function, rep_LIST_1(arg1), rep_FALSE);
}

repv
rep_call_lisp2(repv function, repv arg1, repv arg2)
{
    return rep_funcall(function, rep_LIST_2(arg1, arg2), rep_FALSE);
}

repv
rep_call_lisp3(repv function, repv arg1, repv arg2, repv arg3)
{
    return rep_funcall(function, rep_LIST_3(arg1, arg2, arg3), rep_FALSE);
}

repv
rep_call_lisp4(repv function, repv arg1, repv arg2, repv arg3, repv arg4)
{
    return rep_funcall(function, rep_LIST_4(arg1, arg2, arg3, arg4), rep_FALSE);
}

void
rep_lisp_prin(repv strm, repv obj)
{
    static int print_level = 0;

    switch(rep_TYPE(obj))
    {
	u_char tbuf[40];
	int j;
	int print_length;
	repv tem;

    case rep_Int:
#ifdef HAVE_SNPRINTF
	snprintf(tbuf, sizeof(tbuf), "%ld", rep_INT(obj));
#else
	sprintf(tbuf, "%ld", rep_INT(obj));
#endif
	rep_stream_puts(strm, tbuf, -1, rep_FALSE);
	break;

    case rep_Cons:
	tem = Fsymbol_value(Qprint_level, Qt);
	if(tem && rep_INTP(tem) && print_level >= rep_INT(tem))
	{
	    rep_stream_puts(strm, "...", 3, rep_FALSE);
	    return;
	}
	print_level++;
	rep_stream_putc(strm, '(');
	tem = Fsymbol_value(Qprint_length, Qt);
	print_length = 0;
	while(rep_CONSP(rep_CDR(obj)))
	{
	    if(tem && rep_INTP(tem) && print_length >= rep_INT(tem))
	    {
		rep_stream_puts(strm, "...", 3, rep_FALSE);
		goto cons_out;
	    }
	    rep_print_val(strm, rep_CAR(obj));
	    obj = rep_CDR(obj);
	    rep_stream_putc(strm, ' ');
	    rep_TEST_INT;
	    if(rep_INTERRUPTP)
		goto cons_out;
	    print_length++;
	}
	if(tem && rep_INTP(tem) && print_length >= rep_INT(tem))
	    rep_stream_puts(strm, "...", 3, rep_FALSE);
	else
	{
	    rep_print_val(strm, rep_CAR(obj));
	    if(!rep_NILP(rep_CDR(obj)))
	    {
		rep_stream_puts(strm, " . ", -1, rep_FALSE);
		rep_print_val(strm, rep_CDR(obj));
	    }
	}
    cons_out:
	rep_stream_putc(strm, ')');
	print_level--;
	break;

    case rep_Compiled:
	rep_stream_putc(strm, '#');
	/* FALL THROUGH */
    case rep_Vector:
	{
	    int len = rep_VECT_LEN(obj);
	    rep_stream_putc(strm, '\[');
	    for(j = 0; j < len; j++)
	    {
		if(rep_VECT(obj)->array[j])
		    rep_print_val(strm, rep_VECT(obj)->array[j]);
		else
		    rep_stream_puts(strm, "#<void>", -1, rep_FALSE);
		if(j != (len - 1))
		    rep_stream_putc(strm, ' ');
	    }
	    rep_stream_putc(strm, ']');
	    break;
	}

    case rep_Subr0:
    case rep_Subr1:
    case rep_Subr2:
    case rep_Subr3:
    case rep_Subr4:
    case rep_Subr5:
    case rep_SubrN:
#ifdef HAVE_SNPRINTF
	snprintf(tbuf, sizeof(tbuf), "#<subr %s>", rep_STR(rep_XSUBR(obj)->name));
#else
	sprintf(tbuf, "#<subr %s>", rep_STR(rep_XSUBR(obj)->name));
#endif
	rep_stream_puts(strm, tbuf, -1, rep_FALSE);
	break;

    case rep_SF:
#ifdef HAVE_SNPRINTF
	snprintf(tbuf, sizeof(tbuf),
		 "#<special-form %s>", rep_STR(rep_XSUBR(obj)->name));
#else
	sprintf(tbuf, "#<special-form %s>", rep_STR(rep_XSUBR(obj)->name));
#endif
	rep_stream_puts(strm, tbuf, -1, rep_FALSE);
	break;

    case rep_Var:
#ifdef HAVE_SNPRINTF
	snprintf(tbuf, sizeof(tbuf), "#<var %s>", rep_STR(rep_XSUBR(obj)->name));
#else
	sprintf(tbuf, "#<var %s>", rep_STR(rep_XSUBR(obj)->name));
#endif
	rep_stream_puts(strm, tbuf, -1, rep_FALSE);
	break;

    case rep_Void:
	rep_stream_puts(strm, "#<void>", -1, rep_FALSE);
	break;

    default:
	rep_stream_puts(strm, "#<unknown object type>", -1, rep_FALSE);
    }
}

void
rep_string_princ(repv strm, repv obj)
{
    rep_stream_puts(strm, rep_PTR(obj), -1, rep_TRUE);
}

void
rep_string_print(repv strm, repv obj)
{
    int len = rep_STRING_LEN(obj);
    u_char *s = rep_STR(obj);
    u_char c;

    rep_bool escape_all, escape_newlines;
    repv tem = Fsymbol_value(Qprint_escape, Qt);
    if(tem == Qnewlines)
	escape_all = rep_FALSE, escape_newlines = rep_TRUE;
    else if(tem == Qt)
	escape_all = rep_TRUE, escape_newlines = rep_TRUE;
    else
	escape_all = rep_FALSE, escape_newlines = rep_FALSE;

    rep_stream_putc(strm, '\"');
    while(len-- > 0)
    {
	c = *s++;
	if(escape_all && (c < 32 || c > 126))
	{
	    rep_stream_putc(strm, '\\');
	    rep_stream_putc(strm, '0' + c / 64);
	    rep_stream_putc(strm, '0' + (c % 64) / 8);
	    rep_stream_putc(strm, '0' + c % 8);
	}
	else
	{
	    switch(c)
	    {
	    case '\t':
	    case '\n':
	    case '\f':
		if(!escape_newlines)
		    rep_stream_putc(strm, (int)c);
		else
		    rep_stream_puts(strm, (c == '\t' ? "\\t"
				       : ((c == '\n') ? "\\n" : "\\f")),
				2, rep_FALSE);
		break;

	    case '\\':
		rep_stream_puts(strm, "\\\\", 2, rep_FALSE);
		break;

	    case '"':
		rep_stream_puts(strm, "\\\"", 2, rep_FALSE);
		break;

	    default:
		rep_stream_putc(strm, (int)c);
	    }
	}
    }
    rep_stream_putc(strm, '\"');
}

int
rep_list_length(repv list)
{
    int i = 0;
    while(rep_CONSP(list))
    {
	i++;
	list = rep_CDR(list);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    return i;
    }
    return i;
}

repv
rep_copy_list(repv list)
{
    repv result;
    repv *last = &result;
    while(rep_CONSP(list))
    {
	if(!(*last = Fcons(rep_CAR(list), Qnil)))
	    return rep_NULL;
	list = rep_CDR(list);
	last = &rep_CDR(*last);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    return rep_NULL;
    }
    *last = list;
    return result;
}

/* Used for easy handling of `var' objects */
repv
rep_handle_var_int(repv val, int *data)
{
    if(val)
    {
	if(rep_INTP(val))
	    *data = rep_INT(val);
	return rep_NULL;
    }
    else
	return rep_MAKE_INT(*data);
}

/* Similar, but for variables containing greater than 24 bits of data,
   passed around as a cons cell containing two integers */
repv
rep_handle_var_long_int(repv val, long *data)
{
    if(val)
    {
	if(rep_LONG_INTP(val))
	    *data = rep_LONG_INT(val);
	return rep_NULL;
    }
    else
	return rep_MAKE_LONG_INT(*data);
}

DEFUN("break", Fbreak, Sbreak, (void), rep_Subr0) /*
::doc:Sbreak::
break

The next form to be evaluated will be done so through the Lisp debugger.
::end:: */
{
    rep_single_step_flag = rep_TRUE;
    return Qt;
}

DEFUN_INT("step", Fstep, Sstep, (repv form), rep_Subr1, "xForm to step through") /*
::doc:Sstep::
step FORM

Use the Lisp debugger to evaluate FORM.
::end:: */
{
    repv res;
    rep_bool oldssf = rep_single_step_flag;
    rep_single_step_flag = rep_TRUE;
    res = Feval(form);
    rep_single_step_flag = oldssf;
    return res;
}

DEFUN("macroexpand", Fmacroexpand, Smacroexpand, (repv form, repv env), rep_Subr2) /*
::doc:Smacroexpand::
macroexpand FORM [ENVIRONMENT]

If FORM is a macro call, expand it until it isn't. If ENVIRONMENT is
specified it is an alist of `(MACRO-NAME . DEFINITION)'.
::end:: */
{
    repv car;
    rep_GC_root gc_form, gc_env, gc_car;
    rep_PUSHGC(gc_form, form);
    rep_PUSHGC(gc_env, env);
    rep_PUSHGC(gc_car, car);
top:
    if(rep_CONSP(form))
    {
	car = rep_CAR(form);
	if(rep_SYMBOLP(car))
	{
	    repv tmp;
	    if(!rep_NILP(env) && (tmp = Fassq(car, env)) && rep_CONSP(tmp))
	    {
		car = rep_CDR(tmp);
	    }
	    else
	    {
		car = Fsymbol_function(car, Qt);
		if(rep_CONSP(car))
		{
		    if(rep_CAR(car) != Qmacro)
			goto end;
		    car = rep_CDR(car);
		}
	    }
	    if(rep_VOIDP(car) || rep_NILP(car))
		goto end;
	    if(rep_CONSP(car) && rep_CAR(car) == Qlambda)
	    {
		form = rep_eval_lambda(car, rep_CDR(form), rep_FALSE);
		if(form != rep_NULL)
		    goto top;
	    }
	    else if(rep_COMPILEDP(car) && rep_COMPILED_MACRO_P(car))
	    {
		repv boundlist;
		struct rep_Call lc;

		lc.next = rep_call_stack;
		lc.fun = rep_CAR(form);
		lc.args = rep_CDR(form);
		lc.args_evalled_p = rep_TRUE;
		rep_call_stack = &lc;
		boundlist = rep_bind_lambda_list(rep_COMPILED_LAMBDA(car),
					   rep_CDR(form), rep_FALSE);
		if(boundlist != rep_NULL)
		{
		    rep_GC_root gc_boundlist;
		    rep_PUSHGC(gc_boundlist, boundlist);
		    form = Fjade_byte_code(rep_COMPILED_CODE(car),
					      rep_COMPILED_CONSTANTS(car),
					      rep_MAKE_INT(rep_COMPILED_STACK(car)));
		    rep_POPGC;
		    rep_unbind_symbols(boundlist);
		}
		else
		    form = rep_NULL;
		rep_call_stack = lc.next;

		if(form != rep_NULL)
		    goto top;
	    }
	}
    }
end:
    rep_POPGC; rep_POPGC; rep_POPGC;
    return form;
}

DEFUN("signal", Fsignal, Ssignal, (repv error, repv data), rep_Subr2) /*
::doc:Ssignal::
signal rep_ERROR-SYMBOL DATA

Signal that an error has happened. rep_ERROR-SYMBOL is the name of a symbol
classifying the type of error, it should have a property `error-message'
(a string) with a short description of the error message.
DATA is a list of objects which are relevant to the error -- they will
be made available to any error-handler or printed by the default error
handler.
::end:: */
{
    repv tmp, errlist, on_error;
    /* Can only have one error at once.	 */
    if(rep_throw_value)
	return rep_NULL;
    rep_DECLARE1(error, rep_SYMBOLP);

    errlist = Fcons(error, data);
    on_error = Fsymbol_value(Qdebug_on_error, Qt);
    if(((on_error != rep_NULL && on_error == Qt)
	|| (rep_CONSP(on_error) && (tmp = Fmemq(error, on_error)) && !rep_NILP(tmp)))
       && rep_SYM(Qdebug_error_entry)->function)
    {
	/* Enter debugger. */
	rep_GC_root gc_on_error;
	rep_bool oldssflag = rep_single_step_flag;
	Fset(Qdebug_on_error, Qnil);
	rep_single_step_flag = rep_FALSE;
	rep_PUSHGC(gc_on_error, on_error);
	tmp = rep_funcall(Qdebug_error_entry,
		      Fcons(errlist, Qnil), rep_FALSE);
	rep_POPGC;
	Fset(Qdebug_on_error, on_error);
	if(tmp && (tmp == Qt))
	    rep_single_step_flag = rep_TRUE;
	else
	    rep_single_step_flag = oldssflag;
    }
    rep_throw_value = Fcons(Qerror, errlist);
    return rep_NULL;
}

/* For an error rep_ERROR (the cdr of rep_throw_value), if it matches the error
   handler HANDLER (the car of the handler list), return rep_TRUE. */
rep_bool
rep_compare_error(repv error, repv handler)
{
    if(rep_CONSP(error))
    {
	repv error_sym = rep_CAR(error);
	if(rep_SYMBOLP(handler) && (error_sym == handler || handler == Qerror))
	    return rep_TRUE;
	else if(rep_CONSP(handler))
	{
	    handler = Fmemq(error_sym, handler);
	    return handler != rep_NULL && !rep_NILP(handler);
	}
    }
    return rep_FALSE;
}

DEFUN("condition-case", Fcondition_case, Scondition_case, (repv args), rep_SF) /*
::doc:Scondition-case::
condition-case VAR FORM HANDLERS...

Evaluates FORM with error-handlers in place, if no errors occur return the
value returned by FORM, else the value of whichever handler's body was
evaluated.

Each HANDLER is a list of `(rep_ERROR BODY...)'. rep_ERROR defines which types of
errors the handler catches, either a symbol or a list of symbols. The
special symbol `error' matches all types of errors.

If VAR is non-nil it's a symbol whose values is bound to
`(rep_ERROR-SYMBOL . DATA)' while the handler is evaluated (these are the
arguments given to `signal' when the error was raised).
::end:: */
{
    repv var, res = rep_NULL;
    rep_GC_root gc_args;
    if(!rep_CONSP(args))
	return rep_signal_missing_arg(1);
    rep_PUSHGC(gc_args, args);
    var = rep_CAR(args);
    args = rep_CDR(args);
    if(!rep_CONSP(args))
	return Qnil;
    res = Feval(rep_CAR(args));
    args = rep_CDR(args);
    if(res == rep_NULL && rep_throw_value != rep_NULL
       && (rep_CAR(rep_throw_value) == Qerror) && rep_CONSP(rep_CDR(rep_throw_value)))
    {
	/* an error.  */
	repv error = rep_CDR(rep_throw_value);
	repv throw_val = rep_throw_value;
	rep_throw_value = rep_NULL;
	while(rep_CONSP(args) && rep_CONSP(rep_CAR(args)))
	{
	    repv handler = rep_CAR(args);
	    if(rep_compare_error(error, rep_CAR(handler)))
	    {
		repv bindlist = Qnil;
		rep_GC_root gc_bindlist;
		if(rep_SYMBOLP(var) && !rep_NILP(var))
		{
		    bindlist = rep_bind_symbol(Qnil, var, error);
		    rep_PUSHGC(gc_bindlist, bindlist);
		}
		res = Fprogn(rep_CDR(handler));
		if(rep_SYMBOLP(var) && !rep_NILP(var))
		{
		    rep_POPGC;
		    rep_unbind_symbols(bindlist);
		}
		break;
	    }
	    args = rep_CDR(args);
	    rep_TEST_INT;
	    if(rep_INTERRUPTP)
	    {
		res = rep_NULL;
		break;
	    }
	    if(!rep_CONSP(args))
		rep_throw_value = throw_val; /* reinstall the error */
	}
    }
    rep_POPGC;
    return res;
}

DEFSTRING(unknown_err, "Unknown error");
DEFSTRING(one_err_fmt, "%s");
DEFSTRING(two_err_fmt, "%s: %s");
DEFSTRING(three_err_fmt, "%s: %s, %s");
DEFSTRING(four_err_fmt, "%s: %s, %s, %s");
DEFSTRING(n_err_fmt, "%s: ...");

void
rep_handle_error(repv error, repv data)
{
    repv errstr;
    Fbeep();
    if(!(errstr = Fget(error, Qerror_message)) || !rep_STRINGP(errstr))
	errstr = rep_VAL(&unknown_err);
    switch(rep_list_length(data))
    {

    case 0:
	Fformat(rep_list_3(Qt, rep_VAL(&one_err_fmt), errstr));
	break;
    case 1:
	Fformat(rep_list_4(Qt, rep_VAL(&two_err_fmt), errstr, rep_CAR(data)));
	break;
    case 2:
	Fformat(rep_list_5(Qt, rep_VAL(&three_err_fmt), errstr,
			  rep_CAR(data), rep_CAR(rep_CDR(data))));
	break;
    case 3:
	Fformat(Fcons(Qt, rep_list_5(rep_VAL(&four_err_fmt), errstr,
					  rep_CAR(data), rep_CAR(rep_CDR(data)),
					  rep_CAR(rep_CDR(rep_CDR(data))))));
	break;
    default:
	Fformat(rep_list_3(Qt, rep_VAL(&n_err_fmt), errstr));
    }
}

repv
rep_signal_arg_error(repv obj, int argNum)
{
    return Fsignal(Qbad_arg, rep_list_2(obj, rep_MAKE_INT(argNum)));
}

repv
rep_signal_missing_arg(int argnum)
{
    return Fsignal(Qmissing_arg, rep_LIST_1(rep_MAKE_INT(argnum)));
}

repv
rep_mem_error(void)
{
    return Fsignal(Qno_memory, Qnil);
}

DEFUN("backtrace", Fbacktrace, Sbacktrace, (repv strm), rep_Subr1) /*
::doc:Sbacktrace::
backtrace [STREAM]

Prints a backtrace of the current Lisp call stack to STREAM (or to
`standard-output').
The format is something like:
  FUNCTION (ARGLIST) ARGS-EVALLED-P
where ARGS-EVALLED-P is either `t' or `nil', depending on whether or not
ARGLIST had been evaluated or not before being put into the stack.
::end:: */
{
    struct rep_Call *lc;
    if(rep_NILP(strm)
       && !(strm = Fsymbol_value(Qstandard_output, Qnil)))
    {
	return Fsignal(Qbad_arg, rep_list_2(strm, rep_MAKE_INT(1)));
    }
    lc = rep_call_stack;
    while(lc)
    {
	rep_stream_putc(strm, '\n');
	rep_print_val(strm, lc->fun);
	rep_stream_putc(strm, ' ');
	rep_print_val(strm, lc->args);
	rep_stream_putc(strm, ' ');
	rep_print_val(strm, lc->args_evalled_p);
	lc = lc->next;
    }
    return Qt;
}

DEFUN("max-lisp-depth", Vmax_lisp_depth, Smax_lisp_depth, (repv val), rep_Var) /*
::doc:Smax-lisp-depth::
The maximum number of times that rep_funcall can be called recursively.

This is intended to stop infinite recursion, if the default value of 250 is
too small (you get errors in normal use) set it to something larger.
::end:: */
{
    return rep_handle_var_int(val, &max_lisp_depth);
}

void
rep_lisp_init(void)
{
    rep_INTERN(quote); rep_INTERN(lambda); rep_INTERN(macro);
    rep_INTERN(backquote); rep_INTERN(backquote_unquote); rep_INTERN(backquote_splice);
    rep_INTERN(autoload); rep_INTERN(function);
    rep_INTERN(standard_input); rep_INTERN(standard_output);
    rep_INTERN(debug_entry); rep_INTERN(debug_exit); rep_INTERN(debug_error_entry);
    rep_INTERN(amp_optional); rep_INTERN(amp_rest); rep_INTERN(amp_aux);
    rep_mark_static((repv *)&rep_throw_value);
    rep_ADD_SUBR(Seval);
    rep_ADD_SUBR(Sfuncall);
    rep_ADD_SUBR(Sprogn);
    rep_ADD_SUBR(Sbreak);
    rep_ADD_SUBR_INT(Sstep);
    rep_ADD_SUBR(Smacroexpand);
    rep_ADD_SUBR(Ssignal);
    rep_ADD_SUBR(Scondition_case);
    rep_ADD_SUBR(Sbacktrace);
    rep_ADD_SUBR(Smax_lisp_depth);

    /* Stuff for error-handling */
    rep_INTERN(error_message);
    rep_INTERN(error); rep_ERROR(error);
    rep_INTERN(invalid_function); rep_ERROR(invalid_function);
    rep_INTERN(void_function); rep_ERROR(void_function);
    rep_INTERN(void_value); rep_ERROR(void_value);
    rep_INTERN(bad_arg); rep_ERROR(bad_arg);
    rep_INTERN(invalid_read_syntax); rep_ERROR(invalid_read_syntax);
    rep_INTERN(end_of_stream); rep_ERROR(end_of_stream);
    rep_INTERN(invalid_lambda_list); rep_ERROR(invalid_lambda_list);
    rep_INTERN(missing_arg); rep_ERROR(missing_arg);
    rep_INTERN(invalid_macro); rep_ERROR(invalid_macro);
    rep_INTERN(invalid_autoload); rep_ERROR(invalid_autoload);
    rep_INTERN(no_catcher); rep_ERROR(no_catcher);
    rep_INTERN(file_error); rep_ERROR(file_error);
    rep_INTERN(invalid_stream); rep_ERROR(invalid_stream);
    rep_INTERN(setting_constant); rep_ERROR(setting_constant);
    rep_INTERN(process_error); rep_ERROR(process_error);
#ifdef MINSTACK
    rep_INTERN(stack_error); rep_ERROR(stack_error);
#endif
    rep_INTERN(no_memory); rep_ERROR(no_memory);
    rep_INTERN(user_interrupt); rep_ERROR(user_interrupt);
    rep_INTERN(arith_error); rep_ERROR(arith_error);
    rep_INTERN(term_interrupt);

    rep_INTERN(debug_on_error);
    rep_SYM(Qdebug_on_error)->value = Qnil;
    rep_INTERN(debug_macros);
    rep_SYM(Qdebug_macros)->value = Qnil;

    rep_int_cell = Fcons(Quser_interrupt, Qnil);
    rep_mark_static(&rep_int_cell);
    rep_term_cell = Fcons(Qterm_interrupt, Qnil);
    rep_mark_static(&rep_term_cell);

    rep_INTERN(print_escape); 
    rep_INTERN(print_length);
    rep_INTERN(print_level);
    rep_SYM(Qprint_escape)->value = Qnil;
    rep_SYM(Qprint_length)->value = Qnil;
    rep_SYM(Qprint_level)->value = Qnil;
    rep_INTERN(newlines);
}
