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
#include <assert.h>

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
DEFSYM(structure_ref, "structure-ref");

DEFSYM(standard_input, "standard-input");
DEFSYM(standard_output, "standard-output");

DEFSYM(amp_optional, "&optional");
DEFSYM(amp_rest, "&rest");

/* When a `throw' happens a function stuffs a cons-cell in here with,
   (TAG . repv).
   An error is the above with TAG=Qerror and repv a list of relevant
   data. */
volatile repv rep_throw_value;

/* This cons cell is used for interrupts. We don't know if it's safe to
   call Fcons() (maybe in gc?) so this is always valid.  */
repv rep_int_cell, rep_term_cell;

/* Used to mark tail calling throws */
rep_ALIGN_CELL(static rep_cell tail_call_tag) = { rep_Void };
#define TAIL_CALL_TAG rep_VAL(&tail_call_tag)

DEFSYM(error, "error");
DEFSTRING(err_error, "Error");
DEFSYM(error_message, "error-message");
DEFSYM(invalid_function, "invalid-function");
DEFSTRING(err_invalid_function, "Invalid function");
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

DEFSYM(debug_on_error, "debug-on-error");
DEFSYM(backtrace_on_error, "backtrace-on-error");
DEFSYM(debug_macros, "debug-macros");
DEFSYM(error_handler_function, "error-handler-function"); /*
::doc:debug-on-error::
When an error is signalled this variable controls whether or not to
enter the Lisp debugger immediately. If the variable's value is t or a
list of symbols--one of which is the signalled error symbol--the
debugger is entered.
::end::
::doc:backtrace-on-error::
When an error is signalled this variable controls whether or not to
print a backtrace immediately. If the variable's value is t or a list
of symbols--one of which is the signalled error symbol--the debugger is
entered.
::end::
::doc:debug-macros::
When nil, the debugger isn't entered while expanding macro definitions.
::end::
::doc:error-handler-function::
When set to a function value, called with two arguments (error type
and data) when lisp errors occur.
::end:: */

DEFSYM(print_escape, "print-escape");
DEFSYM(print_length, "print-length");
DEFSYM(print_level, "print-level");
DEFSYM(newlines, "newlines"); /*
::doc:print-escape::
Defines which control characters `print' should quote. Acceptable values
are:
	nil		Only escape double-quote and backslash
	newlines	Escape double-quote, backslash, newline,
			 TAB, and formfeed.
	t		Escape all control codes (characters with a
			 value less than 32), and all characters with
			 a value greater than 126.
::end::
::doc:print-length::
The maximum number of list elements to print before abbreviating.
::end::
::doc:print-level::
The number of list levels to descend when printing before abbreviating.
::end:: */

DEFSYM(load, "load");
DEFSYM(require, "require");

/* When rep_TRUE Feval() calls the "debug-entry" function */
rep_bool rep_single_step_flag;

/* Lexical environment. A list of (SYMBOL . VALUE). Any unbound variables
   are dereferenced in the current structure (global namespace)  */
repv rep_env;

/* Active special bindings, a list of (SYMBOL . VALUE) */
repv rep_special_bindings;

/* The bytecode interpreter to use. A subr or a null pointer */
repv (*rep_bytecode_interpreter)(repv subr, int nargs, repv *args);

/* The lisp-call backtrace; also used for saving and restoring
   the current environment */
struct rep_Call *rep_call_stack;

/* Prevent infinite recursion */
int rep_lisp_depth, rep_max_lisp_depth = 1000;

/* Used to avoid costly interrupt checking too often */
int rep_test_int_counter = 0;

/* Limit before calling test_int_fun() */
int rep_test_int_period = 1000;

/* Function to test asynchronously for interrupts. If it detects an
   interrupt, it should set `rep_throw_value' to `rep_int_cell' */
static void default_test_int (void) { }
void (*rep_test_int_fun)(void) = default_test_int;


/* Reading */

/* The `c' variable which keeps coming up is the lookahead character,
   since each reader function normally has to look at the next character
   to see if it's what it wants. If not, the lookahead is given to
   someone else or unread before exiting... */

/* inline common case of reading from local files; this appears to
   decrease startup time by about 25% */
static inline int
fast_getc (repv stream)
{
    if (rep_FILEP (stream) && rep_LOCAL_FILE_P (stream))
	return getc (rep_FILE (stream)->file.fh);
    else
	return rep_stream_getc (stream);
}
 
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
	    *c_p = fast_getc(strm);
	    continue;

	case ';':
	    {
		register int c;
		while((c = fast_getc(strm)) != EOF && c != '\n' && c != '\f')
		    ;
		*c_p = fast_getc(strm);
		continue;
	    }

	case ')':
	case ']':
	    *c_p = rep_stream_getc(strm);
	    goto end;
	    
	case '.':
	    *c_p = rep_stream_getc(strm);
	    switch (*c_p)
	    {
	    case EOF:
		result = Fsignal(Qend_of_stream, rep_LIST_1(strm));
		goto end;

	    case ' ': case '\t': case '\n': case '\f':
		if(last)
		{
		    repv this = rep_readl(strm, c_p);
		    if (this != rep_NULL)
			rep_CDR (last) = this;
		    else
		    {
			result = rep_NULL;
			goto end;
		    }
		}
		else
		{
		    result = Fsignal(Qinvalid_read_syntax,
				     rep_LIST_1(rep_VAL(&nodot)));
		    goto end;
		}
		continue;

	    default:
		rep_stream_ungetc (strm, *c_p);
		*c_p = '.';
	    }
	    /* fall through */

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

/* Could be a symbol or a number */
static repv
read_symbol(repv strm, int *c_p)
{
    static repv buffer = rep_NULL;
    static size_t buflen = 240;

    repv result;
    u_char *buf;
    int c = *c_p;
    int i = 0;

    /* For parsing numbers, while radix != zero, it might still be
       an integer that we're reading. */
    int radix = -1, sign = 1, nfirst = 0;
    rep_bool exact = rep_TRUE, rational = rep_FALSE;
    rep_bool exponent = rep_FALSE, had_sign = rep_FALSE;
    rep_bool expecting_prefix = rep_FALSE;
    int force_exactness = 0;

    if (buffer == rep_NULL)
    {
	buffer = rep_make_string (buflen + 2);
	rep_mark_static (&buffer);
    }

    buf = rep_STR(buffer);

    while (c != EOF)
    {
	if (i == buflen)
	{
	    repv new;
	    buflen = buflen * 2;
	    new = rep_make_string (buflen + 2);
	    memcpy (rep_STR (new), buf, buflen / 2);
	    buf = rep_STR (new);
	}
	switch(c)
	{
	case ' ':  case '\t': case '\n': case '\f':
	case '(':  case ')':  case '[':  case ']':
	case '\'': case '"':  case ';':  case ',':
	case '`':
	    goto done;

	case '#':
	    if (radix == 0)
		goto done;
	    else
		goto number;

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
	    while((c != EOF) && (c != '|') && (i < buflen))	/* XXX */
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
	    number:
		if (expecting_prefix)
		{
		    switch (c)
		    {
		    case 'b': case 'B':
			radix = 2;
			break;

		    case 'o': case 'O':
			radix = 8;
			break;

		    case 'd': case 'D':
			radix = 10;
			break;

		    case 'x': case 'X':
			radix = 16;
			break;

		    case 'e': case 'E':
			force_exactness = +1;
			break;

		    case 'i': case 'I':
			force_exactness = -1;
			break;

		    default:
			radix = 0;
		    }
		    expecting_prefix = rep_FALSE;
		    nfirst = i + 1;
		}
		/* It still may be a number that we're parsing */
		else if (i == nfirst && (c == '-' || c == '+' || c == '#'))
		{
		    if (c == '#')
		    {
			if (had_sign)
			    radix = 0;	/* not a number? */
			else
			    expecting_prefix = rep_TRUE;
		    }
		    else
		    {
			/* A leading sign */
			sign = (c == '-') ? -1 : 1;
			had_sign = rep_TRUE;
		    }
		    nfirst = i + 1;
		}
		else
		{
		    switch (radix)
		    {
		    case -1:
			/* Deduce the base next (or that we're not
			   looking at a number) */
			if (c == '.')
			{
			    radix = 10;
			    exact = rep_FALSE;
			}
			else if(!(c >= '0' && c <= '9'))
			    radix = 0;
			else if(c == '0')
			    radix = 1;	/* octal or hex */
			else
			    radix = 10;
			break;

		    case 1:
			/* We had a leading zero last character. If
			   this char's an 'x' it's hexadecimal. */
			switch (c)
			{
			case 'x': case 'X':
			    radix = 16;
			    nfirst = i + 1;
			    break;

			case '0': case '1': case '2': case '3':
			case '4': case '5': case '6': case '7':
			    radix = 8;
			    nfirst = i;
			    break;

			case '.': case 'e': case 'E':
			    radix = 10;
			    exact = rep_FALSE;
			    break;

			case '/':
			    radix = 10;
			    rational = rep_TRUE;
			    break;

			default:
			    radix = 0;
			}
			break;

		    default:
			/* Now we're speculatively reading a number
			   of base radix. */
			switch (c)
			{
			case '.':
			    if (exact && radix == 10 && !rational)
				exact = rep_FALSE;
			    else
				radix = 0;
			    break;

			case '/':
			    if (exact && !rational)
				rational = rep_TRUE;
			    else
				radix = 0;
			    break;

			case '-': case '+':
			    if (!exponent)
				goto do_default;
			    break;

			case 'e': case 'E':	/* XXX all scheme exp chars */
			    if (radix == 10)
			    {
				if (!rational && !exponent)
				{
				    exponent = rep_TRUE;
				    exact = rep_FALSE;
				}
				else
				    radix = 0;
				break;
			    }
			    /* fall through */

			default: do_default:
			    if(radix <= 10
			       && !(c >= '0' && c <= ('0' + radix - 1)))
			    {
				radix = 0;
			    }
			    else if(radix == 16 && !isxdigit(c))
				radix = 0;
			}
		    }
		}
	    }
	    buf[i++] = c;
	}
	c = fast_getc(strm);
    }
done:
    buf[i] = 0;
    if (i == 0)
	result = Fsignal (Qinvalid_read_syntax, rep_LIST_1 (strm));
    else if (radix > 0 && nfirst < i)
    {
	/* It was a number of some sort */
	if (radix == 1)
	    result = rep_MAKE_INT (0);
	else
	    result = rep_parse_number (buf + nfirst, i - nfirst, radix, sign,
				       !exact ? rep_NUMBER_FLOAT
				       : rational ? rep_NUMBER_RATIONAL : 0);
	if (result == rep_NULL)
	    goto intern;
	if (force_exactness > 0)
	    result = Finexact_to_exact (result);
	else if (force_exactness < 0)
	    result = Fexact_to_inexact (result);
    }
    else
    {
intern:	rep_set_string_len(buffer, i);
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
		c = fast_getc(strm);
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
    while(1)
    {
	switch(*c_p)
	{
	    repv form;
	    rep_GC_root gc_form;

	case EOF:
	    goto eof;

	case ' ':
	case '\t':
	case '\n':
	case '\f':
	    *c_p = fast_getc(strm);
	    continue;

	case ';':
	    {
		register int c;
		while((c = fast_getc(strm)) != EOF && c != '\n' && c != '\f')
		    ;
		*c_p = rep_stream_getc(strm);
		continue;
	    }

	case '(':
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
		int comment_terminator;

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

	    case '(':
		return read_vector (strm, c_p);

	    case '|':
		/* comment delimited by `#| ... |#' */
		comment_terminator = '|';
		{
		    register int c;
		read_comment:
		    while ((c = fast_getc (strm)) != EOF)
		    {
		    comment_again:
			if (c == comment_terminator)
			{
			    c = rep_stream_getc (strm);
			    if (c == EOF || c == '#')
				break;
			    else
				goto comment_again;
			}
		    }
		    if (c != EOF)
			c = rep_stream_getc (strm);
		    *c_p = c;
		    continue;
		}

	    case '\\':
		{
		    static const struct {
			char *name;
			int value;
		    } char_names[] = {
		      { "space", ' ' },
		      { "newline", '\n' },
		      { "backspace", '\010' },
		      { "tab", '\t' },
		      { "linefeed", '\n' },
		      { "return", '\r' },
		      { "page", '\f' },
		      { "rubout", '\177' },
		      { 0, 0 }
		    };

		    int c = rep_stream_getc (strm), c2, i;

		    if (c == EOF)
			goto eof;
		    if (!isalpha (c))
		    {
			*c_p = rep_stream_getc (strm);
			return rep_MAKE_INT (c);
		    }
		    c2 = rep_stream_getc (strm);
		    if (c2 == EOF)
			goto eof;
		    if (!isalpha (c2))
		    {
			*c_p = c2;
			return rep_MAKE_INT (c);
		    }
		    c = tolower (c);
		    c2 = tolower (c2);
		    for (i = 0; char_names[i].name != 0; i++)
		    {
			if (char_names[i].name[0] == c
			    && char_names[i].name[1] == c2)
			{
			    char *ptr = char_names[i].name + 2;
			    while (1)
			    {
				c = fast_getc (strm);
				if (*ptr == 0)
				{
				    *c_p = c;
				    return rep_MAKE_INT (char_names[i].value);
				}
				if (c == EOF || tolower (c) != *ptr++)
				    goto error;
			    }
			}
		    }
		    goto error;
		}

	    case '!':
		if (rep_FILEP(strm))
		{
		    repv pos = Fseek_file (strm, Qnil, Qnil);
		    if (pos && rep_INTP(pos) && rep_INT(pos) == 2)
		    {
			/* #! at the start of the file. Skip until !# */
			comment_terminator = '!';
			goto read_comment;
		    }
		}
		goto error;

	    case 't': case 'T':
	    case 'f': case 'F':
		form = (tolower (*c_p) == 't') ? rep_scm_t : rep_scm_f;
		*c_p = rep_stream_getc (strm);
		return form;

	    case 'b': case 'B': case 'o': case 'O':
	    case 'd': case 'D': case 'x': case 'X':
	    case 'e': case 'E': case 'i': case 'I':
		rep_stream_ungetc (strm, *c_p);
		*c_p = '#';
		goto identifier;

	    default: error:
		return Fsignal(Qinvalid_read_syntax, rep_LIST_1(strm));
	    }

	default: identifier:
	    form = read_symbol(strm, c_p);
	    if (form && *c_p == '#' && rep_SYMBOLP (form))
	    {
		/* foo#bar expands to (structure-ref foo bar)
		   (this syntax is from Xerox scheme's module system) */
		repv var;
		*c_p = rep_stream_getc (strm);
		var = read_symbol (strm, c_p);
		if (var != 0)
		    return rep_list_3 (Qstructure_ref, form, var);
		else
		    return var;
	    }
	    return form;
	}
    }
    /* not reached */

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
	if(!(tmp = rep_eval(rep_CAR(list), Qnil)))
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
	*last = rep_eval(list, Qnil);
    rep_POPGC; rep_POPGC;
    return result;
}

static repv
copy_to_vector (repv argList, int nargs, repv *args,
		rep_bool eval_args, rep_bool eval_in_env)
{
    if (eval_args)
    {
	repv old_env = rep_env, old_struct = rep_structure;
	rep_GC_root gc_arglist;
	rep_GC_n_roots gc_args;
	rep_GC_root gc_old_env, gc_old_struct;
	int i;
	rep_PUSHGC(gc_arglist, argList);
	rep_PUSHGCN(gc_args, args, 0);
	if (!eval_in_env)
	{
	    rep_env = rep_call_stack->saved_env;
	    rep_structure = rep_call_stack->saved_structure;
	}
	rep_PUSHGC(gc_old_env, old_env);
	rep_PUSHGC(gc_old_struct, old_struct);
	for(i = 0; i < nargs; i++)
	{
	    if((args[i] = rep_eval(rep_CAR(argList), Qnil)) == rep_NULL)
	    {
		rep_POPGC;
		rep_POPGCN;
		rep_POPGC; rep_POPGC;
		return rep_NULL;
	    }
	    argList = rep_CDR(argList);
	    gc_args.count++;
	}
	rep_env = old_env;
	rep_structure = old_struct;
	rep_POPGC;
	rep_POPGCN;
	rep_POPGC; rep_POPGC;
    }
    else
    {
	int i;
	for (i = 0; i < nargs; i++)
	{
	    args[i] = rep_CAR (argList);
	    argList = rep_CDR (argList);
	}
    }
    return Qt;
}

repv
rep_bind_lambda_list_1 (repv lambdaList, repv *args, int nargs,
			repv (*binder) (repv, repv, repv))
{
    enum arg_state {
	STATE_REQUIRED = 1, STATE_OPTIONAL, STATE_REST
    };

    struct binding {
	struct binding *next;
	repv sym;
	repv value;
    };

    repv boundlist;
    enum arg_state state;
    struct binding *frame = 0;

    state = STATE_REQUIRED;
    while (1)
    {
	repv argspec;
	struct binding *item;

	if (rep_CONSP (lambdaList) && rep_SYMBOLP(rep_CAR (lambdaList)))
	{
	    argspec = rep_CAR (lambdaList);
	    lambdaList = rep_CDR (lambdaList);

	    if (rep_STR (rep_SYM (argspec)->name)[0] == '&')
	    {
		if (argspec == Qamp_optional)
		{
		    if (state > STATE_OPTIONAL)
			return Fsignal (Qinvalid_lambda_list,
					rep_LIST_1 (lambdaList));
		    state = STATE_OPTIONAL;
		    continue;
		}
		else if (argspec == Qamp_rest)
		{
		    if (state > STATE_REST)
			return Fsignal (Qinvalid_lambda_list,
					rep_LIST_1 (lambdaList));
		    state = STATE_REST;
		    continue;
		}
	    }
	}
	else if (lambdaList != Qnil && rep_SYMBOLP (lambdaList))
	{
	    state = STATE_REST;
	    argspec = lambdaList;
	    lambdaList = Qnil;
	}
	else
	    break;

	item = alloca (sizeof (struct binding));
	item->next = frame;
	frame = item;
	item->sym = argspec;

	switch (state)
	{
	case STATE_REQUIRED:
	case STATE_OPTIONAL:
	    if (nargs > 0)
	    {
		item->value = *args++;
		nargs--;
	    }
	    else if (state == STATE_OPTIONAL)
		item->value = Qnil;
	    else
	    {
		repv fun = rep_call_stack != 0 ? rep_call_stack->fun : Qnil;
		return Fsignal (Qmissing_arg, rep_list_2 (fun, argspec));
	    }
	    break;

	case STATE_REST:
	    {
		repv list = Qnil;
		repv *ptr = &list;
		while (nargs > 0)
		{
		    *ptr = Fcons (*args++, Qnil);
		    ptr = rep_CDRLOC (*ptr);
		    nargs--;
		}
		item->value = list;
	    }
	    goto out;
	    break;
	}

	rep_TEST_INT;
	if (rep_INTERRUPTP)
	    return rep_NULL;
    }
out:

    /* Instantiate the bindings in reverse order, so that they
       end up in the same order that the compiler compiles
       inline lambdas and tail-recursive function applications */
    boundlist = rep_NEW_FRAME;
    while (frame != 0)
    {
	boundlist = binder (boundlist, frame->sym, frame->value);
	frame = frame->next;
    }

    return boundlist;
}

/* format of lambda-lists is something like,

   [{required-symbols}] [&optional {optional-symbols}] [&rest symbol]

   Note that the lambdaList arg isn't protected from gc by this
   function; it's assumed that this is done by the caller.

   IMPORTANT: this expects the top of the call stack to have the
   saved environments in which arguments need to be evaluated */
repv
rep_bind_lambda_list(repv lambdaList, repv argList,
		     rep_bool eval_args, rep_bool eval_in_env)
{
    repv *evalled_args;
    int evalled_nargs;

    evalled_nargs = rep_list_length(argList);
    evalled_args = alloca(sizeof(repv) * evalled_nargs);

    /* Evaluate arguments, and stick them in the evalled_args array */
    if (!copy_to_vector (argList, evalled_nargs, evalled_args,
			 eval_args, eval_in_env))
    {
	return rep_NULL;
    }
   
    return rep_bind_lambda_list_1 (lambdaList, evalled_args,
				   evalled_nargs, rep_bind_symbol);
}

static repv
eval_lambda(repv lambdaExp, repv argList, rep_bool eval_args,
	    rep_bool eval_in_env, repv tail_posn)
{
    repv result;
again:
    result = rep_NULL;
    lambdaExp = rep_CDR(lambdaExp);
    if(rep_CONSP(lambdaExp))
    {
	repv boundlist;
	rep_GC_root gc_lambdaExp, gc_argList;

	rep_PUSHGC(gc_lambdaExp, lambdaExp);
	rep_PUSHGC(gc_argList, argList);
	boundlist = rep_bind_lambda_list(rep_CAR(lambdaExp), argList,
					 eval_args, eval_in_env);
	rep_POPGC; rep_POPGC;

	if(boundlist)
	{
	    /* The body of the function is only in the tail position
	       if the parameter list only creates lexical bindings */
	    repv new_tail_posn = !rep_SPEC_BINDINGS (boundlist) ? Qt : Qnil;

	    rep_GC_root gc_boundlist;
	    rep_PUSHGC(gc_boundlist, boundlist);
	    result = Fprogn(rep_CDR(lambdaExp), new_tail_posn);
	    rep_POPGC;
	    rep_unbind_symbols(boundlist);

	    if (tail_posn == Qnil
		&& result == rep_NULL && rep_throw_value
		&& rep_CAR (rep_throw_value) == TAIL_CALL_TAG
		&& rep_CONSP (rep_CDR (rep_throw_value)))
	    {
		/* tail position ends here, so unwrap the saved call */
		repv func = rep_CADR (rep_throw_value);
		repv args = rep_CDDR (rep_throw_value);
		rep_throw_value = rep_NULL;
		if (rep_FUNARGP (func) && rep_CONSP (rep_FUNARG (func)->fun)
		    && rep_CAR (rep_FUNARG (func)->fun) == Qlambda)
		{
		    rep_USE_FUNARG (func);
		    lambdaExp = rep_FUNARG (func)->fun;
		    argList = args;
		    eval_args = rep_FALSE;
		    goto again;
		}
		else
		    result = rep_apply (func, args);
	    }
	}
	else
	    result = rep_NULL;
    }
    return result;
}

DEFSTRING(invl_autoload, "Can only autoload from symbols");

/* Autoloads a value; FUNARG is a closure enclosing the autoload
   definition. The definition is a list `(autoload SYMBOL FILE ...)'
   This function tries to load FILE, then returns the value of SYMBOL
   if successful, or rep_NULL for some kind of error.

   Alternatively, for `(autoload SYMBOL SYMBOL-2 ...)' tries to intern
   the structure called SYMBOL-2

   IMPORTANT: to ensure security, closure FUNARG must be active when
   this function is called. */
repv
rep_load_autoload(repv funarg)
{
    repv aload_def, fun, file;

    if (!rep_FUNARGP(funarg))
    {
	return Fsignal(Qinvalid_autoload,
		       rep_list_2(funarg, rep_VAL(&invl_autoload)));
    }

    aload_def = rep_FUNARG(funarg)->fun;
    if (rep_CONSP(aload_def))
	aload_def = rep_CDR(aload_def);
    if (!rep_CONSP(aload_def)
	|| !rep_SYMBOLP(rep_CAR(aload_def))
	|| !rep_CONSP(rep_CDR(aload_def))
	|| !(rep_STRINGP(rep_CAR(rep_CDR(aload_def)))
	     || rep_SYMBOLP(rep_CAR(rep_CDR(aload_def)))))
    {
	return Fsignal(Qinvalid_autoload,
		       rep_list_2(aload_def, rep_VAL(&invl_autoload)));
    }

    fun = rep_CAR(aload_def);
    file = rep_CAR(rep_CDR(aload_def));

    if (rep_STRINGP (file))
    {
	/* loading a file */

	/* Check if the current environment is allowed to load */
	repv load = Fsymbol_value (Qload, Qnil);
	if (load != rep_NULL)
	{
	    rep_GC_root gc_fun, gc_funarg;
	    repv tmp;

	    /* trash the autoload defn, so we don't keep trying to
	       autoload indefinitely. */
	    rep_CDR(aload_def) = Qnil;

	    rep_PUSHGC(gc_funarg, funarg);
	    rep_PUSHGC(gc_fun, fun);
	    /* call through the value instead of just Fload'ing */
	    tmp = rep_call_lisp2 (load, file, Qt);
	    rep_POPGC; rep_POPGC;

	    if (!tmp)
		return rep_NULL;

	    fun = Fsymbol_value (fun, Qnil);
	}
	else
	    fun = rep_NULL;
    }
    else
    {
	/* a symbol naming a structure to intern */

	/* Check if the current environment is allowed to load */
	repv load = Fsymbol_value (Qrequire, Qnil);
	if (load != rep_NULL)
	{
	    rep_GC_root gc_fun, gc_funarg;
	    repv tmp;

	    rep_PUSHGC (gc_funarg, funarg);
	    rep_PUSHGC (gc_fun, fun);
	    tmp = rep_call_lisp1 (load, file);
	    rep_POPGC; rep_POPGC;

	    if (!tmp)
		return rep_NULL;

	    fun = Fsymbol_value (fun, Qnil);
	}
	else
	    fun = rep_NULL;
    }

    if (fun != rep_NULL)
    {
	/* Magically replace one closure by another without losing eq-ness */
	repv tmp = fun;
	if (rep_CONSP(tmp) && rep_CAR(tmp) == Qmacro)
	    tmp = rep_CDR(tmp);
	if (rep_FUNARGP(tmp))
	{
	    rep_FUNARG(funarg)->fun = rep_FUNARG(tmp)->fun;
	    rep_FUNARG(funarg)->name = rep_FUNARG(tmp)->name;
	    rep_FUNARG(funarg)->env = rep_FUNARG(tmp)->env;
	    rep_FUNARG(funarg)->structure = rep_FUNARG(tmp)->structure;
	}
	else
	    rep_FUNARG(funarg)->fun = Qnil;
    }
    return fun;
}

DEFUN ("%load-autoload", F_load_autoload,
       S_load_autoload, (repv def), rep_Subr1)
{
    rep_DECLARE1 (def, rep_FUNARGP);
    rep_USE_FUNARG(def);
    return rep_load_autoload (def);
}

DEFSTRING(max_depth, "max-lisp-depth exceeded, possible infinite recursion?");

static repv
funcall (repv fun, repv arglist, rep_bool eval_args, repv tail_posn)
{
    int type;
    repv result = rep_NULL;
    struct rep_Call lc;
    repv closure = rep_NULL;
    rep_GC_root gc_fun, gc_args, gc_closure;

    rep_TEST_INT;
    if(rep_INTERRUPTP)
	return rep_NULL;

    if(++rep_lisp_depth > rep_max_lisp_depth)
    {
	rep_lisp_depth--;
	return Fsignal(Qerror, rep_LIST_1(rep_VAL(&max_depth)));
    }

    rep_PUSHGC (gc_fun, fun);
    rep_PUSHGC (gc_args, arglist);
    rep_PUSHGC (gc_closure, closure);

    rep_MAY_YIELD;

    lc.fun = fun;
    lc.args = arglist;
    lc.args_evalled_p = eval_args ? Qnil : Qt;
    rep_PUSH_CALL (lc);

    if(rep_data_after_gc >= rep_gc_threshold)
	Fgarbage_collect (Qnil);

again:
    if (rep_FUNARGP(fun))
    {
	closure = fun;
	fun = rep_FUNARG(fun)->fun;
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
	if (closure)
	    rep_USE_FUNARG(closure);
	result = rep_SUBRNFUN(fun)(arglist);
	break;

    case rep_Subr0:
	if (closure)
	    rep_USE_FUNARG(closure);
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
		    argv[i] = rep_eval(rep_CAR(arglist), Qnil);
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
	if (closure)
	    rep_USE_FUNARG(closure);
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
	/* don't allow unclosed lambdas for security reasons */
	if(closure && car == Qlambda)
	{
	    rep_USE_FUNARG (closure);
	    result = eval_lambda (fun, arglist, eval_args,
				  rep_FALSE, tail_posn);
	}
	else if(car == Qmacro)
	{
	    /* A macro. This could occur if autoloading from
	       a macro definition. Try to accommodate.. */
	    if(eval_args)
		goto invalid;		/* can't expand from evaluated args */
	    fun = lc.fun = rep_CDR(fun);
	    goto again;
	}
	else if(closure && car == Qautoload)
	{
	    rep_USE_FUNARG(closure);
	    fun = rep_load_autoload(closure);
	    if(fun)
	    {
		lc.fun = fun;
		goto again;
	    }
	}
	else
	    goto invalid;
	break;

    case rep_Compiled:
	/* don't allow unclosed bytecode for security reasons */
	if (closure)
	{
	    int nargs;
	    repv *args;
	    
	    rep_USE_FUNARG(closure);

	    if (rep_bytecode_interpreter == 0)
		goto invalid;

	    nargs = rep_list_length (arglist);
	    args = alloca (sizeof (repv) * nargs);
	    if (!copy_to_vector (arglist, nargs, args, eval_args, rep_FALSE))
		result = rep_NULL;
	    else
		result = rep_bytecode_interpreter (fun, nargs, args);
	    break;
	}
	/* FALL THROUGH */

    default: invalid:
	Fsignal(Qinvalid_function, rep_LIST_1(lc.fun));
    }

    /* In case I missed a non-local exit somewhere.  */
    if(rep_throw_value != rep_NULL)
	result = rep_NULL;

end:
    rep_POP_CALL(lc);
    rep_POPGC; rep_POPGC; rep_POPGC;
    rep_lisp_depth--;
    return result;
}

/* Applies ARGLIST to FUN. If EVAL-ARGS is true, all arguments will be
   evaluated first. Note that both FUN and ARGLIST are gc-protected
   for the duration of this function. */
repv
rep_funcall(repv fun, repv arglist, rep_bool eval_args)
{
    return funcall (fun, arglist, eval_args, Qnil);
}

repv
rep_apply (repv fun, repv args)
{
    return rep_funcall (fun, args, rep_FALSE);
}

DEFUN("funcall", Ffuncall, Sfuncall, (repv args), rep_SubrN) /*
::doc:funcall::
funcall FUNCTION ARGS...

Calls FUNCTION with arguments ARGS... and returns the result.
::end:: */
{
    if(!rep_CONSP(args))
	return rep_signal_missing_arg(1);
    else
	return rep_funcall(rep_CAR(args), rep_CDR(args), rep_FALSE);
}

static repv
eval(repv obj, repv tail_posn)
{
    switch(rep_TYPE(obj))
    {
	repv ret;

    case rep_Symbol:
	return Fsymbol_value(obj, Qnil);

    case rep_Cons:
	if (++rep_lisp_depth > rep_max_lisp_depth)
	{
	    rep_lisp_depth--;
	    return Fsignal(Qerror, rep_LIST_1(rep_VAL(&max_depth)));
	}
	if (rep_CONSP (rep_CAR (obj)) && rep_CAAR (obj) == Qlambda
	    && Fsymbol_value (Qlambda, Qt) == rep_VAL (&Slambda))
	{
	    /* inline lambda; don't need to enclose it.. */
	    struct rep_Call lc;
	    lc.fun = rep_CAR (obj);
	    lc.args = rep_CDR (obj);
	    lc.args_evalled_p = Qnil;
	    rep_PUSH_CALL (lc);
	    ret = eval_lambda (rep_CAR (obj), rep_CDR (obj),
			       rep_TRUE, rep_TRUE, tail_posn);
	    rep_POP_CALL (lc);
	}
	else
	{
	    repv funcobj;
	    rep_GC_root gc_obj;
	    rep_PUSHGC (gc_obj, obj);
	    funcobj = rep_eval (rep_CAR(obj), Qnil);
	    rep_POPGC;
	    if(funcobj == rep_NULL)
		ret = rep_NULL;
	    else if(rep_CELL8_TYPEP(funcobj, rep_SF))
		ret = rep_SFFUN(funcobj)(rep_CDR(obj), tail_posn);
	    else if(rep_CONSP(funcobj) && rep_CAR(funcobj) == Qmacro)
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

		ret = form ? rep_eval (form, tail_posn) : rep_NULL;
	    }
	    else if (rep_FUNARGP (funcobj) && tail_posn != Qnil)
	    {
		/* This call can be performed later without losing any
		   state, so package it up, then throw back to the
		   innermost non-tail-position, where the function
		   call will be evaluated.. */

		repv args;

		rep_PUSHGC (gc_obj, funcobj);
		args = eval_list (rep_CDR (obj));
		rep_POPGC;

		if (args != rep_NULL)
		{
		    rep_throw_value = Fcons (TAIL_CALL_TAG,
					     Fcons (funcobj, args));
		}
		ret = rep_NULL;
	    }
	    else
	    {
		rep_lisp_depth--;
		return funcall (funcobj, rep_CDR(obj), rep_TRUE, tail_posn);
	    }
	}
	rep_lisp_depth--;
	return ret;

    default:
	return obj;
    }
    /* not reached */
}

repv
rep_eval (repv obj, repv tail_posn)
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
	Fgarbage_collect (Qnil);
	rep_POPGC;
    }

    if(!rep_single_step_flag)
	return eval(obj, tail_posn);

    DbDepth++;
    result = rep_NULL;

    {
	repv dbres;
	repv dbargs = rep_list_3(obj, rep_MAKE_INT(DbDepth),
				 rep_box_pointer (rep_call_stack));
	if(dbargs)
	{
	    rep_GC_root gc_dbargs;
	    struct rep_saved_regexp_data re_data;
	    rep_PUSHGC(gc_dbargs, dbargs);
	    rep_push_regexp_data(&re_data);
	    rep_single_step_flag = rep_FALSE;
	    dbres = (rep_call_with_barrier
		     (Ffuncall, Fcons (Fsymbol_value (Qdebug_entry, Qt),
				       dbargs), rep_TRUE, 0, 0, 0));
	    rep_pop_regexp_data();
	    if (dbres != rep_NULL && rep_CONSP(dbres))
	    {
		switch(rep_INT(rep_CAR(dbres)))
		{
		case 1:
		    /* single step cdr and following stuff  */
		    rep_single_step_flag = rep_TRUE;
		    result = eval(rep_CDR(dbres), Qnil);
		    rep_single_step_flag = rep_FALSE;
		    break;
		case 2:
		    /* run through cdr and step following  */
		    result = eval(rep_CDR(dbres), Qnil);
		    break;
		case 3:
		    /* run cdr and following  */
		    result = eval(rep_CDR(dbres), Qnil);
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
		    rep_push_regexp_data(&re_data);
		    rep_CAR(dbargs) = result;
		    dbres = (rep_call_with_barrier
			     (Ffuncall, Fcons (Fsymbol_value (Qdebug_exit, Qt),
					       dbargs), rep_TRUE, 0, 0, 0));
		    if(!dbres)
			result = rep_NULL;
		    rep_pop_regexp_data();
		}
	    }
	    rep_POPGC;
	}
    }
    DbDepth--;
    rep_single_step_flag = newssflag;
    return result;
}

repv
Feval (repv form)
{
    return rep_eval (form, Qnil);
}

DEFUN("progn", Fprogn, Sprogn, (repv args, repv tail_posn), rep_SF) /*
::doc:progn::
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
	result = rep_eval(rep_CAR(args),
			  rep_CDR (args) == Qnil ? tail_posn : Qnil);
	args = rep_CDR(args);
	rep_TEST_INT;
	if(!result || rep_INTERRUPTP)
	    break;
    }
    rep_POPGC;
    return result;
}

repv
rep_call_lispn (repv fun, int argc, repv *argv)
{
    if (rep_FUNARGP (fun) && rep_COMPILEDP (rep_FUNARG (fun)->fun)
	&& rep_bytecode_interpreter != 0)
    {
	/* Call to bytecode, avoid consing argument list */

	struct rep_Call lc;
	repv ret;

	lc.fun = fun;
	lc.args = rep_void_value;
	lc.args_evalled_p = Qt;
	rep_PUSH_CALL (lc);
	rep_USE_FUNARG (fun);
	ret = rep_bytecode_interpreter (rep_FUNARG (fun)->fun, argc, argv);
	rep_POP_CALL (lc);
	return ret;
    }
    else
    {
	repv args = Qnil;
	argv += argc;
	while (argc-- > 0)
	    args = Fcons (*(--argv), args);
	return rep_funcall (fun, args, rep_FALSE);
    }
}

repv
rep_call_lisp0(repv function)
{
    return rep_call_lispn (function, 0, 0);
}

repv
rep_call_lisp1(repv function, repv arg1)
{
    return rep_call_lispn (function, 1, &arg1);
}

repv
rep_call_lisp2(repv function, repv arg1, repv arg2)
{
    repv vec[2];
    vec[0] = arg1;
    vec[1] = arg2;
    return rep_call_lispn (function, 2, vec);
}

repv
rep_call_lisp3(repv function, repv arg1, repv arg2, repv arg3)
{
    repv vec[3];
    vec[0] = arg1;
    vec[1] = arg2;
    vec[2] = arg3;
    return rep_call_lispn (function, 3, vec);
}

repv
rep_call_lisp4(repv function, repv arg1, repv arg2, repv arg3, repv arg4)
{
    repv vec[4];
    vec[0] = arg1;
    vec[1] = arg2;
    vec[2] = arg3;
    vec[4] = arg4;
    return rep_call_lispn (function, 4, vec);
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
	    rep_stream_putc(strm, '[');
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

    case rep_Funarg:
	if (rep_STRINGP(rep_FUNARG(obj)->name))
	{
	    rep_stream_puts (strm, "#<closure ", -1, rep_FALSE);
	    rep_stream_puts (strm, rep_STR(rep_FUNARG(obj)->name),
			     -1, rep_FALSE);
	    rep_stream_putc (strm, '>');
	}
	else
	    rep_stream_puts(strm, "#<closure>", -1, rep_FALSE);
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
    int old = *data;
    if(rep_INTP(val))
	*data = rep_INT(val);
    return rep_MAKE_INT (old);
}

/* Similar, but for variables containing greater than 24 bits of data,
   passed around as a cons cell containing two integers */
repv
rep_handle_var_long_int(repv val, long *data)
{
    long old = *data;
    if(rep_LONG_INTP(val))
	*data = rep_LONG_INT(val);
    return rep_MAKE_LONG_INT(old);
}

DEFUN("break", Fbreak, Sbreak, (void), rep_Subr0) /*
::doc:break::
break

The next form to be evaluated will be done so through the Lisp debugger.
::end:: */
{
    rep_single_step_flag = rep_TRUE;
    return Qt;
}

DEFUN_INT("step", Fstep, Sstep, (repv form), rep_Subr1, "xForm to step through") /*
::doc:step::
step FORM

Use the Lisp debugger to evaluate FORM.
::end:: */
{
    repv res;
    rep_bool oldssf = rep_single_step_flag;
    rep_single_step_flag = rep_TRUE;
    res = rep_eval(form, Qnil);
    rep_single_step_flag = oldssf;
    return res;
}

DEFUN("signal", Fsignal, Ssignal, (repv error, repv data), rep_Subr2) /*
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
    repv tmp, errlist, on_error;
    /* Can only have one error at once.	 */
    if(rep_throw_value)
	return rep_NULL;
    rep_DECLARE1(error, rep_SYMBOLP);

    on_error = Fsymbol_value (Qbacktrace_on_error, Qt);
    if (on_error == Qt
	|| (rep_CONSP(on_error)
	    && (tmp = Fmemq (error, on_error)) && tmp != Qnil))
    {
	fprintf (stderr, "\nLisp backtrace:");
	Fbacktrace (Fstderr_file());
	fputs ("\n\n", stderr);
    }	

    errlist = Fcons(error, data);
    on_error = Fsymbol_value(Qdebug_on_error, Qt);
    if(((on_error != rep_NULL && on_error == Qt)
	|| (rep_CONSP(on_error)
	    && (tmp = Fmemq(error, on_error)) && !rep_NILP(tmp))))
    {
	/* Enter debugger. */
	rep_GC_root gc_on_error;
	rep_bool oldssflag = rep_single_step_flag;
	Fset(Qdebug_on_error, Qnil);
	rep_single_step_flag = rep_FALSE;
	rep_PUSHGC(gc_on_error, on_error);
	tmp = (rep_call_with_barrier
	       (Ffuncall, Fcons (Fsymbol_value (Qdebug_error_entry, Qt),
				 rep_list_2(errlist,
					    rep_box_pointer (rep_call_stack))),
		rep_TRUE, 0, 0, 0));
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

DEFUN("condition-case", Fcondition_case, Scondition_case,
      (repv args, repv tail_posn), rep_SF) /*
::doc:condition-case::
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
    res = rep_eval(rep_CAR(args), Qnil);
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
		res = Fprogn(rep_CDR(handler), Qnil);
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

void
rep_handle_error(repv error, repv data)
{
    static int mutex;
    if (mutex++ == 0)
    {
	repv fun = Fsymbol_value (Qerror_handler_function, Qt);
	if (Ffunctionp (fun) != Qnil)
	{
	    rep_call_lisp2 (fun, error, data);
	    goto out;
	}
    }

    Fbeep();
    Fwrite (Qt, rep_string_dup ("some kind of error occurred"), Qnil);

out:
    mutex--;
}

repv
rep_signal_arg_error(repv obj, int argNum)
{
    repv fun = rep_call_stack != 0 ? rep_call_stack->fun : Qnil;
    return Fsignal (Qbad_arg, rep_list_3 (fun, obj, rep_MAKE_INT (argNum)));
}

repv
rep_signal_missing_arg(int argnum)
{
    repv fun = rep_call_stack != 0 ? rep_call_stack->fun : Qnil;
    return Fsignal (Qmissing_arg, rep_list_2 (fun, rep_MAKE_INT (argnum)));
}

repv
rep_mem_error(void)
{
#if 0
    /* Nothing really checks for this error.. it will just cause crashes.. */
    return Fsignal(Qno_memory, Qnil);
#else
    fprintf (stderr, "rep: virtual memory exhausted\n");
    abort ();
#endif
}

DEFUN("backtrace", Fbacktrace, Sbacktrace, (repv strm), rep_Subr1) /*
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
    struct rep_Call *lc = rep_call_stack;

    if(rep_NILP(strm) && !(strm = Fsymbol_value(Qstandard_output, Qnil)))
	return rep_signal_arg_error (strm, 1);

    while (lc != 0)
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

DEFUN("debug-frame-environment", Fdebug_frame_environment,
      Sdebug_frame_environment, (repv pointer), rep_Subr1)
{
    struct rep_Call *fp = rep_call_stack;
    struct rep_Call *ptr = rep_unbox_pointer (pointer);
    rep_DECLARE(1, pointer, ptr != 0);
    while (fp != 0 && fp->next != ptr)
	fp = fp->next;
    if (fp != 0)
	return Fcons (fp->saved_env, fp->saved_structure);
    else
	return Qnil;
}

DEFUN ("debug-outer-frame", Fdebug_outer_frame,
       Sdebug_outer_frame, (repv pointer), rep_Subr1)
{
    struct rep_Call *fp = rep_call_stack;
    struct rep_Call *ptr = rep_unbox_pointer (pointer);
    rep_DECLARE(1, pointer, ptr != 0);
    while (fp != 0 && fp != ptr)
	fp = fp->next;
    if (fp != 0 && fp->next != 0)
	return rep_box_pointer (fp->next);
    else
	return Qnil;
}

DEFUN ("debug-inner-frame", Fdebug_inner_frame,
       Sdebug_inner_frame, (repv pointer), rep_Subr1)
{
    struct rep_Call *fp = rep_call_stack;
    struct rep_Call *ptr = rep_unbox_pointer (pointer);
    rep_DECLARE(1, pointer, ptr != 0);
    while (fp != 0 && fp->next != ptr)
	fp = fp->next;
    if (fp != 0)
	return rep_box_pointer (fp);
    else
	return Qnil;
}

DEFUN("max-lisp-depth", Fmax_lisp_depth, Smax_lisp_depth, (repv val), rep_Subr1) /*
::doc:max-lisp-depth::
max-lisp-depth [NEW-VALUE]

The maximum number of times that rep_funcall can be called recursively.

This is intended to stop infinite recursion, if the default value of 250 is
too small (you get errors in normal use) set it to something larger.
::end:: */
{
    return rep_handle_var_int(val, &rep_max_lisp_depth);
}

void
rep_lisp_init(void)
{
    rep_INTERN(quote); rep_INTERN(lambda); rep_INTERN(macro);
    rep_INTERN(backquote); rep_INTERN(backquote_unquote);
    rep_INTERN(backquote_splice);
    rep_INTERN(autoload); rep_INTERN(function);
    rep_INTERN(structure_ref);
    rep_INTERN_SPECIAL(standard_input); rep_INTERN_SPECIAL(standard_output);
    rep_INTERN_SPECIAL(debug_entry); rep_INTERN_SPECIAL(debug_exit);
    rep_INTERN_SPECIAL(debug_error_entry);
    rep_INTERN(amp_optional); rep_INTERN(amp_rest);
    rep_mark_static((repv *)&rep_throw_value);
    rep_ADD_SUBR(S_load_autoload);
    rep_ADD_SUBR(Sfuncall);
    rep_ADD_SUBR(Sprogn);
    rep_ADD_SUBR(Sbreak);
    rep_ADD_SUBR_INT(Sstep);
    rep_ADD_SUBR(Ssignal);
    rep_ADD_SUBR(Scondition_case);
    rep_ADD_SUBR(Sbacktrace);
    rep_ADD_SUBR(Smax_lisp_depth);
    rep_ADD_SUBR(Sdebug_frame_environment);
    rep_ADD_SUBR(Sdebug_outer_frame);
    rep_ADD_SUBR(Sdebug_inner_frame);

    /* Stuff for error-handling */
    rep_INTERN(error_message);
    rep_INTERN(error); rep_ERROR(error);
    rep_INTERN(invalid_function); rep_ERROR(invalid_function);
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
    rep_INTERN(no_memory); rep_ERROR(no_memory);
    rep_INTERN(user_interrupt); rep_ERROR(user_interrupt);
    rep_INTERN(arith_error); rep_ERROR(arith_error);
    rep_INTERN(term_interrupt);

    rep_INTERN_SPECIAL(debug_on_error);
    Fset (Qdebug_on_error, Qnil);
    rep_INTERN_SPECIAL(backtrace_on_error);
    Fset (Qbacktrace_on_error, Qnil);
    rep_INTERN_SPECIAL(debug_macros);
    Fset (Qdebug_macros, Qnil);
    rep_INTERN_SPECIAL(error_handler_function);

    rep_int_cell = Fcons(Quser_interrupt, Qnil);
    rep_mark_static(&rep_int_cell);
    rep_term_cell = Fcons(Qterm_interrupt, Qnil);
    rep_mark_static(&rep_term_cell);

    rep_INTERN_SPECIAL(print_escape); 
    rep_INTERN_SPECIAL(print_length);
    rep_INTERN_SPECIAL(print_level);
    Fset (Qprint_escape, Qnil);
    Fset (Qprint_length, Qnil);
    Fset (Qprint_level, Qnil);
    rep_INTERN(newlines);

    rep_INTERN(load);
    rep_INTERN(require);
}
