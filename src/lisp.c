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
#include <stdarg.h>
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
static repv ex_optional, ex_rest, ex_key;

/* When a `throw' happens a function stuffs a cons-cell in here with,
   (TAG . VALUE). An error is the above with TAG Qerror and VALUE a
   list of relevant data. */
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
DEFSTRING(err_void_value, "Unbound variable");
DEFSYM(bad_arg, "bad-arg");
DEFSTRING(err_bad_arg, "Bad argument");
DEFSYM(invalid_read_syntax, "invalid-read-syntax");
DEFSTRING(err_invalid_read_syntax, "Invalid read syntax");
DEFSYM(end_of_stream, "end-of-stream");
DEFSTRING(err_end_of_stream, "End of stream");
DEFSYM(premature_end_of_stream, "premature-end-of-stream");
DEFSTRING(err_premature_end_of_stream, "Premature end of stream");
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

DEFSYM(ellipsis, "...");

/* When rep_TRUE Feval() calls the "debug-entry" function */
rep_bool rep_single_step_flag;

/* Lexical environment. A list of (SYMBOL . VALUE). Any unbound variables
   are dereferenced in the current structure (global namespace)  */
repv rep_env;

/* Active special bindings, a list of (SYMBOL . VALUE) */
repv rep_special_bindings;

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

static int current_frame_id (void);


/* Reading */

/* The `c' variable which keeps coming up is the lookahead character,
   since each reader function normally has to look at the next character
   to see if it's what it wants. If not, the lookahead is given to
   someone else or unread before exiting... */

static repv readl (repv, register int *, repv);

static rep_bool read_local_file;

/* inline common case of reading from local files; this appears to
   decrease startup time by about 25% */
static inline int
fast_getc (repv stream)
{
    if (read_local_file)
    {
	int c = getc (rep_FILE (stream)->file.fh);
	if (c == '\n')
	    rep_FILE (stream)->line_number++;
	return c;
    }
    else
	return rep_stream_getc (stream);
}

static repv
signal_reader_error (repv type, repv stream, char *message)
{
    repv error_data = Qnil;
    if (message != 0)
	error_data = Fcons (rep_string_dup (message), error_data);
    if (rep_FILEP (stream))
    {
	if ((rep_FILE (stream)->car & rep_LFF_BOGUS_LINE_NUMBER) == 0)
	{
	    error_data = Fcons (rep_MAKE_INT (rep_FILE (stream)->line_number),
				error_data);
	}
	error_data = Fcons (rep_FILE (stream)->name, error_data);
    }
    else
	error_data = Fcons (stream, error_data);
    return Fsignal (type, error_data);
}

static void
read_comment (repv strm, int *c_p)
{
    char terminator = *c_p;
    register int c;
    int depth = 1;
    while ((c = fast_getc (strm)) != EOF)
    {
    again:
	if (c == terminator)
	{
	    c = rep_stream_getc (strm);
	    if (c == EOF || (c == '#' && --depth == 0))
		break;
	    else
		goto again;
	}
	else if (c == '#')
	{
	    c = rep_stream_getc (strm);
	    if (c == EOF)
		break;
	    else if (c == terminator)
		depth++;
	    else
		goto again;
	}
    }
    if (c != EOF)
	c = rep_stream_getc (strm);
    else
    {
	signal_reader_error (Qpremature_end_of_stream,
			     strm, "While reading a comment");
    }
    *c_p = c;
}

static repv
read_list(repv strm, register int *c_p)
{
    repv result = Qnil;
    repv last = rep_NULL;
    long start_line = read_local_file ? rep_FILE (strm)->line_number : -1;
    rep_GC_root gc_result;

    *c_p = rep_stream_getc(strm);
    rep_PUSHGC(gc_result, result);
    while(result != rep_NULL)
    {
	switch(*c_p)
	{
	case EOF:
	    result = signal_reader_error (Qpremature_end_of_stream,
					  strm, "While reading a list");
	    break;

	case ' ':
	case '\t':
	case '\n':
	case '\r':
	case '\f':
	    *c_p = fast_getc(strm);
	    continue;

	case ';':
	    {
		register int c;
		while((c = fast_getc(strm)) != EOF
		      && c != '\n' && c != '\f' && c != '\r')
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
		result = signal_reader_error (Qpremature_end_of_stream,
					      strm, "After `.' in list");
		goto end;

	    case ' ': case '\t': case '\n': case '\f': case '\r':
		if(last)
		{
		    repv this = readl(strm, c_p, Qpremature_end_of_stream);
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
		    result = signal_reader_error (Qinvalid_read_syntax,
						  strm, "Nothing to dot second element of cons to");
		    goto end;
		}
		continue;

	    default:
		rep_stream_ungetc (strm, *c_p);
		*c_p = '.';
	    }
	    goto do_default;

	case '#': {
		int c = rep_stream_getc (strm);
		if (c == EOF)
		    goto end;
		else if (c == '|')
		{
		    *c_p = c;
		    read_comment (strm, c_p);
		    if (rep_INTERRUPTP)
			return rep_NULL;
		    continue;
		}
		rep_stream_ungetc (strm, c);
	    }
	    goto do_default;

	default: do_default:
	    {
		register repv this = Fcons(Qnil, Qnil);
		if(last)
		    rep_CDR(last) = this;
		else
		    result = this;
		rep_CAR(this) = readl(strm, c_p, Qpremature_end_of_stream);
		if(rep_CAR (this) == rep_NULL)
		    result = rep_NULL;
		last = this;
	    }
	}
    }
end:
    rep_POPGC;

    if (result != rep_NULL)
	rep_record_origin (result, strm, start_line);

    return result;
}

/* Could be a symbol or a number */
static repv
read_symbol(repv strm, int *c_p, repv obarray)
{
    static repv buffer = rep_NULL;
    static size_t buflen = 240;

    repv result;
    char *buf;
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
	case ' ':  case '\t': case '\n': case '\f': case '\r':
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
		return signal_reader_error (Qpremature_end_of_stream,
					    strm, "After `\\' in identifer");
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
		return signal_reader_error (Qpremature_end_of_stream,
					    strm, "After `|' in identifier");
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
			    static rep_bool dep_hex, dep_octal;

			case 'x': case 'X':
			    rep_deprecated (&dep_hex, "`0xNN' hexadecimal read syntax");
			    radix = 16;
			    nfirst = i + 1;
			    break;

			case '0': case '1': case '2': case '3':
			case '4': case '5': case '6': case '7':
			    rep_deprecated (&dep_octal, "`0NN' octal read syntax");
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
    {
	result = signal_reader_error (Qinvalid_read_syntax, strm,
				      "Zero length identifier");
    }
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
	result = Ffind_symbol (rep_VAL(buffer), obarray);
	if (result != rep_NULL && result == Qnil)
	{
	    result = Fmake_symbol (rep_string_dupn (buf, i));
	    if (result != rep_NULL)
		result = Fintern_symbol (result, obarray);
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
    char *buf = rep_alloc(buflen);
    register char *cur = buf;
    char *bufend = buf + buflen;
    if(buf)
    {
	while((c != EOF) && (c != '"'))
	{
	    if(cur == bufend)
	    {
		register int newbuflen = buflen * 2;
		register char *newbuf = rep_alloc(newbuflen);
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
		    *cur++ = (char)rep_stream_read_esc(strm, &c);
	    }
	    else
	    {
		*cur++ = c;
		c = fast_getc(strm);
	    }
	}
	if(c == EOF)
	    result = signal_reader_error (Qpremature_end_of_stream,
					  strm, "While reading a string");
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

static repv
skip_chars (repv stream, const char *str, repv ret, int *ptr)
{
    int c;
    while (*str != 0)
    {
	c = rep_stream_getc (stream);
	if (c != *str++)
	{
	    char buf[256];
#ifdef HAVE_SNPRINTF
	    snprintf (buf, sizeof (buf), "Expecting `%s'", str - 1);
#else
	    sprintf (buf, "Expecting `%s'", str - 1);
#endif
	    return signal_reader_error (Qinvalid_read_syntax, stream, buf);
	}
    }

    c = rep_stream_getc (stream);
    switch (c)
    {
    case EOF:
    case ' ': case '\t': case '\n': case '\f': case '\r':
    case '(': case ')': case '[': case ']':
    case '\'': case '"': case ';': case ',':
    case '`':
	*ptr = c;
	return ret;

    default:
	return signal_reader_error (Qinvalid_read_syntax, stream,
				    "expected end of token");
    }
}

/* Using the above readlisp*() functions this classifies each type
   of expression and translates it into a lisp object (repv).
   Returns NULL in case of error. */
static repv
readl(repv strm, register int *c_p, repv end_of_stream_error)
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
	case '\r':
	    *c_p = fast_getc(strm);
	    continue;

	case ';':
	    {
		register int c;
		while((c = fast_getc(strm)) != EOF
		      && c != '\n' && c != '\f' && c != '\r')
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
		return signal_reader_error (Qpremature_end_of_stream,
					    strm, "During ` or ' syntax");
	    }
	    rep_CADR(form) = readl(strm, c_p, Qpremature_end_of_stream);
	    rep_POPGC;
	    if(rep_CADR(form) != rep_NULL)
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
		return signal_reader_error (Qpremature_end_of_stream,
					    strm, "During , syntax");

	    case '@':
		rep_CAR(form) = Qbackquote_splice;
		if((*c_p = rep_stream_getc(strm)) == EOF)
		{
		    rep_POPGC;
		    return signal_reader_error (Qpremature_end_of_stream,
						strm, "During ,@ syntax");
		}
	    }
	    rep_CADR(form) = readl(strm, c_p, Qpremature_end_of_stream);
	    rep_POPGC;
	    if(rep_CADR(form) != rep_NULL)
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
		    return signal_reader_error (Qpremature_end_of_stream,
						strm, "During ? syntax");
		case '\\':
		    if((*c_p = rep_stream_getc(strm)) == EOF)
			return signal_reader_error (Qpremature_end_of_stream,
						    strm, "During ? syntax");
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
		int c;

	    case EOF:
		return signal_reader_error (Qpremature_end_of_stream,
					    strm, "During # syntax");

	    case '\'':
		form = Fcons(Qfunction, Fcons(Qnil, Qnil));
		rep_PUSHGC(gc_form, form);
		if((*c_p = rep_stream_getc(strm)) == EOF)
		{
		    rep_POPGC;
		    return signal_reader_error (Qpremature_end_of_stream,
						strm, "During #' syntax");
		}
		rep_CADR(form) = readl(strm, c_p, Qpremature_end_of_stream);
		rep_POPGC;
		if(rep_CADR(form) == rep_NULL)
		    return rep_NULL;
		else
		    return form;

	    case '[':
		{
		    repv vec = read_vector(strm, c_p);
		    if(vec != rep_NULL)
		    {
			if(rep_VECT_LEN(vec) >= rep_COMPILED_MIN_SLOTS
			   && rep_STRINGP (rep_COMPILED_CODE (vec))
			   && rep_VECTORP (rep_COMPILED_CONSTANTS (vec))
			   && rep_INTP (rep_COMPILED_STACK (vec)))
			{
			    rep_COMPILED(vec)->car = (rep_COMPILED(vec)->car
						   & ~rep_CELL8_TYPE_MASK)
						  | rep_Compiled;
			    return vec;
			}
			return signal_reader_error (Qinvalid_read_syntax,
						    strm, "Invalid bytecode object");
		    }
		    break;
		}

	    case '(':
		return read_vector (strm, c_p);

	    case '|':
		/* comment delimited by `#| ... |#' */
		read_comment (strm, c_p);
		if (rep_INTERRUPTP)
		    return rep_NULL;
		continue;

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

		    int c2, i;

		    c = rep_stream_getc (strm);
		    if (c == EOF)
			return signal_reader_error (Qpremature_end_of_stream,
						    strm, "During #\\ syntax");
		    if (!isalpha (c))
		    {
			*c_p = rep_stream_getc (strm);
			return rep_MAKE_INT (c);
		    }
		    c2 = rep_stream_getc (strm);
		    if (!isalpha (c2) || c2 == EOF)
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
				    return signal_reader_error (Qinvalid_read_syntax, strm, "Unknown character name");
			    }
			}
		    }
		    return signal_reader_error (Qinvalid_read_syntax, strm, "Unknown character name");
		}

	    case '!':
		if (rep_FILEP(strm))
		{
		    repv pos = Fseek_file (strm, Qnil, Qnil);
		    if (pos && rep_INTP(pos) && rep_INT(pos) == 2)
		    {
			/* #! at the start of the file. Skip until !# */
			read_comment (strm, c_p);
			if (rep_INTERRUPTP)
			    return rep_NULL;
			continue;
		    }
		}
		c = rep_stream_getc (strm);
		switch (c)
		{
		case 'o': return skip_chars (strm, "ptional", ex_optional, c_p);
		case 'r': return skip_chars (strm, "est", ex_rest, c_p);
		case 'k': return skip_chars (strm, "ey", ex_key, c_p);
		default:  return signal_reader_error (Qinvalid_read_syntax, strm, "Unknown #! prefixed identifier");
		}

	    case ':':
		rep_stream_ungetc (strm, *c_p);
		*c_p = '#';
		form = read_symbol (strm, c_p, rep_keyword_obarray);
		if (form && rep_SYMBOLP (form))
		    rep_SYM (form)->car |= rep_SF_KEYWORD;
		return form;

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

	    case 'u':
		return skip_chars (strm, "ndefined", rep_undefined_value, c_p);

	    default:
		return signal_reader_error (Qinvalid_read_syntax,
					    strm, "Invalid token");
	    }

	default: identifier:
	    form = read_symbol(strm, c_p, rep_obarray);
	    if (form && *c_p == '#' && rep_SYMBOLP (form))
	    {
		/* foo#bar expands to (structure-ref foo bar)
		   (this syntax is from Xerox scheme's module system) */
		repv var;
		*c_p = rep_stream_getc (strm);
		var = read_symbol (strm, c_p, rep_obarray);
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
    return signal_reader_error (end_of_stream_error, rep_LIST_1(strm), 0);
}

repv
rep_readl (repv stream, int *c_p)
{
    repv form;
    rep_bool old = read_local_file;
    read_local_file = rep_FILEP (stream) && rep_LOCAL_FILE_P (stream);
    form = readl (stream, c_p, Qend_of_stream);
    read_local_file = old;
    return form;
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

static inline void
copy_to_vector (repv argList, int nargs, repv *args)
{
    int i;
    for (i = 0; i < nargs; i++)
    {
	args[i] = rep_CAR (argList);
	argList = rep_CDR (argList);
    }
}

static repv
bind_lambda_list_1 (repv lambdaList, repv *args, int nargs)
{
#define VAR_SYM 0
#define VAR_VALUE 1
#define VAR_EVALP 2
#define VAR_SIZE 3
#define VAR(i,j) vars[(i) * VAR_SIZE + (j)]

    repv *vars = alloca ((rep_list_length (lambdaList) + 1)
			 * VAR_SIZE * sizeof (repv));
    int nvars = 0;

    enum arg_state {
	STATE_REQUIRED = 1, STATE_OPTIONAL, STATE_KEY, STATE_REST
    };

    enum arg_state state;

    /* Pass 1: traverse the lambda list, recording var-value pairs
       and whether each value needs to be evaluated or not.. */

    state = STATE_REQUIRED;
    while (1)
    {
	repv argspec, def;

	if (rep_CONSP (lambdaList))
	{
	    argspec = rep_CAR (lambdaList);
	    lambdaList = rep_CDR (lambdaList);

	    if (argspec == ex_optional || argspec == Qamp_optional)
	    {
		static int dep;
		if (argspec == Qamp_optional)
		    rep_deprecated (&dep, "&optional in lambda list");
		if (state >= STATE_OPTIONAL) {
		invalid: return Fsignal (Qinvalid_lambda_list,
					 rep_LIST_1 (lambdaList));
		}
		state = STATE_OPTIONAL;
		continue;
	    }
	    else if (argspec == ex_key)
	    {
		if (state >= STATE_KEY)
		    goto invalid;
		state = STATE_KEY;
		continue;
	    }
	    else if (argspec == ex_rest || argspec == Qamp_rest)
	    {
		static int dep;
		if (argspec == Qamp_rest)
		    rep_deprecated (&dep, "&rest in lambda list");
		if (state >= STATE_REST)
		    goto invalid;
		state = STATE_REST;
		continue;
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

	if (rep_SYMBOLP (argspec))
	{
	    VAR (nvars, VAR_SYM) = argspec;
	    def = Qnil;
	}
	else if (rep_CONSP (argspec) && rep_SYMBOLP (rep_CAR (argspec)))
	{
	    VAR (nvars, VAR_SYM) = rep_CAR (argspec);
	    if (rep_CONSP (rep_CDR (argspec)))
		def = rep_CADR (argspec);
	    else
		def = Qnil;
	}
	else
	    goto invalid;

	VAR (nvars, VAR_EVALP) = Qnil;
	switch (state)
	{
	    repv key;
            int i;

	case STATE_REQUIRED:
	case STATE_OPTIONAL:
	    if (nargs > 0)
	    {
		VAR (nvars, VAR_VALUE) = *args++;
		nargs--;
	    }
	    else if (state == STATE_OPTIONAL)
	    {
		VAR (nvars, VAR_VALUE) = def;
		VAR (nvars, VAR_EVALP) = Qt;
	    }
	    else
	    {
		repv fun = rep_call_stack != 0 ? rep_call_stack->fun : Qnil;
		return Fsignal (Qmissing_arg, rep_list_2 (fun, argspec));
	    }
	    break;

	case STATE_KEY:
	    key = Fmake_keyword (VAR (nvars, VAR_SYM));
	    VAR (nvars, VAR_VALUE) = def;
	    VAR (nvars, VAR_EVALP) = Qt;
	    for (i = 0; i < nargs - 1; i++)
	    {
		if (args[i] == key && args[i+1] != rep_NULL)
		{
		    VAR (nvars, VAR_VALUE) = args[i+1];
		    VAR (nvars, VAR_EVALP) = Qnil;
		    args[i] = args[i+1] = rep_NULL;
		    break;
		}
	    }
	    break;

	case STATE_REST:
	    {
		repv list = Qnil;
		repv *ptr = &list;
		while (nargs > 0)
		{
		    if (*args != rep_NULL)
		    {
			*ptr = Fcons (*args, Qnil);
			ptr = rep_CDRLOC (*ptr);
		    }
		    args++; nargs--;
		}
		VAR (nvars, VAR_VALUE) = list;
	    }
	    nvars++;
	    goto out;
	    break;
	}

	nvars++;

	rep_TEST_INT;
	if (rep_INTERRUPTP)
	    return rep_NULL;
    }

out:
    /* Pass 2: evaluate any values that need it.. */
    {
	int i;
	rep_GC_n_roots gc_vars;
	rep_PUSHGCN (gc_vars, vars, nvars * VAR_SIZE);
	for (i = 0; i < nvars; i++)
	{
	    if (VAR (i, VAR_EVALP) != Qnil)
	    {
		repv tem = Feval (VAR (i, VAR_VALUE));
		if (tem == rep_NULL)
		{
		    rep_POPGCN;
		    return rep_NULL;
		}
		VAR (i, VAR_VALUE) = tem;
	    }
	}
	rep_POPGCN;
    }

    /* Pass 3: instantiate the bindings */
    {
	int i;
	repv boundlist = rep_NEW_FRAME;
	for (i = 0; i < nvars; i++)
	{
	    boundlist = rep_bind_symbol (boundlist, VAR (i, VAR_SYM),
					 VAR (i, VAR_VALUE));
	}
	return boundlist;
    }
}

/* format of lambda-lists is something like,

   [<required-params>*] [#!optional <optional-param>*]
   [#!key <keyword-param>*] [#!rest <rest-param>]

   A keyword parameter X is associated with an argument by a keyword
   symbol #:X. If no such symbol exists, it's bound to false

   <optional-param> and <keyword-param> is either <symbol> or (<symbol>
   <default>) where <default> is a constant

   Note that the lambdaList arg isn't protected from gc by this
   function; it's assumed that this is done by the caller.

   IMPORTANT: this expects the top of the call stack to have the
   saved environments in which arguments need to be evaluated */
static repv
bind_lambda_list(repv lambdaList, repv argList)
{
    repv *argv;
    int argc;

    argc = rep_list_length (argList);
    argv = alloca (sizeof (repv) * argc);

    /* Evaluate arguments, and stick them in the evalled_args array */
    copy_to_vector (argList, argc, argv);

    return bind_lambda_list_1 (lambdaList, argv, argc);
}

static repv
eval_lambda(repv lambdaExp, repv argList, repv tail_posn)
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
	boundlist = bind_lambda_list(rep_CAR(lambdaExp), argList);
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

   IMPORTANT: to ensure security, closure FUNARG must be active when
   this function is called. */
repv
rep_load_autoload(repv funarg)
{
    repv aload_def, fun, file, load;

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
	|| !rep_STRINGP(rep_CAR(rep_CDR(aload_def))))
    {
	return Fsignal(Qinvalid_autoload,
		       rep_list_2(aload_def, rep_VAL(&invl_autoload)));
    }

    fun = rep_CAR(aload_def);
    file = rep_CAR(rep_CDR(aload_def));

    /* loading a file */

    /* Check if the current environment is allowed to load */
    load = Fsymbol_value (Qload, Qnil);
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

DEFUN ("load-autoload", Fload_autoload,
       Sload_autoload, (repv def), rep_Subr1)
{
    rep_DECLARE1 (def, rep_FUNARGP);
    rep_USE_FUNARG(def);
    return rep_load_autoload (def);
}

DEFSTRING(max_depth, "max-lisp-depth exceeded, possible infinite recursion?");

static repv
apply (repv fun, repv arglist, repv tail_posn)
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

    case rep_SubrN:
	if (closure)
	    rep_USE_FUNARG(closure);
	if (!rep_SUBR_VEC_P (fun))
	    result = rep_SUBRNFUN(fun)(arglist);
	else
	{
	    int length;
	    repv *vec;

	    length = rep_list_length (arglist);
	    vec = alloca (length * sizeof (repv));
	    copy_to_vector (arglist, length, vec);

	    result = rep_SUBRVFUN (fun) (length, vec);
	}
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
	for(i = 0; i < nargs; i++)
	{
	    if(rep_CONSP(arglist))
	    {
		argv[i] = rep_CAR(arglist);
		arglist = rep_CDR(arglist);
	    }
	    else
		break;
	}
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
	    result = eval_lambda (fun, arglist, tail_posn);
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
	    repv (*bc_apply) (repv, int, repv *);

	    rep_USE_FUNARG(closure);
	    bc_apply = rep_STRUCTURE (rep_structure)->apply_bytecode;

	    nargs = rep_list_length (arglist);
	    args = alloca (sizeof (repv) * nargs);
	    copy_to_vector (arglist, nargs, args);
	    if (bc_apply == 0)
		result = rep_apply_bytecode (fun, nargs, args);
	    else
		result = bc_apply (fun, nargs, args);
	    break;
	}
	/* FALL THROUGH */

    default: invalid:
	Fsignal(Qinvalid_function, rep_LIST_1(lc.fun));
    }

    /* In case I missed a non-local exit somewhere.  */
    if(rep_throw_value != rep_NULL)
	result = rep_NULL;

    if ((result == rep_NULL && rep_throw_value == rep_NULL)
	|| (result != rep_NULL && rep_throw_value != rep_NULL))
    {
	fprintf (stderr, "rep: function returned both exception and value, or neither!\n");
	if (lc.fun && Fsubrp (lc.fun) != Qnil
	    && rep_STRINGP (rep_XSUBR (lc.fun)->name))
	{
	    fprintf (stderr, "rep: culprit is subr %s\n",
		     rep_STR (rep_XSUBR (lc.fun)->name));
	}
    }

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
    if (eval_args)
    {
	rep_GC_root gc_fun;
	rep_PUSHGC (gc_fun, fun);
	arglist = eval_list (arglist);
	rep_POPGC;
    }

    return apply (fun, arglist, Qnil);
}

repv
rep_apply (repv fun, repv args)
{
    return apply (fun, args, Qnil);
}

DEFUN("funcall", Ffuncall, Sfuncall, (repv args), rep_SubrN) /*
::doc:rep.lang.interpreter#funcall::
funcall FUNCTION ARGS...

Calls FUNCTION with arguments ARGS... and returns the result.
::end:: */
{
    if(!rep_CONSP(args))
	return rep_signal_missing_arg(1);
    else
	return apply(rep_CAR(args), rep_CDR(args), Qnil);
}

DEFUN("apply", Fapply, Sapply, (repv args), rep_SubrN) /*
::doc:rep.lang.interpreter#apply::
apply FUNCTION ARGS... ARG-LIST

Calls FUNCTION passing all of ARGS to it as well as all elements in ARG-LIST.
ie,
  (apply + 1 2 3 '(4 5 6))
   => 21
::end:: */
{
    repv list = Qnil, *last;
    last = &list;
    if(rep_CONSP(args))
    {
	while(rep_CONSP(rep_CDR(args)))
	{
	    if(!(*last = Fcons(rep_CAR(args), Qnil)))
		return(rep_NULL);
	    last = &rep_CDR(*last);
	    args = rep_CDR(args);
	    rep_TEST_INT;
	    if(rep_INTERRUPTP)
		return(rep_NULL);
	}
	if(!rep_NILP(Flistp(rep_CAR(args))))
	    *last = rep_CAR(args);
	else
	    return rep_signal_arg_error (rep_CAR (args), -1);
	return(Ffuncall(list));
    }
    return rep_signal_missing_arg(1);
}

static repv
eval(repv obj, repv tail_posn)
{
    switch(rep_TYPE(obj))
    {
	repv ret;

    case rep_Symbol:
	if (!rep_KEYWORDP (obj))
	    return Fsymbol_value(obj, Qnil);
	else
	    return obj;

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
	    rep_GC_root gc_obj;
	    struct rep_Call lc;

	    rep_PUSHGC (gc_obj, obj);
	    ret = eval_list (rep_CDR (obj));
	    rep_POPGC;

	    if (ret != rep_NULL)
	    {
		lc.fun = rep_CAR (obj);
		lc.args = ret;
		rep_PUSH_CALL (lc);

		ret = eval_lambda (rep_CAR (obj), ret, tail_posn);

		rep_POP_CALL (lc);
	    }
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
	    else if (tail_posn != Qnil &&
		     (rep_FUNARGP (funcobj) || funcobj == rep_VAL (&Sapply)))
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
		    if (funcobj == rep_VAL (&Sapply))
		    {
			if (!rep_CONSP (args))
			    ret = rep_signal_missing_arg (1);
			else
			{
			    int len = rep_list_length (rep_CDR (args));
			    repv *vec = alloca (len * sizeof (repv));
			    copy_to_vector (rep_CDR (args), len, vec);
			    rep_CDR (args) = Flist_star (len, vec);
			}
		    }
		    else
			args = Fcons (funcobj, args);

		    rep_throw_value = Fcons (TAIL_CALL_TAG, args);
		}
		ret = rep_NULL;
	    }
	    else
	    {
		rep_lisp_depth--;

		rep_PUSHGC (gc_obj, funcobj);
		ret = eval_list (rep_CDR (obj));
		rep_POPGC;

		if (ret != rep_NULL)
		    ret = apply (funcobj, ret, tail_posn);

		return ret;
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
				 rep_MAKE_INT (current_frame_id ()));
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
::doc:rep.lang.interpreter#progn::
progn FORMS...

Eval's each of the FORMS in order returning the value of the last
one.
::end:: */
{
    repv result = Qnil;
    repv old_current = rep_call_stack != 0 ? rep_call_stack->current_form : 0;
    rep_GC_root gc_args, gc_old_current;
    rep_PUSHGC (gc_args, args);
    rep_PUSHGC (gc_old_current, old_current);
    while (rep_CONSP (args))
    {
	if (rep_call_stack != 0)
	    rep_call_stack->current_form = rep_CAR (args);

	result = rep_eval(rep_CAR(args),
			  rep_CDR (args) == Qnil ? tail_posn : Qnil);
	args = rep_CDR(args);
	rep_TEST_INT;
	if(!result || rep_INTERRUPTP)
	    break;
    }
    if (rep_call_stack != 0)
	rep_call_stack->current_form = old_current;

    rep_POPGC; rep_POPGC;
    return result;
}

repv
rep_call_lispn (repv fun, int argc, repv *argv)
{
    if (rep_FUNARGP (fun) && rep_COMPILEDP (rep_FUNARG (fun)->fun))
    {
	/* Call to bytecode, avoid consing argument list */

	struct rep_Call lc;
	repv ret;
	repv (*bc_apply) (repv, int, repv *);

	lc.fun = fun;
	lc.args = rep_void_value;
	rep_PUSH_CALL (lc);
	rep_USE_FUNARG (fun);
	bc_apply = rep_STRUCTURE (rep_structure)->apply_bytecode;
	/* if (bc_apply == 0) */
	    ret = rep_apply_bytecode (rep_FUNARG (fun)->fun, argc, argv);
	/* else
        ret = bc_apply (rep_FUNARG (fun)->fun, argc, argv); */
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
    vec[3] = arg4;
    return rep_call_lispn (function, 4, vec);
}

void
rep_lisp_prin(repv strm, repv obj)
{
    static int print_level = 0;

    switch(rep_TYPE(obj))
    {
	char tbuf[40];
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
	rep_stream_puts (strm, "#<closure ", -1, rep_FALSE);
	if (rep_STRINGP(rep_FUNARG(obj)->name))
	{
	    rep_stream_puts (strm, rep_STR(rep_FUNARG(obj)->name),
			     -1, rep_FALSE);
	}
	else
	{
#ifdef HAVE_SNPRINTF
	    snprintf (tbuf, sizeof(tbuf), "%" rep_PTR_SIZED_INT_CONV "x", obj);
#else
	    sprintf (tbuf, "%" rep_PTR_SIZED_INT_CONV "x", obj);
#endif
	    rep_stream_puts (strm, tbuf, -1, rep_FALSE);
	}
	rep_stream_putc (strm, '>');
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
    char *s = rep_STR(obj);
    char buf[BUFSIZ];
    int bufptr = 0;
    unsigned char c;

#define OUT(c)							\
    do {							\
	if (bufptr == BUFSIZ) {					\
	    rep_stream_puts (strm, buf, BUFSIZ, rep_FALSE);	\
	    bufptr = 0;						\
	}							\
	buf[bufptr++] = (c);					\
    } while (0)

    rep_bool escape_all, escape_newlines;
    repv tem = Fsymbol_value(Qprint_escape, Qt);
    if(tem == Qnewlines)
	escape_all = rep_FALSE, escape_newlines = rep_TRUE;
    else if(tem == Qt)
	escape_all = rep_TRUE, escape_newlines = rep_TRUE;
    else
	escape_all = rep_FALSE, escape_newlines = rep_FALSE;

    OUT ('"');
    while(len-- > 0)
    {
	c = *s++;
	if(escape_all && (c < 32 || c > 126))
	{
	    OUT ('\\');
	    OUT ('0' + c / 64);
	    OUT ('0' + (c % 64) / 8);
	    OUT ('0' + c % 8);
	}
	else
	{
	    switch(c)
	    {
	    case '\t':
	    case '\n':
	    case '\r':
	    case '\f':
		if(!escape_newlines)
		    OUT (c);
		else {
		    OUT ('\\');
		    c = (c == '\t' ? 't'
			 : c == '\n' ? 'n'
			 : c == '\r' ? 'r'
			 : 'f');
		    OUT (c);
		}
		break;

	    case '\\':
		OUT ('\\');
		OUT ('\\');
		break;

	    case '"':
		OUT ('\\');
		OUT ('"');
		break;

	    default:
		OUT (c);
	    }
	}
    }
    OUT ('"');
    if (bufptr > 0)
	rep_stream_puts (strm, buf, bufptr, rep_FALSE);
}

#undef OUT

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

/* FIXME: required by sawfish; remove at some point */
repv Fnconc (repv args)
{
    int len;
    repv *vec;

    len = rep_list_length (args);
    vec = alloca (len * sizeof (repv));
    copy_to_vector (args, len, vec);
    return Fnconc_ (len, vec);
}

/* Used to assign a list of argument values into separate variables.
   Note that optional args without values _are not_ initialized to nil,
   the caller of this function should do that.. */
rep_bool
rep_assign_args (repv list, int required, int total, ...)
{
    int i;
    va_list vars;
    va_start (vars, total);
    for (i = 0; i < total; i++)
    {
	repv *varp = va_arg (vars, repv *);
	if (!rep_CONSP (list))
	{
	    if (i >= required)
		return rep_TRUE;
	    else
	    {
		rep_signal_missing_arg (i);
		return rep_FALSE;
	    }
	}
	*varp = rep_CAR (list);
	list = rep_CDR (list);
	rep_TEST_INT;
	if (rep_INTERRUPTP)
	    return rep_FALSE;
    }
    return rep_TRUE;
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
::doc:rep.lang.debug#break::
break

The next form to be evaluated will be done so through the Lisp debugger.
::end:: */
{
    rep_single_step_flag = rep_TRUE;
    return Qt;
}

DEFUN_INT("step", Fstep, Sstep, (repv form), rep_Subr1, "xForm to step through") /*
::doc:rep.lang.debug#step::
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
::doc:rep.lang.interpreter#signal::
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
    if ((on_error == Qt && error != Qend_of_stream)
	|| (rep_CONSP(on_error)
	    && (tmp = Fmemq (error, on_error)) && tmp != Qnil))
    {
	fprintf (stderr, "\nLisp backtrace:\n");
	Fbacktrace (Fstderr_file());
	fputs ("\n", stderr);
    }

    errlist = Fcons(error, data);
    on_error = Fsymbol_value(Qdebug_on_error, Qt);
    if(((on_error != rep_NULL && on_error == Qt && error != Qend_of_stream)
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
				 rep_list_2(errlist, rep_MAKE_INT (current_frame_id ()))),
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

void
rep_handle_error(repv error, repv data)
{
    DEFSTRING (some_error, "some kind of error occurred");

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
    Fwrite (Qt, rep_VAL (&some_error), Qnil);

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

static int
current_frame_id (void)
{
    int i;
    struct rep_Call *lc;
    i = 0;
    for (lc = rep_call_stack; lc != 0; lc = lc->next)
	i++;
    return i - 1;
}

static struct rep_Call *
stack_frame_ref (int idx)
{
    struct rep_Call *lc;
    int total, wanted;

    total = 0;
    for (lc = rep_call_stack; lc != 0; lc = lc->next)
	total++;

    wanted = (total - 1) - idx;
    if (wanted < 0)
	return 0;

    for (lc = rep_call_stack; lc != 0; lc = lc->next)
    {
	if (wanted-- == 0)
	    return lc;
    }

    return 0;
}

DEFUN("backtrace", Fbacktrace, Sbacktrace, (repv strm), rep_Subr1) /*
::doc:rep.lang.debug#backtrace::
backtrace [STREAM]

Prints a backtrace of the current Lisp call stack to STREAM (or to
`standard-output').
The format is something like:
  FUNCTION (ARGLIST) ARGS-EVALLED-P
where ARGS-EVALLED-P is either `t' or `nil', depending on whether or not
ARGLIST had been evaluated or not before being put into the stack.
::end:: */
{
    repv old_print_escape = Fsymbol_value (Qprint_escape, Qt);
    int total_frames, i;

    if(rep_NILP(strm) && !(strm = Fsymbol_value(Qstandard_output, Qnil)))
	return rep_signal_arg_error (strm, 1);

    Fset (Qprint_escape, Qt);

    total_frames = current_frame_id () + 1;
    i = 0;

    for (i = total_frames - 1; i >= 0; i--)
    {
	struct rep_Call *lc = stack_frame_ref (i);
	repv function_name = Qnil;

	if (lc == 0)
	    continue;

	if (rep_FUNARGP (lc->fun))
	{
	    if (rep_STRINGP (rep_FUNARG (lc->fun)->name))
		function_name = rep_FUNARG (lc->fun)->name;
	}
	else if (Fsubrp (lc->fun) != Qnil)
	{
	    if (rep_STRINGP (rep_XSUBR (lc->fun)->name))
		function_name = rep_XSUBR (lc->fun)->name;
	}
	else if (rep_CONSP (lc->fun) && rep_CAR (lc->fun) == Qlambda
		 && rep_CONSP (rep_CDR (lc->fun)))
	{
	    function_name = rep_list_3 (Qlambda, rep_CADR (lc->fun),
					Qellipsis);
	}

	if (function_name != Qnil)
	{
	    char buf[16];

	    sprintf (buf, "#%-3d ", i);
	    rep_stream_puts (strm, buf, -1, rep_FALSE);

	    rep_princ_val (strm, function_name);

	    if (rep_VOIDP (lc->args)
		|| (rep_STRINGP (function_name)
		    && strcmp (rep_STR (function_name), "run-byte-code") == 0))
		rep_stream_puts (strm, " ...", -1, rep_FALSE);
	    else
	    {
		rep_stream_putc (strm, ' ');
		rep_print_val (strm, lc->args);
	    }

	    if (lc->current_form != rep_NULL)
	    {
		repv origin = Flexical_origin (lc->current_form);
		if (origin && origin != Qnil)
		{
		    char buf[256];
#ifdef HAVE_SNPRINTF
		    snprintf (buf, sizeof (buf), " at %s:%ld",
			      rep_STR (rep_CAR (origin)),
			      (long) rep_INT (rep_CDR (origin)));
#else
		    sprintf (buf, " at %s:%ld",
			     rep_STR (rep_CAR (origin)),
			     (long) rep_INT (rep_CDR (origin)));
#endif
		    rep_stream_puts (strm, buf, -1, rep_FALSE);
		}
	    }

	    rep_stream_putc (strm, '\n');
	}
    }

    Fset (Qprint_escape, old_print_escape);

    return Qt;
}

DEFUN ("stack-frame-ref", Fstack_frame_ref,
       Sstack_frame_ref, (repv idx), rep_Subr1)
{
    struct rep_Call *lc;

    rep_DECLARE1 (idx, rep_INTP);

    lc = stack_frame_ref (rep_INT (idx));

    if (lc != 0)
    {
	return rep_list_5 (lc->fun, rep_VOIDP (lc->args)
			   ? rep_undefined_value : lc->args,
			   lc->current_form ? lc->current_form : Qnil,
			   lc->saved_env, lc->saved_structure);
    }
    else
	return Qnil;
}

DEFUN("max-lisp-depth", Fmax_lisp_depth, Smax_lisp_depth, (repv val), rep_Subr1) /*
::doc:rep.lang.interpreter#max-lisp-depth::
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
    DEFSTRING (optional, "#!optional");
    DEFSTRING (rest, "#!rest");
    DEFSTRING (key, "#!key");
    repv tem;

    rep_INTERN(quote); rep_INTERN(lambda); rep_INTERN(macro);
    rep_INTERN(backquote); rep_INTERN(backquote_unquote);
    rep_INTERN(backquote_splice);
    rep_INTERN(autoload); rep_INTERN(function);
    rep_INTERN(structure_ref);
    rep_INTERN_SPECIAL(standard_input); rep_INTERN_SPECIAL(standard_output);
    rep_INTERN_SPECIAL(debug_entry); rep_INTERN_SPECIAL(debug_exit);
    rep_INTERN_SPECIAL(debug_error_entry);
    rep_INTERN(amp_optional); rep_INTERN(amp_rest);

    ex_optional = Fmake_symbol (rep_VAL (&optional));
    ex_rest = Fmake_symbol (rep_VAL (&rest));
    ex_key = Fmake_symbol (rep_VAL (&key));
    rep_SYM(ex_optional)->car |= rep_SF_LITERAL;
    rep_SYM(ex_rest)->car |= rep_SF_LITERAL;
    rep_SYM(ex_key)->car |= rep_SF_LITERAL;
    rep_mark_static (&ex_optional);
    rep_mark_static (&ex_rest);
    rep_mark_static (&ex_key);

    rep_mark_static((repv *)&rep_throw_value);

    tem = rep_push_structure ("rep.lang.interpreter");
    rep_ADD_SUBR(Sload_autoload);
    rep_ADD_SUBR(Sfuncall);
    rep_ADD_SUBR(Sapply);
    rep_ADD_SUBR(Sprogn);
    rep_ADD_SUBR(Ssignal);
    rep_ADD_SUBR(Smax_lisp_depth);
    rep_pop_structure (tem);

    tem = rep_push_structure ("rep.lang.debug");
    rep_ADD_SUBR(Sbreak);
    rep_ADD_SUBR_INT(Sstep);
    rep_ADD_SUBR(Sbacktrace);
    rep_ADD_SUBR(Sstack_frame_ref);
    rep_pop_structure (tem);

    /* Stuff for error-handling */
    rep_INTERN(error_message);
    rep_INTERN(error); rep_ERROR(error);
    rep_INTERN(invalid_function); rep_ERROR(invalid_function);
    rep_INTERN(void_value); rep_ERROR(void_value);
    rep_INTERN(bad_arg); rep_ERROR(bad_arg);
    rep_INTERN(invalid_read_syntax); rep_ERROR(invalid_read_syntax);
    rep_INTERN(end_of_stream); rep_ERROR(end_of_stream);
    rep_INTERN(premature_end_of_stream); rep_ERROR(premature_end_of_stream);
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

    rep_INTERN(ellipsis);

    /* Allow the bootstrap code to work.. */
    rep_STRUCTURE (rep_default_structure)->imports
	= Fcons (Qrep_lang_interpreter,
		 rep_STRUCTURE (rep_default_structure)->imports);
}
