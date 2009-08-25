/* streams.c -- Lisp stream handling
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

/* These are the Lisp objects which are classed as streams:

   FILE: [rw]
   MARK: [rw] advance pos attribute of mark afterwards
   BUFFER: [rw] from cursor pos
   (NUMBER . STRING): [r] from the NUMBER'th char of STRING
   (STRING . ACTUAL-LENGTH): [w] to after INDEX
   (BUFFER . POS): [rw] from BUFFER, POS is advanced
   (BUFFER . t): [w] end of BUFFER
   FUNCTION: [rw] call FUNCTION, when reading FUNCTION is expected to
  		  return the next character, when writing it is called with
  		  one arg, either character or string.
   PROCESS: [w] write to the stdin of the PROCESS if it's running
   t: [w] display in status line

   Note that when using any of the three BUFFER stream types, the buffer's
   restriction is respected. */

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
#include <fcntl.h>
#include <ctype.h>
#include <stdlib.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

DEFSYM(format_hooks_alist, "format-hooks-alist"); /*
::doc:format-hooks-alist::
Alist of (CHAR . FUNCTION) defining extra format conversions for the
format function. FUNCTION is called as (FUNCTION repv), and should
return the string to be inserted.
::end:: */

int
rep_stream_getc(repv stream)
{
    int c = EOF;
    if(rep_NILP(stream)
       && !(stream = Fsymbol_value(Qstandard_input, Qnil)))
	return c;
    switch(rep_TYPE(stream))
    {
	repv res;
	rep_type *t;

    case rep_Cons:
	res = rep_CAR(stream);
	if(rep_INTP(res) && rep_STRINGP(rep_CDR(stream)))
	{
	    if (rep_INT(res) < rep_STRING_LEN(rep_CDR(stream)))
	    {
		c = (int) ((unsigned char *)rep_STR(rep_CDR(stream)))[rep_INT(res)];
		rep_CAR(stream) = rep_MAKE_INT(rep_INT(res) + 1);
	    }
	    else
		c = EOF;
	    break;
	}
	else if(res == Qlambda)
	    goto function;
	else
	{
	    t = rep_get_data_type(rep_TYPE(rep_CAR(stream)));
	    if (t->getc != 0)
		c = (t->getc)(stream);
	    else
		Fsignal(Qinvalid_stream, rep_LIST_1(stream));
	}
	break;

    case rep_Funarg:
    function:
	if((res = rep_call_lisp0(stream)) && rep_INTP(res))
	    c = rep_INT(res);
	break;

    default:
	if (rep_FILEP(stream))
	{
	    if(rep_NILP(rep_FILE(stream)->name))
		c = EOF;
	    else if(rep_LOCAL_FILE_P(stream))
		c = getc(rep_FILE(stream)->file.fh);
	    else
		c = rep_stream_getc (rep_FILE(stream)->file.stream);

	    if (c == '\n')
		rep_FILE (stream)->line_number++;
	    break;
	}

	t = rep_get_data_type(rep_TYPE(stream));
	if (t->getc != 0)
	    c = (t->getc)(stream);
	else
	    Fsignal(Qinvalid_stream, rep_LIST_1(stream));
    }
    return c;
}

/* Puts back one character, it will be read next call to streamgetc on
   this stream.
   Note that some types of stream don't actually use c, they just rewind
   pointers.
   Never call this unless you *have* *successfully* read from the stream
   previously. (few checks are performed here, I assume they were made in
   streamgetc()).  */
int
rep_stream_ungetc(repv stream, int c)
{
    int rc = rep_FALSE;
    if(rep_NILP(stream)
       && !(stream = Fsymbol_value(Qstandard_input, Qnil)))
	return(rc);
top:
    switch(rep_TYPE(stream))
    {
	repv tmp;
	rep_type *t;

    case rep_Cons:
	tmp = rep_CAR(stream);
	if(rep_INTP(tmp) && rep_STRINGP(rep_CDR(stream)))
	{
	    rep_CAR(stream) = rep_MAKE_INT(rep_INT(tmp) - 1);
	    rc = rep_TRUE;
	    break;
	}
	else if(tmp == Qlambda)
	    goto function;
	else
	{
	    t = rep_get_data_type(rep_TYPE(tmp));
	    if (t->ungetc != 0)
		(t->ungetc)(stream, c);
	    else
		Fsignal(Qinvalid_stream, rep_LIST_1(stream));
	}
	break;

    case rep_Funarg:
    function:
	tmp = rep_MAKE_INT(c);
	if((tmp = rep_call_lisp1(stream, tmp)) && !rep_NILP(tmp))
	    rc = rep_TRUE;
	break;

    default:
	if (rep_FILEP(stream))
	{
	    if (c == '\n')
		rep_FILE (stream)->line_number--;

	    if(rep_LOCAL_FILE_P(stream))
		c = ungetc(c, rep_FILE(stream)->file.fh);
	    else
	    {
		stream = rep_FILE(stream)->file.stream;
		goto top;
	    }
	    break;
	}
	t = rep_get_data_type(rep_TYPE(stream));
	if (t->ungetc != 0)
	    (t->ungetc)(stream, c);
    }
    return(rc);
}

int
rep_stream_putc(repv stream, int c)
{
    int rc = -1;

    if (stream == Qnil && !(stream = Fsymbol_value (Qstandard_output, Qnil)))
	goto bottom;

top:
    switch (rep_TYPE (stream))
    {
	repv args, res, new;
	int len;
	char tmps[2];
	rep_type *t;

    case rep_Cons:
	args = rep_CAR (stream);
	if (rep_STRINGP (args)
	    && rep_STRING_WRITABLE_P(args) && rep_INTP (rep_CDR (stream)))
	{
	    int actuallen = rep_INT (rep_CDR (stream));
	    len = rep_STRING_LEN (args);
	    if (len + 1 >= actuallen)
	    {
		int newlen = actuallen < 16 ? 32 : actuallen * 2;
		new = rep_make_string (newlen + 1);
		if (new == rep_NULL)
		    break;
		memcpy (rep_STR (new), rep_STR (args), len);
		rep_CAR (stream) = new;
		rep_CDR(stream) = rep_MAKE_INT (newlen);
		args = new;
	    }
	    ((unsigned char *)rep_STR (args))[len] = (unsigned char) c;
	    rep_STR (args)[len+1] = 0;
	    rep_set_string_len (args, len + 1);
	    rc = 1;
	    break;
	}
	else if (args == Qlambda)
	    goto function;
	else
	{
	    t = rep_get_data_type (rep_TYPE (rep_CAR (stream)));
	    if (t->putc != 0)
		rc = (t->putc) (stream, c);
	    else
		Fsignal (Qinvalid_stream, rep_LIST_1 (stream));
	}
	break;

    case rep_Symbol:
	if (stream == Qt)
	{
	    tmps[0] = (char) c;
	    tmps[1] = 0;
	    if (rep_message_fun != 0)
		(*rep_message_fun) (rep_append_message, tmps, 1);
	    rc = 1;
	}
	break;

    case rep_Funarg:
    function:
	res = rep_call_lisp1 (stream, rep_MAKE_INT (c));
	if(res != rep_NULL)
	    rc = 1;
	break;

    default:
	if (rep_FILEP (stream))
	{
	    if (rep_NILP (rep_FILE (stream)->name))
		return rep_unbound_file_error (stream);
	    else if (rep_LOCAL_FILE_P (stream))
	    {
		if (putc (c, rep_FILE(stream)->file.fh) != EOF)
		    rc = 1;
	    }
	    else
	    {
		stream = rep_FILE (stream)->file.stream;
		goto top;
	    }
	}
	else
	{
	    t = rep_get_data_type (rep_TYPE (stream));
	    if (t->putc != 0)
		rc = (t->putc) (stream, c);
	    else
		Fsignal (Qinvalid_stream, rep_LIST_1 (stream));
	}
    }

bottom:
    if (rc != 1)
    {
	if (!rep_FILEP (stream)
	    || (rep_FILE (stream)->car & rep_LFF_SILENT_ERRORS) == 0)
	{
	    Fsignal (Qend_of_stream, rep_LIST_1 (stream));
	}
	return 0;
    }
    else
	return 1;
}

int
rep_stream_puts(repv stream, void *data, int bufLen, rep_bool isValString)
{
    char *buf;
    int rc = -1;

    if(stream == Qnil && !(stream = Fsymbol_value (Qstandard_output, Qnil)))
	goto bottom;

    buf = isValString ? rep_STR (data) : data;
    if (bufLen == -1)
	bufLen = isValString ? rep_STRING_LEN (rep_VAL (data)) : strlen (buf);

top:
    switch (rep_TYPE (stream))
    {
	repv args, res, new;
	int len, newlen;
	rep_type *t;

    case rep_Cons:
	args = rep_CAR (stream);
	if (rep_STRINGP (args)
	    && rep_STRING_WRITABLE_P (args) && rep_INTP (rep_CDR (stream)))
	{
	    int actuallen = rep_INT (rep_CDR (stream));
	    len = rep_STRING_LEN (args);
	    newlen = len + bufLen + 1;
	    if (actuallen <= newlen)
	    {
		int tmp = actuallen < 16 ? 32 : actuallen * 2;
		if (tmp > newlen)
		    newlen = tmp;
		new = rep_make_string (newlen + 1);
		if (new == rep_NULL)
		    break;
		memcpy (rep_STR (new), rep_STR (args), len);
		rep_CAR (stream) = new;
		rep_CDR (stream) = rep_MAKE_INT (newlen);
		args = new;
	    }
	    memcpy (rep_STR (args) + len, buf, bufLen);
	    rep_STR (args)[len + bufLen] = 0;
	    rep_set_string_len (args, len + bufLen);
	    rc = bufLen;
	    break;
	}
	else if (args == Qlambda)
	    goto function;
	else
	{
	    t = rep_get_data_type (rep_TYPE (rep_CAR (stream)));
	    if (t->puts != 0)
		rc = (t->puts) (stream, data, bufLen, isValString);
	    else
		Fsignal (Qinvalid_stream, rep_LIST_1(stream));
	}
	break;

    case rep_Symbol:
	if (stream == Qt)
	{
	    if (rep_message_fun != 0)
		(*rep_message_fun) (rep_append_message, buf, bufLen);
	    rc = bufLen;
	}
	break;

    case rep_Funarg:
    function:
	if (isValString)
	    args = rep_VAL (data);
	else
	    args = rep_string_dupn (buf, bufLen);
	res = rep_call_lisp1(stream, args);
	if (res != rep_NULL)
	{
	    /* Output filters don't bother to return anything sane,
	       so lets just assume they always handle everything..

	       I should really spec these things fully.. */

	    rc = bufLen;
	}
	break;

    default:
	if (rep_FILEP(stream))
	{
	    if (rep_NILP (rep_FILE (stream)->name))
		return rep_unbound_file_error (stream);
	    else if (rep_LOCAL_FILE_P (stream))
		rc = fwrite (buf, 1, bufLen, rep_FILE (stream)->file.fh);
	    else
	    {
		stream = rep_FILE (stream)->file.stream;
		goto top;
	    }
	    break;
	}
	t = rep_get_data_type (rep_TYPE(stream));
	if (t->puts != 0)
	    rc = (t->puts) (stream, data, bufLen, isValString);
	else
	    Fsignal (Qinvalid_stream, rep_LIST_1 (stream));
    }

bottom:
    if (rc != bufLen)
    {
	if (!rep_FILEP (stream)
	    || (rep_FILE (stream)->car & rep_LFF_SILENT_ERRORS) == 0)
	{
	    Fsignal (Qend_of_stream, rep_LIST_1 (stream));
	}
	return 0;
    }
    else
	return bufLen;
}

/* Read an escape sequence from STREAM. C_P should contain the first
   character of the escape *not* the escape character. Supported sequences
   are,
     n   newline
     r   carriage return
     f   form feed
     t   horizontal tab
     v   vertical tab
     a   bell
     ^C  control code of C
     012 octal character code
     x12 hex character code
   Otherwise the character is returned as-is.  */
int
rep_stream_read_esc (repv stream, int *c_p)
{
    char c;
    switch (*c_p)
    {
    case 'n':
	c = '\n';
	break;
    case 'r':
	c = '\r';
	break;
    case 'f':
	c = '\f';
	break;
    case 't':
	c = '\t';
	break;
    case 'v':
	c = '\v';
	break;
    case 'a':
	c = '\a';
	break;
    case '^':
	c = toupper (rep_stream_getc (stream)) ^ 0x40;
	break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
	c = *c_p - '0';
	*c_p = rep_stream_getc (stream);
	if ((*c_p >= '0') && (*c_p <= '7'))
	{
	    c = (c * 8) + (*c_p - '0');
	    *c_p = rep_stream_getc (stream);
	    if ((*c_p >= '0') && (*c_p <= '7'))
	    {
		c = (c * 8) + (*c_p - '0');
		break;
	    }
	    else
		return c;
	}
	else
	    return c;
    case 'x':
	c = 0;
	while (1)
	{
	    *c_p = rep_stream_getc (stream);
	    if (!isxdigit (*c_p))
		return c;
	    if ((*c_p >= '0') && (*c_p <= '9'))
		c = (c * 16) + (*c_p - '0');
	    else
		c = (c * 16) + (toupper (*c_p) - 'A') + 10;
	}
    default:
	c = *c_p;
    }
    *c_p = rep_stream_getc (stream);
    return(c);
}

DEFUN("write", Fwrite, Swrite, (repv stream, repv data, repv len), rep_Subr3) /*
::doc:rep.io.streams#write::
write STREAM DATA [LENGTH]

Writes DATA, which can either be a string or a character, to the stream
STREAM, returning the number of characters actually written. If DATA is
a string LENGTH can define how many characters to write.
::end:: */
{
    int actual;
    switch (rep_TYPE (data))
    {
	rep_bool vstring;
	void *arg;

    case rep_Int:
	actual = rep_stream_putc (stream, rep_INT(data));
	break;

    case rep_String:
	if (rep_INTP (len))
	{
	    actual = rep_INT (len);
	    if (actual > rep_STRING_LEN (data))
		return rep_signal_arg_error(len, 3);
	    if (actual == rep_STRING_LEN (data))
	    {
		arg = rep_PTR (data);
		vstring = rep_TRUE;
	    }
	    else
	    {
		arg = rep_STR (data);
		vstring = rep_FALSE;
	    }
	}
	else
	{
	    actual = rep_STRING_LEN (data);
	    vstring = rep_TRUE;
	    arg = rep_PTR (data);
	}
	actual = rep_stream_puts (stream, arg, actual, vstring);
	break;

    default:
	return rep_signal_arg_error (data, 2);
    }

    return !rep_INTERRUPTP ? rep_MAKE_INT (actual) : rep_NULL;
}

DEFUN("read-char", Fread_char, Sread_char, (repv stream), rep_Subr1) /*
::doc:rep.io.streams#read-char::
read-char STREAM

Reads the next character from the input-stream STREAM, if no more characters
are available returns nil.
::end:: */
{
    int rc = rep_stream_getc (stream);
    if(rc != EOF)
	return rep_MAKE_INT (rc);
    else
	return Qnil;
}

DEFUN("peek-char", Fpeek_char, Speek_char, (repv stream), rep_Subr1) /*
::doc:rep.io.streams#peek-char::
peek-char STREAM

Returns the next character from the input-stream STREAM, *without*
removing that character from the head of the stream. If no more
characters are available returns nil.
::end:: */
{
    int c = rep_stream_getc (stream);
    if (c != EOF)
    {
	rep_stream_ungetc (stream, c);
	return rep_MAKE_INT (c);
    }
    else
	return Qnil;
}

DEFUN("read-chars", Fread_chars, Sread_chars,
      (repv stream, repv count), rep_Subr2) /*
::doc:rep.io.streams#read-chars::
read-chars STREAM COUNT

Read upto COUNT characters from the input stream STREAM, returning a
string containing the characters. If EOF is read before reading COUNT
characters, the returned string will contain the characters read up to
that point. If no characters are read, nil will be returned.
::end:: */
{
    char *buf;
    int len;
    rep_DECLARE2 (count, rep_INTP);
    buf = alloca (rep_INT (count));
    if (rep_FILEP (stream) && rep_LOCAL_FILE_P (stream))
    {
	/* Special case for local file streams. */
	len = fread (buf, sizeof (char), rep_INT (count),
		     rep_FILE (stream)->file.fh);

	/* XXX one possibility is to scan for newlines in the buffer.. */
	rep_FILE (stream)->car |= rep_LFF_BOGUS_LINE_NUMBER;
    }
    else
    {
	int c;
	len = 0;
	while (len < rep_INT(count) && (c = rep_stream_getc (stream)) != EOF)
	{
	    buf[len++] = c;
	}
    }
    if (len > 0)
	return rep_string_dupn (buf, len);
    else
	return Qnil;
}

DEFUN("read-line", Fread_line, Sread_line, (repv stream), rep_Subr1) /*
::doc:rep.io.streams#read-line::
read-line STREAM

Read one line of text from STREAM.
::end:: */
{
    char buf[400];
    if (rep_FILEP(stream) && rep_LOCAL_FILE_P (stream))
    {
	/* Special case for file streams. We can read a line in one go.	 */
	if (fgets (buf, sizeof (buf), rep_FILE (stream)->file.fh))
	    return rep_string_dup (buf);
	else
	    return Qnil;
    }
    else
    {
	char *bufp = buf;
	int len = 0, c;
	while ((c = rep_stream_getc (stream)) != EOF)
	{
	    *bufp++ = (char) c;
	    len++;
	    if ((len >= sizeof (buf) - 1) || (c == '\n'))
		break;
	}
	if (len == 0)
	    return Qnil;
	return rep_string_dupn (buf, len);
    }
}

DEFUN("copy-stream", Fcopy_stream, Scopy_stream, (repv source, repv dest), rep_Subr2) /*
::doc:rep.io.streams#copy-stream::
copy-stream SOURCE-STREAM DEST-STREAM

Copy all characters from SOURCE-STREAM to DEST-STREAM until an EOF is
read. Returns the number of characters copied.
::end:: */
{
    int len = 0, c;
    char buf[BUFSIZ+1];
    int i = 0;
    while ((c = rep_stream_getc (source)) != EOF)
    {
	if (i == BUFSIZ)
	{
	    buf[i] = 0;
	    rep_stream_puts (dest, buf, BUFSIZ, rep_FALSE);
	    rep_TEST_INT;
	    if (rep_INTERRUPTP)
		return rep_NULL;
	    i = 0;
	}
	buf[i++] = c;
	len++;
    }
    if (i != 0)
    {
	buf[i] = 0;
	rep_stream_puts (dest, buf, i, rep_FALSE);
    }
    return !rep_INTERRUPTP ? rep_MAKE_INT (len) : rep_NULL;
}

DEFUN("read", Fread, Sread, (repv stream), rep_Subr1) /*
::doc:rep.io.streams#read::
read [STREAM]

Reads one lisp-object from the input-stream STREAM (or the value of the
variable `standard-input' if STREAM is unspecified) and return it.
::end:: */
{
    repv res;
    int c;
    if(stream == Qnil && !(stream = Fsymbol_value (Qstandard_input, Qnil)))
    {
	rep_signal_arg_error (stream, 1);
	return rep_NULL;
    }
    c = rep_stream_getc (stream);
    if (c == EOF)
	res = Fsignal (Qend_of_stream, rep_LIST_1(stream));
    else
	res = rep_readl (stream, &c);
    /* If an error occurred leave stream where it is.  */
    if (res && c != EOF)
	rep_stream_ungetc (stream, c);
    return res;
}

DEFUN("print", Fprint, Sprint, (repv obj, repv stream), rep_Subr2) /*
::doc:rep.io.streams#print::
print OBJECT [STREAM]

First outputs a newline, then prints a text representation of OBJECT to
STREAM (or the contents of the variable `standard-output') in a form suitable
for `read'.
::end:: */
{
    if(stream == Qnil && !(stream = Fsymbol_value (Qstandard_output, Qnil)))
    {
	rep_signal_arg_error (stream, 1);
	return rep_NULL;
    }
    rep_stream_putc (stream, '\n');
    rep_print_val (stream, obj);
    return !rep_INTERRUPTP ? obj : rep_NULL;
}

DEFUN("prin1", Fprin1, Sprin1, (repv obj, repv stream), rep_Subr2) /*
::doc:rep.io.streams#prin1::
prin1 OBJECT [STREAM]

Prints a text representation of OBJECT to STREAM (or the contents of the
variable `standard-output') in a form suitable for `read'.
::end:: */
{
    if(stream == Qnil && !(stream = Fsymbol_value (Qstandard_output, Qnil)))
    {
	rep_signal_arg_error (stream, 1);
	return rep_NULL;
    }
    rep_print_val (stream, obj);
    return !rep_INTERRUPTP ? obj : rep_NULL;
}

DEFUN("princ", Fprinc, Sprinc, (repv obj, repv stream), rep_Subr2) /*
::doc:rep.io.streams#princ::
princ OBJECT [STREAM]

Prints a text representation of OBJECT to STREAM (or the contents of the
variable standard-output), no strange characters are quoted and no quotes
are printed around strings.
::end:: */
{
    if(stream == Qnil && !(stream = Fsymbol_value (Qstandard_output, Qnil)))
    {
	rep_signal_arg_error (stream, 1);
	return rep_NULL;
    }
    rep_princ_val (stream, obj);
    return !rep_INTERRUPTP ? obj : rep_NULL;
}

DEFUN("format", Fformat, Sformat, (repv args), rep_SubrN) /*
::doc:rep.io.streams#format::
format STREAM FORMAT-STRING ARGS...

Writes a string created from the format specification FORMAT-STRING and
the argument-values ARGS to the stream, STREAM. If STREAM is nil a string
is created and returned.

FORMAT-STRING is a template for the result, any `%' characters introduce
a substitution, using the next unused ARG. The substitutions have the
following syntax,

	%[FLAGS][FIELD-WIDTH][.PRECISION]CONVERSION

FIELD-WIDTH is a positive decimal integer, defining the size in
characters of the substitution output. PRECISION is only valid when
printing floating point numbers.

CONVERSION is a character defining how to convert the corresponding ARG
to text. The default options are:

	d	Output ARG as a decimal integer
	x, X	Output ARG as a hexadecimal integer
	o	Output ARG as an octal integer
	c	Output ARG as a character
	s	Output the result of `(princ ARG)'
	S	Output the result of `(prin1 ARG)'

FLAGS is a sequence of zero or more of the following characters,

	-	Left justify substitution within field
	^	Truncate substitution at size of field
	0	Pad the field with zeros instead of spaces
	+	For d, x, and o conversions, output a leading plus
		 sign if ARG is positive
	` '	(A space) For d, x, and o conversions, if the result
		 doesn't start with a plus or minus sign, output a
		 leading space

The list of CONVERSIONS can be extended through the format-hooks-alist
variable; the strings created by these extra conversions are formatted
as if by the `s' conversion. 

Note that the FIELD-WIDTH and all flags currently have no effect on the
`S' conversion, (or the `s' conversion when the ARG isn't a string).
::end:: */
{
    char *fmt, *last_fmt;
    rep_bool make_string;
    repv stream, format, extra_formats = rep_NULL;
    rep_GC_root gc_stream, gc_format, gc_args, gc_extra_formats;
    char c;
    int this_arg = 0;

    if (!rep_CONSP (args))
	return rep_signal_missing_arg (1);
    stream = rep_CAR (args);
    args = rep_CDR (args);
    if (stream == Qnil)
    {
	stream = Fcons (rep_string_dupn ("", 0), rep_MAKE_INT (0));
	make_string = rep_TRUE;
    }
    else
	make_string = rep_FALSE;

    if (!rep_CONSP (args))
	return rep_signal_missing_arg (2);
    format = rep_CAR (args);
    args = rep_CDR (args);
    rep_DECLARE2 (format, rep_STRINGP);
    fmt = rep_STR (format);

    rep_PUSHGC (gc_stream, stream);
    rep_PUSHGC (gc_format, format);
    rep_PUSHGC (gc_args, args);
    rep_PUSHGC (gc_extra_formats, extra_formats);

    last_fmt = fmt;
    while ((c = *fmt++) && !rep_INTERRUPTP)
    {
	if (c == '%')
	{
	    rep_bool left_justify = rep_FALSE, truncate_field = rep_FALSE;
	    rep_bool pad_zeros = rep_FALSE;
	    char leading_char = 0;
	    int field_width = 0, precision = 0;
	    char *tem;

	    if (last_fmt != fmt - 1)
	    {
		rep_stream_puts (stream, last_fmt,
				 fmt - last_fmt - 1, rep_FALSE);
		if (rep_INTERRUPTP)
		    goto exit;
	    }

	    /* Parse the `n$' prefix */
	    tem = fmt;
	    while (1)
	    {
		switch (*tem++)
		{
		    int arg;

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		    break;

		case '$':
		    arg = atoi (fmt);
		    if (arg > 0)
		    {
			this_arg = arg - 1;
			fmt = tem;
		    }
		    goto parse_flags;

		default:
		    goto parse_flags;
		}
	    }

	parse_flags:
	    /* Then scan for flags */
	    c = *fmt++;
	    while (1)
	    {
		switch (c)
		{
		case '-':
		    left_justify = rep_TRUE; break;

		case '^':
		    truncate_field = rep_TRUE; break;

		case '0':
		    pad_zeros = rep_TRUE; break;

		case '+': case ' ':
		    leading_char = c;
		    break;

		default:
		    goto parse_field_width;
		}
		c = *fmt++;
	    }

	    /* Now look for the field width */
	parse_field_width:
	    while(isdigit (c))
	    {
		field_width = field_width * 10 + (c - '0');
		c = *fmt++;
	    }

	    /* Now precision */
	    if (c == '.')
	    {
		c = *fmt++;
		while (c && isdigit (c))
		{
		    precision = precision * 10 + (c - '0');
		    c = *fmt++;
		}
	    }
	    else
		precision = -1;

	    /* Finally, the format specifier */
	    if(c == '%')
		rep_stream_putc (stream, '%');
	    else
	    {
		repv fun;
		repv val = Fnth (rep_MAKE_INT (this_arg), args);
		rep_bool free_str = rep_FALSE;

		if (val == rep_NULL)
		    goto exit;

		switch (c)
		{
		    int radix, len, actual_len;
		    char buf[256], *ptr;

		case 'c':
		    rep_stream_putc (stream, rep_INT (val));
		    break;

		case 'x': case 'X':
		    radix = 16;
		    goto do_number;

		case 'o':
		    radix = 8;
		    goto do_number;

		case 'd':
		    radix = 10;
		do_number:
		    ptr = rep_print_number_to_string (val, radix, precision);
		    if (ptr == 0)
			break;
		    free_str = rep_TRUE;
		    len = strlen (ptr);
		    goto string_out;

		case 's':
		unquoted:
		    if (!rep_STRINGP (val)
			|| (left_justify && field_width == 0))
		    {
			rep_princ_val (stream, val);
			break;
		    }
		    ptr = rep_STR (val);
		    len = rep_STRING_LEN (val);

		string_out:
		    actual_len = len;
		    if (leading_char)
		    {
			if (*ptr != '-')
			    actual_len++;
			else
			    leading_char = 0;
		    }
		    if (field_width == 0 || actual_len >= field_width)
		    {
			if (leading_char)
			    rep_stream_putc (stream, leading_char);
			rep_stream_puts (stream, ptr, truncate_field
					 ? (field_width - (leading_char != 0))
					 : len, rep_FALSE);
		    }
		    else
		    {
			int slen = MIN (field_width - actual_len, sizeof (buf));
			memset (buf, !pad_zeros ? ' ' : '0', slen);
			if (left_justify)
			{
			    if (leading_char)
				rep_stream_putc (stream, leading_char);
			    rep_stream_puts (stream, ptr, len, rep_FALSE);
			}
			rep_stream_puts (stream, buf, slen, rep_FALSE);
			if (!left_justify)
			{
			    if (leading_char)
				rep_stream_putc (stream, leading_char);
			    rep_stream_puts (stream, ptr, len, rep_FALSE);
			}
		    }
		    if (free_str)
			free (ptr);
		    break;

		case 'S':
		    rep_print_val (stream, val);
		    break;

		default:
		    if (extra_formats == rep_NULL)
		    {
			extra_formats
			    = Fsymbol_value (Qformat_hooks_alist, Qt);
		    }
		    if (rep_CONSP (extra_formats)
			&& (fun = Fassq (rep_MAKE_INT (c), extra_formats))
			&& rep_CONSP (fun))
		    {
			val = rep_call_lisp1 (rep_CDR (fun), val);
			if (val == rep_NULL)
			    goto exit;
			else
			{
			    if (val == Qnil)
				val = rep_null_string ();
			    goto unquoted;
			}
		    }
		    else
		    {
			DEFSTRING (err, "Unknown format conversion");
			Fsignal (Qerror, rep_list_2 (rep_VAL (&err),
						     rep_MAKE_INT (c)));
			goto exit;
		    }
		}
		this_arg++;
	    }
	    last_fmt = fmt;
	}
    }

    if (last_fmt != fmt - 1)
	rep_stream_puts (stream, last_fmt, fmt - last_fmt - 1, rep_FALSE);
    if (make_string)
    {
	if (rep_STRING_LEN (rep_CAR (stream)) != rep_INT (rep_CDR (stream)))
	{
	    /* Truncate the stream to it's actual length. */
	    stream = Fcopy_sequence (rep_CAR (stream));
	}
	else
	    stream = rep_CAR (stream);
    }

exit:
    rep_POPGC; rep_POPGC; rep_POPGC; rep_POPGC;

    return !rep_INTERRUPTP ? stream : rep_NULL;
}

DEFUN("make-string-input-stream", Fmake_string_input_stream, Smake_string_input_stream, (repv string, repv start), rep_Subr2) /*
::doc:rep.io.streams#make-string-input-stream::
make-string-input-stream STRING [START]

Returns a input stream, it will supply, in order, the characters in STRING,
starting from START (or the beginning of the string).
::end:: */
{
    rep_DECLARE1 (string, rep_STRINGP);
    return (Fcons (rep_INTP (start) ? start : rep_MAKE_INT (0), string));
}

DEFUN("make-string-output-stream", Fmake_string_output_stream, Smake_string_output_stream, (void), rep_Subr0) /*
::doc:rep.io.streams#make-string-output-stream::
make-string-output-stream

Returns an output stream which will accumulate the characters written to
it for the use of the `get-output-stream-string' function.
::end:: */
{
    return (Fcons (rep_string_dupn ("", 0), rep_MAKE_INT (0)));
}

DEFUN("get-output-stream-string", Fget_output_stream_string, Sget_output_stream_string, (repv strm), rep_Subr1) /*
::doc:rep.io.streams#get-output-stream-string::
get-output-stream-string STRING-OUTPUT-STREAM

Returns a string containing the characters written to the stream STRING-
OUTPUT-STREAM (created by `make-string-output-stream'). The stream is then
reset so that the next call to this function with this stream will only
return the new characters.
::end:: */
{
    repv string;
    if (!rep_CONSP (strm)
	|| !rep_STRINGP (rep_CAR(strm))
	|| !rep_INTP (rep_CDR(strm)))
    {
	return rep_signal_arg_error (strm, 1);
    }

    if (rep_STRING_LEN (rep_CAR (strm)) != rep_INT (rep_CDR (strm)))
    {
	/* Truncate the string to it's actual length. */
	string = Fcopy_sequence (rep_CAR (strm));
    }
    else
	string = rep_CAR (strm);

    /* Reset the stream. */
    rep_CAR (strm) = rep_string_dupn ("", 0);
    rep_CDR (strm) = rep_MAKE_INT (0);

    return string;
}

DEFUN("input-stream-p", Finput_stream_p,
      Sinput_stream_p, (repv arg), rep_Subr1) /*
::doc:rep.io.streams#input-stream-p::
input-stream-p ARG

Returns t if ARG is an input stream.
::end:: */
{
    repv res = Qnil;
    switch (rep_TYPE (arg))
    {
	repv car, cdr;
	rep_type *t;

    case rep_Funarg:
	res = Qt;
	break;

    case rep_Cons:
	car = rep_CAR (arg);
	cdr = rep_CDR (arg);
	if (rep_INTP (car) && rep_STRINGP (cdr))
	    res = Qt;
	else
	{
	    t = rep_get_data_type (rep_TYPE (car));
	    if (t->getc && t->ungetc)
		res = Qt;
	}
	break;

    default:
	if (rep_FILEP (arg))
	    res = Qt;			/* XXX broken */
	else
	{
	    t = rep_get_data_type (rep_TYPE (arg));
	    if (t->getc && t->ungetc)
		res = Qt;
	}
    }
    return res;
}

DEFUN("output-stream-p", Foutput_stream_p,
      Soutput_stream_p, (repv arg), rep_Subr1) /*
::doc:rep.io.streams#output-stream-p::
output-stream-p ARG

Returns t if ARG is an output stream.
::end:: */
{
    repv res = Qnil;
    switch (rep_TYPE (arg))
    {
	repv car, cdr;
	rep_type *t;

    case rep_Symbol:
	if (arg == Qt)
	    res = Qt;
	break;

    case rep_Funarg:
	res = Qt;
	break;

    case rep_Cons:
	car = rep_CAR (arg);
	cdr = rep_CDR (arg);
	if (rep_INTP (cdr) && rep_STRINGP (car))
	    res = Qt;
	else
	{
	    t = rep_get_data_type (rep_TYPE (car));
	    if (t->putc && t->puts)
		res = Qt;
	}
	break;

    default:
	if (rep_FILEP (arg))
	    res = Qt;			/* XXX broken */
	else
	{
	    t = rep_get_data_type (rep_TYPE (arg));
	    if (t->putc && t->puts)
		res = Qt;
	}
    }
    return res;
}

void
rep_streams_init (void)
{
    repv tem = rep_push_structure ("rep.io.streams");
    rep_INTERN_SPECIAL(format_hooks_alist);
    rep_ADD_SUBR(Swrite);
    rep_ADD_SUBR(Sread_char);
    rep_ADD_SUBR(Speek_char);
    rep_ADD_SUBR(Sread_chars);
    rep_ADD_SUBR(Sread_line);
    rep_ADD_SUBR(Scopy_stream);
    rep_ADD_SUBR(Sread);
    rep_ADD_SUBR(Sprint);
    rep_ADD_SUBR(Sprin1);
    rep_ADD_SUBR(Sprinc);
    rep_ADD_SUBR(Sformat);
    rep_ADD_SUBR(Smake_string_input_stream);
    rep_ADD_SUBR(Smake_string_output_stream);
    rep_ADD_SUBR(Sget_output_stream_string);
    rep_ADD_SUBR(Sinput_stream_p);
    rep_ADD_SUBR(Soutput_stream_p);
    rep_pop_structure (tem);
}
