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

#include "jade.h"
#include <lib/jade_protos.h>
#include "regexp.h"

#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <stdlib.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

_PR VALUE sym_format_hooks_alist;
DEFSYM(format_hooks_alist, "format-hooks-alist"); /*
::doc:format_hooks_alist::
Alist of (CHAR . FUNCTION) defining extra format conversions for the
format function. FUNCTION is called as (FUNCTION VALUE), and should
return the string to be inserted.
::end:: */

_PR int stream_getc(VALUE);
_PR int stream_ungetc(VALUE, int);
_PR int stream_putc(VALUE, int);
_PR int stream_puts(VALUE, void *, int, bool);
_PR int stream_read_esc(VALUE, int *);

_PR void streams_init(void);

static int
pos_getc(TX *tx, VALUE *pos)
{
    int c = EOF;
    long row = VROW(*pos);
    long col = VCOL(*pos);
    if(row < tx->tx_LogicalEnd)
    {
	LINE *ln = tx->tx_Lines + row;
	if(col >= (ln->ln_Strlen - 1))
	{
	    if(++row == tx->tx_LogicalEnd)
		--row;
	    else
	    {
		col = 0;
		c = '\n';
	    }
	}
	else
	    c = ln->ln_Line[col++];
    }
    *pos = make_pos(col, row);
    return c;
}

static int
pos_putc(TX *tx, VALUE *pos, int c)
{
    int rc = EOF;
    if(pad_pos(tx, *pos))
    {
	u_char tmps[2];
	VALUE end;
	tmps[0] = (u_char)c;
	tmps[1] = 0;
	end = insert_string(tx, tmps, 1, *pos);
	if(end != LISP_NULL)
	{
	    *pos = end;
	    rc = 1;
	}
    }
    return rc;
}

static int
pos_puts(TX *tx, VALUE *pos, u_char *buf, int bufLen)
{
    if(pad_pos(tx, *pos))
    {
	VALUE end = insert_string(tx, buf, bufLen, *pos);
	if(end != LISP_NULL)
	{
	    *pos = end;
	    return bufLen;
	}
    }
    return EOF;
}

DEFSTRING(non_resident, "Marks used as streams must be resident");
DEFSTRING(proc_not_input, "Processes are not input streams");

int
stream_getc(VALUE stream)
{
    int c = EOF;
    if(NILP(stream)
       && !(stream = cmd_symbol_value(sym_standard_input, sym_nil)))
	return(c);
top:
    switch(VTYPE(stream))
    {
	VALUE res;
	int oldgci;

    case V_File:
	if(NILP(VFILE(stream)->name))
	    return unbound_file_error(stream);
	else if(LOCAL_FILE_P(stream))
	    c = getc(VFILE(stream)->file.fh);
	else
	{
	    stream = VFILE(stream)->file.stream;
	    goto top;
	}
	break;

    case V_Mark:
	if(!MARK_RESIDENT_P(VMARK(stream)))
	    cmd_signal(sym_invalid_stream, list_2(stream, VAL(&non_resident)));
	else
	    c = pos_getc(VTX(VMARK(stream)->file), &VMARK(stream)->pos);
	break;

    case V_Buffer:
	c = pos_getc(VTX(stream), get_tx_cursor_ptr(VTX(stream)));
	break;

    case V_Cons:
	res = VCAR(stream);
	if(INTP(res) && STRINGP(VCDR(stream)))
	{
	    c = (int)VSTR(VCDR(stream))[VINT(res)];
	    if(c)
		VCAR(stream) = MAKE_INT(VINT(res) + 1);
	    else
		c = EOF;
	    break;
	}
	else if(BUFFERP(res) && POSP(VCDR(stream)))
	{
	    c = pos_getc(VTX(res), &VCDR(stream));
	    break;
	}
	else if(res != sym_lambda)
	{
	    cmd_signal(sym_invalid_stream, LIST_1(stream));
	    break;
	}
	/* FALL THROUGH */

    case V_Symbol:
	oldgci = gc_inhibit;
	gc_inhibit = TRUE;
	if((res = call_lisp0(stream)) && INTP(res))
	    c = VINT(res);
	gc_inhibit = oldgci;
	break;

#ifdef HAVE_SUBPROCESSES
    case V_Process:
    {
	cmd_signal(sym_invalid_stream, list_2(stream, VAL(&proc_not_input)));
	break;
    }
#endif

    default:
	cmd_signal(sym_invalid_stream, LIST_1(stream));
    }
    return(c);
}

/* Puts back one character, it will be read next call to streamgetc on
   this stream.
   Note that some types of stream don't actually use c, they just rewind
   pointers.
   Never call this unless you *have* *successfully* read from the stream
   previously. (few checks are performed here, I assume they were made in
   streamgetc()).  */

#define POS_UNGETC(p, tx)				\
    do {						\
	long row = VROW(p), col = VCOL(p);		\
	if(--col < 0)					\
	{						\
	    row--;					\
	    col = (tx)->tx_Lines[row].ln_Strlen - 1;	\
	}						\
	(p) = make_pos(col, row);			\
    } while(0)

int
stream_ungetc(VALUE stream, int c)
{
    int rc = FALSE;
    if(NILP(stream)
       && !(stream = cmd_symbol_value(sym_standard_input, sym_nil)))
	return(rc);
top:
    switch(VTYPE(stream))
    {
	VALUE *ptr;
	VALUE tmp;
	int oldgci;

    case V_File:
	if(LOCAL_FILE_P(stream))
	    c = ungetc(c, VFILE(stream)->file.fh);
	else
	{
	    stream = VFILE(stream)->file.stream;
	    goto top;
	}
	break;

    case V_Mark:
	POS_UNGETC(VMARK(stream)->pos, VTX(VMARK(stream)->file));
	rc = TRUE;
	break;

    case V_Buffer:
	ptr = get_tx_cursor_ptr(VTX(stream));
	POS_UNGETC(*ptr, VTX(stream));
	rc = TRUE;
	break;

    case V_Cons:
	tmp = VCAR(stream);
	if(INTP(tmp) && STRINGP(VCDR(stream)))
	{
	    VCAR(stream) = MAKE_INT(VINT(tmp) - 1);
	    rc = TRUE;
	    break;
	}
	else if(BUFFERP(tmp) && POSP(VCDR(stream)))
	{
	    ptr = &VCDR(stream);
	    POS_UNGETC(*ptr, VTX(tmp));
	    rc = TRUE;
	    break;
	}
	/* FALL THROUGH */

    case V_Symbol:
	tmp = MAKE_INT(c);
	oldgci = gc_inhibit;
	gc_inhibit = TRUE;
	if((tmp = call_lisp1(stream, tmp)) && !NILP(tmp))
	    rc = TRUE;
	gc_inhibit = oldgci;
	break;
    }
    return(rc);
}

int
stream_putc(VALUE stream, int c)
{
    int rc = 0;
    if(NILP(stream)
       && !(stream = cmd_symbol_value(sym_standard_output, sym_nil)))
	return(rc);
top:
    switch(VTYPE(stream))
    {
	VALUE args, res, new;
	int len;
	u_char tmps[2];

    case V_File:
	if(NILP(VFILE(stream)->name))
	    return unbound_file_error(stream);
	else if(LOCAL_FILE_P(stream))
	{
	    if(putc(c, VFILE(stream)->file.fh) != EOF)
		rc = 1;
	}
	else
	{
	    stream = VFILE(stream)->file.stream;
	    goto top;
	}
	break;

    case V_Mark:
	if(!MARK_RESIDENT_P(VMARK(stream)))
	    cmd_signal(sym_invalid_stream,
		       list_2(stream, VAL(&non_resident)));
	else
	    rc = pos_putc(VTX(VMARK(stream)->file), &VMARK(stream)->pos, c);
	break;

    case V_Buffer:
	rc = pos_putc(VTX(stream), get_tx_cursor_ptr(VTX(stream)), c);
	break;

    case V_Cons:
	args = VCAR(stream);
	if(STRINGP(args) && STRING_WRITABLE_P(args) && INTP(VCDR(stream)))
	{
	    int actuallen = VINT(VCDR(stream));
	    len = STRING_LEN(args);
	    if(len + 1 >= actuallen)
	    {
		int newlen = actuallen < 16 ? 32 : actuallen * 2;
		new = make_string(newlen + 1);
		if(!new)
		    break;
		memcpy(VSTR(new), VSTR(args), len);
		VCAR(stream) = new;
		VCDR(stream) = MAKE_INT(newlen);
		args = new;
	    }
	    VSTR(args)[len] = (u_char)c;
	    VSTR(args)[len+1] = 0;
	    set_string_len(args, len + 1);
	    rc = 1;
	    break;
	}
	else if(BUFFERP(args))
	{
	    if(POSP(VCDR(stream)))
		rc = pos_putc(VTX(args), &VCDR(stream), c);
	    else
	    {
		VALUE pos = cmd_restriction_end(args);
		rc = pos_putc(VTX(args), &pos, c);
	    }
	    break;
	}
	else if(args != sym_lambda)
	{
	    cmd_signal(sym_invalid_stream, LIST_1(stream));
	    break;
	}
	/* FALL THROUGH */

    case V_Symbol:
	if(stream == sym_t)
	{
	    if(curr_win->w_Flags & WINFF_MESSAGE)
	    {
		WIN *w = curr_win;
		u_char *s;
		s = str_dupn(w->w_Message, w->w_MessageLen + 1);
		if(s)
		{
		    s[w->w_MessageLen++] = c;
		    s[w->w_MessageLen] = 0;
		    str_free(w->w_Message);
		    w->w_Message = s;
		    w->w_Flags |= WINFF_MESSAGE;
		}
	    }
	    else
	    {
		tmps[0] = (u_char)c;
		tmps[1] = 0;
		messagen(tmps, 1);
	    }
	    rc = 1;
	}
	else
	{
	    int oldgci = gc_inhibit;
	    gc_inhibit = TRUE;
	    if((res = call_lisp1(stream, MAKE_INT(c))) && !NILP(res))
		rc = 1;
	    gc_inhibit = oldgci;
	}
	break;

#ifdef HAVE_SUBPROCESSES
    case V_Process:
	tmps[0] = (u_char)c;
	tmps[1] = 0;
	rc = write_to_process(stream, tmps, 1);
	break;
#endif

    default:
	cmd_signal(sym_invalid_stream, LIST_1(stream));
    }
    return(rc);
}

int
stream_puts(VALUE stream, void *data, int bufLen, bool isValString)
{
    u_char *buf;
    int rc = 0;
    if(NILP(stream)
       && !(stream = cmd_symbol_value(sym_standard_output, sym_nil)))
	return(rc);

    buf = isValString ? VSTR(data) : data;
    if(bufLen == -1)
	bufLen = isValString ? STRING_LEN(VAL(data)) : strlen(buf);
top:
    switch(VTYPE(stream))
    {
	VALUE args, res, new;
	int len, newlen;

    case V_File:
	if(NILP(VFILE(stream)->name))
	    return unbound_file_error(stream);
	else if(LOCAL_FILE_P(stream))
	    rc = fwrite(buf, 1, bufLen, VFILE(stream)->file.fh);
	else
	{
	    stream = VFILE(stream)->file.stream;
	    goto top;
	}
	break;

    case V_Mark:
	if(!MARK_RESIDENT_P(VMARK(stream)))
	    cmd_signal(sym_invalid_stream, list_2(stream, VAL(&non_resident)));
	else
	    rc = pos_puts(VTX(VMARK(stream)->file), &VMARK(stream)->pos,
			  buf, bufLen);
	break;

    case V_Buffer:
	rc = pos_puts(VTX(stream), get_tx_cursor_ptr(VTX(stream)), buf, bufLen);
	break;

    case V_Cons:
	args = VCAR(stream);
	if(STRINGP(args) && STRING_WRITABLE_P(args) && INTP(VCDR(stream)))
	{
	    int actuallen = VINT(VCDR(stream));
	    len = STRING_LEN(args);
	    newlen = len + bufLen + 1;
	    if(actuallen <= newlen)
	    {
		register int tmp = actuallen < 16 ? 32 : actuallen * 2;
		if(tmp > newlen)
		    newlen = tmp;
		new = make_string(newlen + 1);
		if(!new)
		    break;
		memcpy(VSTR(new), VSTR(args), len);
		VCAR(stream) = new;
		VCDR(stream) = MAKE_INT(newlen);
		args = new;
	    }
	    memcpy(VSTR(args) + len, buf, bufLen);
	    VSTR(args)[len + bufLen] = 0;
	    set_string_len(args, len + bufLen);
	    rc = bufLen;
	    break;
	}
	else if(BUFFERP(args))
	{
	    if(POSP(VCDR(stream)))
		rc = pos_puts(VTX(args), &VCDR(stream), buf, bufLen);
	    else
	    {
		VALUE pos = cmd_restriction_end(args);
		rc = pos_puts(VTX(args), &pos, buf, bufLen);
	    }
	    break;
	}
	else if(args != sym_lambda)
	{
	    cmd_signal(sym_invalid_stream, LIST_1(stream));
	    break;
	}
	/* FALL THROUGH */

    case V_Symbol:
	if(stream == sym_t)
	{
	    if(curr_win->w_Flags & WINFF_MESSAGE)
	    {
		WIN *w = curr_win;
		u_char *s;
		newlen = w->w_MessageLen + bufLen;
		s = str_dupn(w->w_Message, newlen);
		if(s)
		{
		    memcpy(s + w->w_MessageLen, buf, bufLen);
		    s[newlen] = 0;
		    str_free(w->w_Message);
		    w->w_Message = s;
		    w->w_MessageLen = newlen;
		    w->w_Flags |= WINFF_MESSAGE;
		}
	    }
	    else
		messagen(buf, bufLen);
	    rc = bufLen;
	}
	else
	{
	    int oldgci = gc_inhibit;
	    if(isValString)
		args = VAL(data);
	    else
		args = string_dupn(buf, bufLen);
	    gc_inhibit = TRUE;
	    if((res = call_lisp1(stream, args)) && !NILP(res))
	    {
		if(INTP(res))
		    rc = VINT(res);
		else
		    rc = bufLen;
	    }
	    gc_inhibit = oldgci;
	}
	break;

#ifdef HAVE_SUBPROCESSES
    case V_Process:
	rc = write_to_process(stream, buf, bufLen);
	break;
#endif

    default:
	cmd_signal(sym_invalid_stream, LIST_1(stream));
    }
    return(rc);
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
stream_read_esc(VALUE stream, int *c_p)
{
    u_char c;
    switch(*c_p)
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
	c = toupper(stream_getc(stream)) ^ 0x40;
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
	*c_p = stream_getc(stream);
	if((*c_p >= '0') && (*c_p <= '7'))
	{
	    c = (c * 8) + (*c_p - '0');
	    *c_p = stream_getc(stream);
	    if((*c_p >= '0') && (*c_p <= '7'))
	    {
		c = (c * 8) + (*c_p - '0');
		break;
	    }
	    else
		return(c);
	}
	else
	    return(c);
    case 'x':
	c = 0;
	while(1)
	{
	    *c_p = stream_getc(stream);
	    if(!isxdigit(*c_p))
		return(c);
	    if((*c_p >= '0') && (*c_p <= '9'))
		c = (c * 16) + (*c_p - '0');
	    else
		c = (c * 16) + (toupper(*c_p) - 'A') + 10;
	}
    default:
	c = *c_p;
    }
    *c_p = stream_getc(stream);
    return(c);
}

_PR VALUE cmd_write(VALUE stream, VALUE data, VALUE len);
DEFUN("write", cmd_write, subr_write, (VALUE stream, VALUE data, VALUE len), V_Subr3, DOC_write) /*
::doc:write::
write STREAM DATA [LENGTH]

Writes DATA, which can either be a string or a character, to the stream
STREAM, returning the number of characters actually written. If DATA is
a string LENGTH can define how many characters to write.
::end:: */
{
    int actual;
    switch(VTYPE(data))
    {
	bool vstring;
	void *arg;
    case V_Int:
	actual = stream_putc(stream, VINT(data));
	break;
    case V_String:
	if(INTP(len))
	{
	    actual = VINT(len);
	    if(actual > STRING_LEN(data))
		return(signal_arg_error(len, 3));
	    if(actual == STRING_LEN(data))
	    {
		arg = VPTR(data);
		vstring = TRUE;
	    }
	    else
	    {
		arg = VSTR(data);
		vstring = FALSE;
	    }
	}
	else
	{
	    actual = STRING_LEN(data);
	    vstring = TRUE;
	    arg = VPTR(data);
	}
	actual = stream_puts(stream, arg, actual, vstring);
	break;
    default:
	return(signal_arg_error(data, 2));
    }
    return(MAKE_INT(actual));
}

_PR VALUE cmd_read_char(VALUE stream);
DEFUN("read-char", cmd_read_char, subr_read_char, (VALUE stream), V_Subr1, DOC_read_char) /*
::doc:read_char::
read-char STREAM

Reads the next character from the input-stream STREAM, if no more characters
are available returns nil.
::end:: */
{
    int rc;
    if((rc = stream_getc(stream)) != EOF)
	return(MAKE_INT(rc));
    return(sym_nil);
}

_PR VALUE cmd_read_line(VALUE stream);
DEFUN("read-line", cmd_read_line, subr_read_line, (VALUE stream), V_Subr1, DOC_read_line) /*
::doc:read_line::
read-line STREAM

Read one line of text from STREAM.
::end:: */
{
    u_char buf[400];
    if(FILEP(stream) && LOCAL_FILE_P(stream))
    {
	/* Special case for file streams. We can read a line in one go.	 */
	if(fgets(buf, 400, VFILE(stream)->file.fh))
	    return string_dup(buf);
	else
	    return sym_nil;
    }
    else
    {
	u_char *bufp = buf;
	int len = 0, c;
	while((c = stream_getc(stream)) != EOF)
	{
	    *bufp++ = (u_char)c;
	    if((++len >= 399) || (c == '\n'))
		break;
	}
	if(len == 0)
	    return(sym_nil);
	return(string_dupn(buf, len));
    }
}

_PR VALUE cmd_copy_stream(VALUE source, VALUE dest);
DEFUN("copy-stream", cmd_copy_stream, subr_copy_stream, (VALUE source, VALUE dest), V_Subr2, DOC_copy_stream) /*
::doc:copy_stream::
copy-stream SOURCE-STREAM DEST-STREAM

Copy all characters from SOURCE-STREAM to DEST-STREAM until an EOF is read.
::end:: */
{
#define COPY_BUFSIZ 512
    int len = 0, c;
    u_char buff[COPY_BUFSIZ];
    u_char *ptr = buff;
    while((c = stream_getc(source)) != EOF)
    {
	if(ptr - buff >= COPY_BUFSIZ - 1)
	{
	    *ptr = 0;
	    if(stream_puts(dest, buff, ptr - buff, FALSE) == EOF)
		break;
	    ptr = buff;
	}
	else
	    *ptr++ = c;
	len++;
	TEST_INT;
	if(INT_P)
	    return LISP_NULL;
    }
    if(ptr != buff)
    {
	*ptr = 0;
	stream_puts(dest, buff, ptr - buff, FALSE);
    }
    if(len)
	return(MAKE_INT(len));
    return(sym_nil);
}

_PR VALUE cmd_read(VALUE);
DEFUN("read", cmd_read, subr_read, (VALUE stream), V_Subr1, DOC_read) /*
::doc:read::
read [STREAM]

Reads one lisp-object from the input-stream STREAM (or the value of the
variable `standard-input' if STREAM is unspecified) and return it.
::end:: */
{
    VALUE res;
    int c;
    if(NILP(stream)
       && !(stream = cmd_symbol_value(sym_standard_input, sym_nil)))
    {
	signal_arg_error(stream, 1);
	return LISP_NULL;
    }
    c = stream_getc(stream);
    if(c == EOF)
	res = cmd_signal(sym_end_of_stream, LIST_1(stream));
    else
	res = readl(stream, &c);
    /* If an error occurred leave stream where it is.  */
    if(res && c != EOF)
	stream_ungetc(stream, c);
    return(res);
}

_PR VALUE cmd_print(VALUE, VALUE);
DEFUN("print", cmd_print, subr_print, (VALUE obj, VALUE stream), V_Subr2, DOC_print) /*
::doc:print::
print OBJECT [STREAM]

First outputs a newline, then prints a text representation of OBJECT to
STREAM (or the contents of the variable `standard-output') in a form suitable
for `read'.
::end:: */
{
    if(NILP(stream)
       && !(stream = cmd_symbol_value(sym_standard_output, sym_nil)))
    {
	signal_arg_error(stream, 1);
	return LISP_NULL;
    }
    stream_putc(stream, '\n');
    print_val(stream, obj);
    return(obj);
}

_PR VALUE cmd_prin1(VALUE, VALUE);
DEFUN("prin1", cmd_prin1, subr_prin1, (VALUE obj, VALUE stream), V_Subr2, DOC_prin1) /*
::doc:prin1::
prin1 OBJECT [STREAM]

Prints a text representation of OBJECT to STREAM (or the contents of the
variable `standard-output') in a form suitable for `read'.
::end:: */
{
    if(NILP(stream)
       && !(stream = cmd_symbol_value(sym_standard_output, sym_nil)))
    {
	signal_arg_error(stream, 1);
	return LISP_NULL;
    }
    print_val(stream, obj);
    return(obj);
}

_PR VALUE cmd_princ(VALUE, VALUE);
DEFUN("princ", cmd_princ, subr_princ, (VALUE obj, VALUE stream), V_Subr2, DOC_princ) /*
::doc:princ::
princ OBJECT [STREAM]

Prints a text representation of OBJECT to STREAM (or the contents of the
variable standard-output), no strange characters are quoted and no quotes
are printed around strings.
::end:: */
{
    if(NILP(stream)
       && !(stream = cmd_symbol_value(sym_standard_output, sym_nil)))
    {
	signal_arg_error(stream, 1);
	return LISP_NULL;
    }
    princ_val(stream, obj);
    return(obj);
}

_PR VALUE cmd_format(VALUE);
DEFUN("format", cmd_format, subr_format, (VALUE args), V_SubrN, DOC_format) /*
::doc:format::
format STREAM FORMAT-STRING ARGS...

Writes a string created from the format specification FORMAT-STRING and
the argument-values ARGS to the stream, STREAM. If STREAM is nil a string
is created and returned.

FORMAT-STRING is a template for the result, any `%' characters introduce
a substitution, using the next unused ARG. The substitutions have the
following syntax,

	%[FLAGS][FIELD-WIDTH]CONVERSION

FIELD-WIDTH is a positive decimal integer, defining the size in characters
of the substitution output.

CONVERSION is a character defining how to convert the corresponding ARG
to text. The default options are:

	d	Output ARG as a decimal integer
	x	Output ARG as a hexadecimal integer
	o	Output ARG as an octal integer
	c	Output ARG as a character
	s	Output the result of `(prin1 ARG)'
	S	Output the result of `(princ ARG)'

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
    u_char *fmt, *last_fmt;
    bool mk_str;
    VALUE stream, format, extra_formats = LISP_NULL;
    GC_root gc_stream, gc_format, gc_extra_formats;
    u_char c;

    if(!CONSP(args))
	return signal_missing_arg(1);
    stream = VCAR(args);
    args = VCDR(args);
    if(NILP(stream))
    {
	stream = cmd_cons(string_dupn("", 0), MAKE_INT(0));
	mk_str = TRUE;
    }
    else
	mk_str = FALSE;

    if(!CONSP(args))
	return signal_missing_arg(2);
    format = VCAR(args);
    args = VCDR(args);
    DECLARE2(format, STRINGP);
    fmt = VSTR(format);

    PUSHGC(gc_stream, stream);
    PUSHGC(gc_format, format);
    PUSHGC(gc_extra_formats, extra_formats);

    last_fmt = fmt;
    while((c = *fmt++))
    {
	if(c == '%')
	{
	    bool left_justify = FALSE, truncate_field = TRUE;
	    bool pad_zeros = FALSE;
	    char *flags_start, *flags_end;
	    char *width_start, *width_end;
	    int field_width = 0;

	    if(last_fmt != fmt)
		stream_puts(stream, last_fmt, fmt - last_fmt - 1, FALSE);

	    /* First scan for flags */
	    flags_start = fmt;
	    c = *fmt++;
	    while(1)
	    {
		switch(c)
		{
		case '-':
		    left_justify = TRUE; break;

		case '^':
		    truncate_field = TRUE; break;

		case '0':
		    pad_zeros = TRUE; break;

		case '+': case ' ':
		    break;

		default:
		    flags_end = fmt - 1;
		    goto parse_field_width;
		}
		c = *fmt++;
	    }

	    /* Now look for the field width */
	parse_field_width:
	    width_start = fmt-1;
	    while(isdigit(c))
	    {
		field_width = field_width * 10 + (c - '0');
		c = *fmt++;
	    }
	    width_end = fmt-1;

	    /* Finally, the format specifier */
	    if(c == '%')
		stream_putc(stream, '%');
	    else
	    {
		VALUE val, fun;

		if(!CONSP(args))
		{
		    stream = signal_missing_arg(0);
		    goto exit;
		}
		val = VCAR(args);
		args = VCDR(args);

		switch(c)
		{
		    u_char buf[256], fmt[32], *ptr;

		case 'd': case 'x': case 'o': case 'c':
		    fmt[0] = '%';
		    strncpy(fmt+1, flags_start, flags_end - flags_start);
		    ptr = fmt+1 + (flags_end - flags_start);
		    strncpy(ptr, width_start, width_end - width_start);
		    ptr += width_end - width_start;
		    *ptr++ = c;
		    *ptr = 0;
#ifdef HAVE_SNPRINTF
		    snprintf(buf, sizeof(buf),
#else
		    sprintf(buf,
#endif
			    fmt, INTP(val) ? VINT(val) : (long)val);
		    stream_puts(stream, buf, -1, FALSE);
		    break;

		case 's':
		unquoted:
		    if(STRINGP(val) && (left_justify || field_width > 0))
		    {
			/* Only handle field width, justification, etc
			   for strings */
			size_t len = STRING_LEN(val);
			if(len >= field_width)
			    stream_puts(stream, VPTR(val),
					truncate_field ? field_width : len,
					TRUE);
			else
			{
			    len = MIN(field_width - len, sizeof(buf));
			    memset(buf, !pad_zeros ? ' ' : '0', len);
			    if(left_justify)
				stream_puts(stream, VPTR(val), -1, TRUE);
			    stream_puts(stream, buf, len, FALSE);
			    if(!left_justify)
				stream_puts(stream, VPTR(val), -1, TRUE);
			}
		    }
		    else
			princ_val(stream, val);
		    break;

		case 'S':
		    print_val(stream, val);
		    break;

		case 0:
		    last_fmt = fmt;
		    goto end_of_input;

		default:
		    if(extra_formats == LISP_NULL)
		    {
			extra_formats
			    = cmd_symbol_value(sym_format_hooks_alist, sym_t);
		    }
		    if(CONSP(extra_formats)
		       && (fun = cmd_assq(MAKE_INT(c), extra_formats))
		       && CONSP(fun))
		    {
			val = call_lisp1(VCDR(fun), val);
			if(val == LISP_NULL)
			{
			    stream = LISP_NULL;
			    goto exit;
			}
			else
			{
			    if(NILP(val))
				val = null_string();
			    goto unquoted;
			}
		    }
		}
	    }
	    last_fmt = fmt;
	}
    }

end_of_input:
    if(last_fmt != fmt - 1)
	stream_puts(stream, last_fmt, fmt - last_fmt - 1, FALSE);
    if(mk_str)
    {
	if(STRING_LEN(VCAR(stream)) != VINT(VCDR(stream)))
	{
	    /* Truncate the stream to it's actual length. */
	    stream = cmd_copy_sequence(VCAR(stream));
	}
	else
	    stream = VCAR(stream);
    }

exit:
    POPGC; POPGC; POPGC;
    return(stream);
}

_PR VALUE cmd_make_string_input_stream(VALUE string, VALUE start);
DEFUN("make-string-input-stream", cmd_make_string_input_stream, subr_make_string_input_stream, (VALUE string, VALUE start), V_Subr2, DOC_make_string_input_stream) /*
::doc:make_string_input_stream::
make-string-input-stream STRING [START]

Returns a input stream, it will supply, in order, the characters in STRING,
starting from START (or the beginning of the string).
::end:: */
{
    DECLARE1(string, STRINGP);
    return(cmd_cons(INTP(start) ? start : MAKE_INT(0), string));
}

_PR VALUE cmd_make_string_output_stream(void);
DEFUN("make-string-output-stream", cmd_make_string_output_stream, subr_make_string_output_stream, (void), V_Subr0, DOC_make_string_output_stream) /*
::doc:make_string_output_stream::
make-string-output-stream

Returns an output stream which will accumulate the characters written to
it for the use of the `get-output-stream-string' function.
::end:: */
{
    return(cmd_cons(string_dupn("", 0), MAKE_INT(0)));
}

_PR VALUE cmd_get_output_stream_string(VALUE strm);
DEFUN("get-output-stream-string", cmd_get_output_stream_string, subr_get_output_stream_string, (VALUE strm), V_Subr1, DOC_get_output_stream_string) /*
::doc:get_output_stream_string::
get-output-stream-string STRING-OUTPUT-STREAM

Returns a string containing the characters written to the stream STRING-
OUTPUT-STREAM (created by `make-string-output-stream'). The stream is then
reset so that the next call to this function with this stream will only
return the new characters.
::end:: */
{
    VALUE string;
    if(!CONSP(strm) || !STRINGP(VCAR(strm)) || !INTP(VCDR(strm)))
	return(signal_arg_error(strm, 1));
    if(STRING_LEN(VCAR(strm)) != VINT(VCDR(strm)))
    {
	/* Truncate the string to it's actual length. */
	string = cmd_copy_sequence(VCAR(strm));
    }
    else
	string = VCAR(strm);
    /* Reset the stream. */
    VCAR(strm) = string_dupn("", 0);
    VCDR(strm) = MAKE_INT(0);
    return(string);
}

_PR VALUE cmd_streamp(VALUE arg);
DEFUN("streamp", cmd_streamp, subr_streamp, (VALUE arg), V_Subr1, DOC_streamp) /*
::doc:streamp::
streamp ARG

Returns t if ARG is a stream.
::end:: */
{
    VALUE res = sym_nil;
    switch(VTYPE(arg))
    {
	VALUE car, cdr;
    case V_File:
    case V_Buffer:
    case V_Mark:
    case V_Process:
    case V_Symbol:
	res = sym_t;
	break;
    case V_Cons:
	car = VCAR(arg);
	cdr = VCDR(arg);
	if((car == sym_lambda)
	   || (BUFFERP(car) && (POSP(cdr) || (cdr == sym_t)))
	   || (INTP(car) && STRINGP(cdr))
	   || (STRINGP(car) && INTP(cdr)))
	    res = sym_t;
	break;
    }
    return(res);
}

_PR VALUE cmd_write_area_to_stream(VALUE stream, VALUE start, VALUE end);
DEFUN("write-area-to-stream", cmd_write_area_to_stream,
      subr_write_area_to_stream, (VALUE stream, VALUE start, VALUE end),
      V_Subr3, DOC_write_area_to_stream) /*
::doc:write_area_to_stream::
write-area-to-stream STREAM START END

Copy the contents of the current buffer from START to END to stream STREAM.
::end:: */
{
    TX *tx = curr_vw->vw_Tx;
    long row, col;
    LINE *line;

    DECLARE2(start, POSP);
    DECLARE3(end, POSP);

    /* Don't call check_section() since that looks at the restriction. */
    if(POS_LESS_P(end, start) || VROW(start) < 0
       || VROW(end) > tx->tx_NumLines)
	return(cmd_signal(sym_invalid_area, list_3(VAL(tx), start, end)));

    row = VROW(start);
    line = tx->tx_Lines + row;
    col = MIN(VCOL(start), line->ln_Strlen - 1);

    while(row <= VROW(end))
    {
	int len = (((row == VROW(end))
		    ? VCOL(end) : line->ln_Strlen - 1) - col);
	if(len > 0
	   && stream_puts(stream, line->ln_Line + col, len, FALSE) != len)
	    return LISP_NULL;
	if(row != VROW(end)
	   && stream_putc(stream, '\n') != 1)
	    return LISP_NULL;
	col = 0;
	row++;
	line++;
    }

    return stream;
}

void
streams_init(void)
{
    INTERN(format_hooks_alist); DOC(format_hooks_alist);
    ADD_SUBR(subr_write);
    ADD_SUBR(subr_read_char);
    ADD_SUBR(subr_read_line);
    ADD_SUBR(subr_copy_stream);
    ADD_SUBR(subr_read);
    ADD_SUBR(subr_print);
    ADD_SUBR(subr_prin1);
    ADD_SUBR(subr_princ);
    ADD_SUBR(subr_format);
    ADD_SUBR(subr_make_string_input_stream);
    ADD_SUBR(subr_make_string_output_stream);
    ADD_SUBR(subr_get_output_stream_string);
    ADD_SUBR(subr_streamp);
    ADD_SUBR(subr_write_area_to_stream);
}
