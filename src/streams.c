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
#include "jade_protos.h"
#define BUILD_JADE
#include "regexp/regexp.h"

#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <stdlib.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

_PR int stream_getc(VALUE);
_PR int stream_ungetc(VALUE, int);
_PR int stream_putc(VALUE, int);
_PR int stream_puts(VALUE, u_char *, int, bool);
_PR int stream_read_esc(VALUE, int *);
_PR void stream_put_cntl(VALUE, int);

_PR void file_sweep(void);
_PR int file_cmp(VALUE, VALUE);
_PR void file_prin(VALUE, VALUE);

_PR void streams_init(void);
_PR void streams_kill(void);

static int
pos_getc(TX *tx, POS *pos)
{
    int c = EOF;
    if(pos->pos_Line < tx->tx_LogicalEnd)
    {
	LINE *ln = tx->tx_Lines + pos->pos_Line;
	if(pos->pos_Col >= (ln->ln_Strlen - 1))
	{
	    if(++pos->pos_Line == tx->tx_LogicalEnd)
	    {
		--pos->pos_Line;
		return(EOF);
	    }
	    pos->pos_Col = 0;
	    return('\n');
	}
	c = ln->ln_Line[pos->pos_Col++];
    }
    return(c);
}

static int
pos_putc(TX *tx, POS *pos, int c)
{
    int rc = EOF;
    if(!read_only(tx) && pad_pos(tx, pos))
    {
	u_char tmps[2];
	tmps[0] = (u_char)c;
	tmps[1] = 0;
	if(iscntrl(c))
	{
	    if(insert_string(tx, tmps, 1, pos))
		rc = 1;
	}
	else
	{
	    POS start = *pos;
	    if(insert_str_n(tx, tmps, 1, pos))
	    {
		undo_record_insertion(tx, &start, pos);
		flag_insertion(tx, &start, pos);
		rc = 1;
	    }
	}
    }
    return(rc);
}

static int
pos_puts(TX *tx, POS *pos, u_char *buf, int bufLen)
{
    int rc = EOF;
    if(!read_only(tx) && pad_pos(tx, pos))
    {
	if(insert_string(tx, buf, bufLen, pos))
	    rc = bufLen;
    }
    return(rc);
}

int
stream_getc(VALUE stream)
{
    int c = EOF;
    if(NILP(stream)
       && !(stream = cmd_symbol_value(sym_standard_input, sym_nil)))
	return(c);
    switch(VTYPE(stream))
    {
	VALUE res;
	int oldgci;

    case V_File:
	if(VFILE(stream)->lf_Name)
	    c = getc(VFILE(stream)->lf_File);
	break;

    case V_Mark:
	if(!VMARK(stream)->mk_Resident)
	    cmd_signal(sym_invalid_stream, list_2(stream, MKSTR("Marks used as streams must be resident")));
	else
	    c = pos_getc(VMARK(stream)->mk_File.tx,
			 &VPOS(VMARK(stream)->mk_Pos));
	break;

    case V_TX:
	c = pos_getc(VTX(stream), get_tx_cursor(VTX(stream)));
	break;

    case V_Cons:
	res = VCAR(stream);
	if(NUMBERP(res) && STRINGP(VCDR(stream)))
	{
	    c = (int)VSTR(VCDR(stream))[VNUM(res)];
	    if(c)
		VCAR(stream) = make_number(VNUM(res) + 1);
	    else
		c = EOF;
	    break;
	}
	else if(BUFFERP(res) && POSP(VCDR(stream)))
	{
	    c = pos_getc(VTX(res), &VPOS(VCDR(stream)));
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
	if((res = call_lisp0(stream)) && NUMBERP(res))
	    c = VNUM(res);
	gc_inhibit = oldgci;
	break;

#ifdef HAVE_SUBPROCESSES
    case V_Process:
	cmd_signal(sym_invalid_stream, list_2(stream, MKSTR("Processes are not input streams")));
	break;
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
#define POS_UNGETC(p,tx) \
    if(--((p)->pos_Col) < 0) \
    { \
	(p)->pos_Line--; \
	(p)->pos_Col = (tx)->tx_Lines[(p)->pos_Line].ln_Strlen - 1; \
    }
int
stream_ungetc(VALUE stream, int c)
{
    int rc = FALSE;
    if(NILP(stream)
       && !(stream = cmd_symbol_value(sym_standard_input, sym_nil)))
	return(rc);
    switch(VTYPE(stream))
    {
	POS *pos;
	VALUE tmp;
	int oldgci;

    case V_File:
	if(ungetc(c, VFILE(stream)->lf_File) != EOF)
	    rc = TRUE;
	break;

    case V_Mark:
	pos = &VPOS(VMARK(stream)->mk_Pos);
	POS_UNGETC(pos, VMARK(stream)->mk_File.tx)
	rc = TRUE;
	break;

    case V_TX:
	pos = get_tx_cursor(VTX(stream));
	POS_UNGETC(pos, VTX(stream))
	rc = TRUE;
	break;

    case V_Cons:
	tmp = VCAR(stream);
	if(NUMBERP(tmp) && STRINGP(VCDR(stream)))
	{
	    VCAR(stream) = make_number(VNUM(tmp) - 1);
	    rc = TRUE;
	    break;
	}
	else if(BUFFERP(tmp) && POSP(VCDR(stream)))
	{
	    POS_UNGETC(&VPOS(VCDR(stream)), VTX(tmp));
	    rc = TRUE;
	    break;
	}
	/* FALL THROUGH */

    case V_Symbol:
	tmp = make_number(c);
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
    switch(VTYPE(stream))
    {
	VALUE args, res, new;
	int len;
	u_char tmps[2];
	POS pos;

    case V_File:
	if(VFILE(stream)->lf_Name)
	{
	    if(putc(c, VFILE(stream)->lf_File) != EOF)
		rc = 1;
	}
	break;

    case V_Mark:
	if(!VMARK(stream)->mk_Resident)
	    cmd_signal(sym_invalid_stream, list_2(stream, MKSTR("Marks used as streams must be resident")));
	else
	{
	    pos = VPOS(VMARK(stream)->mk_Pos);
	    rc = pos_putc(VMARK(stream)->mk_File.tx, &pos, c);
	}
	break;

    case V_TX:
	pos = *(get_tx_cursor(VTX(stream)));
	rc = pos_putc(VTX(stream), &pos, c);
	break;

    case V_Cons:
	args = VCAR(stream);
	if(VTYPEP(args, V_DynamicString) && NUMBERP(VCDR(stream)))
	{
	    int actuallen = VNUM(VCDR(stream));
	    len = STRING_LEN(args);
	    if(len + 1 >= actuallen)
	    {
		int newlen = actuallen < 16 ? 32 : actuallen * 2;
		new = make_string(newlen + 1);
		if(!new)
		    break;
		memcpy(VSTR(new), VSTR(args), len);
		VCAR(stream) = new;
		VCDR(stream) = make_number(newlen);
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
		rc = pos_putc(VTX(args), &VPOS(VCDR(stream)), c);
	    else
	    {
		pos.pos_Line = VTX(args)->tx_LogicalEnd - 1;
		pos.pos_Col = VTX(args)->tx_Lines[pos.pos_Line].ln_Strlen - 1;
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
		    w->w_MiniBuf->vw_Flags |= VWFF_FORCE_REFRESH;
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
	    if((res = call_lisp1(stream, make_number(c))) && !NILP(res))
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
stream_puts(VALUE stream, u_char *buf, int bufLen, bool isValString)
{
    int rc = 0;
    if(NILP(stream)
       && !(stream = cmd_symbol_value(sym_standard_output, sym_nil)))
	return(rc);
    if(bufLen == -1)
	bufLen = isValString ? STRING_LEN(VAL(STRING_HDR(buf))) : strlen(buf);
    switch(VTYPE(stream))
    {
	VALUE args, res, new;
	int len, newlen;
	POS pos;

    case V_File:
	if(VFILE(stream)->lf_Name)
	{
	    if((rc = fwrite(buf, 1, bufLen, VFILE(stream)->lf_File)) == bufLen)
		rc = bufLen;
	}
	break;

    case V_Mark:
	if(!VMARK(stream)->mk_Resident)
	    cmd_signal(sym_invalid_stream, list_2(stream, MKSTR("Marks used as streams must be resident")));
	else
	{
	    pos = VPOS(VMARK(stream)->mk_Pos);
	    rc = pos_puts(VMARK(stream)->mk_File.tx, &pos, buf, bufLen);
	}
	break;

    case V_TX:
	pos = *(get_tx_cursor(VTX(stream)));
	rc = pos_puts(VTX(stream), &pos, buf, bufLen);
	break;

    case V_Cons:
	args = VCAR(stream);
	if(VTYPEP(args, V_DynamicString) && NUMBERP(VCDR(stream)))
	{
	    int actuallen = VNUM(VCDR(stream));
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
		VCDR(stream) = make_number(newlen);
		args = new;
	    }
#if 1
	    memcpy(VSTR(args) + len, buf, bufLen);
	    VSTR(args)[len + bufLen] = 0;
#else
	    strcpy(VSTR(args) + len, buf);
#endif
	    set_string_len(args, len + bufLen);
	    rc = bufLen;
	    break;
	}
	else if(BUFFERP(args))
	{
	    if(POSP(VCDR(stream)))
		rc = pos_puts(VTX(args), &VPOS(VCDR(stream)), buf, bufLen);
	    else
	    {
		pos.pos_Line = VTX(args)->tx_LogicalEnd - 1;
		pos.pos_Col = VTX(args)->tx_Lines[pos.pos_Line].ln_Strlen - 1;
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
		    w->w_MiniBuf->vw_Flags |= VWFF_FORCE_REFRESH;
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
		args = VAL(STRING_HDR(buf));
	    else
		args = string_dupn(buf, bufLen);
	    gc_inhibit = TRUE;
	    if((res = call_lisp1(stream, args)) && !NILP(res))
	    {
		if(NUMBERP(res))
		    rc = VNUM(res);
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

/* Print an escape sequence for the character C to STREAM. */
void
stream_put_cntl(VALUE stream, int c)
{
    u_char buff[40];
    u_char *buf = buff + 1;
    buff[0] = V_StaticString;
    switch(c)
    {
    case '\n':
	strcpy(buf, "\\n");
	break;
    case '\t':
	strcpy(buf, "\\t");
	break;
    case '\r':
	strcpy(buf, "\\r");
	break;
    case '\f':
	strcpy(buf, "\\f");
	break;
    case '\a':
	strcpy(buf, "\\a");
	break;
    default:
	if(c <= 0x3f)
	    sprintf(buf, "\\^%c", c + 0x40);
	else
	    sprintf(buf, "\\%o", (int)c);
	break;
    }
    stream_puts(stream, buf, -1, TRUE);
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
    case V_Number:
	actual = stream_putc(stream, VNUM(data));
	break;
    case V_StaticString:
    case V_DynamicString:
	if(NUMBERP(len))
	{
	    actual = VNUM(len);
	    if(actual > STRING_LEN(data))
		return(signal_arg_error(len, 3));
	    if(actual == STRING_LEN(data))
		vstring = TRUE;
	    else
		vstring = FALSE;
	}
	else
	{
	    actual = STRING_LEN(data);
	    vstring = TRUE;
	}
	actual = stream_puts(stream, VSTR(data), actual, vstring);
	break;
    default:
	return(signal_arg_error(data, 2));
    }
    return(make_number(actual));
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
	return(make_number(rc));
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
    if(FILEP(stream))
    {
	/* Special case for file streams. We can read a line in one go.	 */
	if(VFILE(stream)->lf_Name && fgets(buf, 400, VFILE(stream)->lf_File))
	    return(string_dup(buf));
	return(sym_nil);
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
    int len = 0, i = 0, c;
    u_char buff[402];
    u_char *buf = buff + 1;
    buff[0] = V_StaticString;
    while((c = stream_getc(source)) != EOF)
    {
	if(i == 400)
	{
	    buf[i] = 0;
	    if(stream_puts(dest, buf, i, TRUE) == EOF)
		break;
	    i = 0;
	}
	else
	    buf[i++] = c;
	len++;
	TEST_INT;
	if(INT_P)
	    return(NULL);
    }
    if(i > 0)
    {
	buff[i] = 0;
	stream_puts(dest, buf, i, TRUE);
    }
    if(len)
	return(make_number(len));
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
	return(NULL);
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
	return(NULL);
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
	return(NULL);
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
	return(NULL);
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
a substitution, using the next unused ARG. These format specifiers are
implemented:
   d	  print next ARG as decimal integer
   x	  print next ARG as hexadecimal integer
   o	  print next ARG in octal
   c	  print next ARG as ASCII character
   s	  unquoted representation (as from `princ') of next ARG
   S	  normal print'ed representation of next ARG
   %	  literal percentage character
::end:: */
{
    u_char *fmt, *last_fmt;
    bool mk_str;
    VALUE stream = ARG2;
    u_char c;
    DECLARE1(stream, STRINGP);
    fmt = VSTR(stream);
    stream = ARG1;
    if(NILP(stream))
    {
	stream = cmd_cons(string_dupn("", 0), make_number(0));
	mk_str = TRUE;
    }
    else
	mk_str = FALSE;
    args = move_down_list(args, 2);
    last_fmt = fmt;
    while((c = *fmt++))
    {
	if(c == '%')
	{
	    u_char tbuf[40], nfmt[4];
	    VALUE val = ARG1;
	    if(last_fmt != fmt - 1)
		stream_puts(stream, last_fmt, fmt - last_fmt - 1, FALSE);
	    switch(c = *fmt++)
	    {
	    case 'd':
	    case 'x':
	    case 'o':
	    case 'c':
		nfmt[0] = '%';
		nfmt[1] = 'l';
		nfmt[2] = c;
		nfmt[3] = 0;
		sprintf(tbuf, nfmt, NUMBERP(val) ? VNUM(val) : (long)val);
		stream_puts(stream, tbuf, -1, FALSE);
		break;
	    case 's':
		princ_val(stream, val);
		break;
	    case 'S':
		print_val(stream, val);
		break;
	    case '%':
		stream_putc(stream, '%');
		break;
	    }
	    if(c != '%')
		args = move_down_list(args, 1);
	    last_fmt = fmt;
	}
    }
    if(last_fmt != fmt - 1)
	stream_puts(stream, last_fmt, fmt - last_fmt - 1, FALSE);
    if(mk_str)
    {
	if(STRING_LEN(VCAR(stream)) != VNUM(VCDR(stream)))
	{
	    /* Truncate the stream to it's actual length. */
	    stream = cmd_copy_sequence(VCAR(stream));
	}
	else
	    stream = VCAR(stream);
    }
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
    return(cmd_cons(NUMBERP(start) ? start : make_number(0), string));
}

_PR VALUE cmd_make_string_output_stream(void);
DEFUN("make-string-output-stream", cmd_make_string_output_stream, subr_make_string_output_stream, (void), V_Subr0, DOC_make_string_output_stream) /*
::doc:make_string_output_stream::
make-string-output-stream

Returns an output stream which will accumulate the characters written to
it for the use of the `get-output-stream-string' function.
::end:: */
{
    return(cmd_cons(string_dupn("", 0), make_number(0)));
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
    if(!CONSP(strm) || !STRINGP(VCAR(strm)) || !NUMBERP(VCDR(strm)))
	return(signal_arg_error(strm, 1));
    if(STRING_LEN(VCAR(strm)) != VNUM(VCDR(strm)))
    {
	/* Truncate the string to it's actual length. */
	string = cmd_copy_sequence(VCAR(strm));
    }
    else
	string = VCAR(strm);
    /* Reset the stream. */
    VCAR(strm) = string_dupn("", 0);
    VCDR(strm) = make_number(0);
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
	   || (NUMBERP(car) && STRINGP(cdr))
	   || (STRINGP(car) && NUMBERP(cdr)))
	    res = sym_t;
	break;
    }
    return(res);
}

static LFile *lfile_chain;

void
file_sweep(void)
{
    LFile *lf = lfile_chain;
    lfile_chain = NULL;
    while(lf)
    {
	LFile *nxt = lf->lf_Next;
	if(!GC_MARKEDP(VAL(lf)))
	{
	    if(lf->lf_Name && !(lf->lf_Flags & LFF_DONT_CLOSE))
		fclose(lf->lf_File);
	    str_free(lf);
	}
	else
	{
	    GC_CLR(VAL(lf));
	    lf->lf_Next = lfile_chain;
	    lfile_chain = lf;
	}
	lf = nxt;
    }
}

int
file_cmp(VALUE v1, VALUE v2)
{
    if(VTYPE(v1) == VTYPE(v2))
    {
	if(VFILE(v1)->lf_Name && VFILE(v2)->lf_Name)
	    return(!same_files(VSTR(VFILE(v1)->lf_Name), VSTR(VFILE(v2)->lf_Name)));
    }
    return(1);
}

void
file_prin(VALUE strm, VALUE obj)
{
    stream_puts(strm, "#<file ", -1, FALSE);
    if(VFILE(obj)->lf_Name)
    {
	stream_puts(strm, VSTR(VFILE(obj)->lf_Name), -1, FALSE);
	stream_putc(strm, '>');
    }
    else
	stream_puts(strm, "*unbound*>", -1, FALSE);
}

_PR VALUE cmd_open(VALUE name, VALUE modes, VALUE file);
DEFUN("open", cmd_open, subr_open, (VALUE name, VALUE modes, VALUE file), V_Subr3, DOC_open) /*
::doc:open::
open [FILE-NAME MODE-STRING] [FILE]

Opens a file called FILE-NAME with modes MODE-STRING (standard c-library
modes, ie `r' == read, `w' == write, etc). If FILE is given it is an
existing file object which is to be closed before opening the new file on it.
::end:: */
{
    LFile *lf;
    if(!FILEP(file))
    {
	lf = str_alloc(sizeof(LFile));
	if(lf)
	{
	    lf->lf_Next = lfile_chain;
	    lfile_chain = lf;
	    lf->lf_Type = V_File;
	}
    }
    else
    {
	lf = VFILE(file);
	if(lf->lf_Name && !(lf->lf_Flags & LFF_DONT_CLOSE))
	    fclose(lf->lf_File);
    }
    if(lf)
    {
	lf->lf_File = NULL;
	lf->lf_Name = NULL;
	lf->lf_Flags = 0;
	if(STRINGP(name) && STRINGP(modes))
	{
	    lf->lf_File = fopen(VSTR(name), VSTR(modes));
	    if(lf->lf_File)
	    {
		lf->lf_Name = name;
#ifdef HAVE_UNIX
		/*
		 * set close-on-exec for easy process fork()ing
		 */
		fcntl(fileno(lf->lf_File), F_SETFD, 1);
#endif
	    }
	    else
		return(cmd_signal(sym_file_error, list_2(lookup_errno(), name)));
	}
	return(VAL(lf));
    }
    return(NULL);
}

_PR VALUE cmd_close(VALUE file);
DEFUN("close", cmd_close, subr_close, (VALUE file), V_Subr1, DOC_close) /*
::doc:close::
close FILE

Kills any association between object FILE and the file in the filesystem that
it has open.
::end:: */
{
    DECLARE1(file, FILEP);
    if(VFILE(file)->lf_Name && !(VFILE(file)->lf_Flags & LFF_DONT_CLOSE))
	fclose(VFILE(file)->lf_File);
    VFILE(file)->lf_File = NULL;
    VFILE(file)->lf_Name = NULL;
    return(file);
}

_PR VALUE cmd_flush_file(VALUE file);
DEFUN("flush-file", cmd_flush_file, subr_flush_file, (VALUE file), V_Subr1, DOC_flush_file) /*
::doc:flush_file::
flush-file FILE

Flushes any buffered output on FILE.
::end:: */
{
    DECLARE1(file, FILEP);
    if(VFILE(file)->lf_Name)
	fflush(VFILE(file)->lf_File);
    return(file);
}

_PR VALUE cmd_filep(VALUE arg);
DEFUN("filep", cmd_filep, subr_filep, (VALUE arg), V_Subr1, DOC_filep) /*
::doc:filep::
filep ARG

Returns t if ARG is a file object.
::end:: */
{
    if(FILEP(arg))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_file_bound_p(VALUE file);
DEFUN("file-bound-p", cmd_file_bound_p, subr_file_bound_p, (VALUE file), V_Subr1, DOC_file_bound_p) /*
::doc:file_bound_p::
file-bound-p FILE

Returns t if FILE is currently bound to a physical file.
::end:: */
{
    DECLARE1(file, FILEP);
    if(VFILE(file)->lf_Name)
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_file_binding(VALUE file);
DEFUN("file-binding", cmd_file_binding, subr_file_binding, (VALUE file), V_Subr1, DOC_file_binding) /*
::doc:file_binding::
file-binding FILE

Returns the name of the physical file FILE is bound to, or nil.
::end:: */
{
    DECLARE1(file, FILEP);
    if(VFILE(file)->lf_Name)
	return(VFILE(file)->lf_Name);
    return(sym_nil);
}

_PR VALUE cmd_file_eof_p(VALUE file);
DEFUN("file-eof-p", cmd_file_eof_p, subr_file_eof_p, (VALUE file), V_Subr1, DOC_file_eof_p) /*
::doc:file_eof_p::
file-eof-p FILE

Returns t when the end of FILE is reached.
::end:: */
{
    DECLARE1(file, FILEP);
    if(VFILE(file)->lf_Name && feof(VFILE(file)->lf_File))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_read_file_until(VALUE file, VALUE re, VALUE nocase_p);
DEFUN("read-file-until", cmd_read_file_until, subr_read_file_until, (VALUE file, VALUE re, VALUE nocase_p), V_Subr3, DOC_read_file_until) /*
::doc:read_file_until::
read-file-until FILE REGEXP [IGNORE-CASE-P]

Read lines from the Lisp file object FILE until one matching the regular
expression REGEXP is found. The matching line is returned, or nil if no
lines match.
If IGNORE-CASE-P is non-nil the regexp matching is not case-sensitive.
::end:: */
{
    regexp *prog;
    u_char buf[400];		/* Fix this later. */
    DECLARE1(file, FILEP);
    DECLARE2(re, STRINGP);
    if(!VFILE(file)->lf_Name)
	return(cmd_signal(sym_bad_arg, list_2(MKSTR("File object is unbound"), file)));
    prog = regcomp(VSTR(re));
    if(prog)
    {
	int eflags = NILP(nocase_p) ? 0 : REG_NOCASE;
	FILE *fh = VFILE(file)->lf_File;
	VALUE res = sym_nil;
	while(fgets(buf, 400, fh))
	{
	    if(regexec2(prog, buf, eflags))
	    {
		res = string_dup(buf);
		break;
	    }
	}
	free(prog);
	return(res);
    }
    return(NULL);
}

_PR VALUE cmd_stdin_file(void);
DEFUN("stdin-file", cmd_stdin_file, subr_stdin_file, (void), V_Subr0, DOC_stdin_file) /*
::doc:stdin_file::
stdin-file

Returns the file object representing the editor's standard input.
::end:: */
{
    static VALUE stdin_file;
    if(stdin_file)
	return(stdin_file);
    stdin_file = cmd_open(sym_nil, sym_nil, sym_nil);
    VFILE(stdin_file)->lf_Name = MKSTR("<stdin>");
    VFILE(stdin_file)->lf_File = stdin;
    VFILE(stdin_file)->lf_Flags |= LFF_DONT_CLOSE;
    mark_static(&stdin_file);
    return(stdin_file);
}

_PR VALUE cmd_stdout_file(void);
DEFUN("stdout-file", cmd_stdout_file, subr_stdout_file, (void), V_Subr0, DOC_stdout_file) /*
::doc:stdout_file::
stdout-file

Returns the file object representing the editor's standard output.
::end:: */
{
    static VALUE stdout_file;
    if(stdout_file)
	return(stdout_file);
    stdout_file = cmd_open(sym_nil, sym_nil, sym_nil);
    VFILE(stdout_file)->lf_Name = MKSTR("<stdout>");
    VFILE(stdout_file)->lf_File = stdout;
    VFILE(stdout_file)->lf_Flags |= LFF_DONT_CLOSE;
    mark_static(&stdout_file);
    return(stdout_file);
}

void
streams_init(void)
{
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
    ADD_SUBR(subr_open);
    ADD_SUBR(subr_close);
    ADD_SUBR(subr_flush_file);
    ADD_SUBR(subr_filep);
    ADD_SUBR(subr_file_bound_p);
    ADD_SUBR(subr_file_binding);
    ADD_SUBR(subr_file_eof_p);
    ADD_SUBR(subr_read_file_until);
    ADD_SUBR(subr_stdin_file);
    ADD_SUBR(subr_stdout_file);
}

void
streams_kill(void)
{
    LFile *lf = lfile_chain;
    while(lf)
    {
	LFile *nxt = lf->lf_Next;
	if(lf->lf_Name && !(lf->lf_Flags & LFF_DONT_CLOSE))
	    fclose(lf->lf_File);
	str_free(lf);
	lf = nxt;
    }
    lfile_chain = NULL;
}
