/* find.c -- Searching and replacing
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
#include "regexp/regexp.h"

#include <string.h>
#include <ctype.h>
#include <stdlib.h>

_PR bool findnext(TX *, u_char *, POS *, bool);
_PR bool findprev(TX *, u_char *, POS *, bool);
_PR bool findstrnext(TX *, u_char *, POS *);
_PR bool findstrprev(TX *, u_char *, POS *);
_PR bool findcharnext(TX *, u_char, POS *);
_PR bool findcharprev(TX *, u_char, POS *);
_PR bool replaceit(TX *, u_char *, u_char *, POS *, bool);
_PR bool replaceitstr(TX *, u_char *, u_char *, POS *);
_PR bool mystrcmp(u_char *, u_char *);
_PR u_char *mystrrstrn(u_char *, u_char *, int);
_PR u_char *mystrrchrn(u_char *, u_char, int);
_PR void find_init(void);

/* Storage for remembering where the last match was.
   LAST_STRING is the string that was matched against, only it's address is
   used.
   LAST_START and LAST_END is copied straight out of the regexp struct
   LAST_LINE is either -1 meaning the string was not in a buffer, or the
   line number it came from.  */
static u_char *last_string;
static u_char *last_start[NSUBEXP];
static u_char *last_end[NSUBEXP];
static long last_line;

static VALUE sym_regexp_error;

static void
update_last_match(u_char *str, regexp *prog, long line)
{
    last_string = str;
    memcpy(last_start, (u_char **)prog->startp, sizeof(last_start));
    memcpy(last_end, (u_char **)prog->endp, sizeof(last_end));
    last_line = line;
}

_PR VALUE cmd_find_next_regexp(VALUE re, VALUE pos, VALUE tx, VALUE nocase_p);
DEFUN("find-next-regexp", cmd_find_next_regexp, subr_find_next_regexp, (VALUE re, VALUE pos, VALUE tx, VALUE nocase_p), V_Subr4, DOC_find_next_regexp) /*
::doc:find_next_regexp::
find-next-regexp REGEXP [POS] [BUFFER] [IGNORE-CASE-P]

Scans forwards from POS (or the cursor), in BUFFER, looking for a match
with REGEXP. Returns the position of the next match or nil.

When IGNORE-CASE-P is non-nil the case of matched strings are ignored. Note
that character classes are still case-significant.
::end:: */
{
    POS res;
    DECLARE1(re, STRINGP);
    if(POSP(pos))
	res = VPOS(pos);
    else
	res = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(check_pos(VTX(tx), &res))
    {
	if(findnext(VTX(tx), VSTR(re), &res, !NILP(nocase_p)))
	    return(make_lpos(&res));
	return(sym_nil);
    }
    return NULL;
}

_PR VALUE cmd_find_prev_regexp(VALUE re, VALUE pos, VALUE tx, VALUE nocase_p);
DEFUN("find-prev-regexp", cmd_find_prev_regexp, subr_find_prev_regexp, (VALUE re, VALUE pos, VALUE tx, VALUE nocase_p), V_Subr4, DOC_find_prev_regexp) /*
::doc:find_prev_regexp::
find-prev-regexp REGEXP [POS] [BUFFER] [IGNORE-CASE-P]

Scans backwards from POS (or the cursor), in BUFFER, looking for a match
with REGEXP. Returns the position of the next match or nil.

When IGNORE-CASE-P is non-nil the case of matched strings are ignored. Note
that character classes are still case-significant.
::end:: */
{
    POS res;
    DECLARE1(re, STRINGP);
    if(POSP(pos))
	res = VPOS(pos);
    else
	res = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(check_pos(VTX(tx), &res))
    {
	if(findprev(VTX(tx), VSTR(re), &res, !NILP(nocase_p)))
	    return(make_lpos(&res));
	return(sym_nil);
    }
    return NULL;
}

_PR VALUE cmd_find_next_string(VALUE str, VALUE pos, VALUE tx);
DEFUN("find-next-string", cmd_find_next_string, subr_find_next_string, (VALUE str, VALUE pos, VALUE tx), V_Subr3, DOC_find_next_string) /*
::doc:find_next_string::
find-next-string STRING [POS] [BUFFER]

Scans forwards from POS (or the cursor), in BUFFER, looking for a match
with STRING. Returns the position of the next match or nil.
::end:: */
{
    POS res;
    DECLARE1(str, STRINGP);
    if(POSP(pos))
	res = VPOS(pos);
    else
	res = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(check_pos(VTX(tx), &res))
    {
	if(findstrnext(VTX(tx), VSTR(str), &res))
	    return(make_lpos(&res));
	return(sym_nil);
    }
    return NULL;
}

_PR VALUE cmd_find_prev_string(VALUE str, VALUE pos, VALUE tx);
DEFUN("find-prev-string", cmd_find_prev_string, subr_find_prev_string, (VALUE str, VALUE pos, VALUE tx), V_Subr3, DOC_find_prev_string) /*
::doc:find_prev_string::
find-prev-string STRING [POS] [BUFFER]

Scans backwards from POS (or the cursor), in BUFFER, looking for a match
with STRING. Returns the position of the next match or nil.
::end:: */
{
    POS res;
    DECLARE1(str, STRINGP);
    if(POSP(pos))
	res = VPOS(pos);
    else
	res = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(check_pos(VTX(tx), &res))
    {
	if(findstrprev(VTX(tx), VSTR(str), &res))
	    return(make_lpos(&res));
	return(sym_nil);
    }
    return NULL;
}

_PR VALUE cmd_find_next_char(VALUE ch, VALUE pos, VALUE tx);
DEFUN("find-next-char", cmd_find_next_char, subr_find_next_char, (VALUE ch, VALUE pos, VALUE tx), V_Subr3, DOC_find_next_char) /*
::doc:find_next_char::
find-next-char CHAR [POS] [BUFFER]

Scans forwards from POS (or the cursor), in BUFFER, looking for a match
with CHAR. Returns the position of the next match or nil.
::end:: */
{
    POS res;
    DECLARE1(ch, CHARP);
    if(POSP(pos))
	res = VPOS(pos);
    else
	res = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(check_pos(VTX(tx), &res))
    {
	if(findcharnext(VTX(tx), VCHAR(ch), &res))
	    return(make_lpos(&res));
	return(sym_nil);
    }
    return NULL;
}

_PR VALUE cmd_find_prev_char(VALUE ch, VALUE pos, VALUE tx);
DEFUN("find-prev-char", cmd_find_prev_char, subr_find_prev_char, (VALUE ch, VALUE pos, VALUE tx), V_Subr3, DOC_find_prev_char) /*
::doc:find_prev_char::
find-prev-char CHAR [POS] [BUFFER]

Scans backwards from POS (or the cursor), in BUFFER, looking for a match
with CHAR. Returns the position of the next match or nil.
::end:: */
{
    POS res;
    DECLARE1(ch, CHARP);
    if(POSP(pos))
	res = VPOS(pos);
    else
	res = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(check_pos(VTX(tx), &res))
    {
	if(findcharprev(VTX(tx), VCHAR(ch), &res))
	    return(make_lpos(&res));
	return(sym_nil);
    }
    return NULL;
}

_PR VALUE cmd_replace_regexp(VALUE re, VALUE tplt, VALUE pos, VALUE tx, VALUE nocase_p);
DEFUN("replace-regexp", cmd_replace_regexp, subr_replace_regexp, (VALUE re, VALUE tplt, VALUE pos, VALUE tx, VALUE nocase_p), V_Subr5, DOC_replace_regexp) /*
::doc:replace_regexp::
replace-regexp REGEXP TEMPLATE [POS] [BUFFER] [IGNORE-CASE-P]

If the text at POS or the cursor, matches REGEXP replace it with TEMPLATE,
this is a string that can have the following escape characters,
  \0, \&   whole string matched by REGEXP
  \N	   N'th parenthensized expression (1 <= N <= 9)

When IGNORE-CASE-P is non-nil the case of matched strings are ignored. Note
that character classes are still case-significant.
::end:: */
{
    POS res;
    DECLARE1(re, STRINGP);
    DECLARE2(tplt, STRINGP);
    if(POSP(pos))
	res = VPOS(pos);
    else
	res = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(check_pos(VTX(tx), &res))
    {
	if(!read_only(VTX(tx)) && replaceit(VTX(tx), VSTR(re), VSTR(tplt),
					    &res, !NILP(nocase_p)))
	    return(make_lpos(&res));
	return(sym_nil);
    }
    return NULL;
}

_PR VALUE cmd_replace_string(VALUE orig, VALUE new, VALUE pos, VALUE tx);
DEFUN("replace-string", cmd_replace_string, subr_replace_string, (VALUE orig, VALUE new, VALUE pos, VALUE tx), V_Subr4, DOC_replace_string) /*
::doc:replace_string::
replace-string ORIGINAL NEW [POS] [BUFFER]

If the text at POS, or the cursor, matches ORIGINAL, replace it with the
string NEW.
::end:: */
{
    POS res;
    DECLARE1(orig, STRINGP);
    DECLARE2(new, STRINGP);
    if(POSP(pos))
	res = VPOS(pos);
    else
	res = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(check_pos(VTX(tx), &res))
    {
	if(!read_only(VTX(tx))
	   && replaceitstr(VTX(tx), VSTR(orig), VSTR(new), &res))
	    return(make_lpos(&res));
	return(sym_nil);
    }
    return NULL;
}

_PR VALUE cmd_regexp_expand(VALUE re, VALUE match, VALUE tplt, VALUE nocase_p);
DEFUN("regexp-expand", cmd_regexp_expand, subr_regexp_expand, (VALUE re, VALUE match, VALUE tplt, VALUE nocase_p), V_Subr4, DOC_regexp_expand) /*
::doc:regexp_expand::
regexp-expand REGEXP MATCHSTR TEMPLATE [IGNORE-CASE-P]

If REGEXP matches MATCHSTR then return the string made by expanding the
string TEMPLATE in a similar way to in the function `replace-regexp'.

When IGNORE-CASE-P is non-nil the case of matched strings are ignored. Note
that character classes are still case-significant.
::end:: */
{
    regexp *prog;
    DECLARE1(re, STRINGP);
    DECLARE2(match, STRINGP);
    DECLARE3(tplt, STRINGP);
    prog = regcomp(VSTR(re));
    if(prog)
    {
	VALUE res = sym_nil;
	if(regexec2(prog, VSTR(match), NILP(nocase_p) ? 0 : REG_NOCASE))
	{
	    long replen = regsublen(prog, VSTR(tplt));
	    if(replen && (res = make_string(replen)))
		regsub(prog, VSTR(tplt), VSTR(res));
	    else if(!replen)
		res = string_dup("");
	    update_last_match(VSTR(match), prog, -1);
	}
	free(prog);
	return(res);
    }
    return(NULL);
}

_PR VALUE cmd_regexp_match(VALUE re, VALUE str, VALUE nocase_p);
DEFUN("regexp-match", cmd_regexp_match, subr_regexp_match, (VALUE re, VALUE str, VALUE nocase_p), V_Subr3, DOC_regexp_match) /*
::doc:regexp_match::
regexp-match REGEXP STRING [IGNORE-CASE-P]

Return t if REGEXP matches STRING.

When IGNORE-CASE-P is non-nil the case of matched strings are ignored. Note
that character classes are still case-significant.
::end:: */
{
    regexp *prog;
    DECLARE1(re, STRINGP);
    DECLARE2(str, STRINGP);
    prog = regcomp(VSTR(re));
    if(prog)
    {
	VALUE res;
	if(regexec2(prog, VSTR(str), NILP(nocase_p) ? 0 : REG_NOCASE))
	{
	    update_last_match(VSTR(str), prog, -1);
	    res = sym_t;
	}
	else
	    res = sym_nil;
	free(prog);
	return(res);
    }
    return(NULL);
}

_PR VALUE cmd_regexp_expand_line(VALUE re, VALUE tplt, VALUE pos, VALUE tx, VALUE nocase_p);
DEFUN("regexp-expand-line", cmd_regexp_expand_line, subr_regexp_expand_line, (VALUE re, VALUE tplt, VALUE pos, VALUE tx, VALUE nocase_p), V_Subr5, DOC_regexp_expand_line) /*
::doc:regexp_expand_line::
regexp-expand REGEXP TEMPLATE [POS] [BUFFER] [IGNORE-CASE-P]

If REGEXP matches the line at POS in BUFFER then return the string made
by expanding the string TEMPLATE in a similar way to in the function
`replace-regexp' and the variables `find-last-start-pos' and
`find-last-end-pos' are set to the start and end of the match.

When IGNORE-CASE-P is non-nil the case of matched strings are ignored. Note
that character classes are still case-significant.
::end:: */
{
    POS linepos;
    regexp *prog;
    DECLARE1(re, STRINGP);
    DECLARE2(tplt, STRINGP);
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(POSP(pos))
	linepos = VPOS(pos);
    else
	linepos = *get_tx_cursor(VTX(tx));
    if(check_line(VTX(tx), &linepos) && (prog = regcomp(VSTR(re))))
    {
	VALUE res = sym_nil;
	u_char *line = VTX(tx)->tx_Lines[linepos.pos_Line].ln_Line;
	if(regexec2(prog, line, NILP(nocase_p) ? 0 : REG_NOCASE))
	{
	    long replen;
	    update_last_match(line, prog, linepos.pos_Line);
	    replen = regsublen(prog, VSTR(tplt));
	    if(replen && (res = make_string(replen)))
		regsub(prog, VSTR(tplt), VSTR(res));
	    else if(!replen)
		res = string_dup("");
	}
	free(prog);
	return(res);
    }
    return(NULL);
}

_PR VALUE cmd_regexp_match_line(VALUE re, VALUE pos, VALUE tx, VALUE nocase_p);
DEFUN("regexp-match-line", cmd_regexp_match_line, subr_regexp_match_line, (VALUE re, VALUE pos, VALUE tx, VALUE nocase_p), V_Subr4, DOC_regexp_match_line) /*
::doc:regexp_match_line::
regexp-match-line REGEXP [LINE-POS] [BUFFER] [IGNORE-CASE-P]

Attempts to match the regular-expression REGEXP to the line pointed to by
LINE-POS and BUFFER. If the match succeeds t is returned and the variables
`find-last-start-pos' and `find-last-end-pos' are set to the start and end
of the match.

When IGNORE-CASE-P is non-nil the case of matched strings are ignored. Note
that character classes are still case-significant.
::end:: */
{
    POS linepos;
    regexp *prog;
    DECLARE1(re, STRINGP);
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(POSP(pos))
	linepos = VPOS(pos);
    else
	linepos = *get_tx_cursor(VTX(tx));
    if(check_line(VTX(tx), &linepos) && (prog = regcomp(VSTR(re))))
    {
	VALUE res;
	u_char *line = VTX(tx)->tx_Lines[linepos.pos_Line].ln_Line;
	if(regexec2(prog, line, NILP(nocase_p) ? 0 : REG_NOCASE))
	{
	    update_last_match(line, prog, linepos.pos_Line);
	    res = sym_t;
	}
	else
	    res = sym_nil;
	free(prog);
	return(res);
    }
    return(NULL);
}

_PR VALUE cmd_looking_at(VALUE re, VALUE vpos, VALUE tx, VALUE nocase_p);
DEFUN("looking-at", cmd_looking_at, subr_looking_at, (VALUE re, VALUE vpos, VALUE tx, VALUE nocase_p), V_Subr4, DOC_looking_at) /*
::doc:looking_at::
looking-at REGEXP [POS] [BUFFER] [IGNORE-CASE-P]

Returns t if REGEXP matches the text at POS. Only the text from POS to the
end of the line is matched against.
::end:: */
{
    POS pos;
    regexp *prog;
    DECLARE1(re, STRINGP);
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(POSP(vpos))
	pos = VPOS(vpos);
    else
	pos = *get_tx_cursor(VTX(tx));
    if(check_line(VTX(tx), &pos) && (prog = regcomp(VSTR(re))))
    {
	VALUE res;
	u_char *line = VTX(tx)->tx_Lines[pos.pos_Line].ln_Line + pos.pos_Col;
	if(regexec2(prog, line, ((NILP(nocase_p) ? 0 : REG_NOCASE)
				 | ((pos.pos_Col == 0) ? 0 : REG_NOTBOL)))
	   && (prog->startp[0] == (char *)line))
	{
	    update_last_match(VTX(tx)->tx_Lines[pos.pos_Line].ln_Line,
			      prog, pos.pos_Line);
	    res = sym_t;
	}
	else
	    res = sym_nil;
	free(prog);
	return(res);
    }
    return(NULL);
}

_PR VALUE cmd_match_start(VALUE exp);
DEFUN("match-start", cmd_match_start, subr_match_start, (VALUE exp), V_Subr1, DOC_match_start) /*
::doc:match_start::
match-start [EXPRESSION-INDEX]

Return the position which the EXPRESSION-INDEX'th parenthesised expression
started at in the last successful regexp match. If EXPRESSION-INDEX is
nil or 0 the start of the whole match is returned instead.
The returned value will either be a position if the last match was in a
buffer, or an integer if the last match was in a string (i.e. regexp-match).
::end:: */
{
    long i;
    if(NUMBERP(exp))
    {
	i = VNUM(exp);
	if((i >= NSUBEXP) || (i < 0))
	    return(signal_arg_error(exp, 1));
    }
    else
	i = 0;
    if(last_start[i] == NULL)
	return(sym_nil);
    i = last_start[i] - last_string;
    if(last_line == -1)
	return(make_number(i));
    else
	return(make_lpos2(i, last_line));
}
	
_PR VALUE cmd_match_end(VALUE exp);
DEFUN("match-end", cmd_match_end, subr_match_end, (VALUE exp), V_Subr1, DOC_match_end) /*
::doc:match_end::
match-end [EXPRESSION-INDEX]

Return the position which the EXPRESSION-INDEX'th parenthesised expression
ended at in the last successful regexp match. If EXPRESSION-INDEX is
nil or 0 the end of the whole match is returned instead.
The returned value will either be a position if the last match was in a
buffer, or an integer if the last match was in a string (i.e. regexp-match).
::end:: */
{
    long i;
    if(NUMBERP(exp))
    {
	i = VNUM(exp);
	if((i >= NSUBEXP) || (i < 0))
	    return(signal_arg_error(exp, 1));
    }
    else
	i = 0;
    if(last_end[i] == NULL)
	return(sym_nil);
    i = last_end[i] - last_string;
    if(last_line == -1)
	return(make_number(i));
    else
	return(make_lpos2(i, last_line));
}

_PR VALUE cmd_regexp_quote(VALUE str);
DEFUN("regexp-quote", cmd_regexp_quote, subr_regexp_quote, (VALUE str), V_Subr1, DOC_regexp_quote) /*
::doc:regexp_quote::
regexp-quote STRING

Returns a new version of STRING, any characters which the regexp routines
treat specially (asterisks, square brackets, etc...) is quoted by the escape
character `\'. If the STRING does not contain any regexp meta-characters
it is returned as-is (un-copied).
::end:: */
{
    u_char *buf, *s;
    int buflen = 128, slen, i = 0;
    bool quoted = FALSE;
    VALUE res = NULL;
    DECLARE1(str, STRINGP);
    s = VSTR(str);
    slen = STRING_LEN(str);
    buf = str_alloc(buflen);
    if(!buf)
	goto error;
    while(slen-- > 0)
    {
	u_char c;
	/* Ensure string is long enough, this saves doing this twice. */
	if(i + 2 >= buflen)
	{
	    int newlen = buflen * 2;
	    u_char *newbuf = str_alloc(newlen);
	    if(!newbuf)
		goto error;
	    memcpy(newbuf, buf, i);
	    str_free(buf);
	    buf = newbuf;
	    buflen = newlen;
	}
	switch(c = *s++)
	{
	case '*':
	case '+':
	case '?':
	case '.':
	case '[':
	case ']':
	case '(':
	case ')':
	case '|':
	case '^':
	case '$':
	case '\\':	 /* do I want to do this? */
	    /* quote this character */
	    buf[i++] = '\\';
	    buf[i++] = c;
	    quoted = TRUE;
	    break;
	default:
	    buf[i++] = c;
	    break;
	}
    }
    if(!quoted)
	res = str;
    else
	res = string_dupn(buf, i);
error:
    if(buf)
	str_free(buf);
    return(res);
}

bool
findnext(TX *tx, u_char * re, POS *pos, bool nocase)
{
    bool rc = FALSE;
    regexp *prog;
    prog = regcomp(re);
    if(prog)
    {
	LINE *line = tx->tx_Lines + pos->pos_Line;
	long index;
	if(pos->pos_Col >= line->ln_Strlen)
	    index = line->ln_Strlen - 1;
	else
	    index = pos->pos_Col;
	while(!rc && (tx->tx_LogicalEnd > pos->pos_Line))
	{
	    if(regexec2(prog, line->ln_Line + index,
			(index ? REG_NOTBOL : 0) | (nocase ? REG_NOCASE : 0)))
	    {
		rc = TRUE;
	    }
	    else
	    {
		pos->pos_Line++;
		line++;
		index = 0;
	    }
	}
	if(rc)
	{
	    update_last_match(line->ln_Line, prog, pos->pos_Line);
	    pos->pos_Col = prog->startp[0] - ((char *)line->ln_Line);
	}
	free(prog);
    }
    return(rc);
}

bool
findprev(TX *tx, u_char *re, POS *pos, bool nocase)
{
    bool rc = FALSE;
    regexp *prog = regcomp(re);
    if(prog)
    {
	LINE *line = tx->tx_Lines + pos->pos_Line;
	long index;
	u_char *found = NULL;
	regexp last_match;		/* holds last match positions */
	if(pos->pos_Col >= line->ln_Strlen)
	    pos->pos_Col = line->ln_Strlen - 1;
	/*
	 * This makes the forward searching regexp matcher go backwards.
	 * we simply find as many matches as we can on a line, then return
	 * the last (taking the "start" position into account)
	 */
	while(!rc && (pos->pos_Line >= tx->tx_LogicalStart))
	{
	    index = 0;
	    while(regexec2(prog, line->ln_Line + index,
			   (index ? REG_NOTBOL : 0)
			   | (nocase ? REG_NOCASE : 0)))
	    {
		long oindex = index;
		if(((prog->startp)[0] - (char *)line->ln_Line) > pos->pos_Col)
		    break;
		found = (prog->startp)[0];
		memcpy(&last_match, prog, sizeof(last_match));
		rc = TRUE;
		index = (prog->endp)[0] - (char *)line->ln_Line;
		if(oindex >= index)
		{
		    if(index >= line->ln_Strlen)
			break;
		    else
			index = oindex + 1;
		}
	    }
	    if(!found)
	    {
		line--;
		pos->pos_Line--;
		pos->pos_Col = line->ln_Strlen - 1;
		index = 0;
	    }
	}
	if(rc)
	{
	    update_last_match(line->ln_Line, &last_match, pos->pos_Line);
	    pos->pos_Col = found - line->ln_Line;
	}
	free(prog);
    }
    return(rc);
}

bool
findstrnext(TX *tx, u_char *str, POS *pos)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;
    if(pos->pos_Col >= line->ln_Strlen)
    {
	pos->pos_Col = 0;
	pos->pos_Line++;
	line++;
    }
    while(tx->tx_LogicalEnd > pos->pos_Line)
    {
	u_char *match = strstr(line->ln_Line + pos->pos_Col, str);
	if(match)
	{
	    pos->pos_Col = match - line->ln_Line;
	    return(TRUE);
	}
	pos->pos_Line++;
	pos->pos_Col = 0;
	line++;
    }
    return(FALSE);
}

bool
findstrprev(TX *tx, u_char *str, POS *pos)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;
    if(pos->pos_Col >= line->ln_Strlen)
	pos->pos_Col = line->ln_Strlen - 1;
    while(pos->pos_Line >= tx->tx_LogicalStart)
    {
	u_char *match = mystrrstrn(line->ln_Line, str, pos->pos_Col + 1);
	if(match)
	{
	    pos->pos_Col = match - line->ln_Line;
	    return(TRUE);
	}
	line--;
	pos->pos_Line--;
	pos->pos_Col = line->ln_Strlen - 1;
    }
    return(FALSE);
}

bool
findcharnext(TX *tx, u_char c, POS *pos)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;
    if(pos->pos_Col >= line->ln_Strlen)
    {
	pos->pos_Col = 0;
	pos->pos_Line++;
	line++;
    }
    while(tx->tx_LogicalEnd > pos->pos_Line)
    {
	u_char *match = strchr(line->ln_Line + pos->pos_Col, c);
	if(match)
	{
	    pos->pos_Col = match - line->ln_Line;
	    return(TRUE);
	}
	pos->pos_Line++;
	pos->pos_Col = 0;
	line++;
    }
    return(FALSE);
}

bool
findcharprev(TX *tx, u_char c, POS *pos)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;
    if(pos->pos_Col >= line->ln_Strlen)
	pos->pos_Col = line->ln_Strlen - 1;
    while(pos->pos_Line >= tx->tx_LogicalStart)
    {
	u_char *match = mystrrchrn(line->ln_Line, c, pos->pos_Col + 1);
	if(match)
	{
	    pos->pos_Col = match - line->ln_Line;
	    return(TRUE);
	}
	line--;
	pos->pos_Line--;
	pos->pos_Col = line->ln_Strlen - 1;;
    }
    return(FALSE);
}

bool
replaceit(TX *tx, u_char *re, u_char *str, POS *pos, bool nocase)
{
    bool rc = FALSE;
    regexp *prog;
    if((prog = regcomp(re)) && pad_pos(tx, pos))
    {
	u_char *line = tx->tx_Lines[pos->pos_Line].ln_Line;
	u_char *posstr = line + pos->pos_Col;
	if(((*re != '^') || (pos->pos_Col == 0))
	   && regexec2(prog, posstr, nocase ? REG_NOCASE : 0)
	   && (pos->pos_Col == (prog->startp[0] - ((char *)line))))
	{
	    long replen = regsublen(prog, str);
	    u_char *repstr;
	    if(replen && (repstr = str_alloc(replen)))
	    {
		POS end;
		regsub(prog, str, repstr);
		end.pos_Line = pos->pos_Line;
		end.pos_Col = pos->pos_Col + (prog->endp[0] - prog->startp[0]);
		undo_record_deletion(tx, pos, &end);
		delete_chars(tx, pos, prog->endp[0] - prog->startp[0]);
		flag_deletion(tx, pos, &end);
		end.pos_Col = pos->pos_Col + (replen - 1);
		undo_record_insertion(tx, pos, &end);
		flag_insertion(tx, pos, &end);
		rc = insert_str(tx, repstr, pos);
		str_free(repstr);
	    }
	    else if(replen)
		mem_error();
	}
	else
	    cmd_signal(sym_error, LIST_1(MKSTR("Regexp doesn't match replace position")));
	free(prog);
    }
    return(rc);
}

bool
replaceitstr(TX *tx, u_char *orig, u_char *new, POS *pos)
{
    bool rc = FALSE;
    u_char *line;
    line = tx->tx_Lines[pos->pos_Line].ln_Line + pos->pos_Col;
    if(mystrcmp(orig, line))
    {
	POS end;
	end.pos_Line = pos->pos_Line;
	end.pos_Col = pos->pos_Col + strlen(orig);
	undo_record_deletion(tx, pos, &end);
	delete_chars(tx, pos, strlen(orig));
	flag_deletion(tx, pos, &end);
	end.pos_Col = pos->pos_Col + strlen(new);
	undo_record_insertion(tx, pos, &end);
	flag_insertion(tx, pos, &end);
	rc = insert_str(tx, new, pos);
    }
    else
	cmd_signal(sym_error, LIST_1(MKSTR("String doesn't match replace position")));
    return(rc);
}

void
regerror(char *err)
{
#if 0
    message(err);
#else
    cmd_signal(sym_regexp_error,
	       list_2(MKSTR("Regular-expression"), string_dup(err)));
#endif
}

/*
 * These functions return TRUE if the strings match, if str1 ends before
 * str2 they are still considered to be matching.
 */
bool
mystrcmp(u_char *str1, u_char *str2)
{
    while(*str2 && *str1)
    {
	if(*str2++ != *str1++)
	    return(FALSE);
    }
    if(*str2 || (*str2 == *str1))
	return(TRUE);
    return(FALSE);
}

/*
 * find last occurrence of str2 in str1
 */
u_char *
mystrrstrn(u_char *str1, u_char *str2, int str1Len)
{
    int str2len = strlen(str2);
    register int i;
    u_char c = str2[str2len - 1];
    for(i = str1Len - 1; i >= str2len; i--)
    {
	if(str1[i] == c)
	{
	    u_char *tmp = (str1 + i) - (str2len - 1);
	    if(mystrcmp(str2, tmp))
		return(tmp);
	}
    }
    return(NULL);
}

u_char *
mystrrchrn(u_char *str, u_char c, int strLen)
{
    register u_char *s = str + strLen;
    while(s != str)
    {
	if(*(--s) == c)
	    return(s);
    }
    return(NULL);
}

void
find_init(void)
{
    INTERN(sym_regexp_error, "regexp-error");
    cmd_put(sym_regexp_error, sym_error_message, MKSTR("Regexp error"));
    ADD_SUBR(subr_find_next_regexp);
    ADD_SUBR(subr_find_prev_regexp);
    ADD_SUBR(subr_find_next_string);
    ADD_SUBR(subr_find_prev_string);
    ADD_SUBR(subr_find_next_char);
    ADD_SUBR(subr_find_prev_char);
    ADD_SUBR(subr_replace_string);
    ADD_SUBR(subr_replace_regexp);
    ADD_SUBR(subr_regexp_expand);
    ADD_SUBR(subr_regexp_match);
    ADD_SUBR(subr_regexp_expand_line);
    ADD_SUBR(subr_regexp_match_line);
    ADD_SUBR(subr_looking_at);
    ADD_SUBR(subr_match_start);
    ADD_SUBR(subr_match_end);
    ADD_SUBR(subr_regexp_quote);
}
