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
#define BUILD_JADE
#include "regexp/regexp.h"

#include <string.h>
#include <ctype.h>
#include <stdlib.h>

_PR int buffer_strpbrk(TX *tx, POS *pos, const char *chars);
_PR int buffer_reverse_strpbrk(TX *tx, POS *pos, const char *chars);
_PR int buffer_strchr(TX *tx, POS *pos, char c);
_PR int buffer_reverse_strchr(TX *tx, POS *pos, char c);
_PR int buffer_compare_n(TX *tx, POS *pos, const char *str, int n, void *cmpfn);
_PR int forward_char(int count, TX *tx, POS *pos);
_PR int backward_char(long count, TX *tx, POS *pos);

_PR bool mystrcmp(u_char *, u_char *);
_PR u_char *mystrrstrn(u_char *, u_char *, int);
_PR u_char *mystrrchrn(u_char *, u_char, int);
_PR void find_init(void);



/* Basic buffer utility functions. These are also used by the regjade.c
   regexp code. */

/* Set POS to the position of the first char in TX from POS that is in
   the set of characters CHARS. Returns non-zero if such a character was
   found, zero if not (POS undefined in this case). */
int
buffer_strpbrk(TX *tx, POS *pos, const char *chars)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;
    int chars_has_newline = strchr(chars, '\n') ? 1 : 0;

    if(pos->pos_Col >= line->ln_Strlen)
    {
	pos->pos_Col = 0;
	pos->pos_Line++;
	line++;
    }

    while(pos->pos_Line < tx->tx_LogicalEnd)
    {
	u_char *ptr = strpbrk(line->ln_Line + pos->pos_Col, chars);
	if(ptr != NULL)
	{
	    pos->pos_Col = ptr - line->ln_Line;
	    return 1;
	}
	else if(chars_has_newline)
	{
	    pos->pos_Col = line->ln_Strlen - 1;
	    return 1;
	}
	pos->pos_Col = 0;
	pos->pos_Line++;
	line++;
    }
    return 0;
}

/* Same as buffer_strpbrk() but searches backwards. */
int
buffer_reverse_strpbrk(TX *tx, POS *pos, const char *chars)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;
    int chars_has_newline = strchr(chars, '\n') ? 1 : 0;

    if(pos->pos_Col >= line->ln_Strlen)
    {
	pos->pos_Col = line->ln_Strlen - 2;
	if(chars_has_newline)
	    return 1;
    }
    while(1)
    {
	u_char *match = line->ln_Line + pos->pos_Col;
	while(match >= line->ln_Line)
	{
	    if(strchr(chars, *match) != NULL)
	    {
		pos->pos_Col = match - line->ln_Line;
		return 1;
	    }
	    match--;
	}
	line--;
	if(--pos->pos_Line < tx->tx_LogicalStart)
	    return 0;
	pos->pos_Col = line->ln_Strlen - 2;
	if(chars_has_newline)
	    return 1;
    }
}

/* Set POS to the first character C in TX from POS. Returns non-zero for
   success. */
int
buffer_strchr(TX *tx, POS *pos, char c)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;
    if(pos->pos_Col >= line->ln_Strlen)
    {
	pos->pos_Col = 0;
	pos->pos_Line++;
	line++;
    }

    if(c == '\n')
    {
	if(pos->pos_Line < tx->tx_LogicalEnd - 1)
	{
	    pos->pos_Col = tx->tx_Lines[pos->pos_Line].ln_Strlen - 1;
	    return 1;
	}
	else
	{
	    /* no newline at end of buffer. */
	    return 0;
	}
    }
    else
    {
	while(pos->pos_Line < tx->tx_LogicalEnd)
	{
	    u_char *match = strchr(line->ln_Line + pos->pos_Col, c);
	    if(match)
	    {
		pos->pos_Col = match - line->ln_Line;
		return 1;
	    }
	    pos->pos_Line++;
	    pos->pos_Col = 0;
	    line++;
	}
	return 0;
    }
}

/* Same as buffer_strchr() but searches backwards. */
int
buffer_reverse_strchr(TX *tx, POS *pos, char c)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;

    if(pos->pos_Col >= line->ln_Strlen)
	pos->pos_Col = line->ln_Strlen - 1;

    if(c == '\n')
    {
	if(pos->pos_Col == line->ln_Strlen - 1)
	    return 1;
	if(pos->pos_Line == tx->tx_LogicalStart)
	    return 0;
	else
	{
	    line--;
	    pos->pos_Line--;
	    pos->pos_Col = line->ln_Strlen - 1;
	    return 1;
	}
    }
    else
    {
	while(1)
	{
	    u_char *match = line->ln_Line + pos->pos_Col;
	    while(match >= line->ln_Line)
	    {
		if(*match == c)
		{
		    pos->pos_Col = match - line->ln_Line;
		    return 1;
		}
		match--;
	    }
	    if(pos->pos_Line == tx->tx_LogicalStart)
		return 0;
	    line--;
	    pos->pos_Line--;
	    pos->pos_Col = line->ln_Strlen - 1;
	}
    }
}

/* Compares the string STR to the contents of TX at POS using the function
   CMPFN to do the comparison (must be a pointer to either strncmp() or
   strncasecmp(), or something with the same interface).
   Returns 0 for false, 1 for true. Leaves POS at the end of the match
   if it returns true. */
/* The argument CMPFN is declared void * to hopefully avoid irksome
   compiler warnings. */
#define CMPFN ((int (*)(const char *, const char *, size_t))cmpfn)
int
buffer_compare_n(TX *tx, POS *pos, const char *str, int n, void *cmpfn)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;

    if(pos->pos_Col >= line->ln_Strlen)
    {
	pos->pos_Col = 0;
	pos->pos_Line++;
	line++;
    }

    while(n > 0 && pos->pos_Line < tx->tx_LogicalEnd)
    {
	char *chunk = strchr(str, '\n');
	int len;
	if(chunk == NULL)
	    len = strlen(str);
	else
	    len = chunk - str;
	if(len > n)
	    len = n;
	if(len > 0)
	{
	    if(CMPFN(line->ln_Line + pos->pos_Col, str, len) != 0)
		return 0;
	    pos->pos_Col += len;
	    n -= len;
	    if(len == 0 || chunk == NULL)
		return 1;	/* looked at all n */
	}
	if(pos->pos_Col != line->ln_Strlen - 1)
	    return 0;
	n--;
	pos->pos_Col = 0;
	pos->pos_Line++;
	line++;
	str = chunk + 1;
    }
    return n == 0 ? 1 : 0;
}

/* Move POS forward COUNT characters in TX. Returns non-zero if the end
   of the buffer wasn't reached. */
int
forward_char(int count, TX *tx, POS *pos)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;
    if(pos->pos_Col >= line->ln_Strlen)
	pos->pos_Col = line->ln_Strlen - 1;
    while(count > 0)
    {
	if(count < (line->ln_Strlen - pos->pos_Col))
	{
	    pos->pos_Col += count;
	    count = 0;
	}
	else
	{
	    count -= line->ln_Strlen - pos->pos_Col;
	    pos->pos_Line++;
	    if(pos->pos_Line >= tx->tx_LogicalEnd)
		return 0;
	    line++;
	    pos->pos_Col = 0;
	}
    }
    return 1;
}

/* Move POS backward COUNT characters in TX. Returns non-zero if the start
   of the buffer wasn't reached. */
int
backward_char(long count, TX *tx, POS *pos)
{
    LINE *line = tx->tx_Lines + pos->pos_Line;
    while(count > 0)
    {
	if(count <= pos->pos_Col)
	{
	    pos->pos_Col -= count;
	    count = 0;
	}
	else
	{
	    count -= pos->pos_Col + 1; /* `+ 1' for the assumed '\n' */
	    pos->pos_Line--;
	    if(pos->pos_Line < tx->tx_LogicalStart)
		return 0;
	    line--;
	    pos->pos_Col = line->ln_Strlen - 1;
	}
    }
    return 1;
}


/* Regexp stuff. */

/* Storage for remembering where the last match was.
   LAST_STRING is the string that was matched against, only it's address is
   used.
   LAST_START and LAST_END is copied straight out of the regexp struct
   LAST_LINE is either -1 meaning the string was not in a buffer, or the
   line number it came from.  */
static char *last_string;
static regsubs last_matches;
static regtype last_match_type;

static VALUE sym_regexp_error;

static void
update_last_match(char *str, regexp *prog)
{
    last_string = str;
    memcpy(&last_matches, &prog->matches, sizeof(regsubs));
    last_match_type = prog->lasttype;
}

/* Kludge the saved startp/endp pointers as if the string STR
   had just been matched at LINENO. */
static void
translate_last_match(TX *tx, long lineno, char *str, regexp *prog)
{
    int i;
    for(i = 0; i < NSUBEXP; i++)
    {
	if(prog->matches.string.startp[i] == NULL)
	    last_matches.tx.startp[i].pos_Line = -1;
	else
	{
	    last_matches.tx.startp[i].pos_Line = lineno;
	    last_matches.tx.startp[i].pos_Col
		= prog->matches.string.startp[i] - str;
	}
	if(prog->matches.string.endp[i] == NULL)
	    last_matches.tx.endp[i].pos_Line = -1;
	else
	{
	    last_matches.tx.endp[i].pos_Line = lineno;
	    last_matches.tx.endp[i].pos_Col
		= prog->matches.string.endp[i] - str;
	}
    }
    last_match_type = reg_tx;
}

/* Fix the match buffers to reflect matching the a string from START
   to END. */
static void
set_string_match(POS *start, POS *end)
{
    int i;
    last_string = NULL;
    last_match_type = reg_tx;
    last_matches.tx.startp[0] = *start;
    last_matches.tx.endp[0] = *end;
    for(i = 1; i < NSUBEXP; i++)
    {
	last_matches.tx.startp[i].pos_Line = -1;
	last_matches.tx.endp[i].pos_Line = -1;
    }
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
    if(check_line(VTX(tx), &res))
    {
	VALUE ret = sym_nil;
	regexp *prog;
	prog = regcomp(VSTR(re));
	if(prog != NULL)
	{
	    if(regexec_tx(prog, VTX(tx), &res,
			  NILP(nocase_p) ? 0 : REG_NOCASE))
	    {
		update_last_match(NULL, prog);
		ret = make_lpos(&prog->matches.tx.startp[0]);
	    }
	    free(prog);
	}
	return(ret);
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
    if(check_line(VTX(tx), &res))
    {
	VALUE ret = sym_nil;
	regexp *prog = regcomp(VSTR(re));
	if(prog != NULL)
	{
	    if(regexec_reverse_tx(prog, VTX(tx), &res,
				  NILP(nocase_p) ? 0 : REG_NOCASE))
	    {
		update_last_match(NULL, prog);
		ret = make_lpos(&prog->matches.tx.startp[0]);
	    }
	    free(prog);
	}
	return(ret);
    }
    return NULL;
}

_PR VALUE cmd_find_next_string(VALUE str, VALUE pos, VALUE tx, VALUE nocasep);
DEFUN("find-next-string", cmd_find_next_string, subr_find_next_string, (VALUE str, VALUE pos, VALUE tx, VALUE nocasep), V_Subr4, DOC_find_next_string) /*
::doc:find_next_string::
find-next-string STRING [POS] [BUFFER] [IGNORE-CASE-P]

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
    if(check_line(VTX(tx), &res))
    {
	char first = VSTR(str)[0];
	long len = STRING_LEN(str);
	while(buffer_strchr(VTX(tx), &res, first))
	{
	    POS tem = res;
	    if(buffer_compare_n(VTX(tx), &tem, VSTR(str), len,
				NILP(nocasep) ? strncmp : strncasecmp))
	    {
		set_string_match(&res, &tem);
		return make_lpos(&res);
	    }
	    if(!forward_char(1, VTX(tx), &res))
		break;
	}
	return(sym_nil);
    }
    return NULL;
}

_PR VALUE cmd_find_prev_string(VALUE str, VALUE pos, VALUE tx, VALUE nocasep);
DEFUN("find-prev-string", cmd_find_prev_string, subr_find_prev_string, (VALUE str, VALUE pos, VALUE tx, VALUE nocasep), V_Subr4, DOC_find_prev_string) /*
::doc:find_prev_string::
find-prev-string STRING [POS] [BUFFER] [IGNORE-CASE-P]

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
    if(check_line(VTX(tx), &res))
    {
	char first = VSTR(str)[0];
	long len = STRING_LEN(str);
	while(buffer_reverse_strchr(VTX(tx), &res, first))
	{
	    POS tem = res;
	    if(buffer_compare_n(VTX(tx), &tem, VSTR(str), len,
				NILP(nocasep) ? strncmp : strncasecmp))
	    {
		set_string_match(&res, &tem);
		return make_lpos(&res);
	    }
	    if(!backward_char(1, VTX(tx), &res))
		break;
	}
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
    if(check_line(VTX(tx), &res))
    {
	if(buffer_strchr(VTX(tx), &res, VCHAR(ch)))
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
    if(check_line(VTX(tx), &res))
    {
	if(buffer_reverse_strchr(VTX(tx), &res, VCHAR(ch)))
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
	if(!read_only(VTX(tx)))
	{
	    regexp *prog = regcomp(VSTR(re));
	    if(prog != NULL && pad_pos(VTX(tx), &res))
	    {
		if(regmatch_tx(prog, VTX(tx), &res,
			       !NILP(nocase_p) ? REG_NOCASE : 0)
		   && (POS_EQUAL_P(&res, &prog->matches.tx.startp[0])))
		{
		    /* replen includes terminating zero */
		    long replen = regsublen(prog, VSTR(tplt), tx);
		    u_char *repstr;
		    if(replen && (repstr = str_alloc(replen)))
		    {
			VALUE rc = sym_nil;
			regsub(prog, VSTR(tplt), repstr, tx);
			delete_section(VTX(tx), &res,
				       &prog->matches.tx.endp[0]);
			if(insert_string(VTX(tx), repstr, replen - 1, &res))
			    rc = make_lpos(&res);
			str_free(repstr);
			return rc;
		    }
		    else if(replen)
			mem_error();
		}
		else
		    cmd_signal(sym_error, LIST_1(MKSTR("Regexp doesn't match replace position")));
		free(prog);
	    }
	}
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
	if(!read_only(VTX(tx)))
	{
	    long len = STRING_LEN(orig);
	    POS tem = res;
	    if(buffer_compare_n(VTX(tx), &tem, VSTR(orig), len, strcmp))
	    {
		delete_section(VTX(tx), &res, &tem);
		if(insert_string(VTX(tx), VSTR(new), STRING_LEN(new), &res))
		    return make_lpos(&res);
	    }
	    else
		cmd_signal(sym_error, LIST_1(MKSTR("String doesn't match replace position")));
	}
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
	    long replen = regsublen(prog, VSTR(tplt), NULL);
	    if(replen && (res = make_string(replen)))
		regsub(prog, VSTR(tplt), VSTR(res), NULL);
	    else if(!replen)
		res = string_dup("");
	    update_last_match(VSTR(match), prog);
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
	    update_last_match(VSTR(str), prog);
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
regexp-expand-line REGEXP TEMPLATE [POS] [BUFFER] [IGNORE-CASE-P]

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
	    translate_last_match(VTX(tx), linepos.pos_Line, line, prog);
	    replen = regsublen(prog, VSTR(tplt), NULL);
	    if(replen && (res = make_string(replen)))
		regsub(prog, VSTR(tplt), VSTR(res), NULL);
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
	    translate_last_match(VTX(tx), linepos.pos_Line, line, prog);
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

Returns t if REGEXP matches the text at POS.
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
	VALUE res = sym_nil;
	if(regmatch_tx(prog, VTX(tx), &pos, NILP(nocase_p) ? 0 : REG_NOCASE))
	{
	    update_last_match(NULL, prog);
	    res = sym_t;
	}
	free(prog);
	return res;
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
    if(last_match_type == reg_string)
    {
	if(last_matches.string.startp[i] == NULL)
	    return(sym_nil);
	i = last_matches.string.startp[i] - last_string;
	return(make_number(i));
    }
    else
    {
	if(last_matches.tx.startp[i].pos_Line != -1)
	    return make_lpos(&last_matches.tx.startp[i]);
	return sym_nil;
    }
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
    if(last_match_type == reg_string)
    {
	if(last_matches.string.endp[i] == NULL)
	    return(sym_nil);
	i = last_matches.string.endp[i] - last_string;
	return(make_number(i));
    }
    else
    {
	if(last_matches.tx.endp[i].pos_Line != -1)
	    return make_lpos(&last_matches.tx.endp[i]);
	return sym_nil;
    }
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
