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
#include <lib/jade_protos.h>

#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>

_PR int buffer_strpbrk(TX *tx, Pos *pos, const char *chars);
_PR int buffer_reverse_strpbrk(TX *tx, Pos *pos, const char *chars);
_PR int buffer_strchr(TX *tx, Pos *pos, char c);
_PR int buffer_reverse_strchr(TX *tx, Pos *pos, char c);
_PR int buffer_compare_n(TX *tx, Pos *pos, const char *str, int n, void *cmpfn);
_PR int forward_char(long count, TX *tx, Pos *pos);
_PR int backward_char(long count, TX *tx, Pos *pos);

_PR void mark_regexp_data(void);
_PR void push_regexp_data(struct saved_regexp_data *sd);
_PR void pop_regexp_data(void);

_PR void find_init(void);
_PR void find_kill(void);


/* Basic buffer utility functions. These are also used by the regjade.c
   regexp code.

   All of these break the rule about not modifying the contents of
   position objects. They all do! This is to reduce allocation when
   executing regular expressions. */

/* Set POS to the position of the first char in TX from POS that is in
   the set of characters CHARS. Returns non-zero if such a character was
   found, zero if not (POS undefined in this case). */
int
buffer_strpbrk(TX *tx, Pos *pos, const char *chars)
{
    LINE *line = tx->tx_Lines + PROW(pos);
    int chars_has_newline = strchr(chars, '\n') ? 1 : 0;

    if(PCOL(pos) >= line->ln_Strlen)
    {
	PCOL(pos) = 0;
	PROW(pos)++;
	line++;
    }

    while(PROW(pos) < tx->tx_LogicalEnd)
    {
	u_char *ptr = strpbrk(line->ln_Line + PCOL(pos), chars);
	if(ptr != NULL)
	{
	    PCOL(pos) = ptr - line->ln_Line;
	    return 1;
	}
	else if(chars_has_newline)
	{
	    PCOL(pos) = line->ln_Strlen - 1;
	    return 1;
	}
	PCOL(pos) = 0;
	PROW(pos)++;
	line++;
    }
    return 0;
}

/* Same as buffer_strpbrk() but searches backwards. */
int
buffer_reverse_strpbrk(TX *tx, Pos *pos, const char *chars)
{
    LINE *line = tx->tx_Lines + PROW(pos);
    int chars_has_newline = strchr(chars, '\n') ? 1 : 0;

    if(PCOL(pos) >= line->ln_Strlen)
    {
	PCOL(pos) = line->ln_Strlen - 1;
	if(chars_has_newline)
	    return 1;
    }
    while(1)
    {
	u_char *match = line->ln_Line + PCOL(pos);
	while(match >= line->ln_Line)
	{
	    if(strchr(chars, *match) != NULL)
	    {
		PCOL(pos) = match - line->ln_Line;
		return 1;
	    }
	    match--;
	}
	line--;
	if(--PROW(pos) < tx->tx_LogicalStart)
	    return 0;
	PCOL(pos) = line->ln_Strlen - 1;
	if(chars_has_newline)
	    return 1;
    }
}

/* Set POS to the first character C in TX from POS. Returns non-zero for
   success. */
int
buffer_strchr(TX *tx, Pos *pos, char c)
{
    LINE *line = tx->tx_Lines + PROW(pos);
    if(PCOL(pos) >= line->ln_Strlen)
    {
	PCOL(pos) = 0;
	PROW(pos)++;
	line++;
    }

    if(c == '\n')
    {
	if(PROW(pos) < tx->tx_LogicalEnd - 1)
	{
	    PCOL(pos) = tx->tx_Lines[PROW(pos)].ln_Strlen - 1;
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
	while(PROW(pos) < tx->tx_LogicalEnd)
	{
	    u_char *match = strchr(line->ln_Line + PCOL(pos), c);
	    if(match)
	    {
		PCOL(pos) = match - line->ln_Line;
		return 1;
	    }
	    PROW(pos)++;
	    PCOL(pos) = 0;
	    line++;
	}
	return 0;
    }
}

/* Same as buffer_strchr() but searches backwards. */
int
buffer_reverse_strchr(TX *tx, Pos *pos, char c)
{
    LINE *line = tx->tx_Lines + PROW(pos);

    if(PCOL(pos) >= line->ln_Strlen)
	PCOL(pos) = line->ln_Strlen - 1;

    if(c == '\n')
    {
	if(PCOL(pos) == line->ln_Strlen - 1)
	    return 1;
	if(PROW(pos) == tx->tx_LogicalStart)
	    return 0;
	else
	{
	    line--;
	    PROW(pos)--;
	    PCOL(pos) = line->ln_Strlen - 1;
	    return 1;
	}
    }
    else
    {
	while(1)
	{
	    u_char *match = line->ln_Line + PCOL(pos);
	    while(match >= line->ln_Line)
	    {
		if(*match == c)
		{
		    PCOL(pos) = match - line->ln_Line;
		    return 1;
		}
		match--;
	    }
	    if(PROW(pos) == tx->tx_LogicalStart)
		return 0;
	    line--;
	    PROW(pos)--;
	    PCOL(pos) = line->ln_Strlen - 1;
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
buffer_compare_n(TX *tx, Pos *pos, const char *str, int n, void *cmpfn)
{
    LINE *line = tx->tx_Lines + PROW(pos);

    if(PCOL(pos) >= line->ln_Strlen)
    {
	PCOL(pos) = 0;
	PROW(pos)++;
	line++;
    }

    while(n > 0 && PROW(pos) < tx->tx_LogicalEnd)
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
	    if(CMPFN(line->ln_Line + PCOL(pos), str, len) != 0)
		return 0;
	    PCOL(pos) += len;
	    n -= len;
	    if(len == 0 || chunk == NULL)
		return 1;	/* looked at all n */
	}
	if(PCOL(pos) != line->ln_Strlen - 1)
	    return 0;
	n--;
	PCOL(pos) = 0;
	PROW(pos)++;
	line++;
	str = chunk + 1;
    }
    return n == 0 ? 1 : 0;
}

/* Move POS forward COUNT characters in TX. Returns non-zero if the end
   of the buffer wasn't reached. */
int
forward_char(long count, TX *tx, Pos *pos)
{
    LINE *line = tx->tx_Lines + PROW(pos);
    if(PCOL(pos) >= line->ln_Strlen)
	PCOL(pos) = line->ln_Strlen - 1;
    while(count > 0)
    {
	if(count < (line->ln_Strlen - PCOL(pos)))
	{
	    PCOL(pos) += count;
	    count = 0;
	}
	else
	{
	    count -= line->ln_Strlen - PCOL(pos);
	    PROW(pos)++;
	    if(PROW(pos) >= tx->tx_LogicalEnd)
		return 0;
	    line++;
	    PCOL(pos) = 0;
	}
    }
    return 1;
}

/* Move POS backward COUNT characters in TX. Returns non-zero if the start
   of the buffer wasn't reached. */
int
backward_char(long count, TX *tx, Pos *pos)
{
    LINE *line = tx->tx_Lines + PROW(pos);
    while(count > 0)
    {
	if(count <= PCOL(pos))
	{
	    PCOL(pos) -= count;
	    count = 0;
	}
	else
	{
	    count -= PCOL(pos) + 1; /* `+ 1' for the assumed '\n' */
	    PROW(pos)--;
	    if(PROW(pos) < tx->tx_LogicalStart)
		return 0;
	    line--;
	    PCOL(pos) = line->ln_Strlen - 1;
	}
    }
    return 1;
}


/* Compiling regexps. */

/* A linked list is used to store all recently-used regexps in MRU
   order. At GC the regexps at the tail of the list are freed to
   satisfy the size limit.

   It might be better to use a hash-table. But by experience it seems
   that the cache is usually quite small, and therefore searching the
   list each compilation isn't too bad (and it makes the gc easier).

   Also, the hit-ratio is very good (as I'm typing this, ~0.97) */

struct cached_regexp {
    struct cached_regexp *next;
    VALUE regexp;
    regexp *compiled;
};

static struct cached_regexp *cached_regexps;	/* should be a hash table? */
static int regexp_hits, regexp_misses;
static int regexp_cache_limit = 1024;

_PR VALUE sym_regexp_error;
DEFSYM(regexp_error, "regexp-error");
DEFSTRING(err_regexp_error, "Regexp error");

static regexp *
compile_regexp(VALUE re)
{
    struct cached_regexp **x = &cached_regexps;
    int re_len;
    assert(STRINGP(re));
    re_len = STRING_LEN(re);
    while(*x != 0)
    {
	VALUE saved_re = (*x)->regexp;
	assert(STRINGP(saved_re));
	if(saved_re == re
	   || (STRING_LEN(saved_re) == re_len
	       && memcmp(VSTR(saved_re), VSTR(re), re_len) == 0))
	{
	    /* Found it. Move this node to the head of the list. Then
	       return the compiled copy. */
	    struct cached_regexp *this = *x;
	    if(x != &cached_regexps)
	    {
		*x = this->next;
		this->next = cached_regexps;
		cached_regexps = this;
	    }
	    regexp_hits++;
	    return this->compiled;
	}
	x = &((*x)->next);
    }

    /* No cached copy. Compile it, then add it to the cache. */
    {
	struct cached_regexp *this;
	regexp *compiled = regcomp(VSTR(re));
	if(compiled != 0)
	{
	    this = str_alloc(sizeof(struct cached_regexp));
	    if(this != 0)
	    {
		this->regexp = re;
		this->compiled = compiled;
		this->next = cached_regexps;
		cached_regexps = this;
		regexp_misses++;
		data_after_gc += (sizeof(struct cached_regexp)
				  + compiled->regsize);
		return compiled;
	    }
	}
	return 0;
    }
}

/* Called at GC */
static void
mark_cached_regexps(void)
{
    u_long total = 0;
    struct cached_regexp *x = cached_regexps, *xp = 0;
    while(x != 0 && total < regexp_cache_limit)
    {
	assert(STRINGP(x->regexp));
	MARKVAL(x->regexp);
	total += sizeof(struct cached_regexp) + x->compiled->regsize;
	xp = x;
	x = x->next;
    }
    if(xp != 0)
    {
	/* Free all following regexps */
	x = xp->next;
	xp->next = 0;
	while(x != 0)
	{
	    xp = x->next;
	    free(x->compiled);
	    str_free(x);
	    x = xp;
	}
    }
}

/* Free all cached regexps */
static void
release_cached_regexps(void)
{
    struct cached_regexp *x = cached_regexps;
    cached_regexps = 0;
    while(x != 0)
    {
	struct cached_regexp *next = x->next;
	free(x->compiled);
	str_free(x);
	x = next;
    }
}


/* Storing regexp context. */
	
/* Storage for remembering where the last match was.
   last_match_data is the string or buffer that was matched against.
   last_matches is a copy of the subexpression data of the last match.  */
static regtype last_match_type;
static VALUE last_match_data;
static regsubs last_matches;

static struct saved_regexp_data *saved_matches;

static void
update_last_match(VALUE data, regexp *prog)
{
    last_match_type = prog->lasttype;
    last_match_data = data;
    memcpy(&last_matches, &prog->matches, sizeof(last_matches));
}

/* Called by GC */
void
mark_regexp_data(void)
{
    struct saved_regexp_data *sd;

    /* Don't keep too many cached REs through GC. */
    mark_cached_regexps();

    if(last_match_type == reg_tx)
    {
	int i;
	for(i = 0; i < NSUBEXP; i++)
	{
	    MARKVAL(last_matches.tx.startp[i]);
	    MARKVAL(last_matches.tx.endp[i]);
	}
    }
    MARKVAL(last_match_data);

    for(sd = saved_matches; sd != 0; sd = sd->next)
    {
	if(sd->type == reg_tx)
	{
	    int i;
	    for(i = 0; i < NSUBEXP; i++)
	    {
		MARKVAL(sd->matches.tx.startp[i]);
		MARKVAL(sd->matches.tx.endp[i]);
	    }
	}
	MARKVAL(sd->data);
    }
}

/* Fix the match buffers to reflect matching a string from START to END. */
static void
set_string_match(TX *tx, VALUE start, VALUE end)
{
    int i;
    last_match_data = VAL(tx);
    last_match_type = reg_tx;
    last_matches.tx.startp[0] = start;
    last_matches.tx.endp[0] = end;
    for(i = 1; i < NSUBEXP; i++)
    {
	last_matches.tx.startp[i] = LISP_NULL;
	last_matches.tx.endp[i] = LISP_NULL;
    }
}

void
push_regexp_data(struct saved_regexp_data *sd)
{
    sd->type = last_match_type;
    sd->data = last_match_data;
    memcpy(&sd->matches, &last_matches, sizeof(regsubs));
    sd->next = saved_matches;
    saved_matches = sd;
}

void
pop_regexp_data(void)
{
    struct saved_regexp_data *sd = saved_matches;
    saved_matches = sd->next;
    last_match_type = sd->type;
    last_match_data = sd->data;
    memcpy(&last_matches, &sd->matches, sizeof(regsubs));
}


/* Matching and searching */

_PR VALUE cmd_re_search_forward(VALUE re, VALUE pos, VALUE tx, VALUE nocase_p);
DEFUN("re-search-forward", cmd_re_search_forward, subr_re_search_forward, (VALUE re, VALUE pos, VALUE tx, VALUE nocase_p), V_Subr4, DOC_re_search_forward) /*
::doc:re_search_forward::
re-search-forward REGEXP [POS] [BUFFER] [IGNORE-CASE-P]

Scans forwards from POS (or the cursor), in BUFFER, looking for a match
with REGEXP. Returns the position of the next match or nil. Updates the
match data.

When IGNORE-CASE-P is non-nil the case of matched strings are ignored. Note
that character classes are still case-significant.
::end:: */
{
    DECLARE1(re, STRINGP);
    if(!POSP(pos))
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(check_line(VTX(tx), pos))
    {
	VALUE ret = sym_nil;
	regexp *prog;
	prog = compile_regexp(re);
	if(prog != NULL)
	{
	    if(regexec_tx(prog, VTX(tx), pos, NILP(nocase_p) ? 0 : REG_NOCASE))
	    {
		update_last_match(tx, prog);
		ret = prog->matches.tx.startp[0];
	    }
	    else
		ret = sym_nil;
	}
	return(ret);
    }
    return LISP_NULL;
}

_PR VALUE cmd_re_search_backward(VALUE re, VALUE pos, VALUE tx, VALUE nocase_p);
DEFUN("re-search-backward", cmd_re_search_backward, subr_re_search_backward, (VALUE re, VALUE pos, VALUE tx, VALUE nocase_p), V_Subr4, DOC_re_search_backward) /*
::doc:re_search_backward::
re-search-backward REGEXP [POS] [BUFFER] [IGNORE-CASE-P]

Scans backwards from POS (or the cursor), in BUFFER, looking for a match
with REGEXP. Returns the position of the next match or nil. Updates the
match data.

When IGNORE-CASE-P is non-nil the case of matched strings are ignored. Note
that character classes are still case-significant.
::end:: */
{
    DECLARE1(re, STRINGP);
    if(!POSP(pos))
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(check_line(VTX(tx), pos))
    {
	VALUE ret = sym_nil;
	regexp *prog = compile_regexp(re);
	if(prog != NULL)
	{
	    if(regexec_reverse_tx(prog, VTX(tx), pos,
				     NILP(nocase_p) ? 0 : REG_NOCASE))
	    {
		update_last_match(tx, prog);
		ret = prog->matches.tx.startp[0];
	    }
	    else
		ret = sym_nil;
	}
	return(ret);
    }
    return LISP_NULL;
}

_PR VALUE cmd_search_forward(VALUE str, VALUE pos, VALUE tx, VALUE nocasep);
DEFUN("search-forward", cmd_search_forward, subr_search_forward, (VALUE str, VALUE pos, VALUE tx, VALUE nocasep), V_Subr4, DOC_search_forward) /*
::doc:search_forward::
search-forward STRING [POS] [BUFFER] [IGNORE-CASE-P]

Scans forwards from POS (or the cursor), in BUFFER, looking for a match
with STRING. Returns the position of the next match or nil. Updates the
match data.
::end:: */
{
    DECLARE1(str, STRINGP);
    if(!POSP(pos))
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(check_line(VTX(tx), pos))
    {
	char first[3];
	long len = STRING_LEN(str);
	Pos start;
	COPY_VPOS(&start, pos);
	if(!NILP(nocasep))
	{
	    first[0] = tolower(VSTR(str)[0]);
	    first[1] = toupper(VSTR(str)[0]);
	    first[2] = 0;
	}
	else
	{
	    first[0] = VSTR(str)[0];
	    first[1] = 0;
	}
	while(buffer_strpbrk(VTX(tx), &start, first))
	{
	    Pos end = start;
	    if(buffer_compare_n(VTX(tx), &end, VSTR(str), len,
				NILP(nocasep) ? strncmp : strncasecmp))
	    {
		VALUE vstart = make_pos(PCOL(&start), PROW(&start));
		set_string_match(VTX(tx), vstart,
				 make_pos(PCOL(&end), PROW(&end)));
		return vstart;
	    }
	    if(!forward_char(1, VTX(tx), &start))
		break;
	}
	return(sym_nil);
    }
    return LISP_NULL;
}

_PR VALUE cmd_search_backward(VALUE str, VALUE pos, VALUE tx, VALUE nocasep);
DEFUN("search-backward", cmd_search_backward, subr_search_backward, (VALUE str, VALUE pos, VALUE tx, VALUE nocasep), V_Subr4, DOC_search_backward) /*
::doc:search_backward::
search-backward STRING [POS] [BUFFER] [IGNORE-CASE-P]

Scans backwards from POS (or the cursor), in BUFFER, looking for a match
with STRING. Returns the position of the next match or nil. Updates the
match data.
::end:: */
{
    DECLARE1(str, STRINGP);
    if(!POSP(pos))
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(check_line(VTX(tx), pos))
    {
	char first[3];
	long len = STRING_LEN(str);
	Pos start;
	COPY_VPOS(&start, pos);
	if(!NILP(nocasep))
	{
	    first[0] = tolower(VSTR(str)[0]);
	    first[1] = toupper(VSTR(str)[0]);
	    first[2] = 0;
	}
	else
	{
	    first[0] = VSTR(str)[0];
	    first[1] = 0;
	}
	while(buffer_reverse_strpbrk(VTX(tx), &start, first))
	{
	    Pos end = start;
	    if(buffer_compare_n(VTX(tx), &end, VSTR(str), len,
				NILP(nocasep) ? strncmp : strncasecmp))
	    {
		VALUE vstart = make_pos(PCOL(&start), PROW(&start));
		set_string_match(VTX(tx), vstart,
				 make_pos(PCOL(&end), PROW(&end)));
		return vstart;
	    }
	    if(!backward_char(1, VTX(tx), &start))
		break;
	}
	return(sym_nil);
    }
    return LISP_NULL;
}

_PR VALUE cmd_char_search_forward(VALUE ch, VALUE pos, VALUE tx);
DEFUN("char-search-forward", cmd_char_search_forward, subr_char_search_forward, (VALUE ch, VALUE pos, VALUE tx), V_Subr3, DOC_char_search_forward) /*
::doc:char_search_forward::
char-search-forward CHAR [POS] [BUFFER]

Scans forwards from POS (or the cursor), in BUFFER, looking for a match
with CHAR. Returns the position of the next match or nil.
::end:: */
{
    DECLARE1(ch, INTP);
    if(!POSP(pos))
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(check_line(VTX(tx), pos))
    {
	Pos tem;
	COPY_VPOS(&tem, pos);
	if(buffer_strchr(VTX(tx), &tem, VINT(ch)))
	    return make_pos(PCOL(&tem), PROW(&tem));
	return(sym_nil);
    }
    return LISP_NULL;
}

_PR VALUE cmd_char_search_backward(VALUE ch, VALUE pos, VALUE tx);
DEFUN("char-search-backward", cmd_char_search_backward, subr_char_search_backward, (VALUE ch, VALUE pos, VALUE tx), V_Subr3, DOC_char_search_backward) /*
::doc:char_search_backward::
char-search-backward CHAR [POS] [BUFFER]

Scans backwards from POS (or the cursor), in BUFFER, looking for a match
with CHAR. Returns the position of the next match or nil.
::end:: */
{
    DECLARE1(ch, INTP);
    if(!POSP(pos))
	pos = curr_vw->vw_CursorPos;
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(check_line(VTX(tx), pos))
    {
	Pos tem;
	COPY_VPOS(&tem, pos);
	if(buffer_reverse_strchr(VTX(tx), &tem, VINT(ch)))
	    return make_pos(PCOL(&tem), PROW(&tem));
	return(sym_nil);
    }
    return LISP_NULL;
}

_PR VALUE cmd_string_match(VALUE re, VALUE str, VALUE start, VALUE nocasep);
DEFUN("string-match", cmd_string_match, subr_string_match, (VALUE re, VALUE str, VALUE start, VALUE nocasep), V_Subr4, DOC_string_match) /*
::doc:string_match::
string-match REGEXP STRING [START] [IGNORE-CASE-P]

Return t if REGEXP matches STRING. Updates the match data.

When defined, START is the index of the first character to start
matching at (counting from zero). When IGNORE-CASE-P is non-nil the
case of matched strings are ignored. Note that character classes are
still case-significant.
::end:: */
{
    regexp *prog;
    long xstart = INTP(start) ? VINT(start) : 0;
    DECLARE1(re, STRINGP);
    DECLARE2(str, STRINGP);
    prog = compile_regexp(re);
    if(prog)
    {
	VALUE res;
	if(regexec2(prog, VSTR(str) + xstart,
		    (NILP(nocasep) ? 0 : REG_NOCASE)
		    | (xstart == 0 ? 0 : REG_NOTBOL)))
	{
	    update_last_match(str, prog);
	    res = sym_t;
	}
	else
	    res = sym_nil;
	return(res);
    }
    return LISP_NULL;
}

_PR VALUE cmd_looking_at(VALUE re, VALUE pos, VALUE tx, VALUE nocase_p);
DEFUN("looking-at", cmd_looking_at, subr_looking_at, (VALUE re, VALUE pos, VALUE tx, VALUE nocase_p), V_Subr4, DOC_looking_at) /*
::doc:looking_at::
looking-at REGEXP [POS] [BUFFER] [IGNORE-CASE-P]

Returns t if REGEXP matches the text at POS. Updates the match data.
::end:: */
{
    DECLARE1(re, STRINGP);
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    if(!POSP(pos))
	pos = get_tx_cursor(VTX(tx));
    if(check_line(VTX(tx), pos))
    {
	regexp *prog = compile_regexp(re);
	if(prog != NULL)
	{
	    VALUE res;
	    if(regmatch_tx(prog, VTX(tx), pos, NILP(nocase_p) ? 0 : REG_NOCASE))
	    {
		update_last_match(tx, prog);
		res = prog->matches.tx.startp[0];
	    }
	    else
		res = sym_nil;
	    return res;
	}
    }
    return LISP_NULL;
}

_PR VALUE cmd_string_looking_at(VALUE re, VALUE string, VALUE start, VALUE nocasep);
DEFUN("string-looking-at", cmd_string_looking_at, subr_string_looking_at, (VALUE re, VALUE string, VALUE start, VALUE nocasep), V_Subr4, DOC_string_looking_at) /*
::doc:string_looking_at::
string-looking-at REGEXP STRING [START] [IGNORE-CASE-P]

Returns t if REGEXP matches the STRING (starting at character START).
Updates the match data.
::end:: */
{
    regexp *prog;
    long xstart = INTP(start) ? VINT(start) : 0;
    DECLARE1(re, STRINGP);
    DECLARE2(string, STRINGP);
    prog = compile_regexp(re);
    if(prog != NULL)
    {
	VALUE res;
	if(regmatch_string(prog, VSTR(string) + xstart,
			   (NILP(nocasep) ? 0 : REG_NOCASE)
			   | (xstart == 0 ? 0 : REG_NOTBOL)))
	{
	    update_last_match(string, prog);
	    res = sym_t;
	}
	else
	    res = sym_nil;
	return res;
    }
    return LISP_NULL;
}

_PR VALUE cmd_buffer_compare_string(VALUE string, VALUE pos, VALUE casep, VALUE len);
DEFUN("buffer-compare-string", cmd_buffer_compare_string, subr_buffer_compare_string, (VALUE string, VALUE pos, VALUE casep, VALUE len), V_Subr4, DOC_buffer_compare_string) /*
::doc:buffer_compare_string::
buffer-compare-string STRING [POSITION] [IGNORE-CASE] [LENGTH] 

Compare the characters in STRING with characters in the current buffer.
If LENGTH is defined, at most LENGTH characters are matched. When defined,
POSITION gives the first character in the buffer to match with. Unless
IGNORE-CASE is t, the match is case-significant.

If STRING matches the buffer, then the position of the character following
the last compared character is returned, otherwise nil is returned. Updates
the match data.
::end:: */
{
    long length;
    TX *tx = curr_vw->vw_Tx;
    DECLARE1(string, STRINGP);
    length = STRING_LEN(string);
    if(INTP(len))
    {
	if(VINT(len) <= length)
	    length = VINT(len);
	else
	    return signal_arg_error(len, 2);
    }
    if(!POSP(pos))
	pos = get_tx_cursor(tx);
    if(check_line(tx, pos))
    {
	Pos ppos;
	COPY_VPOS(&ppos, pos);
	if(buffer_compare_n(tx, &ppos, VSTR(string), length,
			    NILP(casep) ? strncmp : strncasecmp))
	{
	    VALUE end = COPY_POS(&ppos);
	    set_string_match(tx, pos, end);
	    return end;
	}
	else
	    return sym_nil;
    }
    else
	return LISP_NULL;
}
    

_PR VALUE cmd_expand_last_match(VALUE template);
DEFUN("expand-last-match", cmd_expand_last_match, subr_expand_last_match, (VALUE template), V_Subr1, DOC_expand_last_match) /*
::doc:expand_last_match::
expand-last-match TEMPLATE-STRING

Expand the saved expressions from the most recent successfully matched
regexp according to TEMPLATE-STRING, a string that may contain any of
the following escape sequences,

  \0, \&   whole string matched by REGEXP
  \N	   N'th parenthensized expression (1 <= N <= 9)
::end:: */
{
    long len;
    VALUE string;
    DECLARE1(template, STRINGP);
    len = regsublen(last_match_type, &last_matches,
		    VSTR(template), VPTR(last_match_data));
    string = make_string(len);
    regsub(last_match_type, &last_matches,
	   VSTR(template), VSTR(string), VPTR(last_match_data));
    return string;
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
    if(INTP(exp))
    {
	i = VINT(exp);
	if((i >= NSUBEXP) || (i < 0))
	    return(signal_arg_error(exp, 1));
    }
    else
	i = 0;
    if(last_match_type == reg_tx)
    {
	if(last_matches.tx.startp[i] != LISP_NULL)
	    return last_matches.tx.startp[i];
	return sym_nil;
    }
    else
    {
	if(last_matches.string.startp[i] == NULL)
	    return(sym_nil);
	i = last_matches.string.startp[i] - (char *)VSTR(last_match_data);
	return(MAKE_INT(i));
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
    if(INTP(exp))
    {
	i = VINT(exp);
	if((i >= NSUBEXP) || (i < 0))
	    return signal_arg_error(exp, 1);
    }
    else
	i = 0;
    if(last_match_type == reg_tx)
    {
	if(last_matches.tx.endp[i] != LISP_NULL)
	    return last_matches.tx.endp[i];
	return sym_nil;
    }
    else
    {
	if(last_matches.string.endp[i] == NULL)
	    return(sym_nil);
	i = last_matches.string.endp[i] - (char *)VSTR(last_match_data);
	return(MAKE_INT(i));
    }
}

_PR VALUE cmd_quote_regexp(VALUE str);
DEFUN("quote-regexp", cmd_quote_regexp, subr_quote_regexp, (VALUE str), V_Subr1, DOC_quote_regexp) /*
::doc:quote_regexp::
quote-regexp STRING

Returns a new version of STRING, any characters which the regexp routines
treat specially (asterisks, square brackets, etc...) is quoted by the escape
character `\'. If the STRING does not contain any regexp meta-characters
it is returned as-is (un-copied).
::end:: */
{
    u_char *buf, *s;
    int buflen = 128, slen, i = 0;
    bool quoted = FALSE;
    VALUE res = LISP_NULL;
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

_PR VALUE cmd_regexp_cache_control(VALUE limit);
DEFUN("regexp-cache-control", cmd_regexp_cache_control,
      subr_regexp_cache_control, (VALUE limit), V_Subr1,
      DOC_regexp_cache_control) /*
::doc:regexp_cache_control::
regexp-cache-control [SOFT-LIMIT]

If SOFT-LIMIT is defined, it specifies the maximum number of bytes that
the regexp cache may occupy after garbage collection.

Returns (SOFT-LIMIT CURRENT-SIZE CURRENT-ENTRIES HITS MISSES).
::end:: */
{
    int current_size = 0, current_items = 0;
    struct cached_regexp *x;

    if(INTP(limit) && VINT(limit) >= 0)
	regexp_cache_limit = VINT(limit);

    x = cached_regexps;
    while(x != 0)
    {
	current_items++;
	current_size += sizeof(struct cached_regexp) + x->compiled->regsize;
	x = x->next;
    }

    return list_5(MAKE_INT(regexp_cache_limit),
		  MAKE_INT(current_size), MAKE_INT(current_items),
		  MAKE_INT(regexp_hits), MAKE_INT(regexp_misses));
}	  

void
regerror(char *err)
{
#if 0
    message(err);
#else
    cmd_signal(sym_regexp_error, LIST_1(string_dup(err)));
#endif
}

void
find_init(void)
{
    INTERN(regexp_error); ERROR(regexp_error);
    ADD_SUBR(subr_re_search_forward);
    ADD_SUBR(subr_re_search_backward);
    ADD_SUBR(subr_search_forward);
    ADD_SUBR(subr_search_backward);
    ADD_SUBR(subr_char_search_forward);
    ADD_SUBR(subr_char_search_backward);
    ADD_SUBR(subr_string_match);
    ADD_SUBR(subr_looking_at);
    ADD_SUBR(subr_string_looking_at);
    ADD_SUBR(subr_buffer_compare_string);
    ADD_SUBR(subr_expand_last_match);
    ADD_SUBR(subr_match_start);
    ADD_SUBR(subr_match_end);
    ADD_SUBR(subr_quote_regexp);
    ADD_SUBR(subr_regexp_cache_control);
}

void
find_kill(void)
{
    release_cached_regexps();
}
