/* values.c -- Handling of Lisp data (includes garbage collection)
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
#include <assert.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

/* #define GC_MONITOR_STK */

#define rep_STRINGBLK_SIZE	510		/* ~4k */

/* Structure of string header allocation blocks */
typedef struct rep_string_block_struct {
    union {
	struct rep_string_block_struct *p;
	/* ensure that the following cons cell is aligned to at
	   least sizeof (rep_string) (for the dcache) */
	rep_string dummy;
    } next;
    rep_string data[rep_STRINGBLK_SIZE];
} rep_string_block;

/* Dumped data */
rep_cons *rep_dumped_cons_start, *rep_dumped_cons_end;
rep_symbol *rep_dumped_symbols_start, *rep_dumped_symbols_end;
repv rep_dumped_non_constants;

int rep_guardian_type;

DEFSYM(after_gc_hook, "after-gc-hook");


/* Type handling */

#define TYPE_HASH_SIZE 32
#define TYPE_HASH(type) (((type) >> 1) & (TYPE_HASH_SIZE-1))

static unsigned int next_free_type = 0;
static rep_type *data_types[TYPE_HASH_SIZE];

void
rep_register_type(unsigned int code, char *name,
		  int (*compare)(repv, repv),
		  void (*princ)(repv, repv),
		  void (*print)(repv, repv),
		  void (*sweep)(void),
		  void (*mark)(repv),
		  void (*mark_type)(void),
		  int (*getc)(repv),
		  int (*ungetc)(repv, int),
		  int (*putc)(repv, int),
		  int (*puts)(repv, void *, int, rep_bool),
		  repv (*bind)(repv),
		  void (*unbind)(repv))
{
    rep_type *t = rep_alloc(sizeof(rep_type));
    if (t == 0)
    {
	rep_mem_error ();
	return;
    }
    t->code = code;
    t->name = name;
    t->compare = compare ? compare : rep_ptr_cmp;
    t->princ = princ;
    t->print = print;
    t->sweep = sweep;
    t->mark = mark;
    t->mark_type = mark_type;
    t->getc = getc;
    t->ungetc = ungetc;
    t->putc = putc;
    t->puts = puts;
    t->bind = bind;
    t->unbind = unbind;
    t->next = data_types[TYPE_HASH(code)];
    data_types[TYPE_HASH(code)] = t;
}

unsigned int
rep_register_new_type(char *name,
		      int (*compare)(repv, repv),
		      void (*princ)(repv, repv),
		      void (*print)(repv, repv),
		      void (*sweep)(void),
		      void (*mark)(repv),
		      void (*mark_type)(void),
		      int (*getc)(repv),
		      int (*ungetc)(repv, int),
		      int (*putc)(repv, int),
		      int (*puts)(repv, void *, int, rep_bool),
		      repv (*bind)(repv),
		      void (*unbind)(repv))
{
    unsigned int code;
    assert(next_free_type != 256);
    code = (next_free_type++ << rep_CELL16_TYPE_SHIFT) | rep_CELL_IS_8 | rep_CELL_IS_16;
    rep_register_type(code, name, compare, princ, print,
		  sweep, mark, mark_type,
		  getc, ungetc, putc, puts, bind, unbind);
    return code;
}

rep_type *
rep_get_data_type(unsigned int code)
{
    rep_type *t = data_types[TYPE_HASH(code)];
    while (t != 0 && t->code != code)
	t = t->next;
    assert (t != 0);
    return t;
}


/* General object handling */

/* Returns zero if V1 == V2, less than zero if V1 < V2, and greater than
   zero otherwise. */
int
rep_value_cmp(repv v1, repv v2)
{
    if(v1 != rep_NULL && v2 != rep_NULL)
    {
	rep_type *t1 = rep_get_data_type(rep_TYPE(v1));
	if (t1 != 0)
	    return (v1 == v2) ? 0 : t1->compare(v1, v2);
	else
	    return (v1 == v2) ? 0 : 1;
    }
    return 1;
}

void
rep_princ_val(repv strm, repv val)
{
    if(val != rep_NULL)
    {
	rep_type *t = rep_get_data_type(rep_TYPE(val));
	rep_GC_root gc_strm, gc_val;
	rep_PUSHGC(gc_strm, strm);
	rep_PUSHGC(gc_val, val);
	t->princ(strm, val);
	rep_POPGC; rep_POPGC;
    }
}

void
rep_print_val(repv strm, repv val)
{
    if(val != rep_NULL)
    {
	rep_type *t = rep_get_data_type(rep_TYPE(val));
	rep_GC_root gc_strm, gc_val;
	rep_PUSHGC(gc_strm, strm);
	rep_PUSHGC(gc_val, val);
	t->print(strm, val);
	rep_POPGC; rep_POPGC;
    }
}

int
rep_type_cmp(repv val1, repv val2)
{
    return !(rep_TYPE(val1) == rep_TYPE(val2));
}


/* Strings */

static rep_string_block *string_block_chain;
static rep_string *string_freelist;
static int allocated_strings, used_strings, allocated_string_bytes;

DEFSTRING(null_string_const, "");

repv
rep_null_string(void)
{
    return rep_VAL(&null_string_const);
}

DEFSTRING(string_overflow, "String too long");

/* PTR should have been allocated using rep_alloc or malloc. Ownership
   of its memory passes to the lisp system. LEN _doesn't_ include the zero
   terminator */
repv
rep_box_string (char *ptr, long len)
{
    rep_string *str;

    if(len > rep_MAX_STRING)
	return Fsignal(Qerror, rep_LIST_1(rep_VAL(&string_overflow)));

    /* find a string header */
    str = string_freelist;
    if(str == NULL)
    {
	rep_string_block *cb;
	cb = rep_alloc(sizeof(rep_string_block));
	if(cb != NULL)
	{
	    int i;
	    allocated_strings += rep_STRINGBLK_SIZE;
	    cb->next.p = string_block_chain;
	    string_block_chain = cb;
	    for(i = 0; i < (rep_STRINGBLK_SIZE - 1); i++)
		cb->data[i].car = rep_VAL(&cb->data[i + 1]);
	    cb->data[i].car = 0;
	    string_freelist = cb->data;
	}
	else
	    return rep_mem_error ();
	str = string_freelist;
    }
    string_freelist = rep_STRING(str->car);
    used_strings++;
    rep_data_after_gc += sizeof(rep_string);

    str->car = rep_MAKE_STRING_CAR (len);
    rep_data_after_gc += len;
    str->data = ptr;
    return rep_VAL (str);
}

/* Return a string object with room for exactly LEN characters. No extra
   byte is allocated for a zero terminator; do this manually if required. */
repv
rep_make_string(long len)
{
    char *data = rep_alloc (len);
    if(data != NULL)
	return rep_box_string (data, len - 1);
    else
	return rep_NULL;
}

repv
rep_string_dupn(const char *src, long slen)
{
    rep_string *dst = rep_STRING(rep_make_string(slen + 1));
    if(dst != NULL)
    {
	memcpy(rep_STR(dst), src, slen);
	rep_STR(dst)[slen] = 0;
    }
    return rep_VAL(dst);
}

repv
rep_string_dup(const char *src)
{
    return rep_string_dupn(src, strlen(src));
}

repv
rep_concat2(char *s1, char *s2)
{
    int len = strlen(s1) + strlen(s2);
    repv res = rep_make_string(len + 1);
    stpcpy(stpcpy(rep_STR(res), s1), s2);
    return(res);
}

repv
rep_concat3(char *s1, char *s2, char *s3)
{
    int len = strlen(s1) + strlen(s2) + strlen(s3);
    repv res = rep_make_string(len + 1);
    stpcpy(stpcpy(stpcpy(rep_STR(res), s1), s2), s3);
    return(res);
}

repv
rep_concat4(char *s1, char *s2, char *s3, char *s4)
{
    int len = strlen(s1) + strlen(s2) + strlen(s3) + strlen(s4);
    repv res = rep_make_string(len + 1);
    stpcpy(stpcpy(stpcpy(stpcpy(rep_STR(res), s1), s2), s3), s4);
    return(res);
}

static int
string_cmp(repv v1, repv v2)
{
    if(rep_STRINGP(v1) && rep_STRINGP(v2))
    {
	long len1 = rep_STRING_LEN(v1);
	long len2 = rep_STRING_LEN(v2);
	long tem = memcmp(rep_STR(v1), rep_STR(v2), MIN(len1, len2));
	return tem != 0 ? tem : (len1 - len2);
    }
    else
	return 1;
}

static void
string_sweep(void)
{
    rep_string_block *cb = string_block_chain;
    string_block_chain = NULL;
    string_freelist = NULL;
    used_strings = 0;
    allocated_string_bytes = 0;
    while(cb != NULL)
    {
	rep_string_block *nxt = cb->next.p;
	rep_string *newfree = NULL, *newfreetail = NULL, *this;
	int i, newused = 0;
	for(i = 0, this = cb->data; i < rep_STRINGBLK_SIZE; i++, this++)
	{
	    /* if on the freelist then the CELL_IS_8 bit
	       will be unset (since the pointer is long aligned) */
	    if(rep_CELL_CONS_P(rep_VAL(this))
	       || !rep_GC_CELL_MARKEDP(rep_VAL(this)))
	    {
		if(!newfreetail)
		    newfreetail = this;
		if (!rep_CELL_CONS_P(rep_VAL(this)))
		    rep_free (this->data);
		this->car = rep_VAL(newfree);
		newfree = this;
	    }
	    else
	    {
		rep_GC_CLR_CELL(rep_VAL(this));
		allocated_string_bytes += rep_STRING_LEN(rep_VAL(this));
		newused++;
	    }
	}
	if(newused == 0)
	{
	    /* Whole block is unused, get rid of it.  */
	    rep_free(cb);
	    allocated_strings -= rep_STRINGBLK_SIZE;
	}
	else
	{
	    if(newfreetail != NULL)
	    {
		/* Link this mini-freelist onto the main one.  */
		newfreetail->car = rep_VAL(string_freelist);
		string_freelist = newfree;
		used_strings += newused;
	    }
	    /* Have to rebuild the block chain as well.  */
	    cb->next.p = string_block_chain;
	    string_block_chain = cb;
	}
	cb = nxt;
    }
}

/* Sets the length-field of the dynamic string STR to LEN. */
rep_bool
rep_set_string_len(repv str, long len)
{
    if(rep_STRING_WRITABLE_P(str))
    {
	rep_STRING(str)->car = rep_MAKE_STRING_CAR(len);
	return rep_TRUE;
    }
    else
	return rep_FALSE;
}


/* Misc */

int
rep_ptr_cmp(repv v1, repv v2)
{
    if(rep_TYPE(v1) == rep_TYPE(v2))
	return !(rep_PTR(v1) == rep_PTR(v2));
    else
	return 1;
}

repv
rep_box_pointer (void *p)
{
    unsigned rep_PTR_SIZED_INT low;
    low = (unsigned rep_PTR_SIZED_INT)p;
    if (low <= rep_LISP_MAX_INT)
	return rep_MAKE_INT (low);
    else
    {
	int i;
	unsigned rep_PTR_SIZED_INT high = (unsigned rep_PTR_SIZED_INT)p;
	for (i = rep_PTR_SIZED_INT_BITS / 2; i < rep_PTR_SIZED_INT_BITS; i++)
	    low &= ~(1 << i);
	high = high >> (rep_PTR_SIZED_INT_BITS/2);
	return Fcons (rep_MAKE_INT(high), rep_MAKE_INT(low));
    }
}

void *
rep_unbox_pointer (repv v)
{
    if (rep_INTP(v))
	return (void *) rep_INT(v);
    else if (rep_CONSP(v))
    {
	unsigned rep_PTR_SIZED_INT low, high;
	low = rep_INT(rep_CDR(v));
	high = rep_INT(rep_CAR(v));
	return (void *) (low | high << (rep_PTR_SIZED_INT_BITS/2));
    }
    else
	return 0;
}


/* Cons */

rep_cons_block *rep_cons_block_chain;
rep_cons *rep_cons_freelist;
int rep_allocated_cons, rep_used_cons;

rep_cons *
rep_allocate_cons (void)
{
    rep_cons *cn;
    cn = rep_cons_freelist;
    if(cn == NULL)
    {
	rep_cons_block *cb;
	cb = rep_alloc(sizeof(rep_cons_block));
	if(cb != NULL)
	{
	    int i;
	    rep_allocated_cons += rep_CONSBLK_SIZE;
	    cb->next.p = rep_cons_block_chain;
	    rep_cons_block_chain = cb;
	    for(i = 0; i < (rep_CONSBLK_SIZE - 1); i++)
		cb->cons[i].cdr = rep_CONS_VAL(&cb->cons[i + 1]);
	    cb->cons[i].cdr = 0;
	    rep_cons_freelist = cb->cons;
	}
	else
	    return rep_CONS (rep_mem_error ());
	cn = rep_cons_freelist;
    }
    return cn;
}

DEFUN("cons", Fcons, Scons, (repv car, repv cdr), rep_Subr2) /*
::doc:rep.data#cons::
cons CAR CDR

Returns a new cons-cell with car CAR and cdr CDR.
::end:: */
{
    rep_cons *c = rep_cons_freelist;
    if (c == 0)
	c = rep_allocate_cons ();
    rep_cons_freelist = rep_CONS (c->cdr);
    rep_used_cons++;
    rep_data_after_gc += sizeof(rep_cons);

    c->car = car;
    c->cdr = cdr;
    return rep_CONS_VAL (c);
}

void
rep_cons_free(repv cn)
{
    rep_CDR(cn) = rep_CONS_VAL(rep_cons_freelist);
    rep_cons_freelist = rep_CONS(cn);
    rep_used_cons--;
}

static void
cons_sweep(void)
{
    rep_cons_block *cb;
    rep_cons *tem_freelist = 0;
    int tem_used = 0;
    for (cb = rep_cons_block_chain; cb != 0; cb = cb->next.p)
    {
	register rep_cons *this = cb->cons;
	rep_cons *last = cb->cons + rep_CONSBLK_SIZE;
	while (this < last)
	{
	    if (!rep_GC_CONS_MARKEDP (rep_CONS_VAL (this)))
	    {
		this->cdr = rep_CONS_VAL (tem_freelist);
		tem_freelist = rep_CONS (this);
	    }
	    else
	    {
		rep_GC_CLR_CONS (rep_CONS_VAL (this));
		tem_used++;
	    }
	    this++;
	}
    }
    rep_cons_freelist = tem_freelist;
    rep_used_cons = tem_used;
}

static int
cons_cmp(repv v1, repv v2)
{
    int rc = 1;
    if(rep_TYPE(v1) == rep_TYPE(v2))
    {
	rc = rep_value_cmp(rep_CAR(v1), rep_CAR(v2));
	if(!rc)
	    rc = rep_value_cmp(rep_CDR(v1), rep_CDR(v2));
    }
    return rc;
}

repv
rep_list_1(repv v1)
{
    return rep_LIST_1(v1);
}

repv
rep_list_2(repv v1, repv v2)
{
    return rep_LIST_2(v1, v2);
}

repv
rep_list_3(repv v1, repv v2, repv v3)
{
    return rep_LIST_3(v1, v2, v3);
}

repv
rep_list_4(repv v1, repv v2, repv v3, repv v4)
{
    return rep_LIST_4(v1, v2, v3, v4);
}

repv
rep_list_5(repv v1, repv v2, repv v3, repv v4, repv v5)
{
    return rep_LIST_5(v1, v2, v3, v4, v5);
}


/* Vectors */

static rep_vector *vector_chain;
static int used_vector_slots;

repv
rep_make_vector(int size)
{
    int len = rep_VECT_SIZE(size);
    rep_vector *v = rep_ALLOC_CELL(len);
    if(v != NULL)
    {
	rep_SET_VECT_LEN(rep_VAL(v), size);
	v->next = vector_chain;
	vector_chain = v;
	used_vector_slots += size;
	rep_data_after_gc += len;
    }
    return rep_VAL(v);
}

static void
vector_sweep(void)
{
    rep_vector *this = vector_chain;
    vector_chain = NULL;
    used_vector_slots = 0;
    while(this != NULL)
    {
	rep_vector *nxt = this->next;
	if(!rep_GC_CELL_MARKEDP(rep_VAL(this)))
	    rep_FREE_CELL(this);
	else
	{
	    this->next = vector_chain;
	    vector_chain = this;
	    used_vector_slots += rep_VECT_LEN(this);
	    rep_GC_CLR_CELL(rep_VAL(this));
	}
	this = nxt;
    }
}

static int
vector_cmp(repv v1, repv v2)
{
    int rc = 1;
    if((rep_TYPE(v1) == rep_TYPE(v2)) && (rep_VECT_LEN(v1) == rep_VECT_LEN(v2)))
    {
	int i;
	int len = rep_VECT_LEN(v1);
	for(i = rc = 0; (i < len) && (rc == 0); i++)
	    rc = rep_value_cmp(rep_VECTI(v1, i), rep_VECTI(v2, i));
    }
    return rc;
}


/* Guardians */

static rep_guardian *guardians;

DEFUN("make-primitive-guardian", Fmake_primitive_guardian,
      Smake_primitive_guardian, (void), rep_Subr0)
{
    rep_guardian *g = rep_ALLOC_CELL (sizeof (rep_guardian));
    rep_data_after_gc += sizeof (rep_guardian);
    g->car = rep_guardian_type;
    g->accessible = Qnil;
    g->inaccessible = Qnil;
    g->next = guardians;
    guardians = g;
    return rep_VAL(g);
}

DEFUN("primitive-guardian-push", Fprimitive_guardian_push,
       Sprimitive_guardian_push, (repv g, repv obj), rep_Subr2)
{
    rep_DECLARE1 (g, rep_GUARDIANP);
    rep_GUARDIAN(g)->accessible = Fcons (obj, rep_GUARDIAN(g)->accessible);
    return g;
}

DEFUN("primitive-guardian-pop", Fprimitive_guardian_pop,
      Sprimitive_guardian_pop, (repv g), rep_Subr1)
{
    rep_DECLARE1 (g, rep_GUARDIANP);
    if (rep_GUARDIAN(g)->inaccessible != Qnil)
    {
	repv ret = rep_CAR (rep_GUARDIAN(g)->inaccessible);
	rep_GUARDIAN(g)->inaccessible = rep_CDR (rep_GUARDIAN(g)->inaccessible);
	return ret;
    }
    else
	return Qnil;
}

static void
mark_guardian (repv g)
{
    /* accessible list is marked by run_guardians */
    rep_MARKVAL (rep_GUARDIAN(g)->inaccessible);
}

static void
run_guardians (void)
{
    struct saved {
	struct saved *next;
	repv obj;
    } *changed = 0;

    /* scan all guardians for unmarked objects that used to be accessible */
    rep_guardian *g;
    for (g = guardians; g != 0; g = g->next)
    {
	repv *ptr = &g->accessible;
	/* cons cells store mark bit in CDR, so mask it out. */
	while ((*ptr & ~rep_VALUE_CONS_MARK_BIT) != Qnil)
	{
	    repv cell = *ptr & ~rep_VALUE_CONS_MARK_BIT;
	    if (!rep_GC_MARKEDP (rep_CAR (cell)))
	    {
		/* move object to inaccessible list */
		struct saved *new;
		/* have to preserve the cons mark bit in *ptr */
		*ptr = rep_GCDR (cell) | (*ptr & rep_VALUE_CONS_MARK_BIT);
		rep_CDR (cell) = g->inaccessible;
		g->inaccessible = cell;

		/* note that we need to mark this object */
		new = alloca (sizeof (struct saved));
		new->obj = rep_CAR (cell);
		new->next = changed;
		changed = new;
	    }
	    else
		ptr = rep_CDRLOC (cell);

	    /* mark the list infrastructure */
	    rep_GC_SET_CONS (cell);
	}
    }

    /* mark any objects that changed state */
    while (changed != 0)
    {
	rep_MARKVAL (changed->obj);
	changed = changed->next;
    }
}

static void
sweep_guardians (void)
{
    rep_guardian *g = guardians;
    guardians = 0;
    while (g)
    {
	rep_guardian *next = g->next;
	if (!rep_GC_CELL_MARKEDP (rep_VAL (g)))
	    rep_FREE_CELL (g);
	else
	{
	    rep_GC_CLR_CELL (rep_VAL (g));
	    g->next = guardians;
	    guardians = g;
	}
	g = next;
    }
}

static void
print_guardian (repv stream, repv obj)
{
    rep_stream_puts (stream, "#<guardian>", -1, rep_FALSE);
}


/* Garbage collection */

static repv **static_roots;
static int next_static_root, allocated_static_roots;

rep_GC_root *rep_gc_root_stack = 0;
rep_GC_n_roots *rep_gc_n_roots_stack = 0;

rep_bool rep_in_gc = rep_FALSE;

/* rep_data_after_gc = bytes of storage used since last gc
   rep_gc_threshold = value that rep_data_after_gc should be before gc'ing
   rep_idle_gc_threshold = value that DAGC should be before gc'ing in idle time */
int rep_data_after_gc, rep_gc_threshold = 200000, rep_idle_gc_threshold = 20000;

#ifdef GC_MONITOR_STK
static int *gc_stack_high_tide;
#endif

void
rep_mark_static(repv *obj)
{
    if (next_static_root == allocated_static_roots)
    {
	int new_size = (allocated_static_roots
			? (allocated_static_roots * 2) : 256);
	if (static_roots != 0)
	    static_roots = rep_realloc (static_roots,
					new_size * sizeof (repv *));
	else
	    static_roots = rep_alloc (new_size * sizeof (repv *));
	assert (static_roots != 0);
	allocated_static_roots = new_size;
    }
    static_roots[next_static_root++] = obj;
}

/* Mark a single Lisp object.
   This attempts to eliminate as much tail-recursion as possible (by
   changing the rep_VAL and jumping back to the `again' label).

   Note that rep_VAL must not be NULL, and must not already have been
   marked, (see the rep_MARKVAL macro in lisp.h) */
void
rep_mark_value(register repv val)
{
#ifdef GC_MONITOR_STK
    int dummy;
    /* Assumes that the stack grows downwards (towards 0) */
    if(&dummy < gc_stack_high_tide)
	gc_stack_high_tide = &dummy;
#endif

again:
    if(rep_INTP(val))
	return;

    /* must be a cell */
    if(rep_CELL_CONS_P(val))
    {
	if(rep_CONS_WRITABLE_P(val))
	{
	    /* A cons. Attempts to walk though whole lists at a time
	       (since Lisp lists mainly link from the cdr).  */
	    rep_GC_SET_CONS(val);
	    if(rep_NILP(rep_GCDR(val)))
		/* End of a list. We can safely
		   mark the car non-recursively.  */
		val = rep_CAR(val);
	    else
	    {
		rep_MARKVAL(rep_CAR(val));
		val = rep_GCDR(val);
	    }
	    if(val && !rep_INTP(val) && !rep_GC_MARKEDP(val))
		goto again;
	    return;
	}
	else
	{
	    /* A constant cons cell. */
	    return;
	}
    }

    if (rep_CELL16P(val))
    {
	/* A user allocated type. */
	rep_type *t = rep_get_data_type(rep_CELL16_TYPE(val));
	rep_GC_SET_CELL(val);
	if (t->mark != 0)
	    t->mark(val);
	return;
    }

    /* So we know that it's a cell8 object */
    switch(rep_CELL8_TYPE(val))
    {
	rep_type *t;

    case rep_Vector:
    case rep_Compiled:
	if(rep_VECTOR_WRITABLE_P(val))
	{
	    int i, len = rep_VECT_LEN(val);
	    rep_GC_SET_CELL(val);
	    for(i = 0; i < len; i++)
		rep_MARKVAL(rep_VECTI(val, i));
	}
	break;

    case rep_Symbol:
	/* Dumped symbols are dumped read-write, so no worries.. */
	rep_GC_SET_CELL(val);
	rep_MARKVAL(rep_SYM(val)->name);
	val = rep_SYM(val)->next;
	if(val && !rep_INTP(val) && !rep_GC_MARKEDP(val))
	    goto again;
	break;

    case rep_String:
	if(!rep_STRING_WRITABLE_P(val))
	    break;
	rep_GC_SET_CELL(val);
	break;

    case rep_Number:
	rep_GC_SET_CELL(val);
	break;

    case rep_Funarg:
	if (!rep_FUNARG_WRITABLE_P(val))
	    break;
	rep_GC_SET_CELL(val);
	rep_MARKVAL(rep_FUNARG(val)->name);
	rep_MARKVAL(rep_FUNARG(val)->env);
	rep_MARKVAL(rep_FUNARG(val)->structure);
	val = rep_FUNARG(val)->fun;
	if (val && !rep_GC_MARKEDP(val))
	    goto again;
	break;

    case rep_Subr0:
    case rep_Subr1:
    case rep_Subr2:
    case rep_Subr3:
    case rep_Subr4:
    case rep_Subr5:
    case rep_SubrN:
    case rep_SF:
	break;

    default:
	t = rep_get_data_type(rep_CELL8_TYPE(val));
	rep_GC_SET_CELL(val);
	if (t->mark != 0)
	    t->mark(val);
    }
}

DEFUN("garbage-threshold", Fgarbage_threshold, Sgarbage_threshold, (repv val), rep_Subr1) /*
::doc:rep.data#garbage-threshold::
garbage-threshold [NEW-VALUE]

The number of bytes of storage which must be used before a garbage-
collection is triggered.
::end:: */
{
    return rep_handle_var_int(val, &rep_gc_threshold);
}

DEFUN("idle-garbage-threshold", Fidle_garbage_threshold, Sidle_garbage_threshold, (repv val), rep_Subr1) /*
::doc:rep.data#idle-garbage-threshold::
idle-garbage-threshold [NEW-VALUE]

The number of bytes of storage which must be used before a garbage-
collection is triggered when the editor is idle.
::end:: */
{
    return rep_handle_var_int(val, &rep_idle_gc_threshold);
}

DEFUN_INT("garbage-collect", Fgarbage_collect, Sgarbage_collect, (repv stats), rep_Subr1, "") /*
::doc:rep.data#garbage-collect::
garbage-collect

Scans all allocated storage for unusable data, and puts it onto the free-
list. This is done automatically when the amount of storage used since the
last garbage-collection is greater than `garbage-threshold'.
::end:: */
{
    int i;
    rep_GC_root *rep_gc_root;
    rep_GC_n_roots *rep_gc_n_roots;
    struct rep_Call *lc;
#ifdef GC_MONITOR_STK
    int dummy;
    gc_stack_high_tide = &dummy;
#endif

    rep_in_gc = rep_TRUE;

    rep_macros_before_gc ();

    /* mark static objects */
    for(i = 0; i < next_static_root; i++)
	rep_MARKVAL(*static_roots[i]);
    /* mark stack based objects protected from GC */
    for(rep_gc_root = rep_gc_root_stack;
	rep_gc_root != 0; rep_gc_root = rep_gc_root->next)
    {
	rep_MARKVAL(*rep_gc_root->ptr);
    }
    for(rep_gc_n_roots = rep_gc_n_roots_stack; rep_gc_n_roots != 0;
	rep_gc_n_roots = rep_gc_n_roots->next)
    {
	for(i = 0; i < rep_gc_n_roots->count; i++)
	    rep_MARKVAL(rep_gc_n_roots->first[i]);
    }

    /* Do data-type specific marking. */
    for (i = 0; i < TYPE_HASH_SIZE; i++)
    {
	rep_type *t = data_types[i];
	while (t != 0)
	{
	    if (t->mark_type != 0)
		t->mark_type();
	    t = t->next;
	}
    }

    rep_mark_regexp_data();
    rep_mark_origins ();

#ifdef HAVE_DYNAMIC_LOADING
    rep_mark_dl_data();
#endif

    /* have to mark the Lisp backtrace.	 */
    lc = rep_call_stack;
    while(lc)
    {
	rep_MARKVAL(lc->fun);
	rep_MARKVAL(lc->args);
	rep_MARKVAL(lc->current_form);
	rep_MARKVAL(lc->saved_env);
	rep_MARKVAL(lc->saved_structure);
	lc = lc->next;
    }

    /* move and mark any guarded objects that became inaccessible */
    run_guardians ();

    /* look for dead weak references */
    rep_scan_weak_refs ();

    /* Finished marking, start sweeping. */

    rep_sweep_tuples ();
    for(i = 0; i < TYPE_HASH_SIZE; i++)
    {
	rep_type *t = data_types[i];
	while (t != 0)
	{
	    if (t->sweep != 0)
		t->sweep();
	    t = t->next;
	}
    }

    rep_data_after_gc = 0;
    rep_in_gc = rep_FALSE;

#ifdef GC_MONITOR_STK
    fprintf(stderr, "gc: stack usage = %d\n",
	    ((int)&dummy) - (int)gc_stack_high_tide);
#endif

    Fcall_hook (Qafter_gc_hook, Qnil, Qnil);

    if(stats != Qnil)
    {
	return rep_list_5(Fcons(rep_MAKE_INT(rep_used_cons),
				rep_MAKE_INT(rep_allocated_cons - rep_used_cons)),
			  Fcons(rep_MAKE_INT(rep_used_tuples),
				rep_MAKE_INT(rep_allocated_tuples
					     - rep_used_tuples)),
			  rep_list_3(rep_MAKE_INT(used_strings),
				     rep_MAKE_INT(allocated_strings),
				     rep_MAKE_INT(allocated_string_bytes)),
			  rep_MAKE_INT(used_vector_slots),
			  Fcons(rep_MAKE_INT(rep_used_funargs),
				rep_MAKE_INT(rep_allocated_funargs
					     - rep_used_funargs)));
    }
    else
	return Qt;
}


void
rep_pre_values_init(void)
{
    rep_register_type(rep_Cons, "cons", cons_cmp,
		  rep_lisp_prin, rep_lisp_prin, cons_sweep, 0, 0, 0, 0, 0, 0, 0, 0);
    rep_register_type(rep_Vector, "vector", vector_cmp,
		  rep_lisp_prin, rep_lisp_prin, vector_sweep, 0, 0, 0, 0, 0, 0, 0, 0);
    rep_register_type(rep_String, "string", string_cmp, rep_string_princ,
		  rep_string_print, string_sweep, 0, 0, 0, 0, 0, 0, 0, 0);
    rep_register_type(rep_Compiled, "bytecode", vector_cmp,
		  rep_lisp_prin, rep_lisp_prin, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    rep_register_type(rep_Void, "void", rep_type_cmp,
		  rep_lisp_prin, rep_lisp_prin, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    rep_register_type(rep_SF, "special-form", rep_ptr_cmp,
		  rep_lisp_prin, rep_lisp_prin, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    rep_register_type(rep_Subr0, "subr0", rep_ptr_cmp,
		  rep_lisp_prin, rep_lisp_prin, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    rep_register_type(rep_Subr1, "subr1", rep_ptr_cmp,
		  rep_lisp_prin, rep_lisp_prin, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    rep_register_type(rep_Subr2, "subr2", rep_ptr_cmp,
		  rep_lisp_prin, rep_lisp_prin, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    rep_register_type(rep_Subr3, "subr3", rep_ptr_cmp,
		  rep_lisp_prin, rep_lisp_prin, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    rep_register_type(rep_Subr4, "subr4", rep_ptr_cmp,
		  rep_lisp_prin, rep_lisp_prin, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    rep_register_type(rep_Subr5, "subr5", rep_ptr_cmp,
		  rep_lisp_prin, rep_lisp_prin, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    rep_register_type(rep_SubrN, "subrn", rep_ptr_cmp,
		  rep_lisp_prin, rep_lisp_prin, 0, 0, 0, 0, 0, 0, 0, 0, 0);

    rep_guardian_type = rep_register_new_type ("guardian", rep_ptr_cmp,
					       print_guardian, print_guardian,
					       sweep_guardians, mark_guardian,
					       0, 0, 0, 0, 0, 0, 0);
}

void
rep_values_init(void)
{
    repv tem = rep_push_structure ("rep.data");
    rep_ADD_SUBR(Scons);
    rep_ADD_SUBR(Sgarbage_threshold);
    rep_ADD_SUBR(Sidle_garbage_threshold);
    rep_ADD_SUBR_INT(Sgarbage_collect);
    rep_ADD_INTERNAL_SUBR(Smake_primitive_guardian);
    rep_ADD_INTERNAL_SUBR(Sprimitive_guardian_push);
    rep_ADD_INTERNAL_SUBR(Sprimitive_guardian_pop);
    rep_INTERN_SPECIAL(after_gc_hook);
    rep_pop_structure (tem);
}

void
rep_values_kill(void)
{
    rep_cons_block *cb = rep_cons_block_chain;
    rep_vector *v = vector_chain;
    rep_string_block *s = string_block_chain;
    while(cb != NULL)
    {
	rep_cons_block *nxt = cb->next.p;
	rep_free(cb);
	cb = nxt;
    }
    while(v != NULL)
    {
	rep_vector *nxt = v->next;
	rep_FREE_CELL(v);
	v = nxt;
    }
    while(s != NULL)
    {
	int i;
	rep_string_block *nxt = s->next.p;
	for (i = 0; i < rep_STRINGBLK_SIZE; i++)
	{
	    if (!rep_CELL_CONS_P (rep_VAL(s->data + i)))
		rep_free (s->data[i].data);
	}
	rep_free(s);
	s = nxt;
    }
    rep_cons_block_chain = NULL;
    vector_chain = NULL;
    string_block_chain = NULL;
}


/* Support for dumped Lisp code */

#ifdef ENABLE_BROKEN_DUMPING
void
rep_dumped_init(char *file)
{
    void *dl = rep_open_dl_library (rep_string_dup (file));
    if (dl == 0)
	fprintf (stderr, "warning: couldn't open dumped filed %s\n", file);
    else
    {
	/* Main function is to go through all dumped symbols, interning
	   them, and changing rep_NULL values to be void. */
	rep_symbol *s;

	/* But first, intern nil, it'll be filled in later. */
	Qnil = Fintern_symbol (rep_VAL(rep_dumped_symbols_end - 1),
			       rep_void_value);

	/* Stop one symbol too early, since we've already added it */
	for (s = rep_dumped_symbols_start; s < rep_dumped_symbols_end - 1; s++)
	{
	    /* Second arg is [OBARRAY], but it's only checked against
	       being a vector. */
	    Fintern_symbol (rep_VAL(s), rep_void_value);
	    if (s->value == rep_NULL)
		s->value = rep_void_value;
	}
    }
}
#endif
