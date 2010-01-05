/* tables.c -- hash tables
   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* notes:

   The api of this module (except for make-table) was mostly borrowed
   from Scheme48. The implementation is all my own fault.. */

#define _GNU_SOURCE

#include "repint.h"
#include <string.h>
#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

typedef unsigned rep_PTR_SIZED_INT hash_value;

typedef struct node_struct node;
struct node_struct {
    node *next;
    repv key, value;
    hash_value hash;
};

typedef struct table_struct table;
struct table_struct {
    repv car;
    table *next;
    int total_buckets, total_nodes;
    node **buckets;
    repv hash_fun;
    repv compare_fun;
    repv guardian;			/* non-null if a weak table */
};

#define TABLEP(v) rep_CELL16_TYPEP(v, table_type)
#define TABLE(v)  ((table *) rep_PTR(v))

static int table_type;
static table *all_tables;

/* ensure X is +ve and in an int */
#define TRUNC(x) (((x) << (rep_VALUE_INT_SHIFT+1)) >> (rep_VALUE_INT_SHIFT+1))


/* type hooks */

static void
table_mark (repv val)
{
    int i;
    for (i = 0; i < TABLE(val)->total_buckets; i++)
    {
	node *n;
	for (n = TABLE(val)->buckets[i]; n != 0; n = n->next)
	{
	    if (!TABLE(val)->guardian)
		rep_MARKVAL(n->key);
	    rep_MARKVAL(n->value);
	}
    }
    rep_MARKVAL(TABLE(val)->hash_fun);
    rep_MARKVAL(TABLE(val)->compare_fun);
    rep_MARKVAL(TABLE(val)->guardian);
}

static void
free_table (table *x)
{
    int i;
    for (i = 0; i < x->total_buckets; i++)
    {
	node *n, *next;
	for (n = x->buckets[i]; n != 0; n = next)
	{
	    next = n->next;
	    rep_free (n);
	}
    }
    if (x->total_buckets > 0)
	rep_free (x->buckets);
    rep_FREE_CELL (x);
}

static void
table_sweep (void)
{
    table *x = all_tables;
    all_tables = 0;
    while (x != 0)
    {
	table *next = x->next;
	if (!rep_GC_CELL_MARKEDP (rep_VAL(x)))
	    free_table (x);
	else
	{
	    rep_GC_CLR_CELL (rep_VAL(x));
	    x->next = all_tables;
	    all_tables = x;
	}
	x = next;
    }
}

static void
table_print (repv stream, repv arg)
{
    rep_stream_puts (stream, "#<table ", -1, rep_FALSE);
    rep_princ_val (stream, TABLE(arg)->hash_fun);
    rep_stream_putc (stream, ' ');
    rep_princ_val (stream, TABLE(arg)->compare_fun);
    rep_stream_putc (stream, '>');
}


/* hash functions */

static inline hash_value
hash_string (register char *ptr)
{
    register hash_value value = 0;
    while (*ptr != 0)
	value = (value * 33) + *ptr++;
    return rep_MAKE_INT (TRUNC (value));
}

DEFUN("string-hash", Fstring_hash, Sstring_hash, (repv string), rep_Subr1) /*
::doc:rep.data.tables#string-hash::
string-hash STRING

Return a positive fixnum somehow related to the contents of STRING,
such that (string= X Y) implies (= (string-hash X) (string-hash Y)).
::end:: */
{
    rep_DECLARE1(string, rep_STRINGP);
    return hash_string (rep_STR (string));
}

DEFUN("symbol-hash", Fsymbol_hash, Ssymbol_hash, (repv sym), rep_Subr1) /*
::doc:rep.data.tables#symbol-hash::
symbol-hash SYMBOL

Return a positive fixnum somehow related to the name of SYMBOL.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    return hash_string (rep_STR (rep_SYM (sym)->name));
}

DEFUN("eq-hash", Feq_hash, Seq_hash, (repv value), rep_Subr1) /*
::doc:rep.data.tables#eq-hash::
eq-hash ARG

Return a positive fixnum somehow related to object ARG, such that (eq X
Y) implies (= (eq-hash X) (eq-hash Y)).
::end:: */
{
    hash_value hv = value;
    return rep_MAKE_INT (TRUNC (hv));
}

/* XXX This is probably _very_ sub-optimal.. */
DEFUN("equal-hash", Fequal_hash, Sequal_hash, (repv x, repv n_), rep_Subr2) /*
::doc:rep.data.tables#equal-hash::
equal-hash ARG

Return a positive fixnum somehow related to ARG, such that (equal X Y)
implies (= (equal-hash X) (equal-hash Y)).
::end:: */
{
    int n = rep_INTP (n_) ? rep_INT (n_) : rep_PTR_SIZED_INT_BITS / 2;
    if (rep_CONSP (x))
    {
	if (n > 0)
	{
	    repv left = Fequal_hash (rep_CAR(x), rep_MAKE_INT (n/2));
	    repv right = Fequal_hash (rep_CDR(x), rep_MAKE_INT (n/2));
	    return rep_MAKE_INT ((rep_INT (left) << 1) + rep_INT (right));
	}
	else
	    return rep_MAKE_INT (rep_Cons);
    }
    else if (rep_VECTORP (x) || rep_COMPILEDP (x))
    {
	hash_value hash = 0;
	int i = MIN (n, rep_VECT_LEN (x));
	while (i-- > 0)
	{
	    repv tem = Fequal_hash (rep_VECTI (x, i), rep_MAKE_INT (n/2));
	    hash = hash * 33 + rep_INT (tem);
	}
	return rep_MAKE_INT (TRUNC (hash));
    }
    else if (rep_STRINGP (x))
	return Fstring_hash (x);
    else if (rep_SYMBOLP (x))
	return Fsymbol_hash (x);
    else if (rep_INTP (x))
    {
	hash_value hash = rep_INT (x);
	return rep_MAKE_INT (TRUNC (hash));
    }
    else if (rep_NUMBERP (x))
    {
	hash_value hash = rep_get_long_uint (x);
	return rep_MAKE_INT (TRUNC (hash));
    }
    else
	return rep_MAKE_INT (rep_TYPE (x) * 255);
}


/* table functions */

DEFUN("make-table", Fmake_table, Smake_table,
      (repv hash_fun, repv cmp_fun, repv is_weak), rep_Subr3) /*
::doc:rep.data.tables#make-table::
make-table HASH-FUNCTION COMPARE-FUNCTION

Create and return a new hash table. When storing and referencing keys
it will use the function HASH-FUNCTION to map keys to hash codes
(positive fixnums), and the predicate function COMPARE-FUNCTION to
compare two keys (should return true if the keys are considered equal).
::end:: */
{
    table *tab;
    rep_DECLARE(1, hash_fun, Ffunctionp (hash_fun) != Qnil);
    rep_DECLARE(2, cmp_fun, Ffunctionp (cmp_fun) != Qnil);

    tab = rep_ALLOC_CELL (sizeof (table));
    rep_data_after_gc += sizeof (table);
    tab->car = table_type;
    tab->next = all_tables;
    all_tables = tab;
    tab->hash_fun = hash_fun;
    tab->compare_fun = cmp_fun;
    tab->total_buckets = 0;
    tab->total_nodes = 0;
    tab->guardian = (is_weak == Qnil) ? rep_NULL : Fmake_primitive_guardian ();

    return rep_VAL(tab);
}

DEFUN("make-weak-table", Fmake_weak_table, Smake_weak_table,
      (repv hash_fun, repv cmp_fun), rep_Subr2) /*
::doc:rep.data.tables#make-weak-table::
make-weak-table HASH-FUNCTION COMPARE-FUNCTION

Similar to `make-table, except that key-value pairs stored in the table
are said to be ``weakly keyed''. That is, they are only retained in the
table as long the key has not been garbage collected.

Unlike with tables created by the `make-table function, the fact that
the key is stored in the table is not considered good enough to prevent
it being garbage collected.
::end:: */
{
    return Fmake_table (hash_fun, cmp_fun, Qt);
}

DEFUN("tablep", Ftablep, Stablep, (repv arg), rep_Subr1) /*
::doc:rep.data.tables#tablep::
tablep ARG

Return true if ARG is a hash table.
::end:: */
{
    return TABLEP(arg) ? Qt : Qnil;
}

static hash_value
hash_key (repv tab, repv key)
{
    repv hash;
    if (TABLE(tab)->hash_fun == rep_VAL(&Sstring_hash))
	hash = Fstring_hash (key);
    else if (TABLE(tab)->hash_fun == rep_VAL(&Ssymbol_hash))
	hash = Fsymbol_hash (key);
    else if (TABLE(tab)->hash_fun == rep_VAL(&Seq_hash))
	hash = Feq_hash (key);
    else if (TABLE(tab)->hash_fun == rep_VAL(&Sequal_hash))
	hash = Fequal_hash (key, Qnil);
    else
    {
	rep_GC_root gc_tab;
	rep_PUSHGC (gc_tab, tab);
	hash = rep_call_lisp1 (TABLE(tab)->hash_fun, key);
	rep_POPGC;
    }
    return rep_INT(hash);
}

static inline int
hash_key_to_bin (repv tab, hash_value hash)
{
    return hash % TABLE(tab)->total_buckets;
}

static inline rep_bool
compare (repv tab, repv val1, repv val2)
{
    repv ret;
    rep_GC_root gc_tab;
    rep_PUSHGC (gc_tab, tab);
    ret = rep_call_lisp2 (TABLE(tab)->compare_fun, val1, val2);
    rep_POPGC;
    return ret != Qnil;
}

static node *
lookup (repv tab, repv key)
{
    hash_value hv;
    node *ptr;
    int index;
    if (TABLE(tab)->total_buckets == 0)
	return 0;
    hv = hash_key (tab, key);
    index = hash_key_to_bin (tab, hv);
    for (ptr = TABLE(tab)->buckets[index]; ptr != 0; ptr = ptr->next)
    {
	if (ptr->hash == hv && compare (tab, key, ptr->key))
	    return ptr;
    }
    return 0;
}

DEFUN("table-ref", Ftable_ref, Stable_ref, (repv tab, repv key), rep_Subr2) /*
::doc:rep.data.tables#table-ref::
table-ref TABLE KEY

Return the value stored in hash table TABLE indexed by object KEY.
Returns false if no such value exists.
::end:: */
{
    node *n;
    rep_DECLARE1(tab, TABLEP);
    n = lookup (tab, key);
    return n ? n->value : Qnil;
}

DEFUN("table-bound-p", Ftable_bound_p,
      Stable_bound_p, (repv tab, repv key), rep_Subr2) /*
::doc:rep.data.tables#table-bound-p::
table-bound-p TABLE KEY

Returns true if the hash table TABLE contains a value associated with
KEY.
::end:: */
{
    node *n;
    rep_DECLARE1(tab, TABLEP);
    n = lookup (tab, key);
    return n ? Qt : Qnil;
}

DEFUN("table-set", Ftable_set, Stable_set,
      (repv tab, repv key, repv value), rep_Subr3) /*
::doc:rep.data.tables#table-set::
table-set TABLE KEY VALUE

Associate VALUE with KEY in hash table TABLE. Returns VALUE.
::end:: */
{
    node *n;
    rep_DECLARE1(tab, TABLEP);
    n = lookup (tab, key);
    if (n == 0)
    {
	int bin;
	n = rep_alloc (sizeof (node));
	rep_data_after_gc += sizeof (node);
	n->key = key;
	n->value = value;
	n->hash = hash_key (tab, key);
	TABLE(tab)->total_nodes++;
	if (TABLE(tab)->total_nodes >= 2 * TABLE(tab)->total_buckets)
	{
	    int old_size, new_size, i;
	    node **new_bins, **old_bins;

	    old_bins = TABLE(tab)->buckets;
	    old_size = TABLE(tab)->total_buckets;

	    /* The (misguided?) idea is to set number of buckets as
	        (2^N) - 1, then increase N each time we get twice as
		many keys as buckets. Start at N=5 */

	    if (old_size == 0)
		new_size = 31;
	    else
		new_size = (old_size + 1) * 2 - 1;

	    new_bins = rep_alloc (sizeof (node *) * new_size);
	    rep_data_after_gc += sizeof (node *) * new_size;
	    memset (new_bins, 0, sizeof (node *) * new_size);

	    TABLE(tab)->buckets = new_bins;
	    TABLE(tab)->total_buckets = new_size;
	    for (i = 0; i < old_size; i++)
	    {
		node *ptr, *next;
		for (ptr = old_bins[i]; ptr != 0; ptr = next)
		{
		    int index = hash_key_to_bin (tab, ptr->hash);
		    next = ptr->next;
		    ptr->next = new_bins[index];
		    new_bins[index] = ptr;
		}
	    }

	    if (old_size > 0)
		rep_free (old_bins);
	}
	bin = hash_key_to_bin (tab, n->hash);
	n->next = TABLE(tab)->buckets[bin];
	TABLE(tab)->buckets[bin] = n;
	if (TABLE(tab)->guardian)
	    Fprimitive_guardian_push (TABLE(tab)->guardian, n->key);
    }
    n->value = value;
    return value;
}

DEFUN("table-unset", Ftable_unset, Stable_unset,
      (repv tab, repv key), rep_Subr2) /*
::doc:rep.data.tables#table-unset::
table-unset TABLE KEY

Remove any value stored in TABLE associated with KEY.
::end:: */
{
    node *n;
    rep_DECLARE1(tab, TABLEP);
    n = lookup (tab, key);
    if (n != 0)
    {
	int bin = hash_key_to_bin (tab, n->hash);
	node **ptr;
	for (ptr = &(TABLE(tab)->buckets[bin]);
	     *ptr != 0; ptr = &((*ptr)->next))
	{
	    if (*ptr == n)
	    {
		*ptr = n->next;
		rep_free (n);
		TABLE(tab)->total_nodes--;
		return Qt;
	    }
	}
    }
    return Qnil;
}

DEFUN("table-walk", Ftable_walk, Stable_walk,
      (repv fun, repv tab), rep_Subr2) /*
::doc:rep.data.tables#table-walk::
table-walk FUNCTION TABLE

Call FUNCTION for every key-value pair stored in hash table TABLE. For
each pair, the function is called with arguments `(KEY VALUE)'.
::end:: */
{
    rep_GC_root gc_tab, gc_fun;
    int i;

    rep_DECLARE1(tab, TABLEP);
    rep_PUSHGC (gc_tab, tab);
    rep_PUSHGC (gc_fun, fun);

    for (i = 0; i < TABLE(tab)->total_buckets; i++)
    {
	node *n;
	for (n = TABLE(tab)->buckets[i]; n != 0; n = n->next)
	{
	    if (!rep_call_lisp2 (fun, n->key, n->value))
		break;
	}
    }

    rep_POPGC; rep_POPGC;
    return rep_throw_value ? rep_NULL : Qnil;
}

DEFUN ("table-size", Ftable_size, Stable_size,
       (repv tab), rep_Subr1) /*
::doc:rep.data.tables#table-size::
table-size TABLE

Returns the number of items currently stored in TABLE.
::end:: */
{
    rep_DECLARE1 (tab, TABLEP);
    return rep_make_long_int (TABLE (tab)->total_nodes);
}

DEFUN("tables-after-gc", Ftables_after_gc, Stables_after_gc, (void), rep_Subr0)
{
    table *x;
    for (x = all_tables; x != 0; x = x->next)
    {
	if (x->guardian)
	{
	    repv key;
	    while ((key = Fprimitive_guardian_pop (x->guardian)) != Qnil)
	    {
		rep_GC_root gc_key;
		rep_PUSHGC (gc_key, key);
		Ftable_unset (rep_VAL (x), key);
		rep_POPGC;
	    }
	}
    }
    return Qnil;
}


/* dl hooks */

repv
rep_dl_init (void)
{
    repv tem;
    table_type = rep_register_new_type ("table", 0, table_print, table_print,
					table_sweep, table_mark,
					0, 0, 0, 0, 0, 0, 0);
    tem = Fsymbol_value (Qafter_gc_hook, Qt);
    if (rep_VOIDP (tem))
	tem = Qnil;
    Fset (Qafter_gc_hook, Fcons (rep_VAL(&Stables_after_gc), tem));

    tem = rep_push_structure ("rep.data.tables");
    /* ::alias:tables rep.data.tables:: */
    rep_alias_structure ("tables");
    rep_ADD_SUBR(Smake_table);
    rep_ADD_SUBR(Smake_weak_table);
    rep_ADD_SUBR(Sstring_hash);
    rep_ADD_SUBR(Ssymbol_hash);
    rep_ADD_SUBR(Seq_hash);
    rep_ADD_SUBR(Sequal_hash);
    rep_ADD_SUBR(Stablep);
    rep_ADD_SUBR(Stable_ref);
    rep_ADD_SUBR(Stable_bound_p);
    rep_ADD_SUBR(Stable_set);
    rep_ADD_SUBR(Stable_unset);
    rep_ADD_SUBR(Stable_walk);
    rep_ADD_SUBR(Stable_size);
    rep_ADD_INTERNAL_SUBR(Stables_after_gc);
    return rep_pop_structure (tem);
}
