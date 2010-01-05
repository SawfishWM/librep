/* structures.c -- rep's module system
   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Uncomment the next line to print cache miss ratios */
/* #define DEBUG 1 */

/* The cache type */
#define SINGLE_SA_CACHE 1

/* Notes:

   rep's module system is based on the Scheme48 system, which itself
   takes ideas from Standard ML and Xerox scheme.

   Modules are known as structures (from SML) and may be anonymous or
   named (as with functions, but in a separate namespace), but only
   named structures may be imported or accessed. Each structure is
   basically a separate global namespace, with a number of variable
   bindings. Each closure contains a reference to the structure it was
   instantiated in, providing the source for referencing any unbound
   variables.

   Each structure presents an interface to any structures that import
   its bindings. This interface is simply the list of symbols whose
   bindings may be referenced from outside.

   Structures may either `open' or `access' other structures; when
   opening a structure all its exported bindings are immediately
   referenceable from the importing structures. Exported bindings from
   accessed structures are referenced using the `structure-ref' form

   Structures are implemented as first-class objects, but only a second-
   class view is presented to most lisp code, this is to enable static
   analysis of package imports and exports at compile time

   Here is the module language grammar adapted from Rees' memo:

   <definition> -> (define-structure <name> <interface> <config> <form>*)
		   (define-interface <name> <interface>)

   <structure> -> (structure <interface> <config> <form>*)

   <interface> -> (export <id>*)
		  <name>
		  (compound-interface <interface>*)

   <config> -> (<clause>*)
	       <clause>

   <clause> -> (open <name>*)
	       (access <name>*)

   Most files will just contain a single `(define-structure ...)' form.
   E.g.:

   (define-structure foo (export foo) (open rep)
     (defun foo (x)
       (1+ x)))

   As Rees points out, this changes load from being used for its side
   effects to being used for its value, the created structure.

   For backwards compatibility, the `require' form now works with both
   simple files and files containing module definitions. E.g. if a file
   called `foo.jl' contains the above example, then doing "(require
   'foo)" would open the module in the current environment.

   Special variables have their own isolated namespace (the structure
   called `%specials') and thus their names can still clash across
   structures..  */

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
#include <assert.h>
#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

#define MIN_BUCKETS 8
#define MAX_MULTIPLIER 2

int rep_structure_type;
static rep_struct *all_structures;

#define rep_INTERFACEP(v) rep_LISTP(v)

/* the currently active namespace */
repv rep_structure;

/* the `default' namespace, where all rep language bindings go */
repv rep_default_structure;

/* the namespace for special variables */
repv rep_specials_structure;

/* the structure namespace */
static repv rep_structures_structure;

DEFSYM(features, "features");
DEFSYM(_structures, "%structures");
DEFSYM(_meta, "%meta");
DEFSYM(rep, "rep");
DEFSYM(_specials, "%specials");
DEFSYM(_user_structure_, "*user-structure*");
DEFSYM(rep_structures, "rep.structures");
DEFSYM(rep_lang_interpreter, "rep.lang.interpreter");
DEFSYM(rep_vm_interpreter, "rep.vm.interpreter");
DEFSYM(external, "external");
DEFSYM(local, "local");

static rep_struct_node *lookup_or_add (rep_struct *s, repv var);


/* cached lookups */

#ifdef DEBUG
/* Hits and misses are obvious. Collisions occur when a miss ejects data
   from the cache, conflicts when a miss ejects data for the _same_ symbol. */
static int ref_cache_hits, ref_cache_misses,
    ref_cache_collisions, ref_cache_conflicts;

static void
print_cache_stats (void)
{
    fprintf (stderr, "ref cache miss ratio: %g\n",
	     (double) ref_cache_misses / (ref_cache_hits + ref_cache_misses));
    fprintf (stderr, "        - collisions: %g\n",
	     (double) ref_cache_collisions / ref_cache_misses);
    fprintf (stderr, "        -  conflicts: %g\n",
	     (double) ref_cache_conflicts / ref_cache_misses);
}
#endif

#if defined SINGLE_DM_CACHE

/* This is a very simple cache; a single direct-mapped table, indexed by
   symbol address */

#define CACHE_SETS 256
#define CACHE_HASH(x) (((x) >> 4) % CACHE_SETS)

struct cache_line {
    rep_struct *s;
    rep_struct_node *n;
};

static struct cache_line ref_cache[CACHE_SETS];

static inline void
enter_cache (rep_struct *s, rep_struct_node *binding)
{
    unsigned int hash = CACHE_HASH (binding->symbol);
    if (ref_cache[hash].s != 0)
    {
#ifdef DEBUG
	if (ref_cache[hash].n->symbol == binding->symbol)
	    ref_cache_conflicts++;
	else
	    ref_cache_collisions++;
#endif
    }
    ref_cache[hash].s = s;
    ref_cache[hash].n = binding;
}

static inline rep_struct_node *
lookup_cache (rep_struct *s, repv var)
{
    unsigned int hash = CACHE_HASH (var);
    if (ref_cache[hash].s == s && ref_cache[hash].n->symbol == var)
    {
#ifdef DEBUG
	ref_cache_hits++;
#endif
	return ref_cache[hash].n;
    }
    else
    {
#ifdef DEBUG
	ref_cache_misses++;
#endif
	return 0;
    }
}

static inline void
cache_invalidate_symbol (repv symbol)
{
    unsigned int hash = CACHE_HASH (symbol);
    if (ref_cache[hash].s != 0 && ref_cache[hash].n->symbol == symbol)
	ref_cache[hash].s = 0;
}

static void
cache_invalidate_struct (rep_struct *s)
{
    int i;
    for (i = 0; i < CACHE_SETS; i++)
    {
	if (ref_cache[i].s == s)
	    ref_cache[i].s = 0;
    }
}

static inline void
cache_flush (void)
{
    /* assumes null pointer == all zeros.. */
    memset (ref_cache, 0, sizeof (ref_cache));
}

#elif defined SINGLE_SA_CACHE 

/* The above doesn't work so well now that there are more modules,
   moving to 4-way set-associative eliminates significant conflict
   misses in most cases. */

#define CACHE_SETS 128
#define CACHE_HASH(x) (((x) >> 3) % CACHE_SETS)
#define CACHE_ASSOC 4

struct cache_line {
    rep_struct *s;
    rep_struct_node *n;
    int age;
};

static struct cache_line ref_cache[CACHE_SETS][CACHE_ASSOC];
static int ref_age;

static inline void
enter_cache (rep_struct *s, rep_struct_node *binding)
{
    unsigned int hash = CACHE_HASH (binding->symbol);
    int i, oldest_i, oldest_age = INT_MAX;
    for (i = 0; i < CACHE_ASSOC; i++)
    {
	if (ref_cache[hash][i].s == 0)
	{
	    oldest_i = i;
	    break;
	}
	else if (ref_cache[hash][i].age < oldest_age)
	{
	    oldest_i = i;
	    oldest_age = ref_cache[hash][i].age;
	}
    }
    assert (oldest_i < CACHE_ASSOC);
#ifdef DEBUG
    if (ref_cache[hash][oldest_i].s != 0)
    {
	if (ref_cache[hash][oldest_i].n->symbol == binding->symbol)
	    ref_cache_conflicts++;
	else
	    ref_cache_collisions++;
    }
#endif
    ref_cache[hash][oldest_i].s = s;
    ref_cache[hash][oldest_i].n = binding;
    ref_cache[hash][oldest_i].age = ++ref_age;
}

static inline rep_struct_node *
lookup_cache (rep_struct *s, repv var)
{
    unsigned int hash = CACHE_HASH (var);
    int i;
    for (i = 0; i < CACHE_ASSOC; i++)
    {
	if (ref_cache[hash][i].s == s && ref_cache[hash][i].n->symbol == var)
	{
#ifdef DEBUG
	    ref_cache_hits++;
#endif
	    ref_cache[hash][i].age++;
	    return ref_cache[hash][i].n;
	}
    }
#ifdef DEBUG
    ref_cache_misses++;
#endif
    return 0;
}

static inline void
cache_invalidate_symbol (repv symbol)
{
    unsigned int hash = CACHE_HASH (symbol);
    int i;
    for (i = 0; i < CACHE_ASSOC; i++)
    {
	if (ref_cache[hash][i].s != 0
	    && ref_cache[hash][i].n->symbol == symbol)
	{
	    ref_cache[hash][i].s = 0;
	}
    }
}

static void
cache_invalidate_struct (rep_struct *s)
{
    int i, j;
    for (i = 0; i < CACHE_SETS; i++)
    {
	for (j = 0; j < CACHE_ASSOC; j++)
	{
	    if (ref_cache[i][j].s == s)
		ref_cache[i][j].s = 0;
	}
    }
}

static inline void
cache_flush (void)
{
    /* assumes null pointer == all zeros.. */
    memset (ref_cache, 0, sizeof (ref_cache));
}

#else /* SINGLE_SA_CACHE */

/* no cache at all */

static inline void
enter_cache (rep_struct *s, rep_struct_node *binding)
{
}

static inline rep_struct_node *
lookup_cache (rep_struct *s, repv var)
{
#ifdef DEBUG
    ref_cache_misses++;
#endif
    return 0;
}

static inline void
cache_invalidate_symbol (repv symbol)
{
}

static void
cache_invalidate_struct (rep_struct *s)
{
}

static void
cache_flush (void)
{
}

#endif /* !SINGLE_DM_CACHE */


/* type hooks */

static void
structure_mark (repv x)
{
    int i;
    for (i = 0; i < rep_STRUCTURE(x)->total_buckets; i++)
    {
	rep_struct_node *n;
	for (n = rep_STRUCTURE(x)->buckets[i]; n != 0; n = n->next)
	{
	    rep_MARKVAL(n->symbol);
	    rep_MARKVAL(n->binding);
	}
    }
    rep_MARKVAL (rep_STRUCTURE (x)->name);
    rep_MARKVAL (rep_STRUCTURE (x)->inherited);
    rep_MARKVAL (rep_STRUCTURE (x)->imports);
    rep_MARKVAL (rep_STRUCTURE (x)->accessible);
    rep_MARKVAL (rep_STRUCTURE (x)->special_env);
}

static void
free_structure (rep_struct *x)
{
    int i;
    cache_invalidate_struct (x);
    for (i = 0; i < x->total_buckets; i++)
    {
	rep_struct_node *n, *next;
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
structure_sweep (void)
{
    rep_struct *x = all_structures;
    all_structures = 0;
    while (x != 0)
    {
	rep_struct *next = x->next;
	if (!rep_GC_CELL_MARKEDP (rep_VAL(x)))
	    free_structure (x);
	else
	{
	    rep_GC_CLR_CELL (rep_VAL(x));
	    x->next = all_structures;
	    all_structures = x;
	}
	x = next;
    }
}

static void
structure_print (repv stream, repv arg)
{
    if (rep_STRUCTURE (arg)->name == Qnil)
	rep_stream_puts (stream, "#<structure>", -1, rep_FALSE);
    else
    {
	rep_stream_puts (stream, "#<structure ", -1, rep_FALSE);
	rep_princ_val (stream, rep_STRUCTURE(arg)->name);
	rep_stream_putc (stream, '>');
    }
}


/* utilities */

/* Return true iff structure S exports a binding of symbol VAR that it
   inherits from one of its opened structures */
static rep_bool
structure_exports_inherited_p (rep_struct *s, repv var)
{
    if (s->car & rep_STF_EXPORT_ALL)
	return rep_TRUE;
    else
    {
	repv tem = s->inherited;
	while (rep_CONSP (tem))
	{
	    if (rep_CAR (tem) == var)
		return rep_TRUE;
	    tem = rep_CDR (tem);
	}
	return rep_FALSE;
    }
}

/* Scan for an immediate binding of symbol VAR in structure S, or return
   a null pointer if no such binding */
static inline rep_struct_node *
lookup (rep_struct *s, repv var)
{
    /* this is also in OP_REFG in lispmach.c */

    rep_struct_node *n;
    if (s->total_buckets != 0)
    {
	for (n = s->buckets[rep_STRUCT_HASH (var, s->total_buckets)];
	     n != 0; n = n->next)
	{
	    if (n->symbol == var)
		return n;
	}
    }
    return 0;
}

static rep_struct_node *
lookup_or_add (rep_struct *s, repv var)
{
    rep_struct_node *n = lookup (s, var);
    if (n == 0)
    {
	if (s->total_buckets == 0)
	{
	    s->total_buckets = MIN_BUCKETS;
	    s->buckets = rep_alloc (sizeof (rep_struct_node *)
				    * s->total_buckets);
	    memset (s->buckets, 0,
		    sizeof (rep_struct_node *) * s->total_buckets);
	    rep_data_after_gc += sizeof (rep_struct_node *) * s->total_buckets;
	}

	if (s->total_bindings > s->total_buckets * MAX_MULTIPLIER)
	{
	    int new_total = s->total_buckets * 2;
	    rep_struct_node **buckets
		= rep_alloc (new_total * sizeof (rep_struct_node *));
	    int i;
	    memset (buckets, 0, new_total * sizeof (rep_struct_node *));
	    rep_data_after_gc += new_total * sizeof (rep_struct_node *);
	    for (i = 0; i < s->total_buckets; i++)
	    {
		rep_struct_node *next;
		for (n = s->buckets[i]; n != 0; n = next)
		{
		    next = n->next;
		    n->next = buckets[rep_STRUCT_HASH (n->symbol, new_total)];
		    buckets[rep_STRUCT_HASH (n->symbol, new_total)] = n;
		}
	    }
	    s->total_buckets = new_total;
	    rep_free (s->buckets);
	    s->buckets = buckets;
	}

	n = rep_alloc (sizeof (rep_struct_node));
	rep_data_after_gc += sizeof (rep_struct_node);
	n->symbol = var;
	n->is_constant = 0;
	n->is_exported = (s->car & rep_STF_EXPORT_ALL) != 0;
	n->next = s->buckets[rep_STRUCT_HASH (var, s->total_buckets)];
	s->buckets[rep_STRUCT_HASH (var, s->total_buckets)] = n;
	s->total_bindings++;

	if (structure_exports_inherited_p (s, var))
	{
	    n->is_exported = 1;
	    s->inherited = Fdelq (var, s->inherited);
	}

	cache_invalidate_symbol (var);
    }
    return n;
}

static void
remove_binding (rep_struct *s, repv var)
{
    if (s->total_buckets != 0)
    {
	rep_struct_node **n;
	for (n = &(s->buckets[rep_STRUCT_HASH (var, s->total_buckets)]);
	     *n != 0; n = &((*n)->next))
	{
	    if ((*n)->symbol == var)
	    {
		rep_struct_node *next = (*n)->next;
		rep_free (*n);
		*n = next;
		cache_invalidate_symbol (var);
		return;
	    }
	}
    }
}

/* Scan for a binding of symbol VAR under structure S, or return null. This
   also searches the exports of any structures that S has opened */
static rep_struct_node *
lookup_recursively (repv s, repv var)
{
    if (rep_SYMBOLP (s))
	s = Fget_structure (s);
    if (s && rep_STRUCTUREP (s)
	&& !(rep_STRUCTURE (s)->car & rep_STF_EXCLUSION))
    {
	rep_struct_node *n;
	n = lookup (rep_STRUCTURE (s), var);
	if (n != 0)
	    return n->is_exported ? n : 0;
	rep_STRUCTURE (s)->car |= rep_STF_EXCLUSION;
	if (structure_exports_inherited_p (rep_STRUCTURE (s), var))
	    n = rep_search_imports (rep_STRUCTURE (s), var);
	rep_STRUCTURE (s)->car &= ~rep_STF_EXCLUSION;
	return n;
    }
    else
	return 0;
}

rep_struct_node *
rep_search_imports (rep_struct *s, repv var)
{
    rep_struct_node *n = lookup_cache (s, var);
    if (n != 0)
	return n;
    else
    {
	repv imports = s->imports;
	while (rep_CONSP (imports))
	{
	    n = lookup_recursively (rep_CAR (imports), var);
	    if (n != 0)
	    {
		enter_cache (s, n);
		return n;
	    }
	    imports = rep_CDR (imports);
	}
	return 0;
    }
}


/* lisp functions */

DEFUN("get-structure", Fget_structure,
      Sget_structure, (repv name), rep_Subr1) /*
::doc:rep.structures#get-structure::
get-structure NAME

Return the structure called NAME (a symbol), or return `nil' if no
such structure.
::end:: */
{
    rep_struct_node *n;
    rep_DECLARE1 (name, rep_SYMBOLP);
    n = lookup (rep_STRUCTURE (rep_structures_structure), name);
    return n ? n->binding : Qnil;
}

DEFUN("name-structure", Fname_structure,
      Sname_structure, (repv structure, repv name), rep_Subr2) /*
::doc:rep.structures#name-structure::
name-structure STRUCTURE NAME

Assign the name NAME (a symbol) to structure object STRUCTURE.
::end:: */
{
    rep_DECLARE1 (structure, rep_STRUCTUREP);
    if (name != Qnil)
    {
	rep_DECLARE2 (name, rep_SYMBOLP);
	Fstructure_define (rep_structures_structure, name, structure);

	/* XXX I'm not sure about this..? */
	if (rep_STRUCTURE (structure)->name == Qnil)
	    rep_STRUCTURE (structure)->name = name;
    }
    else if (rep_STRUCTURE (structure)->name != Qnil)
    {
	/* remove the name->structure relation */
	Fstructure_define (rep_structures_structure,
			   rep_STRUCTURE (structure)->name, Qnil);
    }
    cache_flush ();
    return name;
}

/* environment of thunks are modified! */
DEFUN ("make-structure", Fmake_structure, Smake_structure,
       (repv sig, repv header_thunk, repv body_thunk, repv name), rep_Subr4) /*
::doc:rep.structures#make-structure::
make-structure INTERFACE CONFIG-THUNK BODY-THUNK [NAME]

Create and return a new structure. If NAME is a non-nil symbol the
structure will take that name.

The new structure will be advertised as exporting bindings defined by
INTERFACE (currently just a list of symbols).

If CONFIG-THUNK is non-nil it is a zero-parameter function to be called
to define the configuration of the structure (currently it's opened and
accessed structures.) This thunk will be evaluated in the environment
of the new structure, but with only the `%meta' (module-configuration)
structure opened.

If BODY-THUNK is non-nil it is a zero-parameter function to be called
to define the values of the bindings exported by the structure. It will
be evaluated in the environment of the new structure.

Note that the captured state of the closures CONFIG-THUNK and
BODY-THUNK may be modified by this function!
::end:: */
{
    rep_struct *s;
    repv s_;
    rep_GC_root gc_s;
    rep_GC_root gc_body;

    rep_DECLARE1 (sig, rep_INTERFACEP);
    if (header_thunk != Qnil)
	rep_DECLARE2 (header_thunk, rep_FUNARGP);
    if (body_thunk != Qnil)
	rep_DECLARE3 (body_thunk, rep_FUNARGP);
    if (name != Qnil)
	rep_DECLARE4 (name, rep_SYMBOLP);

    s = rep_ALLOC_CELL (sizeof (rep_struct));
    rep_data_after_gc += sizeof (rep_struct);
    s->car = rep_structure_type;
    s->inherited = sig;
    s->name = name;
    s->total_buckets = s->total_bindings = 0;
    s->imports = Qnil;
    s->accessible = Qnil;
    s->special_env = Qt;
    if (rep_structure != rep_NULL)
	s->apply_bytecode = rep_STRUCTURE (rep_structure)->apply_bytecode;
    else
	s->apply_bytecode = 0;
    s->next = all_structures;
    all_structures = s;

    s_ = rep_VAL (s);
    rep_PUSHGC (gc_s, s_);

    if (s->name != Qnil)
	Fname_structure (rep_VAL (s), s->name);

    rep_PUSHGC (gc_body, body_thunk);
    if (header_thunk != Qnil)
    {
	repv tem;
	s->imports = Fcons (Q_meta, s->imports);
	rep_FUNARG (header_thunk)->structure = s_;
	tem = rep_call_lisp0 (header_thunk);
	s->imports = Fdelq (Q_meta, s->imports);
	if (tem == rep_NULL)
	    s = 0;
    }
    rep_POPGC;

    if (s != 0 && body_thunk != Qnil)
    {
	repv tem;
	rep_FUNARG (body_thunk)->structure = s_;
	tem = rep_call_lisp0 (body_thunk);
	if (tem == rep_NULL)
	    s = 0;
    }
    rep_POPGC;

    if (s != 0)
	return rep_VAL (s);
    else
    {
	/* initialization failed. */
	s = rep_STRUCTURE (s_);
	if (s->name != Qnil)
	    Fname_structure (rep_VAL (s), Qnil);
	return rep_NULL;
    }
}

DEFUN ("%structure-ref", F_structure_ref,
       S_structure_ref, (repv structure, repv var), rep_Subr2) /*
::doc:rep.structures#%structure-ref::
%structure-ref STRUCTURE VAR

Return the value of the binding of symbol VAR in structure object
STRUCTURE or any inner opened structures.

Returns a void value if no such binding.
::end::*/
{
    rep_struct *s;
    rep_struct_node *n;

    rep_DECLARE1 (structure, rep_STRUCTUREP);
    rep_DECLARE2 (var, rep_SYMBOLP);
    s = rep_STRUCTURE (structure);

    /* this is also in OP_REFG in lispmach.c */

    n = lookup (s, var);
    if (n == 0)
	n = rep_search_imports (s, var);
    return (n != 0) ? n->binding : rep_void_value;
}

DEFUN ("structure-bound-p", Fstructure_bound_p,
       Sstructure_bound_p, (repv structure, repv var), rep_Subr2) /*
::doc:rep.structures#structure-bound-p::
structure-bound-p STRUCTURE VAR

Return `t' if symbol VAR has a non-void binding in STRUCTURE.
::end:: */
{
    repv tem = F_structure_ref (structure, var);
    if (tem != rep_NULL)
	tem = rep_VOIDP (tem) ? Qnil : Qt;
    return tem;
}

DEFUN ("structure-set", Fstructure_set, Sstructure_set,
       (repv structure, repv var, repv value), rep_Subr3) /*
::doc:rep.structures#structure-set::
structure-set STRUCTURE VAR VALUE

Set the value of the binding of symbol VAR in structure object
STRUCTURE to VALUE. If no such binding exists, an error is signalled.
::end:: */
{
    rep_struct *s;
    rep_struct_node *n;

    rep_DECLARE1 (structure, rep_STRUCTUREP);
    rep_DECLARE2 (var, rep_SYMBOLP);

    s = rep_STRUCTURE (structure);

    if (!rep_VOIDP (value))
    {
	if (!(s->car & rep_STF_SET_BINDS))
	    n = lookup (s, var);
	else
	    n = lookup_or_add (s, var);
	if (n != 0)
	{
	    if (!n->is_constant)
	    {
		n->binding = value;
		return value;
	    }
	    else
		return Fsignal (Qsetting_constant, rep_LIST_1 (var));
	}
	else
	    return Fsignal(Qvoid_value, rep_LIST_1(var));
    }
    else
    {
	remove_binding (s, var);
	return Qnil;
    }
}

DEFUN ("structure-define", Fstructure_define, Sstructure_define,
       (repv structure, repv var, repv value), rep_Subr3) /*
::doc:rep.structures#structure-define::
structure-define STRUCTURE VAR VALUE

Set the value of the binding of symbol VAR in structure object
STRUCTURE to VALUE. If no such binding exists, one is created.
::end:: */
{
    rep_struct *s;
    rep_struct_node *n;

    rep_DECLARE1 (structure, rep_STRUCTUREP);
    rep_DECLARE2 (var, rep_SYMBOLP);

    s = rep_STRUCTURE (structure);

    if (!rep_VOIDP (value))
    {
	n = lookup_or_add (s, var);
	if (!n->is_constant)
	{
	    n->binding = value;
	    return value;
	}
	else
	    return Fsignal(Qsetting_constant, rep_LIST_1(var));
    }
    else
    {
	remove_binding (s, var);
	return Qnil;
    }
}

DEFUN ("external-structure-ref", Fexternal_structure_ref,
       Sexternal_structure_ref, (repv name, repv var), rep_Subr2) /*
::doc:rep.structures#external-structure-ref::
external-structure-ref STRUCT-NAME VAR

Return the value of the binding of symbol VAR within the structure
called STRUCT-NAME. This structure must have previously been marked as
accessible by the current structure (by using the `access' module
configuration directive).

Signals an error if no such binding exists.
::end:: */
{
    repv tem, val = rep_void_value;
    rep_DECLARE1 (name, rep_SYMBOLP);
    rep_DECLARE2 (var, rep_SYMBOLP);

    /* XXX caching here? */
    tem = Fmemq (name, rep_STRUCTURE (rep_structure)->accessible);
    if (tem == Qnil)
	tem = Fmemq (name, rep_STRUCTURE (rep_structure)->imports);
    if (tem && tem != Qnil)
    {
	rep_struct_node *n = lookup_recursively (name, var);
	if (n != 0)
	    val = n->binding;
    }
    if (!rep_VOIDP (val))
	return val;
    else
	return Fsignal (Qvoid_value, rep_LIST_1 (var));
}

DEFUN ("structure-name", Fstructure_name,
       Sstructure_name, (repv structure), rep_Subr1) /*
::doc:rep.structures#structure-name::
structure-name STRUCTURE

Returns the name of structure object STRUCTURE.
::end:: */
{
    rep_DECLARE1 (structure, rep_STRUCTUREP);
    return rep_STRUCTURE (structure)->name;
}

DEFUN ("structure-interface", Fstructure_interface,
       Sstructure_interface, (repv structure), rep_Subr1) /*
::doc:rep.structures#structure-interface::
structure-interface STRUCTURE

Returns the interface of structure object STRUCTURE.
::end:: */
{
    rep_struct *s;
    repv list;
    int i;
    rep_DECLARE1 (structure, rep_STRUCTUREP);
    s = rep_STRUCTURE (structure);
    list = s->inherited;
    for (i = 0; i < s->total_buckets; i++)
    {
	rep_struct_node *n;
	for (n = s->buckets[i]; n != 0; n = n->next)
	{
	    if (n->is_exported)
		list = Fcons (n->symbol, list);
	}
    }
    return list;
}

DEFUN ("structure-exports-p", Fstructure_exports_p,
       Sstructure_exports_p, (repv structure, repv var), rep_Subr2) /*
::doc:rep.structures#structure-exports-p::
structure-exports-p STRUCTURE VAR

Returns true if structure object STRUCTURE exports a binding of symbol
VAR.
::end:: */
{
    rep_struct_node *n;
    rep_DECLARE1 (structure, rep_STRUCTUREP);
    rep_DECLARE2 (var, rep_SYMBOLP);
    n = lookup (rep_STRUCTURE (structure), var);
    if (n != 0)
	return n->is_exported ? Qlocal : Qnil;
    else
	return (structure_exports_inherited_p
		(rep_STRUCTURE (structure), var) ? Qexternal : Qnil);
}

DEFUN ("structure-imports", Fstructure_imports,
       Sstructure_imports, (repv structure), rep_Subr1) /*
::doc:rep.structures#structure-imports::
structure-imports STRUCTURE

Returns the list of structure names opened by structure object
STRUCTURE.
::end:: */
{
    rep_DECLARE1 (structure, rep_STRUCTUREP);
    return rep_STRUCTURE (structure)->imports;
}

DEFUN ("structure-accessible", Fstructure_accessible,
       Sstructure_accessible, (repv structure), rep_Subr1) /*
::doc:rep.structures#structure-accessible::
structure-accessible STRUCTURE

Returns the list of structure names accessed by structure object
STRUCTURE.
::end:: */
{
    rep_DECLARE1 (structure, rep_STRUCTUREP);
    return rep_STRUCTURE (structure)->accessible;
}

DEFUN ("set-interface", Fset_interface,
       Sset_interface, (repv structure, repv sig), rep_Subr2) /*
::doc:rep.structures#set-interface::
set-interface STRUCTURE INTERFACE

Set the interface of structure object STRUCTURE to INTERFACE.
::end:: */
{
    rep_struct *s;
    int i;

    rep_DECLARE1 (structure, rep_STRUCTUREP);
    rep_DECLARE2 (sig, rep_INTERFACEP);
    s = rep_STRUCTURE (structure);
    s->inherited = Fcopy_sequence (sig);
    s->car &= ~rep_STF_EXPORT_ALL;

    for (i = 0; i < s->total_buckets; i++)
    {
	rep_struct_node *n;
	for (n = s->buckets[i]; n != 0; n = n->next)
	{
	    if (structure_exports_inherited_p (s, n->symbol))
	    {
		n->is_exported = 1;
		s->inherited = Fdelq (n->symbol, s->inherited);
	    }
	    else
		n->is_exported = 0;
	}
    }

    cache_flush ();
    return Qt;
}

DEFUN("structure-file", Fstructure_file,
      Sstructure_file, (repv name), rep_Subr1) /*
::doc:rep.structures#structure-file::
structure-file NAME

Return a string that would be used to locate a structure called NAME (a
symbol).
::end:: */
{
    if (rep_SYMBOLP (name))
	name = rep_SYM (name)->name;
    rep_DECLARE1 (name, rep_STRINGP);
    return rep_structure_file (name);
}

DEFUN("intern-structure", Fintern_structure,
      Sintern_structure, (repv name), rep_Subr1) /*
::doc:rep.structures#intern-structure::
intern-structure STRUCT-NAME

Return the structure called STRUCT-NAME. If no such structure exists,
attempt to load it.
::end:: */
{
    repv tem;
    rep_DECLARE1 (name, rep_SYMBOLP);
    tem = Fget_structure (name);
    if (tem == Qnil)
    {
	repv old = rep_structure;
	rep_GC_root gc_name, gc_old;

	/* We need to load the file from within a well-defined
	   structure, not just the current one. Look for the
	   value of the *root-structure* variable first, then
	   fall back to the default structure */

	rep_structure = rep_default_structure;
	tem = Fsymbol_value (Q_user_structure_, Qt);
	if (!rep_VOIDP (tem))
	{
	    tem = Fget_structure (tem);
	    if (rep_STRUCTUREP (tem))
		rep_structure = tem;
	}

	rep_PUSHGC (gc_old, old);
	rep_PUSHGC (gc_name, name);
	tem = Fload (Fstructure_file (name), Qnil, Qnil, Qnil, Qnil);
	rep_POPGC; rep_POPGC;

	rep_structure = old;

	if (tem != rep_NULL && !rep_STRUCTUREP (tem))
	    tem = Qnil;
    }
    return tem;
}

DEFSTRING (no_struct, "No such structure");

DEFUN ("open-structures", Fopen_structures,
       Sopen_structures, (repv args), rep_Subr1) /*
::doc:rep.structures#open-structures::
open-structures STRUCT-NAMES

Mark that the current structures has opened the list of structures
named in the list STRUCT-NAMES.
::end:: */
{
    rep_struct *dst = rep_STRUCTURE (rep_structure);
    rep_GC_root gc_args;
    repv ret = Qnil;
    rep_DECLARE1 (args, rep_LISTP);
    rep_PUSHGC (gc_args, args);
    while (rep_CONSP (args))
    {
	repv tem = Fmemq (rep_CAR (args), dst->imports);
	if (tem == Qnil)
	{
	    repv s = rep_CAR (args);
	    if (rep_SYMBOLP (s))
		s = Fintern_structure (s);
	    if (!s || !rep_STRUCTUREP (s))
	    {
		ret = Fsignal (Qerror, rep_list_2 (rep_VAL (&no_struct),
						   rep_CAR (args)));
		break;
	    }
	    dst->imports = Fcons (rep_CAR (args), dst->imports);
	}
	args = rep_CDR (args);
    }
    rep_POPGC;
    cache_flush ();
    return ret;
}

DEFUN ("access-structures", Faccess_structures,
       Saccess_structures, (repv args), rep_Subr1) /*
::doc:rep.structures#access-structures::
access-structures STRUCT-NAMES

Mark that the current structures may access the list of structures
named in the list STRUCT-NAMES.
::end:: */
{
    rep_struct *dst = rep_STRUCTURE (rep_structure);
    rep_GC_root gc_args;
    repv ret = Qnil;
    rep_DECLARE1 (args, rep_LISTP);
    rep_PUSHGC (gc_args, args);
    while (rep_CONSP (args))
    {
	repv tem = Fmemq (rep_CAR (args), dst->accessible);
	if (tem == Qnil)
	{
	    repv s = Fintern_structure (rep_CAR (args));
	    if (s == rep_NULL || !rep_STRUCTUREP (s))
	    {
		ret = Fsignal (Qerror, rep_list_2 (rep_VAL (&no_struct),
						   rep_CAR (args)));
		break;
	    }
	    dst->accessible = Fcons (rep_CAR (args), dst->accessible);
	}
	args = rep_CDR (args);
    }
    rep_POPGC;
    cache_flush ();
    return ret;
}

DEFUN ("current-structure", Fcurrent_structure,
      Scurrent_structure, (void), rep_Subr0) /*
::doc:rep.structures#current-structure::
current-structure

Return the current structure object.
::end:: */
{
    return rep_structure;
}

DEFUN ("structurep", Fstructurep, Sstructurep, (repv arg), rep_Subr1) /*
::doc:rep.structures#structurep::
structurep ARG

Return `t' if ARG is a structure object.
::end:: */
{
    return rep_STRUCTUREP (arg) ? Qt : Qnil;
}

DEFUN ("eval", Freal_eval, Seval_real,
       (repv form, repv structure, repv env), rep_Subr3) /*
::doc:rep.structures#eval::
eval FORM [STRUCTURE]

Return the result of evaluating FORM inside structure object STRUCTURE
(with a null lexical environment).
::end:: */
{
    repv result;
    repv old = rep_structure, old_env = rep_env;
    rep_GC_root gc_old, gc_old_env;

    if (structure == Qnil)
	structure = rep_structure;
    rep_DECLARE2 (structure, rep_STRUCTUREP);

    rep_PUSHGC (gc_old, old);
    rep_PUSHGC (gc_old_env, old_env);
    rep_structure = structure;
    rep_env = env;

    result = Feval (form);

    rep_structure = old;
    rep_env = old_env;
    rep_POPGC; rep_POPGC;

    return result;
}

DEFUN ("structure-walk", Fstructure_walk,
       Sstructure_walk, (repv fun, repv structure), rep_Subr2) /*
::doc:rep.structures#structure-walk::
structure-walk FUNCTION STRUCTURE

Call FUNCTION for each binding in structure object STRUCTURE. The
function is called with two arguments, the variable and the binding's
value.
::end:: */
{
    rep_GC_root gc_fun, gc_structure;
    repv ret = Qnil;
    rep_struct *s;
    int i;
    rep_DECLARE2 (structure, rep_STRUCTUREP);
    s = rep_STRUCTURE (structure);
    rep_PUSHGC (gc_fun, fun);
    rep_PUSHGC (gc_structure, structure);
    for (i = 0; i < s->total_buckets; i++)
    {
	rep_struct_node *n;
	for (n = s->buckets[i]; n != 0; n = n->next)
	{
	    if (!rep_VOIDP (n->binding))
	    {
		ret = rep_call_lisp2 (fun, n->symbol, n->binding);
		if (!ret)
		    goto out;
	    }
	}
    }
out:
    rep_POPGC; rep_POPGC;
    return ret;
}

#ifdef DEBUG
DEFUN ("structure-stats", Fstructure_stats,
       Sstructure_stats, (repv structure), rep_Subr1)
{
    rep_struct *s;
    int i, empties = 0;
    rep_DECLARE1 (structure, rep_STRUCTUREP);
    s = rep_STRUCTURE (structure);
    for (i = 0; i < s->total_buckets; i++)
    {
	if (s->buckets[i] == 0)
	    empties++;
    }
    printf ("%d buckets, %d of which are empty,\n%g bindings per non-empty bucket\n",
	    s->total_buckets, empties,
	    (double) s->total_bindings / (s->total_buckets - empties));
    return Qt;
}
#endif

DEFUN ("make-binding-immutable", Fmake_binding_immutable,
       Smake_binding_immutable, (repv var), rep_Subr1) /*
::doc:rep.structures#make-binding-immutable::
make-binding-immutable VAR

Flag that the binding of symbol VAR in the current structure may not be
changed.
::end:: */
{
    rep_struct_node *n;
    rep_DECLARE1(var, rep_SYMBOLP);
    n = lookup (rep_STRUCTURE (rep_structure), var);
    if (n != 0)
    {
	n->is_constant = 1;
	return var;
    }
    else
	return Fsignal (Qvoid_value, rep_LIST_1 (var));
}

DEFUN ("binding-immutable-p", Fbinding_immutable_p,
       Sbinding_immutable_p, (repv var, repv structure), rep_Subr2) /*
::doc:rep.structures#binding-immutable-p::
binding-immutable-p VAR [STRUCTURE]

Return `t' if the binding of symbol VAR in the STRUCTURE has been made
constant.
::end:: */
{
    rep_struct_node *n;
    rep_DECLARE1(var, rep_SYMBOLP);
    if (structure != Qnil)
	rep_DECLARE2(structure, rep_STRUCTUREP);
    else
	structure = rep_structure;
    n = lookup (rep_STRUCTURE (structure), var);
    if (n == 0)
	n = rep_search_imports (rep_STRUCTURE (structure), var);
    return (n != 0 && n->is_constant) ? Qt : Qnil;
}

repv
Fexport_binding (repv var)
{
    rep_struct *s;
    rep_struct_node *n;

    rep_DECLARE1 (var, rep_SYMBOLP);

    s = rep_STRUCTURE (rep_structure);
    n = lookup (s, var);

    if (n != 0)
    {
	if (!n->is_exported)
	{
	    n->is_exported = 1;
	    cache_invalidate_symbol (var);
	}
    }
    else if (!structure_exports_inherited_p (s, var))
    {
	s->inherited = Fcons (var, s->inherited);
	cache_invalidate_symbol (var);
    }

    return Qnil;
}

DEFUN ("export-bindings", Fexport_bindings,
       Sexport_bindings, (repv vars), rep_Subr1)
{
    rep_DECLARE1 (vars, rep_LISTP);

    while (rep_CONSP (vars))
    {
	if (Fexport_binding (rep_CAR (vars)) == rep_NULL)
	    return rep_NULL;

	vars = rep_CDR (vars);
    }

    return Qnil;
}


/* features */

DEFUN("featurep", Ffeaturep, Sfeaturep, (repv feature), rep_Subr1) /*
::doc:rep.structures#featurep::
featurep FEATURE

Return non-nil if feature FEATURE has already been loaded by the current
structure.
::end:: */
{
    repv value;
    rep_DECLARE1 (feature, rep_SYMBOLP);
    value = F_structure_ref (rep_structure, Qfeatures);
    return rep_VOIDP (value) ? Qnil : Fmemq (feature, value);
}

DEFUN("provide", Fprovide, Sprovide, (repv feature), rep_Subr1) /*
::doc:rep.structures#provide::
provide FEATURE

Show that the feature FEATURE (a symbol) has been loaded in the current
structure.
::end:: */
{
    repv value, tem;
    rep_DECLARE1 (feature, rep_SYMBOLP);
    value = F_structure_ref (rep_structure, Qfeatures);
    if (rep_VOIDP (value))
	value = Qnil;
    tem = Fmemq (feature, value);
    if (tem && tem == Qnil)
	value = Fcons (feature, value);
    Fstructure_define (rep_structure, Qfeatures, value);
    return feature;
}

DEFUN_INT("require", Frequire, Srequire, (repv feature), rep_Subr1,
	  "SFeature to load:") /*
::doc:rep.structures#require::
require FEATURE

If FEATURE (a symbol) has not already been loaded, load it. The file
loaded is either FILE (if given), or the print name of FEATURE.
::end:: */
{
    repv tem;
    rep_struct *dst = rep_STRUCTURE (rep_structure);

    rep_DECLARE1 (feature, rep_SYMBOLP);

    if (Ffeaturep (feature) != Qnil)
	return feature;

    /* Need to do all this locally, since the file providing the
       feature/module has to be loaded into the _current_ structure
       (in case it contains bare code). %intern-structure OTOH
       always loads into *root-structure*, since it's often called
       with only the %meta structure imported */

    tem = Fmemq (feature, dst->imports);
    if (tem == Qnil)
    {
	tem = Fget_structure (feature);
	if (!rep_STRUCTUREP (tem))
	{
	    rep_GC_root gc_feature;
	    rep_PUSHGC (gc_feature, feature);
	    tem = Fload (Fstructure_file (feature), Qnil, Qnil, Qnil, Qnil);
	    rep_POPGC;
	    
	    if (tem == rep_NULL)
		return rep_NULL;

	    if (rep_STRUCTUREP (tem))
		Fname_structure (tem, feature);
	}
	if (rep_STRUCTUREP (tem))
	{
	    dst->imports = Fcons (feature, dst->imports);
	    Fprovide (feature);
	    cache_flush ();
	}
    }
    return Qt;
}


/* C interface for structure building */

repv
rep_push_structure_name (repv name)
{
    if (rep_STRINGP (name))
	name = Fintern (name, Qnil);
    if (rep_SYMBOLP (name))
    {
	repv s = Fget_structure (name);
	repv old = rep_structure;
	if (s == Qnil)
	    s = Fmake_structure (Qnil, Qnil, Qnil, name);
	rep_structure = s;
	return old;
    }
    else
	return Qnil;
}

repv
rep_push_structure (const char *name)
{
    return rep_push_structure_name (rep_string_dup (name));
}

repv
rep_pop_structure (repv old)
{
    if (rep_STRUCTUREP (old))
    {
	repv new = rep_structure;
	rep_structure = old;
	return new;
    }
    else
	return Qnil;
}

void
rep_alias_structure (const char *name)
{
    repv sym = Fintern (rep_string_dup (name), Qnil);
    Fname_structure (rep_structure, sym);
}

repv
rep_bootstrap_structure (const char *s)
{
    repv name = rep_string_dup (s);
    repv tem = rep_push_structure_name (name);
    repv ret;

    /* Allow the bootstrap code to manipulate modules.. */
    { rep_struct *tem = rep_STRUCTURE (rep_structure);
      if (tem->name != Qrep_structures)
	  tem->imports = Fcons (Qrep_structures, tem->imports);
      if (tem->name != Qrep_lang_interpreter)
	  tem->imports = Fcons (Qrep_lang_interpreter, tem->imports);
      tem->imports = Fcons (Qrep_vm_interpreter, tem->imports); }

    ret = Fload (Fstructure_file (name), Qnil, Qnil, Qnil, Qnil);

    rep_pop_structure (tem);
    return ret;
}

repv
rep_add_subr(rep_xsubr *subr, rep_bool export)
{
    repv sym = Fintern (subr->name, Qnil);
    if (sym)
    {
	rep_struct *s = rep_STRUCTURE (rep_structure);
	rep_struct_node *n = lookup_or_add (s, sym);
	n->binding = rep_VAL (subr);
	n->is_exported = export;
    }
    return sym;
}

DEFUN("structure-exports-all", Fstructure_exports_all,
      Sstructure_exports_all, (repv s, repv status), rep_Subr2)
{
    rep_DECLARE1 (s, rep_STRUCTUREP);
    if (status)
	rep_STRUCTURE (s)->car |= rep_STF_EXPORT_ALL;
    else
	rep_STRUCTURE (s)->car &= ~rep_STF_EXPORT_ALL;
    return s;
}


DEFUN("structure-set-binds", Fstructure_set_binds,
      Sstructure_set_binds, (repv s, repv status), rep_Subr2)
{
    rep_DECLARE1 (s, rep_STRUCTUREP);
    if (status)
	rep_STRUCTURE (s)->car |= rep_STF_SET_BINDS;
    else
	rep_STRUCTURE (s)->car &= ~rep_STF_SET_BINDS;
    return s;
}

void
rep_structure_exports_all (repv s, rep_bool status)
{
    Fstructure_exports_all (s, status ? Qt : Qnil);
}

void
rep_structure_set_binds (repv s, rep_bool status)
{
    Fstructure_set_binds (s, status ? Qt : Qnil);
}

static repv
invalid_apply_bytecode (repv subr, int nargs, repv *args)
{
    return Fsignal (Qinvalid_function, rep_LIST_1 (subr));
}

DEFUN("structure-install-vm", Fstructure_install_vm,
      Sstructure_install_vm, (repv structure, repv vm), rep_Subr2)
{
    rep_struct *s;
    rep_DECLARE1 (structure, rep_STRUCTUREP);
    s = rep_STRUCTURE (structure);
    if (vm == Qnil)
    {
	s->apply_bytecode = invalid_apply_bytecode;
	return Qnil;
    }
    else
    {
	rep_DECLARE (2, vm, Ffunctionp (vm) != Qnil);
	return rep_call_lisp1 (vm, structure);
    }
}

/* This is a horrible kludge :-(

   The problem is that we are used to doing (setq foo-special 42) in rc
   files, even though foo-special is yet to be marked special. So the
   binding gets made in the current structure, and is then ignored when
   the variable finally gets defvar'd.

   So my solution is to mark a structure as the `user' structure (by
   storing its name in the variable *user-structure*), then check this
   structure for bindings when defvar'ing variables

   This function may not gc */
repv
rep_get_initial_special_value (repv sym)
{
    repv user = F_structure_ref (rep_specials_structure, Q_user_structure_);
    if (!rep_VOIDP (user))
    {
	repv s = Fget_structure (user);
	if (rep_STRUCTUREP (s))
	{
	    repv old = F_structure_ref (s, sym);
	    if (!rep_VOIDP (old))
	    {
		Fstructure_define (s, sym, rep_void_value);
		cache_invalidate_symbol (sym);
		return old;
	    }
	}
    }
    return rep_NULL;
}

repv
rep_documentation_property (repv structure)
{
    repv name = rep_STRUCTURE (structure)->name;
    char *buf;

    if (!rep_SYMBOLP (name))
	return Qnil;

    name = rep_SYM (name)->name;
    buf = alloca (rep_STRING_LEN (name) + 32);
    sprintf (buf, "documentation#%s", rep_STR (name));

    return Fintern (rep_string_dup (buf), Qnil);
}


/* init */

void
rep_pre_structures_init (void)
{
    rep_structure_type = rep_register_new_type ("structure", 0,
						structure_print,
						structure_print,
						structure_sweep,
						structure_mark,
						0, 0, 0, 0, 0, 0, 0);
    rep_default_structure = Fmake_structure (Qnil, Qnil, Qnil, Qnil);
    rep_specials_structure = Fmake_structure (Qnil, Qnil, Qnil, Qnil);
    rep_structures_structure = Fmake_structure (Qnil, Qnil, Qnil, Qnil);
}

void
rep_structures_init (void)
{
    repv tem = rep_push_structure ("rep.structures");

    rep_ADD_SUBR (Smake_structure);
    rep_ADD_SUBR (S_structure_ref);
    rep_ADD_SUBR (Sstructure_bound_p);
    rep_ADD_SUBR (Sstructure_set);
    rep_ADD_SUBR (Sstructure_define);
    rep_ADD_SUBR (Sexternal_structure_ref);
    rep_ADD_SUBR (Sstructure_name);
    rep_ADD_SUBR (Sstructure_interface);
    rep_ADD_SUBR (Sstructure_exports_p);
    rep_ADD_SUBR (Sstructure_imports);
    rep_ADD_SUBR (Sstructure_accessible);
    rep_ADD_SUBR (Sset_interface);
    rep_ADD_SUBR (Sget_structure);
    rep_ADD_SUBR (Sname_structure);
    rep_ADD_SUBR (Sstructure_file);
    rep_ADD_SUBR (Sintern_structure);
    rep_ADD_SUBR (Sopen_structures);
    rep_ADD_SUBR (Saccess_structures);
    rep_ADD_SUBR (Scurrent_structure);
    rep_ADD_SUBR (Sstructurep);
    rep_ADD_SUBR (Seval_real);
    rep_ADD_SUBR (Sstructure_walk);
#ifdef DEBUG
    rep_ADD_SUBR (Sstructure_stats);
#endif
    rep_ADD_SUBR (Smake_binding_immutable);
    rep_ADD_SUBR (Sbinding_immutable_p);
    rep_ADD_SUBR (Sexport_bindings);
    rep_ADD_SUBR (Sstructure_exports_all);
    rep_ADD_SUBR (Sstructure_set_binds);
    rep_ADD_SUBR (Sstructure_install_vm);

    rep_pop_structure (tem);

    tem = rep_push_structure ("rep.module-system");
    rep_ADD_SUBR (Sfeaturep);
    rep_ADD_SUBR (Sprovide);
    rep_ADD_SUBR_INT (Srequire);
    rep_pop_structure (tem);

    rep_INTERN (features);
    rep_INTERN (_structures);
    rep_INTERN (_meta);
    rep_INTERN (rep);
    rep_INTERN (_specials);
    rep_INTERN_SPECIAL (_user_structure_);
    rep_INTERN (rep_structures);
    rep_INTERN (rep_lang_interpreter);
    rep_INTERN (rep_vm_interpreter);
    rep_INTERN (external);
    rep_INTERN (local);

    rep_mark_static (&rep_structure);
    rep_mark_static (&rep_default_structure);
    rep_mark_static (&rep_specials_structure);
    rep_mark_static (&rep_structures_structure);

    Fname_structure (rep_default_structure, Qrep);
    Fname_structure (rep_specials_structure, Q_specials);
    Fname_structure (rep_structures_structure, Q_structures);
#ifdef DEBUG
    atexit (print_cache_stats);
#endif
}
