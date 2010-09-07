/* symbols.c -- Lisp symbol handling
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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define _GNU_SOURCE

#define NDEBUG

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
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>

/* The number of hash buckets in each rep_obarray, this is a prime number. */
#define rep_OBSIZE		509
#define rep_KEY_OBSIZE		127

#define rep_FUNARGBLK_SIZE	204		/* ~4k */

/* Closure allocation blocks */
typedef struct rep_funarg_block_struct {
    struct rep_funarg_block_struct *next;
    rep_ALIGN_CELL(rep_funarg data[rep_FUNARGBLK_SIZE]);
} rep_funarg_block;

/* Main storage of symbols.  */
repv rep_obarray, rep_keyword_obarray;

/* Plist storage */
static repv plist_structure;

DEFSYM(t, "t");

DEFSYM(documentation, "documentation");
DEFSYM(permanent_local, "permanent-local");

/* Function vectors to implement local symbols through. */
repv (*rep_deref_local_symbol_fun)(repv sym) = 0;
repv (*rep_set_local_symbol_fun)(repv sym, repv val) = 0;

/* This value is stored in the cells of a symbol to denote a void object. */
rep_ALIGN_CELL(static rep_cell void_object) = { rep_Void };
repv rep_void_value = rep_VAL(&void_object);

/* The special value which signifies the end of a hash-bucket chain.
   It can be any Lisp object which isn't a symbol.  */
#define OB_NIL rep_VAL(&void_object)

/* Used to mark lexical bindings */
rep_ALIGN_CELL(static rep_cell lextag) = { rep_Void };
#define LEXTAG rep_VAL(&lextag)

static rep_funarg_block *funarg_block_chain;
static rep_funarg *funarg_freelist;
int rep_allocated_funargs, rep_used_funargs;

/* support for scheme boolean type */
repv rep_scm_t, rep_scm_f;

repv rep_undefined_value;


/* Symbol management */

DEFUN("make-symbol", Fmake_symbol, Smake_symbol, (repv name), rep_Subr1) /*
::doc:rep.lang.symbols#make-symbol::
make-symbol NAME

Returns a new, uninterned, symbol with print-name NAME. It's value and
function definition are both void and it has a nil property-list.
::end:: */
{
    rep_DECLARE1(name, rep_STRINGP);
    return rep_make_tuple (rep_Symbol, rep_NULL, name);
}

static void
symbol_sweep(void)
{
    /* Need to clear mark bits of dumped symbols, since they're mutable */
    if (rep_dumped_symbols_start != rep_dumped_symbols_end)
    {
	rep_symbol *s;
	for(s = rep_dumped_symbols_start; s < rep_dumped_symbols_end; s++)
	{
	    if(rep_GC_CELL_MARKEDP(rep_VAL(s)))
		rep_GC_CLR_CELL(rep_VAL(s));
	}
    }
}

static int
symbol_cmp(repv v1, repv v2)
{
    if(rep_TYPE(v1) == rep_TYPE(v2))
    {
	if (v1 == v2)
	    return 0;
	else
	    return rep_value_cmp (rep_SYM(v1)->name, rep_SYM(v2)->name);
    }
    else
	return 1;
}

static void
symbol_princ(repv strm, repv obj)
{
    rep_stream_puts(strm, rep_PTR(rep_SYM(obj)->name), -1, rep_TRUE);
}

static void
symbol_print(repv strm, repv obj)
{
    /* output a maximum of 2n chars for a symbol name of length n */
    char *buf = alloca (rep_STRING_LEN (rep_SYM (obj)->name) * 2);
    register char *out = buf;
    register char *s;
    rep_bool seen_digit = rep_FALSE;

    if (rep_SYMBOL_LITERAL_P (obj))
    {
	symbol_princ (strm, obj);
	return;
    }

    s = rep_STR (rep_SYM (obj)->name);
    switch (*s++)
    {
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
	seen_digit = rep_TRUE;

    case '-': case '+': case '.':

    pass1:
	switch (*s++)
	{
	case 0:
	    if (seen_digit)
		*out++ = '\\';
	    break;

	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	    seen_digit = rep_TRUE;
	case '/': case '.':
	    goto pass1;
	}
    }

    s = rep_STR (rep_SYM (obj)->name);
    while (1)
    {
	char c = *s++;
	switch (c)
	{
	case 0:
	    goto out;

	case ' ': case '\t': case '\n': case '\f':
	case '(': case ')': case '[': case ']':
	case '\'': case '"': case ';': case '\\':
	case '|': case ',': case '`':
	    *out++ = '\\';
	    break;

	case '#':
	    if (!(rep_KEYWORDP (obj) && s-1 == rep_STR (rep_SYM (obj)->name)))
		*out++ = '\\';
	    break;

	default:
	    if (iscntrl (c))
		*out++ = '\\';
	    break;
	}
	*out++ = c;
    }
out:
    rep_stream_puts (strm, buf, out - buf, rep_FALSE);
}

void
rep_intern_static(repv *symp, repv name)
{
    if((*symp = Fintern(name, Qnil)))
	rep_mark_static(symp);
    else
	abort();
}

static inline unsigned long
hash(char *str)
{
    register unsigned long value = 0;
    while(*str)
	value = (value * 33) + *str++;
    return(value);
}

DEFUN("make-obarray", Fmake_obarray, Smake_obarray, (repv size), rep_Subr1) /*
::doc:rep.lang.symbols#make-obarray::
make-obarray SIZE

Creates a new structure for storing symbols in. This is basically a vector
with a few slight differences (all elements initialised to a special value).
::end:: */
{
    rep_DECLARE1(size, rep_INTP);
    return(Fmake_vector(size, OB_NIL));
}

DEFUN("find-symbol", Ffind_symbol, Sfind_symbol, (repv name, repv ob), rep_Subr2) /*
::doc:rep.lang.symbols#find-symbol::
find-symbol NAME [OBARRAY]

Returns the symbol with print-name NAME, found by searching OBARRAY (or
the default `rep_obarray' if nil), or nil if no such symbol exists.
::end:: */
{
    int vsize;
    rep_DECLARE1(name, rep_STRINGP);
    if(!rep_VECTORP(ob))
	ob = rep_obarray;
    if((vsize = rep_VECT_LEN(ob)) == 0)
	return(Qnil);
    ob = rep_VECT(ob)->array[hash(rep_STR(name)) % vsize];
    while(rep_SYMBOLP(ob))
    {
	if(!strcmp(rep_STR(name), rep_STR(rep_SYM(ob)->name)))
	    return(ob);
	ob = rep_SYM(ob)->next;
    }
    return(Qnil);
}

DEFSTRING(already_interned, "Symbol is already interned");

DEFUN("intern-symbol", Fintern_symbol, Sintern_symbol, (repv sym, repv ob), rep_Subr2) /*
::doc:rep.lang.symbols#intern-symbol::
intern-symbol SYMBOL [OBARRAY]

Stores SYMBOL in OBARRAY (or the default). If SYMBOL has already been interned
somewhere an error is signalled.
::end:: */
{
    int vsize, hashid;
    rep_DECLARE1(sym, rep_SYMBOLP);
    if(rep_SYM(sym)->next != rep_NULL)
    {
	Fsignal(Qerror, rep_list_2(rep_VAL(&already_interned), sym));
	return rep_NULL;
    }
    if(!rep_VECTORP(ob))
	ob = rep_obarray;
    if((vsize = rep_VECT_LEN(ob)) == 0)
	return rep_NULL;
    hashid = hash(rep_STR(rep_SYM(sym)->name)) % vsize;
    rep_SYM(sym)->next = rep_VECT(ob)->array[hashid];
    rep_VECT(ob)->array[hashid] = sym;
    return(sym);
}

DEFUN("intern", Fintern, Sintern, (repv name, repv ob), rep_Subr2) /*
::doc:rep.lang.symbols#intern::
intern NAME [OBARRAY]

If a symbol with print-name exists in OBARRAY (or the default) return it.
Else use `(make-symbol NAME)' to create a new symbol, intern that into the
OBARRAY, then return it.
::end:: */
{
    repv sym;
    rep_DECLARE1(name, rep_STRINGP);
    if(!(sym = Ffind_symbol(name, ob)) || (rep_NILP(sym)))
    {
	sym = Fmake_symbol(name);
	if(sym)
	    return(Fintern_symbol(sym, ob));
    }
    return(sym);
}

DEFUN("unintern", Funintern, Sunintern, (repv sym, repv ob), rep_Subr2) /*
::doc:rep.lang.symbols#unintern::
unintern SYMBOL [OBARRAY]

Removes SYMBOL from OBARRAY (or the default). Use this with caution.
::end:: */
{
    repv list;
    int vsize, hashid;
    rep_DECLARE1(sym, rep_SYMBOLP);
    if(!rep_VECTORP(ob))
	ob = rep_obarray;
    if((vsize = rep_VECT_LEN(ob)) == 0)
	return rep_NULL;
    hashid = hash(rep_STR(rep_SYM(sym)->name)) % vsize;
    list = rep_VECT(ob)->array[hashid];
    rep_VECT(ob)->array[hashid] = OB_NIL;
    while(rep_SYMBOLP(list))
    {
	repv nxt = rep_SYM(list)->next;
	if(list != sym)
	{
	    rep_SYM(list)->next = rep_VECT(ob)->array[hashid];
	    rep_VECT(ob)->array[hashid] = rep_VAL(list);
	}
	list = nxt;
    }
    rep_SYM(sym)->next = rep_NULL;
    return(sym);
}


/* Closures */

DEFUN("make-closure", Fmake_closure, Smake_closure,
      (repv fun, repv name), rep_Subr2) /*
::doc:rep.lang.interpreter#make-closure::
make-closure FUNCTION &optional NAME

Return a functional object which makes the closure of FUNCTION and the
current environment.
::end:: */
{
    rep_funarg *f;
    if(!funarg_freelist)
    {
	rep_funarg_block *sb = rep_ALLOC_CELL(sizeof(rep_funarg_block));
	if(sb)
	{
	    int i;
	    rep_allocated_funargs += rep_FUNARGBLK_SIZE;
	    sb->next = funarg_block_chain;
	    funarg_block_chain = sb;
	    for(i = 0; i < (rep_FUNARGBLK_SIZE - 1); i++)
		sb->data[i].car = rep_VAL(&sb->data[i + 1]);
	    sb->data[i].car = rep_VAL(funarg_freelist);
	    funarg_freelist = sb->data;
	}
    }

    f = funarg_freelist;
    funarg_freelist = rep_FUNARG (f->car);
    rep_data_after_gc += sizeof (rep_funarg);
    f->car = rep_Funarg;
    f->fun = fun;
    f->name = name;
    f->env = rep_env;
    f->structure = rep_structure;
    return rep_VAL (f);
}

DEFUN("closure-function", Fclosure_function,
      Sclosure_function, (repv funarg), rep_Subr1) /*
::doc:rep.lang.interpreter#closure-function::
closure-function FUNARG

Return the function value associated with the closure FUNARG.
::end:: */
{
    rep_DECLARE1(funarg, rep_FUNARGP);
    return rep_FUNARG(funarg)->fun;
}

DEFUN("set-closure-function", Fset_closure_function,
      Sset_closure_function, (repv funarg, repv fun), rep_Subr2) /*
::doc:rep.lang.interpreter#set-closure-function::
set-closure-function FUNARG FUNCTION

Set the function value in the closure FUNARG to FUNCTION.
::end:: */
{
    rep_DECLARE1(funarg, rep_FUNARGP);
    rep_FUNARG(funarg)->fun = fun;
    return fun;
}

DEFUN("closure-structure", Fclosure_structure,
      Sclosure_structure, (repv funarg), rep_Subr1) /*
::doc:rep.structures#closure-function::
closure-function FUNARG

Return the structure associated with the closure FUNARG.
::end:: */
{
    rep_DECLARE1(funarg, rep_FUNARGP);
    return rep_FUNARG(funarg)->structure;
}

DEFUN ("set-closure-structure", Fset_closure_structure,
       Sset_closure_structure, (repv closure, repv structure), rep_Subr2)
{
    rep_DECLARE1 (closure, rep_FUNARGP);
    rep_DECLARE2 (structure, rep_STRUCTUREP);
    rep_FUNARG (closure)->structure = structure;
    return Qnil;
}

DEFUN("closure-name", Fclosure_name,
      Sclosure_name, (repv funarg), rep_Subr1) /*
::doc:rep.lang.interpreter#closure-name::
closure-name FUNARG

Return the name associated with the closure FUNARG.
::end:: */
{
    rep_DECLARE1(funarg, rep_FUNARGP);
    return rep_FUNARG(funarg)->name;
}

DEFUN("closurep", Fclosurep, Sclosurep, (repv arg), rep_Subr1) /*
::doc:rep.lang.interpreter#closurep::
funargp ARG

Returns t if ARG is a closure
::end:: */
{
    return rep_FUNARGP(arg) ? Qt : Qnil;
}

DEFUN("set-special-environment", Fset_special_environment,
      Sset_special_environment, (repv env, repv structure), rep_Subr2) /*
::doc:rep.structures#set-special-environment::
set-special-environment ENV STRUCTURE
::end:: */
{
    rep_DECLARE2 (structure, rep_STRUCTUREP);
    rep_STRUCTURE (structure)->special_env = env;
    return Qt;
}

static void
funarg_sweep (void)
{
    rep_funarg_block *sb = funarg_block_chain;
    funarg_freelist = NULL;
    rep_used_funargs = 0;
    while(sb)
    {
	int i;
	rep_funarg_block *nxt = sb->next;
	for(i = 0; i < rep_FUNARGBLK_SIZE; i++)
	{
	    /* if on the freelist then the CELL_IS_8 bit
	       will be unset (since the pointer is long aligned) */
	    if (rep_CELL_CONS_P(rep_VAL(&sb->data[i]))
		|| !rep_GC_CELL_MARKEDP(rep_VAL(&sb->data[i])))
	    {
		sb->data[i].car = rep_VAL(funarg_freelist);
		funarg_freelist = &sb->data[i];
	    }
	    else
	    {
		rep_GC_CLR_CELL(rep_VAL(&sb->data[i]));
		rep_used_funargs++;
	    }
	}
	sb = nxt;
    }
}

/* Returns (SYM . VALUE) if a lexical binding, or nil */
static repv
search_environment (repv sym)
{
    register repv env;
    for (env = rep_env; env != Qnil; env = rep_CDR (env))
    {
	if (rep_CONSP (rep_CAR (env))
	    && rep_CAAR(env) == LEXTAG
	    && rep_CADAR(env) == sym)
	{
	    return rep_CDAR (env);
	}
    }
    return Qnil;
}

/* this is also in lispmach.c and fluids.c */
static inline repv
inlined_search_special_bindings (repv sym)
{
    register repv env;
    for (env = rep_special_bindings; env != Qnil; env = rep_CDR (env))
    {
	if (rep_CAAR(env) == sym)
	    return rep_CAR (env);
    }
    return Qnil;
}

static repv
search_special_bindings (repv sym)
{
    return inlined_search_special_bindings (sym);
}

static inline int
inlined_search_special_environment (repv sym)
{
    register repv env = rep_SPECIAL_ENV;
    while (rep_CONSP(env) && rep_CAR(env) != sym)
	env = rep_CDR(env);

    if (rep_CONSP(env))
	return 1;
    else if (env == Qt)
	return -1;
    else
	return 0;
}

static int
search_special_environment__ (repv sym)
{
    return inlined_search_special_environment (sym);
}

static inline int
search_special_environment (repv sym)
{
    if (rep_SPECIAL_ENV == Qt)
	return -1;
    else
	return search_special_environment__ (sym);
}

repv
rep_call_with_closure (repv closure, repv (*fun)(repv arg), repv arg)
{
    repv ret = rep_NULL;
    if (rep_FUNARGP (closure))
    {
	struct rep_Call lc;
	lc.fun = lc.args = Qnil;
	rep_PUSH_CALL (lc);
	rep_USE_FUNARG (closure);
	ret = fun (arg);
	rep_POP_CALL (lc);
    }
    return ret;
}


/* Symbol binding */

repv
rep_bind_special (repv oldList, repv symbol, repv newVal)
{
    if (inlined_search_special_environment (symbol))
    {
	rep_special_bindings = Fcons (Fcons (symbol, newVal),
				      rep_special_bindings);
	oldList = rep_MARK_SPEC_BINDING (oldList);
    }
    else
	Fsignal (Qvoid_value, rep_LIST_1(symbol));
    return oldList;
}

/* This give SYMBOL a new value, saving the old one onto the front of
   the list OLDLIST. OLDLIST is structured like (NSPECIALS . NLEXICALS)
   Returns the new version of OLDLIST.   */
repv
rep_bind_symbol(repv oldList, repv symbol, repv newVal)
{
    if (oldList == Qnil)
	oldList = rep_NEW_FRAME;

    if (rep_SYM(symbol)->car & rep_SF_SPECIAL)
    {
	/* special binding */
	oldList = rep_bind_special (oldList, symbol, newVal);
    }
    else
    {
	rep_env = Fcons (Fcons (LEXTAG, Fcons (symbol, newVal)), rep_env);
	oldList = rep_MARK_LEX_BINDING (oldList);
    }
    return oldList;
}

/* Undoes what the above function does. Returns the number of special
   bindings undone. */
int
rep_unbind_symbols(repv oldList)
{
    if (oldList != Qnil)
    {
	register repv tem;
	int lexicals, specials;
	int i;

	assert (rep_INTP(oldList));

	lexicals = rep_LEX_BINDINGS (oldList);
	specials = rep_SPEC_BINDINGS (oldList);

	tem = rep_env;
	for (i = lexicals; i > 0; i--)
	    tem = rep_CDR (tem);
	rep_env = tem;

	tem = rep_special_bindings;
	for (i = specials; i > 0; i--)
	{
	    tem = rep_CDR (tem);
	}
	rep_special_bindings = tem;

	assert (rep_special_bindings != rep_void_value);
	assert (rep_env != rep_void_value);

	return specials;
    }
    else
	return 0;
}

repv
rep_add_binding_to_env (repv env, repv sym, repv value)
{
    return Fcons (Fcons (LEXTAG, Fcons (sym, value)), env);
}


/* More lisp functions */

DEFUN("defvar", Fdefvar, Sdefvar, (repv args, repv tail_posn), rep_SF) /*
::doc:rep.lang.interpreter#defvar::
defvar NAME [DEFAULT-VALUE [DOC-STRING]]

Define a special variable called NAME whose standard value is DEFAULT-
VALUE. If NAME is already bound to a value (that's not an autoload
definition) it is left as it is, otherwise DEFAULT-VALUE is evaluated
and the special value of NAME is bound to the result.

If DOC-STRING is given, and is a string, it will be used to set the
`documentation' property of the symbol NAME.

(If the symbol NAME is marked buffer-local the default value of the
variable will be set (if necessary) not the local value.)
::end:: */
{
    if(rep_CONSP(args))
    {
	int spec;
	repv sym = rep_CAR(args), val;
	rep_bool need_to_eval;
	repv tmp = Fdefault_boundp(sym);
	if(!tmp)
	    return rep_NULL;

	if (rep_CONSP(rep_CDR(args)))
	{
	    val = rep_CADR(args);
	    args = rep_CDDR (args);
	}
	else
	{
	    val = Qnil;
	    args = Qnil;
	}

	need_to_eval = rep_TRUE;
	if(!rep_NILP(tmp))
	{
	    /* Variable is bound, see if it's an autoload defn to overwrite. */
	    repv val = Fsymbol_value (sym, Qt);
	    if (rep_FUNARGP(val))
	    {
		val = rep_FUNARG(val)->fun;
		if(rep_CONSP(val) && rep_CAR(val) == Qautoload)
		{
		    Fmakunbound (sym);
		    tmp = Qnil;
		}
	    }
	}

	/* Only allowed to defvar in restricted environments
	   if the symbol hasn't yet been defvar'd or it's weak */
	spec = search_special_environment (sym);
	if (spec == 0 && (rep_SYM(sym)->car & rep_SF_DEFVAR)
	    && !(rep_SYM(sym)->car & rep_SF_WEAK))
	{
	    return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */
	}

	/* if initially making it special, check for a lexical binding
	   in the current module */
	if (!(rep_SYM(sym)->car & rep_SF_SPECIAL))
	{
	    repv tem = rep_get_initial_special_value (sym);
	    if (tem)
	    {
		val = tem;
		need_to_eval = rep_FALSE;
		tmp = Qnil;
	    }
	}

	/* Only set the [default] value if its not boundp or
	   the definition is weak and we're currently unrestricted */
	if(rep_NILP(tmp)
	   || ((rep_SYM(sym)->car & rep_SF_WEAK)
	       && !(rep_SYM(sym)->car & rep_SF_WEAK_MOD)
	       && rep_SPECIAL_ENV == Qt))
	{
	    if (need_to_eval)
	    {
		rep_GC_root gc_sym, gc_args;
		rep_PUSHGC (gc_sym, sym);
		rep_PUSHGC (gc_args, args);
		val = Feval (val);
		rep_POPGC; rep_POPGC;
		if (!val)
		    return rep_NULL;
	    }
	    Fstructure_define (rep_specials_structure, sym, val);
	}

	rep_SYM(sym)->car |= rep_SF_SPECIAL | rep_SF_DEFVAR;

	if (spec == 0)
	{
	    /* defvar'ing an undefvar'd variable from a restricted
	       environment sets it as weak, and adds it to the env */

	    rep_SYM(sym)->car |= rep_SF_WEAK;
	    rep_SPECIAL_ENV = Fcons (sym, rep_SPECIAL_ENV);
	}
	else if (rep_SPECIAL_ENV == Qt && (rep_SYM(sym)->car & rep_SF_WEAK))
	{
	    /* defvar'ing a weak variable from an unrestricted
	       environment removes the weak status, but marks
	       it as `was weak, but now strong'. This prevents
	       exploits such as:

			[restricted special environment]
			(defvar special-var "/bin/rm")

			[unrestricted environment]
			(defvar special-var "ls")

			[back in restricted environment]
			(setq special-var "/bin/rm")
			   --> error

	       Setting the variable the first time (since it's
	       unbound) adds it to the restricted environment,
	       but defvar'ing effectively removes it */

	    rep_SYM(sym)->car &= ~rep_SF_WEAK;
	    rep_SYM(sym)->car |= rep_SF_WEAK_MOD;
	}

	if(rep_CONSP(args))
	{
	    repv doc = rep_CAR(args);
	    if (rep_STRINGP (doc))
	    {
		if (Fput(sym, Qdocumentation, doc) == rep_NULL)
		    return rep_NULL;
	    }
	}
	return sym;
    }
    else
	return rep_signal_missing_arg (1);
}

DEFUN("symbol-value", Fsymbol_value, Ssymbol_value, (repv sym, repv no_err), rep_Subr2) /*
::doc:rep.lang.symbols#symbol-value::
symbol-value SYMBOL

Returns the value of SYMBOL, if SYMBOL is flagged as having buffer-local
values look for one of those first.
::end:: */
/* Second argument (NO-ERR) means don't signal an error if the value is
   void. */
{
    /* Some of this function is hardcoded into the OP_REFQ
       instruction in lispmach.c */
    repv val = rep_void_value;
    rep_DECLARE1(sym, rep_SYMBOLP);

    if (rep_SYM(sym)->car & rep_SF_SPECIAL)
    {
	int spec = inlined_search_special_environment (sym);
	/* modified-weak specials can only be accessed from an
	   unrestricted environment */
	if (spec < 0 || (spec > 0 && !(rep_SYM(sym)->car & rep_SF_WEAK_MOD)))
	{
	    if(rep_SYM(sym)->car & rep_SF_LOCAL)
		val = (*rep_deref_local_symbol_fun)(sym);
	    if (val == rep_void_value)
	    {
		repv tem = inlined_search_special_bindings (sym);
		if (tem != Qnil)
		    val = rep_CDR (tem);
		else
		    val = F_structure_ref (rep_specials_structure, sym);
	    }
	}
    }
    else
    {
	/* lexical variable */
	repv tem = search_environment (sym);
	if (tem != Qnil)
	    val = rep_CDR(tem);
	else
	    val = F_structure_ref (rep_structure, sym);
    }

    if (rep_SYM(sym)->car & rep_SF_DEBUG)
	rep_single_step_flag = rep_TRUE;

    if(no_err == Qnil && rep_VOIDP(val))
	return Fsignal(Qvoid_value, rep_LIST_1(sym));
    else
	return val;
}

DEFUN("default-value", Fdefault_value, Sdefault_value,
      (repv sym, repv no_err), rep_Subr2) /*
::doc:rep.lang.symbols#default-value::
default-value SYMBOL

Returns the default value of the symbol SYMBOL. This will be the value of
SYMBOL in buffers or windows which do not have their own local value.
::end:: */
{
    repv val = rep_void_value;
    rep_DECLARE1(sym, rep_SYMBOLP);

    if (rep_SYM(sym)->car & rep_SF_SPECIAL)
    {
	int spec = search_special_environment (sym);
	if (spec < 0 || (spec > 0 && !(rep_SYM(sym)->car & rep_SF_WEAK_MOD)))
	{
	    repv tem = search_special_bindings (sym);
	    if (tem != Qnil)
		val = rep_CDR (tem);
	    else
		val = F_structure_ref (rep_specials_structure, sym);
	}
    }
    else
	val = F_structure_ref (rep_structure, sym);

    if(no_err == Qnil && rep_VOIDP(val))
	return Fsignal(Qvoid_value, rep_LIST_1(sym));
    else
	return val;
}

static repv
do_set (repv sym, repv val, repv (*setter)(repv st, repv var, repv val))
{
    /* Some of this function is hardcoded into the OP_SETQ
       instruction in lispmach.c */
    rep_DECLARE1(sym, rep_SYMBOLP);

    if (rep_SYM(sym)->car & rep_SF_SPECIAL)
    {
	int spec = inlined_search_special_environment (sym);
	if (spec)
	{
	    repv tem;

	    /* Not allowed to set `modified' variables unless
	       our environment includes all variables implicitly */
	    if (spec > 0 && rep_SYM(sym)->car & rep_SF_WEAK_MOD)
		return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */

	    if(rep_SYM(sym)->car & rep_SF_LOCAL)
	    {
		repv tem = (*rep_set_local_symbol_fun)(sym, val);
		if (tem != rep_NULL)
		    return tem;
		/* Fall through and set the default value. */
	    }
	    tem = inlined_search_special_bindings (sym);
	    if (tem != Qnil)
		rep_CDR (tem) = val;
	    else
		val = Fstructure_define (rep_specials_structure, sym, val);
	}
	else
	    val = Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */
    }
    else
    {
	/* lexical binding */
	repv tem = search_environment (sym);
	if (tem != Qnil)
	    rep_CDR(tem) = val;
	else
	    val = setter (rep_structure, sym, val);
    }
    return val;
}

/* backwards compatibility for C callers */
repv Fset (repv s, repv v) { return do_set (s, v, Fstructure_define); };

DEFUN_INT("set", Freal_set, Sset, (repv s, repv v), rep_Subr2,
	  "vVariable:" rep_DS_NL "xNew value of %s:") /*
::doc:rep.lang.symbols#set::
set SYMBOL repv

Sets the value of SYMBOL to repv. If SYMBOL has a buffer-local binding
in the current buffer or `make-variable-buffer-local' has been called on
SYMBOL the buffer-local value in the current buffer is set. Returns repv.
::end:: */
{
    return do_set (s, v, Fstructure_set);
}

DEFUN("set-default", Fset_default, Sset_default,
      (repv sym, repv val), rep_Subr2) /*
::doc:rep.lang.symbols#set-default::
set-default SYMBOL VALUE

Sets the default value of SYMBOL to VALUE, then returns VALUE.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    if (rep_SYM (sym)->car & rep_SF_SPECIAL)
    {
	int spec = search_special_environment (sym);
	if (spec)
	{
	    repv tem;

	    if (spec > 0 && rep_SYM(sym)->car & rep_SF_WEAK_MOD)
		return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */

	    tem = search_special_bindings (sym);
	    if (tem != Qnil)
		rep_CDR (tem) = val;
	    else
		val = Fstructure_define (rep_specials_structure, sym, val);
	}
	else
	    return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */
    }
    else
	Fstructure_set (rep_structure, sym, val);
    return val;
}

DEFUN("setplist", Fsetplist, Ssetplist, (repv sym, repv prop), rep_Subr2) /*
::doc:rep.lang.symbols#setplist::
setplist SYMBOL PROP-LIST

Sets the property list of SYMBOL to PROP-LIST, returns PROP-LIST.
::end:: */
{
    int spec;
    rep_DECLARE1(sym, rep_SYMBOLP);
    spec = search_special_environment (sym);
    if (spec == 0)
	return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */

    Fstructure_define (plist_structure, sym, prop);
    return prop;
}

DEFUN("symbol-name", Fsymbol_name, Ssymbol_name, (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#symbol-name::
symbol-name SYMBOL

Returns the print-name of SYMBOL.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    return(rep_SYM(sym)->name);
}

DEFUN("default-boundp", Fdefault_boundp, Sdefault_boundp, (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#default-boundp::
default-boundp SYMBOL

Returns t if SYMBOL has a default value.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    if (rep_SYM(sym)->car & rep_SF_SPECIAL)
    {
	repv tem = search_special_bindings (sym);
	if (tem != Qnil)
	    return rep_VOIDP (rep_CDR (tem)) ? Qnil : Qt;
	else
	{
	    tem = F_structure_ref (rep_specials_structure, sym);
	    return rep_VOIDP (tem) ? Qnil : Qt;
	}
    }
    else
	return Fstructure_bound_p (rep_structure, sym);
}

DEFUN("boundp", Fboundp, Sboundp, (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#boundp::
boundp SYMBOL

Returns t if SYMBOL has a value as a variable.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    return(rep_VOIDP(Fsymbol_value(sym, Qt)) ? Qnil : Qt);
}

DEFUN("symbol-plist", Fsymbol_plist, Ssymbol_plist, (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#symbol-plist::
symbol-plist SYMBOL

Returns the property-list of SYMBOL.
::end:: */
{
    int spec;
    repv plist;
    rep_DECLARE1(sym, rep_SYMBOLP);
    spec = search_special_environment (sym);
    if (spec == 0)
	return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */

    plist = F_structure_ref (plist_structure, sym);
    return rep_VOIDP (plist) ? Qnil : plist;
}

DEFUN("gensym", Fgensym, Sgensym, (void), rep_Subr0) /*
::doc:rep.lang.symbols#gensym::
gensym

Returns a new (non-interned) symbol with a unique print name.
::end:: */
{
    static int counter;
    char buf[20];
    counter++;
#ifdef HAVE_SNPRINTF
    snprintf(buf, sizeof(buf), "G%04d", counter);
#else
    sprintf(buf, "G%04d", counter);
#endif
    return(Fmake_symbol(rep_string_dup(buf)));
}

DEFUN("symbolp", Fsymbolp, Ssymbolp, (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#symbolp::
symbolp ARG

Returns t if ARG is a symbol.
::end:: */
{
    return(rep_SYMBOLP(sym) ? Qt : Qnil);
}

DEFUN("setq", Fsetq, Ssetq, (repv args, repv tail_posn), rep_SF) /*
::doc:rep.lang.interpreter#setq::
setq [SYMBOL FORM] ...

Sets the value of each SYMBOL to the value of its corresponding FORM
evaluated, returns the value of the last evaluation.
::end:: */
{
    repv res = Qnil;
    rep_GC_root gc_args;
    rep_PUSHGC(gc_args, args);
    while(rep_CONSP(args) && rep_CONSP(rep_CDR(args)) && rep_SYMBOLP(rep_CAR(args)))
    {
	if(!(res = Feval(rep_CAR(rep_CDR(args)))))
	    goto end;
	if(!Freal_set(rep_CAR(args), res))
	{
	    res = rep_NULL;
	    goto end;
	}
	args = rep_CDR(rep_CDR(args));
    }
end:
    rep_POPGC;
    return(res);
}

DEFUN ("%define", F_define, S_define, (repv args,  repv tail_posn), rep_SF) /*
::doc:rep.lang.interpreter#%define::
%define SYMBOL FORM [DOC-STRING]

Evaluate FORM, then create a top-level binding of SYMBOL whose value is
the result of the evaluation. If such a binding already exists, it will
be overwritten.
::end:: */
{
    repv var, value, doc = Qnil;
    rep_GC_root gc_var, gc_doc;

    if (!rep_assign_args (args, 2, 3, &var, &value, &doc))
	return rep_NULL;

    rep_PUSHGC (gc_var, var);
    rep_PUSHGC (gc_doc, doc);
    value = Feval (value);
    rep_POPGC; rep_POPGC;
    if (value == rep_NULL)
	return rep_NULL;

    value = Fstructure_define (rep_structure, var, value);
    if (value != rep_NULL)
    {
	if (doc != Qnil)
	{
	    repv prop = rep_documentation_property (rep_structure);
	    if (prop != Qnil)
	    {
		if (Fput (var, prop, doc) == rep_NULL)
		    value = rep_NULL;
	    }
	}
    }

    return rep_undefined_value;
}

DEFUN("makunbound", Fmakunbound, Smakunbound, (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#makunbound::
makunbound SYMBOL

Make SYMBOL have no value as a variable.
::end:: */
{
    return Freal_set (sym, rep_void_value);
}

DEFUN("get", Fget, Sget, (repv sym, repv prop), rep_Subr2) /*
::doc:rep.lang.symbols#get::
get SYMBOL PROPERTY

Returns the value of SYMBOL's property PROPERTY. See `put'.
::end:: */
{
    repv plist;
    rep_DECLARE1(sym, rep_SYMBOLP);
    plist = F_structure_ref (plist_structure, sym);
    if (rep_VOIDP (plist))
	return Qnil;
    while(rep_CONSP(plist) && rep_CONSP(rep_CDR(plist)))
    {
	if(rep_CAR(plist) == prop
	   || (!rep_SYMBOLP(prop)
	       && rep_value_cmp (rep_CAR(plist), prop) == 0))
	{
	    return(rep_CAR(rep_CDR(plist)));
	}
	plist = rep_CDR(rep_CDR(plist));
    }
    return(Qnil);
}

DEFUN("put", Fput, Sput, (repv sym, repv prop, repv val), rep_Subr3) /*
::doc:rep.lang.symbols#put::
put SYMBOL PROPERTY repv

Sets the value of SYMBOL's property PROPERTY to repv, this value can be
retrieved with the `get' function.
::end:: */
{
    repv plist, old;
    int spec;
    rep_DECLARE1(sym, rep_SYMBOLP);
    spec = search_special_environment (sym);
    if (spec == 0)
	return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */

    old = F_structure_ref (plist_structure, sym);
    if (rep_VOIDP (old))
	old = Qnil;
    plist = old;
    while(rep_CONSP(plist) && rep_CONSP(rep_CDR(plist)))
    {
	if(rep_CAR(plist) == prop
	   || (!rep_SYMBOLP(prop)
	       && rep_value_cmp (rep_CAR(plist), prop) == 0))
	{
	    if(!rep_CONS_WRITABLE_P(rep_CDR(plist)))
	    {
		/* Can't write into a dumped cell; need to cons
		   onto the head. */
		break;
	    }
	    rep_CAR(rep_CDR(plist)) = val;
	    return val;
	}
	plist = rep_CDR(rep_CDR(plist));
    }
    Fstructure_define (plist_structure, sym, Fcons (prop, Fcons (val, old)));
    return val;
}

DEFUN("apropos", Fapropos, Sapropos, (repv re, repv pred, repv ob), rep_Subr3) /*
::doc:rep.lang.symbols#apropos::
apropos REGEXP [PREDICATE] [OBARRAY]

Returns a list of symbols from OBARRAY (or the default) whose print-name
matches the regular-expression REGEXP. If PREDICATE is given and non-nil,
each symbol which matches is applied to the function PREDICATE, if the value
is non-nil it is considered a match.
::end:: */
{
    rep_regexp *prog;
    rep_DECLARE1(re, rep_STRINGP);
    if(!rep_VECTORP(ob))
	ob = rep_obarray;
    prog = rep_regcomp(rep_STR(re));
    if(prog)
    {
	repv last = Qnil;
	int i, len = rep_VECT_LEN(ob);
	rep_GC_root gc_last, gc_ob, gc_pred;
	rep_PUSHGC(gc_last, last);
	rep_PUSHGC(gc_ob, ob);
	rep_PUSHGC(gc_pred, pred);
	for(i = 0; i < len; i++)
	{
	    repv chain = rep_VECT(ob)->array[i];
	    while(rep_SYMBOLP(chain))
	    {
		if(rep_regexec(prog, rep_STR(rep_SYM(chain)->name)))
		{
		    if(pred && !rep_NILP(pred))
		    {
			repv tmp;
			if(!(tmp = rep_funcall(pred, rep_LIST_1(chain), rep_FALSE))
			   || rep_NILP(tmp))
			{
			    goto next;
			}
		    }
		    last = Fcons(chain, last);
		}
next:
		chain = rep_SYM(chain)->next;
	    }
	}
	rep_POPGC; rep_POPGC; rep_POPGC;
	free(prog);
	return(last);
    }
    return rep_NULL;
}

DEFUN("make-variable-special", Fmake_variable_special,
      Smake_variable_special, (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#make-variable-special::
make-variable-special SYMBOL

Mark SYMBOL as being a special (dynamically-bound) variable.
::end:: */
{
    int spec;
    rep_DECLARE1(sym, rep_SYMBOLP);
    spec = search_special_environment (sym);
    if (spec == 0)
	return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */
    if (!(rep_SYM(sym)->car & rep_SF_SPECIAL))
    {
	repv tem = rep_get_initial_special_value (sym);
	if (tem)
	    Fstructure_define (rep_specials_structure, sym, tem);
    }
    rep_SYM(sym)->car |= rep_SF_SPECIAL;
    return sym;
}

DEFUN("special-variable-p", Fspecial_variable_p, Sspecial_variable_p,
      (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#special-variable-p::
special-variable-p SYMBOL

Returns t if SYMBOL is a special variable (dynamically scoped).
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    return (rep_SYM(sym)->car & rep_SF_SPECIAL) ? Qt : Qnil;
}

DEFUN_INT("trace", Ftrace, Strace, (repv sym), rep_Subr1, "aFunction to trace") /*
::doc:rep.lang.debug#trace::
trace SYMBOL

Flag that whenever SYMBOL is evaluated (as a variable or a function) the
debugger is entered.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    rep_SYM(sym)->car |= rep_SF_DEBUG;
    return(sym);
}

DEFUN_INT("untrace", Funtrace, Suntrace, (repv sym), rep_Subr1, "aFunction to untrace") /*
::doc:rep.lang.debug#untrace::
untrace SYMBOL

Cancel the effect of (trace SYMBOL).
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    rep_SYM(sym)->car &= ~rep_SF_DEBUG;
    return(sym);
}

DEFUN("obarray", Fobarray, Sobarray, (repv val), rep_Subr1) /*
::doc:rep.lang.symbols#obarray::
obarray [NEW-VALUE]
::end:: */
{
    if(val != Qnil)
    {
	rep_DECLARE1(val, rep_VECTORP);
	rep_obarray = val;
    }
    return rep_obarray;
}

DEFUN("make-keyword", Fmake_keyword, Smake_keyword, (repv in), rep_Subr1) /*
::doc:rep.lang.symbols#make-keyword::
make-keyword SYMBOL

Return the keyword symbol that should be used in argument lists to
provide the mark the value of the argument called SYMBOL. An error is
signalled if SYMBOL is itself a keyword.
::end:: */
{
    repv str, name, key;
    int name_len;

    rep_DECLARE (1, in, rep_SYMBOLP (in) && !rep_KEYWORDP (in));

    name = rep_SYM (in)->name;
    name_len = rep_STRING_LEN (name);
    str = rep_make_string (name_len + 3);
    rep_STR (str)[0] = '#';
    rep_STR (str)[1] = ':';
    memcpy (rep_STR (str) + 2, rep_STR (name), name_len);
    rep_STR (str)[name_len+2] = 0;

    key = Fintern (str, rep_keyword_obarray);
    rep_SYM (key)->car |= rep_SF_KEYWORD;
    return key;
}

DEFUN ("keywordp", Fkeywordp, Skeywordp, (repv arg), rep_Subr1) /*
::doc:rep.lang.symbols#keywordp::
keywordp ARG

Return true if ARG is a keyword symbol.
::end:: */
{
    return rep_KEYWORDP (arg) ? Qt : Qnil;
}

int
rep_pre_symbols_init(void)
{
    rep_register_type(rep_Symbol, "symbol", symbol_cmp, symbol_princ,
		      symbol_print, symbol_sweep, 0, 0, 0, 0, 0, 0, 0, 0);
    rep_obarray = Fmake_obarray(rep_MAKE_INT(rep_OBSIZE));
    rep_keyword_obarray = Fmake_obarray(rep_MAKE_INT(rep_KEY_OBSIZE));
    rep_register_type(rep_Funarg, "funarg", rep_ptr_cmp,
		      rep_lisp_prin, rep_lisp_prin, funarg_sweep,
		      0, 0, 0, 0, 0, 0, 0, 0);
    if(rep_obarray && rep_keyword_obarray)
    {
	rep_mark_static(&rep_obarray);
	rep_mark_static(&rep_keyword_obarray);
	return rep_TRUE;
    }
    else
	return rep_FALSE;
}

void
rep_symbols_init(void)
{
    DEFSTRING (hash_f, "#f");
    DEFSTRING (hash_t, "#t");
    DEFSTRING (hash_undefined, "#undefined");

    repv tem;

    rep_pre_datums_init ();		/* initializes Qnil */
    rep_INTERN(t);
    rep_pre_structures_init ();

    rep_USE_DEFAULT_ENV;
    rep_special_bindings = Qnil;
    rep_mark_static (&rep_env);
    rep_mark_static (&rep_special_bindings);

    plist_structure = Fmake_structure (Qnil, Qnil, Qnil, Qnil);
    rep_mark_static (&plist_structure);

    rep_INTERN(documentation);
    rep_INTERN(permanent_local);

    rep_scm_f = Fmake_symbol (rep_VAL (&hash_f));
    rep_scm_t = Fmake_symbol (rep_VAL (&hash_t));
    rep_undefined_value = Fmake_symbol (rep_VAL (&hash_undefined));
    rep_SYM(rep_scm_f)->car |= rep_SF_LITERAL;
    rep_SYM(rep_scm_t)->car |= rep_SF_LITERAL;
    rep_SYM(rep_undefined_value)->car |= rep_SF_LITERAL;
    rep_mark_static (&rep_scm_f);
    rep_mark_static (&rep_scm_t);
    rep_mark_static (&rep_undefined_value);

    tem = rep_push_structure ("rep.lang.symbols");
    rep_ADD_SUBR(Smake_symbol);
    rep_ADD_SUBR(Smake_obarray);
    rep_ADD_SUBR(Sfind_symbol);
    rep_ADD_SUBR(Sintern_symbol);
    rep_ADD_SUBR(Sintern);
    rep_ADD_SUBR(Sunintern);
    rep_ADD_SUBR(Ssymbol_value);
    rep_ADD_SUBR_INT(Sset);
    rep_ADD_SUBR(Ssetplist);
    rep_ADD_SUBR(Ssymbol_name);
    rep_ADD_SUBR(Sdefault_value);
    rep_ADD_SUBR(Sdefault_boundp);
    rep_ADD_SUBR(Sset_default);
    rep_ADD_SUBR(Sboundp);
    rep_ADD_SUBR(Ssymbol_plist);
    rep_ADD_SUBR(Sgensym);
    rep_ADD_SUBR(Ssymbolp);
    rep_ADD_SUBR(Smakunbound);
    rep_ADD_SUBR(Sget);
    rep_ADD_SUBR(Sput);
    rep_ADD_SUBR(Sapropos);
    rep_ADD_SUBR(Smake_variable_special);
    rep_ADD_SUBR(Sspecial_variable_p);
    rep_ADD_SUBR(Sobarray);
    rep_ADD_SUBR(Smake_keyword);
    rep_ADD_SUBR(Skeywordp);
    rep_pop_structure (tem);

    tem = rep_push_structure ("rep.lang.interpreter");
    rep_ADD_SUBR(Ssetq);
    rep_ADD_SUBR(S_define);
    rep_ADD_SUBR(Sdefvar);
    rep_ADD_SUBR(Smake_closure);
    rep_ADD_SUBR(Sclosure_function);
    rep_ADD_SUBR(Sset_closure_function);
    rep_ADD_SUBR(Sclosure_name);
    rep_ADD_SUBR(Sclosurep);
    rep_pop_structure (tem);

    tem = rep_push_structure ("rep.structures");
    rep_ADD_SUBR(Sclosure_structure);
    rep_ADD_SUBR(Sset_closure_structure);
    rep_ADD_SUBR(Sset_special_environment);
    rep_pop_structure (tem);

    tem = rep_push_structure ("rep.lang.debug");
    rep_ADD_SUBR_INT(Strace);
    rep_ADD_SUBR_INT(Suntrace);
    rep_pop_structure (tem);
}
