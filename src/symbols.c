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

#define rep_SYMBOLBLK_SIZE	405		/* ~8k */

/* Symbol allocation blocks */
typedef struct rep_symbol_block_struct {
    struct rep_symbol_block_struct *next;
    rep_ALIGN_CELL(rep_symbol symbols[rep_SYMBOLBLK_SIZE]);
} rep_symbol_block;

/* Main storage of symbols.  */
repv rep_obarray;

/* Plist storage */
static repv plist_structure;

static rep_funarg *funargs;

DEFSYM(nil, "nil");
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

static rep_symbol_block *symbol_block_chain;
static rep_symbol *symbol_freelist;
int rep_allocated_symbols, rep_used_symbols;


/* Symbol management */

DEFUN("make-symbol", Fmake_symbol, Smake_symbol, (repv name), rep_Subr1) /*
::doc:make-symbol::
make-symbol NAME

Returns a new, uninterned, symbol with print-name NAME. It's value and
function definition are both void and it has a nil property-list.
::end:: */
{
    repv sym;
    rep_DECLARE1(name, rep_STRINGP);
    if(!symbol_freelist)
    {
	rep_symbol_block *sb = rep_ALLOC_CELL(sizeof(rep_symbol_block));
	if(sb)
	{
	    int i;
	    rep_allocated_symbols += rep_SYMBOLBLK_SIZE;
	    sb->next = symbol_block_chain;
	    symbol_block_chain = sb;
	    for(i = 0; i < (rep_SYMBOLBLK_SIZE - 1); i++)
		sb->symbols[i].next = rep_VAL(&sb->symbols[i + 1]);
	    sb->symbols[i].next = rep_VAL(symbol_freelist);
	    symbol_freelist = sb->symbols;
	}
    }
    if((sym = rep_VAL(symbol_freelist)))
    {
	symbol_freelist = rep_SYM(rep_SYM(sym)->next);
	rep_SYM(sym)->next = rep_NULL;
	rep_SYM(sym)->car = rep_Symbol;
	rep_SYM(sym)->name = name;
	rep_used_symbols++;
	rep_data_after_gc += sizeof(rep_symbol);
    }
    return(sym);
}

static void
symbol_sweep(void)
{
    rep_symbol_block *sb = symbol_block_chain;
    symbol_freelist = NULL;
    rep_used_symbols = 0;
    while(sb)
    {
	int i;
	rep_symbol_block *nxt = sb->next;
	for(i = 0; i < rep_SYMBOLBLK_SIZE; i++)
	{
	    if(!rep_GC_CELL_MARKEDP(rep_VAL(&sb->symbols[i])))
	    {
		sb->symbols[i].next = rep_VAL(symbol_freelist);
		symbol_freelist = &sb->symbols[i];
	    }
	    else
	    {
		rep_GC_CLR_CELL(rep_VAL(&sb->symbols[i]));
		rep_used_symbols++;
	    }
	}
	sb = nxt;
    }
    /* Need to clear mark bits of dumped symbols, since they're mutable */
    if (rep_dumped_symbols_start != rep_dumped_symbols_end)
    {
	rep_symbol *s;
	for(s = rep_dumped_symbols_start; s < rep_dumped_symbols_end; s++)
	{
	    if(rep_GC_CELL_MARKEDP(rep_VAL(s)))
	    {
		rep_GC_CLR_CELL(rep_VAL(s));
		rep_used_symbols++;
	    }
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
    u_char *s = rep_STR(rep_SYM(obj)->name);
    u_char c;
    rep_bool all_digits = isdigit (*s);

    while((c = *s++))
    {
	switch(c)
	{
	case ' ': case '\t': case '\n': case '\f':
	case '(': case ')': case '[': case ']':
	case '\'': case '"': case ';': case '\\':
	case '|': case '#':
	    rep_stream_putc(strm, (int)'\\');
	    all_digits = rep_FALSE;
	    break;

	case '.': case '/': case 'e': case 'E':
	    if (all_digits)
	    {
		rep_stream_putc (strm, (int) '\\');
		all_digits = rep_FALSE;
	    }
	    break;

	default:
	    if (!isdigit (c))
		all_digits = rep_FALSE;
	    if(iscntrl(c))
		rep_stream_putc(strm, (int)'\\');
	    break;
	}
	rep_stream_putc(strm, (int)c);
    }
}

void
rep_intern_static(repv *symp, repv name)
{
    if((*symp = Fintern(name, Qnil)))
	rep_mark_static(symp);
    else
	abort();
}

static inline u_long
hash(u_char *str)
{
    register u_long value = 0;
    while(*str)
	value = (value * 33) + *str++;
    return(value);
}

DEFUN("make-obarray", Fmake_obarray, Smake_obarray, (repv size), rep_Subr1) /*
::doc:make-obarray::
make-obarray SIZE

Creates a new structure for storing symbols in. This is basically a vector
with a few slight differences (all elements initialised to a special value).
::end:: */
{
    rep_DECLARE1(size, rep_INTP);
    return(Fmake_vector(size, OB_NIL));
}

DEFUN("find-symbol", Ffind_symbol, Sfind_symbol, (repv name, repv ob), rep_Subr2) /*
::doc:find-symbol::
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
::doc:intern-symbol::
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
::doc:intern::
intern NAME [OBARRAY]

If a symbol with print-name exists in OBARRAY (or the default) return it.
Else use `(make-symbol NAME)' to create a new symbol, intern that into the
OBARRAY, then return it.
::end:: */
{
    repv sym;
    rep_DECLARE1(name, rep_STRINGP);
    if(!(sym = Ffind_symbol(name, ob))
       || (rep_NILP(sym) && strcmp(rep_STR(name), "nil")))
    {
	sym = Fmake_symbol(name);
	if(sym)
	    return(Fintern_symbol(sym, ob));
    }
    return(sym);
}

DEFUN("unintern", Funintern, Sunintern, (repv sym, repv ob), rep_Subr2) /*
::doc:unintern::
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
    rep_VECT(ob)->array[hashid] = rep_NULL;
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
::doc:make-closure::
make-closure FUNCTION &optional NAME

Return a functional object which makes the closure of FUNCTION and the
current environment.
::end:: */
{
    rep_funarg *f = rep_ALLOC_CELL (sizeof (rep_funarg));
    rep_data_after_gc += sizeof (rep_funarg);
    f->car = rep_Funarg;
    if (rep_bytecode_interpreter != Fjade_byte_code)
	f->car |= rep_FF_NO_BYTE_CODE;
    f->fun = fun;
    f->name = name;
    f->env = rep_env;
    f->special_env = rep_special_env;
    f->fh_env = rep_fh_env;
    f->structure = rep_structure;
    f->next = funargs;
    funargs = f;
    return rep_VAL (f);
}

DEFUN("closure-function", Fclosure_function,
      Sclosure_function, (repv funarg), rep_Subr1) /*
::doc:closure-function::
closure-function FUNARG

Return the function value associated with the closure FUNARG.
::end:: */
{
    rep_DECLARE1(funarg, rep_FUNARGP);
    return rep_FUNARG(funarg)->fun;
}

DEFUN("set-closure-function", Fset_closure_function,
      Sset_closure_function, (repv funarg, repv fun), rep_Subr2) /*
::doc:set-closure-function::
set-closure-function FUNARG FUNCTION

Set the function value in the closure FUNARG to FUNCTION.
::end:: */
{
    rep_DECLARE1(funarg, rep_FUNARGP);
    rep_FUNARG(funarg)->fun = fun;
    return fun;
}

DEFUN("closurep", Fclosurep, Sclosurep, (repv arg), rep_Subr1) /*
::doc:closurep::
funargp ARG

Returns t if ARG is a closure
::end:: */
{
    return rep_FUNARGP(arg) ? Qt : Qnil;
}

DEFUN("save-environment", Fsave_environment,
      Ssave_environment, (repv args), rep_SF) /*
::doc:save-environment::
save-environment FORMS...
::end:: */
{
    repv ret;
    struct rep_Call lc;
    lc.fun = Qnil;
    lc.args = Qnil;
    lc.args_evalled_p = Qnil;
    rep_PUSH_CALL(lc);
    ret = Fprogn (args);
    rep_POP_CALL(lc);
    return ret;
}

DEFUN("set-environment", Fset_environment,
      Sset_environment, (repv env), rep_Subr1) /*
::doc:set-environment::
set-variable-environment ENV
::end:: */
{
    if (rep_call_stack != 0)
    {
	if (rep_LISTP (env))
	{
	    repv tem;
	    rep_call_stack->saved_env = env;
	    tem = Fassq (Qjade_byte_code, env);
	    if (tem && tem != Qnil)
	    {
		tem = rep_CDR (tem);
		if (rep_CELL8_TYPEP (tem, rep_Subr3))
		    rep_bytecode_interpreter = rep_SUBR4FUN (tem);
		else
		    rep_bytecode_interpreter = 0;
	    }
	}
    }
    return Qt;
}

DEFUN("set-special-environment", Fset_special_environment,
      Sset_special_environment, (repv env), rep_Subr1) /*
::doc:set-special-environment::
set-special-environment ENV
::end:: */
{
    if (rep_call_stack != 0)
    {
	if (rep_LISTP (env))
	    rep_call_stack->saved_special_env = Fcons (Qnil, env);
    }
    return Qt;
}

static void
funarg_sweep (void)
{
    rep_funarg *f = funargs;
    funargs = 0;
    while (f != 0)
    {
	rep_funarg *next = f->next;
	if (!rep_GC_CELL_MARKEDP(rep_VAL(f)))
	    rep_FREE_CELL(f);
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(f));
	    f->next = funargs;
	    funargs = f;
	}
	f = next;
    }
}

/* Returns (SYM . VALUE) if a lexical binding. Returns t if the actual
   value is in the symbol's function slot */
static repv
search_environment (repv sym)
{
    register repv env = rep_env;
    while (rep_CONSP(env) && rep_CAR(rep_CAR(env)) != sym)
	env = rep_CDR(env);
    return rep_CONSP(env) ? rep_CAR(env) : env;
}

/* this is also in lispmach.c */
static inline repv
inlined_search_special_bindings (repv sym)
{
    register repv env = rep_special_bindings;
    while (env != Qnil && rep_CAAR(env) != sym)
	env = rep_CDR(env);
    return env != Qnil ? rep_CAR(env) : env;
}

static repv
search_special_bindings (repv sym)
{
    return inlined_search_special_bindings (sym);
}

static inline int
inlined_search_special_environment (repv sym)
{
    register repv env = rep_CDR(rep_special_env);
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
search_special_environment (repv sym)
{
    return inlined_search_special_environment (sym);
}


/* Symbol binding */

/* This give SYMBOL a new value, saving the old one onto the front of
   the list OLDLIST. OLDLIST is structured like (NSPECIALS . NLEXICALS)
   Returns the new version of OLDLIST.   */
repv
rep_bind_symbol(repv oldList, repv symbol, repv newVal)
{
    if (oldList == Qnil)
	oldList = Fcons (rep_MAKE_INT(0), rep_MAKE_INT(0));

    if (rep_SYM(symbol)->car & rep_SF_SPECIAL)
    {
	/* special binding */
	if (inlined_search_special_environment (symbol))
	{
	    rep_special_bindings = Fcons (Fcons (symbol, newVal),
					  rep_special_bindings);
	    rep_CAR(oldList) = rep_MAKE_INT(rep_INT(rep_CAR(oldList)) + 1);
	}
	else
	    Fsignal (Qvoid_value, rep_LIST_1(symbol));
    }
    else
    {
	/* lexical binding (also in lispmach.c:OP_BIND) */
	rep_env = Fcons (Fcons (symbol, newVal), rep_env);
	rep_CDR(oldList) = rep_MAKE_INT(rep_INT(rep_CDR(oldList)) + 1);
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
	int i;

	assert (rep_CONSP(oldList));
	assert (rep_INTP(rep_CAR(oldList)));
	assert (rep_INTP(rep_CDR(oldList)));

	tem = rep_env;
	for (i = rep_INT (rep_CDR (oldList)); i > 0; i--)
	    tem = rep_CDR (tem);
	rep_env = tem;

	tem = rep_special_bindings;
	for (i = rep_INT (rep_CAR (oldList)); i > 0; i--)
	    tem = rep_CDR (tem);
	rep_special_bindings = tem;

	assert (rep_special_bindings != rep_void_value);
	assert (rep_env != rep_void_value);

	return rep_INT (rep_CAR (oldList));
    }
    else
	return 0;
}


/* More lisp functions */

DEFUN("defvar", Fdefvar, Sdefvar, (repv args), rep_SF) /*
::doc:defvar::
defvar NAME DEFAULT-VALUE [DOC-STRING]

Define a variable called NAME whose standard value is DEFAULT-
VALUE. If NAME is already bound to a value (that's not an autoload
definition) it is left as it is.

If the symbol NAME is marked buffer-local the *default value* of the
variable will be set (if necessary) not the local value.
::end:: */
{
    if(rep_CONSP(args) && rep_CONSP(rep_CDR(args)))
    {
	int spec;
	rep_GC_root gc_args;
	repv sym = rep_CAR(args), val;
	repv tmp = Fdefault_boundp(sym);
	if(!tmp)
	    return rep_NULL;
	rep_PUSHGC(gc_args, args);
	val = Feval(rep_CAR(rep_CDR(args)));
	rep_POPGC;
	if(!val)
	    return rep_NULL;
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
		tmp = Qnil;
	    }
	}

	/* Only set the [default] value if its not boundp or
	   the definition is weak and we're currently unrestricted */
	if(rep_NILP(tmp)
	   || ((rep_SYM(sym)->car & rep_SF_WEAK)
	       && !(rep_SYM(sym)->car & rep_SF_WEAK_MOD)
	       && rep_CDR(rep_special_env) == Qt))
	{
	    F_structure_set (rep_specials_structure, sym, val);
	}

	rep_SYM(sym)->car |= rep_SF_SPECIAL | rep_SF_DEFVAR;

	if (spec == 0)
	{
	    /* defvar'ing an undefvar'd variable from a restricted
	       environment sets it as weak, and adds it to the env */

	    rep_SYM(sym)->car |= rep_SF_WEAK;
	    rep_CDR(rep_special_env) = Fcons (sym, rep_CDR(rep_special_env));
	}
	else if (rep_CDR(rep_special_env) == Qt
		 && (rep_SYM(sym)->car & rep_SF_WEAK))
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

	if(rep_CONSP(rep_CDR(rep_CDR(args))))
	{
	    if (!Fput(sym, Qdocumentation,
		      rep_CAR(rep_CDR(rep_CDR(args)))))
		return rep_NULL;
	}
	return sym;
    }
    else
	return rep_signal_missing_arg(rep_CONSP(args) ? 2 : 1);
}

DEFUN("symbol-value", Fsymbol_value, Ssymbol_value, (repv sym, repv no_err), rep_Subr2) /*
::doc:symbol-value::
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
	if (spec < 0 || !(rep_SYM(sym)->car & rep_SF_WEAK_MOD))
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
	if (rep_CONSP(tem))
	    val = rep_CDR(tem);
	else if (tem == Qt)
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
::doc:default-value::
default-value SYMBOL

Returns the default value of the symbol SYMBOL. This will be the value of
SYMBOL in buffers or windows which do not have their own local value.
::end:: */
{
    repv val = rep_void_value;
    int spec;
    rep_DECLARE1(sym, rep_SYMBOLP);
    
    spec = search_special_environment (sym);
    if (spec < 0 || !(rep_SYM(sym)->car & rep_SF_WEAK_MOD))
    {
	repv tem = search_special_bindings (sym);
	if (tem != Qnil)
	    val = rep_CDR (tem);
	else
	    val = F_structure_ref (rep_specials_structure, sym);
    }

    if(no_err == Qnil && rep_VOIDP(val))
	return Fsignal(Qvoid_value, rep_LIST_1(sym));
    else
	return val;
}

DEFUN_INT("set", Fset, Sset, (repv sym, repv val), rep_Subr2,
	  "vVariable:" rep_DS_NL "xNew value of %s:") /*
::doc:set::
set SYMBOL repv

Sets the value of SYMBOL to repv. If SYMBOL has a buffer-local binding
in the current buffer or `make-variable-buffer-local' has been called on
SYMBOL the buffer-local value in the current buffer is set. Returns repv.
::end:: */
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
		val = F_structure_set (rep_specials_structure, sym, val);
	}
	else
	    val = Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */
    }
    else
    {
	/* lexical binding */
	repv tem = search_environment (sym);
	if (rep_CONSP(tem))
	    rep_CDR(tem) = val;
	else if (tem == Qt)
	    val = F_structure_set (rep_structure, sym, val);
	else
	    val = Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */
    }
    return val;
}

DEFUN("set-default", Fset_default, Sset_default,
      (repv sym, repv val), rep_Subr2) /*
::doc:set-default::
set-default SYMBOL VALUE

Sets the default value of SYMBOL to VALUE, then returns VALUE. Also
makes the symbol special if it isn't already.
::end:: */
{
    int spec;

    rep_DECLARE1(sym, rep_SYMBOLP);

    spec = search_special_environment (sym);
    if (spec)
    {
	repv tem;

	if (spec > 0 && rep_SYM(sym)->car & rep_SF_WEAK_MOD)
	    return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */

	if (!(rep_SYM(sym)->car & rep_SF_SPECIAL))
	    Fmake_variable_special (sym);

	tem = search_special_bindings (sym);
	if (tem != Qnil)
	    rep_CDR (tem) = val;
	else
	    val = F_structure_set (rep_specials_structure, sym, val);
    }
    else
	return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */
    return val;
}

DEFUN("setplist", Fsetplist, Ssetplist, (repv sym, repv prop), rep_Subr2) /*
::doc:setplist::
setplist SYMBOL PROP-LIST

Sets the property list of SYMBOL to PROP-LIST, returns PROP-LIST.
::end:: */
{
    int spec;
    rep_DECLARE1(sym, rep_SYMBOLP);
    spec = search_special_environment (sym);
    if (spec == 0)
	return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */

    F_structure_set (plist_structure, sym, prop);
    return prop;
}

DEFUN("symbol-name", Fsymbol_name, Ssymbol_name, (repv sym), rep_Subr1) /*
::doc:symbol-name::
symbol-name SYMBOL

Returns the print-name of SYMBOL.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    return(rep_SYM(sym)->name);
}

DEFUN("default-boundp", Fdefault_boundp, Sdefault_boundp, (repv sym), rep_Subr1) /*
::doc:default-boundp::
default-boundp SYMBOL

Returns t if SYMBOL has a default value.
::end:: */
{
    repv tem;
    rep_DECLARE1(sym, rep_SYMBOLP);
    tem = search_special_bindings (sym);
    if (tem != Qnil)
	return rep_VOIDP (rep_CDR (tem));
    else
    {
	tem = F_structure_ref (rep_specials_structure, sym);
	return rep_VOIDP (tem) ? Qnil : Qt;
    }
}

DEFUN("boundp", Fboundp, Sboundp, (repv sym), rep_Subr1) /*
::doc:boundp::
boundp SYMBOL

Returns t if SYMBOL has a value as a variable.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    return(rep_VOIDP(Fsymbol_value(sym, Qt)) ? Qnil : Qt);
}

DEFUN("symbol-plist", Fsymbol_plist, Ssymbol_plist, (repv sym), rep_Subr1) /*
::doc:symbol-plist::
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
::doc:gensym::
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
::doc:symbolp::
symbolp ARG

Returns t if ARG is a symbol.
::end:: */
{
    return(rep_SYMBOLP(sym) ? Qt : Qnil);
}

DEFUN("setq", Fsetq, Ssetq, (repv args), rep_SF) /*
::doc:setq::
setq { SYMBOL FORM }...

Sets the value of each SYMBOL to the value of its corresponding FORM
evaluated, returns the value of the last evaluation. ie,
  (setq x 1 y (symbol-name 'nil))
   => "nil"
  x
   => 1
  y
   => "nil"
::end:: */
{
    repv res = Qnil;
    rep_GC_root gc_args;
    rep_PUSHGC(gc_args, args);
    while(rep_CONSP(args) && rep_CONSP(rep_CDR(args)) && rep_SYMBOLP(rep_CAR(args)))
    {
	if(!(res = Feval(rep_CAR(rep_CDR(args)))))
	    goto end;
	if(!Fset(rep_CAR(args), res))
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

DEFUN("makunbound", Fmakunbound, Smakunbound, (repv sym), rep_Subr1) /*
::doc:makunbound::
makunbound SYMBOL

Make SYMBOL have no value as a variable.
::end:: */
{
    return Fset (sym, rep_void_value);
}

DEFUN("get", Fget, Sget, (repv sym, repv prop), rep_Subr2) /*
::doc:get::
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
::doc:put::
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
    F_structure_set (plist_structure, sym, Fcons (prop, Fcons (val, old)));
    return val;
}

DEFUN("apropos", Fapropos, Sapropos, (repv re, repv pred, repv ob), rep_Subr3) /*
::doc:apropos::
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
      Smake_variable_special, (repv sym), rep_Subr1)
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
	    F_structure_set (rep_specials_structure, sym, tem);
    }
    rep_SYM(sym)->car |= rep_SF_SPECIAL;
    return sym;
}

DEFUN("special-variable-p", Fspecial_variable_p, Sspecial_variable_p,
      (repv sym), rep_Subr1) /*
::doc:special-variable-p::
special-variable-p SYMBOL

Returns t if SYMBOL is a special variable (dynamically scoped).
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    return (rep_SYM(sym)->car & rep_SF_SPECIAL) ? Qt : Qnil;
}

DEFUN_INT("trace", Ftrace, Strace, (repv sym), rep_Subr1, "aFunction to trace") /*
::doc:trace::
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
::doc:untrace::
untrace SYMBOL

Cancel the effect of (trace SYMBOL).
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    rep_SYM(sym)->car &= ~rep_SF_DEBUG;
    return(sym);
}

DEFUN("obarray", Fobarray, Sobarray, (repv val), rep_Subr1) /*
::doc:obarray::
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

int
rep_pre_symbols_init(void)
{
    rep_register_type(rep_Symbol, "symbol", symbol_cmp, symbol_princ,
		      symbol_print, symbol_sweep, 0, 0, 0, 0, 0, 0, 0, 0);
    rep_obarray = Fmake_obarray(rep_MAKE_INT(rep_OBSIZE));
    rep_register_type(rep_Funarg, "funarg", rep_ptr_cmp,
		      rep_lisp_prin, rep_lisp_prin, funarg_sweep,
		      0, 0, 0, 0, 0, 0, 0, 0);
    if(rep_obarray)
    {
	rep_mark_static(&rep_obarray);
	return rep_TRUE;
    }
    else
	return rep_FALSE;
}

void
rep_symbols_init(void)
{
    /* Fiddly details of initialising the first symbol. We initialise
       all fields in case it was dumped. */
    Qnil = Fintern(rep_VAL(&str_nil), rep_obarray);
    rep_mark_static(&Qnil);
    rep_INTERN(t);

    rep_pre_structures_init ();

    rep_USE_DEFAULT_ENV;
    rep_special_bindings = Qnil;
    rep_mark_static (&rep_env);
    rep_mark_static (&rep_special_env);
    rep_mark_static (&rep_special_bindings);

    F_structure_set (rep_structure, Qnil, Qnil);
    F_structure_set (rep_structure, Qt, Qt);
    Fmake_binding_immutable (Qnil);
    Fmake_binding_immutable (Qt);

    plist_structure = F_make_structure (Qnil, Qnil, Qnil, Qnil);
    rep_mark_static (&plist_structure);

    rep_INTERN(documentation);
    rep_INTERN(permanent_local);
    rep_ADD_SUBR(Smake_symbol);
    rep_ADD_SUBR(Smake_obarray);
    rep_ADD_SUBR(Sfind_symbol);
    rep_ADD_SUBR(Sintern_symbol);
    rep_ADD_SUBR(Sintern);
    rep_ADD_SUBR(Sunintern);
    rep_ADD_SUBR(Smake_closure);
    rep_ADD_SUBR(Sclosure_function);
    rep_ADD_SUBR(Sset_closure_function);
    rep_ADD_SUBR(Sclosurep);
    rep_ADD_SUBR(Ssave_environment);
    rep_ADD_SUBR(Sset_environment);
    rep_ADD_SUBR(Sset_special_environment);
    rep_ADD_SUBR(Sdefvar);
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
    rep_ADD_SUBR(Ssetq);
    rep_ADD_SUBR(Smakunbound);
    rep_ADD_SUBR(Sget);
    rep_ADD_SUBR(Sput);
    rep_ADD_SUBR(Sapropos);
    rep_ADD_SUBR(Smake_variable_special);
    rep_ADD_SUBR(Sspecial_variable_p);
    rep_ADD_SUBR_INT(Strace);
    rep_ADD_SUBR_INT(Suntrace);
    rep_ADD_SUBR(Sobarray);
}

void
rep_symbols_kill(void)
{
    rep_symbol_block *sb = symbol_block_chain;
    while(sb)
    {
	rep_symbol_block *nxt = sb->next;
	rep_FREE_CELL(sb);
	sb = nxt;
    }
    symbol_block_chain = NULL;
}
