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

/* Main storage of symbols.  */
repv rep_obarray;

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

/* When true, warn about hiding functions when binding */
rep_bool rep_warn_shadowing = rep_FALSE;


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
	rep_SYM(sym)->value = rep_void_value;
	rep_SYM(sym)->prop_list = Qnil;
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
	return(!(rep_SYM(v1) == rep_SYM(v2)));
    return(1);
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
    while((c = *s++))
    {
	switch(c)
	{
	case ' ':
	case '\t':
	case '\n':
	case '\f':
	case '(':
	case ')':
	case '[':
	case ']':
	case '\'':
	case '"':
	case ';':
	case '\\':
	case '|':
	    rep_stream_putc(strm, (int)'\\');
	    break;
	default:
	    if(iscntrl(c))
		rep_stream_putc(strm, (int)'\\');
	    break;
	}
	rep_stream_putc(strm, (int)c);
    }
}

repv
rep_add_subr(rep_xsubr *subr)
{
    repv sym = Fintern(subr->name, rep_obarray);
    if(sym)
    {
	rep_SYM(sym)->value = rep_VAL(subr);
	rep_SYM(sym)->car |= rep_SF_DEFVAR;
	if(subr->car == rep_Var)
	    rep_SYM(sym)->car |= rep_SF_SPECIAL;
    }
    return(sym);
}

repv
rep_add_const_num(repv name, long num)
{
    repv sym = Fintern(name, rep_obarray);
    if(sym)
    {
	rep_SYM(sym)->value = rep_MAKE_INT(num);
	rep_SYM(sym)->car |= rep_SF_CONSTANT | rep_SF_SPECIAL | rep_SF_DEFVAR;
    }
    return(sym);
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
		    rep_bytecode_interpreter = rep_SUBR3FUN (tem);
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

/* this is also in lispmach.c

   Returns (SYM . VALUE) if a lexical binding. Returns t if the actual
   value is in the symbol's function slot */
static inline repv
search_environment (repv sym)
{
    register repv env = rep_env;
    while (rep_CONSP(env) && rep_CAR(rep_CAR(env)) != sym)
	env = rep_CDR(env);
    return rep_CONSP(env) ? rep_CAR(env) : env;
}

static inline int
search_special_environment (repv sym)
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


/* Symbol binding */

/* This give SYMBOL a new value, saving the old one onto the front of
   the list OLDLIST. OLDLIST is structured like,
     ((SYMBOL . OLDVALUE) ...)
   Returns the new version of OLDLIST.   */
repv
rep_bind_symbol(repv oldList, repv symbol, repv newVal)
{
    if (rep_warn_shadowing
	&& Fboundp (symbol) != Qnil
	&& Ffunctionp (Fsymbol_value (symbol, Qt)) != Qnil)
    {
	fprintf (stderr, "warning: shadowing %s\n",
		 rep_STR(rep_SYM(symbol)->name));
    }

    if (rep_SYM(symbol)->car & rep_SF_SPECIAL)
    {
	if (search_special_environment (symbol))
	{
	    repv newbl = Fcons(Fcons(symbol, Qnil), oldList);
	    /* Binding to buffer-local values is a recipe for disaster */
	    rep_CDR(rep_CAR(newbl)) = Fdefault_value(symbol, Qt);
	    Fset_default(symbol, newVal);
	    return newbl;
	}
	else
	{
	    Fsignal (Qvoid_value, rep_LIST_1(symbol));
	    /* no one expects this function to fail :-( */
	    return oldList;
	}
    }
    else
    {
	/* lexical binding (this code also in lispmach.c:OP_BIND) */
	rep_env = Fcons (Fcons (symbol, newVal), rep_env);
	return Fcons (symbol, oldList);
    }
}

/* Undoes what the above function does.  */
void
rep_unbind_symbols(repv oldList)
{
    while(rep_CONSP(oldList))
    {
	repv tmp = rep_CAR(oldList);
	if (rep_CONSP(tmp))
	{
	    /* dynamic binding */
	    Fset_default(rep_CAR(tmp), rep_CDR(tmp));
	}
	else
	{
	    /* lexical binding */
	    rep_env = rep_CDR(rep_env);
	}
	oldList = rep_CDR(oldList);
    }
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
	    repv val = rep_SYM(sym)->value;
	    if (rep_FUNARGP(val))
		val = rep_FUNARG(val)->fun;
	    if(rep_CONSP(val) && rep_CAR(val) == Qautoload)
		tmp = Qnil;
	}

	/* Only allowed to defvar in restricted environments
	   if the symbol hasn't yet been defvar'd or it's weak */
	spec = search_special_environment (sym);
	if (spec == 0 && (rep_SYM(sym)->car & rep_SF_DEFVAR)
	    && !(rep_SYM(sym)->car & rep_SF_WEAK))
	{
	    return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */
	}

	/* Only set the [default] value if its not boundp or
	   the definition is weak and we're currently unrestricted */
	if(rep_NILP(tmp)
	   || ((rep_SYM(sym)->car & rep_SF_WEAK)
	       && !(rep_SYM(sym)->car & rep_SF_WEAK_MOD)
	       && rep_CDR(rep_special_env) == Qt))
	{
	    if(rep_CELL8_TYPEP(rep_SYM(sym)->value, rep_Var))
		rep_VARFUN(rep_SYM(sym)->value)(val);
	    else
		rep_SYM(sym)->value = val;
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
	int spec = search_special_environment (sym);
	/* modified-weak specials can only be accessed from an
	   unrestricted environment */
	if (spec < 0 || !(rep_SYM(sym)->car & rep_SF_WEAK_MOD))
	{
	    if(rep_SYM(sym)->car & rep_SF_LOCAL)
		val = (*rep_deref_local_symbol_fun)(sym);
	    if (val == rep_void_value)
		val = rep_SYM(sym)->value;
	}
    }
    else
    {
	/* lexical variable */
	repv tem = search_environment (sym);
	if (rep_CONSP(tem))
	    val = rep_CDR(tem);
	else if (tem == Qt)
	    val = rep_SYM(sym)->value;
    }

    if(val && (rep_CELL8_TYPEP(val, rep_Var)))
    {
	val = rep_VARFUN(val)(rep_NULL);
	if(val == rep_NULL)
	    val = rep_void_value;
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
	val = rep_SYM(sym)->value;

    if(val && (rep_CELL8_TYPEP(val, rep_Var)))
    {
	val = rep_VARFUN(val)(rep_NULL);
	if(val == rep_NULL)
	    val = rep_void_value;
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

    if (rep_SYM(sym)->car & rep_SF_CONSTANT)
	return Fsignal(Qsetting_constant, rep_LIST_1(sym));

    if (rep_SYM(sym)->car & rep_SF_SPECIAL)
    {
	int spec = search_special_environment (sym);
	if (spec)
	{
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
	    if (rep_CELL8_TYPEP(rep_SYM(sym)->value, rep_Var))
		rep_VARFUN(rep_SYM(sym)->value)(val);
	    else
		rep_SYM(sym)->value = val;
	}
	else
	    return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */
    }
    else
    {
	/* lexical binding */
	repv tem = search_environment (sym);
	if (rep_CONSP(tem))
	    rep_CDR(tem) = val;
	else if (tem == Qt)
	    rep_SYM(sym)->value = val;
	else
	    return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */
    }
    return val;
}

DEFUN("define-value", Fdefine_value, Sdefine_value,
      (repv sym, repv value), rep_Subr2) /*
::doc:define-value::
define-value SYMBOL VALUE

Similar to `set', but marks that the variable has been defined.
::end:: */
{
    repv ret;
    rep_DECLARE1(sym, rep_SYMBOLP);
    ret = Fset (sym, value);
    if (ret != rep_NULL)
    {
	rep_SYM(sym)->car |= rep_SF_DEFVAR;
	if (rep_CDR(rep_special_env) == Qt
	    && (rep_SYM(sym)->car & rep_SF_WEAK))
	{
	    rep_SYM(sym)->car &= ~rep_SF_WEAK;
	    rep_SYM(sym)->car |= rep_SF_WEAK_MOD;
	}
    }
    return ret;
}

DEFUN("set-default", Fset_default, Sset_default,
      (repv sym, repv val), rep_Subr2) /*
::doc:set-default::
set-default SYMBOL VALUE

Sets the default value of SYMBOL to VALUE, then returns VALUE.
::end:: */
{
    int spec;

    rep_DECLARE1(sym, rep_SYMBOLP);

    if (rep_SYM(sym)->car & rep_SF_CONSTANT)
	return Fsignal(Qsetting_constant, rep_LIST_1(sym));

    spec = search_special_environment (sym);
    if (spec)
    {
	if (spec > 0 && rep_SYM(sym)->car & rep_SF_WEAK_MOD)
	    return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */

	if(rep_CELL8_TYPEP(rep_SYM(sym)->value, rep_Var))
	    rep_VARFUN(rep_SYM(sym)->value)(val);
	else
	    rep_SYM(sym)->value = val;
	rep_SYM(sym)->car |= rep_SF_SPECIAL;
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

    rep_SYM(sym)->prop_list = prop;
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
    rep_DECLARE1(sym, rep_SYMBOLP);
    return((rep_VOIDP(rep_SYM(sym)->value)) ? Qnil : Qt);
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
    rep_DECLARE1(sym, rep_SYMBOLP);
    spec = search_special_environment (sym);
    if (spec == 0)
	return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */

    return rep_SYM(sym)->prop_list;
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

DEFUN("setq-default", Fsetq_default, Ssetq_default, (repv args), rep_SF) /*
::doc:setq-default::
setq-default { SYMBOL FORM }...

Sets the default value of each SYMBOL to the value of its corresponding
FORM evaluated, returns the value of the last evaluation. See also setq.
::end:: */
{
    repv res = Qnil;
    rep_GC_root gc_args;
    rep_PUSHGC(gc_args, args);
    while(rep_CONSP(args) && rep_CONSP(rep_CDR(args)) && rep_SYMBOLP(rep_CAR(args)))
    {
	if(!(res = Feval(rep_CAR(rep_CDR(args)))))
	    goto end;
	if(!Fset_default(rep_CAR(args), res))
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

Make SYMBOL have no value as a variable. If the variable was marked
as being special, this status is removed.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    if (rep_SYM(sym)->car & rep_SF_SPECIAL)
    {
	rep_SYM(sym)->value = rep_void_value;
	rep_SYM(sym)->car &= ~(rep_SF_SPECIAL | rep_SF_WEAK | rep_SF_WEAK_MOD);
    }
    else
    {
	repv tem = search_environment (sym);
	if (rep_CONSP(tem))
	    rep_CDR(tem) = rep_void_value;
	else if (tem == Qt)
	    rep_SYM(sym)->value = rep_void_value;
    }
    return(sym);
}

DEFSTRING(no_symbol, "No symbol to bind to in let");

static repv
do_let (repv args, repv (*bind)(repv, repv, repv), void (*unbind)(repv))
{
    repv tmp, *store, oldvals, res = rep_NULL;
    int numsyms = 0;
    if(!rep_CONSP(args))
	return rep_NULL;
    oldvals = Qnil;
    for(tmp = rep_CAR(args); rep_CONSP(tmp); numsyms++)
	tmp = rep_CDR(tmp);
    if(numsyms == 0)
	return(Fprogn(rep_CDR(args)));

    store = alloca(sizeof(repv) * numsyms);
    if(store != NULL)
    {
	int i;
	rep_GC_root gc_args;
	rep_GC_n_roots gc_store;
	rep_PUSHGC(gc_args, args);
	rep_PUSHGCN(gc_store, store, 0);
	i = 0;
	tmp = rep_CAR(args);
	while(rep_CONSP(tmp))
	{
	    if(rep_CONSP(rep_CAR(tmp)))
	    {
		if(!(store[i] = Fprogn(rep_CDR(rep_CAR(tmp)))))
		{
		    rep_POPGCN; rep_POPGC;
		    goto end;
		}
	    }
	    else
		store[i] = Qnil;
	    tmp = rep_CDR(tmp);
	    i++;
	    gc_store.count = i;
	}
	rep_POPGCN;
	rep_POPGC;
	i = 0;
	tmp = rep_CAR(args);
	while(rep_CONSP(tmp))
	{
	    repv sym;
	    switch(rep_TYPE(rep_CAR(tmp)))
	    {
	    case rep_Symbol:
		sym = rep_CAR(tmp);
		break;
	    case rep_Cons:
		sym = rep_CAR(rep_CAR(tmp));
		if(rep_SYMBOLP(sym))
		    break;
		/* FALL THROUGH */
	    default:
		Fsignal(Qerror, rep_LIST_1(rep_VAL(&no_symbol)));
		goto end;
	    }
	    if(!(oldvals = bind (oldvals, sym, store[i])))
		goto end;
	    tmp = rep_CDR(tmp);
	    i++;
	}
	rep_PUSHGC(gc_args, oldvals);
	res = Fprogn(rep_CDR(args));
	rep_POPGC;
end:
	unbind (oldvals);
	return(res);
    }
    return rep_NULL;
}

static repv
do_letstar (repv args, repv (*bind)(repv, repv, repv), void (*unbind)(repv))
{
    repv binds, res = rep_NULL;
    repv oldvals = Qnil;
    rep_GC_root gc_args, gc_oldvals;
    if(!rep_CONSP(args))
	return rep_NULL;
    binds = rep_CAR(args);
    rep_PUSHGC(gc_args, args);
    rep_PUSHGC(gc_oldvals, oldvals);
    while(rep_CONSP(binds))
    {
	if(rep_CONSP(rep_CAR(binds)))
	{
	    if(rep_SYMBOLP(rep_CAR(rep_CAR(binds))))
	    {
		repv val;
		if(!(val = Fprogn(rep_CDR(rep_CAR(binds)))))
		    goto error;
		if(!(oldvals = bind (oldvals, rep_CAR(rep_CAR(binds)), val)))
		    goto error;
	    }
	}
	else
	{
	    if(!(oldvals = bind (oldvals, rep_CAR(binds), Qnil)))
		goto error;
	}
	binds = rep_CDR(binds);
    }
    res = Fprogn(rep_CDR(args));
error:
    rep_POPGC; rep_POPGC;
    unbind (oldvals);
    return(res);
}

DEFUN("let", Flet, Slet, (repv args), rep_SF) /*
::doc:let::
let (SYMBOL-BINDINGS...) BODY...

Binds temporary values to symbols while BODY is being evaluated.
Each SYMBOL-BINDING is either a symbol, in which case that symbol is bound to
nil, or a list. The symbol at the head of this list is bound to the progn'ed
value of the forms making up the tail. ie,
  (let
      ((foo 1 2 3)
       bar)
    (cons foo bar))
   => (3 . nil)

All values of the new bindings are evaluated before any symbols are bound.
::end:: */
{
    return do_let (args, rep_bind_symbol, rep_unbind_symbols);
}

DEFUN("let*", Fletstar, Sletstar, (repv args), rep_SF) /*
::doc:let*::
let* (SYMBOL-BINDINGS...) BODY...

Binds temporary values to symbols while BODY is being evaluated.
Each SYMBOL-BINDING is either a symbol, in which case that symbol is bound to
nil, or a list. The symbol at the head of this list is bound to the progn'ed
value of the forms making up the tail. ie,
  (let*
      ((foo 1 2 3)
       bar)
    (cons foo bar))
   => (3 . nil)

The value of each binding is evaluated just before that symbol is bound,
this means that,
  (setq x 'foo)
  (let*
      ((x 10)
       (y x))
    (cons x y))
   => (10 . 10)
::end:: */
{
    return do_letstar (args, rep_bind_symbol, rep_unbind_symbols);
}

DEFUN("get", Fget, Sget, (repv sym, repv prop), rep_Subr2) /*
::doc:get::
get SYMBOL PROPERTY

Returns the value of SYMBOL's property PROPERTY. See `put'.
::end:: */
{
    repv plist;
    rep_DECLARE1(sym, rep_SYMBOLP);
    plist = rep_SYM(sym)->prop_list;
    while(rep_CONSP(plist) && rep_CONSP(rep_CDR(plist)))
    {
	if(rep_CAR(plist) == prop)
	    return(rep_CAR(rep_CDR(plist)));
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
    repv plist;
    int spec;
    rep_DECLARE1(sym, rep_SYMBOLP);
    spec = search_special_environment (sym);
    if (spec == 0)
	return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */

    plist = rep_SYM(sym)->prop_list;
    while(rep_CONSP(plist) && rep_CONSP(rep_CDR(plist)))
    {
	if(rep_CAR(plist) == prop)
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
    plist = Fcons(prop, Fcons(val, rep_SYM(sym)->prop_list));
    if(plist)
    {
	rep_SYM(sym)->prop_list = plist;
	return val;
    }
    return rep_NULL;
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

DEFUN("set-const-variable", Fset_const_variable, Sset_const_variable, (repv sym, repv stat), rep_Subr2) /*
::doc:set-const-variable::
set-const-variable SYMBOL

Flags that the value of SYMBOL may not be changed.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    if(rep_NILP(stat))
	rep_SYM(sym)->car |= rep_SF_CONSTANT;
    else
	rep_SYM(sym)->car &= ~rep_SF_CONSTANT;
    return(sym);
}

DEFUN("const-variable-p", Fconst_variable_p, Sconst_variable_p, (repv sym), rep_Subr1) /*
::doc:const-variable-p::
const-variable-p SYMBOL

Return t is `set-const-variable' has been called on SYMBOL.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    if(rep_SYM(sym)->car & rep_SF_CONSTANT)
	return(Qt);
    return(Qnil);
}

DEFUN("make-variable-special", Fmake_variable_special,
      Smake_variable_special, (repv sym), rep_Subr1)
{
    int spec;
    rep_DECLARE1(sym, rep_SYMBOLP);
    spec = search_special_environment (sym);
    if (spec == 0)
	return Fsignal (Qvoid_value, rep_LIST_1(sym));	/* XXX */
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

DEFUN("obarray", Vobarray, Sobarray, (repv val), rep_Var) /*
::doc:obarray::
The obarray used by the Lisp reader.
::end:: */
{
    if(val && rep_VECTORP(val))
	rep_obarray = val;
    return(rep_obarray);
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
    rep_SYM(Qnil)->value = Qnil;
    rep_SYM(Qnil)->prop_list = Qnil;
    rep_SYM(Qnil)->car |= rep_SF_CONSTANT | rep_SF_SPECIAL;

    rep_INTERN_SPECIAL(t);
    rep_SYM(Qt)->value = Qt;
    rep_SYM(Qt)->car |= rep_SF_CONSTANT;

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
    rep_ADD_SUBR(Sdefine_value);
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
    rep_ADD_SUBR(Ssetq_default);
    rep_ADD_SUBR(Smakunbound);
    rep_ADD_SUBR(Slet);
    rep_ADD_SUBR(Sletstar);
    rep_ADD_SUBR(Sget);
    rep_ADD_SUBR(Sput);
    rep_ADD_SUBR(Sapropos);
    rep_ADD_SUBR(Sset_const_variable);
    rep_ADD_SUBR(Sconst_variable_p);
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
