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

/* Main storage of symbols.  */
repv rep_obarray;

DEFSYM(nil, "nil");
DEFSYM(t, "t");

DEFSYM(variable_documentation, "variable-documentation");
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

DEFUN("make-symbol", Fmake_symbol, Smake_symbol, (repv name), rep_Subr1) /*
::doc:Smake-symbol::
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
	rep_SYM(sym)->function = rep_void_value;
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
#ifdef rep_DUMPED
    {
	rep_symbol *sym;
	for(sym = &rep_dumped_symbols_start; sym < &rep_dumped_symbols_end; sym++)
	{
	    if(!rep_GC_CELL_MARKEDP(rep_VAL(sym)))
	    {
		/* Don't put this on the free list. There may
		   still be references to it from other dumped
		   constants (that aren't marked).. */
	    }
	    else
	    {
		rep_GC_CLR_CELL(rep_VAL(sym));
		rep_used_symbols++;
	    }
	}
    }
#endif /* rep_DUMPED */
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
	if(subr->car == rep_Var)
	    rep_SYM(sym)->value = rep_VAL(subr);
	else
	    rep_SYM(sym)->function = rep_VAL(subr);
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
	rep_SYM(sym)->car |= rep_SF_CONSTANT;
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

DEFUN("make-rep_obarray", Fmake_obarray, Smake_obarray, (repv size), rep_Subr1) /*
::doc:Smake-obarray::
make-rep_obarray SIZE

Creates a new structure for storing symbols in. This is basically a vector
with a few slight differences (all elements initialised to a special value).
::end:: */
{
    rep_DECLARE1(size, rep_INTP);
    return(Fmake_vector(size, OB_NIL));
}

DEFUN("find-symbol", Ffind_symbol, Sfind_symbol, (repv name, repv ob), rep_Subr2) /*
::doc:Sfind-symbol::
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
::doc:Sintern-symbol::
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
::doc:Sintern::
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
::doc:Sunintern::
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

/* This give SYMBOL a new value, saving the old one onto the front of
   the list OLDLIST. OLDLIST is structured like,
     ((SYMBOL . OLDVALUE) ...)
   Returns the new version of OLDLIST.   */
repv
rep_bind_symbol(repv oldList, repv symbol, repv newVal)
{
    repv newbl = Fcons(Fcons(symbol, Qnil), oldList);
    if(newbl)
    {
	/* Binding to buffer-local values is a recipe for disaster; when
	   the binding is removed the current buffer may be different to
	   when the binding was created. This would result in the wrong
	   value being removed. So binding always works on the *default*
	   value of a variable; this also won't work properly with
	   buffer-local variables but hopefully it's less destructive... */
	rep_CDR(rep_CAR(newbl)) = Fdefault_value(symbol, Qt);
	Fset_default(symbol, newVal);
    }
    return(newbl);
}

/* Undoes what the above function does.  */
void
rep_unbind_symbols(repv oldList)
{
    while(rep_CONSP(oldList))
    {
	repv tmp = rep_CAR(oldList);
	Fset_default(rep_CAR(tmp), rep_CDR(tmp));
	oldList = rep_CDR(oldList);
    }
}

DEFUN("symbol-value", Fsymbol_value, Ssymbol_value, (repv sym, repv no_err), rep_Subr2) /*
::doc:Ssymbol-value::
symbol-value SYMBOL

Returns the value of SYMBOL, if SYMBOL is flagged as having buffer-local
values look for one of those first.
::end:: */
/* Second argument (NO-ERR) means don't signal an error if the value is
   void. */
{
    repv val = rep_void_value;
    rep_DECLARE1(sym, rep_SYMBOLP);

    if(rep_SYM(sym)->car & rep_SF_LOCAL)
	val = (*rep_deref_local_symbol_fun)(sym);
    if(val == rep_void_value)
	val = rep_SYM(sym)->value;

    if(val && (rep_CELL8_TYPEP(val, rep_Var)))
    {
	val = rep_VARFUN(val)(rep_NULL);
	if(val == rep_NULL)
	    val = rep_void_value;
    }
    if(rep_NILP(no_err) && (rep_VOIDP(val)))
	return(Fsignal(Qvoid_value, rep_LIST_1(sym)));
    else
	return(val);
}

DEFUN_INT("set", Fset, Sset, (repv sym, repv val), rep_Subr2, "vVariable:" rep_DS_NL "xNew value of %s:") /*
::doc:Sset::
set SYMBOL repv

Sets the value of SYMBOL to repv. If SYMBOL has a buffer-local binding
in the current buffer or `make-variable-buffer-local' has been called on
SYMBOL the buffer-local value in the current buffer is set. Returns repv.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    if(rep_SYM(sym)->car & rep_SF_CONSTANT)
	return(Fsignal(Qsetting_constant, rep_LIST_1(sym)));
    if(rep_SYM(sym)->car & rep_SF_LOCAL)
    {
	repv tem = (*rep_set_local_symbol_fun)(sym, val);
	if (tem != rep_NULL)
	    return tem;
	/* Fall through and set the default value. */
    }
    if(rep_SYM(sym)->value && (rep_CELL8_TYPEP(rep_SYM(sym)->value, rep_Var)))
	rep_VARFUN(rep_SYM(sym)->value)(val);
    else
	rep_SYM(sym)->value = val;
    return(val);
}

DEFUN("setplist", Fsetplist, Ssetplist, (repv sym, repv prop), rep_Subr2) /*
::doc:Ssetplist::
setplist SYMBOL PROP-LIST

Sets the property list of SYMBOL to PROP-LIST, returns PROP-LIST.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    rep_SYM(sym)->prop_list = prop;
    return(prop);
}

DEFUN("symbol-name", Fsymbol_name, Ssymbol_name, (repv sym), rep_Subr1) /*
::doc:Ssymbol-name::
symbol-name SYMBOL

Returns the print-name of SYMBOL.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    return(rep_SYM(sym)->name);
}

DEFUN("symbol-function", Fsymbol_function, Ssymbol_function, (repv sym, repv no_err), rep_Subr2) /*
::doc:Ssymbol-function::
symbol-function SYMBOL

Returns the function value of SYMBOL.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    if(rep_NILP(no_err) && (rep_VOIDP(rep_SYM(sym)->function)))
	return(Fsignal(Qvoid_function, rep_LIST_1(sym)));
    else
	return(rep_SYM(sym)->function);
}

DEFUN("default-value", Fdefault_value, Sdefault_value, (repv sym, repv no_err), rep_Subr2) /*
::doc:Sdefault-value::
default-value SYMBOL

Returns the default value of the symbol SYMBOL. This will be the value of
SYMBOL in buffers or windows which do not have their own local value.
::end:: */
{
    repv val;
    rep_DECLARE1(sym, rep_SYMBOLP);
    if(rep_SYM(sym)->value && (rep_CELL8_TYPEP(rep_SYM(sym)->value, rep_Var)))
	val = rep_VARFUN(rep_SYM(sym)->value)(rep_NULL);
    else
	val = rep_SYM(sym)->value;
    if(rep_NILP(no_err) && rep_VOIDP(val))
	return(Fsignal(Qvoid_value, rep_LIST_1(sym)));
    else
	return(val);
}

DEFUN("default-boundp", Fdefault_boundp, Sdefault_boundp, (repv sym), rep_Subr1) /*
::doc:Sdefault-boundp::
default-boundp SYMBOL

Returns t if SYMBOL has a default value.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    return((rep_VOIDP(rep_SYM(sym)->value)) ? Qnil : Qt);
}

DEFUN("set-default", Fset_default, Sset_default, (repv sym, repv val), rep_Subr2) /*
::doc:Sset-default::
set-default SYMBOL repv

Sets the default value of SYMBOL to repv, then returns repv.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    if(rep_SYM(sym)->value && (rep_CELL8_TYPEP(rep_SYM(sym)->value, rep_Var)))
	rep_VARFUN(rep_SYM(sym)->value)(val);
    else
	rep_SYM(sym)->value = val;
    return(val);
}

DEFUN("fboundp", Ffboundp, Sfboundp, (repv sym), rep_Subr1) /*
::doc:Sfboundp::
fboundp SYMBOL

Returns t if the function-slot of SYMBOL has a value.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    return(rep_VOIDP(Fsymbol_function(sym, Qt)) ? Qnil : Qt);
}

DEFUN("boundp", Fboundp, Sboundp, (repv sym), rep_Subr1) /*
::doc:Sboundp::
boundp SYMBOL

Returns t if SYMBOL has a value as a variable.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    return(rep_VOIDP(Fsymbol_value(sym, Qt)) ? Qnil : Qt);
}

DEFUN("symbol-plist", Fsymbol_plist, Ssymbol_plist, (repv sym), rep_Subr1) /*
::doc:Ssymbol-plist::
symbol-plist SYMBOL

Returns the property-list of SYMBOL.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    return(rep_SYM(sym)->prop_list);
}

DEFUN("gensym", Fgensym, Sgensym, (void), rep_Subr0) /*
::doc:Sgensym::
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
::doc:Ssymbolp::
symbolp ARG

Returns t if ARG is a symbol.
::end:: */
{
    return(rep_SYMBOLP(sym) ? Qt : Qnil);
}

DEFUN("setq", Fsetq, Ssetq, (repv args), rep_SF) /*
::doc:Ssetq::
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
::doc:Ssetq-default::
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

DEFUN("fset", Ffset, Sfset, (repv sym, repv val), rep_Subr2) /*
::doc:Sfset::
fset SYMBOL repv

Sets the function value of SYMBOL to repv, returns repv.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    rep_SYM(sym)->function = val;
    return(val);
}

DEFUN("makunbound", Fmakunbound, Smakunbound, (repv sym), rep_Subr1) /*
::doc:Smakunbound::
makunbound SYMBOL

Make SYMBOL have no value as a variable.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    rep_SYM(sym)->value = rep_void_value;
    return(sym);
}

DEFUN("fmakunbound", Ffmakunbound, Sfmakunbound, (repv sym), rep_Subr1) /*
::doc:Sfmakunbound::
fmakunbound SYMBOL

Make the function slot of SYMBOL have no value.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    rep_SYM(sym)->function = rep_void_value;
    return(sym);
}

DEFSTRING(no_symbol, "No symbol to bind to in let");

DEFUN("let", Flet, Slet, (repv args), rep_SF) /*
::doc:Slet::
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
	    if(!(oldvals = rep_bind_symbol(oldvals, sym, store[i])))
		goto end;
	    tmp = rep_CDR(tmp);
	    i++;
	}
	rep_PUSHGC(gc_args, oldvals);
	res = Fprogn(rep_CDR(args));
	rep_POPGC;
end:
	rep_unbind_symbols(oldvals);
	return(res);
    }
    return rep_NULL;
}

DEFUN("let*", Fletstar, Sletstar, (repv args), rep_SF) /*
::doc:Sletstar::
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
		if(!(oldvals = rep_bind_symbol(oldvals, rep_CAR(rep_CAR(binds)), val)))
		    goto error;
	    }
	}
	else
	{
	    if(!(oldvals = rep_bind_symbol(oldvals, rep_CAR(binds), Qnil)))
		goto error;
	}
	binds = rep_CDR(binds);
    }
    res = Fprogn(rep_CDR(args));
error:
    rep_POPGC; rep_POPGC;
    rep_unbind_symbols(oldvals);
    return(res);
}

DEFUN("get", Fget, Sget, (repv sym, repv prop), rep_Subr2) /*
::doc:Sget::
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
::doc:Sput::
put SYMBOL PROPERTY repv

Sets the value of SYMBOL's property PROPERTY to repv, this value can be
retrieved with the `get' function.
::end:: */
{
    repv plist;
    rep_DECLARE1(sym, rep_SYMBOLP);
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
::doc:Sapropos::
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
::doc:Sset-const-variable::
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
::doc:Sconst-variable-p::
const-variable-p SYMBOL

Return t is `set-const-variable' has been called on SYMBOL.
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    if(rep_SYM(sym)->car & rep_SF_CONSTANT)
	return(Qt);
    return(Qnil);
}

DEFUN_INT("trace", Ftrace, Strace, (repv sym), rep_Subr1, "aFunction to trace") /*
::doc:Strace::
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
::doc:Suntrace::
untrace SYMBOL

Cancel the effect of (trace SYMBOL).
::end:: */
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    rep_SYM(sym)->car &= ~rep_SF_DEBUG;
    return(sym);
}

DEFUN("obarray", Vobarray, Sobarray, (repv val), rep_Var) /*
::doc:Sobarray::
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
    rep_SYM(Qnil)->function = rep_void_value;
    rep_SYM(Qnil)->prop_list = Qnil;
    rep_SYM(Qnil)->car |= rep_SF_CONSTANT;

    rep_INTERN(t);
    rep_SYM(Qt)->value = Qt;
    rep_SYM(Qt)->car |= rep_SF_CONSTANT;

    rep_INTERN(variable_documentation);
    rep_INTERN(permanent_local);
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
    rep_ADD_SUBR(Ssymbol_function);
    rep_ADD_SUBR(Sdefault_value);
    rep_ADD_SUBR(Sdefault_boundp);
    rep_ADD_SUBR(Sset_default);
    rep_ADD_SUBR(Sfboundp);
    rep_ADD_SUBR(Sboundp);
    rep_ADD_SUBR(Ssymbol_plist);
    rep_ADD_SUBR(Sgensym);
    rep_ADD_SUBR(Ssymbolp);
    rep_ADD_SUBR(Ssetq);
    rep_ADD_SUBR(Ssetq_default);
    rep_ADD_SUBR(Sfset);
    rep_ADD_SUBR(Smakunbound);
    rep_ADD_SUBR(Sfmakunbound);
    rep_ADD_SUBR(Slet);
    rep_ADD_SUBR(Sletstar);
    rep_ADD_SUBR(Sget);
    rep_ADD_SUBR(Sput);
    rep_ADD_SUBR(Sapropos);
    rep_ADD_SUBR(Sset_const_variable);
    rep_ADD_SUBR(Sconst_variable_p);
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
