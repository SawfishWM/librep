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

#include "jade.h"
#include "jade_protos.h"
#define BUILD_JADE
#include "regexp/regexp.h"

#ifdef HAVE_ALLOCA
# include <alloca.h>
#endif

#include <string.h>
#include <ctype.h>
#include <stdlib.h>

/* The special value which signifies the end of a hash-bucket chain.
   It can be any Lisp object which isn't a symbol.  */
#define OB_NIL null_string

_PR void symbol_sweep(void);
_PR int symbol_cmp(VALUE, VALUE);
_PR void symbol_princ(VALUE, VALUE);
_PR void symbol_print(VALUE, VALUE);
_PR VALUE add_subr(XSubr *);
_PR VALUE add_const_num(VALUE, long);
_PR void intern_static(VALUE *, VALUE);
_PR VALUE bind_symbol(VALUE, VALUE, VALUE);
_PR void unbind_symbols(VALUE);
_PR int symbols_init(void);
_PR void symbols_kill(void);

/* Main storage of symbols.  */
_PR VALUE obarray;
VALUE obarray;

_PR VALUE sym_nil, sym_t;
VALUE sym_nil, sym_t;

_PR VALUE sym_variable_documentation;
VALUE sym_variable_documentation;

/* This value is stored in the cells of a symbol to denote a void object. */
_PR VALUE void_value;
static LispObject void_object = { V_Void };
VALUE void_value = &void_object;

static SymbolBlk *symbol_block_chain;
static Symbol *symbol_freelist;
_PR int allocated_symbols, used_symbols;
int allocated_symbols, used_symbols;

_PR VALUE cmd_make_symbol(VALUE);
DEFUN("make-symbol", cmd_make_symbol, subr_make_symbol, (VALUE name), V_Subr1, DOC_make_symbol) /*
::doc:make_symbol::
make-symbol NAME

Returns a new, uninterned, symbol with print-name NAME. It's value and
function definition are both void and it has a nil property-list.
::end:: */
{
    VALUE sym;
    DECLARE1(name, STRINGP);
    if(!symbol_freelist)
    {
	SymbolBlk *sb = mycalloc(sizeof(SymbolBlk));
	if(sb)
	{
	    int i;
	    allocated_symbols += SYMBOLBLK_SIZE;
	    sb->sb_Next = symbol_block_chain;
	    symbol_block_chain = sb;
	    for(i = 0; i < (SYMBOLBLK_SIZE - 1); i++)
		sb->sb_Symbols[i].sym_Next = VAL(&sb->sb_Symbols[i + 1]);
	    sb->sb_Symbols[i].sym_Next = VAL(symbol_freelist);
	    symbol_freelist = sb->sb_Symbols;
	}
    }
    if((sym = VAL(symbol_freelist)))
    {
	symbol_freelist = VSYM(VSYM(sym)->sym_Next);
	VSYM(sym)->sym_Next = NULL;
	VSYM(sym)->sym_Type = V_Symbol;
	VSYM(sym)->sym_Flags = 0;
	VSYM(sym)->sym_Name = name;
	VSYM(sym)->sym_Value = void_value;
	VSYM(sym)->sym_Function = void_value;
	VSYM(sym)->sym_PropList = sym_nil;
	used_symbols++;
	data_after_gc += sizeof(Symbol);
    }
    return(sym);
}

void
symbol_sweep(void)
{
    SymbolBlk *sb = symbol_block_chain;
    symbol_freelist = NULL;
    used_symbols = 0;
    while(sb)
    {
	int i;
	SymbolBlk *nxt = sb->sb_Next;
	for(i = 0; i < SYMBOLBLK_SIZE; i++)
	{
	    if(!GC_MARKEDP(VAL(&sb->sb_Symbols[i])))
	    {
		sb->sb_Symbols[i].sym_Next = VAL(symbol_freelist);
		symbol_freelist = &sb->sb_Symbols[i];
	    }
	    else
	    {
		GC_CLR(VAL(&sb->sb_Symbols[i]));
		used_symbols++;
	    }
	}
	sb = nxt;
    }
}

int
symbol_cmp(VALUE v1, VALUE v2)
{
    if(VTYPE(v1) == VTYPE(v2))
	return(!(VSYM(v1) == VSYM(v2)));
    return(1);
}

void
symbol_princ(VALUE strm, VALUE obj)
{
    stream_puts(strm, VSTR(VSYM(obj)->sym_Name), -1, TRUE);
}

void
symbol_print(VALUE strm, VALUE obj)
{
    u_char *s = VSTR(VSYM(obj)->sym_Name);
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
	    stream_putc(strm, (int)'\\');
	    break;
	default:
	    if(iscntrl(c))
		stream_putc(strm, (int)'\\');
	    break;
	}
	stream_putc(strm, (int)c);
    }
}

VALUE
add_subr(XSubr *subr)
{
    VALUE sym = cmd_intern(subr->subr_Name, obarray);
    if(sym)
    {
	if(subr->subr_Type == V_Var)
	{
	    VSYM(sym)->sym_Value = VAL(subr);
	    VSYM(sym)->sym_PropList = cmd_cons(sym_variable_documentation, cmd_cons(make_number(subr->subr_DocIndex), VSYM(sym)->sym_PropList));
	}
	else
	    VSYM(sym)->sym_Function = VAL(subr);
    }
    return(sym);
}

VALUE
add_const_num(VALUE name, long num)
{
    VALUE sym = cmd_intern(name, obarray);
    if(sym)
    {
	VSYM(sym)->sym_Value = make_number(num);
	VSYM(sym)->sym_Flags |= SF_CONSTANT;
    }
    return(sym);
}

void
intern_static(VALUE *symp, VALUE name)
{
    if((*symp = cmd_intern(name, sym_nil)))
	mark_static(symp);
    else
	abort();
}

static INLINE u_long
hash(u_char *str)
{
    register u_long value = 0;
    while(*str)
	value = (value * 33) + *str++;
    return(value);
}

_PR VALUE cmd_make_obarray(VALUE);
DEFUN("make-obarray", cmd_make_obarray, subr_make_obarray, (VALUE size), V_Subr1, DOC_make_obarray) /*
::doc:make_obarray::
make-obarray SIZE

Creates a new structure for storing symbols in. This is basically a vector
with a few slight differences (all elements initialised to a special value).
::end:: */
{
    DECLARE1(size, NUMBERP);
    return(cmd_make_vector(size, OB_NIL));
}

_PR VALUE cmd_find_symbol(VALUE, VALUE);
DEFUN("find-symbol", cmd_find_symbol, subr_find_symbol, (VALUE name, VALUE ob), V_Subr2, DOC_find_symbol) /*
::doc:find_symbol::
find-symbol NAME [OBARRAY]

Returns the symbol with print-name NAME, found by searching OBARRAY (or
the default `obarray' if nil), or nil if no such symbol exists.
::end:: */
{
    int vsize;
    DECLARE1(name, STRINGP);
    if(!VECTORP(ob))
	ob = obarray;
    if((vsize = VVECT(ob)->vc_Size) == 0)
	return(sym_nil);
    ob = VVECT(ob)->vc_Array[hash(VSTR(name)) % vsize];
    while(SYMBOLP(ob))
    {
	if(!strcmp(VSTR(name), VSTR(VSYM(ob)->sym_Name)))
	    return(ob);
	ob = VSYM(ob)->sym_Next;
    }
    return(sym_nil);
}

_PR VALUE cmd_intern_symbol(VALUE, VALUE);
DEFUN("intern-symbol", cmd_intern_symbol, subr_intern_symbol, (VALUE sym, VALUE ob), V_Subr2, DOC_intern_symbol) /*
::doc:intern_symbol::
intern-symbol SYMBOL [OBARRAY]

Stores SYMBOL in OBARRAY (or the default). If SYMBOL has already been interned
somewhere an error is signalled.
::end:: */
{
    int vsize, hashid;
    DECLARE1(sym, SYMBOLP);
    if(VSYM(sym)->sym_Next != NULL)
    {
	cmd_signal(sym_error, list_2(MKSTR("Symbol is already interned"), sym));
	return(NULL);
    }
    if(!VECTORP(ob))
	ob = obarray;
    if((vsize = VVECT(ob)->vc_Size) == 0)
	return(NULL);
    hashid = hash(VSTR(VSYM(sym)->sym_Name)) % vsize;
    VSYM(sym)->sym_Next = VVECT(ob)->vc_Array[hashid];
    VVECT(ob)->vc_Array[hashid] = sym;
    return(sym);
}

_PR VALUE cmd_intern(VALUE, VALUE);
DEFUN("intern", cmd_intern, subr_intern, (VALUE name, VALUE ob), V_Subr2, DOC_intern) /*
::doc:intern::
intern NAME [OBARRAY]

If a symbol with print-name exists in OBARRAY (or the default) return it.
Else use `(make-symbol NAME)' to create a new symbol, intern that into the
OBARRAY, then return it.
::end:: */
{
    VALUE sym;
    DECLARE1(name, STRINGP);
    if(!(sym = cmd_find_symbol(name, ob))
       || (NILP(sym) && strcmp(VSTR(name), "nil")))
    {
	sym = cmd_make_symbol(name);
	if(sym)
	    return(cmd_intern_symbol(sym, ob));
    }
    return(sym);
}

_PR VALUE cmd_unintern(VALUE, VALUE);
DEFUN("unintern", cmd_unintern, subr_unintern, (VALUE sym, VALUE ob), V_Subr2, DOC_unintern) /*
::doc:unintern::
unintern SYMBOL [OBARRAY]

Removes SYMBOL from OBARRAY (or the default). Use this with caution.
::end:: */
{
    VALUE list;
    int vsize, hashid;
    DECLARE1(sym, SYMBOLP);
    if(!VECTORP(ob))
	ob = obarray;
    if((vsize = VVECT(ob)->vc_Size) == 0)
	return(NULL);
    hashid = hash(VSTR(VSYM(sym)->sym_Name)) % vsize;
    list = VVECT(ob)->vc_Array[hashid];
    VVECT(ob)->vc_Array[hashid] = NULL;
    while(SYMBOLP(list))
    {
	VALUE nxt = VSYM(list)->sym_Next;
	if(list != sym)
	{
	    VSYM(list)->sym_Next = VVECT(ob)->vc_Array[hashid];
	    VVECT(ob)->vc_Array[hashid] = VAL(list);
	}
	list = nxt;
    }
    VSYM(sym)->sym_Next = NULL;
    return(sym);
}

/* This give SYMBOL a new value, saving the old one onto the front of
   the list OLDLIST. OLDLIST is structured like,
     ((SYMBOL . OLDVALUE) ...)
   Returns the new version of OLDLIST.   */
VALUE
bind_symbol(VALUE oldList, VALUE symbol, VALUE newVal)
{
    VALUE newbl = cmd_cons(cmd_cons(symbol, sym_nil), oldList);
    if(newbl)
    {
	/* Binding to buffer-local values is a recipe for disaster; when
	   the binding is removed the current buffer may be different to
	   when the binding was created. This would result in the wrong
	   value being removed. So binding always works on the *default*
	   value of a variable; this also won't work properly with
	   buffer-local variables but hopefully it's less destructive... */
	VCDR(VCAR(newbl)) = cmd_default_value(symbol, sym_t);
	cmd_set_default(symbol, newVal);
    }
    return(newbl);
}

/* Undoes what the above function does.  */
void
unbind_symbols(VALUE oldList)
{
    while(CONSP(oldList))
    {
	VALUE tmp = VCAR(oldList);
	cmd_set_default(VCAR(tmp), VCDR(tmp));
	oldList = VCDR(oldList);
    }
}

_PR VALUE cmd_symbol_value(VALUE, VALUE);
DEFUN("symbol-value", cmd_symbol_value, subr_symbol_value, (VALUE sym, VALUE no_err), V_Subr2, DOC_symbol_value) /*
::doc:symbol_value::
symbol-value SYMBOL

Returns the value of SYMBOL, if SYMBOL is flagged as having buffer-local
values look for one of those first.
::end:: */
/* Second argument (NO-ERR) means don't signal an error if the value is
   void. */
{
    VALUE val;
    DECLARE1(sym, SYMBOLP);
    if((VSYM(sym)->sym_Flags & SF_BUFFER_LOCAL)
       && (val = cmd_assq(sym, curr_vw->vw_Tx->tx_LocalVariables))
       && CONSP(val))
    {
	val = VCDR(val);
    }
    else
	val = VSYM(sym)->sym_Value;
    if(val && (VTYPE(val) == V_Var))
    {
	val = VVARFUN(val)(NULL);
	if(val == NULL)
	    val = void_value;
    }
    if(NILP(no_err) && (VOIDP(val)))
	return(cmd_signal(sym_void_value, LIST_1(sym)));
    else
	return(val);
}

_PR VALUE cmd_set(VALUE, VALUE);
DEFUN_INT("set", cmd_set, subr_set, (VALUE sym, VALUE val), V_Subr2, DOC_set, "vVariable:\nxNew value of %s:") /*
::doc:set::
set SYMBOL VALUE

Sets the value of SYMBOL to VALUE. If SYMBOL has a buffer-local binding
in the current buffer or `make-variable-buffer-local' has been called on
SYMBOL the buffer-local value in the current buffer is set. Returns VALUE.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    if(VSYM(sym)->sym_Flags & SF_CONSTANT)
	return(cmd_signal(sym_setting_constant, LIST_1(sym)));
    if(VSYM(sym)->sym_Flags & SF_BUFFER_LOCAL)
    {
	TX *tx = curr_vw->vw_Tx;
	VALUE tmp;
	if((tmp = cmd_assq(sym, tx->tx_LocalVariables)) && CONSP(tmp))
	{
	    /* A buffer-local value exists, modify it. */
	    VCDR(tmp) = val;
	    return(val);
	}
	else if(VSYM(sym)->sym_Flags & SF_SET_BUFFER_LOCAL)
	{
	    /* Create a new buffer-local value */
	    tx->tx_LocalVariables = cmd_cons(cmd_cons(sym, val),
					     tx->tx_LocalVariables);
	    return(val);
	}
	/* Fall through and set the default value. */
    }
    if(VSYM(sym)->sym_Value && (VTYPE(VSYM(sym)->sym_Value) == V_Var))
	VVARFUN(VSYM(sym)->sym_Value)(val);
    else
	VSYM(sym)->sym_Value = val;
    return(val);
}

_PR VALUE cmd_setplist(VALUE, VALUE);
DEFUN("setplist", cmd_setplist, subr_setplist, (VALUE sym, VALUE prop), V_Subr2, DOC_setplist) /*
::doc:setplist::
setplist SYMBOL PROP-LIST

Sets the property list of SYMBOL to PROP-LIST, returns PROP-LIST.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    VSYM(sym)->sym_PropList = prop;
    return(prop);
}

_PR VALUE cmd_symbol_name(VALUE);
DEFUN("symbol-name", cmd_symbol_name, subr_symbol_name, (VALUE sym), V_Subr1, DOC_symbol_name) /*
::doc:symbol_name::
symbol-name SYMBOL

Returns the print-name of SYMBOL.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    return(VSYM(sym)->sym_Name);
}

_PR VALUE cmd_symbol_function(VALUE, VALUE);
DEFUN("symbol-function", cmd_symbol_function, subr_symbol_function, (VALUE sym, VALUE no_err), V_Subr2, DOC_symbol_function) /*
::doc:symbol_function::
symbol-function SYMBOL

Returns the function value of SYMBOL.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    if(NILP(no_err) && (VOIDP(VSYM(sym)->sym_Function)))
	return(cmd_signal(sym_void_function, LIST_1(sym)));
    else
	return(VSYM(sym)->sym_Function);
}

_PR VALUE cmd_default_value(VALUE, VALUE);
DEFUN("default-value", cmd_default_value, subr_default_value, (VALUE sym, VALUE no_err), V_Subr2, DOC_default_value) /*
::doc:default_value::
default-value SYMBOL

Returns the default value of the symbol SYMBOL. This will be the value of
SYMBOL in buffers or windows which do not have their own local value.
::end:: */
{
    VALUE val;
    DECLARE1(sym, SYMBOLP);
    if(VSYM(sym)->sym_Value && (VTYPE(VSYM(sym)->sym_Value) == V_Var))
	val = VVARFUN(VSYM(sym)->sym_Value)(NULL);
    else
	val = VSYM(sym)->sym_Value;
    if(NILP(no_err) && VOIDP(val))
	return(cmd_signal(sym_void_value, LIST_1(sym)));
    else
	return(val);
}

_PR VALUE cmd_default_boundp(VALUE);
DEFUN("default-boundp", cmd_default_boundp, subr_default_boundp, (VALUE sym), V_Subr1, DOC_default_boundp) /*
::doc:default_boundp::
default-boundp SYMBOL

Returns t if SYMBOL has a default value.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    return((VOIDP(VSYM(sym)->sym_Value)) ? sym_nil : sym_t);
}

_PR VALUE cmd_set_default(VALUE, VALUE);
DEFUN("set-default", cmd_set_default, subr_set_default, (VALUE sym, VALUE val), V_Subr2, DOC_set_default) /*
::doc:set_default::
set-default SYMBOL VALUE

Sets the default value of SYMBOL to VALUE, then returns VALUE.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    if(VSYM(sym)->sym_Value && (VTYPE(VSYM(sym)->sym_Value) == V_Var))
	VVARFUN(VSYM(sym)->sym_Value)(val);
    else
	VSYM(sym)->sym_Value = val;
    return(val);
}

_PR VALUE cmd_fboundp(VALUE);
DEFUN("fboundp", cmd_fboundp, subr_fboundp, (VALUE sym), V_Subr1, DOC_fboundp) /*
::doc:fboundp::
fboundp SYMBOL

Returns t if the function-slot of SYMBOL has a value.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    return(VOIDP(cmd_symbol_function(sym, sym_t)) ? sym_nil : sym_t);
}

_PR VALUE cmd_boundp(VALUE);
DEFUN("boundp", cmd_boundp, subr_boundp, (VALUE sym), V_Subr1, DOC_boundp) /*
::doc:boundp::
boundp SYMBOL

Returns t if SYMBOL has a value as a variable.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    return(VOIDP(cmd_symbol_value(sym, sym_t)) ? sym_nil : sym_t);
}

_PR VALUE cmd_symbol_plist(VALUE);
DEFUN("symbol-plist", cmd_symbol_plist, subr_symbol_plist, (VALUE sym), V_Subr1, DOC_symbol_plist) /*
::doc:symbol_plist::
symbol-plist SYMBOL

Returns the property-list of SYMBOL.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    return(VSYM(sym)->sym_PropList);
}

_PR VALUE cmd_gensym(void);
DEFUN("gensym", cmd_gensym, subr_gensym, (void), V_Subr0, DOC_gensym) /*
::doc:gensym::
gensym

Returns a new (non-interned) symbol with a unique print name.
::end:: */
{
    static int counter;
    char buf[20];
    counter++;
    sprintf(buf, "G%04d", counter);
    return(cmd_make_symbol(string_dup(buf)));
}

_PR VALUE cmd_symbolp(VALUE);
DEFUN("symbolp", cmd_symbolp, subr_symbolp, (VALUE sym), V_Subr1, DOC_symbolp) /*
::doc:symbolp::
symbolp ARG

Returns t if ARG is a symbol.
::end:: */
{
    return(SYMBOLP(sym) ? sym_t : sym_nil);
}

_PR VALUE cmd_setq(VALUE);
DEFUN("setq", cmd_setq, subr_setq, (VALUE args), V_SF, DOC_setq) /*
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
    VALUE res = sym_nil;
    GCVAL gcv_args;
    PUSHGC(gcv_args, args);
    while(CONSP(args) && CONSP(VCDR(args)) && SYMBOLP(VCAR(args)))
    {
	if(!(res = cmd_eval(VCAR(VCDR(args)))))
	    goto end;
	if(!cmd_set(VCAR(args), res))
	{
	    res = NULL;
	    goto end;
	}
	args = VCDR(VCDR(args));
    }
end:
    POPGC;
    return(res);
}

_PR VALUE cmd_setq_default(VALUE);
DEFUN("setq-default", cmd_setq_default, subr_setq_default, (VALUE args), V_SF, DOC_setq_default) /*
::doc:setq_default::
setq-default { SYMBOL FORM }...

Sets the default value of each SYMBOL to the value of its corresponding
FORM evaluated, returns the value of the last evaluation. See also setq.
::end:: */
{
    VALUE res = sym_nil;
    GCVAL gcv_args;
    PUSHGC(gcv_args, args);
    while(CONSP(args) && CONSP(VCDR(args)) && SYMBOLP(VCAR(args)))
    {
	if(!(res = cmd_eval(VCAR(VCDR(args)))))
	    goto end;
	if(!cmd_set_default(VCAR(args), res))
	{
	    res = NULL;
	    goto end;
	}
	args = VCDR(VCDR(args));
    }
end:
    POPGC;
    return(res);
}

_PR VALUE cmd_fset(VALUE, VALUE);
DEFUN("fset", cmd_fset, subr_fset, (VALUE sym, VALUE val), V_Subr2, DOC_fset) /*
::doc:fset::
fset SYMBOL VALUE

Sets the function value of SYMBOL to VALUE, returns VALUE.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    VSYM(sym)->sym_Function = val;
    return(val);
}

_PR VALUE cmd_makunbound(VALUE);
DEFUN("makunbound", cmd_makunbound, subr_makunbound, (VALUE sym), V_Subr1, DOC_makunbound) /*
::doc:makunbound::
makunbound SYMBOL

Make SYMBOL have no value as a variable.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    VSYM(sym)->sym_Value = NULL;
    return(sym);
}

_PR VALUE cmd_fmakunbound(VALUE);
DEFUN("fmakunbound", cmd_fmakunbound, subr_fmakunbound, (VALUE sym), V_Subr1, DOC_fmakunbound) /*
::doc:fmakunbound::
fmakunbound SYMBOL

Make the function slot of SYMBOL have no value.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    VSYM(sym)->sym_Function = NULL;
    return(sym);
}

_PR VALUE cmd_let(VALUE);
DEFUN("let", cmd_let, subr_let, (VALUE args), V_SF, DOC_let) /*
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
    VALUE tmp, *store, oldvals, res = NULL;
    int numsyms = 0;
    if(!CONSP(args))
	return(NULL);
    oldvals = sym_nil;
    for(tmp = VCAR(args); CONSP(tmp); numsyms++)
	tmp = VCDR(tmp);
    if(numsyms == 0)
	return(cmd_progn(VCDR(args)));

#ifdef HAVE_ALLOCA
    store = alloca(sizeof(VALUE) * numsyms);
#else
    store = str_alloc(sizeof(VALUE) * numsyms);
#endif
    if(store != NULL)
    {
	int i;
	GCVAL gcv_args;
	GCVALN gcv_store;
	PUSHGC(gcv_args, args);
	PUSHGCN(gcv_store, store, 0);
	i = 0;
	tmp = VCAR(args);
	while(CONSP(tmp))
	{
	    if(CONSP(VCAR(tmp)))
	    {
		if(!(store[i] = cmd_progn(VCDR(VCAR(tmp)))))
		{
		    POPGCN; POPGC;
		    goto end;
		}
	    }
	    else
		store[i] = sym_nil;
	    tmp = VCDR(tmp);
	    i++;
	    gcv_store.gcv_N = i;
	}
	POPGCN;
	POPGC;
	i = 0;
	tmp = VCAR(args);
	while(CONSP(tmp))
	{
	    VALUE sym;
	    switch(VTYPE(VCAR(tmp)))
	    {
	    case V_Symbol:
		sym = VCAR(tmp);
		break;
	    case V_Cons:
		sym = VCAR(VCAR(tmp));
		if(SYMBOLP(sym))
		    break;
		/* FALL THROUGH */
	    default:
		cmd_signal(sym_error, LIST_1(MKSTR("No symbol to bind to in let")));
		goto end;
	    }
	    if(!(oldvals = bind_symbol(oldvals, sym, store[i])))
		goto end;
	    tmp = VCDR(tmp);
	    i++;
	}
	PUSHGC(gcv_args, oldvals);
	res = cmd_progn(VCDR(args));
	POPGC;
end:
#ifndef HAVE_ALLOCA
	str_free(store);
#endif
	unbind_symbols(oldvals);
	return(res);
    }
    return(NULL);
}

_PR VALUE cmd_letstar(VALUE);
DEFUN("let*", cmd_letstar, subr_letstar, (VALUE args), V_SF, DOC_letstar) /*
::doc:letstar::
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
    VALUE binds, res = NULL;
    VALUE oldvals = sym_nil;
    GCVAL gcv_args, gcv_oldvals;
    if(!CONSP(args))
	return(NULL);
    binds = VCAR(args);
    PUSHGC(gcv_args, args);
    PUSHGC(gcv_oldvals, oldvals);
    while(CONSP(binds))
    {
	if(CONSP(VCAR(binds)))
	{
	    if(SYMBOLP(VCAR(VCAR(binds))))
	    {
		VALUE val;
		if(!(val = cmd_progn(VCDR(VCAR(binds)))))
		    goto error;
		if(!(oldvals = bind_symbol(oldvals, VCAR(VCAR(binds)), val)))
		    goto error;
	    }
	}
	else
	{
	    if(!(oldvals = bind_symbol(oldvals, VCAR(binds), sym_nil)))
		goto error;
	}
	binds = VCDR(binds);
    }
    res = cmd_progn(VCDR(args));
error:
    POPGC; POPGC;
    unbind_symbols(oldvals);
    return(res);
}

_PR VALUE cmd_get(VALUE, VALUE);
DEFUN("get", cmd_get, subr_get, (VALUE sym, VALUE prop), V_Subr2, DOC_get) /*
::doc:get::
get SYMBOL PROPERTY

Returns the value of SYMBOL's property PROPERTY. See `put'.
::end:: */
{
    VALUE plist;
    DECLARE1(sym, SYMBOLP);
    plist = VSYM(sym)->sym_PropList;
    while(CONSP(plist) && CONSP(VCDR(plist)))
    {
	if(VCAR(plist) == prop)
	    return(VCAR(VCDR(plist)));
	plist = VCDR(VCDR(plist));
    }
    return(sym_nil);
}

_PR VALUE cmd_put(VALUE, VALUE, VALUE);
DEFUN("put", cmd_put, subr_put, (VALUE sym, VALUE prop, VALUE val), V_Subr3, DOC_put) /*
::doc:put::
put SYMBOL PROPERTY VALUE

Sets the value of SYMBOL's property PROPERTY to VALUE, this value can be
retrieved with the `get' function.
::end:: */
{
    VALUE plist;
    DECLARE1(sym, SYMBOLP);
    plist = VSYM(sym)->sym_PropList;
    while(CONSP(plist) && CONSP(VCDR(plist)))
    {
	if(VCAR(plist) == prop)
	{
	    VCAR(VCDR(plist)) = val;
	    return(val);
	}
	plist = VCDR(VCDR(plist));
    }
    plist = cmd_cons(prop, cmd_cons(val, VSYM(sym)->sym_PropList));
    if(plist)
    {
	VSYM(sym)->sym_PropList = plist;
	return(val);
    }
    return(NULL);
}

_PR VALUE cmd_make_local_variable(VALUE);
DEFUN("make-local-variable", cmd_make_local_variable, subr_make_local_variable, (VALUE sym), V_Subr1, DOC_make_local_variable) /*
::doc:make_local_variable::
make-local-variable SYMBOL

Gives the variable SYMBOL a buffer-local binding in the current buffer. It
will be the same as the default value to start with. If the current buffer
alread has a buffer-local binding for SYMBOL nothing happens.
Returns SYMBOL.
::end:: */
{
    VALUE slot;
    TX *tx = curr_vw->vw_Tx;
    DECLARE1(sym, SYMBOLP);
    VSYM(sym)->sym_Flags |= SF_BUFFER_LOCAL;
    slot = cmd_assq(sym, tx->tx_LocalVariables);
    if(!slot || !CONSP(slot))
    {
	/* Need to create a binding. */
	tx->tx_LocalVariables = cmd_cons(cmd_cons(sym, VSYM(sym)->sym_Value),
					 tx->tx_LocalVariables);
    }
    return(sym);
}

_PR VALUE cmd_make_variable_buffer_local(VALUE);
DEFUN("make-variable-buffer-local", cmd_make_variable_buffer_local, subr_make_variable_buffer_local, (VALUE sym), V_Subr1, DOC_make_variable_buffer_local) /*
::doc:make_variable_buffer_local::
make-variable-buffer-local SYMBOL

Marks the variable SYMBOL as being automatically buffer-local. Any attempts
at setting SYMBOL result in the current buffer being given its own binding.
Returns SYMBOL.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    VSYM(sym)->sym_Flags |= (SF_BUFFER_LOCAL | SF_SET_BUFFER_LOCAL);
    return(sym);
}

_PR VALUE cmd_buffer_variables(VALUE);
DEFUN("buffer-variables", cmd_buffer_variables, subr_buffer_variables, (VALUE tx), V_Subr1, DOC_buffer_variables) /*
::doc:buffer_variables::
buffer-variables [BUFFER]

Returns a list of (SYMBOL . VALUE) bindings which take effect when the
current buffer is BUFFER.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    return(VTX(tx)->tx_LocalVariables);
}

_PR VALUE cmd_kill_all_local_variables(VALUE);
DEFUN("kill-all-local-variables", cmd_kill_all_local_variables, subr_kill_all_local_variables, (VALUE tx), V_Subr1, DOC_kill_all_local_variables) /*
::doc:kill_all_local_variables::
kill-all-local-variables [BUFFER]

Remove all buffer-local variables from BUFFER.
::end:: */
{
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    VTX(tx)->tx_LocalVariables = sym_nil;
    return(tx);
}

_PR VALUE cmd_kill_local_variable(VALUE, VALUE);
DEFUN("kill-local-variable", cmd_kill_local_variable, subr_kill_local_variable, (VALUE sym, VALUE tx), V_Subr2, DOC_kill_local_variable) /*
::doc:kill_local_variable::
kill-local-variable SYMBOL [BUFFER]

Remove the buffer-local value of the symbol SYMBOL in the specified buffer.
::end:: */
{
    VALUE list;
    DECLARE1(sym, SYMBOLP);
    if(!BUFFERP(tx))
	tx = VAL(curr_vw->vw_Tx);
    list = VTX(tx)->tx_LocalVariables;
    VTX(tx)->tx_LocalVariables = sym_nil;
    while(CONSP(list))
    {
	VALUE nxt = VCDR(list);
	if(VCAR(list) != sym)
	{
	    VCDR(list) = VTX(tx)->tx_LocalVariables;
	    VTX(tx)->tx_LocalVariables = list;
	}
	list = nxt;
    }
    return(sym);
}

_PR VALUE cmd_apropos(VALUE, VALUE, VALUE);
DEFUN("apropos", cmd_apropos, subr_apropos, (VALUE re, VALUE pred, VALUE ob), V_Subr3, DOC_apropos) /*
::doc:apropos::
apropos REGEXP [PREDICATE] [OBARRAY]

Returns a list of symbols from OBARRAY (or the default) whose print-name
matches the regular-expression REGEXP. If PREDICATE is given and non-nil,
each symbol which matches is applied to the function PREDICATE, if the value
is non-nil it is considered a match.
::end:: */
{
    regexp *prog;
    DECLARE1(re, STRINGP);
    if(!VECTORP(ob))
	ob = obarray;
    prog = regcomp(VSTR(re));
    if(prog)
    {
	VALUE last = sym_nil;
	int i;
	GCVAL gcv_last, gcv_ob, gcv_pred;
	PUSHGC(gcv_last, last);
	PUSHGC(gcv_ob, ob);
	PUSHGC(gcv_pred, pred);
	for(i = 0; i < VVECT(ob)->vc_Size; i++)
	{
	    VALUE chain = VVECT(ob)->vc_Array[i];
	    while(SYMBOLP(chain))
	    {
		if(regexec(prog, VSTR(VSYM(chain)->sym_Name)))
		{
		    if(pred && !NILP(pred))
		    {
			VALUE tmp;
			if(!(tmp = funcall(pred, LIST_1(chain)))
			   || NILP(tmp))
			{
			    goto next;
			}
		    }
		    last = cmd_cons(chain, last);
		}
next:
		chain = VSYM(chain)->sym_Next;
	    }
	}
	POPGC; POPGC; POPGC;
	free(prog);
	return(last);
    }
    return(NULL);
}

_PR VALUE cmd_set_const_variable(VALUE sym, VALUE stat);
DEFUN("set-const-variable", cmd_set_const_variable, subr_set_const_variable, (VALUE sym, VALUE stat), V_Subr2, DOC_set_const_variable) /*
::doc:set_const_variable::
set-const-variable SYMBOL

Flags that the value of SYMBOL may not be changed.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    if(NILP(stat))
	VSYM(sym)->sym_Flags |= SF_CONSTANT;
    else
	VSYM(sym)->sym_Flags &= ~SF_CONSTANT;
    return(sym);
}

_PR VALUE cmd_const_variable_p(VALUE sym);
DEFUN("const-variable-p", cmd_const_variable_p, subr_const_variable_p, (VALUE sym), V_Subr1, DOC_const_variable_p) /*
::doc:const_variable_p::
const-variable-p SYMBOL

Return t is `set-const-variable' has been called on SYMBOL.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    if(VSYM(sym)->sym_Flags & SF_CONSTANT)
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_trace(VALUE sym);
DEFUN_INT("trace", cmd_trace, subr_trace, (VALUE sym), V_Subr1, DOC_trace, "aFunction to trace") /*
::doc:trace::
trace SYMBOL

Flag that whenever SYMBOL is evaluated (as a variable or a function) the
debugger is entered.
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    VSYM(sym)->sym_Flags |= SF_DEBUG;
    return(sym);
}

_PR VALUE cmd_untrace(VALUE sym);
DEFUN_INT("untrace", cmd_untrace, subr_untrace, (VALUE sym), V_Subr1, DOC_untrace, "aFunction to untrace") /*
::doc:untrace::
untrace SYMBOL

Cancel the effect of (trace SYMBOL).
::end:: */
{
    DECLARE1(sym, SYMBOLP);
    VSYM(sym)->sym_Flags &= ~SF_DEBUG;
    return(sym);
}

_PR VALUE var_obarray(VALUE val);
DEFUN("obarray", var_obarray, subr_obarray, (VALUE val), V_Var, DOC_obarray) /*
::doc:obarray::
The obarray used by the Lisp reader.
::end:: */
{
    if(val && VECTORP(val))
	obarray = val;
    return(obarray);
}

int
symbols_init(void)
{
    obarray = cmd_make_obarray(make_number(OBSIZE));
    if(obarray)
    {
	mark_static(&obarray);

	/* fiddly details of initialising the first symbol */
	sym_nil = cmd_intern(MKSTR("nil"), obarray);
	mark_static(&sym_nil);
	VSYM(sym_nil)->sym_Value = sym_nil;
	VSYM(sym_nil)->sym_PropList = sym_nil;
	VSYM(sym_nil)->sym_Flags &= SF_CONSTANT;

	INTERN(sym_t, "t");
	VSYM(sym_t)->sym_Value = sym_t;
	VSYM(sym_t)->sym_Flags &= SF_CONSTANT;

	INTERN(sym_variable_documentation, "variable-documentation");
	ADD_SUBR(subr_make_symbol);
	ADD_SUBR(subr_make_obarray);
	ADD_SUBR(subr_find_symbol);
	ADD_SUBR(subr_intern_symbol);
	ADD_SUBR(subr_intern);
	ADD_SUBR(subr_unintern);
	ADD_SUBR(subr_symbol_value);
	ADD_SUBR(subr_set);
	ADD_SUBR(subr_setplist);
	ADD_SUBR(subr_symbol_name);
	ADD_SUBR(subr_symbol_function);
	ADD_SUBR(subr_default_value);
	ADD_SUBR(subr_default_boundp);
	ADD_SUBR(subr_set_default);
	ADD_SUBR(subr_fboundp);
	ADD_SUBR(subr_boundp);
	ADD_SUBR(subr_symbol_plist);
	ADD_SUBR(subr_gensym);
	ADD_SUBR(subr_symbolp);
	ADD_SUBR(subr_setq);
	ADD_SUBR(subr_setq_default);
	ADD_SUBR(subr_fset);
	ADD_SUBR(subr_makunbound);
	ADD_SUBR(subr_fmakunbound);
	ADD_SUBR(subr_let);
	ADD_SUBR(subr_letstar);
	ADD_SUBR(subr_get);
	ADD_SUBR(subr_put);
	ADD_SUBR(subr_make_local_variable);
	ADD_SUBR(subr_make_variable_buffer_local);
	ADD_SUBR(subr_buffer_variables);
	ADD_SUBR(subr_kill_all_local_variables);
	ADD_SUBR(subr_kill_local_variable);
	ADD_SUBR(subr_apropos);
	ADD_SUBR(subr_set_const_variable);
	ADD_SUBR(subr_const_variable_p);
	ADD_SUBR(subr_trace);
	ADD_SUBR(subr_untrace);
	ADD_SUBR(subr_obarray);
	return(TRUE);
    }
    return(FALSE);
}

void
symbols_kill(void)
{
    SymbolBlk *sb = symbol_block_chain;
    while(sb)
    {
	SymbolBlk *nxt = sb->sb_Next;
	myfree(sb);
	sb = nxt;
    }
    symbol_block_chain = NULL;
}
