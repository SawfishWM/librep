/* macros.c -- macroexpand etc
   Copyright (C) 1993, 1994, 2000 John Harper <john@dcs.warwick.ac.uk>
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

/* Commentary:

   The idea is to memoize macro expansions, but only until the next
   garbage collection. This introduces very little memory overhead, two
   cons cells per expansion (the expansion is around anyway until gc)

   Whether it would be useful to keep expansions around for longer is
   something that needs to be looked at later..

   It's actually pretty good on its own. E.g. doing (compile-compiler)
   with all interpreted code gives a miss ratio of about .023  */

#define _GNU_SOURCE

#include "repint.h"
#include <string.h>
#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

#define HIST_SIZE 256
#define HIST_HASH_FN(x) (((x) >> 4) % HIST_SIZE)

/* Each entry is a chain of cons cells. But note that the last cell's
   cdr is dotted to ((repv)0) not Qnil */
static repv history[HIST_SIZE];

static int macro_hits, macro_misses;

DEFSYM(macro_environment, "macro-environment");

static inline repv
symbol_value_in_structure (repv structure, repv sym)
{
    repv old = rep_structure, value;
    rep_structure = structure;
    value = Fsymbol_value (sym, Qt);
    rep_structure = old;
    return value;
}

DEFUN("macroexpand-1", Fmacroexpand_1, Smacroexpand_1,
      (repv form, repv env), rep_Subr2) /*
::doc:rep.lang.interpreter#macroexpand-1::
macroexpand-1 FORM [ENVIRONMENT]

If FORM is a macro call, expand it once and return the resulting form.

If ENVIRONMENT is specified it is a function to call to do the actual
expansion. Any macro expanders recursively calling macroexpand should
pass the value of the `macro-environment' variable to this parameter.
::end:: */
{
    rep_GC_root gc_bindings;
    repv car, bindings;

    if (!rep_CONSP (form))
	return form;

    if (env != Qnil && Ffunctionp (env) != Qnil)
	return rep_call_lisp1 (env, form);

again:
    car = rep_CAR(form);
    if(rep_SYMBOLP(car))
    {
	if (rep_STRUCTUREP (env))
	    /* deref the symbol in the module that it appeared in.. */
	    car = symbol_value_in_structure (env, car);
	else
	    car = Fsymbol_value (car, Qt);
	if (!rep_CONSP(car) || rep_CAR(car) != Qmacro)
	    return form;
	car = rep_CDR(car);
    }
    else if (rep_CONSP(car) && rep_CAR(car) == Qmacro)
	car = rep_CDR(car);

    if (Ffunctionp(car) == Qnil)
	return form;

    if (rep_FUNARGP (car))
    {
	repv fun = rep_FUNARG (car)->fun;
	if (rep_CONSP (fun) && rep_CAR (fun) == Qautoload)
	{
	    /* an autoload. handle this locally. */
	    struct rep_Call lc;
	    rep_GC_root gc_form, gc_env;
	    lc.fun = Qnil;
	    lc.args = Qnil;

	    rep_PUSH_CALL (lc);
	    rep_USE_FUNARG (car);
	    rep_PUSHGC (gc_form, form);
	    rep_PUSHGC (gc_env, env);
	    car = rep_load_autoload (car);
	    rep_POPGC; rep_POPGC;
	    rep_POP_CALL (lc);

	    if (car != rep_NULL)
		goto again;
	    else
		return rep_NULL;
	}
    }

    bindings = rep_bind_symbol (Qnil, Qmacro_environment, rep_structure);
    rep_PUSHGC(gc_bindings, bindings);
    form = rep_funcall (car, rep_CDR(form), rep_FALSE);
    rep_POPGC;
    rep_unbind_symbols (bindings);
    return form;
}

DEFUN("macroexpand", Fmacroexpand, Smacroexpand,
      (repv form, repv env), rep_Subr2) /*
::doc:rep.lang.interpreter#macroexpand::
macroexpand FORM [ENVIRONMENT]

If FORM is a macro call, expand it until it isn't.

If ENVIRONMENT is specified it is a function to call to do the actual
expansion. Any macro expanders recursively calling macroexpand should
pass the value of the `macro-environment' variable to this parameter.
::end:: */
{
    repv input = form, pred, ptr;
    rep_GC_root gc_input, gc_pred;

    if (!rep_CONSP (form))
	return form;

    /* Search the history */
    ptr = history[HIST_HASH_FN(form)];
    while (ptr != 0)
    {
	if (rep_CAAR (ptr) == form)
	{
	    macro_hits++;
	    return rep_CDAR (ptr);
	}
	ptr = rep_CDR (ptr);
    }
    macro_misses++;

    rep_PUSHGC(gc_input, input);
    rep_PUSHGC(gc_pred, pred);
    pred = form;
    while (1)
    {
	form = Fmacroexpand_1 (pred, env);
	if (form == rep_NULL || form == pred)
	    break;
	pred = form;
    }
    rep_POPGC; rep_POPGC;

    if (form != rep_NULL)
    {
	/* Cache for future use */
	unsigned int hash = HIST_HASH_FN(input);
	history[hash] = Fcons (Fcons (input, form), history[hash]);
    }

    return form;
}

void
rep_macros_before_gc (void)
{
    /* XXX Perhaps be more discerning? (We would need to arrange some
       XXX marking then though..) */
    rep_macros_clear_history ();
}

void
rep_macros_clear_history (void)
{
    memset (history, 0, sizeof (history));
}

void
rep_macros_init (void)
{
    repv tem = rep_push_structure ("rep.lang.interpreter");
    rep_ADD_SUBR(Smacroexpand);
    rep_ADD_SUBR(Smacroexpand_1);
    rep_INTERN_SPECIAL(macro_environment);
    Fset (Qmacro_environment, Qnil);
    rep_macros_clear_history ();
    rep_pop_structure (tem);
}
