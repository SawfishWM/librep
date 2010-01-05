/* fluids.c -- anonymous dynamic bindings

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
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "repint.h"

DEFSYM (fluid, "fluid");

/* XXX give fluids their own distinct type..? */

#define FLUIDP(x) rep_CONSP(x)
#define FLUID_GLOBAL_VALUE(x) rep_CDR(x)


/* from symbols.c */

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



DEFUN ("make-fluid", Fmake_fluid, Smake_fluid, (repv value), rep_Subr1) /*
::doc:rep.lang.interpreter#make-fluid::
make-fluid [VALUE]

Create and return an object representing a `fluid' value--an anonymous
dynamically bound variable.

If VALUE is defined the initial value of the fluid is VALUE, otherwise
it is the symbol `nil'.
::end:: */
{
    return Fcons (Qfluid, value);
}

/* hardcoded in lispmach.c */
DEFUN ("fluid", Ffluid, Sfluid, (repv f), rep_Subr1) /*
::doc:rep.lang.interpreter#fluid::
fluid FLUID

Return the value of the most recently created binding of the fluid
variable object FLUID.
::end:: */
{
    repv tem;
    rep_DECLARE1(f, FLUIDP);

    tem = search_special_bindings (f);
    if (tem != Qnil)
	return rep_CDR (tem);
    else
	return FLUID_GLOBAL_VALUE (f);
}

/* hardcoded in lispmach.c */
DEFUN ("fluid-set", Ffluid_set, Sfluid_set, (repv f, repv v), rep_Subr2) /*
::doc:rep.lang.interpreter#fluid-set::
fluid-set FLUID VALUE

Set the value of the most recently created binding of the fluid
variable object FLUID to VALUE.
::end:: */
{
    repv tem;
    rep_DECLARE1(f, FLUIDP);

    tem = search_special_bindings (f);
    if (tem != Qnil)
	rep_CDR (tem) = v;
    else
	FLUID_GLOBAL_VALUE (f) = v;
    return v;
}

DEFUN ("with-fluids", Fwith_fluids, Swith_fluids,
       (repv fluids, repv values, repv thunk), rep_Subr3) /*
::doc:rep.lang.interpreter#with-fluids::
with-fluids FLUIDS VALUES THUNK

Call THUNK and return the value that it returns with new bindings
created for each of the fluid variables specified in the list FLUIDS.
For each member of FLUIDS the corresponding member of the VALUES list
provides the initial value of the new binding.

If the lists FLUIDS and VALUES are not of the same length, an error is
signalled.
::end:: */
{
    repv ret;
    repv old_bindings;
    rep_GC_root gc_old_bindings;

    rep_DECLARE (1, fluids, rep_LISTP (fluids));
    rep_DECLARE (2, values, rep_LISTP (values));
    rep_DECLARE (2, values,
		 rep_list_length (fluids) == rep_list_length (values));

    old_bindings = rep_special_bindings;
    while (rep_CONSP (fluids) && rep_CONSP (values))
    {
	repv f = rep_CAR (fluids), v = rep_CAR (values);
	rep_DECLARE (1, f, FLUIDP (f));
	rep_special_bindings = Fcons (Fcons (f, v), rep_special_bindings);
	fluids = rep_CDR (fluids);
	values = rep_CDR (values);
	rep_TEST_INT;
	if (rep_INTERRUPTP)
	{
	    rep_special_bindings = old_bindings;
	    return rep_NULL;
	}
    }

    rep_PUSHGC (gc_old_bindings, old_bindings);
    ret = rep_call_lisp0 (thunk);
    rep_POPGC;
    rep_special_bindings = old_bindings;
    return ret;
}



void
rep_fluids_init (void)
{
    repv tem = rep_push_structure ("rep.lang.interpreter");
    rep_INTERN (fluid);
    rep_ADD_SUBR (Smake_fluid);
    rep_ADD_SUBR (Sfluid);
    rep_ADD_SUBR (Sfluid_set);
    rep_ADD_SUBR (Swith_fluids);
    rep_pop_structure (tem);
}
