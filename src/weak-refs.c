/* weak-refs.c -- 

   Copyright (C) 2001 John Harper <jsh@pixelslut.com>

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

#define _GNU_SOURCE

#include "repint.h"

#define WEAKP(x)	rep_CELL16_TYPEP(x, weak_ref_type ())
#define WEAK(v)		((rep_tuple *) rep_PTR (v))
#define WEAK_NEXT(v)	(WEAK(v)->a)
#define WEAK_REF(v)	(WEAK(v)->b)

static repv weak_refs;

static int weak_ref_type (void);

DEFUN ("make-weak-ref", Fmake_weak_ref, Smake_weak_ref, (repv ref), rep_Subr1)
{
    repv weak_ref;

    weak_ref = rep_make_tuple (weak_ref_type (), rep_NULL, rep_NULL);
    WEAK_REF (weak_ref) = ref;
    WEAK_NEXT (weak_ref) = weak_refs;
    weak_refs = weak_ref;

    return weak_ref;
}

DEFUN ("weak-ref", Fweak_ref, Sweak_ref, (repv weak), rep_Subr1)
{
    rep_DECLARE1 (weak, WEAKP);
    return WEAK_REF (weak);
}
    
DEFUN ("weak-ref-set", Fweak_ref_set, Sweak_ref_set, (repv weak, repv value), rep_Subr2)
{
    rep_DECLARE1 (weak, WEAKP);
    WEAK_REF (weak) = value;
    return value;
}

void
rep_scan_weak_refs (void)
{
    repv ref = weak_refs;
    weak_refs = rep_NULL;
    while (ref != rep_NULL)
    {
	repv next = WEAK_NEXT (ref);
	if (rep_GC_CELL_MARKEDP (ref))
	{
	    /* this ref wasn't gc'd */
	    WEAK_NEXT (ref) = weak_refs;
	    weak_refs = ref;

	    if (rep_CELLP (WEAK_REF (ref))
		&& !rep_GC_MARKEDP (WEAK_REF (ref)))
	    {
		/* but the object it points to was */
		WEAK_REF (ref) = Qnil;
	    }
	}
	ref = next;
    }
}

static void
weak_ref_print (repv stream, repv arg)
{
    rep_stream_puts (stream, "#<weak-reference>", -1, rep_FALSE);
}

static int
weak_ref_type (void)
{
    static int type;

    if (type == 0)
    {
	type = rep_register_new_type ("weak-ref", rep_ptr_cmp,
				      weak_ref_print, weak_ref_print,
				      0, 0, 0, 0, 0, 0, 0, 0, 0);
    }

    return type;
}

void
rep_weak_refs_init (void)
{
    repv tem = rep_push_structure ("rep.data");
    rep_ADD_SUBR(Smake_weak_ref);
    rep_ADD_SUBR(Sweak_ref);
    rep_ADD_SUBR(Sweak_ref_set);
    rep_pop_structure (tem);
}
