/* datums.c -- user-defined opaque types
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

/* Commentary:

   These were inspired by Rees' The Scheme of Things column:

     ftp://ftp.cs.indiana.edu/pub/scheme-repository/doc/pubs/opaque.ps.gz

   XXX if they get used heavily it would make sense to allocate them in
   XXX blocks of N? */

#define _GNU_SOURCE

#include "repint.h"

typedef struct datum_struct datum;
struct datum_struct {
    repv car;
    datum *next;
    repv id;
    repv printer;
    repv value;
};

static datum *all_datums;
static int datum_type;

#define DATUMP(x) rep_CELL16_TYPEP(x, datum_type)
#define DATUM(x) ((datum *) rep_PTR (x))


/* type hooks */

static void
datum_mark (repv val)
{
    rep_MARKVAL (DATUM (val)->id);
    rep_MARKVAL (DATUM (val)->printer);
    rep_MARKVAL (DATUM (val)->value);
}

static void
datum_sweep (void)
{
    datum *x = all_datums;
    all_datums = 0;
    while (x != 0)
    {
	datum *next = x->next;
	if (!rep_GC_CELL_MARKEDP (rep_VAL(x)))
	    rep_FREE_CELL (x);
	else
	{
	    rep_GC_CLR_CELL (rep_VAL(x));
	    x->next = all_datums;
	    all_datums = x;
	}
	x = next;
    }
}

static void
datum_print (repv stream, repv arg)
{
    if (DATUM (arg)->printer != Qnil)
	rep_call_lisp2 (DATUM (arg)->printer, arg, stream);
    else if (rep_SYMBOLP (DATUM (arg)->id))
    {
	rep_stream_puts (stream, "#<datum ", -1, rep_FALSE);
	rep_stream_puts (stream, rep_PTR (rep_SYM (DATUM (arg)->id)->name), -1, rep_TRUE);
	rep_stream_putc (stream, '>');
    }
    else
	rep_stream_puts (stream, "#<datum>", -1, rep_FALSE);
}


/* lisp functions */

DEFUN ("make-datum", Fmake_datum, Smake_datum,
       (repv value, repv id, repv printer), rep_Subr3)
{
    datum *d = rep_ALLOC_CELL (sizeof (datum));
    d->next = all_datums;
    all_datums = d;
    d->car = datum_type;
    d->id = id;
    d->value = value;
    d->printer = printer;
    return rep_VAL (d);
}

DEFUN ("datum-ref", Fdatum_ref, Sdatum_ref, (repv obj, repv id), rep_Subr2)
{
    rep_DECLARE (1, obj, DATUMP (obj) && DATUM (obj)->id == id);
    return DATUM (obj)->value;
}

DEFUN ("has-type-p", Fhas_type_p,
       Shas_type_p, (repv arg, repv id), rep_Subr2)
{
    return (DATUMP (arg) && DATUM (arg)->id == id) ? Qt : Qnil;
}


/* dl hooks */

void
rep_datums_init (void)
{
    datum_type = rep_register_new_type ("datum", 0, datum_print, datum_print,
					datum_sweep, datum_mark,
					0, 0, 0, 0, 0, 0, 0);

    rep_ADD_SUBR (Smake_datum);
    rep_ADD_SUBR (Sdatum_ref);
    rep_ADD_SUBR (Shas_type_p);
}
