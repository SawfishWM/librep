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

     ftp://ftp.cs.indiana.edu/pub/scheme-repository/doc/pubs/opaque.ps.gz */

#define _GNU_SOURCE

#include "repint.h"

static int datum_type;

/* List of (ID . PRINTER) */
static repv printer_alist;

#define DATUMP(x) rep_CELL16_TYPEP(x, datum_type)
#define DATUM(x) ((datum *) rep_PTR (x))

#define DATUM_ID(x) (rep_TUPLE(x)->a)
#define DATUM_VALUE(x) (rep_TUPLE(x)->b)

/* support for scheme boolean type */
repv rep_scm_t, rep_scm_f;


/* type hooks */

static void
datum_print (repv stream, repv arg)
{
    repv printer = Fassq (DATUM_ID (arg), printer_alist);
    if (printer && rep_CONSP (printer) && rep_CDR (printer) != Qnil)
	rep_call_lisp2 (rep_CDR (printer), arg, stream);
    else if (rep_SYMBOLP (DATUM_ID (arg)))
    {
	rep_stream_puts (stream, "#<datum ", -1, rep_FALSE);
	rep_stream_puts (stream, rep_PTR (rep_SYM (DATUM_ID (arg))->name), -1, rep_TRUE);
	rep_stream_putc (stream, '>');
    }
    else
	rep_stream_puts (stream, "#<datum>", -1, rep_FALSE);
}


/* lisp functions */

DEFUN ("make-datum", Fmake_datum,
       Smake_datum, (repv value, repv id), rep_Subr2) /*
::doc:make-datum::
make-datum VALUE ID

Create and return a new data object of type ID (an arbitrary value), it
will have object VALUE associated with it.
::end:: */
{
    return rep_make_tuple (datum_type, id, value);
}

DEFUN ("define-datum-printer", Fdefine_datum_printer,
       Sdefine_datum_printer, (repv id, repv printer), rep_Subr2) /*
::doc:define-datum-printer::
define-datum-printer ID PRINTER

Register a custom printer for all datums with type ID. When these
objects printed are, the function PRINTER will be called with two
arguments, the datum and the stream to print to.
::end:: */
{
    repv cell = Fassq (id, printer_alist);
    if (cell && rep_CONSP (cell))
	rep_CDR (cell) = printer;
    else
	printer_alist = Fcons (Fcons (id, printer), printer_alist);
    return printer;
}

DEFUN ("datum-ref", Fdatum_ref, Sdatum_ref, (repv obj, repv id), rep_Subr2) /*
::doc:datum-ref::
datum-ref DATUM ID

If data object DATUM has type ID, return its associated value, else
signal an error.
::end:: */
{
    rep_DECLARE (1, obj, DATUMP (obj) && DATUM_ID (obj) == id);
    return DATUM_VALUE (obj);
}

DEFUN ("datum-set", Fdatum_set, Sdatum_set,
       (repv obj, repv id, repv value), rep_Subr3) /*
::doc:datum-set::
datum-set DATUM ID VALUE

If data object DATUM has type ID, modify its associated value to be
VALUE, else signal an error.
::end:: */
{
    rep_DECLARE (1, obj, DATUMP (obj) && DATUM_ID (obj) == id);
    DATUM_VALUE (obj) = value;
    return value;
}

DEFUN ("has-type-p", Fhas_type_p,
       Shas_type_p, (repv arg, repv id), rep_Subr2) /*
::doc:has-type-p::
has-type-p ARG ID

Return `t' if object ARG has data type ID (and thus was initially
created using the `make-datum' function).
::end:: */
{
    return (DATUMP (arg) && DATUM_ID (arg) == id) ? Qt : Qnil;
}


/* support for scheme boolean values */

DEFUN ("%scheme-bool-printer", F_scheme_bool_printer,
       S_scheme_bool_printer, (repv obj, repv stream), rep_Subr2)
{
    rep_stream_puts (stream, obj == rep_scm_t ? "#t" : "#f", 2, rep_FALSE);
    return Qnil;
}


/* dl hooks */

void
rep_datums_init (void)
{
    datum_type = rep_register_new_type ("datum", 0, datum_print, datum_print,
					0, rep_mark_tuple,
					0, 0, 0, 0, 0, 0, 0);

    rep_ADD_SUBR (Smake_datum);
    rep_ADD_SUBR (Sdefine_datum_printer);
    rep_ADD_SUBR (Sdatum_ref);
    rep_ADD_SUBR (Sdatum_set);
    rep_ADD_SUBR (Shas_type_p);
    printer_alist = Qnil;
    rep_mark_static (&printer_alist);

    rep_ADD_INTERNAL_SUBR (S_scheme_bool_printer);
    rep_scm_t = Fmake_datum (Qnil, rep_VAL (&S_scheme_bool_printer));
    rep_scm_f = Fmake_datum (Qnil, rep_VAL (&S_scheme_bool_printer));
    Fdefine_datum_printer (rep_VAL (&S_scheme_bool_printer),
			   rep_VAL (&S_scheme_bool_printer));
    rep_mark_static (&rep_scm_t);
    rep_mark_static (&rep_scm_f);
}
