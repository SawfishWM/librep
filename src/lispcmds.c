/* lispcmds.c -- Lots of standard Lisp functions
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
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define _GNU_SOURCE

#include "repint.h"
#include "build.h"

#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

DEFSTRING(default_rep_directory, REP_DIRECTORY);
DEFSTRING(dot, ".");

static repv default_suffixes;

DEFSYM(or, "or");
DEFSYM(and, "and");
DEFSYM(load_path, "load-path");
DEFSYM(dl_load_path, "dl-load-path");
DEFSYM(after_load_alist, "after-load-alist");
DEFSYM(provide, "provide");
DEFSYM(rep_directory, "rep-directory");
DEFSYM(lisp_lib_directory, "lisp-lib-directory");
DEFSYM(site_lisp_directory, "site-lisp-directory");
DEFSYM(exec_directory, "exec-directory");
DEFSYM(documentation_file, "documentation-file");
DEFSYM(documentation_files, "documentation-files");
DEFSYM(_load_suffixes, "%load-suffixes");
DEFSYM(dl_load_reloc_now, "dl-load-reloc-now");
DEFSYM(load_filename, "load-filename"); /*
::doc:load-path::
A list of directory names. When `load' opens a lisp-file it searches each
directory named in this list in turn until the file is found or the list
is exhausted.
::end::
::doc:dl-load-path::
List of directories searched for dynamically loaded object files.
::end::
::doc:after-load-alist::
A list of (LIBRARY FORMS...). Whenever the `load' command reads a file
of Lisp code LIBRARY, it executes each of FORMS. Note that LIBRARY must
exactly match the FILE argument given to `load'.
::end::
::doc:rep-directory::
The directory in which all installed data files live.
::end::
::doc:lisp-lib-directory::
The name of the directory in which the standard lisp files live.
::end::
::doc:site-lisp-directory::
The name of the directory in which site-specific Lisp files are stored.
::end::
::doc:exec-directory::
The name of the directory containing architecture specific files.
::end::
::doc:documentation-file::
The name of the database containing the lisp-library's documentation strings.
::end::
::doc:documentation-files::
A list of database names containing all documentation strings.
::end::
::doc:dl-load-reloc-now::
When non-nil, dynamically loaded libraries have all symbol relocations
perfromed at load-time, not as required.
::end::
::doc:load-filename::
While using the `load' function to load a Lisp library, this variable is
set to the name of the file being loaded.
::end:: */

DEFUN("quote", Fquote, Squote, (repv args, repv tail_posn), rep_SF) /*
::doc:rep.lang.interpreter#quote::
quote ARG
'ARG

Returns ARG.
::end:: */
{
    if(rep_CONSP(args))
	return(rep_CAR(args));
    return rep_signal_missing_arg(1);
}

DEFUN("lambda", Flambda, Slambda, (repv args, repv tail_posn), rep_SF) /*
::doc:rep.lang.interpreter#lambda::
lambda LAMBDA-LIST BODY...

Evaluates to an anonymous function.
::end:: */
{
    if(rep_CONSP(args))
	return Fmake_closure (Fcons (Qlambda, args), Qnil);
    else
	return rep_signal_missing_arg(1);
}

DEFUN("car", Fcar, Scar, (repv cons), rep_Subr1) /*
::doc:rep.data#car::
car CONS-CELL

Returns the value stored in the car slot of CONS-CELL, or nil if CONS-CELL
is nil.
::end:: */
{
    if(rep_CONSP(cons))
	return(rep_CAR(cons));
    return(Qnil);
}
DEFUN("cdr", Fcdr, Scdr, (repv cons), rep_Subr1) /*
::doc:rep.data#cdr::
cdr CONS-CELL

Returns the value stored in the cdr slot of CONS-CELL, or nil if CONS-CELL
is nil.
::end:: */
{
    if(rep_CONSP(cons))
	return(rep_CDR(cons));
    return(Qnil);
}

DEFUN("list", Flist, Slist, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#list::
list ARGS...

Returns a new list with elements ARGS...
::end:: */
{
    repv lst = Qnil;
    int i;

    for (i = argc - 1; i >= 0; i--)
    {
	lst = Fcons (argv[i], lst);
    }

    return lst;
}

DEFUN("list*", Flist_star, Slist_star, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#list*::
list* ARG1 ARG2 ... ARGN

Returns a new list (ARG1 ARG2 ... ARGN-1 . ARGN). That is, the same as from
`list' but the last argument is dotted to the last but one argument.
::end:: */
{
    repv lst;
    int i;

    if (argc == 0)
	return Qnil;

    lst = argv[argc - 1];
    for (i = argc - 2; i >= 0; i--)
    {
	lst = Fcons (argv[i], lst);
    }

    return lst;
}

DEFUN("make-list", Fmake_list, Smake_list, (repv len, repv init), rep_Subr2) /*
::doc:rep.data#make-list::
make-list LENGTH [INITIAL-repv]

Returns a new list with LENGTH members, each of which is initialised to
INITIAL-repv, or nil.
::end:: */
{
    int i;
    repv list = Qnil;
    rep_DECLARE1(len, rep_INTP);
    if(rep_INT(len) < 0)
	return rep_signal_arg_error(len, 1);
    for(i = 0; list != rep_NULL && i < rep_INT(len); i++)
	list = Fcons(init, list);
    return(list);
}

DEFUN("append", Fappend, Sappend, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#append::
append LISTS...

Non-destructively concatenates each of it's argument LISTS... into one
new list which is returned.
::end:: */
{
    int i;
    repv res = Qnil, *res_end = &res;

    for (i = 0; i < argc; i++)
    {
	if (i != argc - 1)
	{
	    if (!rep_LISTP(argv[i]))
		return rep_signal_arg_error (argv[i], i + 1);

	    /* Only make a new copy if there's another list after this one. */
	    *res_end = rep_copy_list (argv[i]);
	}
	else
	    *res_end = argv[i];

	while (rep_CONSP (*res_end))
	{
	    rep_TEST_INT;
	    if (rep_INTERRUPTP)
		return rep_NULL;
	    res_end = rep_CDRLOC (*res_end);
	}
    }

    return res;
}

DEFUN("nconc", Fnconc_, Snconc, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#nconc::
nconc LISTS...

Destructively concatenates each of it's argument LISTS... into one new
list. Every LIST but the last is modified so that it's last cdr points
to the beginning of the next list. Returns the new list.
::end:: */
{
    int i;
    repv res = Qnil, *res_end = &res;

    for (i = 0; i < argc; i++)
    {
	if (i != argc - 1)
	{
	    if (!rep_LISTP (argv[i]))
		return rep_signal_arg_error (argv[i], i + 1);

	    if (!rep_CONS_WRITABLE_P (argv[i]))
		return Fsignal (Qsetting_constant, rep_LIST_1 (argv[i]));
	}

	*res_end = argv[i];

	while (rep_CONSP (*res_end))
	{
	    rep_TEST_INT;
	    if (rep_INTERRUPTP)
		return rep_NULL;
	    res_end = rep_CDRLOC (*res_end);
	}
    }

    return res;
}

DEFUN("rplaca", Frplaca, Srplaca, (repv cons, repv car), rep_Subr2) /*
::doc:rep.data#rplaca::
rplaca CONS-CELL NEW-CAR

Sets the value of the car slot in CONS-CELL to NEW-CAR.
Returns the CONS-CELL.
::end:: */
{
    rep_DECLARE1(cons, rep_CONSP);
    if(!rep_CONS_WRITABLE_P(cons))
	return Fsignal(Qsetting_constant, rep_LIST_1(cons));
    rep_CAR(cons) = car;
    return(cons);
}

DEFUN("rplacd", Frplacd, Srplacd, (repv cons, repv cdr), rep_Subr2) /*
::doc:rep.data#rplacd::
rplacd CONS-CELL NEW-CDR

Sets the value of the cdr slot in CONS-CELL to NEW-CDR.
Returns the CONS-CELL.
::end:: */
{
    rep_DECLARE1(cons, rep_CONSP);
    if(!rep_CONS_WRITABLE_P(cons))
	return Fsignal(Qsetting_constant, rep_LIST_1(cons));
    rep_CDR(cons) = cdr;
    return(cons);
}

DEFUN("reverse", Freverse, Sreverse, (repv head), rep_Subr1) /*
::doc:rep.data#reverse::
reverse LIST

Returns a new list which is a copy of LIST except that the members are in
reverse order.
::end:: */
{
    repv res = Qnil;
    rep_DECLARE1(head, rep_LISTP);
    while(rep_CONSP(head))
    {
	res = Fcons(rep_CAR(head), res);
	head = rep_CDR(head);
	rep_TEST_INT;
	if(res == rep_NULL || rep_INTERRUPTP)
	    return(rep_NULL);
    }
    return(res);
}

DEFUN("nreverse", Fnreverse, Snreverse, (repv head), rep_Subr1) /*
::doc:rep.data#nreverse::
nreverse LIST

Returns LIST altered so that it's members are in reverse order to what they
were. This function is destructive towards it's argument.
::end:: */
{
    repv res = Qnil;
    repv nxt;
    rep_DECLARE1(head, rep_LISTP);
    if(rep_NILP(head))
	return(head);
    if(!rep_CONS_WRITABLE_P(head))
	return Fsignal(Qsetting_constant, rep_LIST_1(head));
    do {
	if(rep_CONSP(rep_CDR(head)))
	    nxt = rep_CDR(head);
	else
	    nxt = rep_NULL;
	rep_CDR(head) = res;
	res = head;
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    return(rep_NULL);
    } while((head = nxt) != rep_NULL);
    return(res);
}

DEFUN("assoc", Fassoc, Sassoc, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#assoc::
assoc ELT ASSOC-LIST

Searches ASSOC-LIST for a list whose first element is ELT. `assoc' uses
`equal' to compare elements. Returns the sub-list starting from the first 
matching association.
For example,
    (assoc 'three '((one . 1) (two . 2) (three . 3) (four . 4)))
     => (three . 3)
::end:: */
{
    rep_DECLARE2(list, rep_LISTP);
    while(rep_CONSP(list))
    {
	register repv car = rep_CAR(list);
	if(rep_CONSP(car) && (!rep_value_cmp(elt, rep_CAR(car))))
	    return(car);
	list = rep_CDR(list);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    return(rep_NULL);
    }
    return(Qnil);
}

DEFUN("assq", Fassq, Sassq, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#assq::
assq ELT ASSOC-LIST

Searches ASSOC-LIST for a list whose first element is ELT. `assq' uses `eq'
to compare elements. Returns the sub-list starting from the first matching
association.
::end:: */
{
    rep_DECLARE2(list, rep_LISTP);
    while(rep_CONSP(list))
    {
	register repv car = rep_CAR(list);
	if(rep_CONSP(car) && (elt == rep_CAR(car)))
	    return(car);
	list = rep_CDR(list);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    return(rep_NULL);
    }
    return(Qnil);
}

DEFUN("rassoc", Frassoc, Srassoc, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#rassoc::
rassoc ELT ASSOC-LIST

Searches ASSOC-LIST for a cons-cell whose cdr element is `equal' to ELT. 
Returns the first cons-cell which matches, or nil.
For example,
    (rassoc 3 '((one . 1) (two . 2) (three . 3) (four . 4)))
     => (three . 3)
::end:: */
{
    rep_DECLARE2(list, rep_LISTP);
    while(rep_CONSP(list))
    {
	register repv car = rep_CAR(list);
	if(rep_CONSP(car) && (!rep_value_cmp(elt, rep_CDR(car))))
	    return(car);
	list = rep_CDR(list);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    return(rep_NULL);
    }
    return(Qnil);
}

DEFUN("rassq", Frassq, Srassq, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#rassq::
rassq ELT ASSOC-LIST

Searches ASSOC-LIST for a cons-cell whose cdr is `eq' to ELT.
Returns the first matching cons-cell, else nil.
::end:: */
{
    rep_DECLARE2(list, rep_LISTP);
    while(rep_CONSP(list))
    {
	register repv car = rep_CAR(list);
	if(rep_CONSP(car) && (elt == rep_CDR(car)))
	    return(car);
	list = rep_CDR(list);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    return(rep_NULL);
    }
    return(Qnil);
}

DEFUN("nth", Fnth, Snth, (repv index, repv list), rep_Subr2) /*
::doc:rep.data#nth::
nth INDEX LIST

Returns the INDEXth element of LIST. The first element has an INDEX of zero.
::end:: */
{
    int i;
    rep_DECLARE1(index, rep_INTP);
    rep_DECLARE2(list, rep_LISTP);
    i = rep_INT(index);
    if(i < 0)
	return rep_signal_arg_error(index, 1);
    while((i-- > 0) && rep_CONSP(list))
    {
	list = rep_CDR(list);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    return rep_NULL;
    }
    return (i <= 0 && rep_CONSP(list)) ? rep_CAR(list) : Qnil;
}

DEFUN("nthcdr", Fnthcdr, Snthcdr, (repv index, repv list), rep_Subr2) /*
::doc:rep.data#nthcdr::
nthcdr INDEX LIST

Returns the INDEXth cdr of LIST. The first is INDEX zero.
::end:: */
{
    int i;
    rep_DECLARE1(index, rep_INTP);
    rep_DECLARE2(list, rep_LISTP);
    i = rep_INT(index);
    if(i < 0)
	return rep_signal_arg_error(index, 1);
    while((i-- > 0) && rep_CONSP(list))
    {
	list = rep_CDR(list);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    return rep_NULL;
    }
    return list;
}

DEFUN("last", Flast, Slast, (repv list), rep_Subr1) /*
::doc:rep.data#last::
last LIST

Returns the last element of LIST.
::end:: */
{
    rep_DECLARE1(list, rep_LISTP);
    if(rep_CONSP(list))
    {
	while(rep_CONSP(rep_CDR(list)))
	{
	    list = rep_CDR(list);
	    rep_TEST_INT;
	    if(rep_INTERRUPTP)
		return(rep_NULL);
	}
	return(rep_CAR(list));
    }
    return(Qnil);
}

DEFUN("mapcar", Fmapcar, Smapcar, (repv fun, repv list), rep_Subr2) /*
::doc:rep.data#mapcar::
mapcar FUNCTION LIST

Calls FUNCTION-NAME with each element of LIST as an argument in turn and
returns a new list constructed from the results, ie,
  (mapcar (function (lambda (x) (1+ x))) '(1 2 3))
   => (2 3 4)
::end:: */
{
    repv res = Qnil;
    repv *last = &res;
    rep_GC_root gc_list, gc_fun, gc_res;
    rep_DECLARE2(list, rep_LISTP);

    rep_PUSHGC(gc_res, res);
    rep_PUSHGC(gc_fun, fun);
    rep_PUSHGC(gc_list, list);
    while(res != rep_NULL && rep_CONSP(list))
    {
	rep_TEST_INT;
	if(rep_INTERRUPTP
	   || !(*last = Fcons(Qnil, Qnil))
	   || !(rep_CAR(*last) = rep_call_lisp1(fun, rep_CAR(list))))
	    res = rep_NULL;
	else
	{
	    last = &rep_CDR(*last);
	    list = rep_CDR(list);
	}
    }
    rep_POPGC; rep_POPGC; rep_POPGC;
    return res;
}

DEFUN("mapc", Fmapc, Smapc, (repv fun, repv list), rep_Subr2) /*
::doc:rep.data#mapc::
mapc FUNCTION LIST

Applies FUNCTION to each element in LIST, discards the results.
::end:: */
{
    repv res = Qnil;
    rep_GC_root gc_fun, gc_list;
    rep_DECLARE2(list, rep_LISTP);
    rep_PUSHGC(gc_fun, fun);
    rep_PUSHGC(gc_list, list);
    while(res != rep_NULL && rep_CONSP(list))
    {
	rep_TEST_INT;
	if(rep_INTERRUPTP || !rep_call_lisp1(fun, rep_CAR(list)))
	    res = rep_NULL;
	list = rep_CDR(list);
    }
    rep_POPGC; rep_POPGC;
    return res;
}

DEFUN("filter", Ffilter, Sfilter, (repv pred, repv list), rep_Subr2) /*
::doc:rep.data#filter::
filter PREDICATE LIST

Return a new list, consisting of the elements in LIST which the function
PREDICATE returns t when applied to; i.e. something like

(mapcar 'nconc (mapcar #'(lambda (x)
			   (when (PREDICATE x)
			     (list x)))
		       LIST))
::end:: */
{
    repv output = Qnil, *ptr = &output;
    rep_GC_root gc_pred, gc_list, gc_output;
    rep_DECLARE2(list, rep_LISTP);
    rep_PUSHGC(gc_pred, pred);
    rep_PUSHGC(gc_list, list);
    rep_PUSHGC(gc_output, output);
    while(rep_CONSP(list))
    {
	repv tem = rep_call_lisp1(pred, rep_CAR(list));
	rep_TEST_INT;
	if(tem == rep_NULL || rep_INTERRUPTP)
	{
	    output = rep_NULL;
	    break;
	}
	if(!rep_NILP(tem))
	{
	    *ptr = Fcons(rep_CAR(list), Qnil);
	    ptr = &rep_CDR(*ptr);
	}
	list = rep_CDR(list);
    }
    rep_POPGC; rep_POPGC; rep_POPGC;
    return output;
}

DEFUN("member", Fmember, Smember, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#member::
member ELT LIST

If ELT is a member of list LIST then return the tail of the list starting
from the matched ELT, ie,
  (member 1 '(2 1 3))
   => (1 3)
`member' uses `equal' to compare atoms.
::end:: */
{
    rep_DECLARE2(list, rep_LISTP);
    while(rep_CONSP(list))
    {
	if(!rep_value_cmp(elt, rep_CAR(list)))
	    return(list);
	list = rep_CDR(list);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    return(rep_NULL);
    }
    return(Qnil);
}

DEFUN("memq", Fmemq, Smemq, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#memq::
memq ELT LIST

If ELT is a member of list LIST then return the tail of the list starting
from the matched ELT, ie,
  (memq 1 '(2 1 3))
   => (1 3)
`memq' uses `eq' to compare atoms.
::end:: */
{
    rep_DECLARE2(list, rep_LISTP);
    while(rep_CONSP(list))
    {
	if(elt == rep_CAR(list))
	    return(list);
	list = rep_CDR(list);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    return(rep_NULL);
    }
    return(Qnil);
}

DEFUN("memql", Fmemql, Smemql, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#memql::
memql ELT LIST

If ELT is a member of list LIST then return the tail of the list starting
from the matched ELT. `memql' uses `eql' to compare list items.
::end:: */
{
    rep_DECLARE2 (list, rep_LISTP);
    while (rep_CONSP (list))
    {
	if (elt == rep_CAR (list))
	    return list;
	else
	{
	    repv tem = Feql (elt, rep_CAR (list));
	    if (tem && tem != Qnil)
		return list;
	}
	list = rep_CDR (list);
	rep_TEST_INT;
	if (rep_INTERRUPTP)
	    return rep_NULL;
    }
    return Qnil;
}

DEFUN("delete", Fdelete, Sdelete, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#delete::
delete ELT LIST

Returns LIST with any members `equal' to ELT destructively removed.
::end:: */
{
    repv *head = &list;
    rep_DECLARE2(list, rep_LISTP);
    while(rep_CONSP(*head))
    {
	if(!rep_value_cmp(elt, rep_CAR(*head)))
	    *head = rep_CDR(*head);
	else
	    head = &rep_CDR(*head);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    return(rep_NULL);
    }
    return(list);
}

DEFUN("delq", Fdelq, Sdelq, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#delq::
delq ELT LIST

Returns LIST with any members `eq' to ELT destructively removed.
::end:: */
{
    repv *head = &list;
    rep_DECLARE2(list, rep_LISTP);
    while(rep_CONSP(*head))
    {
	if(elt == rep_CAR(*head))
	    *head = rep_CDR(*head);
	else
	    head = &rep_CDR(*head);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    return(rep_NULL);
    }
    return(list);
}

DEFUN("delete-if", Fdelete_if, Sdelete_if, (repv pred, repv list), rep_Subr2) /*
::doc:rep.data#delete-if::
delete-if FUNCTION LIST

Similar to `delete' except that a predicate function, FUNCTION-NAME, is
used to decide which elements to delete (remove destructively).
`delete-if' deletes an element if FUNCTION-NAME returns non-nil when 
applied to that element, ie,
  (delete-if '(lambda (x) (= x 1)) '(1 2 3 4 1 2))
   => (2 3 4 2)
::end:: */
{
    repv *head = &list;
    rep_GC_root gc_list, gc_pred;
    rep_DECLARE2(list, rep_LISTP);
    rep_PUSHGC(gc_list, list);
    rep_PUSHGC(gc_pred, pred);
    while(rep_CONSP(*head))
    {
	repv tmp = rep_call_lisp1(pred, rep_CAR(*head));
	rep_TEST_INT;
	if(rep_INTERRUPTP || !tmp)
	{
	    list = rep_NULL;
	    break;
	}
	if(!rep_NILP(tmp))
	    *head = rep_CDR(*head);
	else
	    head = &rep_CDR(*head);
    }
    rep_POPGC; rep_POPGC;
    return list;
}

DEFUN("delete-if-not", Fdelete_if_not, Sdelete_if_not, (repv pred, repv list), rep_Subr2) /*
::doc:rep.data#delete-if-not::
delete-if-not FUNCTION LIST

Similar to `delete' except that a predicate function, FUNCTION-NAME, is
used to decide which elements to delete (remove destructively).
`delete-if-not' deletes an element if FUNCTION-NAME returns nil when 
applied to that element, ie,
  (delete-if-not '(lambda (x) (= x 1)) '(1 2 3 4 1 2))
   => (1 1)
::end:: */
{
    repv *head = &list;
    rep_GC_root gc_list, gc_pred;
    rep_DECLARE2(list, rep_LISTP);
    rep_PUSHGC(gc_list, list);
    rep_PUSHGC(gc_pred, pred);
    while(rep_CONSP(*head))
    {
	repv tmp = rep_call_lisp1(pred, rep_CAR(*head));
	rep_TEST_INT;
	if(rep_INTERRUPTP || !tmp)
	{
	    list = rep_NULL;
	    break;
	}
	if(rep_NILP(tmp))
	    *head = rep_CDR(*head);
	else
	    head = &rep_CDR(*head);
    }
    rep_POPGC; rep_POPGC;
    return list;
}

DEFUN("vector", Fvector, Svector, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#vector::
vector ARGS...

Returns a new vector with ARGS... as its elements.
::end:: */
{
    repv vec = rep_make_vector (argc);

    if(vec != rep_NULL)
    {
	memcpy (rep_VECT (vec)->array, argv, argc * sizeof (repv));
    }

    return vec;
}

DEFUN("make-vector", Fmake_vector, Smake_vector, (repv size, repv init), rep_Subr2) /*
::doc:rep.data#make-vector::
make-vector SIZE [INITIAL-repv]

Creates a new vector of size SIZE. If INITIAL-repv is provided each element
will be set to that value, else they will all be nil.
::end:: */
{
    int len;
    repv res;
    rep_DECLARE1(size, rep_INTP);
    if(rep_INT(size) < 0)
	return rep_signal_arg_error(size, 1);
    len = rep_INT(size);
    res = rep_make_vector(len);
    if(res)
    {
	int i;
	for(i = 0; i < len; i++)
	    rep_VECTI(res, i) = init;
    }
    return(res);
}

DEFUN("arrayp", Farrayp, Sarrayp, (repv arg), rep_Subr1) /*
::doc:rep.data#arrayp::
arrayp ARG

Returns t when ARG is an array.
::end:: */
{
    return((rep_VECTORP(arg) || rep_STRINGP(arg) || rep_COMPILEDP(arg)) ? Qt : Qnil);
}

DEFUN("aset", Faset, Saset, (repv array, repv index, repv new), rep_Subr3) /*
::doc:rep.data#aset::
aset ARRAY INDEX NEW-VALUE

Sets element number INDEX (a positive integer) of ARRAY (can be a vector
or a string) to NEW-VALUE, returning NEW-VALUE. Note that strings
can only contain characters (ie, integers).
::end:: */
{
    rep_DECLARE2(index, rep_INTP);
    if(rep_INT(index) < 0)
	return rep_signal_arg_error(index, 2);
    if(rep_STRINGP(array))
    {
	if(!rep_STRING_WRITABLE_P(array))
	    return Fsignal(Qsetting_constant, rep_LIST_1(array));
	if(rep_INT(index) < rep_STRING_LEN(array))
	{
	    rep_DECLARE3(new, rep_INTP);
	    ((unsigned char *)rep_STR(array))[rep_INT(index)] = (unsigned char)rep_INT(new);
	    rep_string_modified (array);
	    return(new);
	}
    }
    else if(rep_VECTORP(array) || rep_COMPILEDP(array))
    {
	if(!rep_VECTOR_WRITABLE_P(array))
	    return Fsignal(Qsetting_constant, rep_LIST_1(array));
	if(rep_INT(index) < rep_VECT_LEN(array))
	{
	    rep_VECTI(array, rep_INT(index)) = new;
	    return(new);
	}
    }
    else
	return(rep_signal_arg_error(array, 1));
    return(rep_signal_arg_error(index, 2));
}

DEFUN("aref", Faref, Saref, (repv array, repv index), rep_Subr2) /*
::doc:rep.data#aref::
aref ARRAY INDEX

Returns the INDEXth (a non-negative integer) element of ARRAY, which
can be a vector or a string. INDEX starts at zero.
::end:: */
{
    rep_DECLARE2(index, rep_INTP);
    if(rep_INT(index) < 0)
	return rep_signal_arg_error(index, 2);
    if(rep_STRINGP(array))
    {
	if(rep_INT(index) < rep_STRING_LEN(array))
	    return(rep_MAKE_INT(((unsigned char *)rep_STR(array))[rep_INT(index)]));
    }
    else if(rep_VECTORP(array) || rep_COMPILEDP(array))
    {
	if(rep_INT(index) < rep_VECT_LEN(array))
	    return(rep_VECTI(array, rep_INT(index)));
    }
    else
	return rep_signal_arg_error (array, 1);
    return rep_signal_arg_error (index, 2);
}

DEFUN("make-string", Fmake_string, Smake_string, (repv len, repv init), rep_Subr2) /*
::doc:rep.data#make-string::
make-string LENGTH [INITIAL-VALUE]

Returns a new string of length LENGTH, each character is initialised to
INITIAL-repv, or to space if INITIAL-VALUE is not given.
::end:: */
{
    repv res;
    rep_DECLARE1(len, rep_INTP);
    if(rep_INT(len) < 0)
	return rep_signal_arg_error(len, 1);
    res = rep_make_string(rep_INT(len) + 1);
    if(res)
    {
	memset(rep_STR(res), rep_INTP(init) ? (char)rep_INT(init) : ' ', rep_INT(len));
	rep_STR(res)[rep_INT(len)] = 0;
    }
    return(res);
}

DEFUN("substring", Fsubstring, Ssubstring, (repv string, repv start, repv end), rep_Subr3) /*
::doc:rep.data#substring::
substring STRING START [END]

Returns the portion of STRING starting at character number START and ending
at the character before END (or the end of the string if END is not given).
All indices start at zero.
::end:: */
{
    int slen;
    rep_DECLARE1(string, rep_STRINGP);
    rep_DECLARE2(start, rep_INTP);
    rep_DECLARE3_OPT(end, rep_INTP);
    slen = rep_STRING_LEN(string);
    if(rep_INT(start) > slen || rep_INT(start) < 0)
	return(rep_signal_arg_error(start, 2));
    if(rep_INTP(end))
    {
	if((rep_INT(end) > slen) || (rep_INT(end) < rep_INT(start)))
	    return(rep_signal_arg_error(end, 3));
	return(rep_string_dupn(rep_STR(string) + rep_INT(start), rep_INT(end) - rep_INT(start)));
    }
    else
	return(rep_string_dupn(rep_STR(string) + rep_INT(start), slen - rep_INT(start)));
}

DEFUN("concat", Fconcat, Sconcat, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#concat::
concat ARGS...

Concatenates all ARGS... into a single string, each argument can be a string,
a character or a list or vector of characters.
::end:: */
{
    unsigned int length;
    repv elt, string;
    char *ptr;
    int i;

    /* Pass 1. calculate the length of the new string. */

    length = 0;
    for (i = 0; i < argc; i++)
    {
	elt = argv[i];

	if (rep_INTP (elt))
	{
	    length++;
	}
	else if (rep_CONSP (elt))
	{
	    length += rep_list_length (elt);
	}
	else
	{
	    switch (rep_CELL8_TYPE (elt))
	    {
	    case rep_String:
		length += rep_STRING_LEN (elt);
		break;

	    case rep_Vector:
		length += rep_VECT_LEN (elt);
		break;
	    }
	}
    }

    if (length == 0)
	return rep_null_string ();

    /* Allocate the string. */

    string = rep_make_string (length + 1);
    ptr = rep_STR (string);

    /* Pass 2: copy in the data */

    for (i = 0; i < argc; i++)
    {
	elt = argv[i];

	if (rep_INTP (elt))
	{
	    *ptr++ = rep_INT (elt);
	}
	else if (rep_CONSP (elt))
	{
	    repv tem = elt, c;

	    while (rep_CONSP (tem))
	    {
		c = rep_CAR (tem);

		if (rep_INTP (c))
		    *ptr++ = rep_INT (c);

		tem = rep_CDR (tem);
	    }
	}
	else
	{
	    switch (rep_CELL8_TYPE (elt))
	    {
		int i;
		repv c;

	    case rep_String:
		memcpy (ptr, rep_STR (elt), rep_STRING_LEN (elt));
		ptr += rep_STRING_LEN (elt);
		break;

	    case rep_Vector:
		for (i = 0; i < rep_VECT_LEN (elt); i++)
		{
		    c = rep_VECTI (elt, i);
		    if (rep_INTP (c))
			*ptr++ = rep_INT (c);
		}
		break;
	    }
	}
    }

    if (rep_STRING_LEN (string) != (ptr - rep_STR (string)))
	rep_set_string_len (string, ptr - rep_STR (string));

    *ptr++ = '\0';

    return string;
}

DEFUN("length", Flength, Slength, (repv sequence), rep_Subr1) /*
::doc:rep.data#length::
length SEQUENCE

Returns the number of elements in SEQUENCE (a string, list or vector).
::end:: */
{
    if (sequence == Qnil)
	return rep_MAKE_INT (0);

    switch(rep_TYPE(sequence))
    {
	int i;
    case rep_String:
	return(rep_MAKE_INT(rep_STRING_LEN(sequence)));
	break;
    case rep_Vector: case rep_Compiled:
	return(rep_MAKE_INT(rep_VECT_LEN(sequence)));
	break;
    case rep_Cons:
	i = 0;
	while(rep_CONSP(sequence))
	{
	    sequence = rep_CDR(sequence);
	    i++;
	    rep_TEST_INT;
	    if(rep_INTERRUPTP)
		return(rep_NULL);
	}
	return(rep_MAKE_INT(i));
	break;
    default:
	return rep_signal_arg_error (sequence, 1);
    }
}

DEFUN("copy-sequence", Fcopy_sequence, Scopy_sequence, (repv seq), rep_Subr1) /*
::doc:rep.data#copy-sequence::
copy-sequence SEQUENCE

Returns a new sequence whose elements are eq to those in SEQUENCE.
::end:: */
{
    repv res = Qnil;
    if (seq == Qnil)
	return Qnil;
    switch(rep_TYPE(seq))
    {
    case rep_Cons:
	{
	    repv *last = &res;
	    while(rep_CONSP(seq))
	    {
		rep_TEST_INT;
		if(rep_INTERRUPTP)
		    return(rep_NULL);
		if(!(*last = Fcons(rep_CAR(seq), Qnil)))
		    return(rep_NULL);
		last = &rep_CDR(*last);
		seq = rep_CDR(seq);
	    }
	}
	break;
    case rep_Vector: case rep_Compiled:
	res = rep_make_vector(rep_VECT_LEN(seq));
	if(res)
	{
	    int i, len = rep_VECT_LEN(seq);
	    rep_VECT(res)->car = rep_VECT(seq)->car;
	    for(i = 0; i < len; i++)
		rep_VECTI(res, i) = rep_VECTI(seq, i);
	}
	break;
    case rep_String:
	res = rep_string_dupn(rep_STR(seq), rep_STRING_LEN(seq));
	break;
    default:
	res = rep_signal_arg_error(seq, 1);
    }
    return(res);
}

DEFUN("elt", Felt, Selt, (repv seq, repv index), rep_Subr2) /*
::doc:rep.data#elt::
elt SEQUENCE INDEX

Return the element of SEQUENCE at position INDEX (counting from zero).
::end:: */
{
    if(rep_NILP(Farrayp(seq)))
	return(Fnth(index, seq));
    else
	return(Faref(seq, index));
}

DEFUN("cond", Fcond, Scond, (repv args, repv tail_posn), rep_SF) /*
::doc:rep.lang.interpreter#cond::
cond (CONDITION FORMS... ) ...

Find the first CONDITION which has a value of t when eval'ed, then perform
a progn on its associated FORMS. If there are no FORMS with the CONDITION
then the value of the CONDITION is returned. If no CONDITION is t then
return nil.
An example,
  (cond
    ((stringp foo)
      (title "foo is a string"))
    ((numberp foo)
      (setq bar foo)
      (title "foo is a number"))
    (t
      (title "foo is something else...")))
Note the use of plain `t' on it's own for the last CONDITION, this is
like the last else in an else-if statement in C.
::end:: */
{
    repv res = Qnil;
    rep_GC_root gc_args;
    rep_PUSHGC(gc_args, args);
    while(rep_CONSP(args) && rep_CONSP(rep_CAR(args)))
    {
	repv cndlist = rep_CAR(args);
	if(!(res = rep_eval(rep_CAR(cndlist), Qnil)))
	    break;
	if(!rep_NILP(res))
	{
	    if(rep_CONSP(rep_CDR(cndlist)))
	    {
		if(!(res = Fprogn(rep_CDR(cndlist), tail_posn)))
		    break;
	    }
	    break;
	}
	args = rep_CDR(args);
    }
    rep_POPGC;
    return(res);
}

static inline repv
load_file_exists_p (repv name)
{
    repv tem = Ffile_readable_p (name);
    if (tem && tem != Qnil)
    {
	tem = Ffile_directory_p (name);
	if (tem)
	    return (tem == Qnil) ? Qt : Qnil;
    }
    return tem;
}

DEFUN ("load-file", Fload_file, Sload_file,
       (repv name, repv structure), rep_Subr2) /*
::doc:rep.io.files#load-file::
load-file FILENAME [STRUCTURE]

Load the file of Lisp forms called FILENAME (no suffixes are added, or
paths searched). The file is loaded in a null lexical environment,
within STRUCTURE. The value of the last form evaluated is returned.
::end:: */
{
    repv stream, bindings = Qnil, result, tem;
    rep_GC_root gc_stream, gc_bindings;
    struct rep_Call lc;
    int c;

    if (structure == Qnil)
	structure = rep_structure;

    rep_DECLARE1 (name, rep_STRINGP);
    rep_DECLARE2 (structure, rep_STRUCTUREP);

    rep_PUSHGC (gc_stream, name);
    rep_PUSHGC (gc_bindings, structure);
    stream = Fopen_file (name, Qread);
    rep_POPGC; rep_POPGC;
    if (!stream || !rep_FILEP (stream))
	return rep_NULL;

    bindings = rep_bind_symbol (bindings, Qload_filename, name);
    rep_PUSHGC (gc_stream, stream);
    rep_PUSHGC (gc_bindings, bindings);

    /* Create the lexical environment for the file. */
    lc.fun = Qnil;
    lc.args = Qnil;
    rep_PUSH_CALL (lc);
    rep_env = Qnil;
    rep_structure = structure;

    result = Qnil;
    c = rep_stream_getc (stream);
    while ((c != EOF) && (tem = rep_readl (stream, &c)))
    {
	rep_TEST_INT;
	if (rep_INTERRUPTP || !(result = rep_eval (tem, Qnil)))
	{
	    result = rep_NULL;
	    goto out;
	}
    }
    if (rep_throw_value
	&& rep_CAR (rep_throw_value) == Qerror
	&& rep_CONSP (rep_CDR(rep_throw_value))
	&& rep_CAR (rep_CDR(rep_throw_value)) == Qend_of_stream)
    {
	/* lose the end-of-stream error. */
	rep_throw_value = rep_NULL;
    }
out:
    rep_POP_CALL (lc);
    rep_POPGC; rep_POPGC;

    rep_PUSHGC (gc_stream, result);
    rep_unbind_symbols (bindings);
    Fclose_file (stream);
    rep_POPGC;

    return result;
}

DEFUN ("load-dl-file", Fload_dl_file, Sload_dl_file,
       (repv name, repv structure), rep_Subr2)
{
    struct rep_Call lc;
    repv result;

    if (structure == Qnil)
	structure = rep_structure;

    rep_DECLARE1 (name, rep_STRINGP);
    rep_DECLARE2 (structure, rep_STRUCTUREP);

    /* Create the lexical environment for the file. */
    lc.fun = Qnil;
    lc.args = Qnil;
    rep_PUSH_CALL (lc);
    rep_env = Qnil;
    rep_structure = structure;

#ifdef HAVE_DYNAMIC_LOADING
    result = rep_open_dl_library (name);
#else
    result = Fsignal (Qerror, rep_LIST_1 (rep_string_dup ("No support for dynamic loading of shared libraries")));
#endif

    rep_POP_CALL (lc);
    return result;
}

DEFUN_INT("load", Fload, Sload, (repv file, repv noerr_p, repv nopath_p, repv nosuf_p, repv unused), rep_Subr5, "fLisp file to load:") /*
::doc:rep.io.files#load::
load FILE [NO-ERROR] [NO-PATH] [NO-SUFFIX]

Attempt to open and then read-and-eval the file of Lisp code FILE.

For each directory named in the variable `load-path' tries the value of
FILE with `.jlc' (compiled-lisp) appended to it, then with `.jl' appended
to it, finally tries FILE without modification.

If NO-ERROR is non-nil no error is signalled if FILE can't be found. If
NO-PATH is non-nil the `load-path' variable is not used, just the value
of FILE. If NO-SUFFIX is non-nil no suffixes are appended to FILE.

If the compiled version is older than it's source code, the source code is
loaded and a warning is displayed.
::end:: */
{
    /* Avoid the need to protect these args from GC. */
    rep_bool no_error_p = !rep_NILP(noerr_p);
    rep_bool no_suffix_p = !rep_NILP(nosuf_p);
    rep_bool interp_mode = Fsymbol_value (Qinterpreted_mode, Qt) != Qnil;

    repv name = Qnil, path;
    repv dir = rep_NULL, try = rep_NULL;
    repv result = rep_NULL;
    repv suffixes;
    rep_bool trying_dl = rep_FALSE;

    rep_GC_root gc_file, gc_name, gc_path, gc_dir, gc_try, gc_result, gc_suffixes;

    rep_DECLARE1(file, rep_STRINGP);
    if(rep_NILP(nopath_p))
    {
	path = Fsymbol_value(Qload_path, Qnil);
	if(!path)
	    return(rep_NULL);
    }
    else
	path = Fcons(rep_null_string(), Qnil);

    suffixes = F_structure_ref (rep_structure, Q_load_suffixes);
    if (!suffixes || !rep_CONSP (suffixes))
	suffixes = default_suffixes;

    rep_PUSHGC(gc_name, name);
    rep_PUSHGC(gc_file, file);
    rep_PUSHGC(gc_path, path);
    rep_PUSHGC(gc_dir, dir);
    rep_PUSHGC(gc_try, try);
    rep_PUSHGC(gc_suffixes, suffixes);

    /* Scan the path for the file to load. */
research:
    while(rep_NILP(name) && rep_CONSP(path))
    {
	if (rep_STRINGP (rep_CAR(path)))
	{
	    dir = Fexpand_file_name (file, rep_CAR(path));
	    if(dir == rep_NULL || !rep_STRINGP(dir))
		goto path_error;

	    if(trying_dl || !no_suffix_p)
	    {
		repv tem;
		int i = 1;
		if (!trying_dl && interp_mode)
		    i = 0;
		for(; i >= 0; i--)
		{
#ifdef HAVE_DYNAMIC_LOADING
		    if (trying_dl)
		    {
			if (i == 1)
			    try = rep_concat2(rep_STR(dir), ".la");
			else
			{
			    try = (Fexpand_file_name
				   (rep_concat3 ("lib", rep_STR(file), ".la"),
				    rep_CAR(path)));
			}
		    }
		    else
#endif
		    {
			repv sfx = ((i == 0)
				    ? rep_CAR(suffixes) : rep_CDR(suffixes));
			if (rep_STRINGP (sfx))
			    try = rep_concat2(rep_STR(dir), rep_STR(sfx));
		    }

		    if (try && rep_STRINGP (try))
		    {
			tem = load_file_exists_p (try);
			if(!tem)
			    goto path_error;
			if(tem != Qnil)
			{
			    if(name != Qnil)
			    {
				if(rep_file_newer_than(try, name))
				{
				    if (rep_message_fun != 0)
					(*rep_message_fun)(rep_messagef, "Warning: %s newer than %s, using %s", rep_STR(try), rep_STR(name), rep_STR(try));
				    name = try;
				}
			    }
			    else
				name = try;
			}
		    }
		}
	    }
	    if(!trying_dl && name == Qnil && no_suffix_p)
	    {
		/* Try without a suffix */
		repv tem = load_file_exists_p (dir);
		if(!tem)
		    goto path_error;
		if(tem != Qnil)
		    name = dir;
	    }
	}
	path = rep_CDR(path);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    goto path_error;
    }

#ifdef HAVE_DYNAMIC_LOADING
    if(rep_NILP(name) && !trying_dl)
    {
	if(rep_NILP(nopath_p))
	{
	    path = Fsymbol_value(Qdl_load_path, Qnil);
	    if(!path)
		return rep_NULL;
	}
	else
	    path = rep_LIST_1(rep_null_string());
	trying_dl = rep_TRUE;
	goto research;
    }
#endif

path_error:
    rep_POPGC; rep_POPGC; rep_POPGC; rep_POPGC; rep_POPGC; rep_POPGC;

    if(rep_NILP(name))
    {
	if(!no_error_p)
	    return rep_signal_file_error(file);
	else
	    return Qnil;
    }

    rep_PUSHGC (gc_file, file);
#ifdef HAVE_DYNAMIC_LOADING
    if(trying_dl)
	result = Fload_dl_file (name, rep_structure);
    else
#endif
	result = Fload_file (name, rep_structure);
    rep_POPGC;
    if (result == rep_NULL)
	return rep_NULL;

    /* Loading succeeded. Look for an applicable item in
       the after-load-alist. */
    if (rep_STRUCTUREP (result) && rep_STRUCTURE (result)->name != Qnil)
	/* use the canonical name in case of aliasing.. */
	file = rep_SYM (rep_STRUCTURE (result)->name)->name;
    rep_PUSHGC (gc_result, result);
    rep_PUSHGC (gc_file, file);
    {
	repv tem;
again:
	tem = Fsymbol_value(Qafter_load_alist, Qt);
	if(tem != rep_NULL && rep_CONSP(tem))
	{
	    tem = Fassoc(file, tem);
	    if(tem != rep_NULL && rep_CONSP(tem))
	    {
		/* Delete this entry */
		Fset(Qafter_load_alist,
		     Fdelq(tem, Fsymbol_value (Qafter_load_alist, Qt)));
	    
		/* Then call it */
		tem = rep_CDR (tem);
		while (rep_CONSP (tem) && !rep_INTERRUPTP)
		{
		    rep_GC_root gc_tem;
		    rep_PUSHGC (gc_tem, tem);
		    rep_call_lisp0 (rep_CAR (tem));
		    rep_POPGC;
		    tem = rep_CDR (tem);
		}

		/* Try for another entry */
		goto again;
	    }
	}
    }
    rep_POPGC;
    rep_POPGC;

    return result;
}

DEFUN("equal", Fequal, Sequal, (repv val1, repv val2), rep_Subr2) /*
::doc:rep.data#equal::
equal VALUE1 VALUE2

Compares VALUE1 and VALUE2, compares the actual structure of the objects not
just whether the objects are one and the same. ie, will return t for two
strings built from the same characters in the same order even if the strings'
location in memory is different.
::end:: */
{
    return (rep_value_cmp(val1, val2) == 0) ? Qt : Qnil;
}

DEFUN("eq", Feq, Seq, (repv val1, repv val2), rep_Subr2) /*
::doc:rep.data#eq::
eq VALUE1 VALUE2

Returns t if VALUE1 and VALUE2 are one and the same object. Note that
this may or may not be true for numbers of the same value (see `eql').
::end:: */
{
    return (val1 == val2) ? Qt : Qnil;
}

DEFUN("not", Fnot, Snot, (repv arg), rep_Subr1) /*
::doc:rep.data#not::
not ARG

If ARG is nil returns t, else returns nil.
::end:: */
{
    if(rep_NILP(arg))
	return(Qt);
    return(Qnil);
}

DEFUN("string-head-eq", Fstring_head_eq, Sstring_head_eq, (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string-head-eq::
string-head-eq STRING1 STRING2

Returns t if STRING2 matches the beginning of STRING1, ie,
  (string-head-eq "foobar" "foo")
   => t
  (string-head-eq "foo" "foobar")
   => nil
::end:: */
{
    char *s1, *s2;
    rep_DECLARE1(str1, rep_STRINGP);
    rep_DECLARE2(str2, rep_STRINGP);
    s1 = rep_STR(str1);
    s2 = rep_STR(str2);
    while(*s1 && *s2)
    {
	if(*s1++ != *s2++)
	    return(Qnil);
    }
    if(*s1 || (*s1 == *s2))
	return(Qt);
    return(Qnil);
}

DEFUN("string-equal", Fstring_equal, Sstring_equal, (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string-equal::
string-equal STRING1 STRING2

Returns t if STRING1 and STRING2 are the same, ignoring case.
::end:: */
{
    char *s1, *s2;
    rep_DECLARE1(str1, rep_STRINGP);
    rep_DECLARE2(str2, rep_STRINGP);
    s1 = rep_STR(str1);
    s2 = rep_STR(str2);
    while(*s1 && *s2)
    {
	if (toupper (*s1) != toupper (*s2))
	    return Qnil;
	s1++; s2++;
    }
    return (*s1 || *s2) ? Qnil : Qt;
}

DEFUN("string-lessp", Fstring_lessp, Sstring_lessp, (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string-lessp::
string-lessp STRING1 STRING2

Returns t if STRING1 is `less' than STRING2, ignoring case.
::end:: */
{
    char *s1, *s2;
    rep_DECLARE1(str1, rep_STRINGP);
    rep_DECLARE2(str2, rep_STRINGP);
    s1 = rep_STR(str1);
    s2 = rep_STR(str2);
    while(*s1 && *s2)
    {
	if (toupper (*s1) != toupper (*s2))
	    return (toupper (*s1) < toupper (*s2)) ? Qt : Qnil;
	s1++; s2++;
    }
    return *s2 ? Qt : Qnil;
}

#define APPLY_COMPARISON(op)				\
    int i, sign;					\
    if (argc < 2)					\
	return rep_signal_missing_arg (argc + 1);	\
    for (i = 1; i < argc; i++)				\
    {							\
	repv a = argv[i-1], b = argv[i];		\
	if (rep_NUMBERP (a) || rep_NUMBERP (b))		\
	    sign = rep_compare_numbers (a, b);		\
	else						\
	    sign = rep_value_cmp (a, b);		\
	if (!(sign op 0))				\
	    return Qnil;				\
    }							\
    return Qt;

DEFUN("=", Fnum_eq, Snum_eq, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#=::
= ARG1 ARG2 [ARG3 ...]

Returns t if each value is the same as every other value. (Using
`equal' to compare values, except for numbers, where exactness is
ignored.)
::end:: */
{
    APPLY_COMPARISON(==)
}

DEFUN("/=", Fnum_noteq, Snum_noteq, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#:/=::
/= ARG1 ARG2 ...

Returns t if each value is different from every other value. (Using
`equal' to compare values, except for numbers, where exactness is
ignored.)
::end:: */
{
    repv ret = Fnum_eq (argc, argv);
    return !ret ? rep_NULL : ret == Qnil ? Qt : Qnil;
}

DEFUN(">", Fgtthan, Sgtthan, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#>::
> ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is greater than ARG2, and if ARG2 is greater than ARG3,
and so on. Note that this command isn't limited to numbers, it can do
strings, positions, marks, etc as well.
::end:: */
{
    APPLY_COMPARISON(>)
}

DEFUN(">=", Fgethan, Sgethan, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#>=::
>= ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is greater-or-equal than ARG2. Note that this command
isn't limited to numbers, it can do strings, positions, marks, etc as well.
::end:: */
{
    APPLY_COMPARISON(>=)
}

DEFUN("<", Fltthan, Sltthan, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#<::
< ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is less than ARG2. Note that this command isn't limited to
numbers, it can do strings, positions, marks, etc as well.
::end:: */
{
    APPLY_COMPARISON(<)
}

DEFUN("<=", Flethan, Slethan, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#<=::
<= ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is less-or-equal than ARG2. Note that this command isn't
limited to numbers, it can do strings, positions, marks, etc as well.
::end:: */
{
    APPLY_COMPARISON(<=)
}

DEFUN("null", Fnull, Snull, (repv arg), rep_Subr1) /*
::doc:rep.data#null::
null ARG

Returns t if ARG is nil.
::end:: */
{
    return rep_NILP(arg) ? Qt : Qnil;
}

DEFUN("atom", Fatom, Satom, (repv arg), rep_Subr1) /*
::doc:rep.data#atom::
atom ARG

Returns t if ARG is not a cons-cell.
::end:: */
{
    return rep_CONSP(arg) ? Qnil : Qt;
}

DEFUN("consp", Fconsp, Sconsp, (repv arg), rep_Subr1) /*
::doc:rep.data#consp::
consp ARG

Returns t if ARG is a cons-cell.
::end:: */
{
    return rep_CONSP(arg) ? Qt : Qnil;
}

DEFUN("listp", Flistp, Slistp, (repv arg), rep_Subr1) /*
::doc:rep.data#listp::
listp ARG

Returns t if ARG is a list, (either a cons-cell or nil).
::end:: */
{
    return rep_LISTP(arg) ? Qt : Qnil;
}

DEFUN("stringp", Fstringp, Sstringp, (repv arg), rep_Subr1) /*
::doc:rep.data#stringp::
stringp ARG

Returns t is ARG is a string.
::end:: */
{
    return rep_STRINGP(arg) ? Qt : Qnil;
}

DEFUN("vectorp", Fvectorp, Svectorp, (repv arg), rep_Subr1) /*
::doc:rep.data#vectorp::
vectorp ARG

Returns t if ARG is a vector.
::end:: */
{
    return rep_VECTORP(arg) ? Qt : Qnil;
}

DEFUN("functionp", Ffunctionp, Sfunctionp, (repv arg), rep_Subr1) /*
::doc:rep.lang.interpreter#functionp::
functionp ARG

Returns t if ARG is a function.
::end:: */
{
    switch(rep_TYPE(arg))
    {
    case rep_Subr0:
    case rep_Subr1:
    case rep_Subr2:
    case rep_Subr3:
    case rep_Subr4:
    case rep_Subr5:
    case rep_SubrN:
    case rep_Funarg:
	return Qt;

    case rep_Cons:
	arg = rep_CAR(arg);
	if(arg == Qautoload)
	    return(Qt);
	/* FALL THROUGH */

    default:
	return(Qnil);
    }
}

DEFUN("macrop", Fmacrop, Smacrop, (repv arg), rep_Subr1) /*
::doc:rep.lang.interpreter#macrop::
macrop ARG

Returns t if ARG is a macro.
::end:: */
{
    if(rep_CONSP(arg) && rep_CAR(arg) == Qmacro)
	return Qt;
    else
	return Qnil;
}
	
DEFUN("special-form-p", Fspecial_form_p, Sspecial_form_p, (repv arg), rep_Subr1) /*
::doc:rep.lang.interpreter#special-form-p::
special-form-p ARG

Returns t if ARG is a special-form.
::end:: */
{
    if(rep_TYPEP(arg, rep_SF))
	return(Qt);
    return(Qnil);
}

DEFUN("subrp", Fsubrp, Ssubrp, (repv arg), rep_Subr1) /*
::doc:rep.lang.interpreter#subrp::
subrp ARG

Returns t if arg is a primitive function.
::end:: */
{
    switch(rep_TYPE(arg))
    {
    case rep_Subr0:
    case rep_Subr1:
    case rep_Subr2:
    case rep_Subr3:
    case rep_Subr4:
    case rep_Subr5:
    case rep_SubrN:
    case rep_SF:
	return(Qt);
    default:
	return(Qnil);
    }
}

DEFUN("sequencep", Fsequencep, Ssequencep, (repv arg), rep_Subr1) /*
::doc:rep.data#sequencep::
sequencep ARG

Returns t is ARG is a sequence (a list, vector or string).
::end:: */
{
    if(rep_LISTP(arg) || rep_VECTORP(arg) || rep_STRINGP(arg) || rep_COMPILEDP(arg))
	return Qt;
    else
	return Qnil;
}

DEFUN("subr-name", Fsubr_name, Ssubr_name, (repv subr, repv useVar), rep_Subr2) /*
::doc:rep.lang.interpreter#subr-name::
subr-name SUBR [USE-VAR]

Returns the name (a string) associated with SUBR.
::end:: */
{
    switch(rep_TYPE(subr))
    {
    case rep_Subr0:
    case rep_Subr1:
    case rep_Subr2:
    case rep_Subr3:
    case rep_Subr4:
    case rep_Subr5:
    case rep_SubrN:
    case rep_SF:
	return(rep_SUBR(subr)->name);
    default:
	return(Qnil);
    }
}

DEFUN("call-hook", Fcall_hook, Scall_hook, (repv hook, repv arg_list, repv type), rep_Subr3) /*
::doc:rep.system#call-hook::
call-hook HOOK ARG-LIST [TYPE]

Call the hook named by the symbol HOOK, passing all functions the arguments
in the list ARG-LIST. Note that HOOK may also be the actual list of functions
to call.

TYPE defines how the return values of each function in the hook are
treated. If TYPE is nil they are ignored, if TYPE is the symbol `and'
the hook aborts after a function returns nil, if TYPE is `or' the hook
aborts when a function returns non-nil.

In all cases the value returned by the last-evaluated function is
returned.
::end:: */
{
    rep_GC_root gc_hook, gc_arg_list, gc_type;
    repv res = Qnil;
    rep_DECLARE2(arg_list, rep_LISTP);
    if(!rep_LISTP(hook))
    {
	rep_DECLARE1(hook, rep_SYMBOLP);
	hook = Fsymbol_value(hook, Qt);
	if(rep_VOIDP(hook) || rep_NILP(hook))
	    return Qnil;
    }
    rep_PUSHGC(gc_hook, hook);
    rep_PUSHGC(gc_arg_list, arg_list);
    rep_PUSHGC(gc_type, type);
    while(rep_CONSP(hook))
    {
	res = Ffuncall(Fcons(rep_CAR(hook), arg_list));
	hook = rep_CDR(hook);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    res = rep_NULL;
	if(res == rep_NULL
	   || (type == Qand && rep_NILP(res))
	   || (type == Qor && !rep_NILP(res)))
	    break;
    }
    rep_POPGC; rep_POPGC; rep_POPGC;
    return res;
}

DEFUN("call-with-exception-handler", Fcall_with_exception_handler,
      Scall_with_exception_handler, (repv thunk, repv handler),
      rep_Subr2) /*
::doc:rep.lang.interpreter#call-with-exception-handler::
call-with-exception-handler THUNK HANDLER

Call THUNK and return its value. However if an exception of any form
occurs, call HANDLER with a single argument, the exception data, and
return its value.
::end:: */
    /* Non-local exits don't bother with jmp_buf's and the like, they just
       unwind normally through all levels of recursion with a rep_NULL result.
       This is slow but it's easy to work with.  */
{
    rep_GC_root gc_handler;
    repv ret;

    rep_DECLARE (1, thunk, Ffunctionp (thunk) != Qnil);
    rep_DECLARE (2, handler, Ffunctionp (handler) != Qnil);

    rep_PUSHGC (gc_handler, handler);
    ret = rep_call_lisp0 (thunk);
    rep_POPGC;
    if (ret == rep_NULL)
    {
	repv data = rep_throw_value;
	rep_throw_value = rep_NULL;
	assert (data != rep_NULL);
	ret = rep_call_lisp1 (handler, data);
    }
    return ret;
}

DEFUN("raise-exception", Fraise_exception,
      Sraise_exception, (repv ex), rep_Subr1) /*
::doc:rep.lang.interpreter#raise-exception::
raise-exception DATA

Raise the exception represented by the cons cell DATA.
::end:: */
{
    /* Only one thing can use `rep_throw_value' at once.  */
    rep_DECLARE1 (ex, rep_CONSP);
    if (rep_throw_value == rep_NULL)
	rep_throw_value = ex;
    return rep_NULL;
}

/* XXX compatibility */
repv Fthrow (repv tag, repv value) {
    return Fraise_exception (Fcons (tag, value));
}

DEFSTRING(jl, ".jl");
DEFSTRING(jlc, ".jlc");

static void
add_path (const char *env, repv var)
{
    repv list = Qnil, vec[2];
    char *ptr;

    ptr = getenv (env);
    while (ptr != 0 && *ptr != 0)
    {
	char *end = strchr (ptr, ':');
	list = Fcons (end ? rep_string_dupn (ptr, end - ptr)
		      : rep_string_dup (ptr), list);
	ptr = end ? end + 1 : 0;
    }

    vec[0] = Fnreverse (list);
    vec[1] = Fsymbol_value (var, Qt);
    Fset (var, Fnconc_ (2, vec));
}

void
rep_lispcmds_init(void)
{
    DEFSTRING (common_exec, REP_COMMON_EXEC_DIRECTORY);
    repv tem;

    tem = rep_push_structure ("rep.lang.interpreter");
    rep_ADD_SUBR(Squote);
    rep_ADD_SUBR(Slambda);
    rep_ADD_SUBR(Scond);
    rep_ADD_SUBR(Scall_with_exception_handler);
    rep_ADD_SUBR(Sraise_exception);
    rep_ADD_SUBR(Sfunctionp);
    rep_ADD_SUBR(Smacrop);
    rep_ADD_SUBR(Sspecial_form_p);
    rep_ADD_SUBR(Ssubrp);
    rep_ADD_SUBR(Ssubr_name);
    rep_pop_structure (tem);

    tem = rep_push_structure ("rep.data");
    rep_ADD_SUBR(Scar);
    rep_ADD_SUBR(Scdr);
    rep_ADD_SUBR(Slist);
    rep_ADD_SUBR(Slist_star);
    rep_ADD_SUBR(Smake_list);
    rep_ADD_SUBR(Sappend);
    rep_ADD_SUBR(Snconc);
    rep_ADD_SUBR(Srplaca);
    rep_ADD_SUBR(Srplacd);
    rep_ADD_SUBR(Sreverse);
    rep_ADD_SUBR(Snreverse);
    rep_ADD_SUBR(Sassoc);
    rep_ADD_SUBR(Sassq);
    rep_ADD_SUBR(Srassoc);
    rep_ADD_SUBR(Srassq);
    rep_ADD_SUBR(Snth);
    rep_ADD_SUBR(Snthcdr);
    rep_ADD_SUBR(Slast);
    rep_ADD_SUBR(Smapcar);
    rep_ADD_SUBR(Smapc);
    rep_ADD_SUBR(Sfilter);
    rep_ADD_SUBR(Smember);
    rep_ADD_SUBR(Smemq);
    rep_ADD_SUBR(Smemql);
    rep_ADD_SUBR(Sdelete);
    rep_ADD_SUBR(Sdelq);
    rep_ADD_SUBR(Sdelete_if);
    rep_ADD_SUBR(Sdelete_if_not);
    rep_ADD_SUBR(Svector);
    rep_ADD_SUBR(Smake_vector);
    rep_ADD_SUBR(Sarrayp);
    rep_ADD_SUBR(Saset);
    rep_ADD_SUBR(Saref);
    rep_ADD_SUBR(Smake_string);
    rep_ADD_SUBR(Ssubstring);
    rep_ADD_SUBR(Sconcat);
    rep_ADD_SUBR(Slength);
    rep_ADD_SUBR(Scopy_sequence);
    rep_ADD_SUBR(Selt);
    rep_ADD_SUBR(Snot);
    rep_ADD_SUBR(Sequal);
    rep_ADD_SUBR(Seq);
    rep_ADD_SUBR(Sstring_head_eq);
    rep_ADD_SUBR(Sstring_equal);
    rep_ADD_SUBR(Sstring_lessp);
    rep_ADD_SUBR(Snum_eq);
    rep_ADD_SUBR(Snum_noteq);
    rep_ADD_SUBR(Sgtthan);
    rep_ADD_SUBR(Sgethan);
    rep_ADD_SUBR(Sltthan);
    rep_ADD_SUBR(Slethan);
    rep_ADD_SUBR(Snull);
    rep_ADD_SUBR(Satom);
    rep_ADD_SUBR(Sconsp);
    rep_ADD_SUBR(Slistp);
    rep_ADD_SUBR(Sstringp);
    rep_ADD_SUBR(Svectorp);
    rep_ADD_SUBR(Ssequencep);
    rep_pop_structure (tem);

    tem = rep_push_structure ("rep.io.files");
    rep_ADD_SUBR (Sload_file);
    rep_ADD_SUBR (Sload_dl_file);
    rep_ADD_SUBR_INT(Sload);
    rep_pop_structure (tem);

    tem = rep_push_structure ("rep.system");
    rep_ADD_SUBR(Scall_hook);
    rep_pop_structure (tem);

    rep_INTERN(provide);

    rep_INTERN_SPECIAL(rep_directory);
    if(getenv("REPDIR") != 0)
	Fset (Qrep_directory, rep_string_dup(getenv("REPDIR")));
    else
	Fset (Qrep_directory, rep_VAL(&default_rep_directory));

    rep_INTERN_SPECIAL(lisp_lib_directory);
    if(getenv("REPLISPDIR") != 0)
	Fset (Qlisp_lib_directory, rep_string_dup(getenv("REPLISPDIR")));
    else
	Fset (Qlisp_lib_directory, rep_string_dup(REP_LISP_DIRECTORY));

    rep_INTERN_SPECIAL(site_lisp_directory);
    if(getenv("REPSITELISPDIR") != 0)
	Fset(Qsite_lisp_directory, rep_string_dup(getenv("REPSITELISPDIR")));
    else
	Fset (Qsite_lisp_directory,
	      rep_concat2(rep_STR(Fsymbol_value (Qrep_directory, Qt)),
			  "/site-lisp"));

    rep_INTERN_SPECIAL(exec_directory);
    if(getenv("REPEXECDIR") != 0)
	Fset (Qexec_directory, rep_string_dup(getenv("REPEXECDIR")));
    else
	Fset (Qexec_directory, rep_string_dup(REP_EXEC_DIRECTORY));

    rep_INTERN_SPECIAL(documentation_file);
    if(getenv("REPDOCFILE") != 0)
	Fset (Qdocumentation_file, rep_string_dup(getenv("REPDOCFILE")));
    else
    {
	DEFSTRING (doc_file, REP_DOC_FILE);
	Fset (Qdocumentation_file, rep_VAL (&doc_file));
    }

    rep_INTERN_SPECIAL(documentation_files);
    Fset (Qdocumentation_files,
	  Fcons (Fsymbol_value (Qdocumentation_file, Qt), Qnil));

    rep_INTERN_SPECIAL(load_path);
    Fset (Qload_path, Fcons (Fsymbol_value (Qlisp_lib_directory, Qt),
			     Fcons (Fsymbol_value (Qsite_lisp_directory, Qt),
				    Fcons (rep_VAL(&dot), Qnil))));
    add_path ("REP_LOAD_PATH", Qload_path);

    rep_INTERN_SPECIAL(dl_load_path);
    Fset (Qdl_load_path, Fcons (Fsymbol_value (Qexec_directory, Qt),
				Fcons (rep_VAL (&common_exec), Qnil)));
    add_path ("REP_DL_LOAD_PATH", Qdl_load_path);

    rep_INTERN_SPECIAL(after_load_alist);
    Fset (Qafter_load_alist, Qnil);

    rep_INTERN(or); rep_INTERN(and);

    rep_INTERN_SPECIAL(dl_load_reloc_now);
    Fset (Qdl_load_reloc_now, Qnil);

    rep_INTERN_SPECIAL(load_filename);

    default_suffixes = Fcons (rep_VAL (&jl), rep_VAL (&jlc));
    rep_mark_static (&default_suffixes);
    rep_INTERN (_load_suffixes);
}
