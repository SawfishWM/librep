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

#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

DEFSTRING(default_rep_directory, REP_DIRECTORY);
DEFSTRING(dot, ".");

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

DEFUN("quote", Fquote, Squote, (repv args), rep_SF) /*
::doc:quote::
quote ARG
'ARG

Returns ARG.
::end:: */
{
    if(rep_CONSP(args))
	return(rep_CAR(args));
    return rep_signal_missing_arg(1);
}

DEFUN("lambda", Flambda, Slambda, (repv args), rep_SF) /*
::doc:lambda::
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
::doc:car::
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
::doc:cdr::
cdr CONS-CELL

Returns the value stored in the cdr slot of CONS-CELL, or nil if CONS-CELL
is nil.
::end:: */
{
    if(rep_CONSP(cons))
	return(rep_CDR(cons));
    return(Qnil);
}

DEFUN("list", Flist, Slist, (repv args), rep_SubrN) /*
::doc:list::
list ARGS...

Returns a new list with elements ARGS...
::end:: */
{
    repv res = Qnil;
    repv *ptr = &res;
    while(rep_CONSP(args))
    {
	if(!(*ptr = Fcons(rep_CAR(args), Qnil)))
	    return rep_NULL;
	ptr = &rep_CDR(*ptr);
	args = rep_CDR(args);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    return rep_NULL;
    }
    return res;
}

DEFUN("list*", Flist_star, Slist_star, (repv args), rep_SubrN) /*
::doc:list*::
list* ARG1 ARG2 ... ARGN

Returns a new list (ARG1 ARG2 ... ARGN-1 . ARGN). That is, the same as from
`list' but the last argument is dotted to the last but one argument.
::end:: */
{
    repv res = Qnil;
    repv *ptr = &res;
    while(rep_CONSP(args))
    {
	if(rep_CONSP(rep_CDR(args)))
	{
	    if(!(*ptr = Fcons(rep_CAR(args), Qnil)))
		return rep_NULL;
	}
	else
	    *ptr = rep_CAR(args);
	ptr = &rep_CDR(*ptr);
	args = rep_CDR(args);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    return rep_NULL;
    }
    return res;
}

DEFUN("make-list", Fmake_list, Smake_list, (repv len, repv init), rep_Subr2) /*
::doc:make-list::
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

DEFUN("append", Fappend, Sappend, (repv args), rep_SubrN) /*
::doc:append::
append LISTS...

Non-destructively concatenates each of it's argument LISTS... into one
new list which is returned.
::end:: */
{
    int i = 1;
    repv res = Qnil;
    repv *resend = &res;
    while(rep_CONSP(args))
    {
	if(!rep_LISTP(rep_CAR(args)) && rep_CDR (args) != Qnil)
	    return rep_signal_arg_error(rep_CAR(args), i);
	if(rep_CONSP(rep_CAR(args)) && rep_CONSP(rep_CDR(args)))
	{
	    /* Only make a new copy if there's another list after this
	       one. */
	    *resend = rep_copy_list(rep_CAR(args));
	}
	else
	    *resend = rep_CAR(args);	/* Use the old object */
	while(rep_CONSP(*resend))
	{
	    rep_TEST_INT;
	    if(rep_INTERRUPTP)
		return rep_NULL;
	    resend = &(rep_CDR(*resend));
	}
	args = rep_CDR(args);
	i++;
    }
    return(res);
}

DEFUN("nconc", Fnconc, Snconc, (repv args), rep_SubrN) /*
::doc:nconc::
nconc LISTS...

Destructively concatenates each of it's argument LISTS... into one new
list. Every LIST but the last is modified so that it's last cdr points
to the beginning of the next list. Returns the new list.
::end:: */
{
    int i = 1;
    repv res = Qnil;
    repv *resend = &res;
    while(rep_CONSP(args))
    {
	repv tmp = rep_CAR(args);
	if(!rep_LISTP(tmp))
	    return rep_signal_arg_error(tmp, i);
	if(rep_CONSP(tmp))
	{
	    if(!rep_CONS_WRITABLE_P(tmp))
		return Fsignal(Qsetting_constant, rep_LIST_1(tmp));
	    *resend = tmp;
	    while(rep_CONSP(rep_CDR(tmp)))
	    {
		rep_TEST_INT;
		if(rep_INTERRUPTP)
		    return rep_NULL;
		tmp = rep_CDR(tmp);
	    }
	    resend = &rep_CDR(tmp);
	}
	args = rep_CDR(args);
    }
    return(res);
}

DEFUN("rplaca", Frplaca, Srplaca, (repv cons, repv car), rep_Subr2) /*
::doc:rplaca::
rplaca CONS-CELL NEW-CAR

Sets the value of the car slot in CONS-CELL to NEW-CAR. Returns the new
value.
::end:: */
{
    rep_DECLARE1(cons, rep_CONSP);
    if(!rep_CONS_WRITABLE_P(cons))
	return Fsignal(Qsetting_constant, rep_LIST_1(cons));
    rep_CAR(cons) = car;
    return(car);
}

DEFUN("rplacd", Frplacd, Srplacd, (repv cons, repv cdr), rep_Subr2) /*
::doc:rplacd::
rplacd CONS-CELL NEW-CDR

Sets the value of the cdr slot in CONS-CELL to NEW-CAR. Returns the new
value.
::end:: */
{
    rep_DECLARE1(cons, rep_CONSP);
    if(!rep_CONS_WRITABLE_P(cons))
	return Fsignal(Qsetting_constant, rep_LIST_1(cons));
    rep_CDR(cons) = cdr;
    return(cdr);
}

DEFUN("reverse", Freverse, Sreverse, (repv head), rep_Subr1) /*
::doc:reverse::
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
::doc:nreverse::
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
::doc:assoc::
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
::doc:assq::
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
::doc:rassoc::
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
::doc:rassq::
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
::doc:nth::
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
::doc:nthcdr::
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
::doc:last::
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
::doc:mapcar::
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
::doc:mapc::
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
::doc:filter::
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
::doc:member::
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
::doc:memq::
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
::doc:memql::
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
::doc:delete::
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
::doc:delq::
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
::doc:delete-if::
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
::doc:delete-if-not::
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

DEFUN("vector", Fvector, Svector, (repv args), rep_SubrN) /*
::doc:vector::
vector ARGS...

Returns a new vector with ARGS... as its elements.
::end:: */
{
    repv res = rep_make_vector(rep_list_length(args));
    if(res)
    {
	int i = 0;
	while(rep_CONSP(args))
	{
	    rep_VECTI(res, i) = rep_CAR(args);
	    args = rep_CDR(args);
	    i++;
	    rep_TEST_INT;
	    if(rep_INTERRUPTP)
		return(rep_NULL);
	}
    }
    return(res);
}

DEFUN("make-vector", Fmake_vector, Smake_vector, (repv size, repv init), rep_Subr2) /*
::doc:make-vector::
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
::doc:arrayp::
arrayp ARG

Returns t when ARG is an array.
::end:: */
{
    return((rep_VECTORP(arg) || rep_STRINGP(arg) || rep_COMPILEDP(arg)) ? Qt : Qnil);
}

DEFUN("aset", Faset, Saset, (repv array, repv index, repv new), rep_Subr3) /*
::doc:aset::
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
	    rep_STR(array)[rep_INT(index)] = (u_char)rep_INT(new);
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
::doc:aref::
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
	    return(rep_MAKE_INT(rep_STR(array)[rep_INT(index)]));
    }
    else if(rep_VECTORP(array) || rep_COMPILEDP(array))
    {
	if(rep_INT(index) < rep_VECT_LEN(array))
	    return(rep_VECTI(array, rep_INT(index)));
    }
    else
	return(Fsignal(Qbad_arg, rep_list_2(array, rep_MAKE_INT(1))));
    return(rep_signal_arg_error(index, 2));
}

DEFUN("make-string", Fmake_string, Smake_string, (repv len, repv init), rep_Subr2) /*
::doc:make-string::
make-string LENGTH [INITIAL-repv]

Returns a new string of length LENGTH, each character is initialised to
INITIAL-repv, or to space if INITIAL-repv is not given.
::end:: */
{
    repv res;
    rep_DECLARE1(len, rep_INTP);
    if(rep_INT(len) < 0)
	return rep_signal_arg_error(len, 1);
    res = rep_make_string(rep_INT(len) + 1);
    if(res)
    {
	memset(rep_STR(res), rep_INTP(init) ? (u_char)rep_INT(init) : ' ', rep_INT(len));
	rep_STR(res)[rep_INT(len)] = 0;
    }
    return(res);
}

DEFUN("substring", Fsubstring, Ssubstring, (repv string, repv start, repv end), rep_Subr3) /*
::doc:substring::
substring STRING START [END]

Returns the portion of STRING starting at character number START and ending
at the character before END (or the end of the string is END is not given).
All indices start at zero.
::end:: */
{
    int slen;
    rep_DECLARE1(string, rep_STRINGP);
    rep_DECLARE2(start, rep_INTP);
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

static inline int
extend_concat(u_char **buf, int *bufLen, int i, int addLen)
{
    u_char *newbuf;
    int newbuflen;
    if((i + addLen) < *bufLen)
	return(rep_TRUE);
    newbuflen = (i + addLen) * 2;
    newbuf = rep_alloc(newbuflen);
    if(newbuf)
    {
	memcpy(newbuf, *buf, i);
	rep_free(*buf);
	*buf = newbuf;
	*bufLen = newbuflen;
	return(rep_TRUE);
    }
    rep_mem_error();
    return(rep_FALSE);
}
DEFUN("concat", Fconcat, Sconcat, (repv args), rep_SubrN) /*
::doc:concat::
concat ARGS...

Concatenates all ARGS... into a single string, each argument can be a string,
a character or a list or vector of characters.
::end:: */
{
    int buflen = 128;
    u_char *buf = rep_alloc(buflen);
    if(buf)
    {
	repv res = rep_NULL;
	int i = 0;
	int argnum = 1;
	while(rep_CONSP(args))
	{
	    repv arg = rep_CAR(args);
	    switch(rep_TYPE(arg))
	    {
		int slen, j;
	    case rep_String:
		slen = rep_STRING_LEN(arg);
		if(!extend_concat(&buf, &buflen, i, slen))
		    goto error;
		memcpy(buf + i, rep_STR(arg), slen);
		i += slen;
		break;
	    case rep_Int:
		if(!extend_concat(&buf, &buflen, i, 1))
		    goto error;
		buf[i++] = rep_INT(arg);
		break;
	    case rep_Symbol:
		if(arg != Qnil)
		    break;
		/* FALL THROUGH */
	    case rep_Cons:
		while(rep_CONSP(arg))
		{
		    repv ch = rep_CAR(arg);
		    if(rep_INTP(ch))
		    {
			if(!extend_concat(&buf, &buflen, i, 1))
			    goto error;
			buf[i++] = rep_INT(ch);
		    }
		    arg = rep_CDR(arg);
		    rep_TEST_INT;
		    if(rep_INTERRUPTP)
			goto error;
		}
		break;
	    case rep_Vector:
		{
		    int len = rep_VECT_LEN(arg);
		    for(j = 0; j < len; j++)
		    {
			if(rep_INTP(rep_VECTI(arg, j)))
			{
			    if(!extend_concat(&buf, &buflen, i, 1))
				goto error;
			    buf[i++] = rep_INT(rep_VECTI(arg, j));
			}
		    }
		    break;
		}
	    default:
		res = rep_signal_arg_error(arg, argnum);
		goto error;
	    }
	    args = rep_CDR(args);
	    argnum++;
	}
	res = rep_string_dupn(buf, i);
error:
	rep_free(buf);
	return(res);
    }
    return(rep_NULL);
}

DEFUN("length", Flength, Slength, (repv sequence), rep_Subr1) /*
::doc:length::
length SEQUENCE

Returns the number of elements in SEQUENCE (a string, list or vector).
::end:: */
{
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
    case rep_Symbol:
	if(sequence == Qnil)
	    return(rep_MAKE_INT(0));
	/* FALL THROUGH */
    default:
	Fsignal(Qbad_arg, rep_list_2(sequence, rep_MAKE_INT(1)));
	return(rep_NULL);
    }
}

DEFUN("copy-sequence", Fcopy_sequence, Scopy_sequence, (repv seq), rep_Subr1) /*
::doc:copy-sequence::
copy-sequence SEQUENCE

Returns a new sequence whose elements are eq to those in SEQUENCE.
::end:: */
{
    repv res = Qnil;
    switch(rep_TYPE(seq))
    {
    case rep_Symbol:
	if(!rep_NILP(seq))
	    res = rep_signal_arg_error(seq, 1);
	break;
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
::doc:elt::
elt SEQUENCE INDEX

Return the element of SEQUENCE at position INDEX (counting from zero).
::end:: */
{
    if(rep_NILP(Farrayp(seq)))
	return(Fnth(index, seq));
    else
	return(Faref(seq, index));
}

DEFUN("prog1", Fprog1, Sprog1, (repv args), rep_SF) /*
::doc:prog1::
prog1 FORM1 FORMS...

First evals FORM1 then FORMS, returns the value that FORM1 gave.
::end:: */
{
    if(rep_CONSP(args))
    {
	repv res;
	rep_GC_root gc_args, gc_res;
	rep_PUSHGC(gc_args, args);
	res = Feval(rep_CAR(args));
	if(res)
	{
	    rep_PUSHGC(gc_res, res);
	    if(!Fprogn(rep_CDR(args)))
		res = rep_NULL;
	    rep_POPGC;
	}
	rep_POPGC;
	return(res);
    }
    return rep_signal_missing_arg(1);
}

DEFUN("while", Fwhile, Swhile, (repv args), rep_SF) /*
::doc:while::
while CONDITION FORMS...

Eval CONDITION, if it is non-nil then execute FORMS and repeat the
procedure, else return nil.
::end:: */
{
    if(rep_CONSP(args))
    {
	rep_GC_root gc_args;
	repv cond = rep_CAR(args), wval, body = rep_CDR(args);
	rep_PUSHGC(gc_args, args);
	while((wval = Feval(cond)) && !rep_NILP(wval))
	{
	    rep_TEST_INT;
	    if(rep_INTERRUPTP || !Fprogn(body))
	    {
		wval = rep_NULL;
		break;
	    }
	}
	rep_POPGC;
	if(!wval)
	    return(rep_NULL);
	return(Qnil);
    }
    return rep_signal_missing_arg(1);
}

DEFUN("cond", Fcond, Scond, (repv args), rep_SF) /*
::doc:cond::
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
	if(!(res = Feval(rep_CAR(cndlist))))
	    break;
	if(!rep_NILP(res))
	{
	    if(rep_CONSP(rep_CDR(cndlist)))
	    {
		if(!(res = Fprogn(rep_CDR(cndlist))))
		    break;
	    }
	    break;
	}
	args = rep_CDR(args);
    }
    rep_POPGC;
    return(res);
}

DEFUN("case", Fcase, Scase, (repv args), rep_SF) /*
::doc:case::
case KEY CLAUSES...

Each CLAUSE is `((ITEMS... ) FORMS...)'. Find the first CLAUSE with an
ITEM matching (using `eql') the result of evaluating KEY (only
evaluated once), then evaluate the associated FORMS in a `progn'. The
final clause may have the form `(t FORMS...)', which always matches KEY
if no other CLAUSE has already. Returns `nil' if no clause matches.

If any of the ITEMS appear more than once, then the behaviour is
undefined.
::end:: */
{
    repv key;
    rep_GC_root gc_args;
    int i = 2;

    if (!rep_CONSP(args))
	return rep_signal_missing_arg (0);
    
    rep_PUSHGC(gc_args, args);
    key = Feval (rep_CAR(args));
    rep_POPGC;
    if (key == rep_NULL)
	return rep_NULL;
    args = rep_CDR(args);

    while(rep_CONSP(args))
    {
	if (!rep_CONSP(rep_CAR(args)))
	    return rep_signal_arg_error (rep_CAR(args), i);
	if (rep_CONSP(rep_CAAR(args)))
	{
	    repv ptr = rep_CAAR(args);
	    while (rep_CONSP(ptr))
	    {
		if (Feql (key, rep_CAR(ptr)) != Qnil)
		    return Fprogn (rep_CDAR(args));
		ptr = rep_CDR(ptr);
		rep_TEST_INT;
		if (rep_INTERRUPTP)
		    return rep_NULL;
	    }
	}
	else if (rep_CAAR(args) == Qt)
	    return Fprogn (rep_CDAR(args));
	else
	    return rep_signal_arg_error (rep_CAR(args), i);
	args = rep_CDR(args);
	i++;
    }
    return Qnil;
}

DEFUN("apply", Fapply, Sapply, (repv args), rep_SubrN) /*
::doc:apply::
apply FUNCTION ARGS... ARG-LIST

Calls FUNCTION passing all of ARGS to it as well as all elements in ARG-LIST.
ie,
  (apply '+ 1 2 3 '(4 5 6))
   => 21
::end:: */
{
    repv list = Qnil, *last;
    last = &list;
    if(rep_CONSP(args))
    {
	while(rep_CONSP(rep_CDR(args)))
	{
	    if(!(*last = Fcons(rep_CAR(args), Qnil)))
		return(rep_NULL);
	    last = &rep_CDR(*last);
	    args = rep_CDR(args);
	    rep_TEST_INT;
	    if(rep_INTERRUPTP)
		return(rep_NULL);
	}
	if(!rep_NILP(Flistp(rep_CAR(args))))
	    *last = rep_CAR(args);
	else
	    return(Fsignal(Qbad_arg, rep_LIST_1(rep_CAR(args))));
	return(Ffuncall(list));
    }
    return rep_signal_missing_arg(1);
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

DEFUN_INT("load", Fload, Sload, (repv file, repv noerr_p, repv nopath_p, repv nosuf_p, repv unused), rep_Subr5, "fLisp file to load:") /*
::doc:load::
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

#ifdef HAVE_DYNAMIC_LOADING
    rep_bool trying_dl = rep_FALSE;
#endif

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
	no_suffix_p = rep_TRUE;

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
	    if(!trying_dl && name == Qnil)
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

#ifdef HAVE_DYNAMIC_LOADING
    if(trying_dl)
    {
	rep_PUSHGC(gc_file, file);
	result = rep_open_dl_library (name);
	rep_POPGC;
	if (result == rep_NULL)
	    return rep_NULL;
    }
    else
#endif
    {
	repv stream, bindings = Qnil;
	rep_GC_root gc_stream, gc_bindings;
	struct rep_Call lc;

	repv tem;
	int c;

	rep_PUSHGC(gc_file, file);
	rep_PUSHGC(gc_stream, name);
	stream = Fopen_file(name, Qread);
	rep_POPGC;
	if(!stream || !rep_FILEP(stream))
	{
	    rep_POPGC;
       	    return rep_NULL;
	}

	bindings = rep_bind_symbol (bindings, Qload_filename, name);
	rep_PUSHGC(gc_stream, stream);
	rep_PUSHGC(gc_bindings, bindings);

	/* Create the lexical environment for the file. */
	lc.fun = Qnil;
	lc.args = Qnil;
	lc.args_evalled_p = Qnil;
	rep_PUSH_CALL(lc);
	rep_env = Qnil;

	c = rep_stream_getc(stream);
	while((c != EOF) && (tem = rep_readl(stream, &c)))
	{
	    rep_TEST_INT;
	    if(rep_INTERRUPTP || !(result = Feval(tem)))
	    {
		rep_unbind_symbols (bindings);
		rep_POP_CALL(lc);
		rep_POPGC; rep_POPGC; rep_POPGC;
		return rep_NULL;
	    }
	}
	if (rep_throw_value
	    && rep_CAR(rep_throw_value) == Qerror
	    && rep_CONSP(rep_CDR(rep_throw_value))
	    && rep_CAR(rep_CDR(rep_throw_value)) == Qend_of_stream)
	{
	    /* lose the end-of-stream error. */
	    rep_throw_value = rep_NULL;
	}
	rep_POP_CALL(lc);
	rep_POPGC; rep_POPGC;
	rep_PUSHGC (gc_result, result);
	rep_unbind_symbols (bindings);
	Fclose_file (stream);
	rep_POPGC; rep_POPGC;
    }

    /* Loading succeeded. Look for an applicable item in
       the after-load-alist. */
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
	    
		/* Then evaluate it */
		Fprogn(rep_CDR(tem));

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
::doc:equal::
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
::doc:eq::
eq VALUE1 VALUE2

Returns t if VALUE1 and VALUE2 are one and the same object. Note that
this may or may not be true for numbers of the same value (see `eql').
::end:: */
{
    return (val1 == val2) ? Qt : Qnil;
}

DEFUN("not", Fnot, Snot, (repv arg), rep_Subr1) /*
::doc:not::
not ARG

If ARG is nil returns t, else returns nil.
::end:: */
{
    if(rep_NILP(arg))
	return(Qt);
    return(Qnil);
}

DEFUN("string-head-eq", Fstring_head_eq, Sstring_head_eq, (repv str1, repv str2), rep_Subr2) /*
::doc:string-head-eq::
string-head-eq STRING1 STRING2

Returns t if STRING2 matches the beginning of STRING1, ie,
  (string-head-eq "foobar" "foo")
   => t
  (string-head-eq "foo" "foobar")
   => nil
::end:: */
{
    u_char *s1, *s2;
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
::doc:string-equal::
string-equal STRING1 STRING2

Returns t if STRING1 and STRING2 are the same, ignoring case.
::end:: */
{
    u_char *s1, *s2;
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
::doc:string-lessp::
string-lessp STRING1 STRING2

Returns t if STRING1 is `less' than STRING2, ignoring case.
::end:: */
{
    u_char *s1, *s2;
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
    if(rep_CONSP(args) && rep_CONSP(rep_CDR(args)))	\
    {							\
	repv pred = rep_CAR(args);			\
	int i = 2;					\
	args = rep_CDR(args);				\
	while(rep_CONSP(args))				\
	{						\
	    int sign;					\
	    repv tem = rep_CAR (args);			\
	    if (rep_NUMBERP (pred) || rep_NUMBERP (tem)) \
		sign = rep_compare_numbers (pred, tem);	\
	    else					\
		sign = rep_value_cmp(pred, tem);	\
	    if(!(sign op 0)) 				\
		return Qnil;				\
	    pred = tem;					\
	    args = rep_CDR(args);			\
	    i++;					\
	    rep_TEST_INT;				\
	    if(rep_INTERRUPTP)				\
		return(rep_NULL);			\
	}						\
	return Qt;					\
    }							\
    return rep_signal_missing_arg(rep_CONSP(args) ? 2 : 1);

DEFUN("=", Fnum_eq, Snum_eq, (repv args), rep_SubrN) /*
::doc:=::
= ARG1 ARG2 [ARG3 ...]

Returns t if each value is the same as every other value. (Using
`equal' to compare values, except for numbers, where exactness is
ignored.)
::end:: */
{
    APPLY_COMPARISON(==)
}

DEFUN("/=", Fnum_noteq, Snum_noteq, (repv args), rep_SubrN) /*
::doc::/=::
/= ARG1 ARG2 ...

Returns t if each value is different from every other value. (Using
`equal' to compare values, except for numbers, where exactness is
ignored.)
::end:: */
{
    repv ret = Fnum_eq (args);
    if (ret)
	return ret == Qnil ? Qt : Qnil;
    else
	return rep_NULL;
}

DEFUN(">", Fgtthan, Sgtthan, (repv args), rep_SubrN) /*
::doc:>::
> ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is greater than ARG2, and if ARG2 is greater than ARG3,
and so on. Note that this command isn't limited to numbers, it can do
strings, positions, marks, etc as well.
::end:: */
{
    APPLY_COMPARISON(>)
}

DEFUN(">=", Fgethan, Sgethan, (repv args), rep_SubrN) /*
::doc:>=::
>= ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is greater-or-equal than ARG2. Note that this command
isn't limited to numbers, it can do strings, positions, marks, etc as well.
::end:: */
{
    APPLY_COMPARISON(>=)
}

DEFUN("<", Fltthan, Sltthan, (repv args), rep_SubrN) /*
::doc:<::
< ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is less than ARG2. Note that this command isn't limited to
numbers, it can do strings, positions, marks, etc as well.
::end:: */
{
    APPLY_COMPARISON(<)
}

DEFUN("<=", Flethan, Slethan, (repv args), rep_SubrN) /*
::doc:<=::
<= ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is less-or-equal than ARG2. Note that this command isn't
limited to numbers, it can do strings, positions, marks, etc as well.
::end:: */
{
    APPLY_COMPARISON(<=)
}

DEFUN("null", Fnull, Snull, (repv arg), rep_Subr1) /*
::doc:null::
null ARG

Returns t if ARG is nil.
::end:: */
{
    return rep_NILP(arg) ? Qt : Qnil;
}

DEFUN("atom", Fatom, Satom, (repv arg), rep_Subr1) /*
::doc:atom::
atom ARG

Returns t if ARG is not a cons-cell.
::end:: */
{
    return rep_CONSP(arg) ? Qnil : Qt;
}

DEFUN("consp", Fconsp, Sconsp, (repv arg), rep_Subr1) /*
::doc:consp::
consp ARG

Returns t if ARG is a cons-cell.
::end:: */
{
    return rep_CONSP(arg) ? Qt : Qnil;
}

DEFUN("listp", Flistp, Slistp, (repv arg), rep_Subr1) /*
::doc:listp::
listp ARG

Returns t if ARG is a list, (either a cons-cell or nil).
::end:: */
{
    return rep_LISTP(arg) ? Qt : Qnil;
}

DEFUN("stringp", Fstringp, Sstringp, (repv arg), rep_Subr1) /*
::doc:stringp::
stringp ARG

Returns t is ARG is a string.
::end:: */
{
    return rep_STRINGP(arg) ? Qt : Qnil;
}

DEFUN("vectorp", Fvectorp, Svectorp, (repv arg), rep_Subr1) /*
::doc:vectorp::
vectorp ARG

Returns t if ARG is a vector.
::end:: */
{
    return rep_VECTORP(arg) ? Qt : Qnil;
}

DEFUN("bytecodep", Fbytecodep, Sbytecodep, (repv arg), rep_Subr1) /*
::doc:bytecodep::
bytecodep ARG

Returns t if ARG is a byte code subroutine (i.e. compiled Lisp code).
::end:: */
{
    return rep_COMPILEDP(arg) ? Qt : Qnil;
}

DEFUN("functionp", Ffunctionp, Sfunctionp, (repv arg), rep_Subr1) /*
::doc:functionp::
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
::doc:macrop::
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
::doc:special-form-p::
special-form-p ARG

Returns t if ARG is a special-form.
::end:: */
{
    if(rep_TYPEP(arg, rep_SF))
	return(Qt);
    return(Qnil);
}

DEFUN("subrp", Fsubrp, Ssubrp, (repv arg), rep_Subr1) /*
::doc:subrp::
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
::doc:sequencep::
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
::doc:subr-name::
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
::doc:call-hook::
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

DEFUN("catch", Fcatch, Scatch, (repv args), rep_SF) /*
::doc:catch::
catch TAG FORMS...

Evaluate FORMS in an implicit progn; non-local exits are allowed with
`(throw TAG)'. The value of the `catch' form is either the value of the
progn or the value given to any matching `throw' form.
::end:: */
    /* Non-local exits don't bother with jmp_buf's and the like, they just
       unwind normally through all levels of recursion with a rep_NULL result.
       This is slow but it's easy to work with.  */
{
    if(rep_CONSP(args))
    {
	repv tag, res = rep_NULL;
	rep_GC_root gc_args, gc_tag;
	rep_PUSHGC(gc_args, args);
	tag = Feval(rep_CAR(args));
	if(tag)
	{
	    rep_PUSHGC(gc_tag, tag);
	    if(!(res = Fprogn(rep_CDR(args))))
	    {
		if(rep_throw_value && (rep_CAR(rep_throw_value) == tag))
		{
		    res = rep_CDR(rep_throw_value);
		    rep_throw_value = rep_NULL;
		}
	    }
	    rep_POPGC;
	}
	rep_POPGC;
	return(res);
    }
    return rep_signal_missing_arg(1);
}

DEFUN("throw", Fthrow, Sthrow, (repv tag, repv val), rep_Subr2) /*
::doc:throw::
throw TAG repv

Performs a non-local exit to the `catch' waiting for TAG and return
repv from it. TAG and repv are both evaluated fully.
::end:: */
{
    /* Only one thing can use `rep_throw_value' at once.  */
    if(!rep_throw_value)
	rep_throw_value = Fcons(tag, val);
    return(rep_NULL);
}

DEFUN("unwind-protect", Funwind_protect, Sunwind_protect, (repv args), rep_SF) /*
::doc:unwind-protect::
unwind-protect BODY CLEANUP-FORMS...

Return the result of evaluating BODY. When execution leaves the dynamic
extent of BODY evaluate `(progn CLEANUP-FORMS)' (even if exiting due to
an exception within BODY).

Note that when BODY is exited by calling a continuation, it is
undefined whether or not CLEANUP-FORMS will be evaluated.
::end:: */
{
    if(rep_CONSP(args))
    {
	repv res, throwval;
	rep_GC_root gc_args, gc_res, gc_throwval;
	rep_PUSHGC(gc_args, args);
	res = Feval(rep_CAR(args));
	rep_PUSHGC(gc_res, res);
	throwval = rep_throw_value;
	rep_throw_value = rep_NULL;
	rep_PUSHGC(gc_throwval, throwval);
	if(!Fprogn(rep_CDR(args)))
	    res = rep_NULL;
	/* Only reinstall the old throw if it's actually an error and there
	   was no error in the cleanup forms. This way the newest error
	   takes precedence. */
	if(throwval != 0 && rep_throw_value == 0)
	    rep_throw_value = throwval;
	rep_POPGC; rep_POPGC; rep_POPGC;
	return(res);
    }
    return rep_signal_missing_arg(1);
}

DEFUN("with-object", Fwith_object, Swith_object, (repv args), rep_SF) /*
::doc:with-object::
with-object ARG FORMS...

Evaluate ARG and make its value ``current'' in some way meaningful for
the data type, evaluate all FORMS, then return to the old current value
of whatever was changed. Return the value of the last FORM evaluated.
::end:: */
{
    repv res;
    if(rep_CONSP(args))
    {
	rep_GC_root gc_args;
	rep_PUSHGC(gc_args, args);
	res = Feval(rep_CAR(args));
	if (res != rep_NULL)
	{
	    repv handle = rep_bind_object(res);
	    if (handle != rep_NULL)
	    {
		rep_GC_root gc_handle;
		rep_PUSHGC(gc_handle, handle);
		res = Fprogn(rep_CDR(args));
		rep_POPGC;
		rep_PUSHGC(gc_handle, res);
		rep_unbind_object(handle);
		rep_POPGC;
	    }
	    else
		res = rep_NULL;
	}
	rep_POPGC;
    }
    else
	res = rep_signal_arg_error(res, 1);
    return res;
}

DEFSTRING(jl, ".jl");
DEFSTRING(jlc, ".jlc");

void
rep_lispcmds_init(void)
{
    rep_ADD_SUBR(Squote);
    rep_ADD_SUBR(Slambda);
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
    rep_ADD_SUBR(Sprog1);
    rep_ADD_SUBR(Swhile);
    rep_ADD_SUBR(Scond);
    rep_ADD_SUBR(Scase);
    rep_ADD_SUBR(Sapply);
    rep_ADD_SUBR_INT(Sload);
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
    rep_ADD_SUBR(Sbytecodep);
    rep_ADD_SUBR(Sfunctionp);
    rep_ADD_SUBR(Smacrop);
    rep_ADD_SUBR(Sspecial_form_p);
    rep_ADD_SUBR(Ssubrp);
    rep_ADD_SUBR(Ssequencep);
    rep_ADD_SUBR(Ssubr_name);
    rep_ADD_SUBR(Scall_hook);
    rep_ADD_SUBR(Scatch);
    rep_ADD_SUBR(Sthrow);
    rep_ADD_SUBR(Sunwind_protect);
    rep_ADD_SUBR(Swith_object);

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
	Fset (Qdocumentation_file, rep_string_dup(REP_DOC_FILE));

    rep_INTERN_SPECIAL(documentation_files);
    Fset (Qdocumentation_files,
	  Fcons (Fsymbol_value (Qdocumentation_file, Qt), Qnil));

    rep_INTERN_SPECIAL(load_path);
    Fset (Qload_path, rep_list_3(Fsymbol_value (Qlisp_lib_directory, Qt),
				 Fsymbol_value (Qsite_lisp_directory, Qt),
				 rep_VAL(&dot)));

    rep_INTERN_SPECIAL(dl_load_path);
    Fset (Qdl_load_path, Qnil);
    {
	char *ptr = getenv("LD_LIBRARY_PATH");
	while (ptr != 0 && *ptr != 0)
	{
	    char *end = strchr(ptr, ':');
	    Fset (Qdl_load_path,
		  Fcons(end ? rep_string_dupn(ptr, end - ptr)
			: rep_string_dup(ptr), Fsymbol_value (Qdl_load_path, Qt)));
	    ptr = end ? end + 1 : 0;
	}
    }
    Fset (Qdl_load_path,
	  Fcons (Fsymbol_value (Qexec_directory, Qt),
		 Fcons (rep_string_dup (REP_COMMON_EXEC_DIRECTORY),
			Fnreverse(Fsymbol_value (Qdl_load_path, Qt)))));

    rep_INTERN_SPECIAL(after_load_alist);
    Fset (Qafter_load_alist, Qnil);

    rep_INTERN(or); rep_INTERN(and);

    rep_INTERN_SPECIAL(dl_load_reloc_now);
    Fset (Qdl_load_reloc_now, Qnil);

    rep_INTERN_SPECIAL(load_filename);

    rep_INTERN (_load_suffixes);
    F_structure_set (rep_structure, Q_load_suffixes, Fcons (rep_VAL (&jl),
							    rep_VAL (&jlc)));
}
