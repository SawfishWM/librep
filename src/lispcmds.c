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

#include "jade.h"
#include "jade_protos.h"

#include <string.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

_PR void lispcmds_init(void);

_PR VALUE sym_load_path;
DEFSTRING(lisp_lib_dir, LISP_LIB_DIR);
DEFSTRING(site_lisp_dir, SITE_LISP_DIR);
DEFSTRING(div_zero, "Divide by zero");
DEFSTRING(doc_file, DOC_FILE);

DEFSYM(or, "or");
DEFSYM(and, "and");
DEFSYM(load_path, "load-path");
DEFSYM(after_load_alist, "after-load-alist");
DEFSYM(lisp_lib_dir, "lisp-lib-dir"); /*
::doc:load_path::
A list of directory names. When `load' opens a lisp-file it searches each
directory named in this list in turn until the file is found or the list
is exhausted.
::end::
::doc:after_load_alist::
A list of (LIBRARY FORMS...). Whenever the `load' command reads a file
of Lisp code LIBRARY, it executes each of FORMS. Note that LIBRARY must
exactly match the FILE argument given to `load'.
::end::
::doc:lisp_lib_dir::
The name of the directory in which the standard lisp files live.
::end:: */

_PR VALUE cmd_quote(VALUE);
DEFUN("quote", cmd_quote, subr_quote, (VALUE args), V_SF, DOC_quote) /*
::doc:quote::
quote ARG
'ARG

Returns ARG.
::end:: */
{
    if(CONSP(args))
	return(VCAR(args));
    return signal_missing_arg(1);
}

_PR VALUE cmd_defmacro(VALUE);
DEFUN("defmacro", cmd_defmacro, subr_defmacro, (VALUE args), V_SF, DOC_defmacro) /*
::doc:defmacro::
defmacro NAME LAMBDA-LIST [DOC-STRING] BODY...

Defines a macro called NAME with argument spec. LAMBDA-LIST, documentation
DOC-STRING (optional) and body BODY. The actual function value is
    `(macro lambda LAMBDA-LIST [DOC-STRING] BODY...)'
Macros are called with their arguments un-evaluated, they are expected to
return a form which will be executed to provide the result of the expression.

A pathetic example could be,
  (defmacro foo (x) (list 'cons nil x))
   => foo
  (foo 'bar)
   => (nil . bar)
This makes `(foo X)' a pseudonym for `(cons nil X)'.

Note that macros are expanded at *compile-time* (unless, of course, the Lisp
code has not been compiled).
::end:: */
{
    if(CONSP(args)
       && cmd_fset(VCAR(args), cmd_cons(sym_macro,
					cmd_cons(sym_lambda, VCDR(args)))))
    {
	return(VCAR(args));
    }
    else
	return signal_missing_arg(1);
}

_PR VALUE cmd_defun(VALUE);
DEFUN("defun", cmd_defun, subr_defun, (VALUE args), V_SF, DOC_defun) /*
::doc:defun::
defun NAME LAMBDA-LIST [DOC-STRING] BODY...

Defines a function called NAME with argument specification LAMBDA-LIST,
documentation DOC-STRING (optional) and body BODY. The actual function
value is,
    `(lambda LAMBDA-LIST [DOC-STRING] BODY...)'
::end:: */
{
    if(CONSP(args)
       && cmd_fset(VCAR(args), cmd_cons(sym_lambda, VCDR(args))))
    {
	return(VCAR(args));
    }
    else
	return signal_missing_arg(1);
}

_PR VALUE cmd_defvar(VALUE);
DEFUN("defvar", cmd_defvar, subr_defvar, (VALUE args), V_SF, DOC_defvar) /*
::doc:defvar::
defvar NAME DEFAULT-VALUE [DOC-STRING]

Define a variable called NAME whose standard value is DEFAULT-
VALUE. If NAME is already bound to a value it is left as it is.
If the symbol NAME is marked buffer-local the *default value* of the
variable will be set (if necessary) not the local value.
::end:: */
{
    if(CONSP(args) && CONSP(VCDR(args)))
    {
	GC_root gc_args;
	VALUE sym = VCAR(args), val;
	VALUE tmp = cmd_default_boundp(sym);
	if(!tmp)
	    return LISP_NULL;
	PUSHGC(gc_args, args);
	val = cmd_eval(VCAR(VCDR(args)));
	POPGC;
	if(!val)
	    return LISP_NULL;
	if(NILP(tmp))
	{
	    if(!cmd_set_default(sym, val))
		return LISP_NULL;
	}
	if(CONSP(VCDR(VCDR(args))))
	{
	    if(!cmd_put(sym, sym_variable_documentation, VCAR(VCDR(VCDR(args)))))
		return LISP_NULL;
	}
	return(sym);
    }
    else
	return signal_missing_arg(CONSP(args) ? 2 : 1);
}

DEFSTRING(const_bound, "Constant already bound");

_PR VALUE cmd_defconst(VALUE);
DEFUN("defconst", cmd_defconst, subr_defconst, (VALUE args), V_SF, DOC_defconst) /*
::doc:defconst::
defconst NAME VALUE [DOC-STRING]

Define a constant NAME whose (default) value is VALUE. If NAME is already
bound an error is signalled.

Constants are treated specially by the Lisp compiler, basically they are
hard-coded into the byte-code. For more details see the comments in
the compiler source (`lisp/compiler.jl').
::end:: */
{
    if(CONSP(args))
    {
	VALUE tmp = cmd_default_boundp(VCAR(args));
	if(tmp && !NILP(tmp))
	{
	    return(cmd_signal(sym_error, list_2(VAL(&const_bound),
						VCAR(args))));
	}
	tmp = cmd_defvar(args);
	if(tmp)
	    return(cmd_set_const_variable(tmp, sym_nil));
	return(tmp);
    }
    return signal_missing_arg(1);
}

_PR VALUE cmd_car(VALUE);
DEFUN("car", cmd_car, subr_car, (VALUE cons), V_Subr1, DOC_car) /*
::doc:car::
car CONS-CELL

Returns the value stored in the car slot of CONS-CELL, or nil if CONS-CELL
is nil.
::end:: */
{
    if(CONSP(cons))
	return(VCAR(cons));
    return(sym_nil);
}
_PR VALUE cmd_cdr(VALUE);
DEFUN("cdr", cmd_cdr, subr_cdr, (VALUE cons), V_Subr1, DOC_cdr) /*
::doc:cdr::
cdr CONS-CELL

Returns the value stored in the cdr slot of CONS-CELL, or nil if CONS-CELL
is nil.
::end:: */
{
    if(CONSP(cons))
	return(VCDR(cons));
    return(sym_nil);
}

_PR VALUE cmd_list(VALUE);
DEFUN("list", cmd_list, subr_list, (VALUE args), V_SubrN, DOC_list) /*
::doc:list::
list ARGS...

Returns a new list with elements ARGS...
::end:: */
{
    VALUE res = sym_nil;
    VALUE *ptr = &res;
    while(CONSP(args))
    {
	if(!(*ptr = cmd_cons(VCAR(args), sym_nil)))
	    return LISP_NULL;
	ptr = &VCDR(*ptr);
	args = VCDR(args);
    }
    return(res);
}

_PR VALUE cmd_make_list(VALUE, VALUE);
DEFUN("make-list", cmd_make_list, subr_make_list, (VALUE len, VALUE init), V_Subr2, DOC_make_list) /*
::doc:make_list::
make-list LENGTH [INITIAL-VALUE]

Returns a new list with LENGTH members, each of which is initialised to
INITIAL-VALUE, or nil.
::end:: */
{
    int i;
    VALUE list = sym_nil;
    DECLARE1(len, INTP);
    for(i = 0; list != LISP_NULL && i < VINT(len); i++)
	list = cmd_cons(init, list);
    return(list);
}

_PR VALUE cmd_append(VALUE);
DEFUN("append", cmd_append, subr_append, (VALUE args), V_SubrN, DOC_append) /*
::doc:append::
append LISTS...

Non-destructively concatenates each of it's argument LISTS... into one
new list which is returned.
::end:: */
{
    int i = 1;
    VALUE res = sym_nil;
    VALUE *resend = &res;
    while(CONSP(args))
    {
	if(!LISTP(VCAR(args)))
	    return signal_arg_error(VCAR(args), i);
	if(CONSP(VCAR(args)) && CONSP(VCDR(args)))
	{
	    /* Only make a new copy if there's another list after this
	       one. */
	    *resend = copy_list(VCAR(args));
	}
	else
	    *resend = VCAR(args);	/* Use the old object */
	while(CONSP(*resend))
	{
	    TEST_INT;
	    if(INT_P)
		return LISP_NULL;
	    resend = &(VCDR(*resend));
	}
	args = VCDR(args);
	i++;
    }
    return(res);
}

_PR VALUE cmd_nconc(VALUE);
DEFUN("nconc", cmd_nconc, subr_nconc, (VALUE args), V_SubrN, DOC_nconc) /*
::doc:nconc::
nconc LISTS...

Destructively concatenates each of it's argument LISTS... into one new
list. Every LIST but the last is modified so that it's last cdr points
to the beginning of the next list. Returns the new list.
::end:: */
{
    int i = 1;
    VALUE res = sym_nil;
    VALUE *resend = &res;
    while(CONSP(args))
    {
	VALUE tmp = VCAR(args);
	if(!LISTP(tmp))
	    return signal_arg_error(tmp, i);
	if(CONSP(tmp))
	{
	    *resend = tmp;
	    while(CONSP(VCDR(tmp)))
	    {
		TEST_INT;
		if(INT_P)
		    return LISP_NULL;
		tmp = VCDR(tmp);
	    }
	    resend = &VCDR(tmp);
	}
	args = VCDR(args);
    }
    return(res);
}

_PR VALUE cmd_rplaca(VALUE, VALUE);
DEFUN("rplaca", cmd_rplaca, subr_rplaca, (VALUE cons, VALUE car), V_Subr2, DOC_rplaca) /*
::doc:rplaca::
rplaca CONS-CELL NEW-CAR

Sets the value of the car slot in CONS-CELL to NEW-CAR. Returns the new
value.
::end:: */
{
    DECLARE1(cons, CONSP);
    VCAR(cons) = car;
    return(car);
}

_PR VALUE cmd_rplacd(VALUE, VALUE);
DEFUN("rplacd", cmd_rplacd, subr_rplacd, (VALUE cons, VALUE cdr), V_Subr2, DOC_rplacd) /*
::doc:rplacd::
rplacd CONS-CELL NEW-CDR

Sets the value of the cdr slot in CONS-CELL to NEW-CAR. Returns the new
value.
::end:: */
{
    DECLARE1(cons, CONSP);
    VCDR(cons) = cdr;
    return(cdr);
}

_PR VALUE cmd_reverse(VALUE);
DEFUN("reverse", cmd_reverse, subr_reverse, (VALUE head), V_Subr1, DOC_reverse) /*
::doc:reverse::
reverse LIST

Returns a new list which is a copy of LIST except that the members are in
reverse order.
::end:: */
{
    VALUE res = sym_nil;
    DECLARE1(head, LISTP);
    while(CONSP(head))
    {
	res = cmd_cons(VCAR(head), res);
	head = VCDR(head);
	TEST_INT;
	if(res == LISP_NULL || INT_P)
	    return(LISP_NULL);
    }
    return(res);
}

_PR VALUE cmd_nreverse(VALUE);
DEFUN("nreverse", cmd_nreverse, subr_nreverse, (VALUE head), V_Subr1, DOC_nreverse) /*
::doc:nreverse::
nreverse LIST

Returns LIST altered so that it's members are in reverse order to what they
were. This function is destructive towards it's argument.
::end:: */
{
    VALUE res = sym_nil;
    VALUE nxt;
    DECLARE1(head, LISTP);
    if(NILP(head))
	return(head);
    do {
	if(CONSP(VCDR(head)))
	    nxt = VCDR(head);
	else
	    nxt = LISP_NULL;
	VCDR(head) = res;
	res = head;
	TEST_INT;
	if(INT_P)
	    return(LISP_NULL);
    } while((head = nxt) != LISP_NULL);
    return(res);
}

_PR VALUE cmd_assoc(VALUE, VALUE);
DEFUN("assoc", cmd_assoc, subr_assoc, (VALUE elt, VALUE list), V_Subr2, DOC_assoc) /*
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
    DECLARE2(list, LISTP);
    while(CONSP(list))
    {
	register VALUE car = VCAR(list);
	if(CONSP(car) && (!value_cmp(elt, VCAR(car))))
	    return(car);
	list = VCDR(list);
	TEST_INT;
	if(INT_P)
	    return(LISP_NULL);
    }
    return(sym_nil);
}

_PR VALUE cmd_assq(VALUE, VALUE);
DEFUN("assq", cmd_assq, subr_assq, (VALUE elt, VALUE list), V_Subr2, DOC_assq) /*
::doc:assq::
assq ELT ASSOC-LIST

Searches ASSOC-LIST for a list whose first element is ELT. `assq' uses `eq'
to compare elements. Returns the sub-list starting from the first matching
association.
::end:: */
{
    DECLARE2(list, LISTP);
    while(CONSP(list))
    {
	register VALUE car = VCAR(list);
	if(CONSP(car) && (elt == VCAR(car)))
	    return(car);
	list = VCDR(list);
	TEST_INT;
	if(INT_P)
	    return(LISP_NULL);
    }
    return(sym_nil);
}

_PR VALUE cmd_rassoc(VALUE, VALUE);
DEFUN("rassoc", cmd_rassoc, subr_rassoc, (VALUE elt, VALUE list), V_Subr2, DOC_rassoc) /*
::doc:rassoc::
rassoc ELT ASSOC-LIST

Searches ASSOC-LIST for a cons-cell whose cdr element is `equal' to ELT. 
Returns the first cons-cell which matches, or nil.
For example,
    (rassoc 3 '((one . 1) (two . 2) (three . 3) (four . 4)))
     => (three . 3)
::end:: */
{
    DECLARE2(list, LISTP);
    while(CONSP(list))
    {
	register VALUE car = VCAR(list);
	if(CONSP(car) && (!value_cmp(elt, VCDR(car))))
	    return(car);
	list = VCDR(list);
	TEST_INT;
	if(INT_P)
	    return(LISP_NULL);
    }
    return(sym_nil);
}

_PR VALUE cmd_rassq(VALUE, VALUE);
DEFUN("rassq", cmd_rassq, subr_rassq, (VALUE elt, VALUE list), V_Subr2, DOC_rassq) /*
::doc:rassq::
rassq ELT ASSOC-LIST

Searches ASSOC-LIST for a cons-cell whose cdr is `eq' to ELT.
Returns the first matching cons-cell, else nil.
::end:: */
{
    DECLARE2(list, LISTP);
    while(CONSP(list))
    {
	register VALUE car = VCAR(list);
	if(CONSP(car) && (elt == VCDR(car)))
	    return(car);
	list = VCDR(list);
	TEST_INT;
	if(INT_P)
	    return(LISP_NULL);
    }
    return(sym_nil);
}

_PR VALUE cmd_nth(VALUE, VALUE);
DEFUN("nth", cmd_nth, subr_nth, (VALUE index, VALUE list), V_Subr2, DOC_nth) /*
::doc:nth::
nth INDEX LIST

Returns the INDEXth element of LIST. The first element has an INDEX of zero.
::end:: */
{
    int i;
    DECLARE1(index, INTP);
    DECLARE2(list, LISTP);
    i = VINT(index);
    while((i-- > 0) && CONSP(list))
    {
	list = VCDR(list);
	TEST_INT;
	if(INT_P)
	    return LISP_NULL;
    }
    return (i <= 0 && CONSP(list)) ? VCAR(list) : sym_nil;
}

_PR VALUE cmd_nthcdr(VALUE index, VALUE list);
DEFUN("nthcdr", cmd_nthcdr, subr_nthcdr, (VALUE index, VALUE list), V_Subr2, DOC_nthcdr) /*
::doc:nthcdr::
nthcdr INDEX LIST

Returns the INDEXth cdr of LIST. The first is INDEX zero.
::end:: */
{
    int i;
    DECLARE1(index, INTP);
    DECLARE2(list, LISTP);
    i = VINT(index);
    while((i-- > 0) && CONSP(list))
    {
	list = VCDR(list);
	TEST_INT;
	if(INT_P)
	    return LISP_NULL;
    }
    return list;
}

_PR VALUE cmd_last(VALUE);
DEFUN("last", cmd_last, subr_last, (VALUE list), V_Subr1, DOC_last) /*
::doc:last::
last LIST

Returns the last element of LIST.
::end:: */
{
    DECLARE1(list, LISTP);
    if(CONSP(list))
    {
	while(CONSP(VCDR(list)))
	{
	    list = VCDR(list);
	    TEST_INT;
	    if(INT_P)
		return(LISP_NULL);
	}
	return(VCAR(list));
    }
    return(sym_nil);
}

_PR VALUE cmd_mapcar(VALUE, VALUE);
DEFUN("mapcar", cmd_mapcar, subr_mapcar, (VALUE fun, VALUE list), V_Subr2, DOC_mapcar) /*
::doc:mapcar::
mapcar FUNCTION LIST

Calls FUNCTION-NAME with each element of LIST as an argument in turn and
returns a new list constructed from the results, ie,
  (mapcar (function (lambda (x) (1+ x))) '(1 2 3))
   => (2 3 4)
::end:: */
{
    VALUE res = sym_nil;
    VALUE *last = &res;
    GC_root gc_list, gc_argv, gc_res;
    VALUE argv;
    DECLARE2(list, LISTP);
    argv = cmd_cons(fun, cmd_cons(sym_nil, sym_nil));
    if(argv)
    {
	PUSHGC(gc_res, res);
	PUSHGC(gc_argv, argv);
	PUSHGC(gc_list, list);
	while(res != LISP_NULL && CONSP(list))
	{
	    if(!(*last = cmd_cons(sym_nil, sym_nil)))
		return(LISP_NULL);
	    VCAR(VCDR(argv)) = VCAR(list);
	    if(!(VCAR(*last) = cmd_funcall(argv)))
		res = LISP_NULL;
	    else
	    {
		last = &VCDR(*last);
		list = VCDR(list);
	    }
	    TEST_INT;
	    if(INT_P)
		res = LISP_NULL;
	}
	POPGC; POPGC; POPGC;
    }
    return(res);
}

_PR VALUE cmd_mapc(VALUE, VALUE);
DEFUN("mapc", cmd_mapc, subr_mapc, (VALUE fun, VALUE list), V_Subr2, DOC_mapc) /*
::doc:mapc::
mapc FUNCTION LIST

Applies FUNCTION to each element in LIST, discards the results.
::end:: */
{
    VALUE argv, res = list;
    GC_root gc_argv, gc_list;
    DECLARE2(list, LISTP);
    if(!(argv = cmd_cons(fun, cmd_cons(sym_nil, sym_nil))))
	return(LISP_NULL);
    PUSHGC(gc_argv, argv);
    PUSHGC(gc_list, list);
    while(res != LISP_NULL && CONSP(list))
    {
	VCAR(VCDR(argv)) = VCAR(list);
	if(!cmd_funcall(argv))
	    res = LISP_NULL;
	list = VCDR(list);
	TEST_INT;
	if(INT_P)
	    res = LISP_NULL;
    }
    POPGC; POPGC;
    return(res);
}

_PR VALUE cmd_member(VALUE, VALUE);
DEFUN("member", cmd_member, subr_member, (VALUE elt, VALUE list), V_Subr2, DOC_member) /*
::doc:member::
member ELT LIST

If ELT is a member of list LIST then return the tail of the list starting
from the matched ELT, ie,
  (member 1 '(2 1 3))
   => (1 3)
`member' uses `equal' to compare atoms.
::end:: */
{
    DECLARE2(list, LISTP);
    while(CONSP(list))
    {
	if(!value_cmp(elt, VCAR(list)))
	    return(list);
	list = VCDR(list);
	TEST_INT;
	if(INT_P)
	    return(LISP_NULL);
    }
    return(sym_nil);
}

_PR VALUE cmd_memq(VALUE, VALUE);
DEFUN("memq", cmd_memq, subr_memq, (VALUE elt, VALUE list), V_Subr2, DOC_memq) /*
::doc:memq::
memq ELT LIST

If ELT is a member of list LIST then return the tail of the list starting
from the matched ELT, ie,
  (memq 1 '(2 1 3))
   => (1 3)
`memq' uses `eq' to compare atoms.
::end:: */
{
    DECLARE2(list, LISTP);
    while(CONSP(list))
    {
	if(elt == VCAR(list))
	    return(list);
	list = VCDR(list);
	TEST_INT;
	if(INT_P)
	    return(LISP_NULL);
    }
    return(sym_nil);
}

_PR VALUE cmd_delete(VALUE, VALUE);
DEFUN("delete", cmd_delete, subr_delete, (VALUE elt, VALUE list), V_Subr2, DOC_delete) /*
::doc:delete::
delete ELT LIST

Returns LIST with any members `equal' to ELT destructively removed.
::end:: */
{
    VALUE *head = &list;
    DECLARE2(list, LISTP);
    while(CONSP(*head))
    {
	if(!value_cmp(elt, VCAR(*head)))
	    *head = VCDR(*head);
	else
	    head = &VCDR(*head);
	TEST_INT;
	if(INT_P)
	    return(LISP_NULL);
    }
    return(list);
}

_PR VALUE cmd_delq(VALUE, VALUE);
DEFUN("delq", cmd_delq, subr_delq, (VALUE elt, VALUE list), V_Subr2, DOC_delq) /*
::doc:delq::
delq ELT LIST

Returns LIST with any members `eq' to ELT destructively removed.
::end:: */
{
    VALUE *head = &list;
    DECLARE2(list, LISTP);
    while(CONSP(*head))
    {
	if(elt == VCAR(*head))
	    *head = VCDR(*head);
	else
	    head = &VCDR(*head);
	TEST_INT;
	if(INT_P)
	    return(LISP_NULL);
    }
    return(list);
}

_PR VALUE cmd_delete_if(VALUE, VALUE);
DEFUN("delete-if", cmd_delete_if, subr_delete_if, (VALUE pred, VALUE list), V_Subr2, DOC_delete_if) /*
::doc:delete_if::
delete-if FUNCTION LIST

Similar to `delete' except that a predicate function, FUNCTION-NAME, is
used to decide which elements to delete (remove destructively).
`delete-if' deletes an element if FUNCTION-NAME returns non-nil when 
applied to that element, ie,
  (delete-if '(lambda (x) (= x 1)) '(1 2 3 4 1 2))
   => (2 3 4 2)
::end:: */
{
    VALUE *head = &list;
    VALUE tmp;
    DECLARE2(list, LISTP);
    while(CONSP(*head))
    {
	if(!(tmp = call_lisp1(pred, VCAR(*head))))
	    return(LISP_NULL);
	if(!NILP(tmp))
	    *head = VCDR(*head);
	else
	    head = &VCDR(*head);
	TEST_INT;
	if(INT_P)
	    return(LISP_NULL);
    }
    return(list);
}

_PR VALUE cmd_delete_if_not(VALUE, VALUE);
DEFUN("delete-if-not", cmd_delete_if_not, subr_delete_if_not, (VALUE pred, VALUE list), V_Subr2, DOC_delete_if_not) /*
::doc:delete_if_not::
delete-if-not FUNCTION LIST

Similar to `delete' except that a predicate function, FUNCTION-NAME, is
used to decide which elements to delete (remove destructively).
`delete-if-not' deletes an element if FUNCTION-NAME returns nil when 
applied to that element, ie,
  (delete-if-not '(lambda (x) (= x 1)) '(1 2 3 4 1 2))
   => (1 1)
::end:: */
{
    VALUE *head = &list;
    VALUE tmp;
    DECLARE2(list, LISTP);
    while(CONSP(*head))
    {
	if(!(tmp = call_lisp1(pred, VCAR(*head))))
	    return(LISP_NULL);
	if(NILP(tmp))
	    *head = VCDR(*head);
	else
	    head = &VCDR(*head);
	TEST_INT;
	if(INT_P)
	    return(LISP_NULL);
    }
    return(list);
}

_PR VALUE cmd_vector(VALUE);
DEFUN("vector", cmd_vector, subr_vector, (VALUE args), V_SubrN, DOC_vector) /*
::doc:vector::
vector ARGS...

Returns a new vector with ARGS... as its elements.
::end:: */
{
    VALUE res = make_vector(list_length(args));
    if(res)
    {
	int i = 0;
	while(CONSP(args))
	{
	    VVECTI(res, i) = VCAR(args);
	    args = VCDR(args);
	    i++;
	    TEST_INT;
	    if(INT_P)
		return(LISP_NULL);
	}
    }
    return(res);
}

_PR VALUE cmd_make_vector(VALUE, VALUE);
DEFUN("make-vector", cmd_make_vector, subr_make_vector, (VALUE size, VALUE init), V_Subr2, DOC_make_vector) /*
::doc:make_vector::
make-vector SIZE [INITIAL-VALUE]

Creates a new vector of size SIZE. If INITIAL-VALUE is provided each element
will be set to that value, else they will all be nil.
::end:: */
{
    int len;
    VALUE res;
    DECLARE1(size, INTP);
    len = VINT(size);
    res = make_vector(len);
    if(res)
    {
	int i;
	for(i = 0; i < len; i++)
	    VVECTI(res, i) = init;
    }
    return(res);
}

_PR VALUE cmd_arrayp(VALUE);
DEFUN("arrayp", cmd_arrayp, subr_arrayp, (VALUE arg), V_Subr1, DOC_arrayp) /*
::doc:arrayp::
arrayp ARG

Returns t when ARG is an array.
::end:: */
{
    return((VECTORP(arg) || STRINGP(arg) || COMPILEDP(arg)) ? sym_t : sym_nil);
}

_PR VALUE cmd_aset(VALUE, VALUE, VALUE);
DEFUN("aset", cmd_aset, subr_aset, (VALUE array, VALUE index, VALUE new), V_Subr3, DOC_aset) /*
::doc:aset::
aset ARRAY INDEX NEW-VALUE

Sets element number INDEX (a positive integer) of ARRAY (can be a vector
or a string) to NEW-VALUE, returning NEW-VALUE. Note that strings
can only contain characters (ie, integers).
::end:: */
{
    DECLARE2(index, INTP);
    if(STRINGP(array))
    {
	if(!STRING_WRITABLE_P(array))
	    return cmd_signal(sym_setting_constant, LIST_1(array));
	if(VINT(index) < STRING_LEN(array))
	{
	    DECLARE3(new, INTP);
	    VSTR(array)[VINT(index)] = (u_char)VINT(new);
	    return(new);
	}
    }
    else if(VECTORP(array) || COMPILEDP(array))
    {
	if(!VECTOR_WRITABLE_P(array))
	    return cmd_signal(sym_setting_constant, LIST_1(array));
	if(VINT(index) < VVECT_LEN(array))
	{
	    VVECTI(array, VINT(index)) = new;
	    return(new);
	}
    }
    else
	return(signal_arg_error(array, 1));
    return(signal_arg_error(index, 2));
}

_PR VALUE cmd_aref(VALUE, VALUE);
DEFUN("aref", cmd_aref, subr_aref, (VALUE array, VALUE index), V_Subr2, DOC_aref) /*
::doc:aref::
aref ARRAY INDEX

Returns the INDEXth (a non-negative integer) element of ARRAY, which
can be a vector or a string. INDEX starts at zero.
::end:: */
{
    DECLARE2(index, INTP);
    if(STRINGP(array))
    {
	if(VINT(index) < STRING_LEN(array))
	    return(MAKE_INT(VSTR(array)[VINT(index)]));
    }
    else if(VECTORP(array) || COMPILEDP(array))
    {
	if(VINT(index) < VVECT_LEN(array))
	    return(VVECTI(array, VINT(index)));
    }
    else
	return(cmd_signal(sym_bad_arg, list_2(array, MAKE_INT(1))));
    return(signal_arg_error(index, 2));
}

_PR VALUE cmd_make_string(VALUE, VALUE);
DEFUN("make-string", cmd_make_string, subr_make_string, (VALUE len, VALUE init), V_Subr2, DOC_make_string) /*
::doc:make_string::
make-string LENGTH [INITIAL-VALUE]

Returns a new string of length LENGTH, each character is initialised to
INITIAL-VALUE, or to space if INITIAL-VALUE is not given.
::end:: */
{
    VALUE res;
    DECLARE1(len, INTP);
    res = make_string(VINT(len) + 1);
    if(res)
    {
	memset(VSTR(res), INTP(init) ? (u_char)VINT(init) : ' ', VINT(len));
	VSTR(res)[VINT(len)] = 0;
    }
    return(res);
}

static INLINE int
extend_concat(u_char **buf, int *bufLen, int i, int addLen)
{
    u_char *newbuf;
    int newbuflen;
    if((i + addLen) < *bufLen)
	return(TRUE);
    newbuflen = (i + addLen) * 2;
    newbuf = str_alloc(newbuflen);
    if(newbuf)
    {
	memcpy(newbuf, *buf, i);
	str_free(*buf);
	*buf = newbuf;
	*bufLen = newbuflen;
	return(TRUE);
    }
    mem_error();
    return(FALSE);
}
_PR VALUE cmd_concat(VALUE);
DEFUN("concat", cmd_concat, subr_concat, (VALUE args), V_SubrN, DOC_concat) /*
::doc:concat::
concat ARGS...

Concatenates all ARGS... into a single string, each argument can be a string,
a character or a list or vector of characters.
::end:: */
{
    int buflen = MAXBUCKETSIZE;	/* biggest block that will be cached */
    u_char *buf = str_alloc(buflen);
    if(buf)
    {
	VALUE res = LISP_NULL;
	int i = 0;
	int argnum = 1;
	while(CONSP(args))
	{
	    VALUE arg = VCAR(args);
	    switch(VTYPE(arg))
	    {
		int slen, j;
	    case V_String:
		slen = STRING_LEN(arg);
		if(!extend_concat(&buf, &buflen, i, slen))
		    goto error;
		memcpy(buf + i, VSTR(arg), slen);
		i += slen;
		break;
	    case V_Int:
		if(!extend_concat(&buf, &buflen, i, 1))
		    goto error;
		buf[i++] = VINT(arg);
		break;
	    case V_Symbol:
		if(arg != sym_nil)
		    break;
		/* FALL THROUGH */
	    case V_Cons:
		while(CONSP(arg))
		{
		    VALUE ch = VCAR(arg);
		    if(INTP(ch))
		    {
			if(!extend_concat(&buf, &buflen, i, 1))
			    goto error;
			buf[i++] = VINT(ch);
		    }
		    arg = VCDR(arg);
		    TEST_INT;
		    if(INT_P)
			goto error;
		}
		break;
	    case V_Vector:
		{
		    int len = VVECT_LEN(arg);
		    for(j = 0; j < len; j++)
		    {
			if(INTP(VVECTI(arg, j)))
			{
			    if(!extend_concat(&buf, &buflen, i, 1))
				goto error;
			    buf[i++] = VINT(VVECTI(arg, j));
			}
		    }
		    break;
		}
	    default:
		res = signal_arg_error(arg, argnum);
		goto error;
	    }
	    args = VCDR(args);
	    argnum++;
	}
	res = string_dupn(buf, i);
error:
	str_free(buf);
	return(res);
    }
    return(LISP_NULL);
}

_PR VALUE cmd_length(VALUE);
DEFUN("length", cmd_length, subr_length, (VALUE sequence), V_Subr1, DOC_length) /*
::doc:length::
length SEQUENCE

Returns the number of elements in SEQUENCE (a string, list or vector).
::end:: */
{
    switch(VTYPE(sequence))
    {
	int i;
    case V_String:
	return(MAKE_INT(STRING_LEN(sequence)));
	break;
    case V_Vector: case V_Compiled:
	return(MAKE_INT(VVECT_LEN(sequence)));
	break;
    case V_Cons:
	i = 0;
	while(CONSP(sequence))
	{
	    sequence = VCDR(sequence);
	    i++;
	    TEST_INT;
	    if(INT_P)
		return(LISP_NULL);
	}
	return(MAKE_INT(i));
	break;
    case V_Symbol:
	if(sequence == sym_nil)
	    return(MAKE_INT(0));
	/* FALL THROUGH */
    default:
	cmd_signal(sym_bad_arg, list_2(sequence, MAKE_INT(1)));
	return(LISP_NULL);
    }
}

_PR VALUE cmd_copy_sequence(VALUE);
DEFUN("copy-sequence", cmd_copy_sequence, subr_copy_sequence, (VALUE seq), V_Subr1, DOC_copy_sequence) /*
::doc:copy_sequence::
copy-sequence SEQUENCE

Returns a new sequence whose elements are eq to those in SEQUENCE.
::end:: */
{
    VALUE res = sym_nil;
    switch(VTYPE(seq))
    {
    case V_Symbol:
	if(!NILP(seq))
	    res = signal_arg_error(seq, 1);
	break;
    case V_Cons:
	{
	    VALUE *last = &res;
	    while(CONSP(seq))
	    {
		TEST_INT;
		if(INT_P)
		    return(LISP_NULL);
		if(!(*last = cmd_cons(VCAR(seq), sym_nil)))
		    return(LISP_NULL);
		last = &VCDR(*last);
		seq = VCDR(seq);
	    }
	}
	break;
    case V_Vector: case V_Compiled:
	res = make_vector(VVECT_LEN(seq));
	if(res)
	{
	    int i, len = VVECT_LEN(seq);
	    VVECT(res)->car = VVECT(seq)->car;
	    for(i = 0; i < len; i++)
		VVECTI(res, i) = VVECTI(seq, i);
	}
	break;
    case V_String:
	res = string_dupn(VSTR(seq), STRING_LEN(seq));
	break;
    default:
	res = signal_arg_error(seq, 1);
    }
    return(res);
}

_PR VALUE cmd_elt(VALUE, VALUE);
DEFUN("elt", cmd_elt, subr_elt, (VALUE seq, VALUE index), V_Subr2, DOC_elt) /*
::doc:elt::
elt SEQUENCE INDEX

Return the element of SEQUENCE at position INDEX (counting from zero).
::end:: */
{
    if(NILP(cmd_arrayp(seq)))
	return(cmd_nth(index, seq));
    else
	return(cmd_aref(seq, index));
}

_PR VALUE cmd_prog1(VALUE);
DEFUN("prog1", cmd_prog1, subr_prog1, (VALUE args), V_SF, DOC_prog1) /*
::doc:prog1::
prog1 FORM1 FORMS...

First evals FORM1 then FORMS, returns the value that FORM1 gave.
::end:: */
{
    if(CONSP(args))
    {
	VALUE res;
	GC_root gc_args, gc_res;
	PUSHGC(gc_args, args);
	res = cmd_eval(VCAR(args));
	if(res)
	{
	    PUSHGC(gc_res, res);
	    if(!cmd_progn(VCDR(args)))
		res = LISP_NULL;
	    POPGC;
	}
	POPGC;
	return(res);
    }
    return signal_missing_arg(1);
}

_PR VALUE cmd_prog2(VALUE);
DEFUN("prog2", cmd_prog2, subr_prog2, (VALUE args), V_SF, DOC_prog2) /*
::doc:prog2::
prog2 FORM1 FORM2 FORMS...

Evals FORM1 then FORM2 then the rest. Returns whatever FORM2 gave.
::end:: */
{
    if(CONSP(args) && CONSP(VCDR(args)))
    {
	VALUE res;
	GC_root gc_args, gc_res;
	PUSHGC(gc_args, args);
	if(cmd_eval(VCAR(args)))
	{
	    res = cmd_eval(VCAR(VCDR(args)));
	    if(res)
	    {
		PUSHGC(gc_res, res);
		if(!cmd_progn(VCDR(VCDR(args))))
		    res = LISP_NULL;
		POPGC;
	    }
	}
	else
	    res = LISP_NULL;
	POPGC;
	return(res);
    }
    return signal_missing_arg(CONSP(args) ? 2 : 1);
}

_PR VALUE cmd_while(VALUE);
DEFUN("while", cmd_while, subr_while, (VALUE args), V_SF, DOC_while) /*
::doc:while::
while CONDITION FORMS...

Eval CONDITION, if it is non-nil then execute FORMS and repeat the
procedure, else return nil.
::end:: */
{
    if(CONSP(args))
    {
	GC_root gc_args;
	VALUE cond = VCAR(args), wval, body = VCDR(args);
	PUSHGC(gc_args, args);
	while((wval = cmd_eval(cond)) && !NILP(wval))
	{
	    TEST_INT;
	    if(INT_P || !cmd_progn(body))
	    {
		wval = LISP_NULL;
		break;
	    }
	}
	POPGC;
	if(!wval)
	    return(LISP_NULL);
	return(sym_nil);
    }
    return signal_missing_arg(1);
}

_PR VALUE cmd_cond(VALUE);
DEFUN("cond", cmd_cond, subr_cond, (VALUE args), V_SF, DOC_cond) /*
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
    VALUE res = sym_nil;
    GC_root gc_args;
    PUSHGC(gc_args, args);
    while(CONSP(args) && CONSP(VCAR(args)))
    {
	VALUE cndlist = VCAR(args);
	if(!(res = cmd_eval(VCAR(cndlist))))
	    break;
	if(!NILP(res))
	{
	    if(CONSP(VCDR(cndlist)))
	    {
		if(!(res = cmd_progn(VCDR(cndlist))))
		    break;
	    }
	    break;
	}
	args = VCDR(args);
    }
    POPGC;
    return(res);
}

_PR VALUE cmd_apply(VALUE);
DEFUN("apply", cmd_apply, subr_apply, (VALUE args), V_SubrN, DOC_apply) /*
::doc:apply::
apply FUNCTION ARGS... ARG-LIST

Calls FUNCTION passing all of ARGS to it as well as all elements in ARG-LIST.
ie,
  (apply '+ 1 2 3 '(4 5 6))
   => 21
::end:: */
{
    VALUE list = sym_nil, *last;
    last = &list;
    if(CONSP(args))
    {
	while(CONSP(VCDR(args)))
	{
	    if(!(*last = cmd_cons(VCAR(args), sym_nil)))
		return(LISP_NULL);
	    last = &VCDR(*last);
	    args = VCDR(args);
	    TEST_INT;
	    if(INT_P)
		return(LISP_NULL);
	}
	if(!NILP(cmd_listp(VCAR(args))))
	    *last = VCAR(args);
	else
	    return(cmd_signal(sym_bad_arg, LIST_1(VCAR(args))));
	return(cmd_funcall(list));
    }
    return signal_missing_arg(1);
}

DEFSTRING(r_str, "r");
DEFSTRING(no_load_file, "Can't open lisp-file");

_PR VALUE cmd_load(VALUE file, VALUE noerr_p, VALUE nopath_p, VALUE nosuf_p);
DEFUN_INT("load", cmd_load, subr_load, (VALUE file, VALUE noerr_p, VALUE nopath_p, VALUE nosuf_p), V_Subr4, DOC_load, "fLisp file to load:") /*
::doc:load::
load FILE [NO-ERROR-P] [NO-PATH-P] [NO-SUFFIX-P]

Attempt to open and then read-and-eval the file of Lisp code FILE.

For each directory named in the variable `load-path' tries the value of
FILE with `.jlc' (compiled-lisp) appended to it, then with `.jl' appended
to it, finally tries FILE without modification.

If NO-ERROR-P is non-nil no error is signalled if FILE can't be found.
If NO-PATH-P is non-nil the `load-path' variable is not used, just the value
of FILE.
If NO-SUFFIX-P is non-nil no suffixes are appended to FILE.

If the compiled version is older than it's source code, the source code is
loaded and a warning is displayed.
::end:: */
{
    VALUE name = LISP_NULL, stream, path;
    DECLARE1(file, STRINGP);
    if(NILP(nopath_p))
    {
	path = cmd_symbol_value(sym_load_path, sym_nil);
	if(!path)
	    return(LISP_NULL);
    }
    else
	path = cmd_cons(null_string(), sym_nil);
    while(!name && CONSP(path))
    {
	u_char *dir = STRINGP(VCAR(path)) ? VSTR(VCAR(path)) : (u_char *)"";
	if(NILP(nosuf_p))
	{
	    bool jl_p = file_exists3(dir, VSTR(file), ".jl");
	    if(file_exists3(dir, VSTR(file), ".jlc"))
	    {
		name = concat3(dir, VSTR(file), ".jlc");
		if(jl_p)
		{
		    VALUE tmp = concat3(dir, VSTR(file), ".jl");
		    if(file_mod_time(VSTR(tmp)) > file_mod_time(VSTR(name)))
		    {
			messagef("Warning: %s newer than %s, using .jl",
				     VSTR(tmp), VSTR(name));
			name = tmp;
		    }
		}
	    }
	    else if(jl_p)
		name = concat3(dir, VSTR(file), ".jl");
	}
	if(!name && file_exists2(dir, VSTR(file)))
	    name = concat2(dir, VSTR(file));
	path = VCDR(path);
	TEST_INT;
	if(INT_P)
	    return(LISP_NULL);
    }
    if(!name)
    {
	if(NILP(noerr_p))
	    return(cmd_signal(sym_file_error,
			      list_2(VAL(&no_load_file), file)));
	else
	    return(sym_nil);
    }
    if((stream = cmd_open(name, VAL(&r_str), sym_nil)) && FILEP(stream))
    {
	VALUE obj;
	int c;
	GC_root gc_stream, gc_file;
	PUSHGC(gc_stream, stream);
	PUSHGC(gc_file, file);
	c = stream_getc(stream);
	while((c != EOF) && (obj = readl(stream, &c)))
	{
	    TEST_INT;
	    if(INT_P || !cmd_eval(obj))
	    {
		POPGC; POPGC;
		return(LISP_NULL);
	    }
	}
	POPGC; POPGC;

	/* Loading succeded. Look for an applicable item in
	   the after-load-alist. */
	obj = cmd_symbol_value(sym_after_load_alist, sym_t);
	if(obj != LISP_NULL && CONSP(obj))
	{
	    obj = cmd_assoc(file, obj);
	    if(obj != LISP_NULL && CONSP(obj))
		cmd_progn(VCDR(obj));
	}

	return(sym_t);
    }
    return(LISP_NULL);
}

/*
 * some arithmetic commands
 */

#define APPLY_OP(op)					\
    if(CONSP(args) && INTP(VCAR(args)))			\
    {							\
	long sum = VINT(VCAR(args));			\
	int i = 2;					\
	args = VCDR(args);				\
	while(CONSP(args))				\
	{						\
	    if(!INTP(VCAR(args)))			\
		return signal_arg_error(VCAR(args), i);	\
	    sum = sum op VINT(VCAR(args));		\
	    args = VCDR(args);				\
	    i++;					\
	    TEST_INT;					\
	    if(INT_P)					\
		return(LISP_NULL);			\
	}						\
	return(MAKE_INT(sum));				\
    }							\
    return (CONSP(args)					\
	    ? signal_arg_error(VCAR(args), 1)		\
	    : signal_missing_arg(1));

_PR VALUE cmd_plus(VALUE);
DEFUN("+", cmd_plus, subr_plus, (VALUE args), V_SubrN, DOC_plus) /*
::doc:plus::
+ NUMBERS...

Adds all NUMBERS together. If no arguments are given returns 0.
::end:: */
{
    if(NILP(args))
	return MAKE_INT(0);
    APPLY_OP( + )
}

_PR VALUE cmd_minus(VALUE);
DEFUN("-", cmd_minus, subr_minus, (VALUE args), V_SubrN, DOC_minus) /*
::doc:minus::
- NUMBER [NUMBERS...]

Either returns the negation of NUMBER or the value of NUMBER minus
NUMBERS
::end:: */
{
    if(CONSP(args))
    {
	if(!CONSP(VCDR(args)))
	    return(MAKE_INT(-VINT(VCAR(args))));
	else
	    APPLY_OP( - )
    }
    return signal_missing_arg(1);
}

_PR VALUE cmd_product(VALUE);
DEFUN("*", cmd_product, subr_product, (VALUE args), V_SubrN, DOC_product) /*
::doc:product::
* NUMBERS...

Multiplies all NUMBERS together. If no numbers are given returns 1.
::end:: */
{
    if(NILP(args))
	return MAKE_INT(1);
    APPLY_OP( * )
}

_PR VALUE cmd_divide(VALUE);
DEFUN("/", cmd_divide, subr_divide, (VALUE args), V_SubrN, DOC_divide) /*
::doc:divide::
/ NUMBERS...

Divides NUMBERS (in left-to-right order), ie,
  (/ 100 2
   => 10
::end:: */
{
    if(CONSP(args) && INTP(VCAR(args)))
    {
	long sum = VINT(VCAR(args));
	int i = 1;
	args = VCDR(args);
	while(CONSP(args))
	{
	    if(!INTP(VCAR(args)))
	       return signal_arg_error(VCAR(args), i);
	    if(VINT(VCAR(args)) == 0)
		return cmd_signal(sym_arith_error, LIST_1(VAL(&div_zero)));
	    sum = sum / VINT(VCAR(args));
	    args = VCDR(args);
	    i++;
	    TEST_INT;
	    if(INT_P)
		return(LISP_NULL);
	}
	return(MAKE_INT(sum));
    }
    return (CONSP(args)
	    ? signal_arg_error(VCAR(args), 1) : signal_missing_arg(1));
}

_PR VALUE cmd_remainder(VALUE n1, VALUE n2);
DEFUN("%", cmd_remainder, subr_remainder, (VALUE n1, VALUE n2), V_Subr2, DOC_remainder) /*
::doc:remainder::
% DIVIDEND DIVISOR

Returns the integer remainder after dividing DIVIDEND by DIVISOR.
::end:: */
{
    DECLARE1(n1, INTP);
    DECLARE2(n2, INTP);
    if(VINT(n2) == 0)
	return cmd_signal(sym_arith_error, LIST_1(VAL(&div_zero)));
    return MAKE_INT(VINT(n1) % VINT(n2));
}

_PR VALUE cmd_mod(VALUE n1, VALUE n2);
DEFUN("mod", cmd_mod, subr_mod, (VALUE n1, VALUE n2), V_Subr2, DOC_mod) /*
::doc:mod::
mod DIVIDEND DIVISOR

Returns the value of DIVIDEND modulo DIVISOR; unlike the % (remainder)
function the behaviour of `mod' is well-defined for negative arguments,
we have that,

	(mod X Y) == X - (* Y (floor (/ X Y))),	for Y not equal to zero

assuming that (floor Z) gives the least integer greater than or equal to Z,
and that floating point division is used.
::end:: */
{
    long tem;
    DECLARE1(n1, INTP);
    DECLARE2(n2, INTP);
    if(VINT(n2) == 0)
	return cmd_signal(sym_arith_error, LIST_1(VAL(&div_zero)));

    /* This code from GNU Emacs */
    tem = VINT(n1) % VINT(n2);
    /* If the "remainder" comes out with the wrong sign, fix it.  */
    if (VINT(n2) < 0 ? tem > 0 : tem < 0)
	tem += VINT(n2);

    return MAKE_INT(tem);
}

_PR VALUE cmd_lognot(VALUE);
DEFUN("lognot", cmd_lognot, subr_lognot, (VALUE num), V_Subr1, DOC_lognot) /*
::doc:lognot::
lognot NUMBER

Returns the bitwise logical `not' of NUMBER.
::end:: */
{
    DECLARE1(num, INTP);
    return(MAKE_INT(~VINT(num)));
}

_PR VALUE cmd_not(VALUE);
DEFUN("not", cmd_not, subr_not, (VALUE arg), V_Subr1, DOC_not) /*
::doc:not::
not ARG

If ARG is nil returns t, else returns nil.
::end:: */
{
    if(NILP(arg))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_logior(VALUE);
DEFUN("logior", cmd_logior, subr_logior, (VALUE args), V_SubrN, DOC_logior) /*
::doc:logior::
logior NUMBERS...

Returns the bitwise logical `inclusive-or' of its arguments.
::end:: */
{
    APPLY_OP( | )
}

_PR VALUE cmd_logxor(VALUE);
DEFUN("logxor", cmd_logxor, subr_logxor, (VALUE args), V_SubrN, DOC_logxor) /*
::doc:logxor::
logxor NUMBERS...

Returns the bitwise logical `exclusive-or' of its arguments.
::end:: */
{
    APPLY_OP( ^ )
}

_PR VALUE cmd_logand(VALUE);
DEFUN("logand", cmd_logand, subr_logand, (VALUE args), V_SubrN, DOC_logand) /*
::doc:logand::
logand NUMBERS...

Returns the bitwise logical `and' of its arguments.
::end:: */
{
    APPLY_OP( & )
}

_PR VALUE cmd_equal(VALUE, VALUE);
DEFUN("equal", cmd_equal, subr_equal, (VALUE val1, VALUE val2), V_Subr2, DOC_equal) /*
::doc:equal::
equal VALUE1 VALUE2

Compares VALUE1 and VALUE2, compares the actual structure of the objects not
just whether the objects are one and the same. ie, will return t for two
strings built from the same characters in the same order even if the strings'
location in memory is different.
::end:: */
{
    return (value_cmp(val1, val2) == 0) ? sym_t : sym_nil;
}

_PR VALUE cmd_eq(VALUE, VALUE);
DEFUN("eq", cmd_eq, subr_eq, (VALUE val1, VALUE val2), V_Subr2, DOC_eq) /*
::doc:eq::
eq VALUE1 VALUE2

Returns t if VALUE1 and VALUE2 are one and the same object. Note that
this may or may not be true for numbers of the same value (see `eql').
::end:: */
{
    return (val1 == val2) ? sym_t : sym_nil;
}

_PR VALUE cmd_eql(VALUE arg1, VALUE arg2);
DEFUN("eql", cmd_eql, subr_eql, (VALUE arg1, VALUE arg2), V_Subr2, DOC_eql) /*
::doc:eql::
eql ARG1 ARG2
Similar to `eq' except that numbers (integers, characters) with the same
value will always be considered `eql' (this may or may not be the case
with `eq'.
::end:: */
{
    if(INTP(arg1) && INTP(arg2))
	return(VINT(arg1) == VINT(arg2) ? sym_t : sym_nil);
    else
	return(arg1 == arg2 ? sym_t : sym_nil);
}

_PR VALUE cmd_string_head_eq(VALUE, VALUE);
DEFUN("string-head-eq", cmd_string_head_eq, subr_string_head_eq, (VALUE str1, VALUE str2), V_Subr2, DOC_string_head_eq) /*
::doc:string_head_eq::
string-head-eq STRING1 STRING2

Returns t if STRING2 matches the beginning of STRING1, ie,
  (string-head-eq "foobar" "foo")
   => t
  (string-head-eq "foo" "foobar")
   => nil
::end:: */
{
    u_char *s1, *s2;
    DECLARE1(str1, STRINGP);
    DECLARE2(str2, STRINGP);
    s1 = VSTR(str1);
    s2 = VSTR(str2);
    while(*s1 && *s2)
    {
	if(*s1++ != *s2++)
	    return(sym_nil);
    }
    if(*s1 || (*s1 == *s2))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_num_eq(VALUE num1, VALUE num2);
DEFUN("=", cmd_num_eq, subr_num_eq, (VALUE num1, VALUE num2), V_Subr2, DOC_num_eq) /*
::doc:num_eq::
= NUMBER1 NUMBER2

Returns t if NUMBER1 and NUMBER2 are equal.
::end:: */
{
    DECLARE1(num1, INTP);
    DECLARE2(num2, INTP);
    if(VINT(num1) == VINT(num2))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_num_noteq(VALUE num1, VALUE num2);
DEFUN("/=", cmd_num_noteq, subr_num_noteq, (VALUE num1, VALUE num2), V_Subr2, DOC_num_noteq) /*
::doc:num_noteq::
/= NUMBER1 NUMBER2

Returns t if NUMBER1 and NUMBER2 are unequal.
::end:: */
{
    DECLARE1(num1, INTP);
    DECLARE2(num2, INTP);
    if(VINT(num1) != VINT(num2))
	return(sym_t);
    return(sym_nil);
}

#define APPLY_COMPARISON(op)				\
    if(CONSP(args) && CONSP(VCDR(args)))		\
    {							\
	VALUE pred = VCAR(args);			\
	int i = 2;					\
	args = VCDR(args);				\
	while(CONSP(args))				\
	{						\
	    if(!(value_cmp(pred, VCAR(args)) op 0))	\
		return sym_nil;				\
	    pred = VCAR(args);				\
	    args = VCDR(args);				\
	    i++;					\
	    TEST_INT;					\
	    if(INT_P)					\
		return(LISP_NULL);			\
	}						\
	return sym_t;					\
    }							\
    return signal_missing_arg(CONSP(args) ? 2 : 1);					\

_PR VALUE cmd_gtthan(VALUE);
DEFUN(">", cmd_gtthan, subr_gtthan, (VALUE args), V_SubrN, DOC_gtthan) /*
::doc:gtthan::
> ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is greater than ARG2, and if ARG2 is greater than ARG3,
and so on. Note that this command isn't limited to numbers, it can do
strings, positions, marks, etc as well.
::end:: */
{
    APPLY_COMPARISON(>)
}

_PR VALUE cmd_gethan(VALUE);
DEFUN(">=", cmd_gethan, subr_gethan, (VALUE args), V_SubrN, DOC_gethan) /*
::doc:gethan::
>= ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is greater-or-equal than ARG2. Note that this command
isn't limited to numbers, it can do strings, positions, marks, etc as well.
::end:: */
{
    APPLY_COMPARISON(>=)
}

_PR VALUE cmd_ltthan(VALUE);
DEFUN("<", cmd_ltthan, subr_ltthan, (VALUE args), V_SubrN, DOC_ltthan) /*
::doc:ltthan::
< ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is less than ARG2. Note that this command isn't limited to
numbers, it can do strings, positions, marks, etc as well.
::end:: */
{
    APPLY_COMPARISON(<)
}

_PR VALUE cmd_lethan(VALUE);
DEFUN("<=", cmd_lethan, subr_lethan, (VALUE args), V_SubrN, DOC_lethan) /*
::doc:lethan::
<= ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is less-or-equal than ARG2. Note that this command isn't
limited to numbers, it can do strings, positions, marks, etc as well.
::end:: */
{
    APPLY_COMPARISON(<=)
}

_PR VALUE cmd_max(VALUE);
DEFUN("max", cmd_max, subr_max, (VALUE args), V_SubrN, DOC_max) /*
::doc:max::
max ARGS...

Returns the greatest of its arguments. There must be at least two
arguments.
::end:: */
{
    VALUE max;
    if(!CONSP(args) || !CONSP(VCDR(args)))
	return signal_missing_arg(CONSP(args) ? 2 : 1);
    max = VCAR(args);
    args = VCDR(args);
    while(CONSP(args))
    {
	VALUE tem = VCAR(args);
	args = VCDR(args);
	max = (value_cmp(max, tem) >= 0) ? max : tem;
	TEST_INT;
	if(INT_P)
	    return LISP_NULL;
    }
    return max;
}

_PR VALUE cmd_min(VALUE);
DEFUN("min", cmd_min, subr_min, (VALUE args), V_SubrN, DOC_min) /*
::doc:min::
min ARGS...

Returns the smallest of its arguments. There must be at least two
arguments.
::end:: */
{
    VALUE min;
    if(!CONSP(args) || !CONSP(VCDR(args)))
	return signal_missing_arg(CONSP(args) ? 2 : 1);
    min = VCAR(args);
    args = VCDR(args);
    while(CONSP(args))
    {
	VALUE tem = VCAR(args);
	args = VCDR(args);
	min = (value_cmp(min, tem) <= 0) ? min : tem;
	TEST_INT;
	if(INT_P)
	    return LISP_NULL;
    }
    return min;
}

_PR VALUE cmd_plus1(VALUE);
DEFUN("1+", cmd_plus1, subr_plus1, (VALUE num), V_Subr1, DOC_plus1) /*
::doc:plus1::
1+ NUMBER

Return NUMBER plus 1.
::end:: */
{
    DECLARE1(num, INTP);
    return(MAKE_INT(VINT(num) + 1));
}

_PR VALUE cmd_sub1(VALUE);
DEFUN("1-", cmd_sub1, subr_sub1, (VALUE num), V_Subr1, DOC_sub1) /*
::doc:sub1::
1- NUMBER

Return NUMBER minus 1.
::end:: */
{
    DECLARE1(num, INTP);
    return(MAKE_INT(VINT(num) - 1));
}

_PR VALUE cmd_lsh(VALUE, VALUE);
DEFUN("lsh", cmd_lsh, subr_lsh, (VALUE num, VALUE shift), V_Subr2, DOC_lsh) /*
::doc:lsh::
lsh NUMBER COUNT

Shift the bits in NUMBER by COUNT bits to the left, a negative COUNT means
shift right.
::end:: */
{
    DECLARE1(num, INTP);
    DECLARE2(shift, INTP);
    if(VINT(shift) > 0)
	return(MAKE_INT(VINT(num) << VINT(shift)));
    /* ensure that a zero is in the top bit. */
    return(MAKE_INT((VINT(num) >> -VINT(shift)) & 0x7fffffff));
}

_PR VALUE cmd_ash(VALUE, VALUE);
DEFUN("ash", cmd_ash, subr_ash, (VALUE num, VALUE shift), V_Subr2, DOC_ash) /*
::doc:ash::
ash NUMBER COUNT

Use an arithmetic shift to shift the bits in NUMBER by COUNT bits to the left,
a negative COUNT means shift right.
::end:: */
{
    DECLARE1(num, INTP);
    DECLARE2(shift, INTP);
    if(VINT(shift) > 0)
	return(MAKE_INT(VINT(num) << VINT(shift)));
    return(MAKE_INT(VINT(num) >> -VINT(shift)));
}

_PR VALUE cmd_zerop(VALUE);
DEFUN("zerop", cmd_zerop, subr_zerop, (VALUE num), V_Subr1, DOC_zerop) /*
::doc:zerop::
zerop NUMBER

t if NUMBER is zero.
::end:: */
{
    if(INTP(num) && (VINT(num) == 0))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_null(VALUE);
DEFUN("null", cmd_null, subr_null, (VALUE arg), V_Subr1, DOC_null) /*
::doc:null::
null ARG

Returns t if ARG is nil.
::end:: */
{
    return NILP(arg) ? sym_t : sym_nil;
}

_PR VALUE cmd_atom(VALUE);
DEFUN("atom", cmd_atom, subr_atom, (VALUE arg), V_Subr1, DOC_atom) /*
::doc:atom::
atom ARG

Returns t if ARG is not a cons-cell.
::end:: */
{
    return CONSP(arg) ? sym_t : sym_nil;
}

_PR VALUE cmd_consp(VALUE);
DEFUN("consp", cmd_consp, subr_consp, (VALUE arg), V_Subr1, DOC_consp) /*
::doc:consp::
consp ARG

Returns t if ARG is a cons-cell.
::end:: */
{
    return CONSP(arg) ? sym_t : sym_nil;
}

_PR VALUE cmd_listp(VALUE);
DEFUN("listp", cmd_listp, subr_listp, (VALUE arg), V_Subr1, DOC_listp) /*
::doc:listp::
listp ARG

Returns t if ARG is a list, (either a cons-cell or nil).
::end:: */
{
    return LISTP(arg) ? sym_t : sym_nil;
}

_PR VALUE cmd_numberp(VALUE);
DEFUN("numberp", cmd_numberp, subr_numberp, (VALUE arg), V_Subr1, DOC_numberp) /*
::doc:numberp::
numberp ARG

Return t if ARG is a number.
::end:: */
{
    return INTP(arg) ? sym_t : sym_nil;
}

_PR VALUE cmd_integerp(VALUE);
DEFUN("integerp", cmd_integerp, subr_integerp, (VALUE arg), V_Subr1, DOC_integerp) /*
::doc:integerp::
integerp ARG

Return t if ARG is a integer.
::end:: */
{
    return INTP(arg) ? sym_t : sym_nil;
}

_PR VALUE cmd_stringp(VALUE);
DEFUN("stringp", cmd_stringp, subr_stringp, (VALUE arg), V_Subr1, DOC_stringp) /*
::doc:stringp::
stringp ARG

Returns t is ARG is a string.
::end:: */
{
    return STRINGP(arg) ? sym_t : sym_nil;
}

_PR VALUE cmd_vectorp(VALUE);
DEFUN("vectorp", cmd_vectorp, subr_vectorp, (VALUE arg), V_Subr1, DOC_vectorp) /*
::doc:vectorp::
vectorp ARG

Returns t if ARG is a vector.
::end:: */
{
    return VECTORP(arg) ? sym_t : sym_nil;
}

_PR VALUE cmd_functionp(VALUE);
DEFUN("functionp", cmd_functionp, subr_functionp, (VALUE arg), V_Subr1, DOC_functionp) /*
::doc:functionp::
functionp ARG

Returns t if ARG is a function (ie, a symbol or a list whose car is the
symbol `lambda'
::end:: */
{
    if(SYMBOLP(arg))
    {
	if(!(arg = VSYM(arg)->function))
	    return(sym_nil);
    }
    switch(VTYPE(arg))
    {
    case V_Subr0:
    case V_Subr1:
    case V_Subr2:
    case V_Subr3:
    case V_Subr4:
    case V_Subr5:
    case V_SubrN:
    case V_Compiled:
	return(sym_t);
    case V_Cons:
	arg = VCAR(arg);
	if((arg == sym_lambda) || (arg == sym_autoload))
	    return(sym_t);
	/* FALL THROUGH */
    default:
	return(sym_nil);
    }
}

_PR VALUE cmd_special_form_p(VALUE);
DEFUN("special-form-p", cmd_special_form_p, subr_special_form_p, (VALUE arg), V_Subr1, DOC_special_form_p) /*
::doc:special_form_p::
special-form-p ARG

Returns t if ARG is a special-form.
::end:: */
{
    if(SYMBOLP(arg))
    {
	if(!(arg = VSYM(arg)->function))
	     return(sym_nil);
    }
    if(VTYPEP(arg, V_SF))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_subrp(VALUE arg);
DEFUN("subrp", cmd_subrp, subr_subrp, (VALUE arg), V_Subr1, DOC_subrp) /*
::doc:subrp::
subrp ARG

Returns t if arg is a primitive function.
::end:: */
{
    switch(VTYPE(arg))
    {
    case V_Subr0:
    case V_Subr1:
    case V_Subr2:
    case V_Subr3:
    case V_Subr4:
    case V_Subr5:
    case V_SubrN:
    case V_SF:
    case V_Var:
    case V_Compiled:
	return(sym_t);
    default:
	return(sym_nil);
    }
}

_PR VALUE cmd_sequencep(VALUE arg);
DEFUN("sequencep", cmd_sequencep, subr_sequencep, (VALUE arg), V_Subr1, DOC_sequencep) /*
::doc:sequencep::
sequencep ARG

Returns t is ARG is a sequence (a list, vector or string).
::end:: */
{
    if(LISTP(arg) || VECTORP(arg) || STRINGP(arg) || COMPILEDP(arg))
	return sym_t;
    else
	return sym_nil;
}

_PR VALUE cmd_subr_documentation(VALUE subr, VALUE useVar);
DEFUN("subr-documentation", cmd_subr_documentation, subr_subr_documentation, (VALUE subr, VALUE useVar), V_Subr2, DOC_subr_documentation) /*
::doc:subr_documentation::
subr-documentation SUBR [USE-VAR]

Returns the doc-string associated with SUBR.
::end:: */
{
    if(SYMBOLP(subr))
    {
	if(NILP(useVar))
	{
	    if(VSYM(subr)->function)
		subr = VSYM(subr)->function;
	}
	else
	{
	    if(VSYM(subr)->value)
		subr = VSYM(subr)->value;
	}
    }
    switch(VTYPE(subr))
    {
    case V_Subr0:
    case V_Subr1:
    case V_Subr2:
    case V_Subr3:
    case V_Subr4:
    case V_Subr5:
    case V_SubrN:
    case V_SF:
    case V_Var:
	return(cmd_read_file_from_to(VAL(&doc_file), VSUBR(subr)->doc_index,
				     MAKE_INT((int)'\f')));
    case V_Compiled:
	return VVECTI(subr, COMPILED_DOC);

    default:
	return(sym_nil);
    }
}

_PR VALUE cmd_subr_name(VALUE subr, VALUE useVar);
DEFUN("subr-name", cmd_subr_name, subr_subr_name, (VALUE subr, VALUE useVar), V_Subr2, DOC_subr_name) /*
::doc:subr_name::
subr-name SUBR [USE-VAR]

Returns the name (a string) associated with SUBR.
::end:: */
{
    if(SYMBOLP(subr))
    {
	if(NILP(useVar))
	{
	    if(VSYM(subr)->function)
		subr = VSYM(subr)->function;
	}
	else
	{
	    if(VSYM(subr)->value)
		subr = VSYM(subr)->value;
	}
    }
    switch(VTYPE(subr))
    {
    case V_Subr0:
    case V_Subr1:
    case V_Subr2:
    case V_Subr3:
    case V_Subr4:
    case V_Subr5:
    case V_SubrN:
    case V_SF:
    case V_Var:
	return(VSUBR(subr)->name);
    default:
	return(sym_nil);
    }
}

_PR VALUE cmd_call_hook(VALUE hook, VALUE arg_list, VALUE type);
DEFUN("call-hook", cmd_call_hook, subr_call_hook, (VALUE hook, VALUE arg_list, VALUE type), V_Subr3, DOC_call_hook) /*
::doc:call_hook::
call-hook HOOK-SYMBOL ARG-LIST [TYPE]

Call the hook named HOOK-SYMBOL, passing all functions the arguments in the
list ARG-LIST.

TYPE defines how the return values of each function in the hook are
treated. If TYPE is nil they are ignored, if TYPE is the symbol `and'
the hook aborts after a function returns nil, if TYPE is `or' the hook
aborts when a function returns non-nil.

In all cases the value returned by the last-evaluated function is
returned.
::end:: */
{
    GC_root gc_hook, gc_arg_list, gc_type;
    VALUE res = sym_nil;
    DECLARE1(hook, SYMBOLP);
    DECLARE2(arg_list, LISTP);
    hook = cmd_symbol_value(hook, sym_t);
    if(VOIDP(hook) || NILP(hook))
	return sym_nil;
    PUSHGC(gc_hook, hook);
    PUSHGC(gc_arg_list, arg_list);
    PUSHGC(gc_type, type);
    while(CONSP(hook))
    {
	res = cmd_funcall(cmd_cons(VCAR(hook), arg_list));
	hook = VCDR(hook);
	TEST_INT;
	if(INT_P)
	    res = LISP_NULL;
	if(res == LISP_NULL
	   || (type == sym_and && NILP(res))
	   || (type == sym_or && !NILP(res)))
	    break;
    }
    POPGC; POPGC; POPGC;
    return res;
}

_PR VALUE cmd_catch(VALUE);
DEFUN("catch", cmd_catch, subr_catch, (VALUE args), V_SF, DOC_catch) /*
::doc:catch::
catch TAG FORMS...

Evaluates FORMS, non-local exits are allowed with `(throw TAG)'.
The value of `catch' is either the value of the last FORM or the
value given to the throw command.

There are several pre-defined `catch'es which are,
  'defun
     Around all defuns, the `return' command uses this, it basically does
     (throw 'defun X).
  'exit
     Exits one level of recursive-editing (but doesn't work in the top
     level.
  'top-level
     At the top-level recursive-edit (ie, the one which you're in when
     the editor is started).
  'quit
     Kills the editor.
::end:: */
    /* Non-local exits don't bother with jmp_buf's and the like, they just
       unwind normally through all levels of recursion with a LISP_NULL result.
       This is slow but it's easy to work with.  */
{
    if(CONSP(args))
    {
	VALUE tag, res = LISP_NULL;
	GC_root gc_args, gc_tag;
	PUSHGC(gc_args, args);
	tag = cmd_eval(VCAR(args));
	if(tag)
	{
	    PUSHGC(gc_tag, tag);
	    if(!(res = cmd_progn(VCDR(args))))
	    {
		if(throw_value && (VCAR(throw_value) == tag))
		{
		    res = VCDR(throw_value);
		    throw_value = LISP_NULL;
		}
	    }
	    POPGC;
	}
	POPGC;
	return(res);
    }
    return signal_missing_arg(1);
}

_PR VALUE cmd_throw(VALUE, VALUE);
DEFUN("throw", cmd_throw, subr_throw, (VALUE tag, VALUE val), V_Subr2, DOC_throw) /*
::doc:throw::
throw TAG VALUE

Performs a non-local exit to the `catch' waiting for TAG and return
VALUE from it. TAG and VALUE are both evaluated fully.
::end:: */
{
    /* Only one thing can use `throw_value' at once.  */
    if(!throw_value)
	throw_value = cmd_cons(tag, val);
    return(LISP_NULL);
}

_PR VALUE cmd_unwind_protect(VALUE);
DEFUN("unwind-protect", cmd_unwind_protect, subr_unwind_protect, (VALUE args), V_SF, DOC_unwind_protect) /*
::doc:unwind_protect::
unwind-protect BODY CLEANUP-FORMS...

Eval and return the value of BODY guaranteeing that the CLEANUP-FORMS will
be evalled no matter what happens (ie, error, non-local exit, etc) while
BODY is being evaluated.
::end:: */
{
    if(CONSP(args))
    {
	VALUE res, throwval;
	GC_root gc_args, gc_res, gc_throwval;
	PUSHGC(gc_args, args);
	res = cmd_eval(VCAR(args));
	PUSHGC(gc_res, res);
	throwval = throw_value;
	throw_value = LISP_NULL;
	PUSHGC(gc_throwval, throwval);
	if(!cmd_progn(VCDR(args)))
	    res = LISP_NULL;
	/* Only reinstall the old throw if it's actually an error and there
	   was no error in the cleanup forms. This way the newest error
	   takes precedence. */
	if(throwval != 0 && throw_value == 0)
	    throw_value = throwval;
	POPGC; POPGC; POPGC;
	return(res);
    }
    return signal_missing_arg(1);
}

void
lispcmds_init(void)
{
    ADD_SUBR(subr_quote);
    ADD_SUBR(subr_defmacro);
    ADD_SUBR(subr_defun);
    ADD_SUBR(subr_defvar);
    ADD_SUBR(subr_defconst);
    ADD_SUBR(subr_car);
    ADD_SUBR(subr_cdr);
    ADD_SUBR(subr_list);
    ADD_SUBR(subr_make_list);
    ADD_SUBR(subr_append);
    ADD_SUBR(subr_nconc);
    ADD_SUBR(subr_rplaca);
    ADD_SUBR(subr_rplacd);
    ADD_SUBR(subr_reverse);
    ADD_SUBR(subr_nreverse);
    ADD_SUBR(subr_assoc);
    ADD_SUBR(subr_assq);
    ADD_SUBR(subr_rassoc);
    ADD_SUBR(subr_rassq);
    ADD_SUBR(subr_nth);
    ADD_SUBR(subr_nthcdr);
    ADD_SUBR(subr_last);
    ADD_SUBR(subr_mapcar);
    ADD_SUBR(subr_mapc);
    ADD_SUBR(subr_member);
    ADD_SUBR(subr_memq);
    ADD_SUBR(subr_delete);
    ADD_SUBR(subr_delq);
    ADD_SUBR(subr_delete_if);
    ADD_SUBR(subr_delete_if_not);
    ADD_SUBR(subr_vector);
    ADD_SUBR(subr_make_vector);
    ADD_SUBR(subr_arrayp);
    ADD_SUBR(subr_aset);
    ADD_SUBR(subr_aref);
    ADD_SUBR(subr_make_string);
    ADD_SUBR(subr_concat);
    ADD_SUBR(subr_length);
    ADD_SUBR(subr_copy_sequence);
    ADD_SUBR(subr_elt);
    ADD_SUBR(subr_prog1);
    ADD_SUBR(subr_prog2);
    ADD_SUBR(subr_while);
    ADD_SUBR(subr_cond);
    ADD_SUBR(subr_apply);
    ADD_SUBR_INT(subr_load);
    ADD_SUBR(subr_plus);
    ADD_SUBR(subr_minus);
    ADD_SUBR(subr_product);
    ADD_SUBR(subr_divide);
    ADD_SUBR(subr_remainder);
    ADD_SUBR(subr_mod);
    ADD_SUBR(subr_lognot);
    ADD_SUBR(subr_not);
    ADD_SUBR(subr_logior);
    ADD_SUBR(subr_logxor);
    ADD_SUBR(subr_logand);
    ADD_SUBR(subr_equal);
    ADD_SUBR(subr_eq);
    ADD_SUBR(subr_eql);
    ADD_SUBR(subr_string_head_eq);
    ADD_SUBR(subr_num_eq);
    ADD_SUBR(subr_num_noteq);
    ADD_SUBR(subr_gtthan);
    ADD_SUBR(subr_gethan);
    ADD_SUBR(subr_ltthan);
    ADD_SUBR(subr_lethan);
    ADD_SUBR(subr_max);
    ADD_SUBR(subr_min);
    ADD_SUBR(subr_plus1);
    ADD_SUBR(subr_sub1);
    ADD_SUBR(subr_lsh);
    ADD_SUBR(subr_ash);
    ADD_SUBR(subr_zerop);
    ADD_SUBR(subr_null);
    ADD_SUBR(subr_atom);
    ADD_SUBR(subr_consp);
    ADD_SUBR(subr_listp);
    ADD_SUBR(subr_numberp);
    ADD_SUBR(subr_integerp);
    ADD_SUBR(subr_stringp);
    ADD_SUBR(subr_vectorp);
    ADD_SUBR(subr_functionp);
    ADD_SUBR(subr_special_form_p);
    ADD_SUBR(subr_subrp);
    ADD_SUBR(subr_subr_documentation);
    ADD_SUBR(subr_sequencep);
    ADD_SUBR(subr_subr_name);
    ADD_SUBR(subr_call_hook);
    ADD_SUBR(subr_catch);
    ADD_SUBR(subr_throw);
    ADD_SUBR(subr_unwind_protect);
    INTERN(load_path); DOC(load_path);
    VSYM(sym_load_path)->value = list_3(null_string(),
					VAL(&site_lisp_dir),
					VAL(&lisp_lib_dir));
    INTERN(after_load_alist); DOC(after_load_alist);
    VSYM(sym_after_load_alist)->value = sym_nil;
    INTERN(lisp_lib_dir); DOC(lisp_lib_dir);
    VSYM(sym_lisp_lib_dir)->value = VAL(&lisp_lib_dir);

    INTERN(or); INTERN(and);
}
