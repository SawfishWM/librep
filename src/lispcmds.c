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
VALUE sym_load_path, sym_lisp_lib_dir;
/*
::doc:load_path::
A list of directory names. When `load' opens a lisp-file it searches each
directory named in this list in turn until the file is found or the list
is exhausted.
::end::
::doc:lisp_lib_dir::
The name of the directory in which the standard lisp files live.
::end::
*/

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

_PR VALUE cmd_function(VALUE);
DEFUN("function", cmd_function, subr_function, (VALUE args), V_SF, DOC_function) /*
::doc:function::
function ARG
#'ARG

Normally the same as `quote'. When being compiled, if ARG is not a symbol
it causes ARG to be compiled as a lambda expression.
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
	GCVAL gcv_args;
	VALUE sym = VCAR(args), val;
	VALUE tmp = cmd_default_boundp(sym);
	if(!tmp)
	    return(NULL);
	PUSHGC(gcv_args, args);
	val = cmd_eval(VCAR(VCDR(args)));
	POPGC;
	if(!val)
	    return(NULL);
	if(NILP(tmp))
	{
	    if(!cmd_set_default(sym, val))
		return(NULL);
	}
	if(CONSP(VCDR(VCDR(args))))
	{
	    if(!cmd_put(sym, sym_variable_documentation, VCAR(VCDR(VCDR(args)))))
		return(NULL);
	}
	return(sym);
    }
    else
	return signal_missing_arg(CONSP(args) ? 2 : 1);
}

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
	    return(cmd_signal(sym_error, list_2(MKSTR("Constant already bound"),
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
	    return(NULL);
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
    VALUE res = sym_nil;
    VALUE *last;
    DECLARE1(len, NUMBERP);
    last = &res;
    for(i = 0; i < VNUM(len); i++)
    {
	if(!(*last = cmd_cons(init, sym_nil)))
	    return(NULL);
	last = &VCDR(*last);
    }
    return(res);
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
		return(NULL);
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
		    return(NULL);
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
	if(res == NULL)
	    return(NULL);
	head = VCDR(head);
	TEST_INT;
	if(INT_P)
	    return(NULL);
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
	    nxt = NULL;
	VCDR(head) = res;
	res = head;
	TEST_INT;
	if(INT_P)
	    return(NULL);
    } while((head = nxt));
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
	    return(NULL);
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
	    return(NULL);
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
	    return(NULL);
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
	    return(NULL);
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
    DECLARE1(index, NUMBERP);
    DECLARE2(list, LISTP);
    i = VNUM(index);
    while(i && CONSP(list))
    {
	list = VCDR(list);
	i--;
    }
    if((!i) && CONSP(list))
	return(VCAR(list));
    return(sym_nil);
}

_PR VALUE cmd_nthcdr(VALUE index, VALUE list);
DEFUN("nthcdr", cmd_nthcdr, subr_nthcdr, (VALUE index, VALUE list), V_Subr2, DOC_nthcdr) /*
::doc:nthcdr::
nthcdr INDEX LIST

Returns the INDEXth cdr of LIST. The first is INDEX zero.
::end:: */
{
    int i;
    DECLARE1(index, NUMBERP);
    DECLARE2(list, LISTP);
    i = VNUM(index);
    while(i && CONSP(list))
    {
	list = VCDR(list);
	i--;
    }
    if(!i)
	return(list);
    return(sym_nil);
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
		return(NULL);
	}
	return(list);
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
    GCVAL gcv_list, gcv_argv, gcv_res;
    VALUE argv;
    DECLARE2(list, LISTP);
    argv = cmd_cons(fun, cmd_cons(sym_nil, sym_nil));
    if(argv)
    {
	PUSHGC(gcv_res, res);
	PUSHGC(gcv_argv, argv);
	PUSHGC(gcv_list, list);
	while(res && CONSP(list))
	{
	    if(!(*last = cmd_cons(sym_nil, sym_nil)))
		return(NULL);
	    VCAR(VCDR(argv)) = VCAR(list);
	    if(!(VCAR(*last) = cmd_funcall(argv)))
		res = NULL;
	    else
	    {
		last = &VCDR(*last);
		list = VCDR(list);
	    }
	    TEST_INT;
	    if(INT_P)
	    {
		res = NULL;
		break;
	    }
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
    GCVAL gcv_argv, gcv_list;
    DECLARE2(list, LISTP);
    if(!(argv = cmd_cons(fun, cmd_cons(sym_nil, sym_nil))))
	return(NULL);
    PUSHGC(gcv_argv, argv);
    PUSHGC(gcv_list, list);
    while(res && CONSP(list))
    {
	VCAR(VCDR(argv)) = VCAR(list);
	if(!cmd_funcall(argv))
	    res = NULL;
	list = VCDR(list);
	TEST_INT;
	if(INT_P)
	    res = NULL;
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
	    return(NULL);
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
	    return(NULL);
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
	    return(NULL);
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
	    return(NULL);
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
	    return(NULL);
	if(!NILP(tmp))
	    *head = VCDR(*head);
	else
	    head = &VCDR(*head);
	TEST_INT;
	if(INT_P)
	    return(NULL);
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
	    return(NULL);
	if(NILP(tmp))
	    *head = VCDR(*head);
	else
	    head = &VCDR(*head);
	TEST_INT;
	if(INT_P)
	    return(NULL);
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
	    VVECT(res)->vc_Array[i] = VCAR(args);
	    args = VCDR(args);
	    i++;
	    TEST_INT;
	    if(INT_P)
		return(NULL);
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
    DECLARE1(size, NUMBERP);
    len = VNUM(size);
    res = make_vector(len);
    if(res)
    {
	int i;
	for(i = 0; i < len; i++)
	    VVECT(res)->vc_Array[i] = init;
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
    return((VECTORP(arg) || STRINGP(arg)) ? sym_t : sym_nil);
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
    DECLARE2(index, NUMBERP);
    switch(VTYPE(array))
    {
    case V_DynamicString:
	if(VNUM(index) < STRING_LEN(array))
	{
	    DECLARE3(new, NUMBERP);
	    VSTR(array)[VNUM(index)] = (u_char)VCHAR(new);
	    return(new);
	}
	break;
    case V_Vector:
	if(VNUM(index) < VVECT(array)->vc_Size)
	{
	    VVECT(array)->vc_Array[VNUM(index)] = new;
	    return(new);
	}
	break;
    default:
	return(signal_arg_error(array, 1));
    }
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
    DECLARE2(index, NUMBERP);
    switch(VTYPE(array))
    {
    case V_StaticString:
    case V_DynamicString:
	if(VNUM(index) < STRING_LEN(array))
	    return(make_number(VSTR(array)[VNUM(index)]));
	break;
    case V_Vector:
	if(VNUM(index) < VVECT(array)->vc_Size)
	    return(VVECT(array)->vc_Array[VNUM(index)]);
	break;
    default:
	return(cmd_signal(sym_bad_arg, list_2(array, make_number(1))));
    }
    return(sym_nil);
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
    DECLARE1(len, NUMBERP);
    res = make_string(VNUM(len) + 1);
    if(res)
    {
	memset(VSTR(res), NUMBERP(init) ? (u_char)VCHAR(init) : ' ', VNUM(len));
	VSTR(res)[VNUM(len)] = 0;
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
	VALUE res = NULL;
	int i = 0;
	int argnum = 1;
	while(CONSP(args))
	{
	    VALUE arg = VCAR(args);
	    switch(VTYPE(arg))
	    {
		int slen, j;
	    case V_StaticString:
	    case V_DynamicString:
		slen = STRING_LEN(arg);
		if(!extend_concat(&buf, &buflen, i, slen))
		    goto error;
		memcpy(buf + i, VSTR(arg), slen);
		i += slen;
		break;
	    case V_Char:
		if(!extend_concat(&buf, &buflen, i, 1))
		    goto error;
		buf[i++] = VCHAR(arg);
		break;
	    case V_Symbol:
		if(arg != sym_nil)
		    break;
		/* FALL THROUGH */
	    case V_Cons:
		while(CONSP(arg))
		{
		    VALUE ch = VCAR(arg);
		    if(VTYPEP(ch, V_Char))
		    {
			if(!extend_concat(&buf, &buflen, i, 1))
			    goto error;
			buf[i++] = VCHAR(ch);
		    }
		    arg = VCDR(arg);
		    TEST_INT;
		    if(INT_P)
			goto error;
		}
		break;
	    case V_Vector:
		for(j = 0; j < VVECT(arg)->vc_Size; j++)
		{
		    if(VTYPEP(VVECT(arg)->vc_Array[j], V_Char))
		    {
			if(!extend_concat(&buf, &buflen, i, 1))
			    goto error;
			buf[i++] = VCHAR(VVECT(arg)->vc_Array[j]);
		    }
		}
		break;
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
    return(NULL);
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
    case V_StaticString:
    case V_DynamicString:
	return(make_number(STRING_LEN(sequence)));
	break;
    case V_Vector:
	return(make_number(VVECT(sequence)->vc_Size));
	break;
    case V_Cons:
	i = 0;
	while(CONSP(sequence))
	{
	    sequence = VCDR(sequence);
	    i++;
	    TEST_INT;
	    if(INT_P)
		return(NULL);
	}
	return(make_number(i));
	break;
    case V_Symbol:
	if(sequence == sym_nil)
	    return(make_number(0));
	/* FALL THROUGH */
    default:
	cmd_signal(sym_bad_arg, list_2(sequence, make_number(1)));
	return(NULL);
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
		    return(NULL);
		if(!(*last = cmd_cons(VCAR(seq), sym_nil)))
		    return(NULL);
		last = &VCDR(*last);
		seq = VCDR(seq);
	    }
	}
	break;
    case V_Vector:
	res = make_vector(VVECT(seq)->vc_Size);
	if(res)
	{
	    int i;
	    for(i = 0; i < VVECT(seq)->vc_Size; i++)
		VVECTI(res, i) = VVECTI(seq, i);
	}
	break;
    case V_DynamicString:
    case V_StaticString:
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
	GCVAL gcv_args, gcv_res;
	PUSHGC(gcv_args, args);
	res = cmd_eval(VCAR(args));
	if(res)
	{
	    PUSHGC(gcv_res, res);
	    if(!cmd_progn(VCDR(args)))
		res = NULL;
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
	GCVAL gcv_args, gcv_res;
	PUSHGC(gcv_args, args);
	if(cmd_eval(VCAR(args)))
	{
	    res = cmd_eval(VCAR(VCDR(args)));
	    if(res)
	    {
		PUSHGC(gcv_res, res);
		if(!cmd_progn(VCDR(VCDR(args))))
		    res = NULL;
		POPGC;
	    }
	}
	else
	    res = NULL;
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
	GCVAL gcv_args;
	VALUE cond = VCAR(args), wval, body = VCDR(args);
	PUSHGC(gcv_args, args);
	while((wval = cmd_eval(cond)) && !NILP(wval))
	{
	    TEST_INT;
	    if(INT_P || !cmd_progn(body))
	    {
		wval = NULL;
		break;
	    }
	}
	POPGC;
	if(!wval)
	    return(NULL);
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
    GCVAL gcv_args;
    PUSHGC(gcv_args, args);
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
		return(NULL);
	    last = &VCDR(*last);
	    args = VCDR(args);
	    TEST_INT;
	    if(INT_P)
		return(NULL);
	}
	if(!NILP(cmd_listp(VCAR(args))))
	    *last = VCAR(args);
	else
	    return(cmd_signal(sym_bad_arg, LIST_1(VCAR(args))));
	return(cmd_funcall(list));
    }
    return signal_missing_arg(1);
}

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
    VALUE name = NULL, stream, path;
    DECLARE1(file, STRINGP);
    if(NILP(nopath_p))
    {
	path = cmd_symbol_value(sym_load_path, sym_nil);
	if(!path)
	    return(NULL);
    }
    else
	path = cmd_cons(MKSTR(""), sym_nil);
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
	    return(NULL);
    }
    if(!name)
    {
	if(NILP(noerr_p))
	    return(cmd_signal(sym_file_error,
			      list_2(MKSTR("Can't open lisp-file"), file)));
	else
	    return(sym_nil);
    }
    if((stream = cmd_open(name, MKSTR("r"), sym_nil)) && FILEP(stream))
    {
	VALUE obj;
	int c;
	GCVAL gcv_stream;
	PUSHGC(gcv_stream, stream);
	c = stream_getc(stream);
	while((c != EOF) && (obj = readl(stream, &c)))
	{
	    TEST_INT;
	    if(INT_P || !cmd_eval(obj))
	    {
		POPGC;
		return(NULL);
	    }
	}
	POPGC;
	return(sym_t);
    }
    return(NULL);
}

/*
 * some arithmetic commands
 */

#define APPLY_OP(op)					\
    if(CONSP(args) && NUMBERP(VCAR(args)))		\
    {							\
	long sum = VNUM(VCAR(args));			\
	int i = 2;					\
	args = VCDR(args);				\
	while(CONSP(args))				\
	{						\
	    if(!NUMBERP(VCAR(args)))			\
		return signal_arg_error(VCAR(args), i);	\
	    sum = sum op VNUM(VCAR(args));		\
	    args = VCDR(args);				\
	    i++;					\
	    TEST_INT;					\
	    if(INT_P)					\
		return(NULL);				\
	}						\
	return(make_number(sum));			\
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
	return make_number(0);
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
	    return(make_number(-VNUM(VCAR(args))));
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
	return make_number(1);
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
    if(CONSP(args) && NUMBERP(VCAR(args)))
    {
	long sum = VNUM(VCAR(args));
	int i = 1;
	args = VCDR(args);
	while(CONSP(args))
	{
	    if(!NUMBERP(VCAR(args)))
	       return signal_arg_error(VCAR(args), i);
	    if(VNUM(VCAR(args)) == 0)
		return cmd_signal(sym_arith_error,
				  LIST_1(MKSTR("Divide by zero")));
	    sum = sum / VNUM(VCAR(args));
	    args = VCDR(args);
	    i++;
	    TEST_INT;
	    if(INT_P)
		return(NULL);
	}
	return(make_number(sum));
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
    DECLARE1(n1, NUMBERP);
    DECLARE2(n2, NUMBERP);
    if(VNUM(n2) == 0)
	return cmd_signal(sym_arith_error, LIST_1(MKSTR("Divide by zero")));
    return make_number(VNUM(n1) % VNUM(n2));
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
    DECLARE1(n1, NUMBERP);
    DECLARE2(n2, NUMBERP);
    if(VNUM(n2) == 0)
	return cmd_signal(sym_arith_error, LIST_1(MKSTR("Divide by zero")));

    /* This code from GNU Emacs */
    tem = VNUM(n1) % VNUM(n2);
    /* If the "remainder" comes out with the wrong sign, fix it.  */
    if (VNUM(n2) < 0 ? tem > 0 : tem < 0)
	tem += VNUM(n2);

    return make_number(tem);
}

_PR VALUE cmd_lognot(VALUE);
DEFUN("lognot", cmd_lognot, subr_lognot, (VALUE num), V_Subr1, DOC_lognot) /*
::doc:lognot::
lognot NUMBER

Returns the bitwise logical `not' of NUMBER.
::end:: */
{
    DECLARE1(num, NUMBERP);
    return(make_number(~VNUM(num)));
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

_PR VALUE cmd_or(VALUE);
DEFUN("or", cmd_or, subr_or, (VALUE args), V_SF, DOC_or) /*
::doc:or::
or FORMS...

Evals each FORM while they return nil, returns the first non-nil result or
nil if all FORMS return nil.
::end:: */
{
    VALUE res = sym_nil;
    GCVAL gcv_args, gcv_res;
    PUSHGC(gcv_args, args);
    PUSHGC(gcv_res, res);
    while(res && CONSP(args) && NILP(res))
    {
	res = cmd_eval(VCAR(args));
	args = VCDR(args);
	TEST_INT;
	if(INT_P)
	    res = NULL;
    }
    POPGC;
    POPGC;
    return(res);
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

_PR VALUE cmd_and(VALUE);
DEFUN("and", cmd_and, subr_and, (VALUE args), V_SF, DOC_and) /*
::doc:and::
and FORMS...

Evals each FORM until one returns nil, it returns that value, or t if all
FORMS return t.
::end:: */
{
    VALUE res = sym_t;
    GCVAL gcv_args, gcv_res;
    PUSHGC(gcv_args, args);
    PUSHGC(gcv_res, res);
    while(res && CONSP(args) && !NILP(res))
    {
	res = cmd_eval(VCAR(args));
	args = VCDR(args);
	TEST_INT;
	if(INT_P)
	    res = NULL;
    }
    POPGC;
    POPGC;
    return(res);
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
    if(NUMBERP(arg1) && NUMBERP(arg2))
	return(VNUM(arg1) == VNUM(arg2) ? sym_t : sym_nil);
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
    DECLARE1(num1, NUMBERP);
    DECLARE2(num2, NUMBERP);
    if(VNUM(num1) == VNUM(num2))
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
    DECLARE1(num1, NUMBERP);
    DECLARE2(num2, NUMBERP);
    if(VNUM(num1) != VNUM(num2))
	return(sym_t);
    return(sym_nil);
}

#define APPLY_COMPARISON(op)				\
    if(CONSP(args) && NUMBERP(VCAR(args)))		\
    {							\
	VALUE pred = VCAR(args);			\
	int i = 2;					\
	args = VCDR(args);				\
	while(CONSP(args))				\
	{						\
	    if(!NUMBERP(VCAR(args)))			\
		return signal_arg_error(VCAR(args), i);	\
	    if(!(value_cmp(pred, VCAR(args)) op 0))	\
		return sym_nil;				\
	    pred = VCAR(args);				\
	    args = VCDR(args);				\
	    i++;					\
	    TEST_INT;					\
	    if(INT_P)					\
		return(NULL);				\
	}						\
	return sym_t;					\
    }							\
    return (CONSP(args)					\
	    ? signal_arg_error(VCAR(args), 1)		\
	    : signal_missing_arg(1));

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

_PR VALUE cmd_plus1(VALUE);
DEFUN("1+", cmd_plus1, subr_plus1, (VALUE num), V_Subr1, DOC_plus1) /*
::doc:plus1::
1+ NUMBER

Return NUMBER plus 1.
::end:: */
{
    DECLARE1(num, NUMBERP);
    return(make_number(VNUM(num) + 1));
}

_PR VALUE cmd_sub1(VALUE);
DEFUN("1-", cmd_sub1, subr_sub1, (VALUE num), V_Subr1, DOC_sub1) /*
::doc:sub1::
1- NUMBER

Return NUMBER minus 1.
::end:: */
{
    DECLARE1(num, NUMBERP);
    return(make_number(VNUM(num) - 1));
}

_PR VALUE cmd_lsh(VALUE, VALUE);
DEFUN("lsh", cmd_lsh, subr_lsh, (VALUE num, VALUE shift), V_Subr2, DOC_lsh) /*
::doc:lsh::
lsh NUMBER COUNT

Shift the bits in NUMBER by COUNT bits to the left, a negative COUNT means
shift right.
::end:: */
{
    DECLARE1(num, NUMBERP);
    DECLARE2(shift, NUMBERP);
    if(VNUM(shift) > 0)
	return(make_number(VNUM(num) << VNUM(shift)));
    /* ensure that a zero is in the top bit. */
    return(make_number((VNUM(num) >> -VNUM(shift)) & 0x7fffffff));
}

_PR VALUE cmd_ash(VALUE, VALUE);
DEFUN("ash", cmd_ash, subr_ash, (VALUE num, VALUE shift), V_Subr2, DOC_ash) /*
::doc:ash::
ash NUMBER COUNT

Use an arithmetic shift to shift the bits in NUMBER by COUNT bits to the left,
a negative COUNT means shift right.
::end:: */
{
    DECLARE1(num, NUMBERP);
    DECLARE2(shift, NUMBERP);
    if(VNUM(shift) > 0)
	return(make_number(VNUM(num) << VNUM(shift)));
    return(make_number(VNUM(num) >> -VNUM(shift)));
}

_PR VALUE cmd_zerop(VALUE);
DEFUN("zerop", cmd_zerop, subr_zerop, (VALUE num), V_Subr1, DOC_zerop) /*
::doc:zerop::
zerop NUMBER

t if NUMBER is zero.
::end:: */
{
    if(NUMBERP(num) && (VNUM(num) == 0))
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
    return NUMBERP(arg) ? sym_t : sym_nil;
}

_PR VALUE cmd_integerp(VALUE);
DEFUN("integerp", cmd_integerp, subr_integerp, (VALUE arg), V_Subr1, DOC_integerp) /*
::doc:integerp::
integerp ARG

Return t if ARG is a integer.
::end:: */
{
    return NUMBERP(arg) ? sym_t : sym_nil;
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
	if(!(arg = VSYM(arg)->sym_Function))
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
	if(!(arg = VSYM(arg)->sym_Function))
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
    return (LISTP(arg) || VECTORP(arg) || STRINGP(arg)) ? sym_t : sym_nil;
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
	    if(VSYM(subr)->sym_Function)
		subr = VSYM(subr)->sym_Function;
	}
	else
	{
	    if(VSYM(subr)->sym_Value)
		subr = VSYM(subr)->sym_Value;
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
	return(cmd_read_file_from_to(MKSTR(DOC_FILE),
				     make_number(VSUBR(subr)->subr_DocIndex),
				     make_number((int)'\f')));
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
	    if(VSYM(subr)->sym_Function)
		subr = VSYM(subr)->sym_Function;
	}
	else
	{
	    if(VSYM(subr)->sym_Value)
		subr = VSYM(subr)->sym_Value;
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
	return(VSUBR(subr)->subr_Name);
    default:
	return(sym_nil);
    }
}

_PR VALUE cmd_eval_hook(VALUE);
DEFUN("eval-hook", cmd_eval_hook, subr_eval_hook, (VALUE args), V_SubrN, DOC_eval_hook) /*
::doc:eval_hook::
eval-hook HOOK ARGS...

Evaluate the hook, HOOK (a symbol), with arguments ARGS

The way hooks work is that the hook-symbol's value is a list of functions
to call. Each function in turn is called with ARGS until one returns non-nil,
this non-nil value is then the result of `eval-hook'. If all functions return
nil then `eval-hook' returns nil.
::end:: */
{
    if(CONSP(args))
    {
	VALUE hook = VCAR(args);
	VALUE alist = VCDR(args);
	VALUE res = sym_nil;
	GCVAL gcv_alist, gcv_hook;
	PUSHGC(gcv_alist, alist);
	switch(VTYPE(hook))
	{
	case V_StaticString:
	case V_DynamicString:
	    if(!(hook = cmd_find_symbol(hook, sym_nil)))
		goto end;
	    /* FALL THROUGH */
	case V_Symbol:
	    hook = cmd_symbol_value(hook, sym_t);
	    if(VOIDP(hook))
		goto end;
	    break;
	}
	PUSHGC(gcv_hook, hook);
	while(res && NILP(res) && CONSP(hook))
	{
	    res = funcall(VCAR(hook), alist);
	    hook = VCDR(hook);
	    TEST_INT;
	    if(INT_P)
		res = NULL;
	}
	POPGC;
end:
	POPGC;
	return(res);
    }
    return signal_missing_arg(1);
}

_PR VALUE cmd_eval_hook2(VALUE hook, VALUE arg);
DEFUN("eval-hook2", cmd_eval_hook2, subr_eval_hook2, (VALUE hook, VALUE arg), V_Subr2, DOC_eval_hook2) /*
::doc:eval_hook2::
eval-hook2 HOOK ARG

Similar to `eval-hook', the only reason this function exists is because it
is easier to call a 2-argument function from C than an N-argument function.
::end:: */
{
    VALUE res = sym_nil, alist;
    /* Not possible to use GCVAL's since this is often called from C code
       which may not be protected.  */
    int oldgci = gc_inhibit;
    if(!(alist = cmd_cons(arg, sym_nil)))
	return(NULL);
    gc_inhibit = TRUE;
    switch(VTYPE(hook))
    {
    case V_StaticString:
    case V_DynamicString:
	if(!(hook = cmd_find_symbol(hook, sym_nil)))
	    goto end;
	/* FALL THROUGH */
    case V_Symbol:
	hook = cmd_symbol_value(hook, sym_t);
	if(VOIDP(hook))
	    goto end;
	break;
    }
    while(res && NILP(res) && CONSP(hook))
    {
	res = funcall(VCAR(hook), alist);
	hook = VCDR(hook);
	TEST_INT;
	if(INT_P)
	    res = NULL;
    }
end:
    gc_inhibit = oldgci;
    return(res);
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
       unwind normally through all levels of recursion with a NULL result.
       This is slow but it's easy to work with.  */
{
    if(CONSP(args))
    {
	VALUE tag, res = NULL;
	GCVAL gcv_args, gcv_tag;
	PUSHGC(gcv_args, args);
	tag = cmd_eval(VCAR(args));
	if(tag)
	{
	    PUSHGC(gcv_tag, tag);
	    if(!(res = cmd_progn(VCDR(args))))
	    {
		if(throw_value && (VCAR(throw_value) == tag))
		{
		    res = VCDR(throw_value);
		    throw_value = NULL;
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
    return(NULL);
}

_PR VALUE cmd_return(VALUE);
DEFUN("return", cmd_return, subr_return, (VALUE arg), V_Subr1, DOC_return) /*
::doc:return::
return [VALUE]

Arranges it so that the innermost defun returns VALUE (or nil) as its result.
::end:: */
{
    if(!throw_value)
	throw_value = cmd_cons(sym_defun, arg);
    return(NULL);
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
	GCVAL gcv_args, gcv_res, gcv_throwval;
	PUSHGC(gcv_args, args);
	res = cmd_eval(VCAR(args));
	PUSHGC(gcv_res, res);
	throwval = throw_value;
	throw_value = NULL;
	PUSHGC(gcv_throwval, throwval);
	if(!cmd_progn(VCDR(args)))
	    res = NULL;
	/* Only reinstall the old throw if it's actually an error.
	   Otherwise it could overwrite an error in the CLEANUP-FORMS.
	   This way the oldest error takes precedence. */
	if(throwval != 0)
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
    ADD_SUBR(subr_function);
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
    ADD_SUBR(subr_load);
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
    ADD_SUBR(subr_or);
    ADD_SUBR(subr_logand);
    ADD_SUBR(subr_and);
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
    ADD_SUBR(subr_eval_hook);
    ADD_SUBR(subr_eval_hook2);
    ADD_SUBR(subr_catch);
    ADD_SUBR(subr_throw);
    ADD_SUBR(subr_return);
    ADD_SUBR(subr_unwind_protect);
    INTERN(sym_load_path, "load-path");
    VSYM(sym_load_path)->sym_Value = list_3(null_string,
					    MKSTR(SITE_LISP_DIR),
					    MKSTR(LISP_LIB_DIR));
    DOC_VAR(sym_load_path, DOC_load_path);
    INTERN(sym_lisp_lib_dir, "lisp-lib-dir");
    VSYM(sym_lisp_lib_dir)->sym_Value = MKSTR(LISP_LIB_DIR);
    DOC_VAR(sym_lisp_lib_dir, DOC_lisp_lib_dir);
}
