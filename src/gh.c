/* gh.c -- Guile Helper compat functions

   Copyright (C) 2003 John Harper <jsh@pixelslut.com>

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

/* The GH interface to guile is deprecated, and this is only a partial
   implementation, but it may be useful. E.g. it made it easier to get
   SWIG working with rep.. */

#define _GNU_SOURCE

#include "rep_gh.h"
#include "repint.h"

#include <string.h>

#define UNIMP							\
do {								\
    static int warned;						\
    if (!warned)						\
    {								\
	fprintf (stderr, "%s: unimplemented", __FUNCTION__);	\
	warned = 1;						\
    }								\
} while (0)

#define UNIMP_RET UNIMP; return rep_undefined_value

void gh_enter(int argc, char *argv[], void (*c_main_prog)(int, char **))
{
    UNIMP;
}

void gh_repl(int argc, char *argv[])
{
    UNIMP;
}

repv gh_catch(repv tag, scm_t_catch_body body, void *body_data,
	      scm_t_catch_handler handler, void *handler_data)
{
    UNIMP_RET;
}

repv gh_standard_handler(void *data, repv tag, repv throw_args)
{
    UNIMP_RET;
}

repv gh_eval_str(const char *scheme_code)
{
    UNIMP_RET;
}

repv gh_eval_str_with_catch(const char *scheme_code, scm_t_catch_handler handler)
{
    UNIMP_RET;
}

repv gh_eval_str_with_standard_handler(const char *scheme_code)
{
    UNIMP_RET;
}

repv gh_eval_str_with_stack_saving_handler(const char *scheme_code)
{
    UNIMP_RET;
}

repv gh_eval_file(const char *fname)
{
    UNIMP_RET;
}

repv gh_eval_file_with_catch(const char *scheme_code, scm_t_catch_handler handler)
{
    UNIMP_RET;
}

repv gh_eval_file_with_standard_handler(const char *scheme_code)
{
    UNIMP_RET;
}

repv gh_new_procedure(const char *proc_name, repv (*fn)(),
		      int n_required_args, int n_optional_args, int varp)
{
    UNIMP_RET;
}

repv gh_new_procedure0_0(const char *proc_name, repv (*fn)(void))
{
    return gh_new_procedure (proc_name, fn, 0, 0, 0);
}

repv gh_new_procedure0_1(const char *proc_name, repv (*fn)(repv))
{
    return gh_new_procedure (proc_name, fn, 0, 1, 0);
}

repv gh_new_procedure0_2(const char *proc_name, repv (*fn)(repv, repv))
{
    return gh_new_procedure (proc_name, fn, 0, 2, 0);
}

repv gh_new_procedure1_0(const char *proc_name, repv (*fn)(repv))
{
    return gh_new_procedure (proc_name, fn, 1, 0, 0);
}

repv gh_new_procedure1_1(const char *proc_name, repv (*fn)(repv, repv))
{
    return gh_new_procedure (proc_name, fn, 1, 1, 0);
}

repv gh_new_procedure1_2(const char *proc_name, repv (*fn)(repv, repv, repv))
{
    return gh_new_procedure (proc_name, fn, 1, 2, 0);
}

repv gh_new_procedure2_0(const char *proc_name, repv (*fn)(repv, repv))
{
    return gh_new_procedure (proc_name, fn, 2, 0, 0);
}

repv gh_new_procedure2_1(const char *proc_name, repv (*fn)(repv, repv, repv))
{
    return gh_new_procedure (proc_name, fn, 2, 1, 0);
}

repv gh_new_procedure2_2(const char *proc_name, repv (*fn)(repv, repv, repv, repv))
{
    return gh_new_procedure (proc_name, fn, 2, 2, 0);
}

repv gh_new_procedure3_0(const char *proc_name, repv (*fn)(repv, repv, repv))
{
    return gh_new_procedure (proc_name, fn, 3, 0, 0);
}

repv gh_new_procedure4_0(const char *proc_name, repv (*fn)(repv, repv, repv, repv))
{
    return gh_new_procedure (proc_name, fn, 4, 0, 0);
}

repv gh_new_procedure5_0(const char *proc_name, repv (*fn)(repv, repv, repv, repv, repv))
{
    return gh_new_procedure (proc_name, fn, 5, 0, 0);
}

/* C to Scheme conversion */
repv gh_bool2scm(int x)
{
    return x ? Qt : Qnil;
}

repv gh_int2scm(int x)
{
    return rep_make_long_int (x);
}

repv gh_ulong2scm(unsigned long x)
{
    return rep_make_long_uint (x);
}

repv gh_long2scm(long x)
{
    return rep_make_long_int (x);
}

repv gh_double2scm(double x)
{
    return rep_make_float (x, rep_FALSE);
}

repv gh_char2scm(char c)
{
    return rep_MAKE_INT (c);
}

repv gh_str2scm(const char *s, size_t len)
{
    return rep_string_dupn (s, len);
}

repv gh_str02scm(const char *s)
{
    return rep_string_dup (s);
}

void gh_set_substr(char *src, repv dst, long start, size_t len)
{
    UNIMP;
}

repv gh_symbol2scm(const char *symbol_str)
{
    return Fintern (rep_string_dup (symbol_str), Qnil);
}

repv gh_ints2scm(const int *d, long n)
{
    int i;
    repv vec;

    vec = rep_make_vector (n);
    for (i = 0; i < n; i++)
	rep_VECTI (vec, i) = rep_make_long_int (d[i]);

    return vec;
}

repv gh_doubles2scm(const double *d, long n)
{
    int i;
    repv vec;

    vec = rep_make_vector (n);
    for (i = 0; i < n; i++)
	rep_VECTI (vec, i) = rep_make_float (d[i], rep_FALSE);

    return vec;
}

/* Scheme to C conversion */
int gh_scm2bool(repv obj)
{
    return obj != Qnil;
}

int gh_scm2int(repv obj)
{
    return rep_get_long_int (obj);
}

unsigned long gh_scm2ulong(repv obj)
{
    return rep_get_long_uint (obj);
}

long gh_scm2long(repv obj)
{
    return rep_get_long_int (obj);
}

char gh_scm2char(repv obj)
{
    return rep_INTP (obj) && rep_INT (obj);
}

double gh_scm2double(repv obj)
{
    return rep_get_float (obj);
}

char *gh_scm2newstr(repv str, size_t *lenp)
{
    char *buf;
    size_t len;

    if (!rep_STRINGP (str))
	return NULL;

    len = rep_STRING_LEN (str);
    buf = malloc (len + 1);
    memcpy (buf, rep_STR (str), len);
    buf[len] = 0;

    if (lenp != NULL)
	*lenp = len;

    return buf;
}

void gh_get_substr(repv src, char *dst, long start, size_t len)
{
    if (!rep_STRING (src) || rep_STRING_LEN (src) <= start)
	return;

    len = MIN (len, rep_STRING_LEN (src) - start);
    memcpy (dst, rep_STR (src) + start, len);
}

char *gh_symbol2newstr(repv sym, size_t *lenp)
{
    if (!rep_SYMBOLP (sym))
	return NULL;

    return gh_scm2newstr (rep_SYM (sym)->name, lenp);
}

char *gh_scm2chars(repv vector, char *result)
{
    int len = gh_length (vector), i;

    if (len == 0)
	return result;

    if (result == NULL)
	result = malloc (len * sizeof (result[0]));

    for (i = 0; i < len; i++)
	result[i] = gh_scm2char (Felt (vector, rep_MAKE_INT (i)));

    return result;
}

short *gh_scm2shorts(repv vector, short *result)
{
    int len = gh_length (vector), i;

    if (len == 0)
	return result;

    if (result == NULL)
	result = malloc (len * sizeof (result[0]));

    for (i = 0; i < len; i++)
	result[i] = rep_get_long_int (Felt (vector, rep_MAKE_INT (i)));

    return result;
}

long *gh_scm2longs(repv vector, long *result)
{
    int len = gh_length (vector), i;

    if (len == 0)
	return result;

    if (result == NULL)
	result = malloc (len * sizeof (result[0]));

    for (i = 0; i < len; i++)
	result[i] = rep_get_long_int (Felt (vector, rep_MAKE_INT (i)));

    return result;
}

float *gh_scm2floats(repv vector, float *result)
{
    int len = gh_length (vector), i;

    if (len == 0)
	return result;

    if (result == NULL)
	result = malloc (len * sizeof (result[0]));

    for (i = 0; i < len; i++)
	result[i] = rep_get_float (Felt (vector, rep_MAKE_INT (i)));

    return result;
}

double *gh_scm2doubles(repv vector, double *result)
{
    int len = gh_length (vector), i;

    if (len == 0)
	return result;

    if (result == NULL)
	result = malloc (len * sizeof (result[0]));

    for (i = 0; i < len; i++)
	result[i] = rep_get_float (Felt (vector, rep_MAKE_INT (i)));

    return result;
}

/* type predicates: tell you if an repv object has a given type */

int gh_boolean_p(repv val)
{
    return Qt;
}

int gh_symbol_p(repv val)
{
    return rep_SYMBOLP (val);
}

int gh_char_p(repv val)
{
    return rep_INTP (val);
}

int gh_vector_p(repv val)
{
    return rep_VECTORP (val);
}

int gh_pair_p(repv val)
{
    return rep_CONSP (val);
}

int gh_number_p(repv val)
{
    return rep_NUMERICP (val);
}

int gh_string_p(repv val)
{
    return rep_STRINGP (val);
}

int gh_procedure_p(repv val)
{
    val = Ffunctionp (val);
    return val && val != Qnil;
}

int gh_list_p(repv val)
{
    return rep_LISTP (val);
}

int gh_inexact_p(repv val)
{
    val = Fexactp (val);
    return val && val == Qnil;
}

int gh_exact_p(repv val)
{
    val = Fexactp (val);
    return val && val != Qnil;
}


/* more predicates */
int gh_eq_p(repv x, repv y)
{
    return x == y;
}

int gh_eqv_p(repv x, repv y)
{
    repv val = Feql (x, y);
    return val && val != Qnil;
}

int gh_equal_p(repv x, repv y)
{
    repv val = Fequal (x, y);
    return val && val != Qnil;
}

int gh_string_equal_p(repv s1, repv s2)
{
    return rep_STRINGP (s1) && rep_STRINGP (s2) && gh_equal_p (s1, s2);
}

int gh_null_p(repv l)
{
    return l == Qnil;
}


/* standard Scheme procedures available from C */

repv gh_not(repv val)
{
    return val == Qnil ? Qt : Qnil;
}


repv gh_define(const char *name, repv val)
{
    UNIMP_RET;
}


/* string manipulation routines */
repv gh_make_string(repv k, repv chr)
{
    return Fmake_string (k, chr);
}

repv gh_string_length(repv str)
{
    return Flength (str);
}

repv gh_string_ref(repv str, repv k)
{
    return Faref (str, k);
}

repv gh_string_set_x(repv str, repv k, repv chr)
{
    return Faset (str, k, chr);
}

repv gh_substring(repv str, repv start, repv end)
{
    return Fsubstring (str, start, end);
}

#define APPLY_LIST(lst,fun)		\
    int n = gh_length (lst), i;		\
    repv *v = NULL;			\
    if (n != 0) {			\
	v = alloca (sizeof (repv) * n);	\
	for (i = 0; i < n; i++)	{	\
	    v[i] = rep_CAR (lst);	\
	    lst = rep_CDR (lst);	\
	}				\
    }					\
    return fun (n, v)

repv gh_string_append(repv args)
{
    APPLY_LIST (args, Fconcat);
}

repv gh_vector(repv ls)
{
    APPLY_LIST (ls, Fvector);
}

repv gh_make_vector(repv length, repv val)
{
    return Fmake_vector (length, val);
}

repv gh_vector_set_x(repv vec, repv pos, repv val)
{
    return Faset (vec, pos, val);
}

repv gh_vector_ref(repv vec, repv pos)
{
    return Faref (vec, pos);
}

unsigned long gh_vector_length (repv v)
{
    return gh_length (v);
}

unsigned long gh_uniform_vector_length (repv v)
{
    UNIMP;
    return 0;
}

repv gh_uniform_vector_ref (repv v, repv ilist)
{
    UNIMP_RET;
}

#define gh_list_to_vector(ls) gh_vector(ls)
repv gh_vector_to_list(repv v)
{
    UNIMP_RET;
}


repv gh_lookup (const char *sname)
{
    UNIMP_RET;
}

repv gh_module_lookup (repv module, const char *sname)
{
    UNIMP_RET;
}

repv gh_cons(repv x, repv y)
{
    return Fcons (x, y);
}

repv gh_list(repv elt, ...)
{
    repv lst = Qnil;
    va_list args;

    va_start (args, elt);

    while (elt != rep_undefined_value)
    {
	lst = Fcons (elt, lst);
	elt = va_arg (args, repv);
    }

    va_end (args);
    return Fnreverse (lst);
}

unsigned long gh_length(repv l)
{
    repv len = Flength (l);
    return len && rep_INTP (len) ? rep_INT (len) : 0;
}

repv gh_append(repv args)
{
    APPLY_LIST (args, Fappend);
}

repv gh_append2(repv l1, repv l2)
{
    repv v[2];
    v[0] = l1;
    v[1] = l2;
    return Fappend (2, v);
}

repv gh_append3(repv l1, repv l2, repv l3)
{
    repv v[3];
    v[0] = l1;
    v[1] = l2;
    v[2] = l3;
    return Fappend (3, v);
}

repv gh_append4(repv l1, repv l2, repv l3, repv l4)
{
    repv v[4];
    v[0] = l1;
    v[1] = l2;
    v[2] = l3;
    v[3] = l4;
    return Fappend (4, v);
}

repv gh_reverse(repv ls)
{
    return Freverse (ls);
}

repv gh_list_tail(repv ls, repv k)
{
    return Fnthcdr (k, ls);
}

repv gh_list_ref(repv ls, repv k)
{
    return Fnth (k, ls);
}

repv gh_memq(repv x, repv ls)
{
    return Fmemq (x, ls);
}

repv gh_memv(repv x, repv ls)
{
    return Fmemql (x, ls);
}

repv gh_member(repv x, repv ls)
{
    return Fmember (x, ls);
}

repv gh_assq(repv x, repv alist)
{
    return Fassq (x, alist);
}

repv gh_assv(repv x, repv alist)
{
    UNIMP_RET;
}

repv gh_assoc(repv x, repv alist)
{
    return Fassoc (x, alist);
}

repv gh_car(repv x)
{
    return rep_CONSP (x) ? rep_CAR (x) : rep_undefined_value;
}

repv gh_cdr(repv x)
{
    return rep_CONSP (x) ? rep_CDR (x) : rep_undefined_value;
}

repv gh_caar(repv x)
{
    return gh_car (gh_car (x));
}

repv gh_cadr(repv x)
{
    return gh_car (gh_cdr (x));
}

repv gh_cdar(repv x)
{
    return gh_cdr (gh_car (x));
}

repv gh_cddr(repv x)
{
    return gh_cdr (gh_cdr (x));
}

repv gh_caaar(repv x)
{
    return gh_car (gh_car (gh_car (x)));
}

repv gh_caadr(repv x)
{
    return gh_car (gh_car (gh_cdr (x)));
}

repv gh_cadar(repv x)
{
    return gh_car (gh_cdr (gh_car (x)));
}

repv gh_caddr(repv x)
{
    return gh_car (gh_cdr (gh_cdr (x)));
}

repv gh_cdaar(repv x)
{
    return gh_cdr (gh_car (gh_car (x)));
}

repv gh_cdadr(repv x)
{
    return gh_cdr (gh_car (gh_cdr (x)));
}

repv gh_cddar(repv x)
{
    return gh_cdr (gh_cdr (gh_car (x)));
}

repv gh_cdddr(repv x)
{
    return gh_cdr (gh_cdr (gh_cdr (x)));
}


repv gh_set_car_x(repv pair, repv value)
{
    return Frplaca (pair, value) ? value : rep_undefined_value;
}

repv gh_set_cdr_x(repv pair, repv value)
{
    return Frplacd (pair, value) ? value : rep_undefined_value;
}


/* Calling Scheme functions from C.  */

repv gh_apply (repv proc, repv ls)
{
    return Ffuncall (Fcons (proc, ls));
}

repv gh_call0 (repv proc)
{
    return rep_call_lisp0 (proc);
}

repv gh_call1 (repv proc, repv arg)
{
    return rep_call_lisp1 (proc, arg);
}

repv gh_call2 (repv proc, repv arg1, repv arg2)
{
    return rep_call_lisp2 (proc, arg1, arg2);
}

repv gh_call3 (repv proc, repv arg1, repv arg2, repv arg3)
{
    return rep_call_lisp3 (proc, arg1, arg2, arg3);
}


/* reading and writing Scheme objects.  */

void gh_display (repv x)
{
    UNIMP;
}

void gh_write (repv x)
{
    UNIMP;
}

void gh_newline (void)
{
    UNIMP;
}
