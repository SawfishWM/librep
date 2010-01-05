/* ffi.c -- foreigh function interface plugin

   $Id$

   Copyright (C) 2003 John Harper <jsh@unfactored.org>

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

   (ffi-struct [MEMBER-TYPES ...]) -> TYPE
     -- creates a new structure type

   (ffi-interface RET-TYPE (PARAM-TYPES ...)) -> INTERFACE
     -- creates a new ffi function signature

   (ffi-type BASE-TYPE [PREDICATE] [TYPE->BASE] [BASE->TYPE]) -> TYPE
     -- creates a new type alias

   (ffi-apply INTERFACE FN-POINTER ARG-LIST) -> RET-VALUE
     -- calls a function and returns its result

   Apply works by walking the list of arguments converting everything
   into native or structure types. It calls the function then converts
   any returned value back to lisp data.

   Question: how to handle `out' parameters? E.g.

	void foo (int a, int *out_b) { *out_b = a + 1 }

   one possibility is to recognize `(out TYPE)' as meaning, allocate
   space for one of TYPE on the stack, pass its address to the
   function, then return the converted value (somehow)

   but then how do you handle arrays? E.g.

	void foo (int n, int values[]) {...}

   Perhaps just provide some primitives:

	(ffi-new TYPE #!optional (COUNT 1)) -> POINTER
	(ffi-delete TYPE POINTER)

	(ffi-address-of TYPE POINTER INDEX) -> POINTER'

	(ffi-set! TYPE POINTER VALUE)
	(ffi-get TYPE POINTER) -> VALUE

   this should be enough to allow everything to be handled by higher
   level code. */

#include "repint.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#ifdef HAVE_FFI_H
#include <ffi.h>
#ifndef ALIGN /* was in older ffi.h */
#define ALIGN(v, a)  (((((unsigned) (v))-1) | ((a)-1))+1)
#endif
#endif

#if SIZEOF_VOID_P == SIZEOF_LONG
# define rep_make_pointer(p) rep_make_long_uint ((unsigned long) p)
# define rep_get_pointer(x)  (void *) rep_get_long_uint (x)
# define rep_pointerp(x)     rep_INTEGERP (x)
#elif SIZEOF_VOID_P != SIZEOF_LONG_LONG
# define rep_make_pointer(p) rep_make_longlong_int ((unsigned long long) p)
# define rep_get_pointer(x)  (void *) rep_get_longlong_int (x)
# define rep_pointerp(x)     rep_INTEGERP (x)
#else
# error "weird pointer size"
#endif

#ifdef HAVE_LIBFFI

typedef struct rep_ffi_type_struct rep_ffi_type;
typedef struct rep_ffi_alias_struct rep_ffi_alias;
typedef struct rep_ffi_struct_struct rep_ffi_struct;
typedef struct rep_ffi_interface_struct rep_ffi_interface;

struct rep_ffi_type_struct {
    ffi_type *type;
    unsigned int subtype;
};

enum rep_ffi_subtype_enum {
    rep_FFI_PRIMITIVE = 0,
    rep_FFI_STRUCT,
    rep_FFI_ALIAS,
};

struct rep_ffi_alias_struct {
    rep_ffi_type super;
    repv predicate;
    repv conv_in;
    repv conv_out;
    unsigned int base;
};

struct rep_ffi_struct_struct {
    rep_ffi_type super;
    ffi_type type;
    unsigned int n_elements;
    unsigned int *element_ids;
    ffi_type *elements[1];
};

#define SIZEOF_REP_FFI_STRUCT(n) \
    (sizeof (rep_ffi_struct) + (sizeof (ffi_type *) * (n)))

struct rep_ffi_interface_struct {
    ffi_cif cif;
    ffi_type *ret_type;
    unsigned int n_args;
    ffi_type **arg_types;
    size_t args_size;
    unsigned int ret;
    unsigned int args[1];
};

#define SIZEOF_REP_FFI_INTERFACE(n) \
    (sizeof (rep_ffi_interface) + (sizeof (int) * ((n) - 1)))

static int n_ffi_types, n_alloc_ffi_types;
static rep_ffi_type **ffi_types;

static int n_ffi_interfaces, n_alloc_ffi_interfaces;
static rep_ffi_interface **ffi_interfaces;

#define rep_VALID_TYPE_P(x) \
    (rep_INTP (x) && rep_INT (x) >= 0 && rep_INT (x) < n_ffi_types)
#define rep_VALID_INTERFACE_P(x) \
    (rep_INTP (x) && rep_INT (x) >= 0 && rep_INT (x) < n_ffi_interfaces)

static rep_bool
ffi_types_equal_p (const rep_ffi_type *a, const rep_ffi_type *b)
{
    if (a->subtype != b->subtype)
	return rep_FALSE;
    if (a->type != NULL && a->type == b->type)
	return rep_TRUE;

    switch (a->subtype)
    {
    case rep_FFI_PRIMITIVE:
	return (a->type->type == b->type->type
		&& a->type->size == b->type->size
		&& a->type->alignment == b->type->alignment);

    case rep_FFI_STRUCT: {
	const rep_ffi_struct *sa, *sb;
	unsigned int i;

	sa = (rep_ffi_struct *) a;
	sb = (rep_ffi_struct *) b;

	if (sa->n_elements != sb->n_elements)
	    return rep_FALSE;

	for (i = 0; i < sa->n_elements; i++)
	{
	    if (sa->element_ids[i] != sb->element_ids[i])
		return rep_FALSE;
	}

	return rep_TRUE;
    }

    case rep_FFI_ALIAS: {
	const rep_ffi_alias *aa, *ab;

	aa = (rep_ffi_alias *) a;
	ab = (rep_ffi_alias *) b;

	return (aa->base == ab->base
		&& aa->predicate == ab->predicate
		&& aa->conv_in == ab->conv_in
		&& aa->conv_out == ab->conv_out);
    }

    default:
	return rep_FALSE;
    }
}

static rep_bool
ffi_interfaces_equal_p (const rep_ffi_interface *a, const rep_ffi_interface *b)
{
    unsigned int i;

    if (a->n_args != b->n_args
	|| a->args_size != b->args_size || a->ret != b->ret)
    {
	return rep_FALSE;
    }

    for (i = 0; i < a->n_args; i++)
    {
	if (a->args[i] != b->args[i])
	    return rep_FALSE;
    }

    return rep_TRUE;
}

static unsigned int
ffi_alloc_type (rep_ffi_type *type)
{
    unsigned int i;

    /* FIXME: this is O(N), it should be hashed. */

    for (i = 0; i < n_ffi_types; i++)
    {
	if (ffi_types_equal_p (type, ffi_types[i]))
	{
	    rep_free (type);
	    return i;
	}
    }

    i = n_ffi_types++;

    if (i >= n_alloc_ffi_types)
    {
	n_alloc_ffi_types = MAX (n_alloc_ffi_types * 2, 256);
	ffi_types = rep_realloc (ffi_types, sizeof (ffi_types[0])
				 * n_alloc_ffi_types);
    }

    ffi_types[i] = type;

    return i;
}

static unsigned int
ffi_alloc_interface (rep_ffi_interface *iface)
{
    unsigned int i;

    /* FIXME: this is O(N), it should be hashed. */

    for (i = 0; i < n_ffi_interfaces; i++)
    {
	if (ffi_interfaces_equal_p (iface, ffi_interfaces[i]))
	{
	    rep_free (iface);
	    return i;
	}
    }

    i = n_ffi_interfaces++;

    if (i >= n_alloc_ffi_interfaces)
    {
	n_alloc_ffi_interfaces = MAX (n_alloc_ffi_interfaces * 2, 256);
	ffi_interfaces = rep_realloc (ffi_interfaces,
				      sizeof (ffi_interfaces[0])
				      * n_alloc_ffi_interfaces);
    }

    ffi_interfaces[i] = iface;

    return i;
}

static char *
rep_ffi_marshal (unsigned int type_id, repv value, char *ptr)
{
    rep_ffi_type *type = ffi_types[type_id];

    switch (type->subtype)
    {
	DEFSTRING (err, "unknown ffi type id");
	DEFSTRING (err2, "ffi struct expected a vector or list");

    case rep_FFI_PRIMITIVE:
	switch (type->type->type)
	{
	case FFI_TYPE_VOID:
	    return ptr;

	case FFI_TYPE_INT:
	    *(int *)ptr = (int) rep_get_long_int (value);
	    return ptr + sizeof (int);

	case FFI_TYPE_FLOAT:
	    *(float *)ptr = (float) rep_get_float (value);
	    return ptr + sizeof (float);

	case FFI_TYPE_DOUBLE:
	    *(double *)ptr = (double) rep_get_float (value);
	    return ptr + sizeof (double);

#if FFI_TYPE_LONGDOUBLE != FFI_TYPE_DOUBLE
	case FFI_TYPE_LONGDOUBLE:
	    *(long double *)ptr = (long double) rep_get_float (value);
	    return ptr + sizeof (long double);
#endif

	case FFI_TYPE_UINT8:
	    *(uint8_t *)ptr = (uint8_t) rep_get_long_int (value);
	    return ptr + sizeof (uint8_t);

	case FFI_TYPE_SINT8:
	    *(int8_t *)ptr = (int8_t) rep_get_long_int (value);
	    return ptr + sizeof (int8_t);

	case FFI_TYPE_UINT16:
	    *(uint16_t *)ptr = (uint16_t) rep_get_long_int (value);
	    return ptr + sizeof (uint16_t);

	case FFI_TYPE_SINT16:
	    *(int16_t *)ptr = (int16_t) rep_get_long_int (value);
	    return ptr + sizeof (int16_t);

	case FFI_TYPE_UINT32:
	    *(uint32_t *)ptr = (uint32_t) rep_get_long_int (value);
	    return ptr + sizeof (uint32_t);

	case FFI_TYPE_SINT32:
	    *(int32_t *)ptr = (int32_t) rep_get_long_int (value);
	    return ptr + sizeof (int32_t);

	case FFI_TYPE_UINT64:
	    *(uint64_t *)ptr = (uint64_t) rep_get_longlong_int (value);
	    return ptr + sizeof (uint64_t);

	case FFI_TYPE_SINT64:
	    *(int64_t *)ptr = (int64_t) rep_get_longlong_int (value);
	    return ptr + sizeof (int64_t);

	case FFI_TYPE_POINTER:
	    *(void **)ptr = (rep_STRINGP(value)) ? rep_STR (value) : rep_get_pointer (value);
	    return ptr + sizeof (void *);

	case FFI_TYPE_STRUCT:		/* FIXME: */
	default:
	    Fsignal (Qerror, rep_list_2 (rep_VAL (&err),
					 rep_MAKE_INT (type_id)));
	    return NULL;
	}
	/* not reached */

    case rep_FFI_STRUCT: {
	rep_ffi_struct *s = (rep_ffi_struct *) type;
	rep_GC_root gc_value;
	int i;

	rep_PUSHGC (gc_value, value);

	for (i = 0; i < s->n_elements; i++)
	{
	    repv elt;

	    if (rep_VECTORP (value))
		elt = rep_VECTI (value, i);
	    else if (rep_CONSP (value)) {
		elt = rep_CAR (value);
		value = rep_CDR (value);
	    } else {
		rep_POPGC;
		Fsignal (Qerror, rep_list_2 (rep_VAL (&err2), value));
		return NULL;
	    }

	    ptr = rep_ffi_marshal (s->element_ids[i], elt, ptr);

	    if (ptr == NULL) {
		rep_POPGC;
		return NULL;
	    }
	}

	rep_POPGC;
	return ptr;
    }

    case rep_FFI_ALIAS: {
	rep_ffi_alias *s = (rep_ffi_alias *) type;

	if (s->conv_in != rep_NULL) {
	    value = rep_call_lisp1 (s->conv_in, value);
	    if (value == rep_NULL)
		return NULL;
	}

	return rep_ffi_marshal (s->base, value, ptr);
    }

    default:
	Fsignal (Qerror, rep_list_2 (rep_VAL (&err), rep_MAKE_INT (type_id)));
	return NULL;
    }
}

static char *
rep_ffi_demarshal (unsigned int type_id, char *ptr, repv *value)
{
    rep_ffi_type *type = ffi_types[type_id];

    switch (type->subtype)
    {
	DEFSTRING (err, "unknown ffi type id");

    case rep_FFI_PRIMITIVE:
	switch (type->type->type)
	{
	case FFI_TYPE_VOID:
	    *value = rep_undefined_value;
	    return ptr;

	case FFI_TYPE_INT:
	    *value = rep_make_long_int (*(int *)ptr);
	    return ptr + sizeof (int);

	case FFI_TYPE_FLOAT:
	    *value = rep_make_float (*(float *)ptr, rep_TRUE);
	    return ptr + sizeof (float);

	case FFI_TYPE_DOUBLE:
	    *value = rep_make_float (*(double *)ptr, rep_TRUE);
	    return ptr + sizeof (double);

#if FFI_TYPE_LONGDOUBLE != FFI_TYPE_DOUBLE
	case FFI_TYPE_LONGDOUBLE:
	    *value = rep_make_float (*(long double *)ptr, rep_TRUE);
	    return ptr + sizeof (long double);
#endif

	case FFI_TYPE_UINT8:
	    *value = rep_MAKE_INT (*(uint8_t *)ptr);
	    return ptr + sizeof (uint8_t);

	case FFI_TYPE_SINT8:
	    *value = rep_MAKE_INT (*(int8_t *)ptr);
	    return ptr + sizeof (int8_t);

	case FFI_TYPE_UINT16:
	    *value = rep_MAKE_INT (*(uint16_t *)ptr);
	    return ptr + sizeof (uint16_t);

	case FFI_TYPE_SINT16:
	    *value = rep_MAKE_INT (*(int16_t *)ptr);
	    return ptr + sizeof (int16_t);

	case FFI_TYPE_UINT32:
	    *value = rep_make_long_int (*(uint32_t *)ptr);
	    return ptr + sizeof (uint32_t);

	case FFI_TYPE_SINT32:
	    *value = rep_make_long_int (*(int32_t *)ptr);
	    return ptr + sizeof (int32_t);

	case FFI_TYPE_UINT64:
	    *value = rep_make_longlong_int (*(uint64_t *)ptr);
	    return ptr + sizeof (uint64_t);

	case FFI_TYPE_SINT64:
	    *value = rep_make_longlong_int (*(int64_t *)ptr);
	    return ptr + sizeof (int64_t);

	case FFI_TYPE_POINTER:
	    *value = rep_make_pointer (*(void **)ptr);
	    return ptr + sizeof (void *);

	case FFI_TYPE_STRUCT:		/* FIXME: */
	default:
	    Fsignal (Qerror, rep_list_2 (rep_VAL (&err),
					 rep_MAKE_INT (type_id)));
	    return NULL;
	}
	/* not reached */

    case rep_FFI_STRUCT: {
	rep_ffi_struct *s = (rep_ffi_struct *) type;
	rep_GC_n_roots gc_value;
	int i;

	*value = rep_make_vector (s->n_elements);
	rep_PUSHGCN (gc_value, value, 1);

	for (i = 0; i < s->n_elements; i++)
	{
	    ptr = rep_ffi_demarshal (s->element_ids[i], ptr,
				     &rep_VECTI (*value, i));
	    if (ptr == NULL) {
		rep_POPGCN;
		return NULL;
	    }
	}

	rep_POPGCN;
	return ptr;
    }

    case rep_FFI_ALIAS: {
	rep_ffi_alias *s = (rep_ffi_alias *) type;

	ptr = rep_ffi_marshal (s->base, *value, ptr);

	if (s->conv_in != rep_NULL) {
	    *value = rep_call_lisp1 (s->conv_out, *value);
	    if (*value == rep_NULL)
		return NULL;
	}

	return ptr;
    }

    default:
	Fsignal (Qerror, rep_list_2 (rep_VAL (&err), rep_MAKE_INT (type_id)));
	return NULL;
    }
}

DEFUN ("ffi-struct", Fffi_struct, Sffi_struct, (repv fields), rep_Subr1)
{
    unsigned int i, n;
    rep_ffi_struct *s;

    if (rep_VECTORP (fields))
	n = rep_VECT_LEN (fields);
    else if (rep_CONSP (fields))
	n = rep_list_length (fields);
    else
	return rep_signal_arg_error (fields, 1);

    s = rep_alloc (SIZEOF_REP_FFI_STRUCT (n) + sizeof (unsigned int) * n);
    s->super.type = &s->type;
    s->super.subtype = rep_FFI_STRUCT;

    s->element_ids = (void *) ((char *) s + SIZEOF_REP_FFI_STRUCT (n));

    for (i = 0; i < n; i++)
    {
	repv x;
 
	if (rep_VECTORP (fields))
	    x = rep_VECTI (fields, i);
	else if (rep_CONSP (fields)) {
	    x = rep_CAR (fields);
	    fields = rep_CDR (fields);
	} else
	    x = rep_NULL;

	if (x == rep_NULL || !rep_VALID_TYPE_P (x))
	{
	    rep_free (s);
	    return rep_signal_arg_error (x, 1);
	}

	s->element_ids[i] = rep_INT (x);
	s->elements[i] = ffi_types[rep_INT (x)]->type;
    }
    s->elements[n] = NULL;

    s->n_elements = n;
    s->type.elements = s->elements;
    s->type.size = s->type.alignment = 0;

    /* We should leave size and alignment as zero and let libffi initialize
       them. But that doesn't for me, need the size known at all times. */

    for (i = 0; i < s->n_elements; i++)
    {
	s->type.size = ALIGN (s->type.size, s->elements[i]->alignment);
	s->type.size += s->elements[i]->size;
	s->type.alignment = MAX (s->type.alignment, s->elements[i]->alignment);
    }

    return rep_MAKE_INT (ffi_alloc_type (&s->super));
}

DEFUN ("ffi-type", Fffi_type, Sffi_type,
       (repv base, repv pred, repv in, repv out), rep_Subr4)
{
    rep_ffi_alias *s;

    rep_DECLARE (1, base, rep_VALID_TYPE_P (base));

    s = rep_alloc (sizeof (rep_ffi_alias));

    s->super.subtype = rep_FFI_ALIAS;
    s->super.type = ffi_types[rep_INT (base)]->type;
    s->predicate = pred;
    s->conv_in = in;
    s->conv_out = out;
    s->base = rep_INT (base);

    return rep_MAKE_INT (ffi_alloc_type (&s->super));
}

static repv
ffi_add_primitive_type (ffi_type *type)
{
    rep_ffi_type *s;

    s = rep_alloc (sizeof (rep_ffi_type));

    s->subtype = rep_FFI_PRIMITIVE;
    s->type = type;

    return rep_MAKE_INT (ffi_alloc_type (s));
}

DEFUN ("ffi-interface", Fffi_interface, Sffi_interface,
       (repv ret, repv args), rep_Subr2)
{
    unsigned int i, n;
    rep_ffi_interface *s;

    if (ret != Qnil)
	rep_DECLARE (1, ret, rep_VALID_TYPE_P (ret));

    if (rep_VECTORP (args))
	n = rep_VECT_LEN (args);
    else if (rep_LISTP (args))
	n = rep_list_length (args);
    else
	return rep_signal_arg_error (args, 2);

    s = rep_alloc (SIZEOF_REP_FFI_INTERFACE (n)
		   + sizeof (ffi_type *) * n);
    s->arg_types = (void *) (((char *) s) + SIZEOF_REP_FFI_INTERFACE (n));

    s->n_args = n;

    s->ret = (ret != Qnil) ? rep_INT (ret) : 0;
    s->ret_type = ffi_types[s->ret]->type;

    s->args_size = 0;
    for (i = 0; i < n; i++)
    {
	repv elt;

	if (rep_VECTORP (args))
	    elt = rep_VECTI (args, i);
	else
	{
	    elt = rep_CAR (args);
	    args = rep_CDR (args);
	}

	if (!rep_VALID_TYPE_P (elt))
	{
	    rep_free (s);
	    return rep_signal_arg_error (args, 2);
	}

	s->args[i] = rep_INT (elt);
	s->arg_types[i] = ffi_types[s->args[i]]->type;

	if (s->arg_types[i]->alignment > 1)
	    s->args_size = ALIGN (s->args_size, s->arg_types[i]->alignment);

	s->args_size += s->arg_types[i]->size;
    }

    if (ffi_prep_cif (&s->cif, FFI_DEFAULT_ABI, s->n_args,
		      s->ret_type, s->arg_types) != FFI_OK)
    {
	rep_free (s);
	return rep_signal_arg_error (args, 1);	/* FIXME: */
    }

    return rep_MAKE_INT (ffi_alloc_interface (s));
}

DEFUN ("ffi-apply", Fffi_apply, Sffi_apply,
       (repv iface_id, repv ptr, repv args), rep_Subr3)
{
    rep_ffi_interface *iface;
    void *function_ptr;
    rep_GC_root gc_args;

    rep_DECLARE (1, iface_id, rep_VALID_INTERFACE_P (iface_id));
    rep_DECLARE (2, ptr, rep_pointerp (ptr));

    iface = ffi_interfaces[rep_INT (iface_id)];
    function_ptr = rep_get_pointer (ptr);

    if (function_ptr != NULL)
    {
	/* use arrays of doubles to get good alignment. */
	double _args_data[(iface->args_size >> 3) + 1];
	double _ret_data[(iface->cif.rtype->size >> 3) + 1];
	void *values[iface->n_args];
	char *ret_data = NULL, *args_data = NULL, *args_ptr;
	repv ret_value = rep_undefined_value;
	int i;

	if (iface->cif.rtype->size != 0)
	    ret_data = (char *) _ret_data;

	args_data = (char *) _args_data;
	args_ptr = args_data;

	rep_PUSHGC (gc_args, args);

	for (i = 0; i < iface->n_args; i++)
	{
	    repv elt;

	    values[i] = args_ptr;

	    if (!rep_CONSP (args)) {
		rep_POPGC;
		return rep_signal_arg_error (args, 3);
	    }

	    elt = rep_CAR (args);
	    args = rep_CDR (args);

	    args_ptr = rep_ffi_marshal (iface->args[i], elt, args_ptr);
	    if (args_ptr == NULL) {
		rep_POPGC;
		return rep_NULL;
	    }
	}

	rep_POPGC;

	ffi_call (&iface->cif, function_ptr, ret_data, values);

	if (ret_data != NULL)
	{
	    if (rep_ffi_demarshal (iface->ret, ret_data, &ret_value) == NULL)
		return rep_NULL;
	}

	return ret_value;
    }

    return rep_signal_arg_error (ptr, 2);
}

DEFUN ("ffi-new", Fffi_new, Sffi_new, (repv type_id, repv count), rep_Subr2)
{
    rep_ffi_type *type;
    void *ptr;

    rep_DECLARE1 (type_id, rep_VALID_TYPE_P);
    if (count != Qnil)
	rep_DECLARE2 (count, rep_INTP);
    else
	count = rep_MAKE_INT (1);

    type = ffi_types[rep_INT (type_id)];
    ptr = rep_alloc (type->type->size * rep_INT (count));
    
    return rep_make_pointer (ptr);
}

DEFUN ("ffi-delete", Fffi_delete, Sffi_delete,
       (repv type_id, repv addr), rep_Subr2)
{
    rep_DECLARE1 (type_id, rep_VALID_TYPE_P);
    rep_DECLARE2 (addr, rep_pointerp);

    rep_free (rep_get_pointer (addr));

    return rep_undefined_value;
}

DEFUN ("ffi-address-of", Fffi_address_of, Sffi_address_of,
       (repv type_id, repv addr, repv idx), rep_Subr3)
{
    rep_ffi_type *type;
    char *ptr;
    int i;

    rep_DECLARE1 (type_id, rep_VALID_TYPE_P);
    rep_DECLARE2 (addr, rep_pointerp);
    rep_DECLARE (3, idx, rep_INTP (idx) && rep_INT (idx) >= 0);

    type = ffi_types[rep_INT (type_id)];
    ptr = rep_get_pointer (addr);

    for (i = rep_INT (idx); i > 0; i--)
    {
	ptr = (void *) ALIGN (ptr, type->type->alignment);
	ptr += type->type->size;
    }

    return rep_make_pointer (ptr);
}

DEFUN ("ffi-set!", Fffi_set_, Sffi_set_,
       (repv type_id, repv addr, repv value), rep_Subr4)
{
    rep_ffi_type *type;
    char *ptr;

    rep_DECLARE1 (type_id, rep_VALID_TYPE_P);
    rep_DECLARE2 (addr, rep_pointerp);

    type = ffi_types[rep_INT (type_id)];
    ptr = rep_get_pointer (addr);

    ptr = (void *) ALIGN (ptr, type->type->alignment);

    if (rep_ffi_marshal (rep_INT (type_id), value, ptr) == NULL)
	return rep_NULL;

    return rep_undefined_value;
}

DEFUN ("ffi-get", Fffi_get, Sffi_get, (repv type_id, repv addr), rep_Subr2)
{
    rep_ffi_type *type;
    char *ptr;
    repv value;

    rep_DECLARE1 (type_id, rep_VALID_TYPE_P);
    rep_DECLARE2 (addr, rep_pointerp);

    type = ffi_types[rep_INT (type_id)];
    ptr = rep_get_pointer (addr);

    ptr = (void *) ALIGN (ptr, type->type->alignment);

    if (rep_ffi_demarshal (rep_INT (type_id), ptr, &value) == NULL)
	return rep_NULL;

    return value;
}

#else /* HAVE_LIBFFI */

static repv
no_libffi_error (void)
{
    DEFSTRING (err, "ffi support is not present in this installation");
    return Fsignal (Qerror, Fcons (rep_VAL (&err), Qnil));
}

DEFUN ("ffi-struct", Fffi_struct, Sffi_struct, (repv fields), rep_Subr1)
{
    return no_libffi_error ();
}

DEFUN ("ffi-type", Fffi_type, Sffi_type,
       (repv base, repv pred, repv in, repv out), rep_Subr4)
{
    return no_libffi_error ();
}

DEFUN ("ffi-interface", Fffi_interface, Sffi_interface,
       (repv ret, repv args), rep_Subr2)
{
    return no_libffi_error ();
}

DEFUN ("ffi-apply", Fffi_apply, Sffi_apply,
       (repv iface_id, repv ptr, repv args), rep_Subr3)
{
    return no_libffi_error ();
}

DEFUN ("ffi-new", Fffi_new, Sffi_new, (repv type_id, repv count), rep_Subr2)
{
    return no_libffi_error ();
}

DEFUN ("ffi-delete", Fffi_delete, Sffi_delete,
       (repv type_id, repv addr), rep_Subr2)
{
    return no_libffi_error ();
}

DEFUN ("ffi-address-of", Fffi_address_of, Sffi_address_of,
       (repv type_id, repv addr, repv idx), rep_Subr3)
{
    return no_libffi_error ();
}

DEFUN ("ffi-set!", Fffi_set_, Sffi_set_,
       (repv type_id, repv addr, repv value), rep_Subr4)
{
    return no_libffi_error ();
}

DEFUN ("ffi-get", Fffi_get, Sffi_get, (repv type_id, repv addr), rep_Subr2)
{
    return no_libffi_error ();
}

#endif /* HAVE_LIBFFI */

DEFUN ("ffi-load-library", Fffi_load_library,
       Sffi_load_library, (repv name), rep_Subr1)
{
    int handle;

    rep_DECLARE (1, name, rep_STRINGP (name));

    handle = rep_intern_dl_library (name);

    if (name == -1) {
	DEFSTRING (err, "Can't open shared library");
	return Fsignal (Qerror, rep_list_2 (rep_VAL (&err), name));
    }

    return rep_MAKE_INT (handle);
}

DEFUN ("ffi-lookup-symbol", Fffi_lookup_symbol,
       Sffi_lookup_symbol, (repv handle, repv name), rep_Subr2)
{
    void *ptr;

    if (handle != Qnil)
	rep_DECLARE (1, handle, rep_INTP (handle));
    rep_DECLARE (2, name, rep_STRINGP (name));

    /* anything outside the range of valid handles means RTLD_DEFAULT. */
    ptr = rep_lookup_dl_symbol (handle != Qnil ? rep_INT (handle) : -1,
				rep_STR (name));

    return ptr != NULL ? rep_make_pointer (ptr) : Qnil;
}


/* dl hooks */

DEFSYM (ffi_type_void, "ffi-type-void");
DEFSYM (ffi_type_uint8, "ffi-type-uint8");
DEFSYM (ffi_type_sint8, "ffi-type-sint8");
DEFSYM (ffi_type_uint16, "ffi-type-uint16");
DEFSYM (ffi_type_sint16, "ffi-type-sint16");
DEFSYM (ffi_type_uint32, "ffi-type-uint32");
DEFSYM (ffi_type_sint32, "ffi-type-sint32");
DEFSYM (ffi_type_uint64, "ffi-type-uint64");
DEFSYM (ffi_type_sint64, "ffi-type-sint64");
DEFSYM (ffi_type_float, "ffi-type-float");
DEFSYM (ffi_type_double, "ffi-type-double");
DEFSYM (ffi_type_longdouble, "ffi-type-longdouble");
DEFSYM (ffi_type_pointer, "ffi-type-pointer");

repv
rep_dl_init (void)
{
    repv tem = rep_push_structure ("rep.ffi");

    rep_INTERN (ffi_type_void);
    rep_INTERN (ffi_type_uint8);
    rep_INTERN (ffi_type_sint8);
    rep_INTERN (ffi_type_uint16);
    rep_INTERN (ffi_type_sint16);
    rep_INTERN (ffi_type_uint32);
    rep_INTERN (ffi_type_sint32);
    rep_INTERN (ffi_type_uint64);
    rep_INTERN (ffi_type_sint64);
    rep_INTERN (ffi_type_float);
    rep_INTERN (ffi_type_double);
    rep_INTERN (ffi_type_longdouble);
    rep_INTERN (ffi_type_pointer);

#ifdef HAVE_LIBFFI
    Fset (Qffi_type_void, ffi_add_primitive_type (&ffi_type_void));
    Fset (Qffi_type_uint8, ffi_add_primitive_type (&ffi_type_uint8));
    Fset (Qffi_type_sint8, ffi_add_primitive_type (&ffi_type_sint8));
    Fset (Qffi_type_uint16, ffi_add_primitive_type (&ffi_type_uint16));
    Fset (Qffi_type_sint16, ffi_add_primitive_type (&ffi_type_sint16));
    Fset (Qffi_type_uint32, ffi_add_primitive_type (&ffi_type_uint32));
    Fset (Qffi_type_sint32, ffi_add_primitive_type (&ffi_type_sint32));
    Fset (Qffi_type_uint64, ffi_add_primitive_type (&ffi_type_uint64));
    Fset (Qffi_type_sint64, ffi_add_primitive_type (&ffi_type_sint64));
    Fset (Qffi_type_float, ffi_add_primitive_type (&ffi_type_float));
    Fset (Qffi_type_double, ffi_add_primitive_type (&ffi_type_double));
    Fset (Qffi_type_longdouble, ffi_add_primitive_type (&ffi_type_longdouble));
    Fset (Qffi_type_pointer, ffi_add_primitive_type (&ffi_type_pointer));
#else
    Fset (Qffi_type_void, Qnil);
    Fset (Qffi_type_uint8, Qnil);
    Fset (Qffi_type_sint8, Qnil);
    Fset (Qffi_type_uint16, Qnil);
    Fset (Qffi_type_sint16, Qnil);
    Fset (Qffi_type_uint32, Qnil);
    Fset (Qffi_type_sint32, Qnil);
    Fset (Qffi_type_uint64, Qnil);
    Fset (Qffi_type_sint64, Qnil);
    Fset (Qffi_type_float, Qnil);
    Fset (Qffi_type_double, Qnil);
    Fset (Qffi_type_longdouble, Qnil);
    Fset (Qffi_type_pointer, Qnil);
#endif

    Fexport_binding (Qffi_type_void);
    Fexport_binding (Qffi_type_uint8);
    Fexport_binding (Qffi_type_sint8);
    Fexport_binding (Qffi_type_uint16);
    Fexport_binding (Qffi_type_sint16);
    Fexport_binding (Qffi_type_uint32);
    Fexport_binding (Qffi_type_sint32);
    Fexport_binding (Qffi_type_uint64);
    Fexport_binding (Qffi_type_sint64);
    Fexport_binding (Qffi_type_float);
    Fexport_binding (Qffi_type_double);
    Fexport_binding (Qffi_type_longdouble);
    Fexport_binding (Qffi_type_pointer);

    rep_ADD_SUBR (Sffi_struct);
    rep_ADD_SUBR (Sffi_type);
    rep_ADD_SUBR (Sffi_interface);
    rep_ADD_SUBR (Sffi_apply);

    rep_ADD_SUBR (Sffi_load_library);
    rep_ADD_SUBR (Sffi_lookup_symbol);

    rep_ADD_SUBR (Sffi_new);
    rep_ADD_SUBR (Sffi_delete);
    rep_ADD_SUBR (Sffi_address_of);
    rep_ADD_SUBR (Sffi_set_);
    rep_ADD_SUBR (Sffi_get);

    return rep_pop_structure (tem);
}
