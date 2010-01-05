/* numbers.c -- Implement the tower of numeric types
   Copyright (C) 1993, 1994, 2000 John Harper <john@dcs.warwick.ac.uk>
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
   along with librep; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define _GNU_SOURCE

/* AIX requires this to be the first thing in the file.  */
#include <config.h>
#ifdef __GNUC__
# define alloca __builtin_alloca
#else
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include "repint.h"
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <float.h>
#include <ctype.h>
#include <limits.h>
#include <errno.h>
#include <time.h>

#ifdef HAVE_LOCALE_H
# include <locale.h>
#endif

#ifdef HAVE_GMP
#include <gmp.h>
#endif

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

DEFSTRING(div_zero, "Divide by zero");
DEFSTRING(domain_error, "Domain error");

#if !defined (LONG_LONG_MIN)
# if defined (LONGLONG_MIN)
   /* AIX and IRIX use LONGLONG_ */
#  define LONG_LONG_MIN LONGLONG_MIN
#  define LONG_LONG_MAX LONGLONG_MAX
# elif defined (LLONG_MIN)
   /* Solaris and OpenBSD uses LLONG_ */
#  define LONG_LONG_MIN LLONG_MIN
#  define LONG_LONG_MAX LLONG_MAX
# endif
#endif

/* XXX hmm.. */
#if !defined (LONG_LONG_MAX)
# define LONG_LONG_MAX LONG_MAX
#endif
#if !defined (LONG_LONG_MIN)
# define LONG_LONG_MIN LONG_MIN
#endif

#if !defined (HAVE_STRTOLL) && defined (HAVE_STRTOQ)
# define strtoll strtoq
# define HAVE_STRTOLL 1
#endif


/* Private type definitions */

typedef struct {
    repv car;
#ifdef HAVE_GMP
    mpz_t z;
#else
    rep_long_long z;
#endif
} rep_number_z;

#ifndef HAVE_GMP
# if SIZEOF_LONG_LONG > SIZEOF_LONG
#  define BIGNUM_MIN LONG_LONG_MIN
#  define BIGNUM_MAX LONG_LONG_MAX
# else
#  define BIGNUM_MIN LONG_MIN
#  define BIGNUM_MAX LONG_MAX
# endif
#endif

#ifdef __FreeBSD__
#  define LONG_LONG_MIN LONG_MIN
#  define LONG_LONG_MAX LONG_MAX
#endif

typedef struct {
    repv car;
#ifdef HAVE_GMP
    mpq_t q;
#endif
} rep_number_q;

typedef struct {
    repv car;
    double f;
} rep_number_f;

typedef struct rep_number_block_struct {
    union {
	struct rep_number_block_struct *p;
	/* ensure that the following is aligned correctly */
#ifdef HAVE_GMP
	mpz_t dummy_z;
	mpq_t dummy_q;
#else
	rep_long_long dummy_z;
#endif
	double dummy_f;
    } next;
    rep_number data[1];
} rep_number_block;

#define rep_SIZEOF_NUMBER_BLOCK(n,t) \
    (sizeof (rep_number_block) - sizeof (rep_number) + (t) * (n))

#define rep_NUMBER(v,t) (((rep_number_ ## t *) rep_PTR(v))->t)

#define rep_NUMBER_INEXACT_P(v) (rep_NUMBERP(v) && rep_NUMBER_FLOAT_P(v))

#define ZEROP(x) \
    (rep_INTP (x) ? (x) == rep_MAKE_INT (0) : Fzerop (x) != Qnil)


/* number object handling */

static rep_number_block *number_block_chain[3];
static rep_number *number_freelist[3];
static int number_allocations[3], number_sizeofs[3];
static int allocated_numbers, used_numbers;

static inline int
type_to_index (int type)
{
    return (type == rep_NUMBER_BIGNUM ? 0
	    : type == rep_NUMBER_RATIONAL ? 1 : 2);
}

static void *
make_number (int type)
{
    rep_number *cn;
    int idx = type_to_index (type);
    cn = number_freelist[idx];
    if(cn == NULL)
    {
	int i;
	rep_number_block *cb;
	rep_number *ptr, *next;
	cb = rep_alloc (rep_SIZEOF_NUMBER_BLOCK (number_allocations[idx], 
						 number_sizeofs[idx]));
	allocated_numbers += number_allocations[idx];
	cb->next.p = number_block_chain[idx];
	number_block_chain[idx] = cb;
	ptr = cb->data;
	for(i = 0; i < (number_allocations[idx] - 1); i++, ptr = next)
	{
	    next = (rep_number *) (((char *) ptr) + number_sizeofs[idx]);
	    ptr->car = (repv) next;
	}
	ptr->car = 0;
	number_freelist[idx] = (rep_number *) cb->data;
	cn = number_freelist[idx];
    }
    number_freelist[idx] = (rep_number *) cn->car;
    cn->car = rep_Number | type;
    used_numbers++;
    rep_data_after_gc += sizeof (rep_number);
    return cn;
}

static void
number_sweep(void)
{
    int idx;
    used_numbers = 0;
    for (idx = 0; idx < 3; idx++)
    {
	rep_number_block *cb = number_block_chain[idx];
	number_block_chain[idx] = 0;
	number_freelist[idx] = 0;
	while (cb != 0)
	{
	    rep_number_block *nxt = cb->next.p;
	    rep_number *newfree = 0, *newfreetail = 0, *this;
	    int i, newused = 0;
	    for (i = 0, this = cb->data;
		 i < number_allocations[idx];
		 i++, this = (rep_number *) (((char *) this)
					     + number_sizeofs[idx]))
	    {
		/* if on the freelist then the CELL_IS_8 bit
		   will be unset (since the pointer is long aligned) */
		if (rep_CELL_CONS_P(rep_VAL(this))
		    || !rep_GC_CELL_MARKEDP ((repv) this))
		{
		    if (!newfreetail)
			newfreetail = this;
		    if (!rep_CELL_CONS_P(rep_VAL(this)))
		    {
			switch (idx)
			{
			case 0:
#ifdef HAVE_GMP
			    mpz_clear (((rep_number_z *)this)->z);
#else
			    ((rep_number_z *)this)->z = 0;
#endif
			    break;

			case 1:
#ifdef HAVE_GMP
			    mpq_clear (((rep_number_q *)this)->q);
#endif
			    break;
			}
		    }
		    this->car = rep_VAL (newfree);
		    newfree = this;
		}
		else
		{
		    rep_GC_CLR_CELL ((repv) this);
		    newused++;
		}
	    }
	    if(newused == 0)
	    {
		/* Whole block unused, lets get rid of it.  */
		rep_free(cb);
		allocated_numbers -= number_allocations[idx];
	    }
	    else
	    {
		if(newfreetail != NULL)
		{
		    /* Link this mini-freelist onto the main one.  */
		    newfreetail->car = rep_VAL (number_freelist[idx]);
		    number_freelist[idx] = newfree;
		    used_numbers += newused;
		}
		/* Have to rebuild the block chain as well.  */
		cb->next.p = number_block_chain[idx];
		number_block_chain[idx] = cb;
	    }
	    cb = nxt;
	}
    }
}


/* Promotion */

static repv
dup__ (repv in)
{
    switch (rep_NUMBER_TYPE (in))
    {
	rep_number_z *z;
	rep_number_f *f;

    case rep_NUMBER_BIGNUM:
	z = make_number (rep_NUMBER_BIGNUM);
#ifdef HAVE_GMP
	mpz_init_set (z->z, rep_NUMBER(in,z));
#else
	z->z = rep_NUMBER(in,z);
#endif
	return rep_VAL (z);

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL: {
	rep_number_q *q = make_number (rep_NUMBER_RATIONAL);
	mpq_init (q->q);
	mpq_set (q->q, rep_NUMBER(in,q));
	return rep_VAL (q);
    }
#endif

    case rep_NUMBER_FLOAT:
	f = make_number (rep_NUMBER_FLOAT);
	f->f = rep_NUMBER(in,f);
	return rep_VAL (f);
    }
    abort ();
}

static inline repv
dup (repv in)
{
    if (rep_INTP (in))
	return in;
    else
	return dup__ (in);
}

static repv
promote_to (repv in, int type)
{
    int in_type = rep_NUMERIC_TYPE (in);

    if (in_type >= type)
	return in;

    switch (in_type)
    {
	rep_number_z *z;
	rep_number_f *f;

    case rep_NUMBER_INT:
	switch (type)
	{

	case rep_NUMBER_BIGNUM:
	    z = make_number (rep_NUMBER_BIGNUM);
#ifdef HAVE_GMP
	    mpz_init_set_si (z->z, rep_INT(in));
#else
	    z->z = rep_INT (in);
#endif
	    return rep_VAL (z);

	case rep_NUMBER_RATIONAL:
#ifdef HAVE_GMP
	{
	    rep_number_q *q = make_number (rep_NUMBER_RATIONAL);
	    mpq_init (q->q);
	    mpq_set_si (q->q, rep_INT(in), 1);
	    return rep_VAL (q);
	}
#endif

	case rep_NUMBER_FLOAT:
	    f = make_number (rep_NUMBER_FLOAT);
	    f->f = (double) rep_INT(in);
	    return rep_VAL (f);
	    break;

	default:
	    abort();
	}

    case rep_NUMBER_BIGNUM:
	switch (type)
	{
	case rep_NUMBER_RATIONAL:
#ifdef HAVE_GMP
	{
	    rep_number_q *q = make_number (rep_NUMBER_RATIONAL);
	    mpq_init (q->q);
	    mpq_set_z (q->q, rep_NUMBER(in,z));
	    return rep_VAL (q);
	}
#endif

	case rep_NUMBER_FLOAT:
	    f = make_number (rep_NUMBER_FLOAT);
#ifdef HAVE_GMP
	    f->f = mpz_get_d (rep_NUMBER(in,z));
#else
	    f->f = rep_NUMBER(in,z);
#endif
	    return rep_VAL (f);

	default:
	    abort();
	}

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
	assert (type == rep_NUMBER_FLOAT);
	f = make_number (rep_NUMBER_FLOAT);
	f->f = mpq_get_d (rep_NUMBER(in,q));
	return rep_VAL (f);
#endif

    default:
	abort ();
    }
}

/* IN must be a non-fixnum number */
static repv
maybe_demote (repv in)
{
    assert (rep_NUMBERP(in));
    switch (rep_NUMBER_TYPE(in))
    {
#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
	if (mpz_cmp_ui (mpq_denref (rep_NUMBER (in,q)), 1) == 0)
	{
	    rep_number_z *z = make_number (rep_NUMBER_BIGNUM);
	    mpz_init_set (z->z, mpq_numref (rep_NUMBER (in,q)));
	    in = rep_VAL (z);
	    goto do_bignum;
	}
	break;
#endif

    case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
    do_bignum:
	if (mpz_cmp_si (rep_NUMBER (in,z), rep_LISP_MAX_INT) <= 0
	    && mpz_cmp_si (rep_NUMBER (in,z), rep_LISP_MIN_INT) >= 0)
	{
	    in = rep_MAKE_INT (mpz_get_si (rep_NUMBER (in,z)));
	}
#else
	if (rep_NUMBER (in,z) <= rep_LISP_MAX_INT
	    && rep_NUMBER (in,z) >= rep_LISP_MIN_INT)
	{
	    in = rep_MAKE_INT (rep_NUMBER (in,z));
	}
#endif
    }
    return in;
}

static repv
coerce (repv in, int type)
{
    int in_type = rep_NUMERIC_TYPE (in);
    if (in_type <= type)
	return in;
    switch (in_type)
    {
    case rep_NUMBER_BIGNUM:
	switch (type)
	{
	case rep_NUMBER_INT:
#ifdef HAVE_GMP
	    return rep_MAKE_INT (mpz_get_si (rep_NUMBER (in,z)));
#else
	    return rep_MAKE_INT (rep_NUMBER (in,z));
#endif

	default:
	    abort ();
	}
	break;

	/* XXX implement me.. */
    case rep_NUMBER_RATIONAL:
    case rep_NUMBER_FLOAT:
    default:
	abort ();
    }
    /* not reached. */
    return rep_NULL;
}

static inline void
promote (repv *n1p, repv *n2p)
{
    repv n1 = *n1p;
    repv n2 = *n2p;
    int n1_type = rep_NUMERIC_TYPE (n1);
    int n2_type = rep_NUMERIC_TYPE (n2);

    if (n1_type > n2_type)
	*n2p = promote_to (n2, n1_type);
    else if (n1_type < n2_type)
	*n1p = promote_to (n1, n2_type);
}

static repv
promote_dup__ (repv *n1p, repv *n2p)
{
    repv n1 = *n1p;
    repv n2 = *n2p;
    int n1_type = rep_NUMERIC_TYPE (n1);
    int n2_type = rep_NUMERIC_TYPE (n2);
    repv out = rep_NULL;

    if (n1_type > n2_type)
    {
	out = promote_to (n2, n1_type);
	*n2p = out;
    }
    else if (n1_type < n2_type)
    {
	out = promote_to (n1, n2_type);
	*n1p = out;
    }
    else
	out = dup (*n1p);

    return out;
}

static inline repv
promote_dup (repv *n1p, repv *n2p)
{
    repv n1 = *n1p;
    repv n2 = *n2p;
    if (rep_INTP (n1) && rep_INTP (n2))
	return n1;
    else
	return promote_dup__ (n1p, n2p);
}

repv
rep_make_long_uint (unsigned long in)
{
    if (in < rep_LISP_MAX_INT)
	return rep_MAKE_INT (in);
    else
    {
	rep_number_z *z = make_number (rep_NUMBER_BIGNUM);
#ifdef HAVE_GMP
	mpz_init_set_ui (z->z, in);
#else
	z->z = in;
#endif
	return rep_VAL (z);
    }
}

repv
rep_make_long_int (long in)
{
    if (in >= rep_LISP_MIN_INT && in <= rep_LISP_MAX_INT)
	return rep_MAKE_INT (in);
    else
    {
	rep_number_z *z = make_number (rep_NUMBER_BIGNUM);
#ifdef HAVE_GMP
	mpz_init_set_si (z->z, in);
#else
	z->z = in;
#endif
	return rep_VAL (z);
    }
}

unsigned long
rep_get_long_uint (repv in)
{
    if (rep_INTP (in))
	return rep_INT (in);
    else if (rep_NUMBERP (in))
    {
	switch (rep_NUMBER_TYPE(in))
	{
	case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
	    return mpz_get_ui (rep_NUMBER(in,z));
#else
	    return rep_NUMBER (in,z);
#endif

#ifdef HAVE_GMP
	case rep_NUMBER_RATIONAL:
	    return (unsigned long) mpq_get_d (rep_NUMBER(in,q));
#endif

	case rep_NUMBER_FLOAT:
	    return (unsigned long) rep_NUMBER(in,f);
	}
    }
    else if (rep_CONSP (in)
	     && rep_INTP (rep_CAR (in)) && rep_INTP (rep_CDR (in)))
    {
	return rep_INT (rep_CAR (in)) | (rep_INT (rep_CDR (in)) << 24);
    }
    return 0;
}

long
rep_get_long_int (repv in)
{
    if (rep_INTP (in))
	return rep_INT (in);
    else if (rep_NUMBERP (in))
    {
	switch (rep_NUMBER_TYPE(in))
	{
	case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
	    return mpz_get_si (rep_NUMBER(in,z));
#else
	    return rep_NUMBER (in,z);
#endif

#ifdef HAVE_GMP
	case rep_NUMBER_RATIONAL:
	    return (long) mpq_get_d (rep_NUMBER(in,q));
#endif

	case rep_NUMBER_FLOAT:
	    return (long) rep_NUMBER(in,f);
	}
    }
    else if (rep_CONSP (in)
	     && rep_INTP (rep_CAR (in)) && rep_INTP (rep_CDR (in)))
    {
	return rep_INT (rep_CAR (in)) | (rep_INT (rep_CDR (in)) << 24);
    }
    return 0;
}

#if SIZEOF_LONG_LONG > SIZEOF_LONG

repv
rep_make_longlong_int (rep_long_long in)
{
    if (in <= rep_LISP_MAX_INT && in >= rep_LISP_MIN_INT)
	return rep_MAKE_INT (in);
    else
    {
#ifdef HAVE_GMP
	int sign = (in < 0) ? -1 : 1;
	unsigned rep_long_long uin = (sign < 0) ? -in : in;
	unsigned long bottom = (unsigned long) uin;
	unsigned long top = (unsigned long) (uin >> (CHAR_BIT * sizeof (long)));
	rep_number_z *z = make_number (rep_NUMBER_BIGNUM);
	mpz_init_set_ui (z->z, bottom);
	if (top != 0)
	{
	    mpz_t tem;
	    mpz_init_set_ui (tem, top);
	    mpz_mul_2exp (tem, tem, CHAR_BIT * sizeof (long));
	    mpz_add (z->z, z->z, tem);
	    mpz_clear (tem);
	}
	if (sign < 0)
	    mpz_neg (z->z, z->z);
#else
	rep_number_z *z = make_number (rep_NUMBER_BIGNUM);
	z->z = in;
#endif
	return rep_VAL (z);
    }
}

rep_long_long
rep_get_longlong_int (repv in)
{
    if (rep_INTP (in))
	return rep_INT (in);
    else if (rep_NUMBERP (in))
    {
	switch (rep_NUMBER_TYPE(in))
	{
	case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
	    {
		int sign = mpz_sgn (rep_NUMBER(in,z));
		rep_long_long bottom, top, out;
		mpz_t tem;
		mpz_init_set (tem, rep_NUMBER(in,z));
		if (sign < 0)
		    mpz_neg (tem, tem);
		bottom = mpz_get_ui (tem);
		mpz_tdiv_q_2exp (tem, tem, CHAR_BIT * sizeof (long));
		top = mpz_get_ui (tem);
		out = bottom | (top << (CHAR_BIT * sizeof (long)));
		if (sign < 0)
		    out = -out;
		mpz_clear (tem);
		return out;
	    }
#else
	    return rep_NUMBER (in,z);
#endif

#ifdef HAVE_GMP
	case rep_NUMBER_RATIONAL:
	    return (rep_long_long) mpq_get_d (rep_NUMBER(in,q));
#endif

	case rep_NUMBER_FLOAT:
	    return (rep_long_long) rep_NUMBER(in,f);
	}
    }
    else if (rep_CONSP (in)
	     && rep_INTP (rep_CAR (in)) && rep_INTP (rep_CDR (in)))
    {
	rep_long_long out = rep_INT (rep_CDR (in));
	out = (out << 24) | rep_INT (rep_CAR (in));
	return out;
    }
    return 0;
}

#else /* SIZEOF_LONG_LONG > SIZEOF_LONG */

repv
rep_make_longlong_int (rep_long_long in)
{
    return rep_make_long_int (in);
}

rep_long_long
rep_get_longlong_int (repv in)
{
    return rep_get_long_int (in);
}

#endif /* ! SIZEOF_LONG_LONG > SIZEOF_LONG */

repv
rep_make_float (double in, rep_bool force)
{
    rep_number_f *f;
    if (!force && floor (in) == in)
    {
	if (in < LONG_MAX && in > LONG_MIN)
	    return rep_make_long_int ((long) in);
#if SIZEOF_LONG_LONG > SIZEOF_LONG
	else if (in < LONG_LONG_MAX && in > LONG_LONG_MIN)
	    return rep_make_longlong_int (in);
#endif
    }

    f = make_number (rep_NUMBER_FLOAT);
    f->f = in;
    return rep_VAL (f);
}

double
rep_get_float (repv in)
{
    if (rep_NUMERICP (in))
    {
	switch (rep_NUMERIC_TYPE (in))
	{
	case rep_NUMBER_INT:
	    return rep_INT (in);

	case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
	    return mpz_get_d (rep_NUMBER(in,z));
#else
	    return rep_NUMBER (in,z);
#endif

#ifdef HAVE_GMP
	case rep_NUMBER_RATIONAL:
	    return mpq_get_d (rep_NUMBER(in,q));
#endif

	case rep_NUMBER_FLOAT:
	    return rep_NUMBER(in,f);
	}
    }
    return 0.0;
}

/* this ignores exactness */
int
rep_compare_numbers (repv v1, repv v2)
{
    if(!rep_NUMERICP(v1) || !rep_NUMERICP(v2))
	return 1;
    promote (&v1, &v2);
    switch (rep_NUMERIC_TYPE (v1))
    {
	double d;

    case rep_NUMBER_INT:
	return rep_INT(v1) - rep_INT(v2);

    case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
	return mpz_cmp (rep_NUMBER(v1,z), rep_NUMBER(v2,z));
#else
	return rep_NUMBER(v1,z) - rep_NUMBER(v2,z);
#endif

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
	return mpq_cmp (rep_NUMBER(v1,q), rep_NUMBER(v2,q));
#endif

    case rep_NUMBER_FLOAT:
	d = rep_NUMBER(v1,f) - rep_NUMBER(v2,f);
	return (d < 0) ? -1 : (d > 0) ? +1 : 0;
    }
    return 1;
}

/* this includes exactness in the comparison */
static int
number_cmp (repv v1, repv v2)
{
    int i1, i2;

    if(!rep_NUMERICP(v1) || !rep_NUMERICP(v2))
	return 1;

    i1 = rep_NUMBER_INEXACT_P (v1);
    i2 = rep_NUMBER_INEXACT_P (v2);
    if ((i1 && !i2) || (!i1 && i2))
	return 1;

    promote (&v1, &v2);
    switch (rep_NUMERIC_TYPE (v1))
    {
	double d;

    case rep_NUMBER_INT:
	return rep_INT(v1) - rep_INT(v2);

    case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
	return mpz_cmp (rep_NUMBER(v1,z), rep_NUMBER(v2,z));
#else
	return rep_NUMBER(v1,z) - rep_NUMBER(v2,z);
#endif

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
	return mpq_cmp (rep_NUMBER(v1,q), rep_NUMBER(v2,q));
#endif

    case rep_NUMBER_FLOAT:
	d = rep_NUMBER(v1,f) - rep_NUMBER(v2,f);
	return (d < 0) ? -1 : (d > 0) ? +1 : 0;
    }
    return 1;
}

static const signed int map[] = {
    0,  1,  2,  3,  4,  5,  6,  7,		/* 0x30 -> 0x37 */
    8,  9, -1, -1, -1, -1, -1, -1,
    -1, 10, 11, 12, 13, 14, 15, 16,		/* 0x40 -> 0x48 */
    17, 18, 19, 20, 21, 22, 23, 24,
    25, 26, 27, 28, 29, 30, 31, 32,		/* 0x50 -> 0x58 */
    33, 34, 35, 36
};
#define MAP_SIZE 0x2c

#ifndef HAVE_GMP
static rep_bool
parse_integer_to_float (char *buf, unsigned int len, unsigned int radix,
			int sign, double *output)
{
    double value = 0.0;

    while (len-- > 0)
    {
	int d;
	char c = *buf++;
	d = toupper (c) - '0';
	if (d < 0 || d >= MAP_SIZE)
	    return rep_FALSE;
	d = map [d];
	if (d < 0 || d >= radix)
	    return rep_FALSE;
	value = value * radix + d;
    }

    *output = (sign < 0) ? value * -1.0 : value;
    return rep_TRUE;
}
#endif

#define INSTALL_LOCALE(var, type, locale)	\
    do {					\
	char *tem = setlocale (type, 0);	\
	if (tem != 0)				\
	{					\
	    int len = strlen (tem);		\
	    char *copy = alloca (len + 1);	\
	    memcpy (copy, tem, len);		\
	    copy[len] = 0;			\
	    (var) = copy;			\
	    setlocale (type, locale);		\
	}					\
	else					\
	    (var) = 0;				\
    } while (0)

repv
rep_parse_number (char *buf, unsigned int len, unsigned int radix, int sign, unsigned int type)
{
    if (len == 0)
	goto error;

    switch (type)
    {
	rep_number_z *z;
#ifdef HAVE_GMP
	rep_number_q *q;
#endif
	rep_number_f *f;
	char *tem, *copy, *old_locale;
	double d;
	unsigned int bits;

    case 0:
	switch (radix)
	{
	case 2:
	    bits = len;
	    break;

	case 8:
	    bits = len * 3;
	    break;

	case 10:
	    /* log_2 10 = 3.3219.. ~ 27/8 */
	    bits = (len * 27) / 8;
	    break;

	case 16:
	    bits = len * 4;
	    break;

	default:
	    abort();
	}
	if (bits < rep_LISP_INT_BITS)
	{
	    long value = 0;
	    char c;
	    if (radix == 10)
	    {
		/* optimize most common case */
		while (len-- > 0)
		{
		    c = *buf++;
		    if (c < '0' || c > '9')
			goto error;
		    value = value * 10 + (c - '0');
		}
	    }
	    else
	    {
		while (len-- > 0)
		{
		    int d;
		    c = *buf++;
		    d = toupper (c) - '0';
		    if (d < 0 || d >= MAP_SIZE)
			goto error;
		    d = map [d];
		    if (d < 0 || d >= radix)
			goto error;
		    value = value * radix + d;
		}
	    }
	    return ((sign > 0)
		    ? rep_MAKE_INT (value)
		    : rep_MAKE_INT (value * -1));
	}
	else
	{
	    z = make_number (rep_NUMBER_BIGNUM);
#ifdef HAVE_GMP
	    copy = alloca (len + 1);
	    memcpy (copy, buf, len);
	    copy[len] = 0;
	    if (mpz_init_set_str (z->z, copy, radix) == 0)
	    {
		if (sign < 0)
		    mpz_neg (z->z, z->z);
		return maybe_demote (rep_VAL (z));
	    }
	    else
		goto error;
#else
	    {
		rep_long_long value;
		char *tail;
		copy = alloca (len + 1);
		memcpy (copy, buf, len);
		copy[len] = 0;
		errno = 0;
# ifdef HAVE_STRTOLL
		value = strtoll (copy, &tail, radix);
# else
		value = strtol (copy, &tail, radix);
# endif
		if (errno == ERANGE)
		{
		    /* Overflow - parse to a double, then try to convert
		       back to an int.. */
		    double d;
		    if (parse_integer_to_float (buf, len, radix, sign, &d))
		    {
			if (d > BIGNUM_MIN && d < BIGNUM_MAX)
			{
			    z->z = d;
			    return maybe_demote (rep_VAL (z));
			}
			else
			{
			    f = make_number (rep_NUMBER_FLOAT);
			    f->f = d;
			    return rep_VAL (f);
			}
		    }
		    else
			goto error;
		}
		else if (*tail != 0 || errno != 0)
		    goto error;		/* not all characters used */

		z->z = (sign < 0) ? -value : value;
		return maybe_demote (rep_VAL (z));
	    }
#endif /* !HAVE_GMP */
	}

    case rep_NUMBER_RATIONAL:
	tem = strchr (buf, '/');
	assert (tem != 0);
#ifdef HAVE_GMP
	q = make_number (rep_NUMBER_RATIONAL);
	mpq_init (q->q);
	copy = alloca (tem - buf + 1);
	memcpy (copy, buf, tem - buf);
	copy[tem - buf] = 0;
	if (mpz_set_str (mpq_numref (q->q), copy, radix) == 0
	    && mpz_set_str (mpq_denref (q->q), tem + 1, radix) == 0)
	{
	    if (mpz_sgn (mpq_denref (q->q)) == 0)
		goto error;

	    mpq_canonicalize (q->q);
	    if (sign < 0)
		mpq_neg (q->q, q->q);
	    return maybe_demote (rep_VAL (q));
	}
	else
	    goto error;
#else
	{
	    repv num = rep_parse_number (buf, tem - buf, radix, 1, 0);
	    repv den = rep_parse_number (tem + 1, len - (tem + 1 - buf),
					 radix, 1, 0);
	    if (!num || !den)
		goto error;
	    num = rep_number_div (num, den);
	    if (num && sign < 0)
		num = rep_number_neg (num);
	    return num;
	}
#endif

    case rep_NUMBER_FLOAT:
#ifdef HAVE_SETLOCALE
	INSTALL_LOCALE (old_locale, LC_NUMERIC, "C");
#endif
	d = strtod (buf, &tem);
#ifdef HAVE_SETLOCALE
	if (old_locale != 0)
	    setlocale (LC_NUMERIC, old_locale);
#endif
	if (tem - buf != len)
	    goto error;
	f = make_number (rep_NUMBER_FLOAT);
	f->f = d * sign;
	return rep_VAL (f);
    }
error:
    return rep_NULL;
}

char *
rep_print_number_to_string (repv obj, int radix, int prec)
{
    char *out = 0;

    if (!rep_NUMERICP (obj))
	return strdup ("#<non-number>");

    switch (rep_NUMERIC_TYPE (obj))
    {
	char buf[128], fmt[8], *tem, *old_locale;

    case rep_NUMBER_INT:
	if (radix == 10)
	    tem = "%" rep_PTR_SIZED_INT_CONV "d";
	else if (radix == 16)
	    tem = "%" rep_PTR_SIZED_INT_CONV "x";
	else if (radix == 8)
	    tem = "%" rep_PTR_SIZED_INT_CONV "o";
	else
	{
	    /* XXX implement properly..? */
	    obj = promote_to (obj, rep_NUMBER_BIGNUM);
	    goto do_bignum;
	}
	if (tem != 0)
	{
#ifdef HAVE_SNPRINTF
	    snprintf(buf, sizeof(buf), tem, rep_INT(obj));
#else
	    sprintf(buf, tem, rep_INT(obj));
#endif
	    out = strdup (buf);
	}
	break;

    case rep_NUMBER_BIGNUM:
    do_bignum:
#ifdef HAVE_GMP
	out = mpz_get_str (0, radix, rep_NUMBER(obj,z));
#else
	{
	    static const char *map = "0123456789abcdefghijklmnopqrstuvwxyz";
	    char *ptr = buf, *optr;
	    rep_long_long value = rep_NUMBER(obj,z);
	    int sign = (value < 0) ? -1 : +1;
	    while (value != 0)
	    {
		int digit = value % radix;
		*ptr++ = map[ABS (digit)];
		value = value / radix;
	    }
	    if (sign < 0)
		*ptr++ = '-';
	    out = malloc ((ptr - buf) + 1);
	    for (optr = out; ptr > buf;)
		*optr++ = *(--ptr);
	    *optr = 0;
	}
#endif
	break;

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL: {
	size_t len;
	len = (mpz_sizeinbase (mpq_numref (rep_NUMBER (obj, q)), radix)
	       + mpz_sizeinbase (mpq_denref (rep_NUMBER (obj, q)), radix) + 4);
	out = malloc (len);
	mpz_get_str (out, radix, mpq_numref (rep_NUMBER (obj,q)));
	len = strlen (out);
	out[len++] = '/';
	mpz_get_str (out + len, radix, mpq_denref (rep_NUMBER (obj,q)));
	break;
    }
#endif

    case rep_NUMBER_FLOAT:		/* XXX handle radix arg */
	sprintf (fmt, "%%.%dg", prec < 0 ? 16 : prec);
#ifdef HAVE_SETLOCALE
	INSTALL_LOCALE (old_locale, LC_NUMERIC, "C");
#endif
#ifdef HAVE_SNPRINTF
	snprintf(buf, sizeof(buf), fmt, rep_NUMBER(obj,f));
#else
	sprintf(buf, fmt, rep_NUMBER(obj,f));
#endif
#ifdef HAVE_SETLOCALE
	if (old_locale != 0)
	    setlocale (LC_NUMERIC, old_locale);
#endif
	/* libc doesn't always add a point */
	if (!strchr (buf, '.') && !strchr (buf, 'e') && !strchr (buf, 'E'))
	    strcat (buf, ".");
	out = strdup (buf);
    }
    return out;
}

static void
number_prin (repv stream, repv obj)
{
    if (rep_INTP (obj))
    {
	char buf[64];
#ifdef HAVE_SNPRINTF
	snprintf(buf, sizeof(buf),
		 "%" rep_PTR_SIZED_INT_CONV "d", rep_INT(obj));
#else
	sprintf(buf, "%" rep_PTR_SIZED_INT_CONV "d", rep_INT(obj));
#endif
	rep_stream_puts(stream, buf, -1, rep_FALSE);
    }
    else
    {
	char *string = rep_print_number_to_string (obj, 10, -1);
	if (string != 0)
	{
	    rep_stream_puts (stream, string, -1, rep_FALSE);
	    free (string);
	}
	else
	    rep_stream_puts (stream, "#<unprintable number>", -1, rep_FALSE);
    }
}


/* lisp functions */

repv
rep_number_foldl (repv args, repv (*op)(repv, repv))
{
    if (rep_CONSP (args) && rep_NUMERICP (rep_CAR (args)))
    {
	repv sum = rep_CAR (args);
	int i = 2;
	args = rep_CDR (args);
	while (rep_CONSP (args))
	{
	    repv arg = rep_CAR (args);
	    if (!rep_NUMERICP (arg))
		return rep_signal_arg_error (arg, i);
	    sum = op (sum, arg);
	    args = rep_CDR (args);
	    i++;
	}
	return sum;
    }
    return (rep_CONSP(args) ? rep_signal_arg_error (rep_CAR (args), 1)
	    : rep_signal_missing_arg (1));
}

static inline repv
number_foldv (int argc, repv *argv, repv (*op) (repv, repv))
{
    repv sum;
    int i;

    if (argc < 1)
	return rep_signal_missing_arg (1);
    if (!rep_NUMERICP (argv[0]))
	return rep_signal_arg_error (argv[0], 1);

    sum = argv[0];
    for (i = 1; i < argc; i++)
    {
	if (!rep_NUMERICP (argv[i]))
	    return rep_signal_arg_error (argv[i], i + 1);

	sum = op (sum, argv[i]);
    }

    return sum;
}

repv
rep_integer_foldl (repv args, repv (*op)(repv, repv))
{
    if (rep_CONSP (args) && rep_INTEGERP (rep_CAR (args)))
    {
	repv sum = rep_CAR (args);
	int i = 2;
	args = rep_CDR (args);
	while (rep_CONSP (args))
	{
	    repv arg = rep_CAR (args);
	    if (!rep_INTEGERP (arg))
		return rep_signal_arg_error (arg, i);
	    sum = op (sum, arg);
	    args = rep_CDR (args);
	    i++;
	}
	return sum;
    }
    return (rep_CONSP(args) ? rep_signal_arg_error (rep_CAR (args), 1)
	    : rep_signal_missing_arg (1));
}

static inline repv
integer_foldv (int argc, repv *argv, repv (*op) (repv, repv))
{
    repv sum;
    int i;

    if (argc < 1)
	return rep_signal_missing_arg (1);
    if (!rep_INTEGERP (argv[0]))
	return rep_signal_arg_error (argv[0], 1);

    sum = argv[0];
    for (i = 1; i < argc; i++)
    {
	if (!rep_INTEGERP (argv[i]))
	    return rep_signal_arg_error (argv[i], i + 1);

	sum = op (sum, argv[i]);
    }

    return sum;
}

repv
rep_foldl (repv args, repv (*op)(repv, repv))
{
    if (rep_CONSP (args))
    {
	repv sum = rep_CAR (args);
	int i = 2;
	args = rep_CDR (args);
	while (sum && rep_CONSP (args))
	{
	    repv arg = rep_CAR (args);
	    sum = op (sum, arg);
	    args = rep_CDR (args);
	    i++;
	}
	return sum;
    }
    return rep_signal_missing_arg (1);
}

static inline repv
foldv (int argc, repv *argv, repv (*op) (repv, repv))
{
    repv sum;
    int i;

    if (argc < 1)
	return rep_signal_missing_arg (1);

    sum = argv[0];
    for (i = 1; i < argc; i++)
    {
	sum = op (sum, argv[i]);
    }

    return sum;
}

repv
rep_number_add (repv x, repv y)
{
    repv out;
    rep_DECLARE1 (x, rep_NUMERICP);
    rep_DECLARE2 (y, rep_NUMERICP);
    out = promote_dup (&x, &y);
    switch (rep_NUMERIC_TYPE (out))
    {
	case rep_NUMBER_INT:
	    out = rep_make_long_int (rep_INT (x) + rep_INT (y));
	    break;

	case rep_NUMBER_BIGNUM: {
#ifdef HAVE_GMP
	    mpz_add (rep_NUMBER (out,z), rep_NUMBER (x,z), rep_NUMBER (y,z));
#else
	    double t = (double) rep_NUMBER(x,z) + (double) rep_NUMBER (y,z);
	    if (t > BIGNUM_MIN && t < BIGNUM_MAX)
		rep_NUMBER(out,z) = rep_NUMBER(x,z) + rep_NUMBER(y,z);
	    else
		out = rep_make_float (t, rep_TRUE);
#endif
	    out = maybe_demote (out);
	    break;
	}

#ifdef HAVE_GMP
	case rep_NUMBER_RATIONAL:
	    mpq_add (rep_NUMBER (out,q), rep_NUMBER (x,q), rep_NUMBER (y,q));
	    out = maybe_demote (out);
	    break;
#endif
    
	case rep_NUMBER_FLOAT:
	    rep_NUMBER (out,f) = rep_NUMBER (x,f) + rep_NUMBER (y,f);
	    break;
    }
    return out;
}

repv
rep_number_neg (repv x)
{
    repv out;
    rep_DECLARE1 (x, rep_NUMERICP);
    out = dup (x);
    switch (rep_NUMERIC_TYPE (out))
    {
    case rep_NUMBER_INT:
	out = rep_make_long_int (-rep_INT (x));
	break;

    case rep_NUMBER_BIGNUM: {
#ifdef HAVE_GMP
	mpz_neg (rep_NUMBER(out,z), rep_NUMBER(x,z));
#else
	double t = - (double) rep_NUMBER(x,z);
	if (t > BIGNUM_MIN && t < BIGNUM_MAX)
	    rep_NUMBER(out,z) = - rep_NUMBER(x,z);
	else
	    out = rep_make_float (t, rep_TRUE);
#endif
	break;
    }

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
	mpq_neg (rep_NUMBER(out,q), rep_NUMBER(x,q));
	break;
#endif

    case rep_NUMBER_FLOAT:
	rep_NUMBER(out,f) = -rep_NUMBER(x,f);
	break;
    }
    return out;
}

repv
rep_number_sub (repv x, repv y)
{
    repv out;
    rep_DECLARE1 (x, rep_NUMERICP);
    rep_DECLARE2 (y, rep_NUMERICP);
    out = promote_dup (&x, &y);
    switch (rep_NUMERIC_TYPE (out))
    {
    case rep_NUMBER_INT:
	out = rep_make_long_int (rep_INT (x) - rep_INT (y));
	break;

    case rep_NUMBER_BIGNUM: {
#ifdef HAVE_GMP
	mpz_sub (rep_NUMBER (out,z), rep_NUMBER (x,z), rep_NUMBER (y,z));
#else
	double t = (double) rep_NUMBER (x,z) - (double) rep_NUMBER (y,z);
	if (t > BIGNUM_MIN && t < BIGNUM_MAX)
	    rep_NUMBER (out,z) = rep_NUMBER (x,z) - rep_NUMBER (y,z);
	else
	    out = rep_make_float (t, rep_TRUE);
#endif
	out = maybe_demote (out);
	break;
    }

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
	mpq_sub (rep_NUMBER (out,q), rep_NUMBER (x,q), rep_NUMBER (y,q));
	out = maybe_demote (out);
	break;
#endif
    
    case rep_NUMBER_FLOAT:
	rep_NUMBER (out,f) = rep_NUMBER (x,f) - rep_NUMBER (y,f);
	break;
    }
    return out;
}

repv
rep_number_mul (repv x, repv y)
{
    repv out;
    rep_DECLARE1 (x, rep_NUMERICP);
    rep_DECLARE2 (y, rep_NUMERICP);
    out = promote_dup (&x, &y);
    switch (rep_NUMERIC_TYPE (out))
    {
	rep_long_long tot;

    case rep_NUMBER_INT:
	tot = ((rep_long_long) rep_INT (x)) * ((rep_long_long) rep_INT (y));
	out = rep_make_longlong_int (tot);
	break;

    case rep_NUMBER_BIGNUM: {
#ifdef HAVE_GMP
	mpz_mul (rep_NUMBER (out,z), rep_NUMBER (x,z), rep_NUMBER (y,z));
#else
	double t = (double) rep_NUMBER (x,z) * (double) rep_NUMBER (y,z);
	if (t > BIGNUM_MIN && t < BIGNUM_MAX)
	    rep_NUMBER (out,z) = rep_NUMBER (x,z) * rep_NUMBER (y,z);
	else
	    out = rep_make_float (t, rep_TRUE);
#endif
	out = maybe_demote (out);
	break;
    }

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
	mpq_mul (rep_NUMBER (out,q), rep_NUMBER (x,q), rep_NUMBER (y,q));
	out = maybe_demote (out);
	break;
#endif
    
    case rep_NUMBER_FLOAT:
	rep_NUMBER (out,f) = rep_NUMBER (x,f) * rep_NUMBER (y,f);
	break;
    }
    return out;
}

repv
rep_number_div (repv x, repv y)
{
    repv out;
    rep_DECLARE1 (x, rep_NUMERICP);
    rep_DECLARE2 (y, rep_NUMERICP);

    if (ZEROP (y))
	return Fsignal (Qarith_error, rep_LIST_1 (rep_VAL (&div_zero)));

    out = promote_dup (&x, &y);
    switch (rep_NUMERIC_TYPE (out))
    {
    case rep_NUMBER_INT:
	if (rep_INT (x) % rep_INT (y) == 0)
	    out = rep_MAKE_INT (rep_INT (x) / rep_INT (y));
	else
	{
#ifdef HAVE_GMP
	    unsigned long uy = (rep_INT (y) < 0 ? - rep_INT (y) : rep_INT (y));
	    rep_number_q *q = make_number (rep_NUMBER_RATIONAL);
	    mpq_init (q->q);
	    mpq_set_si (q->q, rep_INT (x), uy);
	    mpq_canonicalize (q->q);
	    if (rep_INT (y) < 0)
		mpq_neg (q->q, q->q);
	    out = rep_VAL (q);
#else
	    rep_number_f *f = make_number (rep_NUMBER_FLOAT);
	    f->f = ((double) rep_INT (x)) / ((double) rep_INT (y));
	    out = rep_VAL (f);
#endif
	}
	break;

    case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
	{
	    mpz_t rem;
	    int sign;
	    mpz_init (rem);
	    mpz_tdiv_r (rem, rep_NUMBER (x,z), rep_NUMBER (y,z));
	    sign = mpz_sgn (rem);
	    mpz_clear (rem);
	    if (sign == 0)
	    {
		mpz_tdiv_q (rep_NUMBER (out,z),
			    rep_NUMBER (x,z), rep_NUMBER (y,z));
		out = maybe_demote (out);
	    }
	    else
	    {
		mpq_t div;
		rep_number_q *q = make_number (rep_NUMBER_RATIONAL);
		mpq_init (q->q);
		mpq_set_z (q->q, rep_NUMBER (x,z));
		mpq_init (div);
		mpq_set_z (div, rep_NUMBER (y,z));
		mpq_div (q->q, q->q, div);
		mpq_clear (div);
		out = rep_VAL (q);
	    }
	}
#else
	if (rep_NUMBER (x,z) % rep_NUMBER (y,z) == 0)
	{
	    rep_number_z *z = make_number (rep_NUMBER_BIGNUM);
	    z->z = rep_NUMBER (x,z) / rep_NUMBER (y,z);
	    out = rep_VAL (z);
	}
	else
	{
	    rep_number_f *f = make_number (rep_NUMBER_FLOAT);
	    f->f = ((double) rep_NUMBER (x,z)) / ((double) rep_NUMBER (y,z));
	    out = rep_VAL (f);
	}
#endif
	break;

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
	mpq_div (rep_NUMBER (out,q), rep_NUMBER (x,q), rep_NUMBER (y,q));
	out = maybe_demote (out);
	break;
#endif
    
    case rep_NUMBER_FLOAT:
	rep_NUMBER (out,f) = rep_NUMBER (x,f) / rep_NUMBER (y,f);
	break;
    }
    return out;
}

repv
rep_number_lognot (repv x)
{
    repv out;
    rep_DECLARE1 (x, rep_NUMERICP);
    switch (rep_NUMERIC_TYPE (x))
    {
	rep_number_z *z;

    case rep_NUMBER_INT:
	out = rep_MAKE_INT (~rep_INT (x));
	break;

    case rep_NUMBER_BIGNUM:
	z = make_number (rep_NUMBER_BIGNUM);
#ifdef HAVE_GMP
	mpz_init (z->z);
	mpz_com (z->z, rep_NUMBER (x,z));
#else
	z->z = ~ rep_NUMBER (x,z);
#endif
	out = rep_VAL (z);
	break;

    default:
	return rep_signal_arg_error (x, 1);
    }
    return out;
}

repv
rep_number_logior (repv x, repv y)
{
    repv out;
    rep_DECLARE1 (x, rep_NUMERICP);
    rep_DECLARE2 (y, rep_NUMERICP);
    out = promote_dup (&x, &y);
    switch (rep_NUMERIC_TYPE (out))
    {
    case rep_NUMBER_INT:
	out = rep_MAKE_INT (rep_INT (x) | rep_INT (y));
	break;

    case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
	mpz_ior (rep_NUMBER (out,z), rep_NUMBER (x,z), rep_NUMBER (y,z));
#else
	rep_NUMBER (out,z) = rep_NUMBER (x,z) | rep_NUMBER (y,z);
#endif
	break;

    default:
	return rep_signal_arg_error (x, 1);
    }
    return out;
}

repv
rep_number_logxor (repv x, repv y)
{
    repv out;
    rep_DECLARE1 (x, rep_NUMERICP);
    rep_DECLARE2 (y, rep_NUMERICP);
    out = promote_dup (&x, &y);
    switch (rep_NUMERIC_TYPE (out))
    {
#ifdef HAVE_GMP
	mpz_t tem;
#endif

    case rep_NUMBER_INT:
	out = rep_MAKE_INT (rep_INT (x) ^ rep_INT (y));
	break;

    case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
	/* XXX is this correct: x^y = x|y & ~(x&y) */
	mpz_init (tem);
	mpz_ior (tem, rep_NUMBER (x,z), rep_NUMBER (y,z));
	mpz_and (rep_NUMBER (out,z), rep_NUMBER (x,z), rep_NUMBER (y,z));
	mpz_com (rep_NUMBER (out,z), rep_NUMBER (out,z));
	mpz_and (rep_NUMBER (out,z), rep_NUMBER (out,z), tem);
	mpz_clear (tem);
#else
	rep_NUMBER (out,z) = rep_NUMBER (x,z) ^ rep_NUMBER (y,z);
#endif
	break;

    default:
	return rep_signal_arg_error (x, 1);
    }
    return out;
}

repv
rep_number_logand (repv x, repv y)
{
    repv out;
    rep_DECLARE1 (x, rep_NUMERICP);
    rep_DECLARE2 (y, rep_NUMERICP);
    out = promote_dup (&x, &y);
    switch (rep_NUMERIC_TYPE (out))
    {
    case rep_NUMBER_INT:
	out = rep_MAKE_INT (rep_INT (x) & rep_INT (y));
	break;

    case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
	mpz_and (rep_NUMBER (out,z), rep_NUMBER (x,z), rep_NUMBER (y,z));
#else
	rep_NUMBER (out,z) = rep_NUMBER (x,z) & rep_NUMBER (y,z);
#endif
	break;

    default:
	return rep_signal_arg_error (x, 1);
    }
    return out;
}

repv
rep_number_max (repv x, repv y)
{
    repv max;
    if (rep_NUMBERP (x) || rep_NUMBERP (y))
    {
	max = (rep_compare_numbers (x, y) >= 0) ? x : y;
	if (rep_NUMBER_INEXACT_P (x) || rep_NUMBER_INEXACT_P (y))
	    max = Fexact_to_inexact (max);
    }
    else
	max = (rep_value_cmp(x, y) >= 0) ? x : y;
    return max;
}

repv
rep_number_min (repv x, repv y)
{
    repv min;
    if (rep_NUMBERP (x) || rep_NUMBERP (y))
    {
	min = (rep_compare_numbers (x, y) <= 0) ? x : y;
	if (rep_NUMBER_INEXACT_P (x) || rep_NUMBER_INEXACT_P (y))
	    min = Fexact_to_inexact (min);
    }
    else
	min = (rep_value_cmp(x, y) <= 0) ? x : y;
    return min;
}

repv
rep_integer_gcd (repv x, repv y)
{
    repv out = promote_dup (&x, &y);
    if (rep_INTP (x))
    {
	/* Euclid's algorithm */
	long m = rep_INT (x), n = rep_INT (y);
	m = ABS (m); n = ABS (n);
	while(m != 0)
	{
	    long t = n % m;
	    n = m;
	    m = t;
	}
	out = rep_MAKE_INT (n);
    }
    else
    {
#ifdef HAVE_GMP
	mpz_gcd (rep_NUMBER(out,z), rep_NUMBER(x,z), rep_NUMBER(y,z));
#else
	/* Euclid's algorithm */
	rep_long_long m = rep_NUMBER (x,z), n = rep_NUMBER (y,z);
	m = ABS (m); n = ABS (n);
	while(m != 0)
	{
	    rep_long_long t = n % m;
	    n = m;
	    m = t;
	}
	rep_NUMBER (out,z) = n;
#endif
    }
    return out;
}

DEFUN("+", Fplus, Splus, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#+::
+ NUMBERS...

Adds all NUMBERS together. If no arguments are given returns 0.
::end:: */
{
    if (argc == 0)
	return rep_MAKE_INT (0);
    else
	return number_foldv (argc, argv, rep_number_add);
}

DEFUN("-", Fminus, Sminus, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#-::
- NUMBER [NUMBERS...]

Either returns the negation of NUMBER or the value of NUMBER minus
NUMBERS
::end:: */
{
    if (argc == 0)
	return rep_signal_missing_arg (1);
    else if (argc == 1)
	return rep_number_neg (argv[0]);
    else
	return number_foldv (argc, argv, rep_number_sub);
}

DEFUN("*", Fproduct, Sproduct, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#*::
* NUMBERS...

Multiplies all NUMBERS together. If no numbers are given returns 1.
::end:: */
{
    if (argc == 0)
	return rep_MAKE_INT (1);
    else
	return number_foldv (argc, argv, rep_number_mul);
}

DEFUN("/", Fdivide, Sdivide, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#/::
/ NUMBERS...

Divides NUMBERS (in left-to-right order).
::end:: */
{
    if (argc == 0)
	return rep_signal_missing_arg (1);
    else if (argc == 1)
	return rep_number_div (rep_MAKE_INT (1), argv[0]);
    else
	return number_foldv (argc, argv, rep_number_div);
}

DEFUN("remainder", Fremainder, Sremainder, (repv n1, repv n2), rep_Subr2) /*
::doc:rep.lang.math#remainder::
remainder DIVIDEND DIVISOR

Returns the integer remainder after dividing DIVIDEND by DIVISOR.
::end:: */
{
    repv out;
    rep_DECLARE1(n1, rep_NUMERICP);
    rep_DECLARE2(n2, rep_NUMERICP);
    if(ZEROP (n2))
	return Fsignal (Qarith_error, rep_LIST_1 (rep_VAL (&div_zero)));

    out = promote_dup (&n1, &n2);
    switch (rep_NUMERIC_TYPE (out))
    {
    case rep_NUMBER_INT:
	out = rep_MAKE_INT (rep_INT (n1) % rep_INT (n2));
	break;

    case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
	mpz_tdiv_r (rep_NUMBER(out,z), rep_NUMBER(n1,z), rep_NUMBER(n2,z));
#else
	{
	    rep_number_z *z = make_number (rep_NUMBER_BIGNUM);
	    z->z = rep_NUMBER (n1,z) % rep_NUMBER (n2,z);
	    out = rep_VAL (z);
	}
#endif
	out = maybe_demote (out);
	break;

    default:
	return rep_signal_arg_error (n1, 1);
    }
    return out;
}

DEFUN("mod", Fmod, Smod, (repv n1, repv n2), rep_Subr2) /*
::doc:rep.lang.math#mod::
mod DIVIDEND DIVISOR

Returns the value of DIVIDEND modulo DIVISOR; unlike the % (remainder)
function the behaviour of `mod' is well-defined for negative arguments,
we have that,

	(mod X Y) == X - (* Y (floor (/ X Y))),	for Y not equal to zero

assuming that (floor Z) gives the least integer greater than or equal to Z,
and that floating point division is used.
::end:: */
{
    repv out;
    rep_DECLARE1(n1, rep_NUMERICP);
    rep_DECLARE2(n2, rep_NUMERICP);
    if(ZEROP (n2))
	return Fsignal (Qarith_error, rep_LIST_1 (rep_VAL (&div_zero)));

    out = promote_dup (&n1, &n2);
    switch (rep_NUMERIC_TYPE (out))
    {
	long tem;
#ifdef HAVE_GMP
	int sign;
#else
        rep_number_z *z;
#endif

    case rep_NUMBER_INT:
	/* This code from GNU Emacs */
	tem = rep_INT (n1) % rep_INT (n2);
	/* If the "remainder" comes out with the wrong sign, fix it.  */
	if (rep_INT (n2) < 0 ? tem > 0 : tem < 0)
	    tem += rep_INT (n2);
	out = rep_MAKE_INT (tem);
	break;

    case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
	mpz_tdiv_r (rep_NUMBER(out,z), rep_NUMBER(n1,z), rep_NUMBER(n2,z));
	/* If the "remainder" comes out with the wrong sign, fix it.  */
	sign = mpz_sgn (rep_NUMBER(out,z));
	if (mpz_sgn (rep_NUMBER(n2,z)) < 0 ? sign > 0 : sign < 0)
	    mpz_add (rep_NUMBER(out,z), rep_NUMBER(out,z), rep_NUMBER(n2,z));
#else
	z = make_number (rep_NUMBER_BIGNUM);
	z->z = rep_NUMBER (n1,z) % rep_NUMBER (n2,z);
	if (rep_NUMBER (n2,z) < 0 ? z->z > 0 : z->z < 0)
	    z->z += rep_NUMBER (n2,z);
	out = rep_VAL (z);
#endif
	out = maybe_demote (out);
	break;

    default:
	return rep_signal_arg_error (n1, 1);
    }
    return out;
}

DEFUN("quotient", Fquotient, Squotient, (repv x, repv y), rep_Subr2) /*
::doc:rep.lang.math#quotient::
quotient DIVIDEND DIVISOR

Returns the integer quotient from dividing integers DIVIDEND and
DIVISOR.
::end:: */
{
    repv out;
    rep_DECLARE1 (x, rep_INTEGERP);
    rep_DECLARE2 (y, rep_INTEGERP);
    if(ZEROP (y))
	return Fsignal (Qarith_error, rep_LIST_1 (rep_VAL (&div_zero)));
    out = promote_dup (&x, &y);
    if (rep_INTP (x))
	out = rep_MAKE_INT (rep_INT (x) / rep_INT (y));
    else
    {
#ifdef HAVE_GMP
	mpz_tdiv_q (rep_NUMBER(out,z), rep_NUMBER(x,z), rep_NUMBER(y,z));
#else
	rep_NUMBER(out,z) = rep_NUMBER (x,z) / rep_NUMBER (y,z);
#endif
	out = maybe_demote (out);
    }
    return out;
}

DEFUN("lognot", Flognot, Slognot, (repv num), rep_Subr1) /*
::doc:rep.lang.math#lognot::
lognot NUMBER

Returns the bitwise logical `not' of NUMBER.
::end:: */
{
    rep_DECLARE1(num, rep_NUMERICP);
    return rep_number_lognot (num);
}

DEFUN("logior", Flogior, Slogior, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#logior::
logior NUMBERS...

Returns the bitwise logical `inclusive-or' of its arguments.
::end:: */
{
    if (argc == 0)
	return rep_MAKE_INT (0);
    else
	return number_foldv (argc, argv, rep_number_logior);
}

DEFUN("logxor", Flogxor, Slogxor, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#logxor::
logxor NUMBERS...

Returns the bitwise logical `exclusive-or' of its arguments.
::end:: */
{
    return number_foldv (argc, argv, rep_number_logxor);
}

DEFUN("logand", Flogand, Slogand, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#logand::
logand NUMBERS...

Returns the bitwise logical `and' of its arguments.
::end:: */
{
    return number_foldv (argc, argv, rep_number_logand);
}

DEFUN("eql", Feql, Seql, (repv arg1, repv arg2), rep_Subr2) /*
::doc:rep.data#eql::
eql ARG1 ARG2

Similar to `eq' except that numbers with the same value will always be
considered `eql' (this may or may not be the case with `eq').

Note however that exact and inexact versions of the same number are not
considered the same value. As a rule of thumb, if two numbers print the
same, they will be considered `eql'.
::end:: */
{
    if(rep_NUMERICP (arg1) && rep_NUMERICP (arg2))
	return number_cmp (arg1, arg2) == 0 ? Qt : Qnil;
    else
	return arg1 == arg2 ? Qt : Qnil;
}

DEFUN("zerop", Fzerop, Szerop, (repv num), rep_Subr1) /*
::doc:rep.lang.math#zerop::
zerop NUMBER

Return t if NUMBER is zero.
::end:: */
{
    if(rep_NUMERICP (num))
    {
	switch (rep_NUMERIC_TYPE (num))
	{
	case rep_NUMBER_INT:
	    return num == rep_MAKE_INT (0) ? Qt : Qnil;

	case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
	    return mpz_sgn (rep_NUMBER(num,z)) == 0 ? Qt : Qnil;
#else
	    return rep_NUMBER (num,z) == 0 ? Qt : Qnil;
#endif

#ifdef HAVE_GMP
	case rep_NUMBER_RATIONAL:
	    return mpq_sgn (rep_NUMBER(num,q)) == 0 ? Qt : Qnil;
#endif

	case rep_NUMBER_FLOAT:
	    return rep_NUMBER(num,f) == 0 ? Qt : Qnil;
	}
    }
    return Qnil;
}

DEFUN("1+", Fplus1, Splus1, (repv num), rep_Subr1) /*
::doc:rep.lang.math#1+::
1+ NUMBER

Return NUMBER plus 1.
::end:: */
{
    rep_DECLARE1(num, rep_NUMERICP);
    switch (rep_NUMERIC_TYPE (num))
    {
#ifdef HAVE_GMP
	mpq_t temq;
#endif

    case rep_NUMBER_INT:
	return rep_make_long_int (rep_INT (num) + 1);

    case rep_NUMBER_BIGNUM:
	num = dup (num);
#ifdef HAVE_GMP
	mpz_add_ui (rep_NUMBER (num,z), rep_NUMBER (num,z), 1);
#else
	rep_NUMBER (num,z) = rep_NUMBER (num,z) + 1;
#endif
	return maybe_demote (num);

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
	num = dup (num);
	mpq_init (temq);
	mpq_set_ui (temq, 1, 1);
	mpq_add (rep_NUMBER (num,q), rep_NUMBER (num,q), temq);
	mpq_clear (temq);
	return maybe_demote (num);
#endif

    case rep_NUMBER_FLOAT:
	num = dup (num);
	rep_NUMBER (num,f) = rep_NUMBER (num,f) + 1;
	return num;
    }
    abort ();
}

DEFUN("1-", Fsub1, Ssub1, (repv num), rep_Subr1) /*
::doc:rep.lang.math#1-::
1- NUMBER

Return NUMBER minus 1.
::end:: */
{
    rep_DECLARE1(num, rep_NUMERICP);
    switch (rep_NUMERIC_TYPE (num))
    {
#ifdef HAVE_GMP
	mpq_t temq;
#endif

    case rep_NUMBER_INT:
	return rep_make_long_int (rep_INT (num) - 1);

    case rep_NUMBER_BIGNUM:
	num = dup (num);
#ifdef HAVE_GMP
	mpz_sub_ui (rep_NUMBER (num,z), rep_NUMBER (num,z), 1);
#else
	rep_NUMBER (num,z) = rep_NUMBER (num,z) - 1;
#endif
	return maybe_demote (num);

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
	num = dup (num);
	mpq_init (temq);
	mpq_set_si (temq, 1, 1);
	mpq_sub (rep_NUMBER (num,q), rep_NUMBER (num,q), temq);
	mpq_clear (temq);
	return maybe_demote (num);
#endif

    case rep_NUMBER_FLOAT:
	num = dup (num);
	rep_NUMBER (num,f) = rep_NUMBER (num,f) - 1;
	return num;
    }
    abort ();
}

DEFUN("ash", Fash, Sash, (repv num, repv shift), rep_Subr2) /*
::doc:rep.lang.math#ash::
ash NUMBER COUNT

Use an arithmetic shift to shift the bits in NUMBER by COUNT bits to
the left, a negative COUNT means shift right.

Both NUMBER and COUNT must be integers.
::end:: */
{
    rep_DECLARE1(num, rep_INTEGERP);
    rep_DECLARE2(shift, rep_INTEGERP);

    shift = coerce (shift, rep_NUMBER_INT);
    switch (rep_NUMERIC_TYPE (num))
    {
	rep_number_z *z;
	rep_long_long tot;

    case rep_NUMBER_INT:
	if (rep_INT (shift) >= rep_LISP_INT_BITS)
	{
	    num = promote_to (num, rep_NUMBER_BIGNUM);
	    goto do_bignum;
	}
	else
	{
	    if (rep_INT (shift) > 0)
		tot = ((rep_long_long) rep_INT (num)) << rep_INT (shift);
	    else
		tot = ((rep_long_long) rep_INT (num)) >> -rep_INT (shift);
	}
	return rep_make_longlong_int (tot);

    case rep_NUMBER_BIGNUM:
    do_bignum:
	z = make_number (rep_NUMBER_BIGNUM);
#ifdef HAVE_GMP
	mpz_init (z->z);
	if (rep_INT (shift) > 0)
	    mpz_mul_2exp (z->z, rep_NUMBER (num,z), rep_INT (shift));
	else
	    mpz_div_2exp (z->z, rep_NUMBER (num,z), - rep_INT (shift));
#else
	if (rep_INT (shift) > 0)
	{
	    long i, this;
	    double factor = 1, t;
	    for (i = rep_INT (shift); i > 0; i -= this)
	    {
		this = MIN (sizeof (long) * CHAR_BIT - 1, i);
		factor = factor * (1L << this);
	    }
	    t = (double) rep_NUMBER (num,z) * factor;
	    if (t > BIGNUM_MIN && t < BIGNUM_MAX)
		z->z = rep_NUMBER (num,z) << rep_INT (shift);
	    else
		return rep_make_float (t, rep_TRUE);
	}
	else
	    z->z = rep_NUMBER (num,z) >> -rep_INT (shift);
#endif
	return maybe_demote (rep_VAL (z));

    default:
	return rep_signal_arg_error (num, 1);
    }
}

DEFUN("floor", Ffloor, Sfloor, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#floor::
floor NUMBER

Round NUMBER downwards to the nearest integer less than or equal to
NUMBER.
::end:: */
{
    rep_DECLARE1 (arg, rep_NUMERICP);
    switch (rep_NUMERIC_TYPE (arg))
    {
    case rep_NUMBER_INT:
    case rep_NUMBER_BIGNUM:
	return arg;

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
	return rep_make_long_int (floor (mpq_get_d (rep_NUMBER (arg,q))));
#endif

    case rep_NUMBER_FLOAT:
	return rep_make_float (floor (rep_NUMBER (arg,f)), rep_TRUE);
    }
    abort ();
}	

DEFUN("ceiling", Fceiling, Sceiling, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#ceiling::
ceiling NUMBER

Round NUMBER upwards to the nearest integer greater than or equal to
NUMBER.
::end:: */
{
    rep_DECLARE1 (arg, rep_NUMERICP);
    switch (rep_NUMERIC_TYPE (arg))
    {
    case rep_NUMBER_INT:
    case rep_NUMBER_BIGNUM:
	return arg;

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
	return rep_make_long_int (ceil (mpq_get_d (rep_NUMBER (arg,q))));
#endif

    case rep_NUMBER_FLOAT:
	return rep_make_float (ceil (rep_NUMBER (arg,f)), rep_TRUE);
    }
    abort ();
}

DEFUN("truncate", Ftruncate, Struncate, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#truncate::
truncate NUMBER

Round NUMBER to the nearest integer between NUMBER and zero.
::end:: */
{
    rep_DECLARE1 (arg, rep_NUMERICP);
    switch (rep_NUMERIC_TYPE (arg))
    {
	double d;

    case rep_NUMBER_INT:
    case rep_NUMBER_BIGNUM:
	return arg;

    default:
#ifdef HAVE_GMP
        if (rep_NUMBER_RATIONAL_P (arg))
	    d = mpq_get_d (rep_NUMBER(arg,q));
	else
#endif
	    d = rep_NUMBER(arg,f);
	d = (d < 0.0) ? -floor (-d) : floor (d);
#ifdef HAVE_GMP
        if (rep_NUMBER_RATIONAL_P (arg))
	    return rep_make_long_int ((long) d);
	else
#endif
	    return rep_make_float (d, rep_TRUE);
    }
    abort ();
}

DEFUN("round", Fround, Sround, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#round::
round NUMBER

Round NUMBER to the nearest integer. Halfway cases are rounded to the
nearest even integer.
::end:: */
{
    rep_DECLARE1 (arg, rep_NUMERICP);
    switch (rep_NUMERIC_TYPE (arg))
    {
	double d, plus_half, result;

    case rep_NUMBER_INT:
    case rep_NUMBER_BIGNUM:
	return arg;

    default:
#ifdef HAVE_GMP
        if (rep_NUMBER_RATIONAL_P (arg))
	    d = mpq_get_d (rep_NUMBER(arg,q));
	else
#endif
	    d = rep_NUMBER(arg,f);
	/* from guile */
	plus_half = d + 0.5;
	result = floor (plus_half);
	/* Adjust so that the round is towards even.  */
	d = ((plus_half == result && plus_half / 2 != floor (plus_half / 2))
	     ? result - 1 : result);
#ifdef HAVE_GMP
        if (rep_NUMBER_RATIONAL_P (arg))
	    return rep_make_long_int ((long) d);
	else
#endif
	    return rep_make_float (d, rep_TRUE);
    }
    abort ();
}

DEFUN("exp", Fexp, Sexp, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#exp::
exp X

Return `e' (the base of natural logarithms) raised to the power X.
::end:: */
{
    rep_DECLARE1 (arg, rep_NUMERICP);
    return rep_make_float (exp (rep_get_float (arg)), rep_TRUE);
}

DEFUN("log", Flog_, Slog, (repv arg, repv base), rep_Subr2) /*
::doc:rep.lang.math#log::
log X [BASE]

Return the logarithm of X in base BASE. An arithmetic error is
signalled if X is less than zero. If BASE isn't defined, return the
natural logarithm of X.
::end:: */
{
    double d, b;

    rep_DECLARE1 (arg, rep_NUMERICP);
    rep_DECLARE2_OPT (base, rep_NUMERICP);

    d = rep_get_float (arg);

    if (base != Qnil)
    {
	b = rep_get_float (base);
	if (d >= 0 && b >= 0)
	    return rep_make_float (log (d) / log (b), rep_TRUE);
    }
    else
    {
	if (d >= 0)
	    return rep_make_float (log (d), rep_TRUE);
    }

    return Fsignal (Qarith_error, rep_LIST_1 (rep_VAL (&domain_error)));
}

/* XXX compat */
repv Flog (repv x) { return Flog_ (x, Qnil); }

DEFUN("sin", Fsin, Ssin, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#sin::
sin X

Returns the sine of X, in radians.
::end:: */
{
    rep_DECLARE1 (arg, rep_NUMERICP);
    return rep_make_float (sin (rep_get_float (arg)), rep_TRUE);
}

DEFUN("cos", Fcos, Scos, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#cos::
cos X

Returns the cosine of X, in radians.
::end:: */
{
    rep_DECLARE1 (arg, rep_NUMERICP);
    return rep_make_float (cos (rep_get_float (arg)), rep_TRUE);
}

DEFUN("tan", Ftan, Stan, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#tan::
tan X

Returns the tangent of X, in radians.
::end:: */
{
    rep_DECLARE1 (arg, rep_NUMERICP);
    return rep_make_float (tan (rep_get_float (arg)), rep_TRUE);
}

DEFUN("asin", Fasin, Sasin, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#asin::
asin X

Return the arc sine of X (the value whose sine is X), in radians.
::end:: */
{
    double d;
    rep_DECLARE1 (arg, rep_NUMERICP);
    d = rep_get_float (arg);
    if (d >= -1.0 && d <= 1.0)
	return rep_make_float (asin (d), rep_TRUE);
    else
	return Fsignal (Qarith_error, rep_LIST_1 (rep_VAL (&domain_error)));
}

DEFUN("acos", Facos, Sacos, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#acos::
acos X

Return the arc cosine of X (the value whose cosine is X), in radians.
::end:: */
{
    double d;
    rep_DECLARE1 (arg, rep_NUMERICP);
    d = rep_get_float (arg);
    if (d >= -1.0 && d <= 1.0)
	return rep_make_float (acos (d), rep_TRUE);
    else
	return Fsignal (Qarith_error, rep_LIST_1 (rep_VAL (&domain_error)));
}

DEFUN("atan", Fatan, Satan, (repv y, repv x), rep_Subr2) /*
::doc:rep.lang.math#atan::
atan X

Returns the arc tangent of X (the value whose tangent is X), in
radians.

atan Y X

Returns the arc tangent of Y/X, in radians. The signs of both arguments
are used to determine the quadrant of the result, and X is permitted to
be zero.
::end:: */
{
    rep_DECLARE1 (y, rep_NUMERICP);
    if (!rep_NUMERICP (x))
	return rep_make_float (atan (rep_get_float (y)), rep_TRUE);
    else
	return rep_make_float (atan2 (rep_get_float (y),
				      rep_get_float (x)), rep_TRUE);
}

DEFUN("sqrt", Fsqrt, Ssqrt, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#sqrt::
sqrt X

Returns the nonnegative square root of X. If X is negative, signals an
arithmetic error (should return a complex number).
::end:: */
{
    double d;
    rep_DECLARE1 (arg, rep_NUMERICP);
    d = rep_get_float (arg);
    if (d >= 0)
	return rep_make_float (sqrt (d), rep_NUMBER_INEXACT_P (arg));
    else
	return Fsignal (Qarith_error, rep_LIST_1 (rep_VAL (&domain_error)));
}

DEFUN("expt", Fexpt, Sexpt, (repv arg1, repv arg2), rep_Subr2) /*
::doc:rep.lang.math#expt::
expt X Y

Returns X raised to the power Y.

If X is negative and Y is a non-integer, then an arithmetic error is
signalled (mathematically should return a complex number).
::end:: */
{
    repv out;
    rep_DECLARE1 (arg1, rep_NUMERICP);
    rep_DECLARE1 (arg2, rep_NUMERICP);

    if (rep_INTEGERP (arg1) && rep_INTP (arg2))
    {
	if (rep_INTP (arg1))
	{
	    arg1 = promote_to (arg1, rep_NUMBER_BIGNUM);
	    out = arg1;
	}
	else
	    out = dup (arg1);
#ifdef HAVE_GMP
	{
	    int neg = rep_INT (arg2) < 0;
	    mpz_pow_ui (rep_NUMBER(out,z), rep_NUMBER(arg1,z),
			neg ? -rep_INT(arg2) : rep_INT (arg2));
	    if (neg)
		out = rep_number_div (rep_MAKE_INT (1), out);
	}
#else
	{
	    double t = pow (rep_NUMBER (arg1,z), rep_INT (arg2));
	    out = rep_make_float (t, rep_FALSE);
	}
#endif
    }
    else
    {
	double x, y;
	x = rep_get_float (arg1);
	y = rep_get_float (arg2);
	if (x >= 0 || ceil (y) == y)
	{
	    out = rep_make_float (pow (x, y),
				  rep_NUMBER_INEXACT_P (arg1)
				  || rep_NUMBER_INEXACT_P (arg2));
	}
	else
	    out = Fsignal (Qarith_error, rep_LIST_1 (rep_VAL (&domain_error)));
    }
    return out;
}

DEFUN("gcd", Fgcd, Sgcd, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#gcd::
gcd ...

Return the greatest common divisor of the integer arguments. The result
is always non-negative. Returns 0 with arguments.
::end:: */
{
    if (argc == 0)
	return rep_MAKE_INT (0);
    else if (argc == 1)
    {
	rep_DECLARE1 (argv[0], rep_INTEGERP);
	return rep_integer_gcd (argv[0], argv[0]);
    }
    else
	return integer_foldv (argc, argv, rep_integer_gcd);
}

DEFUN("numberp", Fnumberp, Snumberp, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#numberp::
numberp ARG

Return t if ARG is a number.
::end:: */
{
    return rep_NUMERICP (arg) ? Qt : Qnil;
}

DEFUN("integerp", Fintegerp, Sintegerp, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#integerp::
integerp ARG

Return t if ARG is a integer.
::end:: */
{
    if (!rep_NUMERICP (arg))
	return Qnil;
    switch (rep_NUMERIC_TYPE (arg))
    {
    case rep_NUMBER_INT: case rep_NUMBER_BIGNUM:
	return Qt;

    case rep_NUMBER_RATIONAL:
	return Qnil;

    case rep_NUMBER_FLOAT:
	return (floor (rep_NUMBER(arg,f)) == rep_NUMBER(arg,f)) ? Qt : Qnil;

    default:
	abort ();
    }
}

DEFUN("fixnump", Ffixnump, Sfixnump, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#fixnump::
fixnump ARG

Return t if ARG is a fixnum (i.e. an integer that fits in a Lisp
pointer).
::end:: */
{
    return rep_INTP (arg) ? Qt : Qnil;
}

DEFUN("exactp", Fexactp, Sexactp, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#exactp::
exactp ARG

Return t if ARG is an exact number.
::end:: */
{
    return (rep_INTP (arg)
	    || (rep_NUMBERP (arg) && !rep_NUMBER_FLOAT_P (arg))) ? Qt : Qnil;
}

DEFUN("exact->inexact", Fexact_to_inexact,
      Sexact_to_inexact, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#exact->inexact::
exact->inexact X

Returns an inexact (i.e. floating point) representation of X.
::end:: */
{
    rep_DECLARE1(arg, rep_NUMERICP);
    if (!rep_INTP (arg) && rep_NUMBER_FLOAT_P (arg))
	return arg;
    else
	return rep_make_float (rep_get_float (arg), rep_TRUE);
}

static void
rationalize (repv arg, double *numerator, double *denominator)
{
    double x, y;
    int expt;
 
    /* X/Y always equals the input value. Tactic is to iteratively
       multiply both X and Y by 2 until X is an integer. We bound
       the number of iterations to the size of the mantissa
       by starting with the normalized value... */

    x = frexp (rep_get_float (arg), &expt);
    y = pow (2.0, -expt);

    while (x - floor (x) > DBL_EPSILON)
    {
	x = x * 2.0;
	y = y * 2.0;
    }

    if (numerator != NULL)
	*numerator = x;
    if (denominator != NULL)
	*denominator = y;
}

DEFUN("inexact->exact", Finexact_to_exact,
      Sinexact_to_exact, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#inexact->exact::
inexact->exact X

Returns an exact representation of X. This may involve a loss of
accuracy.
::end:: */
{
    rep_DECLARE1(arg, rep_NUMERICP);

    if (rep_INTP (arg) || !rep_NUMBER_FLOAT_P (arg))
	return arg;

#ifdef HAVE_GMP
    else
    {
	double x, y;
	rep_number_z *z;

	rationalize (arg, &x, &y);
	z = make_number (rep_NUMBER_BIGNUM);
	mpz_init_set_d (z->z, (x / y));

	return maybe_demote (rep_VAL (z));
    }
#else
    else
    {
	double x, y;
	rep_number_z *z;

	rationalize (arg, &x, &y);
	z = make_number (rep_NUMBER_BIGNUM);
	z->z = x / y;

	return maybe_demote (rep_VAL (z));
    }
#endif
}

DEFUN("numerator", Fnumerator, Snumerator, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#numerator::
numerator X

Return the numerator of rational number X.
::end:: */
{
    rep_bool inexact = rep_FALSE;
    double x;

    rep_DECLARE1(arg, rep_NUMERICP);

#ifdef HAVE_GMP
    if (rep_NUMBER_RATIONAL_P (arg))
    {
	rep_number_z *z = make_number (rep_NUMBER_BIGNUM);
	mpz_init_set (z->z, mpq_numref (rep_NUMBER(arg,q)));
	return maybe_demote (rep_VAL (z));
    }
#endif

    if (rep_NUMBER_INEXACT_P (arg))
	inexact = rep_TRUE;

    rationalize (arg, &x, NULL);

    return rep_make_float (x, inexact);
}

DEFUN("denominator", Fdenominator, Sdenominator, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#denominator::
denominator X

Return the denominator of rational number X.
::end:: */
{
    rep_bool inexact = rep_FALSE;
    double y;

    rep_DECLARE1(arg, rep_NUMERICP);

#ifdef HAVE_GMP
    if (rep_NUMBER_RATIONAL_P (arg))
    {
	rep_number_z *z = make_number (rep_NUMBER_BIGNUM);
	mpz_init_set (z->z, mpq_denref (rep_NUMBER(arg,q)));
	return maybe_demote (rep_VAL (z));
    }
#endif

    if (rep_NUMBER_INEXACT_P (arg))
	inexact = rep_TRUE;

    rationalize (arg, NULL, &y);

    return rep_make_float (y, inexact);
}

DEFUN("max", Fmax, Smax, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#max::
max ARGS...

Returns the greatest of its arguments. There must be at least two
arguments. When comparing numbers, any inexact arguments cause the
result to be inexact.
::end:: */
{
    return foldv (argc, argv, rep_number_max);
}

DEFUN("min", Fmin, Smin, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#min::
min ARGS...

Returns the smallest of its arguments. There must be at least two
arguments. When comparing numbers, any inexact arguments cause the
result to be inexact.
::end:: */
{
    return foldv (argc, argv, rep_number_min);
}

DEFUN("string->number", Fstring_to_number,
      Sstring_to_number, (repv string, repv radix_), rep_Subr2) /*
::doc:rep.lang.math#string->number::
string->number STRING [RADIX]

Return the number represented by STRING. If RADIX is specified, the
number is parsed from that base, otherwise base 10 is assumed.
::end:: */
{
    int type = 0;
    int sign = 1;
    int force_exactness = 0;
    int radix;
    char *ptr;
    repv ret;

    rep_DECLARE1 (string, rep_STRINGP);
    if (radix_ == Qnil)
	radix_ = rep_MAKE_INT (10);
    rep_DECLARE (2, radix_, rep_INTP (radix_) && rep_INT (radix_) > 0);

    ptr = rep_STR (string);
    radix = rep_INT (radix_);

    while (*ptr == '#')
    {
	switch (ptr[1])
	{
	case 'b': case 'B':
	    radix = 2;
	    break;

	case 'o': case 'O':
	    radix = 8;
	    break;

	case 'd': case 'D':
	    radix = 10;
	    break;

	case 'x': case 'X':
	    radix = 16;
	    break;

	case 'e': case 'E':
	    force_exactness = +1;
	    break;

	case 'i': case 'I':
	    force_exactness = -1;
	    break;

	default:
	    return Qnil;
	}
	ptr += 2;
    }

    if (*ptr == '-' || *ptr == '+')
    {
	if (*ptr == '-')
	    sign = -1;
	ptr++;
    }

    if (strchr (ptr, '/'))
	type = rep_NUMBER_RATIONAL;
    else if (radix == 10)
    {
	if (strchr (ptr, '.') || strchr (ptr, 'e') || strchr (ptr, 'E'))
	    type = rep_NUMBER_FLOAT;
    }

    ret = rep_parse_number (ptr, rep_STRING_LEN (string)
			    - (ptr - rep_STR (string)), radix, sign, type);
    if (ret == rep_NULL)
	ret = Qnil;
    else if (force_exactness > 0)
	ret = Finexact_to_exact (ret);
    else if (force_exactness < 0)
	ret = Fexact_to_inexact (ret);

    return ret;
}

DEFUN("number->string", Fnumber_to_string,
      Snumber_to_string, (repv z, repv radix), rep_Subr2) /*
::doc:rep.lang.math#number->string::
number->string Z [RADIX]

Return a string containing a printed representation of the number Z. If
RADIX is specified, print the number in that base, otherwise print it
in base 10.
::end:: */
{
    char *out;
    rep_DECLARE1 (z, rep_NUMERICP);
    if (radix == Qnil)
	radix = rep_MAKE_INT (10);
    rep_DECLARE (2, radix, rep_INTP (radix) && rep_INT (radix) > 0);

    out = rep_print_number_to_string (z, rep_INT (radix), -1);
    if (out == 0)
	return Qnil;
    else
	return rep_box_string (out, strlen (out));
}


/* Random number generation */

#if defined (HAVE_GMP) && defined (HAVE_GMP_RANDINIT) && __GNU_MP__ >= 4

static gmp_randstate_t random_state;

static void
ensure_random_state (void)
{
    static rep_bool initialized;

    if (!initialized)
    {
	/* Generate the best random numbers up to 128 bits, the
	   maximum allowed by gmp */
	gmp_randinit (random_state, GMP_RAND_ALG_DEFAULT, 128);

	/* Initialize to a known seed */
	gmp_randseed_ui (random_state, 0);

	initialized = rep_TRUE;
    }
}

static void
random_seed (unsigned long seed)
{
    ensure_random_state ();
    gmp_randseed_ui (random_state, seed);
}

static repv
random_new (repv limit_)
{
    rep_number_z *z = make_number (rep_NUMBER_BIGNUM);
    repv limit = promote_to (limit_, rep_NUMBER_BIGNUM);

    ensure_random_state ();
    mpz_init (z->z);
    mpz_urandomm (z->z, random_state, rep_NUMBER (limit, z));

    return maybe_demote (rep_VAL (z));
}

#else /* HAVE_GMP */

/* Try to work out how many bits of randomness rand() will give.. */
#ifdef HAVE_LRAND48
# define RAND_BITS 31
# define rand lrand48
# define srand srand48
#else
# if RAND_MAX == 32768
#  define RAND_BITS 15
# elif RAND_MAX == 2147483647
#  define RAND_BITS 31
# else
#  define RAND_BITS 63
# endif
#endif

static void
random_seed (unsigned long seed)
{
    srand (seed);
}

static repv
random_new (repv limit_)
{
    long limit = rep_get_long_int (limit_);
    long divisor, val;

    if (limit <= 0 || limit > rep_LISP_MAX_INT)
	return rep_signal_arg_error (limit_, 1);

    divisor = rep_LISP_MAX_INT / limit;
    do {
	val = rand ();
	if (rep_LISP_INT_BITS-1 > RAND_BITS)
	{
	    val = (val << RAND_BITS) | rand ();
	    if (rep_LISP_INT_BITS-1 > 2*RAND_BITS)
	    {
		val = (val << RAND_BITS) | rand ();
		if (rep_LISP_INT_BITS-1 > 3*RAND_BITS)
		{
		    val = (val << RAND_BITS) | rand ();
		    if (rep_LISP_INT_BITS-1 > 4*RAND_BITS)
			val = (val << RAND_BITS) | rand ();
		}
	    }
	}
	/* Ensure VAL is positive (assumes twos-complement) */
	val &= ~(~rep_VALUE_CONST(0) << (rep_LISP_INT_BITS - 1));
	val /= divisor;
    } while (val >= limit);

    return rep_make_long_int (val);
}

#endif /* !HAVE_GMP */

DEFUN("random", Frandom, Srandom, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#random::
random [LIMIT]

Produce a pseudo-random number between zero and LIMIT (or the largest
positive integer representable). If LIMIT is the symbol `t' the
generator is seeded with the current time of day.
::end:: */
{
    repv limit;

    if (arg == Qt)
    {
	unsigned long seed = time (0);
	seed = (seed << 8) | (rep_getpid () & 0xff);
	random_seed (seed);
	return Qt;
    }

    rep_DECLARE1_OPT (arg, rep_INTEGERP);
    if (rep_INTEGERP (arg))
	limit = arg;
    else
	limit = rep_MAKE_INT (rep_LISP_MAX_INT);

    if (rep_compare_numbers (limit, rep_MAKE_INT (0)) <= 0)
	return rep_signal_arg_error (limit, 1);

    return random_new (limit);
}


/* init */

void
rep_numbers_init (void)
{
    int i;
    repv tem;

    rep_register_type(rep_Int, "integer", number_cmp,
		      number_prin, number_prin,
		      0, 0, 0, 0, 0, 0, 0, 0, 0);
    rep_register_type(rep_Number, "number", number_cmp,
		      number_prin, number_prin,
		      number_sweep, 0, 0, 0, 0, 0, 0, 0, 0);

    number_sizeofs[0] = sizeof (rep_number_z);
    number_sizeofs[1] = sizeof (rep_number_q);
    number_sizeofs[2] = sizeof (rep_number_f);
    for (i = 0; i < 3; i++)
    {
	number_allocations[i] = ((2040 - sizeof (rep_number_block))
				 / number_sizeofs[i]);
    }

    tem = rep_push_structure ("rep.lang.math");
    rep_ADD_SUBR(Splus);
    rep_ADD_SUBR(Sminus);
    rep_ADD_SUBR(Sproduct);
    rep_ADD_SUBR(Sdivide);
    rep_ADD_SUBR(Sremainder);
    rep_ADD_SUBR(Smod);
    rep_ADD_SUBR(Squotient);
    rep_ADD_SUBR(Slognot);
    rep_ADD_SUBR(Slogior);
    rep_ADD_SUBR(Slogxor);
    rep_ADD_SUBR(Slogand);
    rep_ADD_SUBR(Szerop);
    rep_ADD_SUBR(Splus1);
    rep_ADD_SUBR(Ssub1);
    rep_ADD_SUBR(Sash);
    rep_ADD_SUBR(Sfloor);
    rep_ADD_SUBR(Sceiling);
    rep_ADD_SUBR(Struncate);
    rep_ADD_SUBR(Sround);
    rep_ADD_SUBR(Sexp);
    rep_ADD_SUBR(Slog);
    rep_ADD_SUBR(Ssin);
    rep_ADD_SUBR(Scos);
    rep_ADD_SUBR(Stan);
    rep_ADD_SUBR(Sasin);
    rep_ADD_SUBR(Sacos);
    rep_ADD_SUBR(Satan);
    rep_ADD_SUBR(Ssqrt);
    rep_ADD_SUBR(Sexpt);
    rep_ADD_SUBR(Sgcd);
    rep_ADD_SUBR(Snumberp);
    rep_ADD_SUBR(Sintegerp);
    rep_ADD_SUBR(Sfixnump);
    rep_ADD_SUBR(Sexactp);
    rep_ADD_SUBR(Sexact_to_inexact);
    rep_ADD_SUBR(Sinexact_to_exact);
    rep_ADD_SUBR(Snumerator);
    rep_ADD_SUBR(Sdenominator);
    rep_ADD_SUBR(Smax);
    rep_ADD_SUBR(Smin);
    rep_ADD_SUBR(Sstring_to_number);
    rep_ADD_SUBR(Snumber_to_string);
    rep_ADD_SUBR(Srandom);
    rep_pop_structure (tem);

    tem = rep_push_structure ("rep.data");
    rep_ADD_SUBR(Seql);
    rep_pop_structure (tem);
}
