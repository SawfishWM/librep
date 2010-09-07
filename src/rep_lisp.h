/* rep_lisp.h -- Data structures/objects for Lisp
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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* library-private definitions are in repint.h */

#ifndef REP_LISP_H
#define REP_LISP_H

#include <stdio.h>

/* Stringify X. Expands macros in X. */
#define rep_QUOTE(x) rep_QUOTE__(x)
#define rep_QUOTE__(x) #x

/* Concat two tokens. Expands macros in X and Y. */
#define rep_CONCAT(x, y) rep_CONCAT__(x, y)
#define rep_CONCAT__(x, y) x##y


/* Lisp values. */

/* A `repv' is a lisp value, perhaps a pointer to an object, but not a real
   pointer; it's two lowest bits define its type. */
typedef unsigned rep_PTR_SIZED_INT repv;

/* The number of bits in the lisp value type. */
#define rep_VALUE_BITS rep_PTR_SIZED_INT_BITS

/* Get the integer constant X in the lisp value type */
#define rep_VALUE_CONST(x) rep_CONCAT(x, rep_PTR_SIZED_INT_SUFFIX)


/* Structure of Lisp objects and the pointers to them. */

/* Bit definitions for repv pointers. The lowest bit is always zero
   except during GC. If bit one is set the object is a 30-bit signed
   integer, with the data bits stored in the pointer as bits 2->31.

   Otherwise (i.e. bit 1 of the pointer is clear), the value is a
   pointer to a "cell"; all objects other than integers are represented
   by various types of cells. Every cell has a repv as its first
   element (called the car), the lowest bits of this define the actual
   type of the cell.

   If bit zero of the car is unset, the cell is a cons, a pair of two
   values the car and the cdr (the GC mark bit of the cons is bit zero
   of the cdr).

   If bit zero of the car is set, then further type information is
   stored in bits 1->5 of the car, with bit 5 used to denote statically
   allocated objects and bit 7 the mark bit.

   So there are 2^4 possible types of cells. This isn't enough, so bit
   6 of the car is used to denote a ``cell16'' type -- a cell in which
   bits 8->15 give the actual type. These cell16 types are allocated
   dynamically.

   Note that some assumptions are made about data object alignment. All
   Lisp cells _must_ be aligned to four-byte boundaries. If using GNU
   CC, we'll use the alignment attribute. Otherwise the rep_ALIGN macro
   needs setting.. */

#define rep_VALUE_CONS_MARK_BIT	1
#define rep_VALUE_IS_INT	2
#define rep_VALUE_INT_SHIFT	2
#define rep_CELL_ALIGNMENT	rep_PTR_SIZED_INT_SIZEOF

#if rep_CELL_ALIGNMENT <= rep_MALLOC_ALIGNMENT
  /* Allocate SIZE bytes of memory, aligned to NORMAL_ALIGNMENT */
# define rep_ALLOC_CELL(n) rep_alloc(n)
  /* Free something allocated by rep_ALLOC_CELL */
# define rep_FREE_CELL(x)  rep_free(x)
#else
# error "Need an aligned memory allocator"
#endif

/* A ``null pointer'', i.e. an invalid object. This has the important
   property of being a proper null pointer (i.e. (void *)0) when
   converted to a pointer, i.e. rep_PTR(rep_NULL) == NULL. */
#define rep_NULL	(0)

/* Align the variable or struct member D to the necessary cell alignment.
   This is used like: ``rep_ALIGN_CELL(rep_cell foo) = ...'' */
#ifdef __GNUC__
# define rep_ALIGN_CELL(d) d __attribute__ ((aligned (rep_CELL_ALIGNMENT)))
#elif defined (__digital__) && defined (__unix__) && defined (__DECC)
# if rep_CELL_ALIGNMENT >= rep_PTR_SIZED_INT_SIZEOF
   /* "the C compiler aligns an int (32 bits) on a 4-byte boundary and
      a long (64 bits) on an 8-byte boundary" (Tru64 Programmer's Guide) */
#  define rep_ALIGN_CELL(d) d
# else
#  error "You need to fix alignment for Tru64"
# endif
#else
/* # warning Lets hope your compiler aligns to 4 byte boundaries.. */
# define rep_ALIGN_CELL(d) d
#endif

/* Is repv V a cell type? */
#define rep_CELLP(v)		(((v) & rep_VALUE_IS_INT) == 0)

/* Is repv V a fixnum? */
#define rep_INTP(v)		(!rep_CELLP(v))

/* Convert a repv into a signed integer. */
#define rep_INT(v)		(((rep_PTR_SIZED_INT)(v)) \
				 >> rep_VALUE_INT_SHIFT)

/* Convert a signed integer into a repv. */
#define rep_MAKE_INT(x)		(((x) << rep_VALUE_INT_SHIFT) \
				 | rep_VALUE_IS_INT)

/* Bounds of the integer type */
#define rep_LISP_INT_BITS	(rep_VALUE_BITS - rep_VALUE_INT_SHIFT)
#define rep_LISP_MAX_INT	((rep_VALUE_CONST(1) \
				  << (rep_LISP_INT_BITS - 1)) - 1)
#define rep_LISP_MIN_INT	(-(rep_VALUE_CONST(1) \
				   << (rep_LISP_INT_BITS - 1)))

/* backwards compatibility */
#define rep_MAKE_LONG_INT(x) rep_make_long_int(x)
#define rep_LONG_INT(v) rep_get_long_int(v)
#define rep_LONG_INTP(v) 						\
    (rep_INTEGERP(v)							\
     || (rep_CONSP(v) && rep_INTP(rep_CAR(v)) && rep_INTP(rep_CDR(v))))


/* Structure of a cell */

typedef struct {
    /* Low bits of this value define type of the cell. See below. All
       other bits (8->31) are available */
    repv car;

    /* Data follows, in real objects. */
} rep_cell;

/* If bit zero is set in the car of a cell, bits 1->4 of the car are
   type data, bit 5 denotes a cell16 type, bit 6 is set if the object
   is allocated statically, bit 7 is the GC mark bit. This means a
   maximum of 2^3, i.e. 16, cell8 types.

   cell16 types have eight extra type bits, bits 8->15, this gives 256
   dynamically allocated type codes: [256 k + 0x21 | k <- [0..255]]. */

#define rep_CELL_IS_8		0x01
#define rep_CELL_IS_16		0x20
#define rep_CELL_STATIC_BIT	0x40
#define rep_CELL_MARK_BIT	0x80
#define rep_CELL8_TYPE_MASK	0x3f
#define rep_CELL8_TYPE_BITS	8
#define rep_CELL16_TYPE_MASK	0xff21	/* is8 and is16 bits set */
#define rep_CELL16_TYPE_SHIFT	8
#define rep_CELL16_TYPE_BITS	16

/* Build a `rep_cell *' pointer out of a repv of a normal type */
#define rep_PTR(v) 		((rep_cell *)(v))

/* Build a repv out of a pointer to a Lisp_Normal object */
#define rep_VAL(x)		((repv)(x))

/* Is V of cell8 type? */
#define rep_CELL8P(v)		(rep_PTR(v)->car & rep_CELL_IS_8)

/* Is V a cons? */
#define rep_CELL_CONS_P(v)	(!rep_CELL8P(v))

/* Is V statically allocated? */
#define rep_CELL_STATIC_P(v)	(rep_PTR(v)->car & rep_CELL_STATIC_BIT)

/* Is V not an integer or cons? */
#define rep_CELL8_TYPE(v) 	(rep_PTR(v)->car & rep_CELL8_TYPE_MASK)

/* Get the actual cell8 type of V to T */
#define rep_SET_CELL8_TYPE(v, t) \
   (rep_PTR(v)->car = (rep_PTR(v)->car & rep_CELL8_TYPE_MASK) | (t))

/* Is V of cell16 type? */
#define rep_CELL16P(v)		(rep_PTR(v)->car & rep_CELL_IS_16)

/* Get the actual cell16 type of V */
#define rep_CELL16_TYPE(v)	(rep_PTR(v)->car & rep_CELL16_TYPE_MASK)

/* Set the actual cell16 type of V to T */
#define rep_SET_CELL16_TYPE(v, t) \
   (rep_PTR(v)->car = (rep_PTR(v)->car & rep_CELL16_TYPE_MASK) | (t))


/* Structure of a cons cell, the only non-cell8 ptr type */

typedef struct {
    repv car;
    repv cdr;				/* low bit is GC mark */
} rep_cons;

#define rep_CONSP(v)	(rep_CELLP(v) && rep_CELL_CONS_P(v))

/* Build a repv out of a pointer to a rep_cons object */
#define rep_CONS_VAL(x)	rep_VAL(x)

/* Get a pointer to a cons cell from a repv. */
#define rep_CONS(v)	((rep_cons *) rep_PTR(v))

/* Get the car or cdr from a cons repv. */
#define rep_CAR(v)	(rep_CONS(v)->car)
#define rep_CDR(v)	(rep_CONS(v)->cdr)
#define rep_CDRLOC(v)	(&(rep_CONS(v)->cdr))

/* Get the cdr when GC is in progress. */
#define rep_GCDR(v)	(rep_CDR(v) & ~rep_VALUE_CONS_MARK_BIT)

/* True if cons cell V is mutable (i.e. not read-only). */
#define rep_CONS_WRITABLE_P(v) \
    (! (rep_CONS(v) >= rep_dumped_cons_start \
	&& rep_CONS(v) < rep_dumped_cons_end))


/* Type data */

/* Information about each type */
typedef struct rep_type_struct {
    struct rep_type_struct *next;
    char *name;
    unsigned int code;

    /* Compares two values, rc is similar to strcmp() */
    int (*compare)(repv val1, repv val2);

    /* Prints a textual representation of the object, not necessarily in 
       a read'able format */
    void (*princ)(repv stream, repv obj);

    /* Prints a textual representation of the object, if possible in
       a read'able format */
    void (*print)(repv stream, repv obj);

    /* When non-null, a function that should be called during the
       sweep phase of garbage collection. */
    void (*sweep)(void);

    /* When non-null, a function to mark OBJ and all objects
       it references. */
    void (*mark)(repv obj);

    /* When called, should mark any objects that must persist across
       the GC, no matter what. */
    void (*mark_type)(void);

    /* When non-null, functions called for the stream OBJ. */
    int (*getc)(repv obj);
    int (*ungetc)(repv obj, int c);
    int (*putc)(repv obj, int c);
    int (*puts)(repv obj, void *data, int length, rep_bool lisp_obj_p);

    /* When non-null, a function to ``bind'' to OBJ temporarily,
       returning some handle for later unbinding. */
    repv (*bind)(repv obj);

    /* When non-null, a function to ``unbind'' OBJ, the result of
       the earlier bind call. */
    void (*unbind)(repv obj);
} rep_type;

/* Each type of Lisp object has a type code associated with it.

   Note how non-cons cells are given odd values, so that the
   rep_CELL_IS_8 bit doesn't have to be masked out. */

#define rep_Cons	0x00		/* made up */
#define rep_Symbol	0x01
#define rep_Int		0x02		/* made up */
#define rep_Vector	0x03
#define rep_String	0x05
#define rep_Compiled	0x07
#define rep_Void	0x09
#define rep_Reserved	0x0b
#define rep_Number	0x0d
#define rep_SF		0x0f
#define rep_Subr0	0x11
#define rep_Subr1	0x13
#define rep_Subr2	0x15
#define rep_Subr3	0x17
#define rep_Subr4	0x19
#define rep_Subr5	0x1b
#define rep_SubrN	0x1d
#define rep_Funarg	0x1f

/* Assuming that V is a cell, return the type code */
#define rep_CELL_TYPE(v) (rep_CONSP(v) ? rep_Cons		\
			  : !rep_CELL16P(v) ? rep_CELL8_TYPE(v)	\
			  : rep_CELL16_TYPE(v))

/* Return a type code given a repv */
#define rep_TYPE(v)	(rep_INTP(v) ? rep_Int : rep_CELL_TYPE(v))

/* true if V is of type T (T must be a cell8 type) */
#define rep_CELL8_TYPEP(v, t) \
    (rep_CELLP(v) && rep_CELL8_TYPE(v) == (t))

#define rep_CELL16_TYPEP(v, t) \
    (rep_CELLP(v) && rep_CELL16_TYPE(v) == (t))

/* true if V is of type T. */
#define rep_TYPEP(v, t)	(rep_TYPE(v) == t)


/* tuples, cells containing two values */

typedef struct {
    repv car;
    repv a, b;
} rep_tuple;

#define rep_TUPLE(v)		((rep_tuple *) rep_PTR (v))


/* Numbers (private defs in numbers.c) */

/* Is V a non-fixnum number? */
#define rep_NUMBERP(v)		rep_CELL8_TYPEP(v, rep_Number)

/* Is V numeric? */
#define rep_NUMERICP(v)		(rep_INTP(v) || rep_NUMBERP(v))

/* bits 8-9 of car define number type (except when on freelist) */
typedef rep_cell rep_number;

/* these are in order of promotion */
#define rep_NUMBER_INT		0	/* faked */
#define rep_NUMBER_BIGNUM	0x100
#define rep_NUMBER_RATIONAL	0x200
#define rep_NUMBER_FLOAT	0x400

#define rep_NUMBER_TYPE(v)	(((rep_number *)rep_PTR(v))->car & 0x700)
#define rep_NUMBER_BIGNUM_P(v)	(rep_NUMBER_TYPE(v) & rep_NUMBER_BIGNUM)
#define rep_NUMBER_RATIONAL_P(v) (rep_NUMBER_TYPE(v) & rep_NUMBER_RATIONAL)
#define rep_NUMBER_FLOAT_P(v)	(rep_NUMBER_TYPE(v) & rep_NUMBER_FLOAT)

#define rep_NUMERIC_TYPE(v) \
    (rep_INTP(v) ? rep_NUMBER_INT : rep_NUMBER_TYPE(v))

#define rep_INTEGERP(v) \
    (rep_INTP(v) || (rep_NUMBERP(v) && rep_NUMBER_BIGNUM_P(v)))


/* Strings */

typedef struct rep_string_struct {
    /* Bits 0->7 are standard cell8 defines. Bits 8->31 store the length
       of the string. This means that strings can't contain more than
       2^24-1 bytes (thats about 16.7MB) */
    repv car;

    /* Pointer to the (zero-terminated) characters */
    char *data;
} rep_string;

#define rep_STRING_LEN_SHIFT	8
#define rep_MAX_STRING \
    ((rep_VALUE_CONST(1) << (rep_VALUE_BITS - rep_STRING_LEN_SHIFT)) - 1)

#define rep_STRINGP(v)		rep_CELL8_TYPEP(v, rep_String)
#define rep_STRING(v)		((rep_string *) rep_PTR(v))

#define rep_STRING_LEN(v)	(rep_STRING(v)->car >> rep_STRING_LEN_SHIFT)

#define rep_MAKE_STRING_CAR(len) (((len) << rep_STRING_LEN_SHIFT) | rep_String)

/* True if this string may be written to; generally static strings
   are made from C string-constants and usually in read-only storage. */
#define rep_STRING_WRITABLE_P(s) (!rep_CELL_STATIC_P(s))

/* Define a variable V, containing a static string S. This must be cast
   to a repv via the rep_VAL() macro when using. */
#define DEFSTRING(v, s)					\
    rep_ALIGN_CELL(static const rep_string v) = {	\
	((sizeof(s) - 1) << rep_STRING_LEN_SHIFT)	\
	| rep_CELL_STATIC_BIT | rep_String,		\
	(char *)s					\
    }

#define rep_STR(v)	(rep_STRING(v)->data)

/* Use this to get a newline into a DEFSTRING */
#define rep_DS_NL "\n"


/* Symbols */

/* symbol object, actual allocated as a tuple */
typedef struct {
    repv car;				/* bits 8->11 are flags */
    repv next;				/* next symbol in rep_obarray bucket */
    repv name;
} rep_symbol;

#define rep_SF_KEYWORD	(1 << (rep_CELL8_TYPE_BITS + 0))

/* Means that the symbol's value may be in some form of local storage,
   if so then that occurrence takes precedence. */
#define rep_SF_LOCAL 	(1 << (rep_CELL8_TYPE_BITS + 1))

/* This means that setting the value of the symbol always sets the
   local value, even if one doesn't already exist.  */
#define rep_SF_SET_LOCAL (1 << (rep_CELL8_TYPE_BITS + 2))

/* When a function is evaluated whose symbol has this bit set, the
   next evaluated form will invoke the Lisp debugger. */
#define rep_SF_DEBUG	(1 << (rep_CELL8_TYPE_BITS + 3))

/* Dynamically bound */
#define rep_SF_SPECIAL	(1 << (rep_CELL8_TYPE_BITS + 4))

/* A special, but was first set from an environment in which specials
   can't normally be accessed; if the symbol is later defvar'd its
   original value will be overwritten. */
#define rep_SF_WEAK	(1 << (rep_CELL8_TYPE_BITS + 5))

/* A variable that was weak, but has been modified via defvar from an
   unrestricted special environment */
#define rep_SF_WEAK_MOD	(1 << (rep_CELL8_TYPE_BITS + 6))

/* Set when the variable has been defvar'd */
#define rep_SF_DEFVAR	(1 << (rep_CELL8_TYPE_BITS + 7))

#define rep_SF_LITERAL	(1 << (rep_CELL8_TYPE_BITS + 8))

#define rep_SYM(v)		((rep_symbol *)rep_PTR(v))
#define rep_SYMBOLP(v)		rep_CELL8_TYPEP(v, rep_Symbol)

#define rep_NILP(v)		((v) == Qnil)
#define rep_LISTP(v)		(rep_NILP(v) || rep_CONSP(v))

#define rep_KEYWORDP(v)		(rep_SYMBOLP(v) \
				 && (rep_SYM(v)->car & rep_SF_KEYWORD) != 0)

#define rep_SYMBOL_LITERAL_P(v)	((rep_SYM(v)->car & rep_SF_LITERAL) != 0)


/* Vectors */

typedef struct rep_vector_struct {
    repv car;				/* size is bits 8->31 */
    struct rep_vector_struct *next;
    repv array[1];
} rep_vector;

/* Bytes to allocate for S objects */
#define rep_VECT_SIZE(s)	((sizeof(repv) * ((s)-1)) + sizeof(rep_vector))

#define rep_VECT(v)		((rep_vector *)rep_PTR(v))
#define rep_VECTI(v,i)		(rep_VECT(v)->array[(i)])

#define rep_VECT_LEN(v)		(rep_VECT(v)->car >> 8)
#define rep_SET_VECT_LEN(v,l)	(rep_VECT(v)->car = ((l) << 8 | rep_Vector))

#define rep_VECTORP(v)		rep_CELL8_TYPEP(v, rep_Vector)

#define rep_VECTOR_WRITABLE_P(v) (!rep_CELL_STATIC_P(v))


/* Compiled Lisp functions; this is a vector. Some of these definitions
   are probably hard coded into lispmach.c */

#define rep_COMPILEDP(v)	rep_CELL8_TYPEP(v, rep_Compiled)
#define rep_COMPILED(v)		((rep_vector *)rep_PTR(v))

/* First elt is byte-code string */
#define rep_COMPILED_CODE(v)	rep_VECTI(v, 0)

/* Second is constant vector */
#define rep_COMPILED_CONSTANTS(v) rep_VECTI(v, 1)

/* Third is an (opaque) integer: memory requirements */
#define rep_COMPILED_STACK(v)	rep_VECTI(v, 2)

#define rep_COMPILED_MIN_SLOTS	3

/* Optional fifth element is documentation. */
#define rep_COMPILED_DOC(v)	((rep_VECT_LEN(v) >= 4) \
				 ? rep_VECTI(v, 3) : Qnil)

/* Optional sixth element is interactive specification. */
#define rep_COMPILED_INTERACTIVE(v) ((rep_VECT_LEN(v) >= 5) \
				     ? rep_VECTI(v, 4) : Qnil)


/* Files */

/* A file object.  */
typedef struct rep_file_struct {
    repv car;				/* single flag at bit 16 */
    struct rep_file_struct *next;

    /* Name as user sees it */
    repv name;

    /* Function to call to handle file operations,
       or t for file in local fs */
    repv handler;

    /* Data for handler's use; for local files, this is the
       name of the file opened in the local fs. */
    repv handler_data;

    /* For local files, a buffered file handle; for others some sort
       of stream. */
    union {
	FILE *fh;
	repv stream;
    } file;

    /* For input streams */
    int line_number;
} rep_file;

/* When this bit is set in flags, the file handle is never fclose()'d,
   i.e. this file points to something like stdin. */
#define rep_LFF_DONT_CLOSE	(1 << (rep_CELL16_TYPE_BITS + 0))
#define rep_LFF_BOGUS_LINE_NUMBER (1 << (rep_CELL16_TYPE_BITS + 1))
#define rep_LFF_SILENT_ERRORS	(1 << (rep_CELL16_TYPE_BITS + 2))

#define rep_FILE(v)		((rep_file *)rep_PTR(v))
#define rep_FILEP(v)		rep_CELL16_TYPEP(v, rep_file_type)

#define rep_LOCAL_FILE_P(v)	(rep_FILE(v)->handler == Qt)


/* Built-in subroutines */

/* Calling conventions are straightforward, returned value is result
   of function. But returning rep_NULL signifies some kind of abnormal
   exit (i.e. an error or throw, or ..?), should be treated as
   rep_INTERRUPTP defined below is */

/* C subroutine, can take from zero to five arguments.  */
typedef struct {
    repv car;
    union {
	repv (*fun0)(void);
	repv (*fun1)(repv);
	repv (*fun2)(repv, repv);
	repv (*fun3)(repv, repv, repv);
	repv (*fun4)(repv, repv, repv, repv);
	repv (*fun5)(repv, repv, repv, repv, repv);
	repv (*funv)(int, repv *);
    } fun;
    repv name;
    repv int_spec;
} rep_subr;

typedef struct {
    repv car;
    repv (*fun)();
    repv name;
    repv int_spec;			/* put this in plist? */
} rep_xsubr;

/* If set in rep_SubrN types, it'll be passed a vector of args,
   instead of a list */
#define rep_SUBR_VEC      (1 << (rep_CELL8_TYPE_BITS + 0))
#define rep_SUBR_VEC_P(v) (rep_SUBR(v)->car & rep_SUBR_VEC)
#define rep_SubrV         (rep_SubrN | rep_SUBR_VEC)

#define rep_XSUBR(v)	((rep_xsubr *) rep_PTR(v))
#define rep_SUBR(v)	((rep_subr *) rep_PTR(v))
#define rep_SUBR0FUN(v)	(rep_SUBR(v)->fun.fun0)
#define rep_SUBR1FUN(v)	(rep_SUBR(v)->fun.fun1)
#define rep_SUBR2FUN(v)	(rep_SUBR(v)->fun.fun2)
#define rep_SUBR3FUN(v)	(rep_SUBR(v)->fun.fun3)
#define rep_SUBR4FUN(v)	(rep_SUBR(v)->fun.fun4)
#define rep_SUBR5FUN(v)	(rep_SUBR(v)->fun.fun5)
#define rep_SUBRNFUN(v)	(rep_SUBR(v)->fun.fun1)
#define rep_SUBRVFUN(v)	(rep_SUBR(v)->fun.funv)
#define rep_SFFUN(v)	(rep_SUBR(v)->fun.fun2)


/* Closures */

typedef struct rep_funarg_struct {
    repv car;
    repv fun;
    repv name;
    repv env;
    repv structure;
} rep_funarg;

#define rep_FUNARG(v) ((rep_funarg *)rep_PTR(v))
#define rep_FUNARGP(v) (rep_CELL8_TYPEP(v, rep_Funarg))

#define rep_FUNARG_WRITABLE_P(v) (!rep_CELL_STATIC_P(v))


/* Guardians */

#define rep_GUARDIAN(v)		((rep_guardian *) rep_PTR(v))
#define rep_GUARDIANP(v)	rep_CELL16_TYPEP(v, rep_guardian_type)


/* Other definitions */

/* Macros for other types */
#define rep_VOIDP(v)	rep_CELL8_TYPEP(v, rep_Void)

/* Building lists */
#define rep_LIST_1(v1)			Fcons(v1, Qnil)
#define rep_LIST_2(v1,v2)		Fcons(v1, rep_LIST_1(v2))
#define rep_LIST_3(v1,v2,v3)		Fcons(v1, rep_LIST_2(v2, v3))
#define rep_LIST_4(v1,v2,v3,v4)		Fcons(v1, rep_LIST_3(v2, v3, v4))
#define rep_LIST_5(v1,v2,v3,v4,v5)	Fcons(v1, rep_LIST_4(v2, v3, v4, v5))

#define rep_CAAR(obj)           rep_CAR (rep_CAR (obj))
#define rep_CDAR(obj)           rep_CDR (rep_CAR (obj))
#define rep_CADR(obj)           rep_CAR (rep_CDR (obj))
#define rep_CDDR(obj)           rep_CDR (rep_CDR (obj))

#define rep_CAAAR(obj)          rep_CAR (rep_CAR (rep_CAR (obj)))
#define rep_CDAAR(obj)          rep_CDR (rep_CAR (rep_CAR (obj)))
#define rep_CADAR(obj)          rep_CAR (rep_CDR (rep_CAR (obj)))
#define rep_CDDAR(obj)          rep_CDR (rep_CDR (rep_CAR (obj)))
#define rep_CAADR(obj)          rep_CAR (rep_CAR (rep_CDR (obj)))
#define rep_CDADR(obj)          rep_CDR (rep_CAR (rep_CDR (obj)))
#define rep_CADDR(obj)          rep_CAR (rep_CDR (rep_CDR (obj)))
#define rep_CDDDR(obj)          rep_CDR (rep_CDR (rep_CDR (obj)))

#define rep_CAAAAR(obj)         rep_CAR (rep_CAR (rep_CAR (rep_CAR (obj))))
#define rep_CDAAAR(obj)         rep_CDR (rep_CAR (rep_CAR (rep_CAR (obj))))
#define rep_CADAAR(obj)         rep_CAR (rep_CDR (rep_CAR (rep_CAR (obj))))
#define rep_CDDAAR(obj)         rep_CDR (rep_CDR (rep_CAR (rep_CAR (obj))))
#define rep_CAADAR(obj)         rep_CAR (rep_CAR (rep_CDR (rep_CAR (obj))))
#define rep_CDADAR(obj)         rep_CDR (rep_CAR (rep_CDR (rep_CAR (obj))))
#define rep_CADDAR(obj)         rep_CAR (rep_CDR (rep_CDR (rep_CAR (obj))))
#define rep_CDDDAR(obj)         rep_CDR (rep_CDR (rep_CDR (rep_CAR (obj))))
#define rep_CAAADR(obj)         rep_CAR (rep_CAR (rep_CAR (rep_CDR (obj))))
#define rep_CDAADR(obj)         rep_CDR (rep_CAR (rep_CAR (rep_CDR (obj))))
#define rep_CADADR(obj)         rep_CAR (rep_CDR (rep_CAR (rep_CDR (obj))))
#define rep_CDDADR(obj)         rep_CDR (rep_CDR (rep_CAR (rep_CDR (obj))))
#define rep_CAADDR(obj)         rep_CAR (rep_CAR (rep_CDR (rep_CDR (obj))))
#define rep_CDADDR(obj)         rep_CDR (rep_CAR (rep_CDR (rep_CDR (obj))))
#define rep_CADDDR(obj)         rep_CAR (rep_CDR (rep_CDR (rep_CDR (obj))))
#define rep_CDDDDR(obj)         rep_CDR (rep_CDR (rep_CDR (rep_CDR (obj))))


/* Garbage collection definitions */

/* gc macros for cell8/16 values */
#define rep_GC_CELL_MARKEDP(v)	(rep_PTR(v)->car & rep_CELL_MARK_BIT)
#define rep_GC_SET_CELL(v)	(rep_PTR(v)->car |= rep_CELL_MARK_BIT)
#define rep_GC_CLR_CELL(v)	(rep_PTR(v)->car &= ~rep_CELL_MARK_BIT)

/* gc macros for cons values */
#define rep_GC_CONS_MARKEDP(v)	(rep_CDR(v) & rep_VALUE_CONS_MARK_BIT)
#define rep_GC_SET_CONS(v)	(rep_CDR(v) |= rep_VALUE_CONS_MARK_BIT)
#define rep_GC_CLR_CONS(v)	(rep_CDR(v) &= ~rep_VALUE_CONS_MARK_BIT)

/* True when cell V has been marked. */
#define rep_GC_MARKEDP(v) \
    (rep_CELL_CONS_P(v) ? rep_GC_CONS_MARKEDP(v) : rep_GC_CELL_MARKEDP(v))

/* Set the mark bit of cell V. */
#define rep_GC_SET(v)		\
    do {			\
	if(rep_CELLP(v))	\
	    rep_GC_SET_CELL(v);	\
	else			\
	    rep_GC_SET_CONS(v);	\
    } while(0)

/* Clear the mark bit of cell V. */
#define rep_GC_CLR(v)		\
    do {			\
	if(rep_CELLP(v))	\
	    rep_GC_CLR_CELL(v);	\
	else			\
	    rep_GC_CLR_CONS(v);	\
    } while(0)

/* Recursively mark object V. */
#define rep_MARKVAL(v)						\
    do {							\
	if(v != 0 && !rep_INTP(v) && !rep_GC_MARKEDP(v))	\
	    rep_mark_value(v);					\
    } while(0)

/* A stack of dynamic GC roots, i.e. objects to start marking from.  */
typedef struct rep_gc_root {
    repv *ptr;
    struct rep_gc_root *next;
} rep_GC_root;

typedef struct rep_gc_n_roots {
    repv *first;
    int count;
    struct rep_gc_n_roots *next;
} rep_GC_n_roots;

/* Push a root to VAL using ROOT as storage (ROOT is rep_GC_root type) */
#define rep_PUSHGC(root, val)			\
    do {					\
	(root).ptr = &(val);			\
	(root).next = rep_gc_root_stack;	\
	rep_gc_root_stack = &(root);		\
    } while(0)

/* Push a root to N values starting at PTR using ROOT as storage
   (ROOT is rep_GC_n_roots type) */
#define rep_PUSHGCN(root, ptr, n)		\
    do {					\
	(root).first = (ptr);			\
	(root).count = (n);			\
	(root).next = rep_gc_n_roots_stack;	\
	rep_gc_n_roots_stack = &(root);		\
    } while(0)

#if !defined (rep_PARANOID_GC)

# define rep_POPGC (rep_gc_root_stack = rep_gc_root_stack->next)
# define rep_POPGCN (rep_gc_n_roots_stack = rep_gc_n_roots_stack->next)

#else

/* Check that gc roots are popped when they should have been;
   assumes downwards growing stack */

# if defined (__GNUC__) && defined (sparc)
#  define rep_get_sp(var) asm ("mov %%sp, %0" : "=r" (var))
# else
#  error "don't know how to get stack ptr on this arch, undef rep_PARANOID_GC"
# endif

#define rep_CHECK_GC(root)	\
    char *sp; rep_get_sp(sp);	\
    if (sp > (char *) root)	\
	abort ();

# define rep_POPGC 					\
    do {						\
	rep_CHECK_GC(rep_gc_root_stack)			\
	rep_gc_root_stack = rep_gc_root_stack->next;	\
    } while (0)

# define rep_POPGCN 						\
    do {							\
	rep_CHECK_GC(rep_gc_n_roots_stack)			\
	rep_gc_n_roots_stack = rep_gc_n_roots_stack->next;	\
    } while (0)

#endif


/* Macros for declaring functions */

/* Define a function named NAME (a string), whose function body will
   be called FSYM, whose rep_subr will be called SSYM, with argument
   list ARGS, of type code TYPE. */
#define DEFUN(name,fsym,ssym,args,type)					\
    DEFSTRING(rep_CONCAT(ssym, __name), name);				\
    extern repv fsym args;						\
    rep_ALIGN_CELL(rep_xsubr ssym) = { type, (repv (*)()) fsym,		\
				       rep_VAL(&rep_CONCAT(ssym, __name)), \
				       rep_NULL };			\
    repv fsym args

/* Same as above but with an extra arg -- an interactive-spec string. */
#define DEFUN_INT(name,fsym,ssym,args,type,interactive)	\
    DEFSTRING(rep_CONCAT(ssym, __name), name);				\
    DEFSTRING(rep_CONCAT(ssym, __int), interactive);			\
    extern repv fsym args;						\
    rep_ALIGN_CELL(rep_xsubr ssym) = { type, (repv (*)()) fsym,		\
				       rep_VAL(&rep_CONCAT(ssym, __name)), \
				       rep_VAL(&rep_CONCAT(ssym, __int)) };\
    repv fsym args

/* Add a subroutine */    
#define rep_ADD_SUBR(subr) rep_add_subr(&subr, rep_TRUE)

/* Add a non-exported subroutine */
#define rep_ADD_INTERNAL_SUBR(subr) rep_add_subr(&subr, rep_FALSE)

/* Add an interactive subroutine */    
#define rep_ADD_SUBR_INT(subr) rep_add_subr(&subr, rep_TRUE)

/* Declare a symbol stored in variable QX. */
#define DEFSYM(x, name) \
    repv Q ## x; DEFSTRING(str_ ## x, name)

/* Intern a symbol stored in QX, whose name (a lisp string) is stored
   in str_X (i.e. declared with DEFSYM) */
#define rep_INTERN(x) rep_intern_static(& Q ## x, rep_VAL(& str_ ## x))

/* Same as above, but also marks the variable as dynamically scoped */
#define rep_INTERN_SPECIAL(x) 					\
    do {							\
	rep_intern_static (& Q ## x, rep_VAL(& str_ ## x));	\
	Fmake_variable_special (Q ## x);			\
	rep_SYM(Q ## x)->car |= rep_SF_DEFVAR;			\
    } while (0)

/* Add an error string called err_X for symbol stored in QX */
#define rep_ERROR(x) \
    Fput(Q ## x, Qerror_message, rep_VAL(& err_ ## x))


/* Macros for ensuring an object is of a certain type i.e. to ensure
   first arg `foo' is a string, rep_DECLARE1(foo, rep_STRINGP);  */

#define rep_DECLARE(n,x,e)		\
    do { 				\
	if(! (e)) 			\
	{ 				\
	    rep_signal_arg_error(x, n); \
	    return rep_NULL; 		\
	} 				\
    } while(0)

#define rep_DECLARE1(x,t) rep_DECLARE(1,x,t(x))
#define rep_DECLARE2(x,t) rep_DECLARE(2,x,t(x))
#define rep_DECLARE3(x,t) rep_DECLARE(3,x,t(x))
#define rep_DECLARE4(x,t) rep_DECLARE(4,x,t(x))
#define rep_DECLARE5(x,t) rep_DECLARE(5,x,t(x))

#define rep_DECLARE1_OPT(x,t) rep_DECLARE(1, x, (x) == Qnil || t(x))
#define rep_DECLARE2_OPT(x,t) rep_DECLARE(2, x, (x) == Qnil || t(x))
#define rep_DECLARE3_OPT(x,t) rep_DECLARE(3, x, (x) == Qnil || t(x))
#define rep_DECLARE4_OPT(x,t) rep_DECLARE(4, x, (x) == Qnil || t(x))
#define rep_DECLARE5_OPT(x,t) rep_DECLARE(5, x, (x) == Qnil || t(x))


/* Macros for interrupt handling */

#define rep_MAY_YIELD						\
    do {							\
	if (rep_pending_thread_yield && rep_thread_lock == 0)	\
	    Fthread_yield ();					\
    } while (0)

#define rep_FORBID rep_thread_lock++
#define rep_PERMIT rep_thread_lock--
#define rep_PREEMPTABLE_P (rep_thread_lock <= 0)

/* rep_TEST_INT is called before testing rep_INTERRUPTP, if necessary the
   target operating system will define it to be something useful.
   There's also a variant rep_TEST_INT_SLOW that should be used by code that
   only checks a few times or less a second */
#ifndef rep_TEST_INT

# define rep_TEST_INT						\
    do {							\
	if(++rep_test_int_counter > rep_test_int_period) { 	\
	    (*rep_test_int_fun)();				\
	    rep_test_int_counter = 0;				\
	    rep_pending_thread_yield = rep_TRUE;		\
	}							\
    } while(0)

# define rep_TEST_INT_SLOW		\
    do {				\
	(*rep_test_int_fun)();		\
	rep_test_int_counter = 0;	\
	if (!rep_INTERRUPTP)		\
	    Fthread_yield ();		\
    } while(0)

#else /* !rep_TEST_INT */

# ifndef rep_TEST_INT_SLOW
#  define rep_TEST_INT_SLOW rep_TEST_INT
# endif

#endif

/* True when an interrupt has occurred; this means that the function
   should exit as soon as possible, returning rep_NULL. */
#define rep_INTERRUPTP (rep_throw_value != rep_NULL)


/* End-of-list / false value

   The canonical method of getting '() is to access the `Qnil' variable.

   But we know that that currently points to `rep_eol_datum'. So avoid
   lots of global variable referencing by hardcoding that value for
   library-internal code. */

extern repv Qnil;

#ifdef rep_INTERNAL
  extern rep_tuple rep_eol_datum;
# ifdef rep_DEFINE_QNIL
    repv Qnil = rep_VAL (&rep_eol_datum);
# endif
  /* OS X has problems with this */
# ifndef __APPLE__
#  define Qnil rep_VAL(&rep_eol_datum)
# endif
#endif


/* Storing timestamps */

#define rep_MAKE_TIME(time) \
    Fcons(rep_MAKE_INT(time / 86400), rep_MAKE_INT(time % 86400))

#define rep_GET_TIME(time) \
    (rep_INT(rep_CAR(time)) * 86400 + rep_INT(rep_CDR(time)))

#define rep_TIMEP(v) rep_CONSP(v)

#endif /* REP_LISP_H */
