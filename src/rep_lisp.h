/* lisp.h -- Data structures/objects for Lisp
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

#ifndef _LISP_H
#define _LISP_H

#ifndef _VALUE_H
# include "value.h"
#endif

/* These numbers weren't just plucked from the air, they make the blocks
   of objects fit as close as possible into powers of 2 sized blocks. */
#define CONSBLK_SIZE	510		/* ~4k */
#define SYMBOLBLK_SIZE	340		/* ~8k */

/* The number of hash buckets in each obarray, this is a prime number. */
#define OBSIZE		509


/* Structure of Lisp objects and the pointers to them (VALUEs) */

/* Bit definitions for VALUE pointers. The lowest bit is always zero
   except during GC. If bit one is set the object is a 30-bit signed
   integer, with the data bits stored in the pointer as bits 2->31.

   If bit one is clear the VALUE is a pointer to a "cell", all objects
   apart from integers are represented by various types of cells.
   Every cell has a VALUE as its first element, the lowest bits of this
   VALUE define the actual type of the cell.

   If bit zero is unset, the cell is a cons, a pair of two values the
   car and the cdr (the GC mark bit of the cons is bit zero of the cdr).

   If bit zero is set the cell more type information is stored in bits
   1->6, with bit 7 the mark bit. */

#define VALUE_CONS_MARK_BIT	1
#define VALUE_IS_INT		2
#define VALUE_INT_SHIFT		2
#define CELL_ALIGNMENT		4

#define CELLP(v)	(((v) & VALUE_IS_INT) == 0)
#define INTP(v)		(!CELLP(v))

/* Convert a VALUE into a signed integer. */
#define VINT(v)		(((PTR_SIZED_INT)(v)) >> VALUE_INT_SHIFT)

/* Convert a signed integer into a VALUE. */
#define MAKE_INT(x)	(((x) << VALUE_INT_SHIFT) | VALUE_IS_INT)

/* Assuming we're using 32 bit VALUEs. */
#define LISP_INT_BITS   (32 - VALUE_INT_SHIFT)
#define LISP_MAX_INT	((1 << (LISP_INT_BITS - 1)) - 1)
#define LISP_MIN_INT	(-(1 << (LISP_INT_BITS - 1)))

/* Store anything needing >24 bits (future expansion and all that),
   in a cons cell, as one 24 bit, and one eight bit quantity. */
#define MAKE_LONG_INT(x) \
    cmd_cons(MAKE_INT((x) & 0x00ffffff), MAKE_INT((x) >> 24))

/* Convert a cons cell with two integers into a signed long int. */
#define VLONG_INT(v)	(VINT(VCAR(v)) | (VINT(VCDR(v)) << 24))

/* True when V is a long integer. */
#define LONG_INTP(v)	(CONSP(v) && INTP(VCAR(v)) && INTP(VCDR(v)))

#if CELL_ALIGNMENT <= STRMEM_ALIGNMENT
  /* Allocate SIZE bytes of memory, aligned to NORMAL_ALIGNMENT */
# define ALLOC_CELL(n) str_alloc(n)
  /* Free something allocated by ALLOC_OBJECT */
# define FREE_CELL(x)  str_free(x)
#else
# error Need an aligned malloc()
#endif

/* Compatibility */
#define ALLOC_OBJECT ALLOC_CELL
#define FREE_OBJECT FREE_CELL

/* A ``null pointer'', i.e. an invalid object. This has the important
   property of being a proper null pointer (i.e. (void *)0) when
   converted to a pointer, i.e. VPTR(LISP_NULL) == NULL. */
#define LISP_NULL	(0)

#define ALIGN_CELL	CONCAT(ALIGN_, CELL_ALIGNMENT)


/* Structure of a cell */

typedef struct {
    /* Low bits of this value define type of the cell. See below. All
       other bits (8->31) are available */
    VALUE car;

    /* Data follows, in real objects. */
} Lisp_Cell;

/* If this bit is set in the car of a cell, bits 1->5 of the car
   are type data, bit 6 is set if the object is allocated staticaly,
   bit 7 is the GC mark bit. This means a maximum of 2^4, i.e. 32,
   cell8 types. */
#define CELL_IS_8		1
#define CELL8_MARK_BIT		0x80
#define CELL8_STATIC_BIT	0x40
#define CELL8_TYPE_MASK		0x3f
#define CELL8_TYPE_BITS		8

/* Build a `Lisp_Cell *' pointer out of a VALUE of a normal type */
#define VPTR(v) 		((Lisp_Cell *)(v))

/* Build a VALUE out of a pointer to a Lisp_Normal object */
#define VAL(x)			((VALUE)(x))

#define CELL_CONS_P(v)		(!(VPTR(v)->car & CELL_IS_8))

#define VCELL8_TYPE(v)	 	((VPTR(v)->car & CELL8_TYPE_MASK))

#define SET_CELL8_TYPE(v, t) \
   (VPTR(v)->car = (VPTR(v)->car & CELL8_TYPE_MASK) | (t))

#define VCELL8_STATIC_P(v)	(VPTR(v)->car & CELL8_STATIC_BIT)


/* Structure of a cons cell */

typedef struct {
    VALUE car;
    VALUE cdr;				/* low bit is GC mark */
} Lisp_Cons;

/* Structure of cons allocation blocks */
typedef struct lisp_cons_block {
    struct lisp_cons_block *next;

    /* Actual start address of the allocation block. To enforce
       alignment, this may be slightly before the start of the
       structure. */
    void *alloc_address;

    /* The cons cells */
    Lisp_Cons cons[CONSBLK_SIZE] ALIGN_CELL;
} Lisp_Cons_Block;

#define CONS_ALIGNMENT	CELL_ALIGNMENT

#define CONSP(v)	(CELLP(v) && CELL_CONS_P(v))

/* Build a VALUE out of a pointer to a Lisp_Cons object */
#define CONS_VAL(x)	VAL(x)

/* Get a pointer to a cons cell from a VALUE. */
#define VCONS(v)	((Lisp_Cons *)VPTR(v))

/* Get the car or cdr from a cons VALUE. */
#define VCAR(v)		(VCONS(v)->car)
#define VCDR(v)		(VCONS(v)->cdr)

/* Get the cdr when GC is in progress. */
#define VGCDR(v)	(VCDR(v) & ~VALUE_CONS_MARK_BIT)


/* Type data */

/* Each type of Lisp object has a type code associated with it. For
   normal objects, this code is stored in the `type' field.

   Note that changing the order of this structure at all, must be
   complemented by changing values.c:data_types.

   Also note how non-cons cells are given odd values, so that the
   CELL_IS_8 bit doesn't have to be masked out. */

#define V_Cons		0x00
#define V_Symbol	0x01
#define V_Int		0x02
#define V_Vector	0x03
#define V_String	0x05
#define V_Void		0x07
#define V_Process	0x09
#define V_Var		0x0b
#define V_SF		0x0d
#define V_Subr0		0x0f
#define V_Subr1		0x11
#define V_Subr2		0x13
#define V_Subr3		0x15
#define V_Subr4		0x17
#define V_Subr5		0x19
#define V_SubrN		0x1b
#define V_Buffer	0x1d
#define V_Window	0x1f
#define V_View		0x21
#define V_Mark		0x23
#define V_File		0x25
#define V_GlyphTable	0x27
#define V_Compiled	0x29
#define V_MAX		0x40		/* nothing from here on */

/* Assuming that V is a cell, return the type code */
#define VCELL_TYPE(v)	(CONSP(v) ? V_Cons : VCELL8_TYPE(v))

/* Return a type code given a VALUE */
#define VTYPE(v)	(INTP(v) ? V_Int : VCELL_TYPE(v))

/* true if V is of type T (T must be a cell8 type) */
#define VCELL8_TYPEP(v,t) (CELLP(v) && VCELL8_TYPE(v) == (t))

/* true if V is of type T. */
#define VTYPEP(v,t)	(VTYPE(v) == t)


/* Information about each type */
typedef struct {
    /* Compares two values, rc is similar to strcmp() */
    int (*compare)(VALUE val1, VALUE val2);

    /* Prints a textual representation of the object, not necessarily in 
       a read'able format */
    void (*princ)(VALUE stream, VALUE obj);

    /* Prints a textual representation of the object, if possible in
       a read'able format */
    void (*print)(VALUE stream, VALUE obj);

    /* When non-null, a function that should be called during the
       sweep phase of garbage collection. */
    void (*sweep)(void);

    /* this is the name of the type */
    char *name;
} Lisp_Type_Data;

/* An array of these things, indexed by type code */
extern Lisp_Type_Data data_types[V_MAX];

/* These are also defined as functions (lower-case'd names)...  */
#define VALUE_CMP(v1,v2) data_types[VTYPE(v1)].compare(v1,v2)
#define PRINC_VAL(s,v)	data_types[VTYPE(v)].princ(s,v)
#define PRINT_VAL(s,v)	data_types[VTYPE(v)].print(s,v)


/* Strings */

/* If INLINE_STATIC_STRINGS is defined we use some inline assembly
   to create statically allocated Lisp_String constants that are
   (almost) exactly the same as dynamically allocated strings. If
   it's not defined static strings must differ froom dynamic strings
   introducing extra overhead (the need to differentiate between
   the two). */

typedef struct {
    /* Bits 0->7 are standard cell8 defines. Bits 8->31 store the length
       of the string.

       This means that strings can't contain more than 2^24-1 bytes
       (thats about 16.7MB) */
    VALUE car;

    union {
#ifndef INLINE_STATIC_STRINGS
	/* There's no way I can see to make a C compiler generate
	   a variably sized string into a structure at compile-time..?
	   Unless we know how to do this in assembler, put a pointer to
	   the string instead.. */
	u_char *static_string;
#endif
	u_char dynamic_string[1];
    } data;
} Lisp_String;

#define LISP_MAX_STRING		((1 << 24) - 1)
#define STRING_LEN_SHIFT	8

/* The number of bytes that need to be allocated for a string cell
   containg X string bytes (including terminating zero). */
#define DSTRING_SIZE(x) 	(sizeof(VALUE) + (x))

#define STRINGP(v)		VCELL8_TYPEP(v, V_String)
#define VSTRING(v)		((Lisp_String *)VPTR(v))

#define STRING_LEN(v)		(VSTRING(v)->car >> STRING_LEN_SHIFT)

#define MAKE_STRING_CAR(len)	(((len) << STRING_LEN_SHIFT) | V_String)

/* True if this string may be written to; generally V_StaticString types
   are made from C string-constants and usually in read-only storage. */
#define STRING_WRITABLE_P(s)	(!VCELL8_STATIC_P(s))

#ifndef INLINE_STATIC_STRINGS

/* Structure for initialising statically allocated strings. */
struct static_string {
    VALUE car;
    u_char *data;
};

/* Define a variable V, containing a static string S. This must be cast
   to a VALUE via the VAL() macro when using. */
# define DEFSTRING(v, s) 					\
    static struct static_string v ALIGN_CELL			\
	= { ((sizeof(s) - 1) << STRING_LEN_SHIFT)		\
	    | CELL8_STATIC_BIT | V_String, s }

# define VSTR(v)	((VSTRING(v)->car & STRING_STATIC)	\
			 ? VSTRING(v)->data.static_string	\
			 : VSTRING(v)->data.dynamic_string)

/* Use this to get a newline into a DEFSTRING */
# define DS_NL "\n"

#else /* INLINE_STATIC_STRINGS */

/* This macro will compile a DEFSTRING expression into exactly the
   same form as the normal dynamically allocated string. The size/type,
   followed immediately by the data itself.. */
# define DEFSTRING(v, s)						\
    extern Lisp_String v;						\
    __asm__ (".align " QUOTE(CELL_ALIGNMENT) "\n"			\
	     QUOTE(v) ":\n"						\
	     "\t.long (((2f-1f)-1) << " QUOTE(STRING_LEN_SHIFT) ") | "	\
	     QUOTE(CELL8_STATIC_BIT) " | " QUOTE(V_String) "\n"		\
	     "1:\t.asciz \"" s "\"\n2:\n")

# define VSTR(v)	(VSTRING(v)->data.dynamic_string)

/* Use this to get a newline into a DEFSTRING. Need this since we want
   the assembler to expand the \n not the C compiler. */
# define DS_NL "\\n"

#endif /* INLINE_STATIC_STRINGS */


/* Symbols */

/* Symbol object, each symbol has 4 basic attributes, a name, its value
   as a variable, its value as a function and a property-list.
   Symbols are generally stored in hash tables (obarray) with collisions
   chained from the `sym_Next' field.  */
typedef struct {
    VALUE car;				/* bits 8->11 are flags */
    VALUE next;				/* next symbol in obarray bucket */
    VALUE name;
    VALUE value;
    VALUE function;
    VALUE prop_list;
} Lisp_Symbol;

/* This bit set in car means that the value is a constant, and therefore
   can't be modified. */
#define SF_CONSTANT	(1 << (CELL8_TYPE_BITS + 0))

/* Means that the symbol's value may be in the buffer-local storage, if so
   then that occurrence takes precedence. */
#define SF_BUFFER_LOCAL (1 << (CELL8_TYPE_BITS + 1))

/* This means that setting the value of the symbol always sets the
   buffer-local value, even if one doesn't already exist.  */
#define SF_SET_BUFFER_LOCAL (1 << (CELL8_TYPE_BITS + 2))

/* When a function is evaluated whose symbol has this bit set, the
   next evaluated form will invoke the Lisp debugger. */
#define SF_DEBUG	(1 << (CELL8_TYPE_BITS + 3))

/* Symbol allocation blocks */
typedef struct lisp_symbol_block {
    struct lisp_symbol_block *next;
    Lisp_Symbol symbols[SYMBOLBLK_SIZE] ALIGN_CELL;
} Lisp_Symbol_Block;

#define VSYM(v)		((Lisp_Symbol *)VPTR(v))
#define SYMBOLP(v)	VCELL8_TYPEP(v, V_Symbol)

#define NILP(v)		((v) == sym_nil)
#define LISTP(v)	(NILP(v) || CONSP(v))


/* Vectors */

typedef struct lisp_vector {
    VALUE car;				/* size is bits 8->31 */
    struct lisp_vector *next;
    VALUE array[0];
} Lisp_Vector;

/* Bytes to allocate for S objects */
#define VECT_SIZE(s)	((sizeof(VALUE) * (s)) + sizeof(Lisp_Vector))

#define VVECT(v)	((Lisp_Vector *)VPTR(v))
#define VVECTI(v,i)	(VVECT(v)->array[(i)])

#define VVECT_LEN(v)	(VVECT(v)->car >> 8)
#define VSET_VECT_LEN(v,l) (VVECT(v)->car = ((l) << 8 | V_Vector))

#define VECTORP(v)	VCELL8_TYPEP(v, V_Vector)

#define VECTOR_WRITABLE_P(v) (!VCELL8_STATIC_P(v))


/* Compiled Lisp functions; this is a vector. Some of these definitions
   are probably hard coded into lispmach.c */

#define COMPILEDP(v)	    VCELL8_TYPEP(v, V_Compiled)
#define VCOMPILED(v)	    ((Lisp_Vector *)VPTR(v))

/* First element is lambda list. */
#define COMPILED_LAMBDA(v)	VVECTI(v, 0)

/* Second is byte-code string */
#define COMPILED_CODE(v)	VVECTI(v, 1)

/* Third is constant vector */
#define COMPILED_CONSTANTS(v)	VVECTI(v, 2)

/* Fourth is an integer, low 16 bits is stack usage bit 16=macrop */
#define COMPILED_STACK(v)	(VINT(VVECTI(v, 3)) & 0x0ffff)
#define COMPILED_MACRO_P(v)	(VINT(VVECTI(v, 3)) & 0x10000)

#define COMPILED_MIN_SLOTS	4

/* Optional fifth element is documentation. */
#define COMPILED_DOC(v)		((VVECT_LEN(v) >= 5) ? VVECTI(v, 4) : sym_nil)

/* Optional sixth element is interactive specification. */
#define COMPILED_INTERACTIVE(v) ((VVECT_LEN(v) >= 6) ? VVECTI(v, 5) : sym_nil)

#define COMPILED_WRITABLE_P(v) VECTOR_WRITABLE_P(v)


/* Positions */

/* A pointer to a buffer position. There's a conventions that positions
   accessed via VALUE pointers (and VCOL, VROW macros) are _read_only_,
   while those accessed through Pos * pointers (and PCOL, PROW macros)
   are _read_write_, probably allocated on the stack. */

#define POSP(v) (CONSP(v) && INTP(VCAR(v)) && INTP(VCDR(v)))

/* We define the column in the cdr and the row in the car, so that
   the normal cons-comparison (car first, then cdr) will work as the
   old pos-comparison used to (i.e. row-major). */
#define MAKE_POS(col, row) cmd_cons(MAKE_INT(row), MAKE_INT(col))
#define VCOL(v) (VINT(VCDR(v)))
#define VROW(v) (VINT(VCAR(v)))

/* These should never be used unless it's clear there can be
   no other references to V. */
#define VSETCOL(v,c) (VCDR(v) = MAKE_INT(c))
#define VSETROW(v,r) (VCAR(v) = MAKE_INT(r))

/* These all want VALUE pointers */
#define POS_EQUAL_P(s,e) \
    ((VROW(s) == VROW(e)) && (VCOL(s) == VCOL(e)))
#define POS_GREATER_P(s,e) \
    ((VROW(s) > VROW(e)) || ((VROW(s) == VROW(e)) && (VCOL(s) > VCOL(e))))
#define POS_GREATER_EQUAL_P(s,e) \
    ((VROW(s) > VROW(e)) || ((VROW(s) == VROW(e)) && (VCOL(s) >= VCOL(e))))
#define POS_LESS_P(s,e) \
    ((VROW(s) < VROW(e)) || ((VROW(s) == VROW(e)) && (VCOL(s) < VCOL(e))))
#define POS_LESS_EQUAL_P(s,e) \
    ((VROW(s) < VROW(e)) || ((VROW(s) == VROW(e)) && (VCOL(s) <= VCOL(e))))

/* A more conventional C structure, used in the editor internals to
   avoid the gratuitous masking and shifting otherwise required. */
typedef struct {
    long row, col;
} Pos;

#define PCOL(p) ((p)->col)
#define PROW(p) ((p)->row)

#define COPY_VPOS(p, v) 	\
    do {			\
	PROW(p) = VROW(v);	\
	PCOL(p) = VCOL(v);	\
    } while(0)

#define COPY_POS(p) MAKE_POS(PCOL(p), PROW(p))

/* These all want Pos pointers */
#define PPOS_EQUAL_P(s,e) \
    ((PROW(s) == PROW(e)) && (PCOL(s) == PCOL(e)))
#define PPOS_GREATER_P(s,e) \
    ((PROW(s) > PROW(e)) || ((PROW(s) == PROW(e)) && (PCOL(s) > PCOL(e))))
#define PPOS_GREATER_EQUAL_P(s,e) \
    ((PROW(s) > PROW(e)) || ((PROW(s) == PROW(e)) && (PCOL(s) >= PCOL(e))))
#define PPOS_LESS_P(s,e) \
    ((PROW(s) < PROW(e)) || ((PROW(s) == PROW(e)) && (PCOL(s) < PCOL(e))))
#define PPOS_LESS_EQUAL_P(s,e) \
    ((PROW(s) < PROW(e)) || ((PROW(s) == PROW(e)) && (PCOL(s) <= PCOL(e))))


/* Files */

/* A file object.  */
typedef struct lisp_file {
    VALUE car;				/* single flag at bit 8 */
    struct lisp_file *next;
    VALUE name;
    FILE *file;
} Lisp_File;

/* When this bit is set in flags, the file handle is never fclose()'d,
   i.e. this file points to something like stdin. */
#define LFF_DONT_CLOSE (1 << CELL8_TYPE_BITS)

#define VFILE(v)	((Lisp_File *)VPTR(v))
#define FILEP(v)	VCELL8_TYPEP(v, V_File)


/* Built-in subroutines */

/* C subroutine, can take from zero to five arguments.  */
typedef struct {
    VALUE car;
    union {
	VALUE (*fun0)(void);
	VALUE (*fun1)(VALUE);
	VALUE (*fun2)(VALUE, VALUE);
	VALUE (*fun3)(VALUE, VALUE, VALUE);
	VALUE (*fun4)(VALUE, VALUE, VALUE, VALUE);
	VALUE (*fun5)(VALUE, VALUE, VALUE, VALUE, VALUE);
    } fun;
    VALUE name;
    VALUE doc_index;
    VALUE int_spec;
} Lisp_Subr;

typedef struct {
    VALUE car;
    void *fun;
    VALUE name;
    VALUE doc_index;
    VALUE int_spec;
} Lisp_XSubr;

#define VXSUBR(v)	((Lisp_XSubr *)VPTR(v))
#define VSUBR(v)	((Lisp_Subr *)VPTR(v))
#define VSUBR0FUN(v)	(VSUBR(v)->fun.fun0)
#define VSUBR1FUN(v)	(VSUBR(v)->fun.fun1)
#define VSUBR2FUN(v)	(VSUBR(v)->fun.fun2)
#define VSUBR3FUN(v)	(VSUBR(v)->fun.fun3)
#define VSUBR4FUN(v)	(VSUBR(v)->fun.fun4)
#define VSUBR5FUN(v)	(VSUBR(v)->fun.fun5)
#define VSUBRNFUN(v)	(VSUBR(v)->fun.fun1)
#define VSFFUN(v)	(VSUBR(v)->fun.fun1)
#define VVARFUN(v)	(VSUBR(v)->fun.fun1)


/* Other definitions */

/* Macros for other types */
#define VMARK(v)	((Mark *)VPTR(v))
#define VTX(v)		((TX *)VPTR(v))
#define VBUFFER(v)	VTX(v)
#define VPROC(v)	((struct Proc *)VPTR(v))
#define VWIN(v)		((WIN *)VPTR(v))
#define VVIEW(v)	((VW *)VPTR(v))
#define VGLYPHTAB(v)	((GlyphTable *)VPTR(v))
	
#define BUFFERP(v)	VCELL8_TYPEP(v, V_Buffer)
#define MARKP(v)	VCELL8_TYPEP(v, V_Mark)
#define PROCESSP(v)	VCELL8_TYPEP(v, V_Process)
#define WINDOWP(v)	(VCELL8_TYPEP(v, V_Window) && VWIN(v)->w_Window)
#define VIEWP(v)	(VCELL8_TYPEP(v, V_View) && VVIEW(v)->vw_Win)
#define GLYPHTABP(v)	VCELL8_TYPEP(v, V_GlyphTable)
#define VOIDP(v)	VCELL8_TYPEP(v, V_Void)

/* Building lists */
#define LIST_1(v1)	       cmd_cons(v1, sym_nil)
#define LIST_2(v1,v2)	       cmd_cons(v1, LIST_1(v2))
#define LIST_3(v1,v2,v3)       cmd_cons(v1, LIST_2(v2, v3))
#define LIST_4(v1,v2,v3,v4)    cmd_cons(v1, LIST_3(v2, v3, v4))
#define LIST_5(v1,v2,v3,v4,v5) cmd_cons(v1, LIST_4(v2, v3, v4, v5))


/* Garbage collection definitions */

#define GC_CELL_MARKEDP(v)	(VPTR(v)->car & CELL8_MARK_BIT)
#define GC_SET_CELL(v)		(VPTR(v)->car |= CELL8_MARK_BIT)
#define GC_CLR_CELL(v)		(VPTR(v)->car &= ~CELL8_MARK_BIT)

#define GC_CONS_MARKEDP(v)	(VCDR(v) & VALUE_CONS_MARK_BIT)
#define GC_SET_CONS(v)		(VCDR(v) |= VALUE_CONS_MARK_BIT)
#define GC_CLR_CONS(v)		(VCDR(v) &= ~VALUE_CONS_MARK_BIT)

/* True when cell V has been marked. */
#define GC_MARKEDP(v) \
    (CELL_CONS_P(v) ? GC_CONS_MARKEDP(v) : GC_CELL_MARKEDP(v))

/* Set the mark bit of cell V. */
#define GC_SET(v)		\
    do {			\
	if(CELLP(v))		\
	    GC_SET_CELL(v);	\
	else			\
	    GC_SET_CONS(v);	\
    } while(0)

/* Clear the mark bit of cell V. */
#define GC_CLR(v)		\
    do {			\
	if(CELLP(v))		\
	    GC_CLR_CELL(v);	\
	else			\
	    GC_CLR_CONS(v);	\
    } while(0)

/* Recursively mark object V. */
#define MARKVAL(v)					\
    do {						\
	if(v != 0 && !INTP(v) && !GC_MARKEDP(v))	\
	    mark_value(v);				\
    } while(0)

/* A stack of dynamic GC roots, i.e. objects to start marking from.  */
typedef struct gc_root {
    VALUE *ptr;
    struct gc_root *next;
} GC_root;

typedef struct gc_n_roots {
    VALUE *first;
    int count;
    struct gc_n_roots *next;
} GC_n_roots;

#define POPGC (gc_root_stack = gc_root_stack->next)
#define PUSHGC(root, val)			\
    do {					\
	(root).ptr = &(val);			\
	(root).next = gc_root_stack;		\
	gc_root_stack = &(root);		\
    } while(0)

#define POPGCN (gc_n_roots_stack = gc_n_roots_stack->next)
#define PUSHGCN(root, ptr, n)			\
    do {					\
	(root).first = (ptr);			\
	(root).count = (n);			\
	(root).next = gc_n_roots_stack;		\
	gc_n_roots_stack = &(root);		\
    } while(0)


/* More other stuff */

/* Keeps a backtrace of all lisp functions called. NOT primitives. */
struct Lisp_Call {
    struct Lisp_Call *next;
    VALUE fun;
    VALUE args;
    /* t if `args' is list of *evalled* arguments.  */
    VALUE args_evalled_p;
};


/* Macros for declaring functions */

/* Define a function named NAME (a string), whose function body will
   be called FSYM, whose Lisp_Subr will be called SSYM, with argument
   list ARGS, of type code TYPE, whose doc-string is at index DOCINDEX
   in the documentation file. */
#define DEFUN(name,fsym,ssym,args,type,docindex)			\
    DEFSTRING(CONCAT(ssym, __name), name);				\
    Lisp_XSubr ALIGN_CELL ssym = { type, fsym, LISP_NULL,		\
				   MAKE_INT(docindex), LISP_NULL }; 	\
    VALUE fsym args

/* Same as above but with an extra arg -- an interactive-spec string. */
#define DEFUN_INT(name,fsym,ssym,args,type,docindex,interactive)	\
    DEFSTRING(CONCAT(ssym, __name), name);				\
    DEFSTRING(CONCAT(ssym, __int), interactive);			\
    Lisp_XSubr ALIGN_CELL ssym = { type, fsym, LISP_NULL,		\
				   MAKE_INT(docindex), LISP_NULL };	\
    VALUE fsym args

/* Add a subroutine */    
#define ADD_SUBR(subr) 				\
    do {					\
	subr.name = VAL(&CONCAT(subr, __name));	\
	add_subr(&subr);			\
    } while (0)

/* Add an interactive subroutine */    
#define ADD_SUBR_INT(subr)				\
    do {						\
	subr.name = VAL(&CONCAT(subr, __name));		\
	subr.int_spec = VAL(&CONCAT(subr, __int));	\
	add_subr(&subr);				\
    } while (0)

/* Declare a symbol stored in variable sym_X. */
#define DEFSYM(x, name) \
    VALUE CONCAT(sym_, x); DEFSTRING(CONCAT(str_, x), name)

/* Intern a symbol stored in sym_X, whose name (a lisp string) is stored
   in str_X (i.e. declared with DEFSYM) */
#define INTERN(x) intern_static(&CONCAT(sym_, x), VAL(&CONCAT(str_, x)))

/* Add an error string called err_X for symbol stored in sym_X */
#define ERROR(x) \
    cmd_put(CONCAT(sym_, x), sym_error_message, VAL(&CONCAT(err_, x)))

/* Add a documentation string for the variable stored in sym_X, pointing at
   index DOC_X */
#define DOC(x)	 						\
    cmd_put(CONCAT(sym_, x), sym_variable_documentation,	\
	    MAKE_INT(CONCAT(DOC_, x)))


/* Macros for ensuring an object is of a certain type i.e. to ensure
   first arg `foo' is a string, DECLARE1(foo, STRINGP);  */

#define DECLARE(n,x,t) 			\
    do { 				\
	if(! t(x)) 			\
	{ 				\
	    signal_arg_error(x, n); 	\
	    return LISP_NULL; 		\
	} 				\
    } while(0)

#define DECLARE1(x,t) DECLARE(1,x,t)
#define DECLARE2(x,t) DECLARE(2,x,t)
#define DECLARE3(x,t) DECLARE(3,x,t)
#define DECLARE4(x,t) DECLARE(4,x,t)
#define DECLARE5(x,t) DECLARE(5,x,t)


/* Macros for interrupt handling */

/* This macro is called before testing INT_P, if necessary the target
   operating system header files will define it to be something useful. */
#ifndef TEST_INT
# define TEST_INT do { ; } while(0)
#endif

/* True when an interrupt has occurred; this means that the function
   should exit as soon as possible, returning LISP_NULL. */
#define INT_P (throw_value != LISP_NULL)

#endif /* _LISP_H */
