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

/* Bit definitions for VALUE pointers. The lowest bit is used in the
   car of cons cells as the cell's mark bit. The next bit defines
   whether the object is a `normal' object (see below) or a cons cell
   or an integer. If this bit is set (i.e. cons or number), the next
   bit defines whether it's a cons cell or a number (set = number).
   This bit is never touched in pointers to normal objects.

   What this means is that normal objects must be aligned on a four
   byte boundary (since the bottom two bits will be masked out), while
   cons cells must start on an eight byte boundary. Also, since the
   bottom two bits of a VALUE pointing to a normal object will be zero, no
   masking need be performed to access it's location (providing GC is
   not underway).

   Finally, integers also lose their bottom three bits, meaning that they
   must be left-shifted three bits when stored. */

#define VALUE_CONS_MARK_BIT	1
#define GC_CONS_MARK_BIT	VALUE_CONS_MARK_BIT
#define VALUE_IS_CONS_OR_INT	2
#define VALUE_IS_INT		4

#define VALUE_INT_SHIFT		3
#define CONS_ALIGNMENT		8
#define NORMAL_ALIGNMENT	4

#define NORMALP(v)	(((v) & VALUE_IS_CONS_OR_INT) == 0)
#define INTP(v)		(!NORMALP(v) && (((v) & VALUE_IS_INT) != 0))
#define CONSP(v)	(!NORMALP(v) && (((v) & VALUE_IS_INT) == 0))

/* Used in GC; turn off the cons mark bit */
#define GCREF(v)	((v) & ~VALUE_CONS_MARK_BIT)

/* Convert a VALUE into a signed integer. */
#define VINT(v)		(((PTR_SIZED_INT)(v)) >> VALUE_INT_SHIFT)

/* Convert a signed integer into a VALUE. */
#define MAKE_INT(x)	(((x) << VALUE_INT_SHIFT) \
			 | VALUE_IS_CONS_OR_INT | VALUE_IS_INT)

/* Store anything needing >24 bits (future expansion and all that),
   in a cons cell, as one 24 bit, and one eight bit quantity. */
#define MAKE_LONG_INT(x) \
    cmd_cons(MAKE_INT((x) & 0x00ffffff), MAKE_INT((x) >> 24))

/* Convert a cons cell with two integers into a signed long int. */
#define VLONG_INT(v) (VINT(VCAR(v)) | (VINT(VCDR(v)) << 24))

/* True when V is a long integer. */
#define LONG_INTP(v) (CONSP(v) && INTP(VCAR(v)) && INTP(VCDR(v)))

#if NORMAL_ALIGNMENT <= STRMEM_ALIGNMENT
  /* Allocate SIZE bytes of memory, aligned to NORMAL_ALIGNMENT */
# define ALLOC_OBJECT(n) str_alloc(n)
  /* Free something allocated by ALLOC_OBJECT */
# define FREE_OBJECT(x)  str_free(x)
#else
# error Need an aligned malloc()
#endif

/* A ``null pointer'', i.e. an invalid object. This has the important
   property of being a proper null pointer (i.e. (void *)0) when
   converted to a pointer, i.e. VCONS(LISP_NULL) == NULL, and
   VPTR(LISP_NULL) == NULL. */
#define LISP_NULL (0)


/* Structure of a cons cell */
typedef struct {
    VALUE car;
    VALUE cdr;
} Lisp_Cons;

/* Structure of cons allocation blocks */
typedef struct lisp_cons_block {
    struct lisp_cons_block *next;

    /* Actual start address of the allocation block. To enforce
       alignment, this may be slightly before the start of the
       structure. */
    void *alloc_address;

    /* The cons cells */
    Lisp_Cons cons[CONSBLK_SIZE] CONCAT(ALIGN_, CONS_ALIGNMENT);
} Lisp_Cons_Block;

/* Build a VALUE out of a pointer to a Lisp_Cons object */
#define CONS_VAL(x)	(((VALUE)(x)) | VALUE_IS_CONS_OR_INT)

/* Get a pointer to a cons cell from a VALUE. */
#define VCONS(v)	((Lisp_Cons *)((v) & ~(VALUE_CONS_MARK_BIT \
					       | VALUE_IS_CONS_OR_INT)))

/* Get the car or cdr from a cons VALUE. */
#define VCAR(v)		(VCONS(v)->car)
#define VCDR(v)		(VCONS(v)->cdr)


/* Structure of a normal object */
typedef struct {
    /* Tag defining the type of this object. Bit 7 is reserved for gc,
       at all other times it will be zero.  */
    u_char type;

    /* Data follows, in real objects. */
} Lisp_Normal;

/* Mark bit in normal objects */
#define GC_NORMAL_MARK_BIT 0x80

/* Build a VALUE out of a pointer to a Lisp_Normal object */
#define VAL(x)		((VALUE)(x))

/* Build a `Lisp_Normal *' pointer out of a VALUE of a normal type */
#define VPTR(v) 	((Lisp_Normal *)(v))


/* Type data */

/* Each type of Lisp object has a type code associated with it. For
   normal objects, this code is stored in the `type' field.
   Note that changing the order of this structure at all, must be
   complemented by changing values.c:data_types. */
enum Lisp_Type
{
    /* Not a normal object. Given code zero since (v & VALUE_IS_INT) is
       zero for a cons cell */
    V_Cons = 0,

    V_StaticString = 1,
    V_DynamicString = 2,
    V_Vector = 3,
    V_Int = 4,				/* (v & VALUE_IS_INT) == 4 for int */
    V_Symbol,
    V_Void,
    V_Var,				/* subr with one arg */
    V_Subr0,
    V_Subr1,
    V_Subr2,
    V_Subr3,
    V_Subr4,
    V_Subr5,
    V_SubrN,
    V_SF,
    V_Buffer,
    V_Window,
    V_View,
    V_Mark,
    V_File,
    V_Process,
    V_GlyphTable,

    V_MAX				/* not a type */
};

/* Assuming that V is of normal type, return the type code */
#define VNORMAL_TYPE(v)	(VPTR(v)->type)

/* Similar for cons or int */
#define VUNNORMAL_TYPE(v) ((v) & VALUE_IS_INT)

/* Return a type code given a VALUE */
#define VTYPE(v)	(NORMALP(v) ? VNORMAL_TYPE(v) : VUNNORMAL_TYPE(v))

/* true if V is of type T (T must be a normal type) */
#define VNORMAL_TYPEP(v,t) (NORMALP(v) && VNORMAL_TYPE(v) == (t))

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

/* String data types. the `String' type is what a VALUE points to, if
   the string is dynamic it gets a length field in the word before the
   String struct proper.  */
typedef struct {
    /* data[0] is type, data[1->N] is data. */
    u_char data[1];
} Lisp_String;

typedef struct {
    int length;
    u_char data[1];		/* VALUEs point here */
} Lisp_DynamicString;
#define DSTR_SIZE(s) (sizeof(int) + 1 + (s))

#define VSTRING(v)	((Lisp_String *)VPTR(v))
#define VSTR(v)		(&VSTRING(v)->data[1])

#define STRINGP(v)	(NORMALP(v)					\
			 && (VNORMAL_TYPE(v) == V_StaticString		\
			     || VNORMAL_TYPE(v) == V_DynamicString))

/* Get the beginning of a DynamicString from a String.  */
#define DSTRING_HDR(s) ((Lisp_DynamicString *)(((char *)(s)) - sizeof(int)))

/* Define a variable V, containing a static string S. This must be cast
   to a VALUE via the VAL() macro when using. */
#define DEFSTRING(v,s) \
    char v [] CONCAT(ALIGN_, NORMAL_ALIGNMENT) = "\1" s


/* Get the beginning of the String struct from a (char *)  */
#define STRING_HDR(s) ((Lisp_String *)(((char *)(s))-1))

/* Find the length of this String. */
#define STRING_LEN(s)  ((VNORMAL_TYPE(s) == V_DynamicString)		\
			? (DSTRING_HDR(s)->length) : strlen(VSTR(s)))

/* True if this string may be written to; generally V_StaticString types
   are made from C string-constants and usually in read-only storage. */
#define STRING_WRITEABLE_P(s) (VNORMAL_TYPE(s) != V_StaticString)


/* Vectors */

typedef struct lisp_vector {
    u_char type;
    struct lisp_vector *next ALIGN_4;
    int size;
    VALUE array[0];
} Lisp_Vector;

#define VECT_SIZE(s) ((sizeof(VALUE) * (s)) + sizeof(Lisp_Vector))

#define VVECT(v)	((Lisp_Vector *)VPTR(v))
#define VVECTI(v,i)	(VVECT(v)->array[(i)])

#define VECTORP(v)	VNORMAL_TYPEP(v, V_Vector)


/* Symbols */

/* Symbol object, each symbol has 4 basic attributes, a name, its value
   as a variable, its value as a function and a property-list.
   Symbols are generally stored in hash tables (obarray) with collisions
   chained from the `sym_Next' field.  */
typedef struct {
    u_char type;
    u_char flags;
    VALUE next ALIGN_4;			/* next symbol in obarray bucket */
    VALUE name;
    VALUE value;
    VALUE function;
    VALUE prop_list;
} Lisp_Symbol;

/* This bit set in flags means that the value is a constant, and therefore
   can't be modified. */
#define SF_CONSTANT	1

/* Means that the symbol's value may be in the buffer-local storage, if so
   then that occurrence takes precedence. */
#define SF_BUFFER_LOCAL 2

/* This means that setting the value of the symbol always sets the
   buffer-local value, even if one doesn't already exist.  */
#define SF_SET_BUFFER_LOCAL 4

/* When a function is evaluated whose symbol has this bit set, the
   next evaluated form will invoke the Lisp debugger. */
#define SF_DEBUG	8

/* Symbol allocation blocks */
typedef struct lisp_symbol_block {
    struct lisp_symbol_block *next;
    Lisp_Symbol symbols[SYMBOLBLK_SIZE] CONCAT(ALIGN_, NORMAL_ALIGNMENT);
} Lisp_Symbol_Block;

#define VSYM(v)		((Lisp_Symbol *)VPTR(v))
#define SYMBOLP(v)	VNORMAL_TYPEP(v, V_Symbol)

#define NILP(v)		((v) == sym_nil)
#define LISTP(v)	(NILP(v) || CONSP(v))


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
    u_char type;
    u_char flags;
    struct lisp_file *next ALIGN_4;
    VALUE name;
    FILE *file;
} Lisp_File;

/* When this bit is set in flags, the file handle is never fclose()'d,
   i.e. this file points to something like stdin. */
#define LFF_DONT_CLOSE 1

#define VFILE(v)	((Lisp_File *)VPTR(v))
#define FILEP(v)	VNORMAL_TYPEP(v, V_File)


/* Built-in subroutines */

/* C subroutine, can take from zero to five arguments.  */
typedef struct {
    u_char type;
    union {
	VALUE (*fun0)(void);
	VALUE (*fun1)(VALUE);
	VALUE (*fun2)(VALUE, VALUE);
	VALUE (*fun3)(VALUE, VALUE, VALUE);
	VALUE (*fun4)(VALUE, VALUE, VALUE, VALUE);
	VALUE (*fun5)(VALUE, VALUE, VALUE, VALUE, VALUE);
    } fun ALIGN_4;
    VALUE name;
    int doc_index;
    VALUE int_spec;
} Lisp_Subr;

typedef struct {
    u_char type;
    void *fun ALIGN_4;
    VALUE name;
    int doc_index;
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
	
#define BUFFERP(v)	VNORMAL_TYPEP(v, V_Buffer)
#define MARKP(v)	VNORMAL_TYPEP(v, V_Mark)
#define PROCESSP(v)	VNORMAL_TYPEP(v, V_Process)
#define WINDOWP(v)	(VNORMAL_TYPEP(v, V_Window) && VWIN(v)->w_Window)
#define VIEWP(v)	(VNORMAL_TYPEP(v, V_View) && VVIEW(v)->vw_Win)
#define GLYPHTABP(v)	VNORMAL_TYPEP(v, V_GlyphTable)
#define VOIDP(v)	VNORMAL_TYPEP(v, V_Void)

/* Building lists */
#define LIST_1(v1)	       cmd_cons(v1, sym_nil)
#define LIST_2(v1,v2)	       cmd_cons(v1, LIST_1(v2))
#define LIST_3(v1,v2,v3)       cmd_cons(v1, LIST_2(v2, v3))
#define LIST_4(v1,v2,v3,v4)    cmd_cons(v1, LIST_3(v2, v3, v4))
#define LIST_5(v1,v2,v3,v4,v5) cmd_cons(v1, LIST_4(v2, v3, v4, v5))


/* Garbage collection definitions */

#define GC_NORMAL_MARKEDP(v)	(VNORMAL_TYPE(v) & GC_NORMAL_MARK_BIT)
#define GC_SET_NORMAL(v)	(VNORMAL_TYPE(v) |= GC_NORMAL_MARK_BIT)
#define GC_CLR_NORMAL(v)	(VNORMAL_TYPE(v) &= ~GC_NORMAL_MARK_BIT)

#define GC_CONS_MARKEDP(v)	(VCAR(v) & GC_CONS_MARK_BIT)
#define GC_SET_CONS(v)		(VCAR(v) |= GC_CONS_MARK_BIT)
#define GC_CLR_CONS(v)		(VCAR(v) &= ~GC_CONS_MARK_BIT)

/* True when value V has been marked. */
#define GC_MARKEDP(v)						\
    (NORMALP(v)							\
     ? VNORMAL_TYPE(v) & GC_NORMAL_MARK_BIT			\
     : (CONSP(v) ? VCAR(v) & GC_CONS_MARK_BIT : TRUE))

/* Set the mark bit of object V. */
#define GC_SET(v)		\
    do {			\
	if(NORMALP(v))		\
	    GC_SET_NORMAL(v);	\
	else if CONSP(v)	\
	    GC_SET_CONS(v);	\
    } while(0)

/* Clear the mark bit of object V. */
#define GC_CLR(v)		\
    do {			\
	if(NORMALP(v))		\
	    GC_CLR_NORMAL(v);	\
	else if(CONSP(v)	\
	    GC_CLR_CONS(v);	\
    } while(0)

/* Recursively mark object V. */
#define MARKVAL(v)					\
    do {						\
	register VALUE tem = GCREF(v);			\
	if(tem != 0 && !GC_MARKEDP(tem) && !INTP(tem))	\
	    mark_value(tem);				\
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
#define DEFUN(name,fsym,ssym,args,type,docindex)		\
    static DEFSTRING(CONCAT(ssym, __name), name);		\
    Lisp_XSubr ALIGN_4 ssym = { type, fsym, LISP_NULL,		\
				docindex, LISP_NULL };		\
    VALUE fsym args

/* Same as above but with an extra arg -- an interactive-spec string. */
#define DEFUN_INT(name,fsym,ssym,args,type,docindex,interactive)	\
    static DEFSTRING(CONCAT(ssym, __name), name);			\
    static DEFSTRING(CONCAT(ssym, __int), interactive);			\
    Lisp_XSubr ALIGN_4 ssym = { type, fsym, LISP_NULL,			\
				docindex, LISP_NULL };			\
    VALUE fsym args

/* Add a subroutine */    
#define ADD_SUBR(subr) 				\
    do {					\
	subr.name = VAL(CONCAT(subr, __name));	\
	add_subr(&subr);			\
    } while (0)

/* Add an interactive subroutine */    
#define ADD_SUBR_INT(subr)				\
    do {						\
	subr.name = VAL(CONCAT(subr, __name));		\
	subr.int_spec = VAL(CONCAT(subr, __int));	\
	add_subr(&subr);				\
    } while (0)

/* Declare a symbol stored in variable sym_X. */
#define DEFSYM(x, name) \
    VALUE CONCAT(sym_, x); static DEFSTRING(CONCAT(str_, x), name)

/* Intern a symbol stored in sym_X, whose name (a lisp string) is stored
   in str_X (i.e. declared with DEFSYM) */
#define INTERN(x) intern_static(&CONCAT(sym_, x), VAL(CONCAT(str_, x)))

/* Add an error string called err_X for symbol stored in sym_X */
#define ERROR(x) \
    cmd_put(CONCAT(sym_, x), sym_error_message, VAL(CONCAT(err_, x)))

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

#define ARG1	(find_member_by_index(args, 1))
#define ARG2	(find_member_by_index(args, 2))
#define ARG3	(find_member_by_index(args, 3))
#define ARG4	(find_member_by_index(args, 4))
#define ARG(n)	(find_member_by_index(args, n))


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
