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

/*
 * These numbers weren't just plucked from the air, they make the blocks
 * of objects fit as close as possible into powers of 2 sized blocks.
 */
#define CONSBLK_SIZE	682
#define SYMBOLBLK_SIZE	170
#define NUMBERBLK_SIZE	127
#define LPOSBLK_SIZE	170

#define OBSIZE		509

enum ValueType
{
    /* Static strings are C string constants, use the macro MKSTR to make
       them from a normal string constant.  */
    V_StaticString = 0,
    V_DynamicString,
    V_Number,
#define V_Char V_Number
    V_Cons,
    V_Vector,
    V_Symbol,
    V_Mark,
    V_Pos,
    /* SUBR with one argument, this arg is new value of variable to set the
       var, or NULL to make it return the variable's value.  */
    V_Var,
    V_Subr0,
    V_Subr1,
    V_Subr2,
    V_Subr3,
    V_Subr4,
    V_Subr5,
    V_SubrN,
    V_SF,
    V_Buffer,
#define V_TX V_Buffer
    V_Window,
    V_View,
    V_File,
    V_Process,
    V_GlyphTable,
    V_Void
};

#define VAL(x)		((VALUE)(x))
#define VPTR(v)		(v)
#define VSTRING(v)	((String *)(v))
#define VSTR(v)		(&VSTRING(v)->str_Mem[1])
#define VNUMBER(v)	((Number *)(v))
#define VNUM(v)		(VNUMBER(v)->num_Data.number)
#define VCHAR(v)	VNUM(v)
#define VCONS(v)	((Cons *)(v))
#define VCAR(v)		(VCONS(v)->cn_Car)
#define VCDR(v)		(VCONS(v)->cn_Cdr)
#define VVECT(v)	((Vector *)(v))
#define VVECTI(v,i)	(VVECT(v)->vc_Array[(i)])
#define VSYM(v)		((Symbol *)(v))
#define VMARK(v)	((Mark *)(v))
#define VLPOS(v)	((LPos *)(v))
#define VPOS(v)		(VLPOS(v)->lp_Data.pos)
#define VXSUBR(v)	((XSubr *)(v))
#define VSUBR(v)	((Subr *)(v))
#define VSUBR0FUN(v)	(VSUBR(v)->subr_Fun.fun0)
#define VSUBR1FUN(v)	(VSUBR(v)->subr_Fun.fun1)
#define VSUBR2FUN(v)	(VSUBR(v)->subr_Fun.fun2)
#define VSUBR3FUN(v)	(VSUBR(v)->subr_Fun.fun3)
#define VSUBR4FUN(v)	(VSUBR(v)->subr_Fun.fun4)
#define VSUBR5FUN(v)	(VSUBR(v)->subr_Fun.fun5)
#define VSUBRNFUN(v)	(VSUBR(v)->subr_Fun.fun1)
#define VSFFUN(v)	(VSUBR(v)->subr_Fun.fun1)
#define VVARFUN(v)	(VSUBR(v)->subr_Fun.fun1)
#define VTX(v)		((TX *)(v))
#define VBUFFER(v)	VTX(v)
#define VFILE(v)	((LFile *)(v))
#define VPROC(v)	((struct Proc *)(v))
#define VWIN(v)		((WIN *)(v))
#define VVIEW(v)	((VW *)(v))
#define VGLYPHTAB(v)	((GlyphTable *)(v))

#define VTYPE(v)	((v)->type)
#define VTYPEP(v,t)	(VTYPE(v) == (t))
#define NILP(v)		((v) == sym_nil)
#define STRINGP(v)	(VTYPEP(v, V_StaticString) || VTYPEP(v, V_DynamicString))
#define NUMBERP(v)	VTYPEP(v, V_Number)
#define CHARP(v)	NUMBERP(v)
#define CONSP(v)	VTYPEP(v, V_Cons)
#define VECTORP(v)	VTYPEP(v, V_Vector)
#define SYMBOLP(v)	VTYPEP(v, V_Symbol)
#define BUFFERP(v)	VTYPEP(v, V_Buffer)
#define POSP(v)		VTYPEP(v, V_Pos)
#define MARKP(v)	VTYPEP(v, V_Mark)
#define FILEP(v)	VTYPEP(v, V_File)
#define PROCESSP(v)	VTYPEP(v, V_Process)
#define WINDOWP(v)	(VTYPEP(v, V_Window) && VWIN(v)->w_Window)
#define VIEWP(v)	VTYPEP(v, V_View)
#define GLYPHTABP(v)	VTYPEP(v, V_GlyphTable)
#define VOIDP(v)	VTYPEP(v, V_Void)

#define GC_MARK_BIT	0x80
#define GC_MARK(v)	(VTYPE(v) & GC_MARK_BIT)
#define GC_MARKEDP(v)	(GC_MARK(v) != 0)
#define GC_SET(v)	(VTYPE(v) |= GC_MARK_BIT)
#define GC_CLR(v)	(VTYPE(v) &= ~GC_MARK_BIT)
#define MARKVAL(v)	do { if((v) && !GC_MARKEDP(v)) mark_value(v); } while(0)


typedef struct ValClass {
    /* compares two values, rc is similar to strcmp() */
    int	  (*vc_Cmp)(VALUE val1, VALUE val2);
    /* prints a textual representation of the object, not necessarily in 
       a read'able format */
    void  (*vc_Princ)(VALUE stream, VALUE obj);
    /* prints a textual representation of the object, if possible in
       a read'able format */
    void  (*vc_Print)(VALUE stream, VALUE obj);
    /* this is the name of the type */
    VALUE   vc_Name;
} ValClass;

/* The following is an array of VALCLASS structs, the array index corresponds
   to the VTF_* numbers  */
extern ValClass ValueClasses[];

/* These are also defined as functions (lower-case'd names)...  */
#define VALUE_CMP(v1,v2) ValueClasses[VTYPE(v1)].vc_Cmp(v1,v2)
#define PRINC_VAL(s,v)	ValueClasses[VTYPE(v)].vc_Princ(s,v)
#define PRINT_VAL(s,v)	ValueClasses[VTYPE(v)].vc_Print(s,v)

/* ...except these which aren't.  */
#define VALNAME(v)	(ValueClasses[VTYPE(v)].vc_Name)


/* String data types. the `String' type is what a VALUE points to, if
   the string is dynamic it gets a length field in the word before the
   String struct proper.  */
typedef struct {
    /* str_Mem[0] is type, str_Mem[1->N] is data. */
    u_char	    str_Mem[1];
} String;

typedef struct {
    int		    ds_Length;
    u_char	    ds_Mem[1];
} DynamicString;
#define DSTR_SIZE(s) (sizeof(int) + 1 + (s))

/* Get the beginning of a DynamicString from a String.  */
#define DSTRING_HDR(s) ((DynamicString *)(((char *)(s)) - sizeof(int)))

/* Make a static string from a normal C string constant, ie,
   MKSTR("foo") -> "\0foo"  */
#define MKSTR(s) (VAL(("\0" s)))

/* Get the beginning of the String struct from a (char *)  */
#define STRING_HDR(s) ((String *)(((char *)(s))-1))

/* Find the length of this String. */
#define STRING_LEN(s) \
 (VTYPEP(s, V_DynamicString) ? (DSTRING_HDR(s)->ds_Length) : strlen(VSTR(s)))

/* True if this string may be written to; generally V_StaticString types
   are made from C string-constants and usually in read-only storage. */
#define STRING_WRITEABLE_P(s) (!VTYPEP(s, V_StaticString))

	
/* Number type. Generally a 32-bit signed integer.  */
typedef struct _Number {
    u_char	    num_Type;
    union {
	long		number;
	struct _Number *next;
    }		    num_Data;
} Number;

typedef struct _NumberBlk {
    struct _NumberBlk *nb_Next;
    Number	    nb_Numbers[NUMBERBLK_SIZE];
} NumberBlk;


/* Cons-cell, a pair of VALUEs, used amongst other things to construct
   singly-linked lists (chained through the cdr, last pointer is nil).  */
typedef struct {
    u_char	    cn_Type;
    VALUE	    cn_Car;
    VALUE	    cn_Cdr;
} Cons;

typedef struct _ConsBlk {
    struct _ConsBlk *cb_Next;
    Cons	    cb_Cons[CONSBLK_SIZE];
} ConsBlk;


/* Vector of VALUEs.  */
typedef struct _Vector {
    u_char	    vc_Type;
    struct _Vector *vc_Next;
    int		    vc_Size;
    VALUE	    vc_Array[0];
} Vector;
#define VECT_SIZE(s) ((sizeof(VALUE) * (s)) + sizeof(Vector))


/* Symbol object, each symbol has 4 basic attributes, a name, its value
   as a variable, its value as a function and a property-list.
   Symbols are generally stored in hash tables (obarray) with collisions
   chained from the `sym_Next' field.  */
typedef struct _Symbol {
    u_char	sym_Type;
    u_char	sym_Flags;
    VALUE	sym_Next;	/* next symbol in obarray bucket */
    VALUE	sym_Name;
    VALUE	sym_Value;
    VALUE	sym_Function;
    VALUE	sym_PropList;
} Symbol;

#define SF_CONSTANT	1
/* Means that the symbol's value may be in the buffer-local storage, if so
   then that occurrence takes precedence. */
#define SF_BUFFER_LOCAL 2
/* This means that setting the value of the symbol always sets the
   buffer-local value, even if one doesn't already exist.  */
#define SF_SET_BUFFER_LOCAL 4
#define SF_DEBUG	8	/* Break on next lisp form. */

typedef struct _SymbolBlk {
    struct _SymbolBlk *sb_Next;
    Symbol	   sb_Symbols[SYMBOLBLK_SIZE];
} SymbolBlk;


/* Lisp version of the POS structure. */
typedef union _LPos {
    struct {
	u_char		type;
	struct POS	pos;
    }		    lp_Data;
    union _LPos	   *lp_Next;
} LPos;

typedef struct _LPosBlk {
    struct _LPosBlk *lb_Next;
    LPos	    lb_Pos[LPOSBLK_SIZE];
} LPosBlk;


/* A file object.  */
typedef struct _LFile {
    u_char	    lf_Type;
    u_char	    lf_Flags;
    struct _LFile  *lf_Next;
    VALUE	    lf_Name;
    FILE	   *lf_File;
} LFile;
#define LFF_DONT_CLOSE 1


/* C subroutine, can take from zero to five arguments.  */
typedef struct {
    u_char	    subr_Type;
    union {
	VALUE	      (*fun0)(void);
	VALUE	      (*fun1)(VALUE);
	VALUE	      (*fun2)(VALUE, VALUE);
	VALUE	      (*fun3)(VALUE, VALUE, VALUE);
	VALUE	      (*fun4)(VALUE, VALUE, VALUE, VALUE);
	VALUE	      (*fun5)(VALUE, VALUE, VALUE, VALUE, VALUE);
    }		    subr_Fun;
    VALUE	    subr_Name;
    int		    subr_DocIndex;
    VALUE	    subr_IntSpec;
} Subr;

typedef struct {
    u_char	    subr_Type;
    void	   *subr_Fun;
    VALUE	    subr_Name;
    int		    subr_DocIndex;
    VALUE	    subr_IntSpec;
} XSubr;


#define LIST_1(v1)	       cmd_cons(v1, sym_nil)
#define LIST_2(v1,v2)	       cmd_cons(v1, LIST_1(v2))
#define LIST_3(v1,v2,v3)       cmd_cons(v1, LIST_2(v2, v3))
#define LIST_4(v1,v2,v3,v4)    cmd_cons(v1, LIST_3(v2, v3, v4))
#define LIST_5(v1,v2,v3,v4,v5) cmd_cons(v1, LIST_4(v2, v3, v4, v5))


/* Keeps a backtrace of all lisp functions called. NOT primitives. */
struct LispCall {
    struct LispCall *lc_Next;
    VALUE	    lc_Fun;
    VALUE	    lc_Args;
    /* t if `lc_Args' is list of *evalled* arguments.  */
    VALUE           lc_ArgsEvalledP;
};


/* A stack of these providing additional entry points for the mark phase of
   garbage collection.  */
typedef struct _GCVAL {
    VALUE	   *gcv_Value;
    struct _GCVAL  *gcv_Next;
} GCVAL;

typedef struct _GCVALN {
    VALUE	   *gcv_First;
    int		    gcv_N;
    struct _GCVALN *gcv_Next;
} GCVALN;

#define POPGC (gcv_stack = gcv_stack->gcv_Next)
#define PUSHGC(gcv, val)			\
    do {					\
	(gcv).gcv_Value = &(val);		\
	(gcv).gcv_Next = gcv_stack;		\
	gcv_stack = &(gcv);			\
    } while(0)

#define POPGCN (gcvn_stack = gcvn_stack->gcv_Next)
#define PUSHGCN(gcv, valp, n)			\
    do {					\
	(gcv).gcv_First = (valp);		\
	(gcv).gcv_N = (n);			\
	(gcv).gcv_Next = gcvn_stack;		\
	gcvn_stack = &(gcv);			\
    } while(0)


/* Macros for defining functions and their SUBR structures. */
#define DEFUN(name,fsym,ssym,args,type,docindex)		\
    XSubr ssym = { type, fsym, MKSTR(name), docindex, NULL };	\
    VALUE fsym args

/* Same as above but with an extra arg -- an interactive-spec string. */
#define DEFUN_INT(name,fsym,ssym,args,type,docindex,interactive)	    \
    XSubr ssym = { type, fsym, MKSTR(name), docindex, MKSTR(interactive) }; \
    VALUE fsym args
    
#define ADD_SUBR(subr) add_subr(&subr)
#define ADD_CONST_NUM(name,num) add_const_num(MKSTR(name), num)
#define INTERN(sym,name) intern_static(&sym, MKSTR(name))
#define DOC_VAR(sym,docIndex) \
    cmd_put(sym, sym_variable_documentation, make_number(docIndex))


/* Macros for ensuring an object is of a certain type
   ie, to ensure first arg `foo' is a string,
     DECLARE1(foo, STRINGP);  */

#define DECLARE(n,x,t) \
    do { \
	if(! t(x)) \
	{ \
	    signal_arg_error(x, n); \
	    return(NULL); \
	} \
    } while(0)

#define DECLARE1(x,t)	    DECLARE(1,x,t)
#define DECLARE2(x,t)	    DECLARE(2,x,t)
#define DECLARE3(x,t)	    DECLARE(3,x,t)
#define DECLARE4(x,t)	    DECLARE(4,x,t)
#define DECLARE5(x,t)	    DECLARE(5,x,t)

#define ARG1	(find_member_by_index(args, 1))
#define ARG2	(find_member_by_index(args, 2))
#define ARG3	(find_member_by_index(args, 3))
#define ARG4	(find_member_by_index(args, 4))
#define ARG(n)	(find_member_by_index(args, n))


/* Macros for interrupt handling */

#ifndef TEST_INT
# define TEST_INT do { ; } while(0)
#endif

#define INT_P (throw_value != NULL)

#endif /* _LISP_H */
