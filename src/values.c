/* values.c -- Handling of Lisp data (includes garbage collection)
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
#include <stdlib.h>
#include <assert.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

/* #define GC_MONITOR_STK */

_PR int value_cmp(VALUE, VALUE);
_PR void princ_val(VALUE, VALUE);
_PR void print_val(VALUE, VALUE);
_PR int type_cmp(VALUE, VALUE);
_PR VALUE null_string(void);
_PR VALUE make_string(int);
_PR VALUE string_dupn(const u_char *, int);
_PR VALUE string_dup(const u_char *);
_PR VALUE concat2(u_char *, u_char *);
_PR VALUE concat3(u_char *, u_char *, u_char *);
_PR int string_cmp(VALUE, VALUE);
static void string_sweep(void);
_PR bool set_string_len(VALUE, long);
_PR int number_cmp(VALUE, VALUE);
_PR int ptr_cmp(VALUE, VALUE);
_PR void cons_free(VALUE);
static void cons_sweep(void);
_PR int cons_cmp(VALUE, VALUE);
_PR VALUE list_1(VALUE);
_PR VALUE list_2(VALUE, VALUE);
_PR VALUE list_3(VALUE, VALUE, VALUE);
_PR VALUE list_4(VALUE, VALUE, VALUE, VALUE);
_PR VALUE list_5(VALUE, VALUE, VALUE, VALUE, VALUE);
_PR VALUE make_vector(int);
static void vector_sweep(void);
_PR int vector_cmp(VALUE, VALUE);
_PR VALUE make_pos(long, long);

_PR void mark_static(VALUE *);
_PR void mark_value(VALUE);

_PR void pre_values_init (void);
_PR void values_init(void);
_PR void values_kill (void);
_PR void dumped_init(void);

#define DT_NULL { 0, 0, 0, 0, 0 }
Lisp_Type_Data data_types[V_MAX] = {
    { cons_cmp, lisp_prin, lisp_prin, cons_sweep, "cons" },
    { symbol_cmp, symbol_princ, symbol_print, symbol_sweep, "symbol" },
    { number_cmp, lisp_prin, lisp_prin, NULL, "number" },
    { vector_cmp, lisp_prin, lisp_prin, vector_sweep, "vector" },
    DT_NULL,
    { string_cmp, string_princ, string_print, string_sweep, "string" },
    DT_NULL,
    { vector_cmp, lisp_prin, lisp_prin, NULL, "bytecode-subr" },
    DT_NULL,
    { type_cmp, lisp_prin, lisp_prin, NULL, "void" },
    DT_NULL,
    { type_cmp, lisp_prin, lisp_prin, NULL, "process" },
    DT_NULL,
    { ptr_cmp, lisp_prin, lisp_prin, NULL, "var" },
    DT_NULL,
    { ptr_cmp, lisp_prin, lisp_prin, NULL, "special-form" },
    DT_NULL,
    { ptr_cmp, lisp_prin, lisp_prin, NULL, "subr-0" },
    DT_NULL,
    { ptr_cmp, lisp_prin, lisp_prin, NULL, "subr-1" },
    DT_NULL,
    { ptr_cmp, lisp_prin, lisp_prin, NULL, "subr-2" },
    DT_NULL,
    { ptr_cmp, lisp_prin, lisp_prin, NULL, "subr-3" },
    DT_NULL,
    { ptr_cmp, lisp_prin, lisp_prin, NULL, "subr-4" },
    DT_NULL,
    { ptr_cmp, lisp_prin, lisp_prin, NULL, "subr-5" },
    DT_NULL,
    { ptr_cmp, lisp_prin, lisp_prin, NULL, "subr-n" },
    DT_NULL,
    { ptr_cmp, buffer_prin, buffer_prin, buffer_sweep, "buffer" },
    DT_NULL,
    { ptr_cmp, window_prin, window_prin, window_sweep, "window" },
    DT_NULL,
    { ptr_cmp, view_prin, view_prin, view_sweep, "view" },
    DT_NULL,
    { mark_cmp, mark_prin, mark_prin, mark_sweep, "mark" },
    DT_NULL,
    { ptr_cmp, file_prin, file_prin, file_sweep, "file" },
    DT_NULL,
    { ptr_cmp, glyphtable_prin, glyphtable_prin,
      glyphtable_sweep, "glyph-table" },
};


/* General object handling */

/* Returns zero if V1 == V2, less than zero if V1 < V2, and greater than
   zero otherwise. */
int
value_cmp(VALUE v1, VALUE v2)
{
    if(v1 != LISP_NULL && v2 != LISP_NULL)
    {
	/* If the two objects are the same object then they must be
	   equivalent :-) */
	return (v1 == v2) ? 0 : VALUE_CMP(v1, v2);
    }
    return 1;
}

void
princ_val(VALUE strm, VALUE val)
{
    if(val != LISP_NULL)
	PRINC_VAL(strm, val);
}

void
print_val(VALUE strm, VALUE val)
{
    if(val != LISP_NULL)
	PRINT_VAL(strm, val);
}

int
type_cmp(VALUE val1, VALUE val2)
{
    return !(VTYPE(val1) == VTYPE(val2));
}


/* Strings */

static StrMem lisp_strmem;

DEFSTRING(null_string_const, "");

VALUE
null_string(void)
{
    return VAL(&null_string_const);
}

DEFSTRING(string_overflow, "String too long");

/* Return a string object with room for exactly LEN characters. No extra
   byte is allocated for a zero terminator; do this manually if required. */
VALUE
make_string(int len)
{
    Lisp_String *str;
    int memlen;

    if(len > LISP_MAX_STRING)
	return cmd_signal(sym_error, LIST_1(VAL(&string_overflow)));

    memlen = DSTRING_SIZE(len);
    str = sm_alloc(&lisp_strmem, memlen);
    if(str != NULL)
    {
	str->car = MAKE_STRING_CAR(len - 1);
	data_after_gc += memlen;
	return VAL(str);
    }
    return LISP_NULL;
}

VALUE
string_dupn(const u_char *src, int slen)
{
    Lisp_String *dst = VSTRING(make_string(slen + 1));
    if(dst != NULL)
    {
	memcpy(VSTR(dst), src, slen);
	VSTR(dst)[slen] = 0;
    }
    return VAL(dst);
}

VALUE
string_dup(const u_char *src)
{
    return string_dupn(src, strlen(src));
}

VALUE
concat2(u_char *s1, u_char *s2)
{
    int len = strlen(s1) + strlen(s2);
    VALUE res = make_string(len + 1);
    stpcpy(stpcpy(VSTR(res), s1), s2);
    return(res);
}
VALUE
concat3(u_char *s1, u_char *s2, u_char *s3)
{
    int len = strlen(s1) + strlen(s2) + strlen(s3);
    VALUE res = make_string(len + 1);
    stpcpy(stpcpy(stpcpy(VSTR(res), s1), s2), s3);
    return(res);
}

int
string_cmp(VALUE v1, VALUE v2)
{
    if(STRINGP(v1) && STRINGP(v2))
    {
	long len1 = STRING_LEN(v1);
	long len2 = STRING_LEN(v2);
	long tem = memcmp(VSTR(v1), VSTR(v2), MIN(len1, len2));
	return tem != 0 ? tem : (len1 - len2);
    }
    else
	return 1;
}

static void
string_sweep(void)
{
    int bucket;
    MemChunk *mlc;
    for(bucket = 0; bucket < NUMBUCKETS; bucket++)
    {
	MemChunk **freelist = &lisp_strmem.sm_MemBuckets[bucket].mbu_FreeList;
	MemBlock *mbl = (MemBlock *)lisp_strmem.sm_MemBuckets[bucket].mbu_MemBlocks.mlh_Head;
	MemBlock *nxt;
	int chnksiz = MCHNK_SIZE((bucket + 1) * GRAIN);
	int numchnks = lisp_strmem.sm_ChunksPerBlock[bucket];
	*freelist = NULL;
	lisp_strmem.sm_MemBuckets[bucket].mbu_FreeCount = 0;
	while((nxt = (MemBlock *)mbl->mbl_Node.mln_Succ))
	{
	    MemChunk *mc = mbl->mbl_Chunks;
	    int j;
	    for(j = 0; j < numchnks; j++)
	    {
		if(mc->mc_BlkType != MBT_FREE)
		{
		    register Lisp_String *ds = (Lisp_String *)mc->mc_Mem.mem;
		    if(GC_CELL_MARKEDP(VAL(ds)))
			GC_CLR_CELL(VAL(ds));
		    else
		    {
			mc->mc_BlkType = MBT_FREE;
			mc->mc_Mem.nextfree = *freelist;
			lisp_strmem.sm_MemBuckets[bucket].mbu_FreeCount++;
			*freelist = mc;
		    }
		}
		mc = (MemChunk *)((char *)mc + chnksiz);
	    }
	    mbl = nxt;
	}
    }
    mlc = lisp_strmem.sm_MallocChain;
    lisp_strmem.sm_MallocChain = NULL;
    while(mlc)
    {
	MemChunk *nxtmlc = mlc->mc_Header.next;
	register Lisp_String *ds = (Lisp_String *)mlc->mc_Mem.mem;
	if(!GC_CELL_MARKEDP(VAL(ds)))
	    sys_free(mlc);
	else
	{
	    GC_CLR_CELL(VAL(ds));
	    mlc->mc_Header.next = lisp_strmem.sm_MallocChain;
	    lisp_strmem.sm_MallocChain = mlc;
	}
	mlc = nxtmlc;
    }
}

/* Sets the length-field of the dynamic string STR to LEN. */
bool
set_string_len(VALUE str, long len)
{
    if(STRING_WRITABLE_P(str))
    {
	VSTRING(str)->car = MAKE_STRING_CAR(len);
	return TRUE;
    }
    else
	return FALSE;
}


/* Misc */

int
number_cmp(VALUE v1, VALUE v2)
{
    if(VTYPE(v1) == VTYPE(v2))
	return VINT(v1) - VINT(v2);
    else
	return 1;
}

int
ptr_cmp(VALUE v1, VALUE v2)
{
    if(VTYPE(v1) == VTYPE(v2))
	return !(VPTR(v1) == VPTR(v2));
    else
	return 1;
}


/* Cons */

static Lisp_Cons_Block *cons_block_chain;
static Lisp_Cons *cons_freelist;
static int allocated_cons, used_cons;

_PR VALUE cmd_cons(VALUE, VALUE);
DEFUN("cons", cmd_cons, subr_cons, (VALUE car, VALUE cdr), V_Subr2, DOC_cons) /*
::doc:cons::
cons CAR-VALUE CDR-VALUE

Returns a new cons-cell with car CAR-VALUE and cdr CDR-VALUE.
::end:: */
{
    Lisp_Cons *cn;
    cn = cons_freelist;
    if(cn == NULL)
    {
	Lisp_Cons_Block *cb;
#if MALLOC_ALIGNMENT >= CONS_ALIGNMENT
	cb = sys_alloc(sizeof(Lisp_Cons_Block));
#else
	cb = sys_alloc(sizeof(Lisp_Cons_Block) + CONS_ALIGNMENT - 1);
#endif
	if(cb != NULL)
	{
	    int i;
#if MALLOC_ALIGNMENT >= CONS_ALIGNMENT
	    cb->alloc_address = cb;
#else
	    void *tem = cb;
	    cb = (Lisp_Cons_Block *)(((PTR_SIZED_INT)cb)
				     & ~(CONS_ALIGNMENT - 1));
	    cb->alloc_address = tem;
#endif
	    allocated_cons += CONSBLK_SIZE;
	    cb->next = cons_block_chain;
	    cons_block_chain = cb;
	    for(i = 0; i < (CONSBLK_SIZE - 1); i++)
		cb->cons[i].cdr = CONS_VAL(&cb->cons[i + 1]);
	    cb->cons[i].cdr = 0;
	    cons_freelist = cb->cons;
	}
	else
	    return mem_error();
	cn = cons_freelist;
    }
    cons_freelist = VCONS(cn->cdr);
    cn->car = car;
    cn->cdr = cdr;
    used_cons++;
    data_after_gc += sizeof(Lisp_Cons);
    return CONS_VAL(cn);
}

void
cons_free(VALUE cn)
{
    VCDR(cn) = CONS_VAL(cons_freelist);
    cons_freelist = VCONS(cn);
    used_cons--;
}

static void
cons_sweep(void)
{
    Lisp_Cons_Block *cb = cons_block_chain;
    cons_block_chain = NULL;
    cons_freelist = NULL;
    used_cons = 0;
    while(cb != NULL)
    {
	Lisp_Cons_Block *nxt = cb->next;
	Lisp_Cons *newfree = NULL, *newfreetail = NULL, *this;
	int i, newused = 0;
	for(i = 0, this = cb->cons; i < CONSBLK_SIZE; i++, this++)
	{
	    if(!GC_CONS_MARKEDP(CONS_VAL(this)))
	    {
		if(!newfreetail)
		    newfreetail = this;
		this->cdr = CONS_VAL(newfree);
		newfree = this;
	    }
	    else
	    {
		GC_CLR_CONS(CONS_VAL(this));
		newused++;
	    }
	}
	if(newused == 0)
	{
	    /* Whole ConsBlk unused, lets get rid of it.  */
	    sys_free(cb->alloc_address);
	    allocated_cons -= CONSBLK_SIZE;
	}
	else
	{
	    if(newfreetail != NULL)
	    {
		/* Link this mini-freelist onto the main one.  */
		newfreetail->cdr = CONS_VAL(cons_freelist);
		cons_freelist = newfree;
		used_cons += newused;
	    }
	    /* Have to rebuild the ConsBlk chain as well.  */
	    cb->next = cons_block_chain;
	    cons_block_chain = cb;
	}
	cb = nxt;
    }
}

int
cons_cmp(VALUE v1, VALUE v2)
{
    int rc = 1;
    if(VTYPE(v1) == VTYPE(v2))
    {
	rc = value_cmp(VCAR(v1), VCAR(v2));
	if(!rc)
	    rc = value_cmp(VCDR(v1), VCDR(v2));
    }
    return rc;
}

VALUE
list_1(VALUE v1)
{
    return LIST_1(v1);
}

VALUE
list_2(VALUE v1, VALUE v2)
{
    return LIST_2(v1, v2);
}

VALUE
list_3(VALUE v1, VALUE v2, VALUE v3)
{
    return LIST_3(v1, v2, v3);
}

VALUE
list_4(VALUE v1, VALUE v2, VALUE v3, VALUE v4)
{
    return LIST_4(v1, v2, v3, v4);
}

VALUE
list_5(VALUE v1, VALUE v2, VALUE v3, VALUE v4, VALUE v5)
{
    return LIST_5(v1, v2, v3, v4, v5);
}


/* Vectors */

static Lisp_Vector *vector_chain;
static int used_vector_slots;

VALUE
make_vector(int size)
{
    int len = VECT_SIZE(size);
    Lisp_Vector *v = ALLOC_OBJECT(len);
    if(v != NULL)
    {
	VSET_VECT_LEN(VAL(v), size);
	v->next = vector_chain;
	vector_chain = v;
	used_vector_slots += size;
	data_after_gc += len;
    }
    return VAL(v);
}

static void
vector_sweep(void)
{
    Lisp_Vector *this = vector_chain;
    vector_chain = NULL;
    used_vector_slots = 0;
    while(this != NULL)
    {
	Lisp_Vector *nxt = this->next;
	if(!GC_CELL_MARKEDP(VAL(this)))
	    FREE_OBJECT(this);
	else
	{
	    this->next = vector_chain;
	    vector_chain = this;
	    used_vector_slots += VVECT_LEN(this);
	    GC_CLR_CELL(VAL(this));
	}
	this = nxt;
    }
}

int
vector_cmp(VALUE v1, VALUE v2)
{
    int rc = 1;
    if((VTYPE(v1) == VTYPE(v2)) && (VVECT_LEN(v1) == VVECT_LEN(v2)))
    {
	int i;
	int len = VVECT_LEN(v1);
	for(i = rc = 0; (i < len) && (rc == 0); i++)
	    rc = value_cmp(VVECTI(v1, i), VVECTI(v2, i));
    }
    return rc;
}


/* Positions */

VALUE
make_pos(long col, long row)
{
    return MAKE_POS(col, row);
}

_PR VALUE cmd_pos(VALUE, VALUE);
DEFUN("pos", cmd_pos, subr_pos, (VALUE x, VALUE y), V_Subr2, DOC_pos) /*
::doc:pos::
pos COLUMN ROW

Returns a new position object with coordinates (COLUMN , ROW).
::end:: */
{
    long col = INTP(x) ? VINT(x) : VCOL(curr_vw->vw_CursorPos);
    long row = INTP(y) ? VINT(y) : VROW(curr_vw->vw_CursorPos);
    return MAKE_POS(col ,row);
}

_PR VALUE cmd_copy_pos(VALUE pos);
DEFUN("copy-pos", cmd_copy_pos, subr_copy_pos, (VALUE pos), V_Subr1, DOC_copy_pos) /*
::doc:copy_pos::
copy-pos POS

Returns a new copy of POS.
::end:: */
{
    DECLARE1(pos, POSP);
    return MAKE_POS(VCOL(pos), VROW(pos));
}


/* Garbage collection */

#define STATIC_ROOTS 256
static VALUE *static_roots[STATIC_ROOTS];
static int next_static_root;

_PR GC_root *gc_root_stack;
_PR GC_n_roots *gc_n_roots_stack;
GC_root *gc_root_stack;
GC_n_roots *gc_n_roots_stack;

/* data_after_gc = bytes of storage used since last gc
   gc_threshold = value that data_after_gc should be before gc'ing
   idle_gc_threshold = value that DAGC should be before gc'ing in idle time
   gc_inhibit = protects against against gc in critical section when TRUE  */
_PR int data_after_gc, gc_threshold, idle_gc_threshold, gc_inhibit;
int data_after_gc, gc_threshold = 100000, idle_gc_threshold = 20000, gc_inhibit;

#ifdef GC_MONITOR_STK
static int *gc_stack_high_tide;
#endif

void
mark_static(VALUE *obj)
{
    assert(next_static_root < STATIC_ROOTS);
    static_roots[next_static_root++] = obj;
}

/* Mark a single Lisp object.
   This attempts to eliminate as much tail-recursion as possible (by
   changing the VAL and jumping back to the `again' label).

   Note that VAL must not be NULL, and must not already have been
   marked, (see the MARKVAL macro in lisp.h) */
void
mark_value(register VALUE val)
{
#ifdef GC_MONITOR_STK
    int dummy;
    /* Assumes that the stack grows downwards (towards 0) */
    if(&dummy < gc_stack_high_tide)
	gc_stack_high_tide = &dummy;
#endif

#ifdef GC_MINSTACK
    /* This is a real problem. I can't safely stop marking since this means
       that some lisp data won't have been marked and therefore the sweep
       will screw up. But if I just keep on merrily recursing I risk
       blowing the stack.  */
    if(STK_SIZE <= GC_MINSTACK)
    {
	STK_WARN("garbage-collect(major problem!)");
	/* Perhaps I should longjmp() back to the start of the gc, then quit
	   totally?  */
	return;
    }
#endif

again:
    if(INTP(val))
	return;

    if(CONSP(val))
    {
	if(CONS_WRITABLE_P(val))
	{
	    /* A cons. Attempts to walk though whole lists at a time
	       (since Lisp lists mainly link from the cdr).  */
	    GC_SET_CONS(val);
	    if(NILP(VGCDR(val)))
		/* End of a list. We can safely
		   mark the car non-recursively.  */
		val = VCAR(val);
	    else
	    {
		MARKVAL(VCAR(val));
		val = VGCDR(val);
	    }
	    if(val && !INTP(val) && !GC_MARKEDP(val))
		goto again;
	    return;
	}
	else
	{
	    /* A constant cons cell. */
	    return;
	}
    }

    /* So we know that it's a cell8 object */
    switch(VCELL8_TYPE(val))
    {
    case V_Vector:
    case V_Compiled:
#ifdef DUMPED
	/* Ensure that read-only objects aren't marked */
	if(VECTOR_WRITABLE_P(val))
#endif
	{
	    int i, len = VVECT_LEN(val);
	    GC_SET_CELL(val);
	    for(i = 0; i < len; i++)
		MARKVAL(VVECTI(val, i));
	}
	break;

    case V_Symbol:
	/* Dumped symbols are dumped read-write, so no worries.. */
	GC_SET_CELL(val);
	MARKVAL(VSYM(val)->name);
	MARKVAL(VSYM(val)->value);
	MARKVAL(VSYM(val)->function);
	MARKVAL(VSYM(val)->prop_list);
	val = VSYM(val)->next;
	if(val && !INTP(val) && !GC_MARKEDP(val))
	    goto again;
	break;

    case V_Buffer:
	GC_SET_CELL(val);
	MARKVAL(VTX(val)->tx_FileName);
	MARKVAL(VTX(val)->tx_CanonicalFileName);
	MARKVAL(VTX(val)->tx_BufferName);
	MARKVAL(VTX(val)->tx_ModeName);
	MARKVAL(VTX(val)->tx_MinorModeNameList);
	MARKVAL(VTX(val)->tx_MinorModeNameString);
	MARKVAL(VTX(val)->tx_GlyphTable);
	MARKVAL(VTX(val)->tx_UndoList);
	MARKVAL(VTX(val)->tx_ToUndoList);
	MARKVAL(VTX(val)->tx_UndoneList);
	MARKVAL(VTX(val)->tx_SavedCPos);
	MARKVAL(VTX(val)->tx_SavedWPos);
	MARKVAL(VTX(val)->tx_SavedBlockPos[0]);
	MARKVAL(VTX(val)->tx_SavedBlockPos[1]);
	val = VTX(val)->tx_LocalVariables;
	if(val && !INTP(val) && !GC_MARKEDP(val) && !NILP(val))
	    goto again;
	break;

    case V_Window:
	GC_SET_CELL(val);
	MARKVAL(VWIN(val)->w_FontName);
#ifdef HAVE_AMIGA
	MARKVAL(VWIN(val)->w_WindowSys.ws_ScreenName);
#endif
	val = VAL(VWIN(val)->w_ViewList);
	if(val != 0 && !INTP(val) && !GC_MARKEDP(val) && !NILP(val))
	    goto again;
	break;

    case V_View:
	GC_SET_CELL(val);
	MARKVAL(VAL(VVIEW(val)->vw_Tx));
	MARKVAL(VVIEW(val)->vw_BufferList);
	MARKVAL(VVIEW(val)->vw_CursorPos);
	MARKVAL(VVIEW(val)->vw_LastCursorPos);
	MARKVAL(VVIEW(val)->vw_DisplayOrigin);
	MARKVAL(VVIEW(val)->vw_BlockS);
	MARKVAL(VVIEW(val)->vw_BlockE);
	val = VAL(VVIEW(val)->vw_NextView);
	if(val != 0 && !INTP(val) && !GC_MARKEDP(val) && !NILP(val))
	    goto again;
	break;

    case V_File:
	GC_SET_CELL(val);
	MARKVAL(VFILE(val)->name);
	MARKVAL(VFILE(val)->handler);
	MARKVAL(VFILE(val)->handler_data);
	if(!LOCAL_FILE_P(val))
	    MARKVAL(VFILE(val)->file.stream);
	break;

    case V_Process:
	GC_SET_CELL(val);
#ifdef HAVE_SUBPROCESSES
	proc_mark(val);
#endif
	break;

    case V_Mark:
	GC_SET_CELL(val);
	if(!MARK_RESIDENT_P(VMARK(val)))
	{
	    /* TXs don't get marked here. They should still be able to
	       be gc'd if there's marks pointing to them. The marks will
	       just get made non-resident.  */
	    MARKVAL(VMARK(val)->file);
	}
	MARKVAL(VMARK(val)->pos);
	break;

    case V_String:
	if(!STRING_WRITABLE_P(val))
	    break;
	/* FALL THROUGH */
    case V_GlyphTable:
	GC_SET_CELL(val);
	break;

    case V_Var:
    case V_Subr0:
    case V_Subr1:
    case V_Subr2:
    case V_Subr3:
    case V_Subr4:
    case V_Subr5:
    case V_SubrN:
    case V_SF:
    }
}

_PR VALUE var_garbage_threshold(VALUE val);
DEFUN("garbage-threshold", var_garbage_threshold, subr_garbage_threshold, (VALUE val), V_Var, DOC_garbage_threshold) /*
::doc:garbage_threshold::
The number of bytes of storage which must be used before a garbage-
collection is triggered.
::end:: */
{
    return handle_var_int(val, &gc_threshold);
}

_PR VALUE var_idle_garbage_threshold(VALUE val);
DEFUN("idle-garbage-threshold", var_idle_garbage_threshold, subr_idle_garbage_threshold, (VALUE val), V_Var, DOC_idle_garbage_threshold) /*
::doc:idle_garbage_threshold::
The number of bytes of storage which must be used before a garbage-
collection is triggered when the editor is idle.
::end:: */
{
    return handle_var_int(val, &idle_gc_threshold);
}

#ifndef NO_GC_MSG
DEFSTRING(gc_start, "Garbage collecting...");
DEFSTRING(gc_done, "Garbage collecting...done.");
#endif

_PR VALUE cmd_garbage_collect(VALUE noStats);
DEFUN_INT("garbage-collect", cmd_garbage_collect, subr_garbage_collect, (VALUE noStats), V_Subr1, DOC_garbage_collect, "") /*
::doc:garbage_collect::
garbage-collect

Scans all allocated storage for unusable data, and puts it onto the free-
list. This is done automatically when the amount of storage used since the
last garbage-collection is greater than `garbage-threshold'.
::end:: */
{
    int i;
    GC_root *gc_root;
    GC_n_roots *gc_n_roots;
    WIN *win;
    struct Lisp_Call *lc;
#ifndef NO_GC_MSG
    u_char *old_msg;
    u_long old_msg_len;
    bool old_log_msgs;
#endif
#ifdef GC_MONITOR_STK
    int dummy;
    gc_stack_high_tide = &dummy;
#endif

    if(gc_inhibit)
	return(sym_nil);

#ifndef NO_GC_MSG
    old_log_msgs = log_messages;
    log_messages = FALSE;
    save_message(curr_win, &old_msg, &old_msg_len);
    cmd_message(VAL(&gc_start), sym_t);
#endif

    /* gc the undo lists */
    undo_trim();

    /* mark static objects */
    for(i = 0; i < next_static_root; i++)
	MARKVAL(*static_roots[i]);
    /* mark stack based objects protected from GC */
    for(gc_root = gc_root_stack; gc_root != 0; gc_root = gc_root->next)
	MARKVAL(*gc_root->ptr);
    for(gc_n_roots = gc_n_roots_stack; gc_n_roots != 0;
	gc_n_roots = gc_n_roots->next)
    {
	for(i = 0; i < gc_n_roots->count; i++)
	    MARKVAL(gc_n_roots->first[i]);
    }

    /* Don't want any open windows mysteriously vanishing so,  */
    win = win_chain;
    while(win != 0)
    {
	if(win->w_Window)
	    MARKVAL(VAL(win));
	win = win->w_Next;
    }

#ifdef HAVE_AMIGA
    /* Mark the strings in the menu strip.  */
    ami_mark_menus();
#endif

    /* have to mark the Lisp backtrace.	 */
    lc = lisp_call_stack;
    while(lc)
    {
	MARKVAL(lc->fun);
	MARKVAL(lc->args);
	/* don't bother marking `args_evalled_p' it's always `nil' or `t'  */
	lc = lc->next;
    }

    mark_regexp_data();

    /* Finished marking, start sweeping. */

    for(i = 0; i < V_MAX; i++)
	if(data_types[i].sweep != NULL)
	    data_types[i].sweep();

    /* This seems an ideal time to reclaim any general strings... */
    sm_flush(&main_strmem);
    flush_all_buffers();

#ifndef NO_GC_MSG
    cmd_message(VAL(&gc_done), sym_t);
    restore_message(curr_win, old_msg, old_msg_len);
    log_messages = old_log_msgs;
#endif

    data_after_gc = 0;

#ifdef GC_MONITOR_STK
    fprintf(stderr, "gc: stack usage = %d\n",
	    ((int)&dummy) - (int)gc_stack_high_tide);
#endif

    if(NILP(noStats))
    {
	return(list_3(cmd_cons(MAKE_INT(used_cons),
			       MAKE_INT(allocated_cons - used_cons)),
		      cmd_cons(MAKE_INT(used_symbols),
			       MAKE_INT(allocated_symbols - used_symbols)),
		      MAKE_INT(used_vector_slots)));
    }
    return(sym_t);
}


void
pre_values_init(void)
{
    sm_init(&lisp_strmem);
    lisp_strmem.sm_UseMallocChain = TRUE;
}

void
values_init(void)
{
    ADD_SUBR(subr_cons);
    ADD_SUBR(subr_pos);
    ADD_SUBR(subr_copy_pos);
    ADD_SUBR(subr_garbage_threshold);
    ADD_SUBR(subr_idle_garbage_threshold);
    ADD_SUBR_INT(subr_garbage_collect);
}

void
values_kill(void)
{
    Lisp_Cons_Block *cb = cons_block_chain;
    Lisp_Vector *v = vector_chain;
    while(cb != NULL)
    {
	Lisp_Cons_Block *nxt = cb->next;
	sys_free(cb->alloc_address);
	cb = nxt;
    }
    while(v != NULL)
    {
	Lisp_Vector *nxt = v->next;
	FREE_OBJECT(v);
	v = nxt;
    }
    cons_block_chain = NULL;
    vector_chain = NULL;
    sm_kill(&lisp_strmem);
}


/* Support for dumped Lisp code */

#ifdef DUMPED

void
dumped_init(void)
{
    /* Main function is to go through all dumped symbols, interning
       them, and changing LISP_NULL values to be void. */
    Lisp_Symbol *sym;

    /* But first, intern nil, it'll be filled in later. */
    sym_nil = cmd_intern_symbol(VAL(DUMPED_SYM_NIL), void_value);

    /* Initialise allocated_X counts */
    allocated_cons = &dumped_cons_end - &dumped_cons_start;
    allocated_symbols = &dumped_symbols_end - &dumped_symbols_start;
    /* ish.. */
    used_vector_slots = ((&dumped_vectors_end - &dumped_vectors_start)
			 + (&dumped_bytecode_end - &dumped_bytecode_start));

    /* Stop one symbol too early, since we've already added it (nil) */
    for(sym = &dumped_symbols_start; sym < (&dumped_symbols_end)-1; sym++)
    {
	/* Second arg is [OBARRAY], but it's only checked against
	   being a vector. */
	cmd_intern_symbol(VAL(sym), void_value);
	if(sym->value == LISP_NULL)
	    sym->value = void_value;
	if(sym->function == LISP_NULL)
	    sym->function = void_value;
	if(sym->prop_list == LISP_NULL)
	    sym->prop_list = sym_nil;
    }
}

#endif /* DUMPED */
