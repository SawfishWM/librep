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
_PR int nil_cmp(VALUE, VALUE);
_PR VALUE make_string(int);
_PR VALUE string_dupn(const u_char *, int);
_PR VALUE string_dup(const u_char *);
_PR int string_cmp(VALUE, VALUE);
_PR bool set_string_len(VALUE, long);
_PR int number_cmp(VALUE, VALUE);
_PR int ptr_cmp(VALUE, VALUE);
_PR void cons_free(VALUE);
_PR int cons_cmp(VALUE, VALUE);
_PR VALUE list_1(VALUE);
_PR VALUE list_2(VALUE, VALUE);
_PR VALUE list_3(VALUE, VALUE, VALUE);
_PR VALUE list_4(VALUE, VALUE, VALUE, VALUE);
_PR VALUE list_5(VALUE, VALUE, VALUE, VALUE, VALUE);
_PR VALUE make_vector(int);
_PR VALUE make_pos(long, long);
_PR int pos_cmp(VALUE, VALUE);
_PR void pos_prin(VALUE, VALUE);
_PR int vector_cmp(VALUE, VALUE);

_PR void mark_static(VALUE *);
_PR void mark_value(VALUE);

_PR void values_init (void);
_PR void values_init2(void);
_PR void values_kill (void);

Lisp_Type_Data data_types[] = {
    { cons_cmp, lisp_prin, lisp_prin, "cons" },
    { string_cmp, string_princ, string_print, "string" },
    { string_cmp, string_princ, string_print, "string" },
    { vector_cmp, lisp_prin, lisp_prin, "vector" },
    { number_cmp, lisp_prin, lisp_prin, "number" },
    { symbol_cmp, symbol_princ, symbol_print, "symbol" },
    { nil_cmp, lisp_prin, lisp_prin, "void" },
    { ptr_cmp, lisp_prin, lisp_prin, "var" },
    { ptr_cmp, lisp_prin, lisp_prin, "subr-0" },
    { ptr_cmp, lisp_prin, lisp_prin, "subr-1" },
    { ptr_cmp, lisp_prin, lisp_prin, "subr-2" },
    { ptr_cmp, lisp_prin, lisp_prin, "subr-3" },
    { ptr_cmp, lisp_prin, lisp_prin, "subr-4" },
    { ptr_cmp, lisp_prin, lisp_prin, "subr-5" },
    { ptr_cmp, lisp_prin, lisp_prin, "subr-n" },
    { ptr_cmp, lisp_prin, lisp_prin, "special-form" },
    { ptr_cmp, buffer_prin, buffer_prin, "buffer" },
    { ptr_cmp, window_prin, window_prin, "window" },
    { ptr_cmp, view_prin, view_prin, "view" },
    { mark_cmp, mark_prin, mark_prin, "mark" },
    { file_cmp, file_prin, file_prin, "file" },
#ifdef HAVE_SUBPROCESSES
    { ptr_cmp, proc_prin, proc_prin, "process" },
#else
    { nil_cmp, lisp_prin, lisp_prin, "process" },
#endif
    { ptr_cmp, glyphtable_prin, glyphtable_prin, "glyph-table" },
};


/* General object handling */

int
value_cmp(VALUE v1, VALUE v2)
{
    if(v1 && v2)
    {
	/* If the two objects are the same object then they must be
	   equivalent :-) */
	return(v1 == v2 ? 0 : VALUE_CMP(v1, v2));
    }
    return(1);
}

void
princ_val(VALUE strm, VALUE val)
{
    if(val)
	PRINC_VAL(strm, val);
}

void
print_val(VALUE strm, VALUE val)
{
    if(val)
	PRINT_VAL(strm, val);
}

int
nil_cmp(VALUE val1, VALUE val2)
{
    if(VTYPE(val1) == VTYPE(val2))
	return(0);
    return(1);
}


/* Strings */

static StrMem lisp_strmem;
_PR char null_string[];
DEFSTRING(null_string, "");

/* Return a string object with room for exactly LEN characters. No extra
   byte is allocated for a zero terminator; do this manually if required. */
VALUE
make_string(int len)
{
    Lisp_DynamicString *str;
    int memlen = DSTR_SIZE(len);
    str = sm_alloc(&lisp_strmem, memlen);
    if(str)
    {
	str->length = len - 1;
	str->data[0] = V_DynamicString;
	data_after_gc += memlen;
	return(VAL(&str->data[0]));
    }
    return LISP_NULL;
}

VALUE
string_dupn(const u_char *src, int slen)
{
    Lisp_String *dst = VSTRING(make_string(slen + 1));
    if(dst)
    {
	memcpy(dst->data + 1, src, slen);
	dst->data[slen+1] = 0;
    }
    return(VAL(dst));
}

VALUE
string_dup(const u_char * src)
{
    return(string_dupn(src, strlen(src)));
}

int
string_cmp(VALUE v1, VALUE v2)
{
    if(STRINGP(v1) && STRINGP(v2))
	return(strcmp(VSTR(v1), VSTR(v2)));
    return(1);
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
	while((nxt = (MemBlock *)mbl->mbl_Node.mln_Succ))
	{
	    MemChunk *mc = mbl->mbl_Chunks;
	    int j;
	    for(j = 0; j < numchnks; j++)
	    {
		if(mc->mc_BlkType != MBT_FREE)
		{
		    register Lisp_DynamicString *ds
		        = (Lisp_DynamicString *)mc->mc_Mem.mem;
		    if(ds->data[0] & GC_NORMAL_MARK_BIT)
			ds->data[0] &= ~GC_NORMAL_MARK_BIT;
		    else
		    {
			mc->mc_BlkType = MBT_FREE;
			mc->mc_Mem.nextfree = *freelist;
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
	register Lisp_DynamicString *ds
	    = (Lisp_DynamicString *)mlc->mc_Mem.mem;
	if(ds->data[0] == V_DynamicString)
	    myfree(mlc);
	else
	{
	    ds->data[0] = V_DynamicString;
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
    if(VNORMAL_TYPEP(str, V_DynamicString))
    {
	DSTRING_HDR(str)->length = len;
	return(TRUE);
    }
    return(FALSE);
}


/* Misc */

int
number_cmp(VALUE v1, VALUE v2)
{
    if(VTYPE(v1) == VTYPE(v2))
	return(VINT(v1) - VINT(v2));
    return(1);
}

int
ptr_cmp(VALUE v1, VALUE v2)
{
    if(VTYPE(v1) == VTYPE(v2))
	return(!(VPTR(v1) == VPTR(v2)));
    return(1);
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
    if(!cn)
    {
	Lisp_Cons_Block *cb;
#if MALLOC_ALIGNMENT >= CONS_ALIGNMENT
	cb = mymalloc(sizeof(Lisp_Cons_Block));
#else
	cb = mymalloc(sizeof(Lisp_Cons_Block) + CONS_ALIGNMENT - 1);
#endif
	if(cb)
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
    while(cb)
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
	    myfree(cb->alloc_address);
	    allocated_cons -= CONSBLK_SIZE;
	}
	else
	{
	    if(newfreetail)
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
    return(rc);
}

VALUE
list_1(VALUE v1)
{
    return(LIST_1(v1));
}

VALUE
list_2(VALUE v1, VALUE v2)
{
    return(LIST_2(v1, v2));
}

VALUE
list_3(VALUE v1, VALUE v2, VALUE v3)
{
    return(LIST_3(v1, v2, v3));
}

VALUE
list_4(VALUE v1, VALUE v2, VALUE v3, VALUE v4)
{
    return(LIST_4(v1, v2, v3, v4));
}

VALUE
list_5(VALUE v1, VALUE v2, VALUE v3, VALUE v4, VALUE v5)
{
    return(LIST_5(v1, v2, v3, v4, v5));
}


/* Vectors */

static Lisp_Vector *vector_chain;
static int used_vector_slots;

VALUE
make_vector(int size)
{
    int len = VECT_SIZE(size);
    Lisp_Vector *v = ALLOC_OBJECT(len);
    if(v)
    {
	v->type = V_Vector;
	v->next = vector_chain;
	vector_chain = v;
	v->size = size;
	used_vector_slots += size;
	data_after_gc += len;
    }
    return(VAL(v));
}

static void
vector_sweep(void)
{
    Lisp_Vector *this = vector_chain;
    vector_chain = NULL;
    used_vector_slots = 0;
    while(this)
    {
	Lisp_Vector *nxt = this->next;
	if(!GC_NORMAL_MARKEDP(VAL(this)))
	    FREE_OBJECT(this);
	else
	{
	    this->next = vector_chain;
	    vector_chain = this;
	    used_vector_slots += this->size;
	    GC_CLR_NORMAL(VAL(this));
	}
	this = nxt;
    }
}

int
vector_cmp(VALUE v1, VALUE v2)
{
    int rc = 1;
    if((VTYPE(v1) == VTYPE(v2)) && (VVECT(v1)->size == VVECT(v2)->size))
    {
	int i;
	for(i = rc = 0; (i < VVECT(v1)->size) && (!rc); i++)
	    rc = value_cmp(VVECT(v1)->array[i], VVECT(v2)->array[i]);
    }
    return(rc);
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

#define STATIC_ROOTS 128
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
    if(!NORMALP(val))
    {
	/* Either a cons cell or a number. We can ignore numbers, so */
	if(INTP(val))
	    return;

	/* Must be a cons. Attempts to walk though whole lists at a time
	   (since Lisp lists mainly link from the cdr).  */
	GC_SET_CONS(val);
	if(NILP(VCDR(val)))
	    /* End of a list. We can safely mark the car non-recursively.  */
	    val = GCREF(VCAR(val));
	else
	{
	    MARKVAL(GCREF(VCAR(val)));
	    val = VCDR(val);
	}
	if(val && !GC_MARKEDP(val))
	    goto again;
	return;
    }

    /* So we know that it's a normal object */
    switch(VNORMAL_TYPE(val))
    {
    case V_Vector:
	{
	    register int i;
	    GC_SET_NORMAL(val);
	    for(i = 0; i < VVECT(val)->size; i++)
		MARKVAL(VVECT(val)->array[i]);
	}
	break;

    case V_Symbol:
	GC_SET_NORMAL(val);
	MARKVAL(VSYM(val)->name);
	MARKVAL(VSYM(val)->value);
	MARKVAL(VSYM(val)->function);
	MARKVAL(VSYM(val)->prop_list);
	val = VSYM(val)->next;
	if(val && !GC_MARKEDP(val))
	    goto again;
	break;

    case V_Buffer:
	GC_SET_NORMAL(val);
	MARKVAL(VTX(val)->tx_FileName);
	MARKVAL(VTX(val)->tx_BufferName);
	MARKVAL(VTX(val)->tx_ModeName);
	MARKVAL(VTX(val)->tx_MinorModeNameList);
	MARKVAL(VTX(val)->tx_MinorModeNameString);
	MARKVAL(VTX(val)->tx_GlyphTable);
	MARKVAL(VTX(val)->tx_UndoList);
	MARKVAL(VTX(val)->tx_ToUndoList);
	MARKVAL(VTX(val)->tx_UndoneList);
	MARKVAL(VTX(val)->tx_ModStart);
	MARKVAL(VTX(val)->tx_ModEnd);
	MARKVAL(VTX(val)->tx_SavedCPos);
	MARKVAL(VTX(val)->tx_SavedWPos);
	MARKVAL(VTX(val)->tx_SavedBlockPos[0]);
	MARKVAL(VTX(val)->tx_SavedBlockPos[1]);
	val = VTX(val)->tx_LocalVariables;
	if(!GC_MARKEDP(val) && !NILP(val))
	    goto again;
	break;

    case V_Window:
	GC_SET_NORMAL(val);
	MARKVAL(VWIN(val)->w_FontName);
#ifdef HAVE_AMIGA
	MARKVAL(VWIN(val)->w_WindowSys.ws_ScreenName);
#endif
	val = VAL(VWIN(val)->w_ViewList);
	if(val != 0 && !GC_MARKEDP(val) && !NILP(val))
	    goto again;
	break;

    case V_View:
	GC_SET_NORMAL(val);
	MARKVAL(VAL(VVIEW(val)->vw_Tx));
	MARKVAL(VVIEW(val)->vw_BufferList);
	MARKVAL(VVIEW(val)->vw_CursorPos);
	MARKVAL(VVIEW(val)->vw_LastCursorPos);
	MARKVAL(VVIEW(val)->vw_DisplayOrigin);
	MARKVAL(VVIEW(val)->vw_LastDisplayOrigin);
	MARKVAL(VVIEW(val)->vw_BlockS);
	MARKVAL(VVIEW(val)->vw_BlockE);
	MARKVAL(VVIEW(val)->vw_LastBlockS);
	MARKVAL(VVIEW(val)->vw_LastBlockE);
	val = VAL(VVIEW(val)->vw_NextView);
	if(val != 0 && !GC_MARKEDP(val) && !NILP(val))
	    goto again;
	break;

    case V_File:
	GC_SET_NORMAL(val);
	MARKVAL(VFILE(val)->name);
	break;

    case V_Process:
	GC_SET_NORMAL(val);
#ifdef HAVE_SUBPROCESSES
	proc_mark(val);
#endif
	break;

    case V_Mark:
	GC_SET_NORMAL(val);
	if(!VMARK(val)->mk_Resident)
	{
	    /* TXs don't get marked here. They should still be able to
	       be gc'd if there's marks pointing to them. The marks will
	       just get made non-resident.  */
	    MARKVAL(VMARK(val)->mk_File.name);
	}
	MARKVAL(VMARK(val)->mk_Pos);
	break;

    case V_DynamicString:
    case V_GlyphTable:
	GC_SET_NORMAL(val);
	break;

    case V_StaticString:
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

_PR VALUE cmd_garbage_collect(VALUE noStats);
DEFUN_INT("garbage-collect", cmd_garbage_collect, subr_garbage_collect, (VALUE noStats), V_Subr1, DOC_garbage_collect, "") /*
::doc:garbage_collect::
garbage-collect

Scans all allocated storage for unusable data, and puts it onto the free-
list. This is done automatically when the amount of storage used since the
last garbage-collection is greater than `garbage-threshold'.
::end:: */
{
#ifndef NO_GC_MSG
    static DEFSTRING(gc_start, "Garbage collecting...");
    static DEFSTRING(gc_done, "Garbage collecting...done.");
#endif
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
    cmd_message(VAL(gc_start), sym_t);
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

    string_sweep();
    cons_sweep();
    vector_sweep();
    symbol_sweep();
    file_sweep();
    buffer_sweep();
    mark_sweep();
    window_sweep();
    view_sweep();
#ifdef HAVE_SUBPROCESSES
    proc_sweep();
#endif
    glyphtable_sweep();

    /* This seems an ideal time to reclaim any general strings... */
    sm_flush(&main_strmem);

#ifndef NO_GC_MSG
    cmd_message(VAL(gc_done), sym_t);
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
values_init(void)
{
    lisp_strmem.sm_UseMallocChain = TRUE;
    sm_init(&lisp_strmem);
}

void
values_init2(void)
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
    while(cb)
    {
	Lisp_Cons_Block *nxt = cb->next;
	myfree(cb->alloc_address);
	cb = nxt;
    }
    while(v)
    {
	Lisp_Vector *nxt = v->next;
	FREE_OBJECT(v);
	v = nxt;
    }
    cons_block_chain = NULL;
    vector_chain = NULL;
    sm_kill(&lisp_strmem);
}
