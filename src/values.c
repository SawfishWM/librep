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

#define STATIC_SMALL_NUMBERS 256

_PR int value_cmp(VALUE, VALUE);
_PR void princ_val(VALUE, VALUE);
_PR void print_val(VALUE, VALUE);
_PR int nil_cmp(VALUE, VALUE);
_PR VALUE make_string(int);
_PR VALUE string_dupn(const u_char *, int);
_PR VALUE string_dup(const u_char *);
_PR int string_cmp(VALUE, VALUE);
_PR bool set_string_len(VALUE, long);
_PR VALUE make_number(long);
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
_PR VALUE make_lpos(POS *);
_PR VALUE make_lpos2(long, long);
_PR int lpos_cmp(VALUE, VALUE);
_PR void lpos_prin(VALUE, VALUE);
_PR int vector_cmp(VALUE, VALUE);

_PR void mark_static(VALUE *);
_PR void mark_value(VALUE);

_PR void values_init (void);
_PR void values_init2(void);
_PR void values_kill (void);

ValClass ValueClasses[] = {
    { string_cmp, string_princ, string_print, MKSTR("string") },
    { string_cmp, string_princ, string_print, MKSTR("string") },
    { number_cmp, lisp_prin, lisp_prin, MKSTR("number") },
    { cons_cmp, lisp_prin, lisp_prin, MKSTR("cons") },
    { vector_cmp, lisp_prin, lisp_prin, MKSTR("vector") },
    { symbol_cmp, symbol_princ, symbol_print, MKSTR("symbol") },
    { mark_cmp, mark_prin, mark_prin, MKSTR("mark") },
    { lpos_cmp, lpos_prin, lpos_prin, MKSTR("pos") },
    { ptr_cmp, lisp_prin, lisp_prin, MKSTR("var") },
    { ptr_cmp, lisp_prin, lisp_prin, MKSTR("subr-0") },
    { ptr_cmp, lisp_prin, lisp_prin, MKSTR("subr-1") },
    { ptr_cmp, lisp_prin, lisp_prin, MKSTR("subr-2") },
    { ptr_cmp, lisp_prin, lisp_prin, MKSTR("subr-3") },
    { ptr_cmp, lisp_prin, lisp_prin, MKSTR("subr-4") },
    { ptr_cmp, lisp_prin, lisp_prin, MKSTR("subr-5") },
    { ptr_cmp, lisp_prin, lisp_prin, MKSTR("subr-n") },
    { ptr_cmp, lisp_prin, lisp_prin, MKSTR("special-form") },
    { ptr_cmp, buffer_prin, buffer_prin, MKSTR("buffer") },
    { ptr_cmp, window_prin, window_prin, MKSTR("window") },
    { ptr_cmp, view_prin, view_prin, MKSTR("view") },
    { file_cmp, file_prin, file_prin, MKSTR("file") },
#ifdef HAVE_SUBPROCESSES
    { ptr_cmp, proc_prin, proc_prin, MKSTR("process") },
#else
    { nil_cmp, lisp_prin, lisp_prin, MKSTR("process") },
#endif
    { ptr_cmp, glyphtable_prin, glyphtable_prin, MKSTR("glyph-table") },
    { nil_cmp, lisp_prin, lisp_prin, MKSTR("void") },
};

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

static StrMem lisp_strmem;
_PR VALUE null_string;
VALUE null_string = MKSTR("");

/* Return a string object with room for exactly LEN characters. No extra
   byte is allocated for a zero terminator; do this manually if required. */
VALUE
make_string(int len)
{
    DynamicString *str;
    int memlen = DSTR_SIZE(len);
    str = sm_alloc(&lisp_strmem, memlen);
    if(str)
    {
	str->ds_Length = len - 1;
	str->ds_Mem[0] = V_DynamicString;
	data_after_gc += memlen;
	return(VAL(&str->ds_Mem[0]));
    }
    return(NULL);
}

VALUE
string_dupn(const u_char *src, int slen)
{
    String *dst = VSTRING(make_string(slen + 1));
    if(dst)
    {
	memcpy(dst->str_Mem + 1, src, slen);
	dst->str_Mem[slen+1] = 0;
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
		    register DynamicString *ds = (DynamicString *)mc->mc_Mem.mem;
		    if(ds->ds_Mem[0] & GC_MARK_BIT)
			ds->ds_Mem[0] &= ~GC_MARK_BIT;
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
	register DynamicString *ds = (DynamicString *)mlc->mc_Mem.mem;
	if(ds->ds_Mem[0] == V_DynamicString)
	    myfree(mlc);
	else
	{
	    ds->ds_Mem[0] = V_DynamicString;
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
    if(VTYPEP(str, V_DynamicString))
    {
	DSTRING_HDR(str)->ds_Length = len;
	return(TRUE);
    }
    return(FALSE);
}

static NumberBlk *number_block_chain;
static Number *number_freelist;
static int allocated_numbers, used_numbers;

#ifdef STATIC_SMALL_NUMBERS
static Number small_numbers[STATIC_SMALL_NUMBERS];
#endif

VALUE
make_number(long n)
{
    Number *num;
#ifdef STATIC_SMALL_NUMBERS
    if((n < STATIC_SMALL_NUMBERS) && (n >= 0))
	return(VAL(&small_numbers[n]));
#endif
    if(!(num = number_freelist))
    {
	NumberBlk *nb = mymalloc(sizeof(NumberBlk));
	if(nb)
	{
	    int i;
	    allocated_numbers += NUMBERBLK_SIZE;
	    nb->nb_Next = number_block_chain;
	    number_block_chain = nb;
	    for(i = 0; i < (NUMBERBLK_SIZE - 1); i++)
		nb->nb_Numbers[i].num_Data.next = &nb->nb_Numbers[i + 1];
	    nb->nb_Numbers[i].num_Data.next = number_freelist;
	    number_freelist = nb->nb_Numbers;
	}
	num = number_freelist;
    }
    number_freelist = num->num_Data.next;
    num->num_Type = V_Number;
    num->num_Data.number = n;
    used_numbers++;
    data_after_gc += sizeof(Number);
    return(VAL(num));
}

static void
number_sweep(void)
{
    NumberBlk *nb = number_block_chain;
    int i;
    number_freelist = NULL;
    used_numbers = 0;
    while(nb)
    {
	NumberBlk *nxt = nb->nb_Next;
	for(i = 0; i < NUMBERBLK_SIZE; i++)
	{
	    if(!GC_MARKEDP(VAL(&nb->nb_Numbers[i])))
	    {
		nb->nb_Numbers[i].num_Data.next = number_freelist;
		number_freelist = &nb->nb_Numbers[i];
	    }
	    else
	    {
		GC_CLR(VAL(&nb->nb_Numbers[i]));
		used_numbers++;
	    }
	}
	nb = nxt;
    }
#ifdef STATIC_SMALL_NUMBERS
    for(i = 0; i < STATIC_SMALL_NUMBERS; i++)
	GC_CLR(VAL(&small_numbers[i]));
#endif
}

int
number_cmp(VALUE v1, VALUE v2)
{
    if(VTYPE(v1) == VTYPE(v2))
	return(VNUM(v1) - VNUM(v2));
    return(1);
}

int
ptr_cmp(VALUE v1, VALUE v2)
{
    if(VTYPE(v1) == VTYPE(v2))
	return(!(VPTR(v1) == VPTR(v2)));
    return(1);
}

static ConsBlk *cons_block_chain;
static Cons *cons_freelist;
static int allocated_cons, used_cons;

_PR VALUE cmd_cons(VALUE, VALUE);
DEFUN("cons", cmd_cons, subr_cons, (VALUE car, VALUE cdr), V_Subr2, DOC_cons) /*
::doc:cons::
cons CAR-VALUE CDR-VALUE

Returns a new cons-cell with car CAR-VALUE and cdr CDR-VALUE.
::end:: */
{
    Cons *cn;
    cn = cons_freelist;
    if(!cn)
    {
	ConsBlk *cb = mycalloc(sizeof(ConsBlk));
	if(cb)
	{
	    int i;
	    allocated_cons += CONSBLK_SIZE;
	    cb->cb_Next = cons_block_chain;
	    cons_block_chain = cb;
	    for(i = 0; i < (CONSBLK_SIZE - 1); i++)
		cb->cb_Cons[i].cn_Cdr = VAL(&cb->cb_Cons[i + 1]);
	    cb->cb_Cons[i].cn_Cdr = NULL;
	    cons_freelist = cb->cb_Cons;
	}
	cn = cons_freelist;
    }
    cons_freelist = VCONS(cn->cn_Cdr);
    cn->cn_Type = V_Cons;
    cn->cn_Car = car;
    cn->cn_Cdr = cdr;
    used_cons++;
    data_after_gc += sizeof(Cons);
    return(VAL(cn));
}

void
cons_free(VALUE cn)
{
    VCDR(cn) = VAL(cons_freelist);
    cons_freelist = VCONS(cn);
    used_cons--;
}

static void
cons_sweep(void)
{
    ConsBlk *cb = cons_block_chain;
    cons_block_chain = NULL;
    cons_freelist = NULL;
    used_cons = 0;
    while(cb)
    {
	ConsBlk *nxt = cb->cb_Next;
	Cons *newfree = NULL, *newfreetail = NULL, *this;
	int i, newused = 0;
	for(i = 0, this = cb->cb_Cons; i < CONSBLK_SIZE; i++, this++)
	{
	    if(!GC_MARKEDP(VAL(this)))
	    {
		if(!newfreetail)
		    newfreetail = this;
		this->cn_Cdr = VAL(newfree);
		newfree = this;
	    }
	    else
	    {
		GC_CLR(VAL(this));
		newused++;
	    }
	}
	if(newused == 0)
	{
	    /* Whole ConsBlk unused, lets get rid of it.  */
	    myfree(cb);
	    allocated_cons -= CONSBLK_SIZE;
	}
	else
	{
	    if(newfreetail)
	    {
		/* Link this mini-freelist onto the main one.  */
		newfreetail->cn_Cdr = VAL(cons_freelist);
		cons_freelist = newfree;
		used_cons += newused;
	    }
	    /* Have to rebuild the ConsBlk chain as well.  */
	    cb->cb_Next = cons_block_chain;
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

static Vector *vector_chain;
static int used_vector_slots;

VALUE
make_vector(int size)
{
    int len = VECT_SIZE(size);
    Vector *v = mycalloc(len);
    if(v)
    {
	v->vc_Type = V_Vector;
	v->vc_Next = vector_chain;
	vector_chain = v;
	v->vc_Size = size;
	used_vector_slots += size;
	data_after_gc += len;
    }
    return(VAL(v));
}

static void
vector_sweep(void)
{
    Vector *this = vector_chain;
    vector_chain = NULL;
    used_vector_slots = 0;
    while(this)
    {
	Vector *nxt = this->vc_Next;
	if(!GC_MARKEDP(VAL(this)))
	    myfree(this);
	else
	{
	    this->vc_Next = vector_chain;
	    vector_chain = this;
	    used_vector_slots += this->vc_Size;
	    GC_CLR(VAL(this));
	}
	this = nxt;
    }
}

int
vector_cmp(VALUE v1, VALUE v2)
{
    int rc = 1;
    if((VTYPE(v1) == VTYPE(v2)) && (VVECT(v1)->vc_Size == VVECT(v2)->vc_Size))
    {
	int i;
	for(i = rc = 0; (i < VVECT(v1)->vc_Size) && (!rc); i++)
	    rc = value_cmp(VVECT(v1)->vc_Array[i], VVECT(v2)->vc_Array[i]);
    }
    return(rc);
}

static LPosBlk *lpos_block_chain;
static LPos *lpos_free_list;
static int used_lpos, allocated_lpos;

VALUE
make_lpos(POS *pos)
{
    LPos *lp = lpos_free_list;
    if(!lp)
    {
	LPosBlk *lb = mycalloc(sizeof(LPosBlk));
	if(lb)
	{
	    int i;
	    allocated_lpos += LPOSBLK_SIZE;
	    lb->lb_Next = lpos_block_chain;
	    lpos_block_chain = lb;
	    for(i = 0; i < (LPOSBLK_SIZE - 1); i++)
		lb->lb_Pos[i].lp_Next = &lb->lb_Pos[i + 1];
	    lb->lb_Pos[i].lp_Next = lpos_free_list;
	    lpos_free_list = lb->lb_Pos;
	}
	lp = lpos_free_list;
    }
    lpos_free_list = lp->lp_Next;
    lp->lp_Data.type = V_Pos;
    if(pos)
	lp->lp_Data.pos = *pos;
    used_lpos++;
    data_after_gc += sizeof(LPos);
    return(VAL(lp));
}

VALUE
make_lpos2(long x, long y)
{
    POS tmp;
    tmp.pos_Col = x;
    tmp.pos_Line = y;
    return(make_lpos(&tmp));
}

_PR VALUE cmd_pos(VALUE, VALUE);
DEFUN("pos", cmd_pos, subr_pos, (VALUE x, VALUE y), V_Subr2, DOC_pos) /*
::doc:pos::
pos X Y

Returns a new position object with coordinates (X , Y).
::end:: */
{
    POS tmp;
    if(NUMBERP(x))
	tmp.pos_Col = VNUM(x);
    else
	tmp.pos_Col = curr_vw->vw_CursorPos.pos_Col;
    if(NUMBERP(y))
	tmp.pos_Line = VNUM(y);
    else
	tmp.pos_Line = curr_vw->vw_CursorPos.pos_Line;
    return(make_lpos(&tmp));
}

_PR VALUE cmd_copy_pos(VALUE pos);
DEFUN("copy-pos", cmd_copy_pos, subr_copy_pos, (VALUE pos), V_Subr1, DOC_copy_pos) /*
::doc:copy_pos::
copy-pos POS

Returns a new copy of POS.
::end:: */
{
    DECLARE1(pos, POSP);
    return(make_lpos(&VPOS(pos)));
}

void
lpos_prin(VALUE strm, VALUE obj)
{
    u_char tbuf[32];
    sprintf(tbuf, "#<pos %ld %ld>", VPOS(obj).pos_Col, VPOS(obj).pos_Line);
    stream_puts(strm, tbuf, -1, FALSE);
}

static void
lpos_sweep(void)
{
    LPosBlk *lb = lpos_block_chain;
    lpos_free_list = NULL;
    used_lpos = 0;
    while(lb)
    {
	int i;
	LPosBlk *nxt = lb->lb_Next;
	for(i = 0; i < LPOSBLK_SIZE; i++)
	{
	    if(!GC_MARKEDP(VAL(&lb->lb_Pos[i])))
	    {
		lb->lb_Pos[i].lp_Next = lpos_free_list;
		lpos_free_list = &lb->lb_Pos[i];
	    }
	    else
	    {
		GC_CLR(VAL(&lb->lb_Pos[i]));
		used_lpos++;
	    }
	}
	lb = nxt;
    }
}

int
lpos_cmp(VALUE v1, VALUE v2)
{
    int rc = 1;
    if(VTYPE(v2) == VTYPE(v1))
    {
	if(!(rc = VPOS(v1).pos_Line - VPOS(v2).pos_Line))
	    rc = VPOS(v1).pos_Col - VPOS(v2).pos_Col;
    }
    return(rc);
}

/*
 * Garbage Collection is here
 */
#define NUM_STATIC_OBJS 128
static VALUE *static_marks[NUM_STATIC_OBJS];
static int next_static;

_PR GCVAL *gcv_stack;
_PR GCVALN *gcvn_stack;
GCVAL *gcv_stack;
GCVALN *gcvn_stack;

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
    assert(next_static < NUM_STATIC_OBJS);
    static_marks[next_static++] = obj;
}

/* Mark a single Lisp object.
   This attempts to eliminate as much tail-recursion as possible (by
   changing the VAL and jumping back to the `again' label).  */
void
mark_value(register VALUE val)
{
#ifdef GC_MONITOR_STK
    int dummy;
    /* Assumes that the stack grows downwards (towards 0) */
    if(&dummy < gc_stack_high_tide)
	gc_stack_high_tide = &dummy;
#endif
#if 0
    /* This is done in the macro MARKVAL(), it saves an unnecessary function
       call.  */
    if((val == NULL) || GC_MARKEDP(val))
	return;
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
    switch(VTYPE(val))
    {
    case V_Cons:
	/* Attempts to walk though whole lists at a time (since Lisp
	   lists mainly link from the cdr).  */
	GC_SET(val);
	if(NILP(VCDR(val)))
	{
	    /* End of a list. We can safely mark the car non-recursively.  */
	    val = VCAR(val);
	}
	else
	{
	    MARKVAL(VCAR(val));
	    val = VCDR(val);
	}
	if(val && !GC_MARKEDP(val))
	    goto again;
	break;

    case V_Vector:
	{
	    register int i;
	    GC_SET(val);
	    for(i = 0; i < VVECT(val)->vc_Size; i++)
		MARKVAL(VVECT(val)->vc_Array[i]);
	}
	break;

    case V_Symbol:
	GC_SET(val);
	MARKVAL(VSYM(val)->sym_Name);
	MARKVAL(VSYM(val)->sym_Value);
	MARKVAL(VSYM(val)->sym_Function);
	MARKVAL(VSYM(val)->sym_PropList);
	val = VSYM(val)->sym_Next;
	if(val && !GC_MARKEDP(val))
	    goto again;
	break;

    case V_Buffer:
	GC_SET(val);
	MARKVAL(VTX(val)->tx_FileName);
	MARKVAL(VTX(val)->tx_BufferName);
	MARKVAL(VTX(val)->tx_ModeName);
	MARKVAL(VTX(val)->tx_MinorModeNameList);
	MARKVAL(VTX(val)->tx_MinorModeNameString);
	MARKVAL(VTX(val)->tx_GlyphTable);
	MARKVAL(VTX(val)->tx_UndoList);
	MARKVAL(VTX(val)->tx_ToUndoList);
	MARKVAL(VTX(val)->tx_UndoneList);
	val = VTX(val)->tx_LocalVariables;
	if(!GC_MARKEDP(val) && !NILP(val))
	    goto again;
	break;

    case V_Window:
	GC_SET(val);
	MARKVAL(VWIN(val)->w_FontName);
#ifdef HAVE_AMIGA
	MARKVAL(VWIN(val)->w_WindowSys.ws_ScreenName);
#endif
	val = VAL(VWIN(val)->w_ViewList);
	if(val != 0 && !GC_MARKEDP(val) && !NILP(val))
	    goto again;
	break;

    case V_View:
	GC_SET(val);
	MARKVAL(VAL(VVIEW(val)->vw_Tx));
	MARKVAL(VVIEW(val)->vw_BufferList);
	val = VAL(VVIEW(val)->vw_NextView);
	if(val != 0 && !GC_MARKEDP(val) && !NILP(val))
	    goto again;
	break;

    case V_File:
	GC_SET(val);
	MARKVAL(VFILE(val)->lf_Name);
	break;

    case V_Process:
	GC_SET(val);
#ifdef HAVE_SUBPROCESSES
	proc_mark(val);
#endif
	break;

    case V_Mark:
	GC_SET(val);
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
    case V_Number:
    case V_Pos:
    case V_GlyphTable:
	GC_SET(val);
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
    if(val)
    {
	if(NUMBERP(val))
	    gc_threshold = VNUM(val);
	return(NULL);
    }
    return(make_number(gc_threshold));
}

_PR VALUE var_idle_garbage_threshold(VALUE val);
DEFUN("idle-garbage-threshold", var_idle_garbage_threshold, subr_idle_garbage_threshold, (VALUE val), V_Var, DOC_idle_garbage_threshold) /*
::doc:idle_garbage_threshold::
The number of bytes of storage which must be used before a garbage-
collection is triggered when the editor is idle.
::end:: */
{
    if(val)
    {
	if(NUMBERP(val))
	    idle_gc_threshold = VNUM(val);
	return(NULL);
    }
    return(make_number(idle_gc_threshold));
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
    int i;
    GCVAL *gcv;
    GCVALN *gcvn;
    WIN *win;
    struct LispCall *lc;
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

#ifdef HAVE_SUBPROCESSES
    /* Make sure nothing plays with process structs while gc'ing  */
    protect_procs();
#endif

#ifndef NO_GC_MSG
    old_log_msgs = log_messages;
    log_messages = FALSE;
    save_message(curr_win, &old_msg, &old_msg_len);
    cmd_message(MKSTR("Garbage collecting..."), sym_t);
#endif

    /* gc the undo lists */
    undo_trim();

    /* mark static objects */
    for(i = 0; i < next_static; i++)
	MARKVAL(*static_marks[i]);
    /* mark stack based objects protected from GC */
    for(gcv = gcv_stack; gcv; gcv = gcv->gcv_Next)
	MARKVAL(*gcv->gcv_Value);
    for(gcvn = gcvn_stack; gcvn; gcvn = gcvn->gcv_Next)
    {
	for(i = 0; i < gcvn->gcv_N; i++)
	    MARKVAL(gcvn->gcv_First[i]);
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
	MARKVAL(lc->lc_Fun);
	MARKVAL(lc->lc_Args);
	/* don't bother marking `lc_ArgsEvalledP' it's always `nil' or `t'  */
	lc = lc->lc_Next;
    }

    string_sweep();
    number_sweep();
    cons_sweep();
    vector_sweep();
    lpos_sweep();
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

#ifdef HAVE_SUBPROCESSES
    /* put SIGCHLD back to normal */
    unprotect_procs();
#endif

#ifndef NO_GC_MSG
    cmd_message(MKSTR("Garbage collecting...done."), sym_t);
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
	return(list_5(cmd_cons(make_number(used_cons),
			       make_number(allocated_cons - used_cons)),
		      cmd_cons(make_number(used_numbers),
			       make_number(allocated_numbers-used_numbers-1)),
		      cmd_cons(make_number(used_symbols),
			       make_number(allocated_symbols - used_symbols)),
		      cmd_cons(make_number(used_lpos),
			       make_number(allocated_lpos - used_lpos)),
		      make_number(used_vector_slots)));
    }
    return(sym_t);
}

void
values_init(void)
{
#ifdef STATIC_SMALL_NUMBERS
    register int i;
    for(i = 0; i < STATIC_SMALL_NUMBERS; i++)
    {
	small_numbers[i].num_Type = V_Number;
	small_numbers[i].num_Data.number = i;
    }
#endif
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
    ADD_SUBR(subr_garbage_collect);
}

void
values_kill(void)
{
    ConsBlk *cb = cons_block_chain;
    NumberBlk *nb = number_block_chain;
    Vector *v = vector_chain;
    LPosBlk *lb = lpos_block_chain;
    while(cb)
    {
	ConsBlk *nxt = cb->cb_Next;
	myfree(cb);
	cb = nxt;
    }
    while(nb)
    {
	NumberBlk *nxt = nb->nb_Next;
	myfree(nb);
	nb = nxt;
    }
    while(v)
    {
	Vector *nxt = v->vc_Next;
	myfree(v);
	v = nxt;
    }
    while(lb)
    {
	LPosBlk *nxt = lb->lb_Next;
	myfree(lb);
	lb = nxt;
    }
    cons_block_chain = NULL;
    number_block_chain = NULL;
    vector_chain = NULL;
    lpos_block_chain = NULL;
    sm_kill(&lisp_strmem);
}
