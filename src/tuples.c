/* tuples.c -- management of `tuples' (car and two values)

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
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define _GNU_SOURCE

#include "repint.h"

#define rep_TUPLEBLK_SIZE	680		/* ~8k */

/* Symbol allocation blocks */
typedef struct rep_tuple_block_struct rep_tuple_block;
struct rep_tuple_block_struct {
    rep_tuple_block *next;
    rep_ALIGN_CELL(rep_tuple tuples[rep_TUPLEBLK_SIZE]);
};

static rep_tuple_block *tuple_block_chain;
static rep_tuple *tuple_freelist;
int rep_allocated_tuples, rep_used_tuples;

repv
rep_make_tuple (repv car, repv a, repv b)
{
    rep_tuple *t;
    if (tuple_freelist == 0)
    {
	rep_tuple_block *sb = rep_ALLOC_CELL (sizeof (rep_tuple_block));
	if (sb != 0)
	{
	    int i;
	    rep_allocated_tuples += rep_TUPLEBLK_SIZE;
	    sb->next = tuple_block_chain;
	    tuple_block_chain = sb;
	    for (i = 0; i < (rep_TUPLEBLK_SIZE - 1); i++)
	    {
		sb->tuples[i].a = rep_VAL (&sb->tuples[i + 1]);
		sb->tuples[i].car = 0;
	    }
	    sb->tuples[i].a = rep_VAL (tuple_freelist);
	    sb->tuples[i].car = 0;
	    tuple_freelist = sb->tuples;
	}
	else
	    abort ();
    }
    t = tuple_freelist;
    tuple_freelist = rep_TUPLE (t->a);
    t->car = car;
    t->a = a;
    t->b = b;
    rep_used_tuples++;
    rep_data_after_gc += sizeof (rep_tuple);
    return rep_VAL (t);
}

void
rep_mark_tuple (repv t)
{
    rep_MARKVAL (rep_TUPLE (t)->a);
    rep_MARKVAL (rep_TUPLE (t)->b);
}

void
rep_sweep_tuples (void)
{
    rep_tuple_block *sb;
    rep_tuple *tem_freelist = 0;
    int tem_used = 0;
    for (sb = tuple_block_chain; sb != 0; sb = sb->next)
    {
	rep_tuple *this = sb->tuples;
	rep_tuple *last = &(sb->tuples[rep_TUPLEBLK_SIZE]);
	while (this < last)
	{
	    if (!rep_GC_CELL_MARKEDP (rep_VAL (this)))
	    {
		this->a = rep_VAL (tem_freelist);
		tem_freelist = this;
	    }
	    else
	    {
		rep_GC_CLR_CELL (rep_VAL (this));
		tem_used++;
	    }
	    this++;
	}
    }
    tuple_freelist = tem_freelist;
    rep_used_tuples = tem_used;
}

void
rep_tuples_kill(void)
{
    rep_tuple_block *sb = tuple_block_chain;
    while (sb != 0)
    {
	rep_tuple_block *nxt = sb->next;
	rep_FREE_CELL (sb);
	sb = nxt;
    }
    tuple_block_chain = NULL;
}
