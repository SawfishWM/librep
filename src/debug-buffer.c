/* debug-buffer.c -- Trace recording
   Copyright (C) 1997 John Harper <john@dcs.warwick.ac.uk>
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

#define _GNU_SOURCE

#include "repint.h"

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

struct debug_buf {
    struct debug_buf *next;
    char *name;
    int size, ptr;
    rep_bool wrapped;
    char data[1];
};

#define DB_SIZE(n) (sizeof(struct debug_buf) + (n) - 1)

static struct debug_buf *db_chain;

void *
rep_db_alloc(char *name, int size)
{
    struct debug_buf *db = rep_alloc(DB_SIZE(size));
    if(db == NULL)
    {
	perror("create_debug_buf");
	abort();
    }
    db->name = name;
    db->size = size;
    db->ptr = 0;
    db->wrapped = rep_FALSE;
    db->next = db_chain;
    db_chain = db;

    return db;
}

void
rep_db_free(void *_db)
{
    struct debug_buf *db = _db;
    struct debug_buf **x = &db_chain;
    while(*x != NULL)
    {
	if(*x == db)
	{
	    *x = db->next;
	    break;
	}
	x = &((*x)->next);
    }
    rep_free(db);
}

void
rep_db_vprintf(void *_db, char *fmt, va_list args)
{
    char buf[256];
    int length;
    struct debug_buf *db = _db;

#ifdef HAVE_SNPRINTF
    vsnprintf(buf, sizeof(buf), fmt, args);
#else
    vsprintf(buf, fmt, args);
#endif
    length = strlen(buf);
    if(length > db->size - db->ptr)
    {
	int before = db->size - db->ptr;
	int after = MIN(length - before, db->size - before);
	memcpy(db->data + db->ptr, buf, before);
	memcpy(db->data, buf + before, after);
	db->ptr = after;
	db->wrapped = rep_TRUE;
    }
    else
    {
	memcpy(db->data + db->ptr, buf, length);
	db->ptr += length;
    }
}

void
rep_db_printf(void *_db, char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    rep_db_vprintf(_db, fmt, args);
    va_end(args);
}

void
rep_db_print_backtrace(void *_db, char *fun)
{
#if defined(__GNUC__) && ! defined(BROKEN_ALPHA_GCC)

#define BT_BASE 1
#define BT_DEPTH 8

    void *stack[BT_BASE+BT_DEPTH];
    int i;

    /* It seems that in Linux/egcs-1.1 __builtin_return_address()
       will segfault when reaching the top of the stack frame.
       The work-around is to see if we can get the frame address,
       if so it should be safe to go for the return address. */
# define STACK_PROBE(i)						\
    do {							\
	if(i == BT_BASE || stack[i-1] != 0)			\
	{							\
	    void *frame = __builtin_frame_address(i);		\
	    if(frame != 0)					\
		stack[i] = __builtin_return_address(i);		\
	    else						\
		stack[i] = 0;					\
	}							\
	else							\
	    stack[i] = 0;					\
    } while(0)

    /* Should be from BT_BASE to BT_BASE+BT_DEPTH-1 */
    STACK_PROBE(1);  STACK_PROBE(2);  STACK_PROBE(3);  STACK_PROBE(4);
    STACK_PROBE(5);  STACK_PROBE(6);  STACK_PROBE(7);  STACK_PROBE(8);

    rep_db_printf(_db, "\nBacktrace in `%s':\n", fun);
    for(i = BT_BASE; i < BT_BASE+BT_DEPTH && stack[i] != 0; i++)
    {
#ifdef DB_RESOLVE_SYMBOLS
	if(stack[i] == 0)
	    rep_db_printf(_db, "\t(nil)\n");
	else
	{
	    char *name;
	    void *addr;
	    if(rep_find_c_symbol(stack[i], &name, &addr))
	    {
		rep_db_printf(_db, "\t<%s+%d>\n", name,
			      ((char *)stack[i]) - ((char *)addr));
	    }
	    else
		rep_db_printf(_db, "\t0x%08lx\n", stack[i]);
	}
#else
	rep_db_printf(_db, "\t0x%08lx\n", stack[i]);
#endif
    }
#endif
}

void *
rep_db_return_address(void)
{
#if defined(__GNUC__) && ! defined(BROKEN_ALPHA_GCC)
    return __builtin_return_address(1);
#else
    return 0;
#endif
}

void
rep_db_spew(void *_db)
{
     struct debug_buf *db = _db;

     if(db->wrapped || db->ptr > 0)
     {
	 fprintf(stderr, "\nstruct debug_buf %s:\n", db->name);
	 if(db->wrapped)
	 {
	     fwrite(db->data + db->ptr, 1, db->size - db->ptr, stderr);
	     fwrite(db->data, 1, db->ptr, stderr);
	 }
	 else
	     fwrite(db->data, 1, db->ptr, stderr);
     }
}

void
rep_db_spew_all(void)
{
    struct debug_buf *db = db_chain;
    while(db != NULL)
    {
	rep_db_spew(db);
	db = db->next;
    }
}

void
rep_db_kill(void)
{
    struct debug_buf *db = db_chain;
    rep_db_spew_all();
    db_chain = NULL;
    while(db != NULL)
    {
	struct debug_buf *next = db->next;
	rep_free(db);
	db = next;
    }
}
