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

#include "jade.h"
#include "jade_protos.h"
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
    bool wrapped;
    char data[1];
};

#define DB_SIZE(n) (sizeof(struct debug_buf) + (n) - 1)

_PR void *db_alloc(char *name, int size);
_PR void db_free(void *db);
_PR void db_printf(void *db, char *fmt, ...);
_PR void db_spew(void *_db);
_PR void db_spew_all(void);
_PR void db_kill(void);

static struct debug_buf *db_chain;

void *
db_alloc(char *name, int size)
{
    struct debug_buf *db = malloc(DB_SIZE(size));
    if(db == NULL)
    {
	perror("create_debug_buf");
	abort();
    }
    db->name = name;
    db->size = size;
    db->ptr = 0;
    db->wrapped = FALSE;
    db->next = db_chain;
    db_chain = db;

    return db;
}

void
db_free(void *_db)
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
    free(db);
}

void
db_printf(void *_db, char *fmt, ...)
{
    va_list args;
    char buf[256];
    int length;
    struct debug_buf *db = _db;

    va_start(args, fmt);

    vsprintf(buf, fmt, args);
    length = strlen(buf);
    if(length > db->size - db->ptr)
    {
	int before = db->size - db->ptr;
	int after = MIN(length - before, db->size - before);
	memcpy(db->data + db->ptr, buf, before);
	memcpy(db->data, buf + before, after);
	db->ptr = after;
	db->wrapped = TRUE;
    }
    else
    {
	memcpy(db->data + db->ptr, buf, length);
	db->ptr += length;
    }

    va_end(args);
}

void
db_spew(void *_db)
{
     struct debug_buf *db = _db;

     fprintf(stderr, "\nstruct debug_buf %s:\n", db->name);
     if(db->wrapped)
     {
	 fwrite(db->data + db->ptr, 1, db->size - db->ptr, stderr);
	 fwrite(db->data, 1, db->ptr, stderr);
     }
     else
	 fwrite(db->data, 1, db->ptr, stderr);
}

void
db_spew_all(void)
{
    struct debug_buf *db = db_chain;
    while(db != NULL)
    {
	db_spew(db);
	db = db->next;
    }
}

void
db_kill(void)
{
    struct debug_buf *db = db_chain;
    db_chain = NULL;
    while(db != NULL)
    {
	struct debug_buf *next = db->next;
	free(db);
	db = next;
    }
}
