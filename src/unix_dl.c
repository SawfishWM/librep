/* unix_dl.c -- Dynamic loading of C modules
   Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
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

#ifdef HAVE_DYNAMIC_LOADING

_PR void *open_dl_library(VALUE file_name);
_PR void mark_dl_data(void);

#ifdef HAVE_DLFCN_H
# include <dlfcn.h>
#endif

#include <assert.h>

struct dl_info {
    struct dl_info *next;
    VALUE file_name;
    void *handle;
};

static struct dl_info *dl_list;

static struct dl_info *
find_dl(VALUE file)
{
    struct dl_info *x = dl_list;
    assert(STRINGP(file));
    while(x != 0)
    {
	assert(STRINGP(x->file_name));
	if(!strcmp(VSTR(file), VSTR(x->file_name)))
	    return x;
	x = x->next;
    }
    return 0;
}

void *
open_dl_library(VALUE file_name)
{
    struct dl_info *x = find_dl(file_name);
    if(x == 0)
    {
	Lisp_XSubr **functions;
	void *handle = dlopen(VSTR(file_name), RTLD_LAZY);
	if(handle == 0)
	{
	    char *err = dlerror();
	    if(err != 0)
		cmd_signal(sym_error, LIST_1(string_dup(err)));
	    return 0;
	}
	x = str_alloc(sizeof(struct dl_info));
	if(x == 0)
	{
	    mem_error();
	    return 0;
	}
	x->file_name = file_name;
	x->handle = handle;

	functions = dlsym(handle, "jade_subrs");
	if(functions != 0)
	{
	    while(*functions != 0)
	    {
		add_subr(*functions);
		functions++;
	    }
	}

	x->next = dl_list;
	dl_list = x->next;
    }
    return x;
}

void
mark_dl_data(void)
{
    struct dl_info *x = dl_list;
    while(x != 0)
    {
	MARKVAL(x->file_name);
	x = x->next;
    }
}

#endif /* HAVE_DYNAMIC_LOADING */
