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
#include <lib/jade_protos.h>

_PR bool find_c_symbol(void *, char **, void **);

#ifdef HAVE_DYNAMIC_LOADING

_PR void *open_dl_library(VALUE file_name);
_PR void mark_dl_data(void);
_PR void kill_dl_libraries(void);

#ifdef HAVE_DLFCN_H
# include <dlfcn.h>
#endif

#include <assert.h>

struct dl_lib_info {
    struct dl_lib_info *next;
    VALUE file_name;
    void *handle;
};

static struct dl_lib_info *dl_list;

static struct dl_lib_info *
find_dl(VALUE file)
{
    struct dl_lib_info *x = dl_list;
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
    struct dl_lib_info *x = find_dl(file_name);
    if(x == 0)
    {
	Lisp_XSubr **functions;
	VALUE (*init_func)(VALUE);
	void *handle = dlopen(VSTR(file_name), RTLD_LAZY);
	if(handle == 0)
	{
	    char *err = dlerror();
	    if(err != 0)
		cmd_signal(sym_error, LIST_1(string_dup(err)));
	    return 0;
	}
	x = str_alloc(sizeof(struct dl_lib_info));
	if(x == 0)
	{
	    mem_error();
	    return 0;
	}
	x->file_name = file_name;
	x->handle = handle;

	init_func = dlsym(handle, "jade_init");
	if(init_func != 0)
	{
	    VALUE ret = init_func(file_name);
	    if(ret == LISP_NULL)
	    {
		/* error. abort abort.. */
		str_free(x);
		dlclose(handle);
		return 0;
	    }
	}

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
	dl_list = x;
    }
    return x;
}

void
mark_dl_data(void)
{
    struct dl_lib_info *x = dl_list;
    while(x != 0)
    {
	MARKVAL(x->file_name);
	x = x->next;
    }
}

void
kill_dl_libraries(void)
{
    struct dl_lib_info *x = dl_list;
    dl_list = 0;
    while(x != 0)
    {
	struct dl_lib_info *next = x->next;
	void (*exit_func)(void) = dlsym(x->handle, "jade_kill");
	if(exit_func != 0)
	    exit_func();
	dlclose(x->handle);
	str_free(x);
	x = next;
    }
}

/* Attempt to find the name and address of the nearest symbol before or
   equal to PTR */
bool
find_c_symbol(void *ptr, char **symbol_name_p, void **symbol_addr_p)
{
    Dl_info info;
    if(dladdr(ptr, &info) != 0)
    {
	*symbol_name_p = (char *)info.dli_sname;
	*symbol_addr_p = info.dli_saddr;
	return TRUE;
    }
    else
	return FALSE;
}
	
#else /* HAVE_DYNAMIC_LOADING */

bool
find_c_symbol(void *ptr, char **name_p, void **addr_p)
{
    return FALSE;
}

#endif /* !HAVE_DYNAMIC_LOADING */
