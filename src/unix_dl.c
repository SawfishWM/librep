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

#define _GNU_SOURCE

/* AIX requires this to be the first thing in the file.  */
#include <config.h>
#ifdef __GNUC__
# define alloca __builtin_alloca
#else
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include "repint.h"
#include <assert.h>
#include <string.h>

/* we define some extensions to the libtool .la file. As well as using
   the dlname entry to find the .so file to open, we also look for:

     rep_open_globally=[yes|no]

	whether or not to open with RTLD_GLOBAL

     rep_requires='FEATURES...'

	FEATURES is space separated list of feature symbols.
	Each of which must be provided by a dl object. */

#ifdef HAVE_DYNAMIC_LOADING

#if defined (HAVE_DLFCN_H)
# include <dlfcn.h>
# if ! defined (RTLD_LAZY)
#  if defined (DL_LAZY)
#   define RTLD_LAZY DL_LAZY
#  else
    /* from gmodule-dl.c ``The Perl sources say, RTLD_LAZY needs to be
       defined as (1), at least for Solaris 1.'' */
#   define RTLD_LAZY 1
#  endif
# endif
# if ! defined (RTLD_GLOBAL)
#  if defined (DL_GLOBAL)
#   define RTLD_GLOBAL DL_GLOBAL
#  else
#   define RTLD_GLOBAL 0
#  endif
# endif
# if ! defined (RTLD_LOCAL)
#  if defined (DL_LOCAL)
#   define RTLD_LOCAL DL_LOCAL
#  else
#   define RTLD_LOCAL 0
#  endif
# endif
# if ! defined (RTLD_NOW)
#  if defined (DL_NOW)
#   define RTLD_NOW DL_NOW
#  else
#   define RTLD_NOW 0
#  endif
# endif
# if defined (BROKEN_RTLD_GLOBAL)
#  undef RTLD_GLOBAL
#  define RTLD_GLOBAL 0
# endif

#elif defined (HAVE_DL_H)
# include <dl.h>
# if ! defined (BIND_IMMEDIATE)
#  define BIND_IMMEDIATE 0
# endif
# if ! defined (BIND_DEFERRED)
#  define BIND_DEFERRED 0
# endif
# if ! defined (BIND_NONFATAL)
#  define BIND_NONFATAL 0
# endif
# if ! defined (DYNAMIC_PATH)
#  define DYNAMIC_PATH 0
# endif
#endif

struct dl_lib_info {
    struct dl_lib_info *next;
    repv file_name;
    repv feature_sym;
    repv structure;
    void *handle;
};

static struct dl_lib_info *dl_list;

#if !defined (HAVE_DLOPEN) && defined (HAVE_SHL_LOAD)
static inline void *
dlsym (void *handle, char *sym)
{
    void *addr;
    if (shl_findsym (&handle, sym, TYPE_UNDEFINED, &addr) == 0)
	return addr;
    else
	return 0;
}

static inline void
dlclose (void *handle)
{
    shl_unload (handle);
}
#endif

#ifndef DLSYM_NEED_USCORE
# define x_dlsym dlsym
#else
static void *
x_dlsym (void *handle, char *sym)
{
    void *ptr = 0;
    char *tem = alloca (strlen(sym) + 2);
    tem[0] = '_';
    strcpy (tem + 1, sym);
    ptr = dlsym (handle, tem);
    return ptr;
}
#endif

static struct dl_lib_info *
find_dl(repv file)
{
    struct dl_lib_info *x = dl_list;
    assert(rep_STRINGP(file));
    while(x != 0)
    {
	assert(rep_STRINGP(x->file_name));
	if(!strcmp(rep_STR(file), rep_STR(x->file_name)))
	    return x;
	x = x->next;
    }
    return 0;
}

static struct dl_lib_info *
find_dl_by_feature(repv feature)
{
    struct dl_lib_info *x = dl_list;
    assert (rep_STRINGP(feature));
    while(x != 0)
    {
	if(rep_SYMBOLP(x->feature_sym)
	   && strcmp (rep_STR(rep_SYM(x->feature_sym)->name),
		      rep_STR(feature)) == 0)
	{
	    return x;
	}
	x = x->next;
    }
    return 0;
}

static rep_bool
load_requires (char *ptr)
{
    ptr += strspn (ptr, " \t");
    while (*ptr != 0)
    {
	char *end = ptr + strcspn (ptr, " \t");
	repv sym = Fintern (rep_string_dupn (ptr, end - ptr), Qnil);
	if (F_intern_structure (sym) == rep_NULL)
	    return rep_FALSE;
	ptr = end + strspn (end, " \t");
    }
    return rep_TRUE;
}

static void
signal_error (char *msg)
{
    if (Qerror != 0)
	Fsignal (Qerror, rep_LIST_1 (rep_string_dup (msg)));
    else
	fprintf (stderr, "error: %s\n", msg);
}

repv
rep_open_dl_library(repv file_name)
{
    struct dl_lib_info *x = find_dl(file_name);
    if(x == 0)
    {
	/* We're trying to open a _libtool_ dl object. i.e it's a
	   file ending in .la that contains a dlname=FOO line
	   pointing to the actual DL object (in the same directory). */

	char buf[256];
	char *dlname = 0;
	rep_bool open_globally = rep_FALSE;
	FILE *fh = fopen(rep_STR(file_name), "r");
	if (fh == 0)
	{
	    rep_signal_file_error(file_name);
	    return 0;
	}
	while (fgets(buf, sizeof(buf), fh))
	{
	    if (strncmp("dlname='", buf, sizeof("dlname='") - 1) == 0)
	    {
		char *ptr = buf + sizeof("dlname='") - 1;
		u_char *base;
		char *end = strchr(ptr, '\'');
		if (end != 0 && end > ptr)
		{
		    *end = 0;
		    base = strrchr(rep_STR(file_name), '/');
		    if (base == 0)
		    {
			dlname = alloca (strlen (ptr) + 1);
			strcpy (dlname, ptr);
		    }
		    else
		    {
			base++;
			dlname = alloca (strlen(ptr) +
					 base - rep_STR(file_name) + 1);
			memcpy(dlname, rep_STR(file_name),
			       base - rep_STR(file_name));
			strcpy(dlname + (base - rep_STR(file_name)), ptr);
		    }
		}
	    }
	    else if (strncmp("rep_open_globally=", buf,
			     sizeof("rep_open_globally=") - 1) == 0)
	    {
		char *ptr = buf + sizeof ("rep_open_globally=") - 1;
		if (strncmp ("yes", ptr, 3) == 0)
		    open_globally = rep_TRUE;
	    }
	    else if (strncmp("rep_requires='", buf,
			     sizeof ("rep_requires='") - 1) == 0)
	    {
		char *ptr = buf + sizeof ("rep_requires='") - 1;
		char *end = strchr (ptr, '\'');
		if (end != 0)
		{
		    rep_GC_root gc_file_name;
		    rep_bool success;
		    char *string = alloca (end - ptr + 1);
		    memcpy (string, ptr, end - ptr);
		    string[end - ptr] = 0;
		    rep_PUSHGC (gc_file_name, file_name);
		    success = load_requires (string);
		    rep_POPGC;
		    if (!success)
			goto out;
		}
	    }
	}
	fclose(fh);
	if (!dlname)
	    signal_error ("Can't find dlname in .la object");
	else
	{
	    rep_xsubr **functions;
	    repv (*init_func)(repv);
	    repv *feature_sym;
	    void *handle;
	    rep_bool relocate_now = rep_FALSE;
	    if (Qdl_load_reloc_now
		&& Fsymbol_value (Qdl_load_reloc_now, Qt) != Qnil)
	    {
		relocate_now = rep_TRUE;
	    }

#if defined (HAVE_DLOPEN)
	    handle = dlopen(dlname,
			    (relocate_now ? RTLD_NOW : RTLD_LAZY)
			    | (open_globally ? RTLD_GLOBAL : RTLD_LOCAL));
#elif defined (HAVE_SHL_LOAD)
	    /* XXX how do we open these locally/globally? */
	    handle = shl_load (dlname,
			       (relocate_now ? BIND_IMMEDIATE : BIND_DEFERRED)
			       | BIND_NONFATAL | DYNAMIC_PATH, 0L);
#endif
	    if(handle == 0)
	    {
		char *err;
#ifdef HAVE_DLERROR
		err = dlerror();
#else
		err = "unknown dl error";
#endif
		if(err != 0)
		    signal_error (err);
		return 0;
	    }
	    x = rep_alloc(sizeof(struct dl_lib_info));
	    if(x == 0)
	    {
		rep_mem_error();
		dlclose(handle);
		return 0;
	    }
	    x->file_name = file_name;
	    x->handle = handle;
	    x->feature_sym = Qnil;
	    x->structure = Qnil;

	    x->next = dl_list;
	    dl_list = x;

	    init_func = x_dlsym(handle, "rep_dl_init");
	    if(init_func != 0)
	    {
		repv ret = init_func(file_name);
		if(Qnil != rep_NULL			/* initialising */
		   && (ret == rep_NULL || ret == Qnil))
		{
		    /* error. abort abort.. */
		    struct dl_lib_info **ptr;
		    for (ptr = &dl_list; *ptr != 0; ptr = &((*ptr)->next))
		    {
			if (*ptr == x)
			    *ptr = x->next;
			break;
		    }		    
		    rep_free(x);
		    dlclose(handle);
		    return 0;
		}
		else if (ret && rep_SYMBOLP(ret) && ret != Qt)
		    x->feature_sym = ret;
		else if (ret && rep_STRUCTUREP (ret))
		{
		    x->structure = ret;
		    ret = F_structure_name (ret);
		    if (ret && rep_SYMBOLP (ret))
			x->feature_sym = ret;
		}
	    }

	    feature_sym = x_dlsym (handle, "rep_dl_feature");
	    if (feature_sym != 0
		&& *feature_sym != 0 && rep_SYMBOLP(*feature_sym))
	    {
		x->feature_sym = *feature_sym;
	    }

	    functions = x_dlsym(handle, "rep_dl_subrs");
	    if(functions != 0)
	    {
		while(*functions != 0)
		{
		    rep_add_subr(*functions, rep_TRUE);
		    functions++;
		}
	    }
	}
    }
out:

    if (x != 0)
    {
	if (x->feature_sym != Qnil && x->structure == Qnil)
	{
	    /* only `provide' the feature if there's no associated
	       structure (since we haven't actually imported it) */
	    Fprovide (x->feature_sym);
	}
	return x->structure;
    }
    else
	return rep_NULL;
}

void
rep_mark_dl_data(void)
{
    struct dl_lib_info *x = dl_list;
    while(x != 0)
    {
	rep_MARKVAL(x->file_name);
	rep_MARKVAL(x->feature_sym);
	rep_MARKVAL(x->structure);
	x = x->next;
    }
}

void
rep_kill_dl_libraries(void)
{
    struct dl_lib_info *x = dl_list;
    dl_list = 0;
    while(x != 0)
    {
	struct dl_lib_info *next = x->next;
	void (*exit_func)(void) = x_dlsym(x->handle, "rep_dl_kill");
	if(exit_func != 0)
	    exit_func();
#if 0
	/* Closing libraries is a _bad_ idea. There's no way
	   of knowing if any pointers to their contents exist.
	   For example, it's impossible to completely expunge
	   libgtk/libgdk, since they install an atexit () handler.. */
	dlclose(x->handle);
#endif
	rep_free(x);
	x = next;
    }
}

void *
rep_find_dl_symbol (repv feature, char *symbol)
{
    struct dl_lib_info *x;
    assert(rep_SYMBOLP(feature));
    x = find_dl_by_feature (rep_SYM(feature)->name);
    if (x != 0)
	return x_dlsym (x->handle, symbol);
    else
	return 0;
}

/* Attempt to find the name and address of the nearest symbol before or
   equal to PTR */
rep_bool
rep_find_c_symbol(void *ptr, char **symbol_name_p, void **symbol_addr_p)
{
#ifdef HAVE_DLADDR
    Dl_info info;
    if(dladdr(ptr, &info) != 0)
    {
	*symbol_name_p = (char *)info.dli_sname;
	*symbol_addr_p = info.dli_saddr;
	return rep_TRUE;
    }
    else
#endif
	return rep_FALSE;
}
	
#else /* HAVE_DYNAMIC_LOADING */

rep_bool
rep_find_c_symbol(void *ptr, char **name_p, void **addr_p)
{
    return rep_FALSE;
}

#endif /* !HAVE_DYNAMIC_LOADING */
