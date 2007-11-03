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
# if ! defined (RTLD_DEFAULT)
#  define RTLD_DEFAULT ((void *) 0)
# endif
# if defined (BROKEN_RTLD_GLOBAL)
#  undef RTLD_GLOBAL
#  define RTLD_GLOBAL 0
# endif

#elif defined (HAVE_DL_H) || defined (HAVE_SYS_DL_H)
# if defined (HAVE_DL_H)
#  include <dl.h>
# else
#  include <sys/dl.h>
# endif
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
    repv file_name;
    repv feature_sym;
    repv structure;
    void *handle;
    rep_bool is_rep_module;
};

static int n_dl_libs, n_alloc_dl_libs;
static struct dl_lib_info *dl_libs;

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

static int
find_dl (repv file)
{
    int i;

    assert (rep_STRINGP (file));

    for (i = 0; i < n_dl_libs; i++)
    {
	assert (rep_STRINGP (dl_libs[i].file_name));
	if (!strcmp (rep_STR (file), rep_STR (dl_libs[i].file_name)))
	    return i;
    }

    return -1;
}

static int
find_dl_by_feature(repv feature)
{
    int i;

    assert (rep_STRINGP(feature));

    for (i = 0; i < n_dl_libs; i++)
    {
	if (rep_SYMBOLP (dl_libs[i].feature_sym)
	    && strcmp (rep_STR (rep_SYM (dl_libs[i].feature_sym)->name),
		       rep_STR (feature)) == 0)
	{
	    return i;
	}
    }

    return -1;
}

static rep_bool
load_requires (char *ptr)
{
    ptr += strspn (ptr, " \t");
    while (*ptr != 0)
    {
	char *end = ptr + strcspn (ptr, " \t");
	repv sym = Fintern (rep_string_dupn (ptr, end - ptr), Qnil);
	if (Fintern_structure (sym) == rep_NULL)
	    return rep_FALSE;
	ptr = end + strspn (end, " \t");
    }
    return rep_TRUE;
}

static void
signal_error (const char *msg)
{
    if (Qerror != 0)
	Fsignal (Qerror, rep_LIST_1 (rep_string_dup (msg)));
    else
	fprintf (stderr, "error: %s\n", msg);
}

int
rep_intern_dl_library (repv file_name)
{
    const char *dlname = 0;
    rep_bool open_globally = rep_FALSE;
    rep_bool is_rep_module = rep_TRUE;
    int idx;
    const char *tem;
    int len;

    idx = find_dl (file_name);
    if(idx >= 0)
	return idx;

    tem = rep_STR (file_name);
    len = strlen (tem);

    if (len >= 3 && strcmp (tem + len - 3, ".la") == 0)
    {
	/* We're trying to open a _libtool_ dl object. i.e it's a
	   file ending in .la that contains a dlname=FOO line
	   pointing to the actual DL object (in the same directory). */

	char buf[256];
	FILE *fh;

	fh = fopen(rep_STR(file_name), "r");
	if (fh == 0)
	{
	    rep_signal_file_error(file_name);
	    return -1;
	}

	while (fgets(buf, sizeof(buf), fh))
	{
	    if (strncmp("dlname='", buf, sizeof("dlname='") - 1) == 0)
	    {
		char *ptr = buf + sizeof("dlname='") - 1;
		char *base;
		char *end = strchr(ptr, '\'');
		if (end != 0 && end > ptr)
		{
		    char *name;

		    *end = 0;
		    base = strrchr(rep_STR(file_name), '/');

		    if (base == 0)
		    {
			name = alloca (strlen (ptr) + 1);
			strcpy (name, ptr);
		    }
		    else
		    {
			base++;
			name = alloca (strlen(ptr) +
				       base - rep_STR(file_name) + 1);
			memcpy(name, rep_STR(file_name),
			       base - rep_STR(file_name));
			strcpy(name + (base - rep_STR(file_name)), ptr);
		    }

		    dlname = name;
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
			return -1;
		}
	    }
	}
	fclose(fh);
    }
    else
    {
	/* not .la, assume a native library name */

	dlname = rep_STR (file_name);
	is_rep_module = rep_FALSE;
    }

    if (dlname == NULL)
    {
	char err[256];
#ifdef HAVE_SNPRINTF
	snprintf (err, sizeof (err), "Can't find dlname in %s", rep_STR (file_name));
#else
	sprintf (err, "Can't find dlname in %s", rep_STR (file_name));
#endif
	signal_error (err);
	return -1;
    }
    else
    {
	void *handle;
	rep_bool relocate_now = rep_FALSE;
	struct dl_lib_info *x;

	if (Qdl_load_reloc_now && Fsymbol_value (Qdl_load_reloc_now, Qt) != Qnil)
	{
	    relocate_now = rep_TRUE;
	}

#if defined (HAVE_DLOPEN)
	handle = dlopen(dlname, (relocate_now ? RTLD_NOW : RTLD_LAZY)
			| (open_globally ? RTLD_GLOBAL : RTLD_LOCAL));
#elif defined (HAVE_SHL_LOAD)
	/* XXX how do we open these locally/globally? */
	handle = shl_load (dlname,
			   (relocate_now ? BIND_IMMEDIATE : BIND_DEFERRED)
			   | BIND_NONFATAL | DYNAMIC_PATH, 0L);
#endif

	if(handle == NULL)
	{
	    const char *err;
#ifdef HAVE_DLERROR
	    err = dlerror();
#else
	    err = "unknown dl error";
#endif
	    if(err != 0)
		signal_error (err);
	    return -1;
	}

	if (n_alloc_dl_libs == n_dl_libs)
	{
	    int new_n = MAX (n_alloc_dl_libs * 2, 32);
	    void *ptr;

	    ptr = rep_realloc (dl_libs, new_n * sizeof (struct dl_lib_info));
	    if (ptr == NULL)
	    {
		rep_mem_error();
		dlclose(handle);
		return -1;
	    }

	    dl_libs = ptr;
	    n_alloc_dl_libs = new_n;
	}

	idx = n_dl_libs++;
	x = &dl_libs[idx];

	x->file_name = file_name;
	x->handle = handle;
	x->feature_sym = Qnil;
	x->structure = Qnil;
	x->is_rep_module = is_rep_module;

	if (is_rep_module)
	{
	    repv (*init_func)(repv);

	    init_func = x_dlsym(handle, "rep_dl_init");
	    if(init_func != 0)
	    {
		repv ret;

		ret = init_func(file_name);

		if(Qnil != rep_NULL			/* initialising */
		   && (ret == rep_NULL || ret == Qnil))
		{
		    /* error. abort abort.. */

		    --n_dl_libs;
		    dlclose(handle);
		    return -1;
		}
		else if (ret && rep_SYMBOLP(ret) && ret != Qt)
		    x->feature_sym = ret;
		else if (ret && rep_STRUCTUREP (ret))
		{
		    x->structure = ret;
		    ret = rep_STRUCTURE (ret)->name;
		    if (ret && rep_SYMBOLP (ret))
			x->feature_sym = ret;
		}
	    }
	}
    }

    return idx;
}

repv
rep_open_dl_library(repv file_name)
{
    int idx;

    idx = rep_intern_dl_library (file_name);
    if (idx < 0)
	return rep_NULL;

    if (dl_libs[idx].is_rep_module)
    {
	if (dl_libs[idx].feature_sym != Qnil && dl_libs[idx].structure == Qnil)
	{
	    /* only `provide' the feature if there's no associated
	       structure (since we haven't actually imported it) */
	    Fprovide (dl_libs[idx].feature_sym);
	}
	return dl_libs[idx].structure;
    }
    else
	return Qt;
}

void *
rep_lookup_dl_symbol (int idx, const char *name)
{
    void *handle;

    handle = (idx >= 0 && idx < n_dl_libs) ? dl_libs[idx].handle : RTLD_DEFAULT;

    return x_dlsym (handle, name);
}

void
rep_mark_dl_data(void)
{
    int i;

    for (i = 0; i < n_dl_libs; i++)
    {
	rep_MARKVAL(dl_libs[i].file_name);
	rep_MARKVAL(dl_libs[i].feature_sym);
	rep_MARKVAL(dl_libs[i].structure);
    }
}

void
rep_kill_dl_libraries(void)
{
    int i;

    for (i = 0; i < n_dl_libs; i++)
    {
	if (dl_libs[i].is_rep_module)
	{
	    void (*exit_func) (void);

	    exit_func = x_dlsym (dl_libs[i].handle, "rep_dl_kill");
	    if(exit_func != 0)
		(*exit_func) ();
	}

#if 0
	/* Closing libraries is a _bad_ idea. There's no way
	   of knowing if any pointers to their contents exist.
	   For example, it's impossible to completely expunge
	   libgtk/libgdk, since they install an atexit () handler.. */

	dlclose(x->handle);
#endif
    }

    n_dl_libs = n_alloc_dl_libs = 0;
    rep_free (dl_libs);
    dl_libs = NULL;
}

void *
rep_find_dl_symbol (repv feature, char *symbol)
{
    int idx;

    assert (rep_SYMBOLP (feature));

    idx = find_dl_by_feature (rep_SYM(feature)->name);

    if (idx <= 0)
	return NULL;

    return x_dlsym (dl_libs[idx].handle, symbol);
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
