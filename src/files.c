/* files.c -- Extendable file handling
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

#include "repint.h"

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#ifdef NEED_MEMORY_H
# include <memory.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifndef DEV_SLASH_NULL
# define DEV_SLASH_NULL "/dev/null"
#endif

/* List of operations. If there's a file handler defined for the file
   being manipulated it will be called to execute the operation.

   (file-name-absolute-p NAME)
   (expand-file-name NAME)
   (local-file-name NAME)
   (canonical-file-name NAME)

   (file-name-nondirectory NAME)
   (file-name-directory NAME)
   (file-name-as-directory NAME)
   (directory-file-name NAME)

   (open-file NAME ACCESS-TYPE)
   (close-file FILE)
   (flush-file FILE)
   (seek-file FILE [OFFSET] [WHENCE])

   [ XXX these are for jade only, must be defined later.. ]
   (write-buffer-contents FILE-OR-NAME START END)
   (read-file-contents FILE-OR-NAME)
   (insert-file-contents FILE-OR-NAME)

   (delete-file NAME)
   (rename-file OLD-NAME NEW-NAME)
   (copy-file SOURCE DEST)
   (copy-file-to-local-fs SOURCE LOCAL-DEST)
   (copy-file-from-local-fs LOCAL-SOURCE DEST)
   (make-directory NAME)
   (delete-directory NAME)

   (file-exists-p NAME)
   (file-regular-p NAME)
   (file-readable-p NAME)
   (file-writable-p NAME)
   (file-directory-p NAME)
   (file-symlink-p NAME)
   (file-owner-p NAME)
   (file-nlinks NAME)
   (file-size NAME)
   (file-modes NAME)
   (file-modes-as-string NAME)
   (set-file-modes NAME MODES)
   (file-modtime NAME)
   (directory-files NAME)
   (read-symlink NAME)
   (make-symlink NAME CONTENTS)

   ACCESS-TYPE is one of `read', `write' or `append'.
   WHENCE is one off `nil', `start', `end'. */

DEFSYM(file_handler_alist, "file-handler-alist"); /*
::doc:file-handler-alist::
a list of `(REGEXP . HANDLER)'. If REGEXP matches the name of a file
being manipulated the function HANDLER is called as (HANDLER OPERATION
ARGS...) where ARGS matches how the original function is called.
::end:: */

DEFSYM(default_directory, "default-directory"); /*
::doc:default-directory::
Buffer-local variable absolutely defining the directory to which all files
accessed in the buffer are resolved from (unless they're absolute.) 
::end:: */

/* List of all allocated file objects */
static rep_file *file_list;

int rep_file_type;

DEFSYM(file_name_absolute_p, "file-name-absolute-p");
DEFSYM(expand_file_name, "expand-file-name");
DEFSYM(local_file_name, "local-file-name");
DEFSYM(canonical_file_name, "canonical-file-name");
DEFSYM(file_name_nondirectory, "file-name-nondirectory");
DEFSYM(file_name_directory, "file-name-directory");
DEFSYM(file_name_as_directory, "file-name-as-directory");
DEFSYM(directory_file_name, "directory-file-name");
DEFSYM(open_file, "open-file");
DEFSYM(close_file, "close-file");
DEFSYM(flush_file, "flush-file");
DEFSYM(seek_file, "seek-file");
DEFSYM(delete_file, "delete-file");
DEFSYM(rename_file, "rename-file");
DEFSYM(make_directory, "make-directory");
DEFSYM(delete_directory, "delete-directory");
DEFSYM(copy_file, "copy-file");
DEFSYM(copy_file_to_local_fs, "copy-file-to-local-fs");
DEFSYM(copy_file_from_local_fs, "copy-file-from-local-fs");
DEFSYM(file_readable_p, "file-readable-p");
DEFSYM(file_writable_p, "file-writable-p");
DEFSYM(file_executable_p, "file-executable-p");
DEFSYM(file_exists_p, "file-exists-p");
DEFSYM(file_regular_p, "file-regular-p");
DEFSYM(file_directory_p, "file-directory-p");
DEFSYM(file_symlink_p, "file-symlink-p");
DEFSYM(file_owner_p, "file-owner-p");
DEFSYM(file_gid_p, "file-gid-p");
DEFSYM(file_uid_p, "file-uid-p");
DEFSYM(file_nlinks, "file-nlinks");
DEFSYM(file_size, "file-size");
DEFSYM(file_modes, "file-modes");
DEFSYM(set_file_modes, "set-file-modes");
DEFSYM(file_modes_as_string, "file-modes-as-string");
DEFSYM(file_modtime, "file-modtime");
DEFSYM(directory_files, "directory-files");
DEFSYM(read_symlink, "read-symlink");
DEFSYM(make_symlink, "make-symlink");

DEFSYM(start, "start");
DEFSYM(end, "end");

DEFSYM(read, "read");
DEFSYM(write, "write");
DEFSYM(append, "append");

DEFSYM(fh_env_key, "fh-env-key");

/* Vector of blocked operations */
struct blocked_op *rep_blocked_ops[op_MAX];

int rep_op_write_buffer_contents = op_write_buffer_contents;
int rep_op_read_file_contents = op_read_file_contents;
int rep_op_insert_file_contents = op_insert_file_contents;



DEFSYM (rep_io_file_handlers, "rep.io.file-handlers");

static inline repv
get_fh_env (void)
{
    repv ret = F_structure_ref (rep_structure, Qfh_env_key);
    return rep_VOIDP (ret) ? Qt : ret;
}

/* this is duplicated in rep/io/file-handlers.jl */
static inline repv
file_handler_ref (repv handler)
{
    repv tem = Fget_structure (Qrep_io_file_handlers);
    if (tem != Qnil)
    {
	tem = F_structure_ref (tem, handler);
	if (!tem || rep_VOIDP (tem))
	    tem = Qnil;
    }
    return tem;
}

repv
rep_signal_file_error(repv cdr)
{
    repv data = Fcons(rep_lookup_errno(), Qnil);
    if(cdr)
    {
	if(rep_CONSP(cdr) || rep_NILP(cdr))
	    rep_CDR(data) = cdr;
	else
	    rep_CDR(data) = Fcons(cdr, Qnil);
    }
    return Fsignal(Qfile_error, data);
}

DEFSTRING(unbound_file, "File is unbound");
repv
rep_unbound_file_error(repv file)
{
    return rep_signal_file_error(rep_list_2(rep_VAL(&unbound_file), file));
}

/* Note that this function never returns rep_NULL. It preserves the
   regexp match data throughout. */
repv
rep_get_file_handler(repv file_name, int op)
{
    repv list = Fsymbol_value(Qfile_handler_alist, Qt);
    struct rep_saved_regexp_data matches;
    if(!list)
	return Qnil;
    rep_DECLARE1(file_name, rep_STRINGP);
    rep_push_regexp_data(&matches);
    while(rep_CONSP(list) && rep_CONSP(rep_CAR(list)))
    {
	repv tem = Fstring_match(rep_CAR(rep_CAR(list)), file_name,
				     Qnil, Qnil);
	if(tem && !rep_NILP(tem))
	{
	    /* Check that this operation isn't already active. */
	    struct blocked_op *ptr = rep_blocked_ops[op];
	    repv handler = rep_CDR(rep_CAR(list));
	    while(ptr != 0 && ptr->handler != handler)
		ptr = ptr->next;
	    if(ptr == 0)
	    {
		rep_pop_regexp_data();
		return handler;
	    }
	}
	list = rep_CDR(list);
	rep_TEST_INT;
	if(rep_INTERRUPTP)
	    break;
    }
    rep_pop_regexp_data();
    return Qnil;
}

/* Call the file handler function HANDLER, for file operation
   OP/SYM. Pass NARGS arguments to it (each a lisp object). Note that
   for the duration of the call, all args and HANDLER will be
   gc-protected, and the the regexp match data is preserved. */
repv
rep_call_file_handler(repv handler, int op, repv sym, int nargs, ...)
{
    struct blocked_op op_data;
    struct rep_saved_regexp_data matches;
    repv arg_list = Qnil;
    repv *ptr = &arg_list;
    repv res;
    int i;
    va_list args;

    va_start(args, nargs);
    for(i = 0; i < nargs; i++)
    {
	*ptr = Fcons((repv)va_arg(args, repv), Qnil);
	ptr = &rep_CDR(*ptr);
    }
    va_end(args);
    arg_list = Fcons(sym, arg_list);

    /* before it gets dereferenced */
    op_data.handler = handler;

    if (rep_SYMBOLP(handler))
    {
	repv fh_env = get_fh_env ();
	if (fh_env == Qt)
	    handler = file_handler_ref (handler);
	else
	{
	    repv tem = Fassq (handler, fh_env);
	    if (tem && rep_CONSP(tem))
	    {
		if (rep_CDR(tem) == Qt)
		    handler = file_handler_ref (handler);
		else if (rep_FUNARGP(rep_CDR(tem)))
		    handler = rep_CDR(tem);
	    }
	}
    }

    if (handler != rep_NULL && !rep_VOIDP (handler))
    {
	rep_push_regexp_data(&matches);
	op_data.next = rep_blocked_ops[op];
	rep_blocked_ops[op] = &op_data;
	/* handler and arg_list are automatically protected by rep_funcall */
	res = rep_funcall(handler, arg_list, rep_FALSE);
	rep_blocked_ops[op] = op_data.next;
	rep_pop_regexp_data();
    }
    else
	res = rep_NULL;

    return res;
}

/* *rep_FILEP may be an opened file, or the name of a file. Returns the handler
   to call, or nil if no handler exists, or rep_NULL if an error occurred.
   Expands *rep_FILEP to its canonical form, leaving this value in *rep_FILEP. */
repv
rep_get_handler_from_file_or_name(repv *filep, int op)
{
    repv file = *filep, handler;
    if(!rep_FILEP(file) && !rep_STRINGP(file))
	return rep_signal_arg_error(file, 1);
    if(rep_FILEP(file))
    {
	if(rep_NILP(rep_FILE(file)->name))
	    return rep_unbound_file_error(file);
	handler = rep_FILE(file)->handler;
	if(handler == Qt)
	    handler = Qnil;
    }
    else
    {
	file = Fexpand_file_name(file, Qnil);
	if(file)
	{
	    *filep = file;
	    handler = rep_get_file_handler(file, op);
	}
	else
	    handler = Qnil;
    }
    return handler;
}

/* Expand *FILE-NAME leaving the result in *FILE-NAME, and find
   its handler for OP. Return the handler or nil. */
repv
rep_expand_and_get_handler(repv *file_namep, int op)
{
    repv file_name = *file_namep, handler;
    rep_DECLARE1(file_name, rep_STRINGP);
    file_name = Fexpand_file_name(file_name, Qnil);
    if(!file_name)
	return rep_NULL;
    handler = rep_get_file_handler(file_name, op);
    *file_namep = file_name;
    return handler;
}

/* Similar to above, but also tries to make file name local to the
   underlying fs if at all possible. */
repv
rep_localise_and_get_handler(repv *file_namep, int op)
{
    repv file_name = *file_namep, handler;
    rep_DECLARE1(file_name, rep_STRINGP);
    file_name = Flocal_file_name(file_name);
    if(!file_name)
	return rep_NULL;
    if(rep_NILP(file_name))
    {
	file_name = Fexpand_file_name(*file_namep, Qnil);
	if(!file_name)
	    return rep_NULL;
    }
    handler = rep_get_file_handler(file_name, op);
    *file_namep = file_name;
    return handler;
}


/* File name handling */

DEFUN("file-name-absolute-p", Ffile_name_absolute_p,
      Sfile_name_absolute_p, (repv file), rep_Subr1) /*
::doc:rep.io.files#file-name-absolute-p::
file-name-absolute-p FILE-NAME

Returns t if FILE-NAME is context-independent, i.e. it does not name a file
relative to the default-directory.
::end:: */
{
    repv handler;
    rep_DECLARE1(file, rep_STRINGP);
    handler = rep_get_file_handler(file, op_file_name_absolute_p);
    if(rep_NILP(handler))
	return rep_file_name_absolute_p(file);
    else
	return rep_call_file_handler(handler, op_file_name_absolute_p,
				     Qfile_name_absolute_p, 1, file);
}

DEFUN("expand-file-name", Fexpand_file_name, Sexpand_file_name,
      (repv file_name, repv dir_name), rep_Subr2) /*
::doc:rep.io.files#expand-file-name::
expand-file-name FILE-NAME [BASE-DIR]

Expands FILE-NAME assuming that it specifies a file relative to BASE-DIR.
If BASE-DIR is undefined it is taken as the current value of the
`default-directory' variable. While expanding the file name, any obvious
simplifications will be performed (e.g. on Unix the removal of "." and
".." where possible).

Note that the returned file name will only be absolute if one of the
following conditions is met:
  1. BASE-DIR (or `default-directory') is absolute
  2. FILE-NAME is already absolute.

Note for file handler implementors: when a handler is called for the
`expand-file-name' operation, it will only ever receive one argument,
the already expanded file name. The only action that may be need to
be taken is to simplify the file name (e.g. removing "." and ".." entries
or whatever).
::end:: */
{
    repv abs, handler;
    rep_GC_root gc_file_name, gc_dir_name;

    rep_DECLARE1(file_name, rep_STRINGP);

    rep_PUSHGC(gc_file_name, file_name);
    rep_PUSHGC(gc_dir_name, dir_name);
    abs = Ffile_name_absolute_p(file_name);
    if(!abs)
    {
	rep_POPGC; rep_POPGC;
	return rep_NULL;
    }
    else if(rep_NILP(abs))
    {
	/* Not absolute, tack on DIR */

	if(!rep_STRINGP(dir_name))
	    dir_name = Fsymbol_value(Qdefault_directory, Qt);
	if(rep_VOIDP(dir_name))
	    dir_name = Qnil;
	dir_name = Ffile_name_as_directory(dir_name);
	if(dir_name && rep_STRINGP(dir_name) && rep_STRING_LEN(dir_name) > 0)
	    file_name = rep_concat2(rep_STR(dir_name), rep_STR(file_name));
    }
    rep_POPGC; rep_POPGC;
    if(!file_name)
	return rep_NULL;

    /* Now simplify FILE-NAME. */

    handler = rep_get_file_handler(file_name, op_expand_file_name);
    if(rep_NILP(handler))
	return rep_expand_file_name(file_name);
    else
	return rep_call_file_handler(handler, op_expand_file_name,
				     Qexpand_file_name, 1, file_name);
}

DEFUN("local-file-name", Flocal_file_name, Slocal_file_name,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#local-file-name::
local-file-name FILE-NAME

When possible, return a string absolutely naming the file in the local
file system that FILE-NAME refers to. If FILE-NAME does not refer to
a file in the local system, return nil.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_local_file_name);
    if(!handler)
	return rep_NULL;
    if(rep_NILP(handler))
	/* Assume that it's already a local file. */
	return file;
    else
	return rep_call_file_handler(handler, op_local_file_name,
				     Qlocal_file_name, 1, file);
}

DEFUN("canonical-file-name", Fcanonical_file_name, Scanonical_file_name,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#canonical-file-name::
canonical-file-name FILE-NAME

Return the canonical name of the file called FILE-NAME. The canonical name
of a file is defined such that two files can be compared simply by comparing
their canonical names; if the names match, they refer to the same file.

(Note that the opposite isn't always true, if two canonical names don't
match the file could still be the same, for example via links. On most
operating systems, symbolic links will be expanded where possible.)
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_canonical_file_name);
    if(!handler)
	return rep_NULL;
    if(rep_NILP(handler))
	return rep_canonical_file_name(file);
    else
	return rep_call_file_handler(handler, op_canonical_file_name,
				     Qcanonical_file_name, 1, file);
}

DEFUN("file-name-nondirectory", Ffile_name_nondirectory,
      Sfile_name_nondirectory, (repv file), rep_Subr1) /*
::doc:rep.io.files#file-name-nondirectory::
file-name-nondirectory FILE-NAME

Return the directory component of FILE-NAME, including the final
directory separator.
::end:: */
{
    repv handler;
    rep_DECLARE1(file, rep_STRINGP);
    handler = rep_get_file_handler(file, op_file_name_nondirectory);
    if(rep_NILP(handler))
	return rep_file_name_nondirectory(file);
    else
	return rep_call_file_handler(handler, op_file_name_nondirectory,
				     Qfile_name_nondirectory, 1, file);
}

DEFUN("file-name-directory", Ffile_name_directory,
      Sfile_name_directory, (repv file), rep_Subr1) /*
::doc:rep.io.files#file-name-directory::
file-name-directory FILE-NAME

Return the file name component of FILE-NAME, i.e. everything following
the final directory separator.
::end:: */
{
    repv handler;
    rep_DECLARE1(file, rep_STRINGP);
    handler = rep_get_file_handler(file, op_file_name_directory);
    if(rep_NILP(handler))
	return rep_file_name_directory(file);
    else
	return rep_call_file_handler(handler, op_file_name_directory,
				     Qfile_name_directory, 1, file);
}

DEFUN("file-name-as-directory", Ffile_name_as_directory,
      Sfile_name_as_directory, (repv file), rep_Subr1) /*
::doc:rep.io.files#file-name-as-directory::
file-name-as-directory FILE-NAME

Return FILE-NAME such that it names a directory (i.e with a terminating
directory separator character.)
::end:: */
{
    repv handler;
    rep_DECLARE1(file, rep_STRINGP);
    handler = rep_get_file_handler(file, op_file_name_as_directory);
    if(rep_NILP(handler))
	return rep_file_name_as_directory(file);
    else
	return rep_call_file_handler(handler, op_file_name_as_directory,
				     Qfile_name_as_directory, 1, file);
}

DEFUN("directory-file-name", Fdirectory_file_name,
      Sdirectory_file_name, (repv file), rep_Subr1) /*
::doc:rep.io.files#directory-file-name::
directory-file-name DIR-NAME

Return the name of the file representing the directory called DIR-NAME.
This is the opposite of file-name-as-directory, since its effect is to
_remove_ any terminating directory separator.
::end:: */
{
    repv handler;
    rep_DECLARE1(file, rep_STRINGP);
    handler = rep_get_file_handler(file, op_directory_file_name);
    if(rep_NILP(handler))
	return rep_directory_file_name(file);
    else
	return rep_call_file_handler(handler, op_directory_file_name,
				     Qdirectory_file_name, 1, file);
}


/* input handlers */

struct input_handler {
    struct input_handler *next;
    int fd;
    repv function;
};

static struct input_handler *input_handlers;

static void
input_handler_callback (int fd)
{
    struct input_handler *x;
    for (x = input_handlers; x != 0; x = x->next)
    {
	if (x->fd == fd)
	{
	    rep_call_lisp0 (x->function);
	    break;
	}
    }
}    

DEFUN("set-input-handler", Fset_input_handler, Sset_input_handler,
      (repv file, repv function), rep_Subr2) /*
::doc:rep.io.files#set-input-handler::
set-input-handler LOCAL-FILE FUNCTION

Arrange for FUNCTION to be called whenever pending input is available
on LOCAL-FILE. Note that this makes LOCAL-FILE do non-blocking input.
::end:: */
{
    int fd;
    rep_DECLARE(1, file, rep_FILEP(file) && rep_LOCAL_FILE_P(file));
    fd = fileno(rep_FILE(file)->file.fh);
    if (function != Qnil)
    {
	struct input_handler *x;
	for (x = input_handlers; x != 0; x = x->next)
	{
	    if (x->fd == fd)
	    {
		x->function = function;
		return function;
	    }
	}
	x = rep_alloc (sizeof (struct input_handler));
	x->next = input_handlers;
	input_handlers = x;
	x->fd = fd;
	x->function = function;
	rep_register_input_fd (fd, input_handler_callback);
	return function;
    }
    else
    {
	struct input_handler **p;
	for (p = &input_handlers; *p != 0; p = &((*p)->next))
	{
	    if ((*p)->fd == fd)
	    {
		struct input_handler *x = *p;
		*p = x->next;
		rep_deregister_input_fd (fd);
		rep_free (x);
	    }
	}
	return Qnil;
    }
}

static void
mark_input_handlers (void)
{
    struct input_handler *x;
    for (x = input_handlers; x != 0; x = x->next)
    {
	rep_MARKVAL(x->function);
    }
}


/* File structures */

static repv
make_file(void)
{
    repv file = rep_VAL(rep_ALLOC_CELL(sizeof(rep_file)));
    if(file == rep_NULL)
	return rep_mem_error();
    rep_data_after_gc += sizeof (rep_file);
    rep_FILE(file)->car = rep_file_type | rep_LFF_BOGUS_LINE_NUMBER;
    rep_FILE(file)->name = Qnil;
    rep_FILE(file)->handler = Qnil;
    rep_FILE(file)->handler_data = Qnil;
    rep_FILE(file)->file.stream = Qnil;
    rep_FILE(file)->next = file_list;
    file_list = rep_FILE(file);
    return file;
}

static void
file_sweep(void)
{
    rep_file *lf = file_list;
    file_list = NULL;
    while(lf)
    {
	rep_file *nxt = lf->next;
	if(!rep_GC_CELL_MARKEDP(rep_VAL(lf)))
	{
	    if(rep_LOCAL_FILE_P(rep_VAL(lf))
	       && !(lf->car & rep_LFF_DONT_CLOSE))
	    {
		fclose(lf->file.fh);
	    }
	    rep_FREE_CELL(lf);
	}
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(lf));
	    lf->next = file_list;
	    file_list = lf;
	}
	lf = nxt;
    }
}

static void
file_prin(repv strm, repv obj)
{
    rep_stream_puts(strm, "#<file ", -1, rep_FALSE);
    if(rep_FILE(obj)->name != Qnil)
    {
	rep_stream_puts(strm, rep_PTR(rep_FILE(obj)->name), -1, rep_TRUE);
	rep_stream_putc(strm, '>');
    }
    else
	rep_stream_puts(strm, "*unbound*>", -1, rep_FALSE);
}

static void
file_mark(repv val)
{
    rep_MARKVAL(rep_FILE(val)->name);
    rep_MARKVAL(rep_FILE(val)->handler);
    rep_MARKVAL(rep_FILE(val)->handler_data);
    if(!rep_LOCAL_FILE_P(val))
	rep_MARKVAL(rep_FILE(val)->file.stream);
}

DEFUN("filep", Ffilep, Sfilep, (repv arg), rep_Subr1) /*
::doc:rep.io.files#filep::
filep ARG

Returns t if ARG is a file object.
::end:: */
{
    return rep_FILEP(arg) ? Qt : Qnil;
}

DEFUN("file-binding", Ffile_binding, Sfile_binding,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#file-binding::
file-binding FILE

Returns the name of the logical file that FILE was opened to access, or nil
if it has been closed, but is still to be garbage collected.
::end:: */
{
    rep_DECLARE1(file, rep_FILEP);
    return rep_FILE(file)->name;
}

DEFUN("file-ttyp", Ffile_ttyp, Sfile_ttyp, (repv file), rep_Subr1) /*
::doc:rep.io.files#file-ttyp::
file-ttyp FILE

Returns true if FILE is linked to a tty.
::end:: */
{
    rep_DECLARE1 (file, rep_FILEP);
    return (rep_LOCAL_FILE_P (file)
	    && isatty (fileno (rep_FILE (file)->file.fh))) ? Qt : Qnil;
}

DEFUN("file-bound-stream", Ffile_bound_stream, Sfile_bound_stream,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#file-bound-stream::
file-bound-stream FILE

If file object FILE doesn't refer to a local file, return the stream
that it's bound to.
::end:: */
{
    rep_DECLARE1(file, rep_FILEP);
    return !rep_LOCAL_FILE_P(file) ? rep_FILE(file)->file.stream : Qnil;
}

DEFUN("file-handler-data", Ffile_handler_data, Sfile_handler_data,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#file-handler-data::
file-handler-data FILE

Return the handler-specific data for FILE.
::end:: */
{
    rep_DECLARE1(file, rep_FILEP);
    return rep_FILE(file)->handler_data;
}

DEFUN("set-file-handler-data", Fset_file_handler_data,
      Sset_file_handler_data, (repv file, repv data), rep_Subr2) /*
::doc:rep.io.files#set-file-handler-data::
set-file-handler-data FILE DATA

Set the handler-specific data of file object FILE to DATA.
::end:: */
{
    rep_DECLARE1(file, rep_FILEP);
    rep_FILE(file)->handler_data = data;
    return data;
}


/* Low level file handling Lisp functions */

DEFUN("open-file", Fopen_file, Sopen_file,
      (repv file_name, repv access_type), rep_Subr2) /*
::doc:rep.io.files#open-file::
open-file FILE-NAME ACCESS-TYPE

Return a new file object referencing the logical file called FILE-NAME,
for ACCESS-TYPE requests. ACCESS-TYPE can be one of the symbols:

	read		For input
	write		Truncate or create the file, and open for output
	append		Open for output at the end of the file.
::end:: */
{
    repv handler, file;
    rep_GC_root gc;

    rep_DECLARE1(file_name, rep_STRINGP);
    rep_DECLARE2(access_type, rep_SYMBOLP);

    rep_PUSHGC(gc, access_type);
    file_name = Fexpand_file_name(file_name, Qnil);
    rep_POPGC;
    if(!file_name)
	return file_name;

    handler = rep_get_file_handler(file_name, op_open_file);
    if(rep_NILP(handler))
    {
	file = make_file();
	if(file != rep_NULL)
	{
	    rep_FILE(file)->file.fh = fopen(rep_STR(file_name),
					    (access_type == Qwrite ? "w"
					     : (access_type == Qappend ? "a"
						: "r")));
	    if(rep_FILE(file)->file.fh == 0)
		return rep_signal_file_error(file_name);
	    rep_FILE(file)->handler = Qt;
	    rep_FILE(file)->handler_data = file_name;
	    if (access_type != Qwrite)
	    {
		rep_FILE (file)->line_number = 1;
		rep_FILE (file)->car &= ~rep_LFF_BOGUS_LINE_NUMBER;
	    }
	}
    }
    else
	file = rep_call_file_handler(handler, op_open_file, Qopen_file,
				     2, file_name, access_type);
    if(file && rep_FILEP(file))
    {
	/* Install the original file name. */
	rep_FILE(file)->name = file_name;
    }
    return file;
}

DEFUN("make-file-from-stream", Fmake_file_from_stream,
      Smake_file_from_stream,
      (repv file_name, repv stream, repv handler),
      rep_Subr3) /*
::doc:rep.io.files#make-file-from-stream::
make-file-from-stream FILE-NAME STREAM HANDLER

Return a new file object that refers to the logical file called FILE-NAME,
that is not in the local filing system. All access to the file object
will be directed through the stream object STREAM, and the file handler
function HANDLER.
::end:: */
{
    repv file;
    rep_DECLARE1(file_name, rep_STRINGP);
    file = make_file();
    if(file != rep_NULL)
    {
	rep_FILE(file)->name = file_name;
	rep_FILE(file)->handler = handler;
	rep_FILE(file)->file.stream = stream;
    }
    return file;
}

DEFUN("close-file", Fclose_file, Sclose_file, (repv file), rep_Subr1) /*
::doc:rep.io.files#close-file::
close-file FILE

Signal that there will be no more I/O through the file object FILE.
::end:: */
{
    rep_DECLARE1(file, rep_FILEP);
    if(rep_NILP(rep_FILE(file)->name))
	return rep_unbound_file_error(file);
    if(rep_LOCAL_FILE_P(file))
    {
	Fset_input_handler (file, Qnil);
	if (!(rep_FILE(file)->car & rep_LFF_DONT_CLOSE))
	    fclose(rep_FILE(file)->file.fh);
	else
	{
	    /* One of stdin, stdout, stderr. freopen onto /dev/null */
	    char *mode;
	    if (rep_FILE(file)->file.fh == stdin)
		mode = "r";
	    else
		mode = "w";
	    freopen (DEV_SLASH_NULL, mode, rep_FILE(file)->file.fh);
	    return Qt;
	}
    }
    else
	rep_call_file_handler(rep_FILE(file)->handler, op_close_file,
			      Qclose_file, 1, file);
    rep_FILE(file)->name = Qnil;
    rep_FILE(file)->handler = Qnil;
    rep_FILE(file)->file.stream = Qnil;
    return Qt;
}

DEFUN("flush-file", Fflush_file, Sflush_file,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#flush-file::
flush-file FILE

Flush any buffered output on FILE. This is usually unnecessary since
all output will be flushed when FILE is eventually closed.
::end:: */
{
    rep_DECLARE1(file, rep_FILEP);
    if(rep_NILP(rep_FILE(file)->name))
	return rep_unbound_file_error(file);
    if(rep_LOCAL_FILE_P(file))
	fflush(rep_FILE(file)->file.fh);
    else
	rep_call_file_handler(rep_FILE(file)->handler, op_flush_file,
			      Qflush_file, 1, file);
    return file;
}

DEFUN("seek-file", Fseek_file, Sseek_file,
      (repv file, repv offset, repv where), rep_Subr3) /*
::doc:rep.io.files#seek-file::
seek-file FILE [OFFSET] [WHERE-FROM]

Called as (seek-file FILE), returns the distance in bytes from the start
of the file that the next character would be read from.

Called as (seek-file FILE OFFSET [WHERE]) alters the position from which the
next byte will be read. WHERE can be one of,

	nil		OFFSET bytes after the current position
	start		OFFSET bytes after the beginning of the file
	end		OFFSET bytes before the end of the file.

Note that not all files may be seekable; if (seek-file FILE) returns
nil (i.e. the current position is unknown) any attempts to set the
current position will also fail.
::end:: */
{
    rep_DECLARE1(file, rep_FILEP);
    rep_DECLARE2_OPT(offset, rep_INTP);
    if(!rep_FILE(file)->name)
	return rep_unbound_file_error(file);
    if(rep_LOCAL_FILE_P(file))
    {
	if(offset == Qnil)
	    return rep_make_long_int (ftell(rep_FILE(file)->file.fh));
	else
	{
	    int whence = SEEK_CUR;
	    if(where == Qstart)
		whence = SEEK_SET;
	    else if(where == Qend)
		whence = SEEK_END;

	    if (whence == SEEK_SET && offset == rep_MAKE_INT (0))
	    {
		rep_FILE (file)->line_number = 1;
		rep_FILE (file)->car &= ~rep_LFF_BOGUS_LINE_NUMBER;
	    }
	    else
		rep_FILE (file)->car |= rep_LFF_BOGUS_LINE_NUMBER;

	    if(fseek(rep_FILE(file)->file.fh,
		     rep_get_long_int(offset), whence) != 0)
	    {
		if (rep_FILE (file)->car & rep_LFF_SILENT_ERRORS)
		    return Qnil;
		else
		    return rep_signal_file_error(rep_LIST_1(file));
	    }
	    else
		return Qt;
	}
    }
    else
	return rep_call_file_handler(rep_FILE(file)->handler, op_seek_file,
				     Qseek_file, 3, file, offset, where);
}

DEFUN("set-file-ignore-errors", Fset_file_ignore_errors,
      Sset_file_ignore_errors, (repv file, repv status), rep_Subr2)
{
    rep_DECLARE1 (file, rep_FILEP);
    rep_FILE (file)->car &= ~rep_LFF_SILENT_ERRORS;
    rep_FILE (file)->car |= (status == Qnil) ? 0 : rep_LFF_SILENT_ERRORS;
    return rep_undefined_value;
}


/* General file operations */

DEFUN_INT("delete-file", Fdelete_file, Sdelete_file, (repv file_name),
	  rep_Subr1, "fFile to delete:") /*
::doc:rep.io.files#delete-file::
delete-file FILE-NAME

Delete the file called FILE-NAME.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file_name, op_delete_file);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_delete_file(file_name);
    else
	return rep_call_file_handler(handler, op_delete_file,
				     Qdelete_file, 1, file_name);
}

DEFSTRING(cant_rename, "Can't rename files across handlers");
DEFUN_INT("rename-file", Frename_file, Srename_file,
	  (repv old, repv new), rep_Subr2,
	  "fOld name of file:" rep_DS_NL "FNew name of file:") /*
::doc:rep.io.files#rename-file::
rename-file OLD-NAME NEW-NAME

Rename the file called OLD-NAME so that it is called NEW-NAME. Note that
this almost certainly won't work across filing systems.
::end:: */
{
    repv old_handler, new_handler;
    rep_GC_root gc_old, gc_new;

    rep_PUSHGC(gc_old, old);
    rep_PUSHGC(gc_new, new);
    old_handler = rep_localise_and_get_handler(&old, op_rename_file);
    new_handler = rep_localise_and_get_handler(&new, op_rename_file);
    rep_POPGC; rep_POPGC;
    if(!old_handler || !new_handler)
	return rep_NULL;

    if(old_handler == new_handler)
    {
	if(rep_NILP(old_handler))
	    /* Both names on local fs. */
	    return rep_rename_file(old, new);
	else
	    return rep_call_file_handler(old_handler, op_rename_file,
					 Qrename_file, 2, old, new);
    }
    else
	/* TODO: use copy ops to make this work. */
	return Fsignal(Qfile_error, rep_LIST_1(rep_VAL(&cant_rename)));
}

DEFUN_INT("make-directory", Fmake_directory, Smake_directory,
	  (repv dir_name), rep_Subr1,
	  "DDirectory to create:") /*
::doc:rep.io.files#make-directory::
make-directory DIRECTORY-NAME

Create a directory called DIRECTORY-NAME.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&dir_name, op_make_directory);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_make_directory(dir_name);
    else
	return rep_call_file_handler(handler, op_make_directory,
				     Qmake_directory, 1, dir_name);
}

DEFUN_INT("delete-directory", Fdelete_directory, Sdelete_directory,
	  (repv dir_name), rep_Subr1,
	  "DDirectory to delete:") /*
::doc:rep.io.files#delete-directory::
delete-directory DIRECTORY-NAME

Delete the directory called DIRECTORY-NAME. Note that the directory in
question should be empty.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&dir_name, op_delete_directory);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_delete_directory(dir_name);
    else
	return rep_call_file_handler(handler, op_delete_directory,
				     Qdelete_directory, 1, dir_name);
}

DEFUN_INT("copy-file", Fcopy_file, Scopy_file, (repv src, repv dst),
	  rep_Subr2, "fSource file:" rep_DS_NL "FDestination file:") /*
::doc:rep.io.files#copy-file::
copy-file SOURCE DESTINATION

Create a new copy of the file called SOURCE, as the file called DESTINATION.
::end:: */
{
    repv src_handler, dst_handler, res;
    rep_GC_root gc_src, gc_dst;

    rep_PUSHGC(gc_src, src);
    rep_PUSHGC(gc_dst, dst);
    src_handler = rep_localise_and_get_handler(&src, op_copy_file);
    dst_handler = rep_localise_and_get_handler(&dst, op_copy_file);
    rep_POPGC; rep_POPGC;
    if(!src_handler || !dst_handler)
	return rep_NULL;

    if(src_handler == dst_handler)
    {
	if(rep_NILP(src_handler))
	    /* Both names on local fs. */
	    res = rep_copy_file(src, dst);
	else
	    res = rep_call_file_handler(src_handler, op_copy_file,
					Qcopy_file, 2, src, dst);
    }
    else if(rep_NILP(src_handler))
    {
	/* Copying from local to remote */
	res = rep_call_file_handler(dst_handler, op_copy_file_from_local_fs,
				    Qcopy_file_from_local_fs, 2, src, dst);
    }
    else if(rep_NILP(dst_handler))
    {
	/* Copying from remote to local */
	res = rep_call_file_handler(src_handler, op_copy_file_to_local_fs,
				    Qcopy_file_to_local_fs, 2, src, dst);
    }
    else
    {
	/* Copy from remote-1 to remote-2 via local fs. */
	repv temp = Fmake_temp_name();
	if(temp)
	{
	    res = rep_call_file_handler(src_handler, op_copy_file_to_local_fs,
					Qcopy_file_to_local_fs, 2, src, temp);
	    if(res)
	    {
		res = rep_call_file_handler(dst_handler,
					    op_copy_file_from_local_fs,
					    Qcopy_file_from_local_fs,
					    2, temp, dst);
	    }
	    remove(rep_STR(temp));
	}
	else
	    res = rep_NULL;
    }
    return res;
}
    

/* File attribute operations */

DEFUN("file-readable-p", Ffile_readable_p, Sfile_readable_p,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#file-readable-p::
file-readable-p FILE-NAME

Returns t if the file called FILE-NAME is available for reading from.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_file_readable_p);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_file_readable_p(file);
    else
	return rep_call_file_handler(handler, op_file_readable_p,
				     Qfile_readable_p, 1, file);
}

DEFUN("file-writable-p", Ffile_writable_p, Sfile_writable_p,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#file-writeable-p::
file-writable-p FILE-NAME

Returns t if the file called FILE-NAME is available for writing to.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_file_writable_p);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_file_writable_p(file);
    else
	return rep_call_file_handler(handler, op_file_writable_p,
				     Qfile_writable_p, 1, file);
}

DEFUN("file-executable-p", Ffile_executable_p, Sfile_executable_p,
		(repv file), rep_Subr1) /*
::doc:rep.io.files#file-executable-p::
file-executable-p FILE-NAME

Returns t if the file called FILE-NAME is executable.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_file_executable_p);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_file_executable_p(file);
    else
	return rep_call_file_handler(handler, op_file_executable_p,
			             Qfile_executable_p, 1, file);
}

DEFUN("file-exists-p", Ffile_exists_p, Sfile_exists_p,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#file-exists-p::
file-exists-p FILE-NAME

Returns t if the file called FILE-NAME exists.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_file_exists_p);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_file_exists_p(file);
    else
	return rep_call_file_handler(handler, op_file_exists_p,
				     Qfile_exists_p, 1, file);
}

DEFUN("file-regular-p", Ffile_regular_p, Sfile_regular_p,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#file-regular-p::
file-regular-p FILE-NAME

Returns t if the file called FILE-NAME is a normal file, ie, not a
directory, device, symbolic link, etc...
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_file_regular_p);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_file_regular_p(file);
    else
	return rep_call_file_handler(handler, op_file_regular_p,
				     Qfile_regular_p, 1, file);
}

DEFUN("file-directory-p", Ffile_directory_p, Sfile_directory_p,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#file-directory-p::
file-directory-p FILE-NAME

Returns t if the file called FILE-NAME is a directory.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_file_directory_p);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_file_directory_p(file);
    else
	return rep_call_file_handler(handler, op_file_directory_p,
				     Qfile_directory_p, 1, file);
}

DEFUN("file-symlink-p", Ffile_symlink_p, Sfile_symlink_p,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#file-symlink-p::
file-symlink-p FILE-NAME

Returns t if the file called FILE-NAME is a symbolic link to another file.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_file_symlink_p);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_file_symlink_p(file);
    else
	return rep_call_file_handler(handler, op_file_symlink_p,
				     Qfile_symlink_p, 1, file);
}

DEFUN("file-owner-p", Ffile_owner_p, Sfile_owner_p,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#file-owner-p::
file-owner-p FILE-NAME

Returns t if the ownership (uid & gid) of the file called FILE-NAME is the
same as that of any files written by the editor.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_file_owner_p);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_file_owner_p(file);
    else
	return rep_call_file_handler(handler, op_file_owner_p,
				     Qfile_owner_p, 1, file);
}

DEFUN("file-gid-p", Ffile_gid_p, Sfile_gid_p,
	(repv file), rep_Subr1) /*
::doc::rep.io.files#file-gid-p::
file-gid-p FILE-NAME

Returns the gid of the file called FILE-NAME
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_file_gid_p);
    if (!handler)
	 return handler;
    if(rep_NILP(handler))
	 return rep_file_gid_p(file);
    else
	 return rep_call_file_handler(handler, op_file_gid_p,
			              Qfile_gid_p, 1, file);
}

DEFUN("file-uid-p", Ffile_uid_p, Sfile_uid_p,
	(repv file), rep_Subr1) /*
::doc::rep.io.files#file-uid-p::
file-uid-p FILE-NAME

Returns the uid of the file called FILE-NAME
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_file_uid_p);
    if (!handler)
	 return handler;
    if (rep_NILP(handler))
	 return rep_file_uid_p(file);
    else
	 return rep_call_file_handler(handler, op_file_uid_p,
			 		Qfile_uid_p, 1, file);
}

DEFUN("file-nlinks", Ffile_nlinks, Sfile_nlinks,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#file-nlinks::
file-nlinks FILE-NAME

Returns the number of links pointing to the file called FILE-NAME. This will
be one if FILE-NAME has only one name. Doesn't count symbolic links.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_file_nlinks);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_file_nlinks(file);
    else
	return rep_call_file_handler(handler, op_file_nlinks,
				     Qfile_nlinks, 1, file);
}

DEFUN("file-size", Ffile_size, Sfile_size,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#file-size::
file-size FILE-NAME

Returns the size of the file called FILE-NAME in bytes.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_file_size);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_file_size(file);
    else
	return rep_call_file_handler(handler, op_file_size,
				     Qfile_size, 1, file);
}

DEFUN("file-modes", Ffile_modes, Sfile_modes,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#file-modes::
file-modes FILE-NAME

Return the access permissions of the file called FILE-NAME. Note that the
format of this object is filing system dependent. It's only portable use
is as an argument to set-file-modes.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_file_modes);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_file_modes(file);
    else
	return rep_call_file_handler(handler, op_file_modes,
				     Qfile_modes, 1, file);
}

DEFUN("set-file-modes", Fset_file_modes, Sset_file_modes,
      (repv file, repv modes), rep_Subr2) /*
::doc:rep.io.files#set-file-modes::
set-file-modes FILE-NAME MODES

Sets the access permissions of the file called FILE-NAME to MODES. The only
portable way of getting MODES is from the `file-modes' function since it
may change across filing systems.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_set_file_modes);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_set_file_modes(file, modes);
    else
	return rep_call_file_handler(handler, op_set_file_modes,
				     Qset_file_modes, 2, file, modes);
}

DEFUN("file-modes-as-string", Ffile_modes_as_string,
      Sfile_modes_as_string, (repv file), rep_Subr1) /*
::doc:rep.io.files#file-modes-as-string::
file-modes-as-string FILE-NAME

Returns a ten character string describing the attributes of the file
called FILE-NAME.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_file_modes_as_string);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_file_modes_as_string(file);
    else
	return rep_call_file_handler(handler, op_file_modes_as_string,
				     Qfile_modes_as_string, 1, file);
}

DEFUN("file-modtime", Ffile_modtime, Sfile_modtime,
      (repv file), rep_Subr1) /*
::doc:rep.io.files#file-modtime::
file-modtime FILE-NAME

Return the time that the file called FILE-NAME was last modified, as a cons
cell storing two integers, the low 24 bits, and the high bits.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_file_modtime);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_file_modtime(file);
    else
	return rep_call_file_handler(handler, op_file_modtime,
				     Qfile_modtime, 1, file);
}

rep_bool
rep_file_newer_than(repv name1, repv name2)
{
    rep_bool res = rep_FALSE;
    repv time1;
    rep_GC_root gc_name1, gc_name2;

    rep_PUSHGC(gc_name1, name1);
    rep_PUSHGC(gc_name2, name2);
    time1 = Ffile_modtime(name1);
    if(time1 && !rep_NILP(time1))
    {
	repv time2;
	rep_GC_root gc_time1;

	rep_PUSHGC(gc_time1, time1);
	time2 = Ffile_modtime(name2);
	rep_POPGC;

	if(time2 && !rep_NILP(time2))
	{
	    repv foo = Ftime_later_p(time1, time2);
	    if(foo && !rep_NILP(foo))
		res = rep_TRUE;
	}
    }
    rep_POPGC; rep_POPGC;

    return res;
}

DEFUN("directory-files", Fdirectory_files, Sdirectory_files,
      (repv dir), rep_Subr1) /*
::doc:rep.io.files#directory-files::
directory-files DIRECTORY

Returns a list of the names of all files in the directory called DIRECTORY.
The list is unsorted.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&dir, op_directory_files);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_directory_files(dir);
    else
	return rep_call_file_handler(handler, op_directory_files,
				     Qdirectory_files, 1, dir);
}

DEFUN("read-symlink", Fread_symlink, Sread_symlink, (repv file), rep_Subr1) /*
::doc:rep.io.files#read-symlink::
read-symlink FILENAME

Return the string that is the contents of the symbolic link FILENAME. This
string may be relative to the directory containing FILENAME.

Signals an error if FILENAME isn't a symbolic link.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_read_symlink);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_read_symlink(file);
    else
	return rep_call_file_handler(handler, op_read_symlink,
				     Qread_symlink, 1, file);
}

DEFUN("make-symlink", Fmake_symlink, Smake_symlink,
      (repv file, repv contents), rep_Subr2) /*
::doc:rep.io.files#make-symlink::
make-symlink FILENAME CONTENTS

Create a symbolic link FILENAME pointing to the file called CONTENTS.
CONTENTS may be relative to the directory containing FILENAME.
::end:: */
{
    repv handler = rep_expand_and_get_handler(&file, op_make_symlink);
    rep_DECLARE2(contents, rep_STRINGP);
    if(!handler)
	return handler;
    if(rep_NILP(handler))
	return rep_make_symlink(file, contents);
    else
	return rep_call_file_handler(handler, op_make_symlink,
				     Qmake_symlink, 2, file, contents);
}


/* Utility functions */

repv
rep_file_fdopen (int fd, char *mode)
{
    rep_file *ptr;
    for (ptr = file_list; ptr != 0; ptr = ptr->next)
    {
	if (rep_LOCAL_FILE_P (rep_VAL (ptr)) && fileno (ptr->file.fh) == fd)
	    return rep_VAL (ptr);
    }
    ptr = rep_FILE (make_file ());
    ptr->handler = Qt;
    ptr->file.fh = fdopen (fd, mode);
    if (ptr->file.fh == 0)
	return rep_NULL;
    else
	return rep_VAL (ptr);
}

DEFSTRING(stdin_name, "<stdin>");
DEFUN("stdin-file", Fstdin_file, Sstdin_file, (void), rep_Subr0) /*
::doc:rep.io.files#stdin-file::
stdin-file

Returns the file object representing the editor's standard input.
::end:: */
{
    static repv stdin_file;
    if(stdin_file)
	return stdin_file;
    stdin_file = make_file();
    rep_FILE(stdin_file)->name = rep_VAL(&stdin_name);
    rep_FILE(stdin_file)->handler = Qt;
    rep_FILE(stdin_file)->file.fh = stdin;
    rep_FILE(stdin_file)->car |= rep_LFF_DONT_CLOSE;
    rep_mark_static(&stdin_file);
    return stdin_file;
}

DEFSTRING(stdout_name, "<stdout>");
DEFUN("stdout-file", Fstdout_file, Sstdout_file, (void), rep_Subr0) /*
::doc:rep.io.files#stdout-file::
stdout-file

Returns the file object representing the editor's standard output.
::end:: */
{
    static repv stdout_file;
    if(stdout_file)
	return stdout_file;
    stdout_file = make_file();
    rep_FILE(stdout_file)->name = rep_VAL(&stdout_name);
    rep_FILE(stdout_file)->handler = Qt;
    rep_FILE(stdout_file)->file.fh = stdout;
    rep_FILE(stdout_file)->car |= rep_LFF_DONT_CLOSE;
    rep_mark_static(&stdout_file);
    return stdout_file;
}

DEFSTRING(stderr_name, "<stderr>");
DEFUN("stderr-file", Fstderr_file, Sstderr_file, (void), rep_Subr0) /*
::doc:rep.io.files#stderr-file::
stderr-file

Returns the file object representing the editor's standard output.
::end:: */
{
    static repv stderr_file;
    if(stderr_file)
	return stderr_file;
    stderr_file = make_file();
    rep_FILE(stderr_file)->name = rep_VAL(&stderr_name);
    rep_FILE(stderr_file)->handler = Qt;
    rep_FILE(stderr_file)->file.fh = stderr;
    rep_FILE(stderr_file)->car |= rep_LFF_DONT_CLOSE;
    rep_mark_static(&stderr_file);
    return stderr_file;
}

DEFSTRING(no_temp, "Can't create temporary file name");
DEFUN("make-temp-name", Fmake_temp_name, Smake_temp_name, (void), rep_Subr0) /*
::doc:rep.io.files#make-temp-name::
make-temp-name

Returns the name of a unique file in the local filing system.
::end:: */
{
    char buf[L_tmpnam];
    if(tmpnam(buf))
	return rep_string_dup(buf);
    else
	return rep_signal_file_error(rep_VAL(&no_temp));
}

DEFUN("set-file-handler-environment", Fset_file_handler_environment,
      Sset_file_handler_environment, (repv env, repv structure), rep_Subr2) /*
::doc:rep.io.files#set-file-handler-environment::
set-file-handler-environment ENV
::end:: */
{
    return Fstructure_define (structure, Qfh_env_key, env);
}


/* init */

void
rep_files_init(void)
{
    repv tem;

    Qfh_env_key = Fmake_symbol (rep_VAL (&str_fh_env_key));
    rep_mark_static (&Qfh_env_key);

    rep_INTERN_SPECIAL(file_handler_alist);
    Fset (Qfile_handler_alist, Qnil);

    rep_INTERN_SPECIAL(default_directory);
    tem = rep_getpwd();
    if (tem == rep_NULL)
	tem = rep_null_string ();
    Fset (Qdefault_directory, tem);

    rep_INTERN(file_name_absolute_p);
    rep_INTERN(expand_file_name);
    rep_INTERN(local_file_name);
    rep_INTERN(canonical_file_name);
    rep_INTERN(file_name_nondirectory);
    rep_INTERN(file_name_directory);
    rep_INTERN(file_name_as_directory);
    rep_INTERN(directory_file_name);
    rep_INTERN(open_file);
    rep_INTERN(close_file);
    rep_INTERN(flush_file);
    rep_INTERN(seek_file);
    rep_INTERN(delete_file);
    rep_INTERN(rename_file);
    rep_INTERN(make_directory);
    rep_INTERN(delete_directory);
    rep_INTERN(copy_file);
    rep_INTERN(copy_file_to_local_fs);
    rep_INTERN(copy_file_from_local_fs);
    rep_INTERN(file_readable_p);
    rep_INTERN(file_writable_p);
    rep_INTERN(file_executable_p);
    rep_INTERN(file_exists_p);
    rep_INTERN(file_regular_p);
    rep_INTERN(file_directory_p);
    rep_INTERN(file_symlink_p);
    rep_INTERN(file_owner_p);
    rep_INTERN(file_gid_p);
    rep_INTERN(file_uid_p);
    rep_INTERN(file_nlinks);
    rep_INTERN(file_size);
    rep_INTERN(file_modes);
    rep_INTERN(set_file_modes);
    rep_INTERN(file_modes_as_string);
    rep_INTERN(file_modtime);
    rep_INTERN(directory_files);
    rep_INTERN(read_symlink);
    rep_INTERN(make_symlink);

    rep_INTERN(start); rep_INTERN(end);
    rep_INTERN(read); rep_INTERN(write); rep_INTERN(append);

    rep_INTERN(rep_io_file_handlers);

    tem = rep_push_structure ("rep.io.files");

    rep_ADD_SUBR(Sfilep);
    rep_ADD_SUBR(Sfile_binding);
    rep_ADD_SUBR(Sfile_ttyp);
    rep_ADD_SUBR(Sfile_bound_stream);
    rep_ADD_SUBR(Sfile_handler_data);
    rep_ADD_SUBR(Sset_file_handler_data);

    rep_ADD_SUBR(Sfile_name_absolute_p);
    rep_ADD_SUBR(Sexpand_file_name);
    rep_ADD_SUBR(Slocal_file_name);
    rep_ADD_SUBR(Scanonical_file_name);
    rep_ADD_SUBR(Sfile_name_nondirectory);
    rep_ADD_SUBR(Sfile_name_directory);
    rep_ADD_SUBR(Sfile_name_as_directory);
    rep_ADD_SUBR(Sdirectory_file_name);

    rep_ADD_SUBR(Sset_input_handler);

    rep_ADD_SUBR(Sopen_file);
    rep_ADD_SUBR(Smake_file_from_stream);
    rep_ADD_SUBR(Sclose_file);
    rep_ADD_SUBR(Sflush_file);
    rep_ADD_SUBR(Sseek_file);
    rep_ADD_SUBR(Sset_file_ignore_errors);

    rep_ADD_SUBR_INT(Sdelete_file);
    rep_ADD_SUBR_INT(Srename_file);
    rep_ADD_SUBR_INT(Scopy_file);
    rep_ADD_SUBR_INT(Smake_directory);
    rep_ADD_SUBR_INT(Sdelete_directory);

    rep_ADD_SUBR(Sfile_readable_p);
    rep_ADD_SUBR(Sfile_writable_p);
    rep_ADD_SUBR(Sfile_executable_p);
    rep_ADD_SUBR(Sfile_exists_p);
    rep_ADD_SUBR(Sfile_regular_p);
    rep_ADD_SUBR(Sfile_directory_p);
    rep_ADD_SUBR(Sfile_symlink_p);
    rep_ADD_SUBR(Sfile_owner_p);
    rep_ADD_SUBR(Sfile_gid_p);
    rep_ADD_SUBR(Sfile_uid_p);
    rep_ADD_SUBR(Sfile_nlinks);
    rep_ADD_SUBR(Sfile_size);
    rep_ADD_SUBR(Sfile_modes);
    rep_ADD_SUBR(Sset_file_modes);
    rep_ADD_SUBR(Sfile_modes_as_string);
    rep_ADD_SUBR(Sfile_modtime);
    rep_ADD_SUBR(Sdirectory_files);
    rep_ADD_SUBR(Sread_symlink);
    rep_ADD_SUBR(Smake_symlink);

    rep_ADD_SUBR(Sstdin_file);
    rep_ADD_SUBR(Sstdout_file);
    rep_ADD_SUBR(Sstderr_file);
    rep_ADD_SUBR(Smake_temp_name);
    rep_ADD_SUBR(Sset_file_handler_environment);

    rep_pop_structure (tem);

    /* Initialise the type information. */
    rep_file_type = rep_register_new_type("file", rep_ptr_cmp,
					  file_prin, file_prin, file_sweep,
					  file_mark, mark_input_handlers,
					  0, 0, 0, 0, 0, 0);
}

void
rep_files_kill(void)
{
    rep_file *lf = file_list;
    while(lf)
    {
	rep_file *nxt = lf->next;
	if(rep_LOCAL_FILE_P(rep_VAL(lf))
	   && !(lf->car & rep_LFF_DONT_CLOSE))
	{
	    fclose(lf->file.fh);
	}
	rep_FREE_CELL(lf);
	lf = nxt;
    }
    file_list = NULL;
}
