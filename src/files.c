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

#include "jade.h"
#include "jade_protos.h"

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#ifdef NEED_MEMORY_H
# include <memory.h>
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

   (write-buffer-contents FILE-OR-NAME START END)
   (read-file-contents FILE-OR-NAME)
   (insert-file-contents FILE-OR-NAME)

   (delete-file NAME)
   (rename-file OLD-NAME NEW-NAME)
   (copy-file SOURCE DEST)
   (copy-file-to-local-fs SOURCE LOCAL-DEST)
   (copy-file-from-local-fs LOCAL-SOURCE DEST)

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

   ACCESS-TYPE is one of `read', `write' or `append'.
   WHENCE is one off `nil', `start', `end'. */

DEFSYM(file_handler_alist, "file-handler-alist"); /*
::doc:file_handler_alist::
a list of `(REGEXP . HANDLER)'. If REGEXP matches the name of a file
being manipulated the function HANDLER is called as (HANDLER OPERATION
ARGS...) where ARGS matches how the original function is called.
::end:: */

_PR VALUE sym_default_directory;
DEFSYM(default_directory, "default-directory"); /*
::doc:default_directory::
Buffer-local variable absolutely defining the directory to which all files
accessed in the buffer are resolved from (unless they're absolute.) 
::end:: */

/* List of all allocated file objects */
static Lisp_File *file_list;

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
DEFSYM(write_buffer_contents, "write-buffer-contents");
DEFSYM(read_file_contents, "read-file-contents");
DEFSYM(insert_file_contents, "insert-file-contents");
DEFSYM(delete_file, "delete-file");
DEFSYM(rename_file, "rename-file");
DEFSYM(copy_file, "copy-file");
DEFSYM(copy_file_to_local_fs, "copy-file-to-local-fs");
DEFSYM(copy_file_from_local_fs, "copy-file-from-local-fs");
DEFSYM(file_readable_p, "file-readable-p");
DEFSYM(file_writable_p, "file-writable-p");
DEFSYM(file_exists_p, "file-exists-p");
DEFSYM(file_regular_p, "file-regular-p");
DEFSYM(file_directory_p, "file-directory-p");
DEFSYM(file_symlink_p, "file-symlink-p");
DEFSYM(file_owner_p, "file-owner-p");
DEFSYM(file_nlinks, "file-nlinks");
DEFSYM(file_size, "file-size");
DEFSYM(file_modes, "file-modes");
DEFSYM(set_file_modes, "set-file-modes");
DEFSYM(file_modes_as_string, "file-modes-as-string");
DEFSYM(file_modtime, "file-modtime");
DEFSYM(directory_files, "directory-files");

_PR VALUE sym_start, sym_end;
DEFSYM(start, "start");
DEFSYM(end, "end");

_PR VALUE sym_read, sym_write, sym_append;
DEFSYM(read, "read");
DEFSYM(write, "write");
DEFSYM(append, "append");

static bool read_file_into_tx(TX *tx, FILE *fh, long file_length);

_PR VALUE signal_file_error(VALUE cdr);
_PR VALUE unbound_file_error(VALUE file);
_PR void file_sweep(void);
_PR void file_prin(VALUE strm, VALUE obj);
_PR bool file_newer_than(VALUE name1, VALUE name2);
_PR void files_init(void);
_PR void files_kill(void);

enum file_ops {
    op_file_name_absolute_p = 0,
    op_expand_file_name,
    op_local_file_name,
    op_canonical_file_name,
    op_file_name_nondirectory,
    op_file_name_directory,
    op_file_name_as_directory,
    op_directory_file_name,
    op_open_file,
    op_close_file,
    op_flush_file,
    op_seek_file,
    op_write_buffer_contents,
    op_read_file_contents,
    op_insert_file_contents,
    op_delete_file,
    op_rename_file,
    op_copy_file,
    op_copy_file_to_local_fs,
    op_copy_file_from_local_fs,
    op_file_readable_p,
    op_file_writable_p,
    op_file_exists_p,
    op_file_regular_p,
    op_file_directory_p,
    op_file_symlink_p,
    op_file_owner_p,
    op_file_nlinks,
    op_file_size,
    op_file_modes,
    op_set_file_modes,
    op_file_modes_as_string,
    op_file_modtime,
    op_directory_files,

    op_MAX
};

struct blocked_op {
    struct blocked_op *next;
    VALUE handler;
};

/* Vector of blocked operations */
static struct blocked_op *blocked_ops[op_MAX];



VALUE
signal_file_error(VALUE cdr)
{
    VALUE data = cmd_cons(lookup_errno(), sym_nil);
    if(cdr)
    {
	if(CONSP(cdr) || NILP(cdr))
	    VCDR(data) = cdr;
	else
	    VCDR(data) = cmd_cons(cdr, sym_nil);
    }
    return cmd_signal(sym_file_error, data);
}

DEFSTRING(unbound_file, "File is unbound");
VALUE
unbound_file_error(VALUE file)
{
    return signal_file_error(list_2(VAL(&unbound_file), file));
}

/* Note that this function never returns LISP_NULL. It preserves the
   regexp match data throughout. */
static VALUE
get_file_handler(VALUE file_name, int op)
{
    VALUE list = cmd_symbol_value(sym_file_handler_alist, sym_t);
    struct saved_regexp_data matches;
    if(!list)
	return sym_nil;
    DECLARE1(file_name, STRINGP);
    push_regexp_data(&matches);
    while(CONSP(list) && CONSP(VCAR(list)))
    {
	VALUE tem = cmd_string_match(VCAR(VCAR(list)), file_name,
				     sym_nil, sym_nil);
	if(tem && !NILP(tem))
	{
	    /* Check that this operation isn't already active. */
	    struct blocked_op *ptr = blocked_ops[op];
	    VALUE handler = VCDR(VCAR(list));
	    while(ptr != 0 && ptr->handler != handler)
		ptr = ptr->next;
	    if(ptr == 0)
	    {
		pop_regexp_data();
		return handler;
	    }
	}
	list = VCDR(list);
	TEST_INT;
	if(INT_P)
	    break;
    }
    pop_regexp_data();
    return sym_nil;
}

/* Call the file handler function HANDLER, for file operation
   OP/SYM. Pass NARGS arguments to it (each a lisp object). Note that
   for the duration of the call, all args and HANDLER will be
   gc-protected, and the the regexp match data is preserved. */
static VALUE
call_handler(VALUE handler, int op, VALUE sym, int nargs, ...)
{
    struct blocked_op op_data;
    struct saved_regexp_data matches;
    VALUE arg_list = sym_nil;
    VALUE *ptr = &arg_list;
    VALUE res;
    int i;
    va_list args;

    va_start(args, nargs);
    for(i = 0; i < nargs; i++)
    {
	*ptr = cmd_cons(va_arg(args, VALUE), sym_nil);
	ptr = &VCDR(*ptr);
    }
    va_end(args);
    arg_list = cmd_cons(sym, arg_list);

    push_regexp_data(&matches);
    op_data.next = blocked_ops[op];
    blocked_ops[op] = &op_data;
    op_data.handler = handler;
    /* handler and arg_list are automatically gc-protected by funcall */
    res = funcall(handler, arg_list, FALSE);
    blocked_ops[op] = op_data.next;
    pop_regexp_data();

    return res;
}

/* *FILEP may be an opened file, or the name of a file. Returns the handler
   to call, or nil if no handler exists, or LISP_NULL if an error occurred.
   Expands *FILEP to its canonical form, leaving this value in *FILEP. */
static VALUE
get_handler_from_file_or_name(VALUE *filep, int op)
{
    VALUE file = *filep, handler;
    if(!FILEP(file) && !STRINGP(file))
	return signal_arg_error(file, 1);
    if(FILEP(file))
    {
	if(NILP(VFILE(file)->name))
	    return unbound_file_error(file);
	handler = VFILE(file)->handler;
	if(handler == sym_t)
	    handler = sym_nil;
    }
    else
    {
	file = cmd_expand_file_name(file, sym_nil);
	if(file)
	{
	    *filep = file;
	    handler = get_file_handler(file, op);
	}
	else
	    handler = sym_nil;
    }
    return handler;
}

/* Expand *FILE-NAME leaving the result in *FILE-NAME, and find
   its handler for OP. Return the handler or nil. */
static VALUE
expand_and_get_handler(VALUE *file_namep, int op)
{
    VALUE file_name = *file_namep, handler;
    DECLARE1(file_name, STRINGP);
    file_name = cmd_expand_file_name(file_name, sym_nil);
    if(!file_name)
	return LISP_NULL;
    handler = get_file_handler(file_name, op);
    *file_namep = file_name;
    return handler;
}

/* Similar to above, but also tries to make file name local to the
   underlying fs if at all possible. */
static VALUE
localise_and_get_handler(VALUE *file_namep, int op)
{
    VALUE file_name = *file_namep, handler;
    DECLARE1(file_name, STRINGP);
    file_name = cmd_local_file_name(file_name);
    if(!file_name)
	return LISP_NULL;
    if(NILP(file_name))
    {
	file_name = cmd_expand_file_name(*file_namep, sym_nil);
	if(!file_name)
	    return LISP_NULL;
    }
    handler = get_file_handler(file_name, op);
    *file_namep = file_name;
    return handler;
}


/* File name handling */

_PR VALUE cmd_file_name_absolute_p(VALUE file);
DEFUN("file-name-absolute-p", cmd_file_name_absolute_p,
      subr_file_name_absolute_p, (VALUE file), V_Subr1,
      DOC_file_name_absolute_p) /*
::doc:file_name_absolute_p::
file-name-absolute-p FILE-NAME

Returns t if FILE-NAME is context-independent, i.e. it does not name a file
relative to the default-directory.
::end:: */
{
    VALUE handler;
    DECLARE1(file, STRINGP);
    handler = get_file_handler(file, op_file_name_absolute_p);
    if(NILP(handler))
	return sys_file_name_absolute_p(file);
    else
	return call_handler(handler, op_file_name_absolute_p,
			    sym_file_name_absolute_p, 1, file);
}

_PR VALUE cmd_expand_file_name(VALUE, VALUE);
DEFUN("expand-file-name", cmd_expand_file_name, subr_expand_file_name,
      (VALUE file_name, VALUE dir_name), V_Subr2, DOC_expand_file_name) /*
::doc:expand_file_name::
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
    VALUE abs, handler;
    GC_root gc_file_name, gc_dir_name;

    DECLARE1(file_name, STRINGP);

    PUSHGC(gc_file_name, file_name);
    PUSHGC(gc_dir_name, dir_name);
    abs = cmd_file_name_absolute_p(file_name);
    if(!abs)
    {
	POPGC; POPGC;
	return LISP_NULL;
    }
    else if(NILP(abs))
    {
	/* Not absolute, tack on DIR */

	if(!STRINGP(dir_name))
	    dir_name = cmd_symbol_value(sym_default_directory, sym_t);
	if(VOIDP(dir_name))
	    dir_name = sym_nil;
	dir_name = cmd_file_name_as_directory(dir_name);
	if(dir_name && STRINGP(dir_name) && STRING_LEN(dir_name) > 0)
	    file_name = concat2(VSTR(dir_name), VSTR(file_name));
    }
    POPGC; POPGC;
    if(!file_name)
	return LISP_NULL;

    /* Now simplify FILE-NAME. */

    handler = get_file_handler(file_name, op_expand_file_name);
    if(NILP(handler))
	return sys_expand_file_name(file_name);
    else
	return call_handler(handler, op_expand_file_name,
			    sym_expand_file_name, 1, file_name);
}

_PR VALUE cmd_local_file_name(VALUE);
DEFUN("local-file-name", cmd_local_file_name, subr_local_file_name,
      (VALUE file), V_Subr1, DOC_local_file_name) /*
::doc:local_file_name::
local-file-name FILE-NAME

When possible, return a string absolutely naming the file in the local
file system that FILE-NAME refers to. If FILE-NAME does not refer to
a file in the local system, return nil.
::end:: */
{
    VALUE handler = expand_and_get_handler(&file, op_local_file_name);
    if(!handler)
	return LISP_NULL;
    if(NILP(handler))
	/* Assume that it's already a local file. */
	return file;
    else
	return call_handler(handler, op_local_file_name,
			    sym_local_file_name, 1, file);
}

_PR VALUE cmd_canonical_file_name(VALUE);
DEFUN("canonical-file-name", cmd_canonical_file_name, subr_canonical_file_name,
      (VALUE file), V_Subr1, DOC_canonical_file_name) /*
::doc:canonical_file_name::
canonical-file-name FILE-NAME

Return the canonical name of the file called FILE-NAME. The canonical name
of a file is defined such that two files can be compared simply by comparing
their canonical names; if the names match, they refer to the same file.

(Note that the opposite isn't always true, if two canonical names don't
match the file could still be the same, for example via links. On most
operating systems, symbolic links will be expanded where possible.)
::end:: */
{
    VALUE handler = expand_and_get_handler(&file, op_canonical_file_name);
    if(!handler)
	return LISP_NULL;
    if(NILP(handler))
	return sys_canonical_file_name(file);
    else
	return call_handler(handler, op_canonical_file_name,
			    sym_canonical_file_name, 1, file);
}

_PR VALUE cmd_file_name_nondirectory(VALUE);
DEFUN("file-name-nondirectory", cmd_file_name_nondirectory,
      subr_file_name_nondirectory, (VALUE file), V_Subr1,
      DOC_file_name_nondirectory) /*
::doc:file_name_nondirectory::
file-name-nondirectory FILE-NAME

Return the directory component of FILE-NAME, including the final
directory separator.
::end:: */
{
    VALUE handler;
    DECLARE1(file, STRINGP);
    handler = get_file_handler(file, op_file_name_nondirectory);
    if(NILP(handler))
	return sys_file_name_nondirectory(file);
    else
	return call_handler(handler, op_file_name_nondirectory,
			    sym_file_name_nondirectory, 1, file);
}

_PR VALUE cmd_file_name_directory(VALUE);
DEFUN("file-name-directory", cmd_file_name_directory,
      subr_file_name_directory, (VALUE file), V_Subr1,
      DOC_file_name_directory) /*
::doc:file_name_directory::
file-name-directory FILE-NAME

Return the file name component of FILE-NAME, i.e. everything following
the final directory separator.
::end:: */
{
    VALUE handler;
    DECLARE1(file, STRINGP);
    handler = get_file_handler(file, op_file_name_directory);
    if(NILP(handler))
	return sys_file_name_directory(file);
    else
	return call_handler(handler, op_file_name_directory,
			    sym_file_name_directory, 1, file);
}

_PR VALUE cmd_file_name_as_directory(VALUE);
DEFUN("file-name-as-directory", cmd_file_name_as_directory,
      subr_file_name_as_directory, (VALUE file), V_Subr1,
      DOC_file_name_as_directory) /*
::doc:file_name_as_directory::
file-name-as-directory FILE-NAME

Return FILE-NAME such that it names a directory (i.e with a terminating
directory separator character.)
::end:: */
{
    VALUE handler;
    DECLARE1(file, STRINGP);
    handler = get_file_handler(file, op_file_name_as_directory);
    if(NILP(handler))
	return sys_file_name_as_directory(file);
    else
	return call_handler(handler, op_file_name_as_directory,
			    sym_file_name_as_directory, 1, file);
}

_PR VALUE cmd_directory_file_name(VALUE);
DEFUN("directory-file-name", cmd_directory_file_name,
      subr_directory_file_name, (VALUE file), V_Subr1,
      DOC_directory_file_name) /*
::doc:directory_file_name::
directory-file-name DIR-NAME

Return the name of the file representing the directory called DIR-NAME.
This is the opposite of file-name-as-directory, since its effect is to
_remove_ any terminating directory separator.
::end:: */
{
    VALUE handler;
    DECLARE1(file, STRINGP);
    handler = get_file_handler(file, op_directory_file_name);
    if(NILP(handler))
	return sys_directory_file_name(file);
    else
	return call_handler(handler, op_directory_file_name,
			    sym_directory_file_name, 1, file);
}


/* File structures */

static VALUE
make_file(void)
{
    VALUE file = VAL(ALLOC_OBJECT(sizeof(Lisp_File)));
    if(file == LISP_NULL)
	return mem_error();
    VFILE(file)->car = V_File;
    VFILE(file)->name = sym_nil;
    VFILE(file)->handler = sym_nil;
    VFILE(file)->handler_data = sym_nil;
    VFILE(file)->file.stream = sym_nil;
    VFILE(file)->next = file_list;
    file_list = VFILE(file);
    return file;
}

void
file_sweep(void)
{
    Lisp_File *lf = file_list;
    file_list = NULL;
    while(lf)
    {
	Lisp_File *nxt = lf->next;
	if(!GC_CELL_MARKEDP(VAL(lf)))
	{
	    if(LOCAL_FILE_P(VAL(lf)) && !(lf->car & LFF_DONT_CLOSE))
		fclose(lf->file.fh);
	    FREE_OBJECT(lf);
	}
	else
	{
	    GC_CLR_CELL(VAL(lf));
	    lf->next = file_list;
	    file_list = lf;
	}
	lf = nxt;
    }
}

void
file_prin(VALUE strm, VALUE obj)
{
    stream_puts(strm, "#<file ", -1, FALSE);
    if(VFILE(obj)->name)
    {
	stream_puts(strm, VPTR(VFILE(obj)->name), -1, TRUE);
	stream_putc(strm, '>');
    }
    else
	stream_puts(strm, "*unbound*>", -1, FALSE);
}

_PR VALUE cmd_filep(VALUE arg);
DEFUN("filep", cmd_filep, subr_filep, (VALUE arg), V_Subr1, DOC_filep) /*
::doc:filep::
filep ARG

Returns t if ARG is a file object.
::end:: */
{
    return FILEP(arg) ? sym_t : sym_nil;
}

_PR VALUE cmd_file_binding(VALUE file);
DEFUN("file-binding", cmd_file_binding, subr_file_binding,
      (VALUE file), V_Subr1, DOC_file_binding) /*
::doc:file_binding::
file-binding FILE

Returns the name of the logical file that FILE was opened to access, or nil
if it has been closed, but is still to be garbage collected.
::end:: */
{
    DECLARE1(file, FILEP);
    return VFILE(file)->name;
}

_PR VALUE cmd_file_bound_stream(VALUE file);
DEFUN("file-bound-stream", cmd_file_bound_stream, subr_file_bound_stream,
      (VALUE file), V_Subr1, DOC_file_bound_stream) /*
::doc:file_bound_stream::
file-bound-stream FILE

If file object FILE doesn't refer to a local file, return the stream
that it's bound to.
::end:: */
{
    DECLARE1(file, FILEP);
    return !LOCAL_FILE_P(file) ? VFILE(file)->file.stream : sym_nil;
}

_PR VALUE cmd_file_handler_data(VALUE);
DEFUN("file-handler-data", cmd_file_handler_data, subr_file_handler_data,
      (VALUE file), V_Subr1, DOC_file_handler_data) /*
::doc:file_handler_data::
file-handler-data FILE

Return the handler-specific data for FILE.
::end:: */
{
    DECLARE1(file, FILEP);
    return VFILE(file)->handler_data;
}

_PR VALUE cmd_set_file_handler_data(VALUE, VALUE);
DEFUN("set-file-handler-data", cmd_set_file_handler_data,
      subr_set_file_handler_data, (VALUE file, VALUE data), V_Subr2,
      DOC_set_file_handler_data) /*
::doc:set_file_handler_data::
set-file-handler-data FILE DATA

Set the handler-specific data of file object FILE to DATA.
::end:: */
{
    DECLARE1(file, FILEP);
    VFILE(file)->handler_data = data;
    return data;
}


/* Low level file handling Lisp functions */

_PR VALUE cmd_open_file(VALUE, VALUE);
DEFUN("open-file", cmd_open_file, subr_open_file,
      (VALUE file_name, VALUE access_type), V_Subr2, DOC_open_file) /*
::doc:open_file::
open-file FILE-NAME ACCESS-TYPE

Return a new file object referencing the logical file called FILE-NAME,
for ACCESS-TYPE requests. ACCESS-TYPE can be one of the symbols:

	read		For input
	write		Truncate or create the file, and open for output
	append		Open for output at the end of the file.
::end:: */
{
    VALUE handler, file;
    GC_root gc;

    DECLARE1(file_name, STRINGP);
    DECLARE2(access_type, SYMBOLP);

    PUSHGC(gc, access_type);
    file_name = cmd_expand_file_name(file_name, sym_nil);
    POPGC;
    if(!file_name)
	return file_name;

    handler = get_file_handler(file_name, op_open_file);
    if(NILP(handler))
    {
	file = make_file();
	if(file != LISP_NULL)
	{
	    VFILE(file)->file.fh = fopen(VSTR(file_name),
					 (access_type == sym_write ? "w"
					  : (access_type == sym_append ? "a"
					     : "r")));
	    if(VFILE(file)->file.fh == 0)
		return signal_file_error(file_name);
	    VFILE(file)->handler = sym_t;
	    VFILE(file)->handler_data = file_name;
	}
    }
    else
	file = call_handler(handler, op_open_file, sym_open_file,
			    2, file_name, access_type);
    if(file && FILEP(file))
    {
	/* Install the original file name. */
	VFILE(file)->name = file_name;
    }
    return file;
}

_PR VALUE cmd_make_file_from_stream(VALUE, VALUE, VALUE);
DEFUN("make-file-from-stream", cmd_make_file_from_stream,
      subr_make_file_from_stream,
      (VALUE file_name, VALUE stream, VALUE handler),
      V_Subr3, DOC_make_file_from_stream) /*
::doc:make_file_from_stream::
make-file-from-stream FILE-NAME STREAM HANDLER

Return a new file object that refers to the logical file called FILE-NAME,
that is not in the local filing system. All access to the file object
will be directed through the stream object STREAM, and the file handler
function HANDLER.
::end:: */
{
    VALUE file;
    DECLARE1(file_name, STRINGP);
    file = make_file();
    if(file != LISP_NULL)
    {
	VFILE(file)->name = file_name;
	VFILE(file)->handler = handler;
	VFILE(file)->file.stream = stream;
    }
    return file;
}

_PR VALUE cmd_close_file(VALUE);
DEFUN("close-file", cmd_close_file, subr_close_file, (VALUE file), V_Subr1,
      DOC_close_file) /*
::doc:close_file::
close-file FILE

Signal that there will be no more I/O through the file object FILE.
::end:: */
{
    DECLARE1(file, FILEP);
    if(NILP(VFILE(file)->name))
	return unbound_file_error(file);
    if(LOCAL_FILE_P(file) && !(VFILE(file)->car & LFF_DONT_CLOSE))
	fclose(VFILE(file)->file.fh);
    else
	call_handler(VFILE(file)->handler, op_close_file,
		     sym_close_file, 1, file);
    VFILE(file)->name = sym_nil;
    VFILE(file)->handler = sym_nil;
    VFILE(file)->file.stream = sym_nil;
    return sym_t;
}

_PR VALUE cmd_flush_file(VALUE file);
DEFUN("flush-file", cmd_flush_file, subr_flush_file,
      (VALUE file), V_Subr1, DOC_flush_file) /*
::doc:flush_file::
flush-file FILE

Flush any buffered output on FILE. This is usually unnecessary since
all output will be flushed when FILE is eventually closed.
::end:: */
{
    DECLARE1(file, FILEP);
    if(NILP(VFILE(file)->name))
	return unbound_file_error(file);
    if(LOCAL_FILE_P(file))
	fflush(VFILE(file)->file.fh);
    else
	call_handler(VFILE(file)->handler, op_flush_file,
		     sym_flush_file, 1, file);
    return file;
}

_PR VALUE cmd_seek_file(VALUE file, VALUE offset, VALUE where);
DEFUN("seek-file", cmd_seek_file, subr_seek_file,
      (VALUE file, VALUE offset, VALUE where), V_Subr3, DOC_seek_file) /*
::doc:seek_file::
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
    DECLARE1(file, FILEP);
    if(!VFILE(file)->name)
	return unbound_file_error(file);
    if(LOCAL_FILE_P(file))
    {
	if(!INTP(offset))
	    return MAKE_INT(ftell(VFILE(file)->file.fh));
	else
	{
	    int whence = SEEK_CUR;
	    if(where == sym_start)
		whence = SEEK_SET;
	    else if(where == sym_end)
		whence = SEEK_END;
	    if(fseek(VFILE(file)->file.fh, VINT(offset), whence) != 0)
		return signal_file_error(LIST_1(file));
	    else
		return sym_t;
	}
    }
    else
	return call_handler(VFILE(file)->handler, op_seek_file, sym_seek_file,
			    3, file, offset, where);
}


/* Buffer-file functions */

_PR VALUE cmd_write_buffer_contents(VALUE, VALUE, VALUE);
DEFUN_INT("write-buffer-contents", cmd_write_buffer_contents,
	  subr_write_buffer_contents, (VALUE file, VALUE start, VALUE end),
	  V_Subr3, DOC_write_buffer_contents,
	  "-FWrite block to file:" DS_NL "m" DS_NL "M") /*
::doc:write_buffer_contents::
write-buffer-contents FILE-OR-NAME [START] [END]

Writes the text between positions START and END in the current buffer
to FILE-OR-NAME, which may be either a string naming a file to overwrite,
or a file object.

If START or END aren't defined they are taken from the start and end of
the buffer (ignoring the current restriction).
::end:: */
{
    TX *tx = curr_vw->vw_Tx;
    VALUE handler;
    GC_root gc_start, gc_end;

    PUSHGC(gc_start, start);
    PUSHGC(gc_end, end);
    handler = get_handler_from_file_or_name(&file, op_write_buffer_contents);
    POPGC; POPGC;
    if(handler == LISP_NULL)
	return handler;

    if(!POSP(start))
	start = cmd_start_of_buffer(VAL(tx), sym_t);
    if(!POSP(end))
	end = cmd_end_of_buffer(VAL(tx), sym_t);

    if(NILP(handler))
    {
	long row, col;
	LINE *line;
	FILE *fh;

	/* Don't call check_section() since that looks at the restriction. */
	if(POS_LESS_P(end, start) || VROW(start) < 0
	   || VROW(end) > tx->tx_NumLines)
	    return(cmd_signal(sym_invalid_area, list_3(VAL(tx), start, end)));

	if(FILEP(file))
	    fh = VFILE(file)->file.fh;
	else
	{
	    fh = fopen(VSTR(file), "w");
	    if(fh == 0)
		return signal_file_error(file);
	}

	row = VROW(start);
	line = tx->tx_Lines + row;
	col = MIN(VCOL(start), line->ln_Strlen - 1);

	while(row <= VROW(end))
	{
	    int len = (((row == VROW(end))
			? VCOL(end) : line->ln_Strlen - 1) - col);
	    if(len > 0 && fwrite(line->ln_Line + col, 1, len, fh) != len) 
	    {
		return signal_file_error(file);
	    }
	    if(row != VROW(end))
		fputc('\n', fh);
	    col = 0;
	    row++;
	    line++;
	}
	if(!FILEP(file))
	    fclose(fh);
	return file;
    }
    else
	return call_handler(handler, op_write_buffer_contents,
			    sym_write_buffer_contents, 3, file, start, end);
}

_PR VALUE cmd_read_file_contents(VALUE);
DEFUN("read-file-contents", cmd_read_file_contents, subr_read_file_contents,
      (VALUE file), V_Subr1, DOC_read_file_contents) /*
::doc:read_file_contents::
read-file-contents FILE-OR-NAME

Overwrites the text in BUFFER with that from the file FILE-OR-NAME.

FILE-OR-NAME is either a string naming the file to be opened or a Lisp file
object to be used. Also removes any restriction on BUFFER.
::end:: */
{
    VALUE handler
	= get_handler_from_file_or_name(&file, op_read_file_contents);
    TX *tx = curr_vw->vw_Tx;
    if(handler == LISP_NULL)
	return handler;

    cmd_unrestrict_buffer(VAL(tx));

    if(NILP(handler))
    {
	FILE *fh;
	u_long file_length;
	VALUE res = LISP_NULL;
	VALUE start;

	if(FILEP(file))
	{
	    fh = VFILE(file)->file.fh;
	    file_length = sys_file_length(VFILE(file)->handler_data);
	}
	else
	{
	    fh = fopen(VSTR(file), "r");
	    if(fh == 0)
		return signal_file_error(file);
	    file_length = sys_file_length(file);
	}

	start = make_pos(0, 0);
	undo_record_deletion(tx, start, cmd_end_of_buffer(VAL(tx), sym_t));
	kill_line_list(tx);
	if(read_file_into_tx(tx, fh, file_length))
	{
	    undo_record_insertion(tx, start, cmd_end_of_buffer(VAL(tx),
							       sym_t));
	    res = VAL(tx);
	}
	else
	    clear_line_list(tx);

	if(!FILEP(file))
	    fclose(fh);
	
	return res;
    }
    else
	return call_handler(handler, op_read_file_contents,
			    sym_read_file_contents, 1, file);
}

_PR VALUE cmd_insert_file_contents(VALUE);
DEFUN("insert-file-contents", cmd_insert_file_contents,
      subr_insert_file_contents, (VALUE file), V_Subr1,
      DOC_insert_file_contents) /*
::doc:insert_file_contents::
insert-file-contents FILE-OR-NAME

Insert the contents of FILE-OR-NAME (the name of a file, or a file object)
before the cursor in the current buffer.
::end:: */
{
    VALUE handler
	= get_handler_from_file_or_name(&file, op_insert_file_contents);
    if(handler == LISP_NULL)
	return handler;
    if(NILP(handler))
    {
	TX *tx = curr_vw->vw_Tx;
	FILE *fh;
	u_char buf[BUFSIZ];
	long len;
	VALUE pos = curr_vw->vw_CursorPos;

	if(FILEP(file))
	    fh = VFILE(file)->file.fh;
	else
	{
	    fh = fopen(VSTR(file), "r");
	    if(fh == 0)
		return signal_file_error(file);
	}

	while(pos != LISP_NULL && (len = fread(buf, 1, BUFSIZ, fh)) > 0)
	    pos = insert_string(tx, buf, len, pos);
	if(!FILEP(file))
	    fclose(fh);
	return VAL(tx);
    }
    else
	return call_handler(handler, op_insert_file_contents,
			    sym_insert_file_contents, 1, file);
}


/* General file operations */

_PR VALUE cmd_delete_file(VALUE);
DEFUN_INT("delete-file", cmd_delete_file, subr_delete_file, (VALUE file_name),
	  V_Subr1, DOC_delete_file, "fFile to delete:") /*
::doc:delete_file::
delete-file FILE-NAME

Delete the file called FILE-NAME.
::end:: */
{
    VALUE handler = expand_and_get_handler(&file_name, op_delete_file);
    if(!handler)
	return handler;
    if(NILP(handler))
	return sys_delete_file(file_name);
    else
	return call_handler(handler, op_delete_file, sym_delete_file,
			    1, file_name);
}

DEFSTRING(cant_rename, "Can't rename files across handlers");
_PR VALUE cmd_rename_file(VALUE, VALUE);
DEFUN_INT("rename-file", cmd_rename_file, subr_rename_file,
	  (VALUE old, VALUE new), V_Subr2, DOC_rename_file,
	  "fOld name of file:" DS_NL "FNew name of file:") /*
::doc:rename_file::
rename-file OLD-NAME NEW-NAME

Rename the file called OLD-NAME so that it is called NEW-NAME. Note that
this almost certainly won't work across filing systems.
::end:: */
{
    VALUE old_handler, new_handler;
    GC_root gc_old, gc_new;

    PUSHGC(gc_old, old);
    PUSHGC(gc_new, new);
    old_handler = localise_and_get_handler(&old, op_rename_file);
    new_handler = localise_and_get_handler(&new, op_rename_file);
    POPGC; POPGC;
    if(!old_handler || !new_handler)
	return LISP_NULL;

    if(old_handler == new_handler)
    {
	if(NILP(old_handler))
	    /* Both names on local fs. */
	    return sys_rename_file(old, new);
	else
	    return call_handler(old_handler, op_rename_file,
				sym_rename_file, 2, old, new);
    }
    else
	/* TODO: use copy ops to make this work. */
	return cmd_signal(sym_file_error, LIST_1(VAL(&cant_rename)));
}

_PR VALUE cmd_copy_file(VALUE, VALUE);
DEFUN_INT("copy-file", cmd_copy_file, subr_copy_file, (VALUE src, VALUE dst),
	  V_Subr2, DOC_copy_file, "fSource file:" DS_NL "FDestination file:") /*
::doc:copy_file::
copy-file SOURCE DESTINATION

Create a new copy of the file called SOURCE, as the file called DESTINATION.
::end:: */
{
    VALUE src_handler, dst_handler;
    GC_root gc_src, gc_dst;

    PUSHGC(gc_src, src);
    PUSHGC(gc_dst, dst);
    src_handler = localise_and_get_handler(&src, op_copy_file);
    dst_handler = localise_and_get_handler(&dst, op_copy_file);
    POPGC; POPGC;
    if(!src_handler || !dst_handler)
	return LISP_NULL;

    if(src_handler == dst_handler)
    {
	if(NILP(src_handler))
	    /* Both names on local fs. */
	    return sys_copy_file(src, dst);
	else
	    return call_handler(src_handler, op_copy_file,
				sym_copy_file, 2, src, dst);
    }
    else
    {
	/* Copy via local fs. */
	VALUE temp = cmd_make_temp_name(), res;
	if(!temp)
	    return temp;
	res = call_handler(src_handler, op_copy_file_to_local_fs,
			   sym_copy_file_to_local_fs, 2, src, temp);
	if(!res)
	    return res;
	res = call_handler(dst_handler, op_copy_file_from_local_fs,
			   sym_copy_file_from_local_fs, 2, temp, dst);
	remove(VSTR(temp));
	return res;
    }
}
    

/* File attribute operations */

_PR VALUE cmd_file_readable_p(VALUE file);
DEFUN("file-readable-p", cmd_file_readable_p, subr_file_readable_p,
      (VALUE file), V_Subr1, DOC_file_readable_p) /*
::doc:file_readable_p::
file-readable-p FILE-NAME

Returns t if the file called FILE-NAME is available for reading from.
::end:: */
{
    VALUE handler = expand_and_get_handler(&file, op_file_readable_p);
    if(!handler)
	return handler;
    if(NILP(handler))
	return sys_file_readable_p(file);
    else
	return call_handler(handler, op_file_readable_p,
			    sym_file_readable_p, 1, file);
}

_PR VALUE cmd_file_writable_p(VALUE file);
DEFUN("file-writable-p", cmd_file_writable_p, subr_file_writable_p,
      (VALUE file), V_Subr1, DOC_file_writeable_p) /*
::doc:file_writeable_p::
file-writable-p FILE-NAME

Returns t if the file called FILE-NAME is available for writing to.
::end:: */
{
    VALUE handler = expand_and_get_handler(&file, op_file_writable_p);
    if(!handler)
	return handler;
    if(NILP(handler))
	return sys_file_writable_p(file);
    else
	return call_handler(handler, op_file_writable_p,
			    sym_file_writable_p, 1, file);
}

_PR VALUE cmd_file_exists_p(VALUE file);
DEFUN("file-exists-p", cmd_file_exists_p, subr_file_exists_p,
      (VALUE file), V_Subr1, DOC_file_exists_p) /*
::doc:file_exists_p::
file-exists-p FILE-NAME

Returns t if the file called FILE-NAME exists.
::end:: */
{
    VALUE handler = expand_and_get_handler(&file, op_file_exists_p);
    if(!handler)
	return handler;
    if(NILP(handler))
	return sys_file_exists_p(file);
    else
	return call_handler(handler, op_file_exists_p,
			    sym_file_exists_p, 1, file);
}

_PR VALUE cmd_file_regular_p(VALUE file);
DEFUN("file-regular-p", cmd_file_regular_p, subr_file_regular_p,
      (VALUE file), V_Subr1, DOC_file_regular_p) /*
::doc:file_regular_p::
file-regular-p FILE-NAME

Returns t if the file called FILE-NAME is a normal file, ie, not a
directory, device, symbolic link, etc...
::end:: */
{
    VALUE handler = expand_and_get_handler(&file, op_file_regular_p);
    if(!handler)
	return handler;
    if(NILP(handler))
	return sys_file_regular_p(file);
    else
	return call_handler(handler, op_file_regular_p,
			    sym_file_regular_p, 1, file);
}

_PR VALUE cmd_file_directory_p(VALUE file);
DEFUN("file-directory-p", cmd_file_directory_p, subr_file_directory_p,
      (VALUE file), V_Subr1, DOC_file_directory_p) /*
::doc:file_directory_p::
file-directory-p FILE-NAME

Returns t if the file called FILE-NAME is a directory.
::end:: */
{
    VALUE handler = expand_and_get_handler(&file, op_file_directory_p);
    if(!handler)
	return handler;
    if(NILP(handler))
	return sys_file_directory_p(file);
    else
	return call_handler(handler, op_file_directory_p,
			    sym_file_directory_p, 1, file);
}

_PR VALUE cmd_file_symlink_p(VALUE file);
DEFUN("file-symlink-p", cmd_file_symlink_p, subr_file_symlink_p,
      (VALUE file), V_Subr1, DOC_file_symlink_p) /*
::doc:file_symlink_p::
file-symlink-p FILE-NAME

Returns t if the file called FILE-NAME is a symbolic link to another file.
::end:: */
{
    VALUE handler = expand_and_get_handler(&file, op_file_symlink_p);
    if(!handler)
	return handler;
    if(NILP(handler))
	return sys_file_symlink_p(file);
    else
	return call_handler(handler, op_file_symlink_p,
			    sym_file_symlink_p, 1, file);
}

_PR VALUE cmd_file_owner_p(VALUE file);
DEFUN("file-owner-p", cmd_file_owner_p, subr_file_owner_p,
      (VALUE file), V_Subr1, DOC_file_owner_p) /*
::doc:file_owner_p::
file-owner-p FILE-NAME

Returns t if the ownership (uid & gid) of the file called FILE-NAME is the
same as that of any files written by the editor.
::end:: */
{
    VALUE handler = expand_and_get_handler(&file, op_file_owner_p);
    if(!handler)
	return handler;
    if(NILP(handler))
	return sys_file_owner_p(file);
    else
	return call_handler(handler, op_file_owner_p,
			    sym_file_owner_p, 1, file);
}

_PR VALUE cmd_file_nlinks(VALUE file);
DEFUN("file-nlinks", cmd_file_nlinks, subr_file_nlinks,
      (VALUE file), V_Subr1, DOC_file_nlinks) /*
::doc:file_nlinks::
file-nlinks FILE-NAME

Returns the number of links pointing to the file called FILE-NAME. This will
be one if FILE-NAME has only one name. Doesn't count symbolic links.
::end:: */
{
    VALUE handler = expand_and_get_handler(&file, op_file_nlinks);
    if(!handler)
	return handler;
    if(NILP(handler))
	return sys_file_nlinks(file);
    else
	return call_handler(handler, op_file_nlinks, sym_file_nlinks, 1, file);
}

_PR VALUE cmd_file_size(VALUE file);
DEFUN("file-size", cmd_file_size, subr_file_size,
      (VALUE file), V_Subr1, DOC_file_size) /*
::doc:file_size::
file-size FILE-NAME

Returns the size of the file called FILE-NAME in bytes.
::end:: */
{
    VALUE handler = expand_and_get_handler(&file, op_file_size);
    if(!handler)
	return handler;
    if(NILP(handler))
	return sys_file_size(file);
    else
	return call_handler(handler, op_file_size, sym_file_size, 1, file);
}

_PR VALUE cmd_file_modes(VALUE file);
DEFUN("file-modes", cmd_file_modes, subr_file_modes,
      (VALUE file), V_Subr1, DOC_file_modes) /*
::doc:file_modes::
file-modes FILE-NAME

Return the access permissions of the file called FILE-NAME. Note that the
format of this object is filing system dependent. It's only portable use
is as an argument to set-file-modes.
::end:: */
{
    VALUE handler = expand_and_get_handler(&file, op_file_modes);
    if(!handler)
	return handler;
    if(NILP(handler))
	return sys_file_modes(file);
    else
	return call_handler(handler, op_file_modes, sym_file_modes, 1, file);
}

_PR VALUE cmd_set_file_modes(VALUE file, VALUE modes);
DEFUN("set-file-modes", cmd_set_file_modes, subr_set_file_modes,
      (VALUE file, VALUE modes), V_Subr2, DOC_set_file_modes) /*
::doc:set_file_modes::
set-file-modes FILE-NAME MODES

Sets the access permissions of the file called FILE-NAME to MODES. The only
portable way of getting MODES is from the `file-modes' function since it
may change across filing systems.
::end:: */
{
    VALUE handler = expand_and_get_handler(&file, op_set_file_modes);
    if(!handler)
	return handler;
    if(NILP(handler))
	return sys_set_file_modes(file, modes);
    else
	return call_handler(handler, op_set_file_modes,
			    sym_set_file_modes, 2, file, modes);
}

_PR VALUE cmd_file_modes_as_string(VALUE file);
DEFUN("file-modes-as-string", cmd_file_modes_as_string,
      subr_file_modes_as_string, (VALUE file), V_Subr1,
      DOC_file_modes_as_string) /*
::doc:file_modes_as_string::
file-modes-as-string FILE-NAME

Returns a ten character string describing the attributes of the file
called FILE-NAME.
::end:: */
{
    VALUE handler = expand_and_get_handler(&file, op_file_modes_as_string);
    if(!handler)
	return handler;
    if(NILP(handler))
	return sys_file_modes_as_string(file);
    else
	return call_handler(handler, op_file_modes_as_string,
			    sym_file_modes_as_string, 1, file);
}

_PR VALUE cmd_file_modtime(VALUE file);
DEFUN("file-modtime", cmd_file_modtime, subr_file_modtime,
      (VALUE file), V_Subr1, DOC_file_modtime) /*
::doc:file_modtime::
file-modtime FILE-NAME

Return the time that the file called FILE-NAME was last modified, as a cons
cell storing two integers, the low 24 bits, and the high bits.
::end:: */
{
    VALUE handler = expand_and_get_handler(&file, op_file_modtime);
    if(!handler)
	return handler;
    if(NILP(handler))
	return sys_file_modtime(file);
    else
	return call_handler(handler, op_file_modtime,
			    sym_file_modtime, 1, file);
}

bool
file_newer_than(VALUE name1, VALUE name2)
{
    bool res = FALSE;
    VALUE time1;
    GC_root gc_name1, gc_name2;

    PUSHGC(gc_name1, name1);
    PUSHGC(gc_name2, name2);
    time1 = cmd_file_modtime(name1);
    if(time1 && !NILP(time1))
    {
	VALUE time2;
	GC_root gc_time1;

	PUSHGC(gc_time1, time1);
	time2 = cmd_file_modtime(name2);
	POPGC;

	if(time2 && !NILP(time2))
	{
	    VALUE foo = cmd_time_later_p(time1, time2);
	    if(foo && !NILP(foo))
		res = TRUE;
	}
    }
    POPGC; POPGC;

    return res;
}

_PR VALUE cmd_directory_files(VALUE dir);
DEFUN("directory-files", cmd_directory_files, subr_directory_files,
      (VALUE dir), V_Subr1, DOC_directory_files) /*
::doc:directory_files::
directory-files DIRECTORY

Returns a list of the names of all files in the directory called DIRECTORY.
The list is unsorted.
::end:: */
{
    VALUE handler = expand_and_get_handler(&dir, op_directory_files);
    if(!handler)
	return handler;
    if(NILP(handler))
	return sys_directory_files(dir);
    else
	return call_handler(handler, op_directory_files,
			    sym_directory_files, 1, dir);
}


/* Utility functions */

DEFSTRING(stdin_name, "<stdin>");

_PR VALUE cmd_stdin_file(void);
DEFUN("stdin-file", cmd_stdin_file, subr_stdin_file, (void), V_Subr0, DOC_stdin_file) /*
::doc:stdin_file::
stdin-file

Returns the file object representing the editor's standard input.
::end:: */
{
    static VALUE stdin_file;
    if(stdin_file)
	return stdin_file;
    stdin_file = make_file();
    VFILE(stdin_file)->name = VAL(&stdin_name);
    VFILE(stdin_file)->handler = sym_t;
    VFILE(stdin_file)->file.fh = stdin;
    VFILE(stdin_file)->car |= LFF_DONT_CLOSE;
    mark_static(&stdin_file);
    return stdin_file;
}

DEFSTRING(stdout_name, "<stdout>");

_PR VALUE cmd_stdout_file(void);
DEFUN("stdout-file", cmd_stdout_file, subr_stdout_file, (void), V_Subr0, DOC_stdout_file) /*
::doc:stdout_file::
stdout-file

Returns the file object representing the editor's standard output.
::end:: */
{
    static VALUE stdout_file;
    if(stdout_file)
	return stdout_file;
    stdout_file = make_file();
    VFILE(stdout_file)->name = VAL(&stdout_name);
    VFILE(stdout_file)->handler = sym_t;
    VFILE(stdout_file)->file.fh = stdout;
    VFILE(stdout_file)->car |= LFF_DONT_CLOSE;
    mark_static(&stdout_file);
    return stdout_file;
}

DEFSTRING(stderr_name, "<stderr>");

_PR VALUE cmd_stderr_file(void);
DEFUN("stderr-file", cmd_stderr_file, subr_stderr_file, (void), V_Subr0, DOC_stderr_file) /*
::doc:stderr_file::
stderr-file

Returns the file object representing the editor's standard output.
::end:: */
{
    static VALUE stderr_file;
    if(stderr_file)
	return stderr_file;
    stderr_file = make_file();
    VFILE(stderr_file)->name = VAL(&stderr_name);
    VFILE(stderr_file)->handler = sym_t;
    VFILE(stderr_file)->file.fh = stderr;
    VFILE(stderr_file)->car |= LFF_DONT_CLOSE;
    mark_static(&stderr_file);
    return stderr_file;
}

_PR VALUE cmd_make_temp_name(void);
DEFSTRING(no_temp, "Can't create temporary file name");
DEFUN("make-temp-name", cmd_make_temp_name, subr_make_temp_name, (void), V_Subr0, DOC_make_temp_name) /*
::doc:make_temp_name::
make-temp-name

Returns the name of a unique file in the local filing system.
::end:: */
{
    char buf[L_tmpnam];
    if(tmpnam(buf))
	return string_dup(buf);
    else
	return signal_file_error(VAL(&no_temp));
}


/* Low level stuff */

/* The average number of chars-per-line in the last file read. At first
   I tried an average over all files ever loaded; this seems better since
   likely use will involve locality in loading similar files.
   Initialised to a guessed value */
static u_long last_avg_line_length = 40;

/* Read a file into a tx structure, the line list should have been
   killed. FILE-LENGTH is the length of the file to be loaded, or -1
   if the length is unknown. */
static bool
read_file_into_tx(TX *tx, FILE *fh, long file_length)
{
    bool rc = FALSE;
    u_char buf[BUFSIZ];
    long len, linenum, alloced_lines, chars_read = 0;
    LINE *line;

    /* First calculate the rate at which the line list is allocated. */
    if(file_length > 0)
    {
	/* We know the length of the file, and the average chars-per-line
	   of the last file loaded. */
	long predicted_lines = file_length / last_avg_line_length;
	if(predicted_lines < 64)
	    predicted_lines = 64;
	if(predicted_lines > 1024)
	    predicted_lines = 1024;
	if(!resize_line_list(tx, predicted_lines, 0))
	    goto abortmem;
	alloced_lines = predicted_lines;
    }
    else
    {
	/* Don't know the length of the file. Let resize_line_list()
	   take care of everything. */
	if(!resize_line_list(tx, 1, 0))
	    goto abortmem;
	alloced_lines = 1;
    }
    linenum = 0;
    line = tx->tx_Lines;

    while((len = fread(buf, 1, BUFSIZ, fh)) > 0)
    {
	u_char *new;
	long newlen;
	u_char *eol, *cur = buf;
	while((eol = memchr(cur, '\n', (buf + len) - cur)))
	{
	    if(line->ln_Strlen != 0)
	    {
		newlen = line->ln_Strlen + (eol - cur);
		new = alloc_line_buf(tx, newlen);
		memcpy(new, line->ln_Line, line->ln_Strlen);
		memcpy(new + line->ln_Strlen - 1, cur, eol - cur);
		new[newlen-1] = 0;
		free_line_buf(tx, line->ln_Line);
		line->ln_Line = new;
		line->ln_Strlen = newlen;
	    }
	    else
	    {
		newlen = eol - cur;
		new = alloc_line_buf(tx, newlen + 1);
		if(new == NULL)
		    goto abortmem;
		memcpy(new, cur, newlen);
		new[newlen] = 0;
		line->ln_Line = new;
		line->ln_Strlen = newlen+1;
	    }
	    chars_read += line->ln_Strlen;

	    if(++linenum >= alloced_lines)
	    {
		/* Need to grow the line list. If we don't know the size
		   of the file just pass it off to resize_line_list().. */
		if(file_length < 0)
		{
		    if(!resize_line_list(tx, 1, linenum))
			goto abortmem;
		    alloced_lines++;
		}
		else
		{
		    /* We know the file_length, and the average bytes-per-line
		       so far. Re-calibrate our prediction of the total
		       number of lines. */
		    long predicted_lines = file_length * linenum / chars_read;
		    /* Some restrictions on the growth rate */
		    if(predicted_lines < linenum + 32)
			predicted_lines = linenum + 32;
		    if(predicted_lines > linenum + 1024)
			predicted_lines = linenum + 1024;
		    if(!resize_line_list(tx, predicted_lines - alloced_lines,
					 linenum))
			goto abortmem;
		    alloced_lines = predicted_lines;
		}
		line = tx->tx_Lines + linenum;
	    }
	    else
		line++;
	    cur = eol + 1;
	}
	if(cur < buf + len)
	{
            if(line->ln_Strlen)
	    {
                /* Only way we can get here is if there were *no* newlines in
                   the chunk we just read. */
		newlen = line->ln_Strlen + len;
		new = alloc_line_buf(tx, newlen);
		if(!new)
		    goto abortmem;
		memcpy(new, line->ln_Line, line->ln_Strlen - 1);
		memcpy(new + (line->ln_Strlen - 1), buf, len);
		new[newlen-1] = 0;
		free_line_buf(tx, line->ln_Line);
		line->ln_Line = new;
		line->ln_Strlen = newlen;
	    }
            else
	    {
		newlen = (buf + len) - cur;
		line->ln_Line = alloc_line_buf(tx, newlen + 1);
		if(!line->ln_Line)
		    goto abortmem;
		memcpy(line->ln_Line, cur, newlen);
		line->ln_Line[newlen] = 0;
		line->ln_Strlen = newlen + 1;
	    }
	}
    }
    if(line->ln_Strlen == 0)
    {
	line->ln_Line = alloc_line_buf(tx, 1);
	if(line->ln_Line == NULL)
	    goto abortmem;
	line->ln_Line[0] = 0;
	line->ln_Strlen = 1;
    }
    else
	chars_read += line->ln_Strlen;
    linenum++;

    if(!resize_line_list(tx, linenum - alloced_lines, linenum))
	goto abortmem;

    /* If the average line length seems sensible, set it as the
       length of the "last-read" file */
    if(chars_read / linenum > 10 && chars_read / linenum < 100)
	last_avg_line_length = chars_read / linenum;

    tx->tx_LogicalStart = 0;
    tx->tx_LogicalEnd = tx->tx_NumLines;

    tx->tx_Changes++;
    rc = TRUE;

    if(0)
    {
	/* This only gets executed if we aborted while reading the file. */
abortmem:
	mem_error();
	clear_line_list(tx);
    }

    return(rc);
}


/* init */

void
files_init(void)
{
    INTERN(file_handler_alist); DOC(file_handler_alist);
    VSYM(sym_file_handler_alist)->value = sym_nil;

    INTERN(default_directory); DOC(default_directory);
    VSYM(sym_default_directory)->value = sys_getpwd();
    if(VSYM(sym_default_directory)->value == LISP_NULL)
	VSYM(sym_default_directory)->value = null_string();
    cmd_make_variable_buffer_local(sym_default_directory);

    INTERN(file_name_absolute_p);
    INTERN(expand_file_name);
    INTERN(local_file_name);
    INTERN(canonical_file_name);
    INTERN(file_name_nondirectory);
    INTERN(file_name_directory);
    INTERN(file_name_as_directory);
    INTERN(directory_file_name);
    INTERN(open_file);
    INTERN(close_file);
    INTERN(flush_file);
    INTERN(seek_file);
    INTERN(write_buffer_contents);
    INTERN(read_file_contents);
    INTERN(insert_file_contents);
    INTERN(delete_file);
    INTERN(rename_file);
    INTERN(copy_file);
    INTERN(copy_file_to_local_fs);
    INTERN(copy_file_from_local_fs);
    INTERN(file_readable_p);
    INTERN(file_writable_p);
    INTERN(file_exists_p);
    INTERN(file_regular_p);
    INTERN(file_directory_p);
    INTERN(file_symlink_p);
    INTERN(file_owner_p);
    INTERN(file_nlinks);
    INTERN(file_size);
    INTERN(file_modes);
    INTERN(set_file_modes);
    INTERN(file_modes_as_string);
    INTERN(file_modtime);
    INTERN(directory_files);

    INTERN(start); INTERN(end);
    INTERN(read); INTERN(write); INTERN(append);

    ADD_SUBR(subr_filep);
    ADD_SUBR(subr_file_binding);
    ADD_SUBR(subr_file_bound_stream);
    ADD_SUBR(subr_file_handler_data);
    ADD_SUBR(subr_set_file_handler_data);

    ADD_SUBR(subr_file_name_absolute_p);
    ADD_SUBR(subr_expand_file_name);
    ADD_SUBR(subr_local_file_name);
    ADD_SUBR(subr_canonical_file_name);
    ADD_SUBR(subr_file_name_nondirectory);
    ADD_SUBR(subr_file_name_directory);
    ADD_SUBR(subr_file_name_as_directory);
    ADD_SUBR(subr_directory_file_name);

    ADD_SUBR(subr_open_file);
    ADD_SUBR(subr_close_file);
    ADD_SUBR(subr_flush_file);
    ADD_SUBR(subr_seek_file);

    ADD_SUBR_INT(subr_write_buffer_contents);
    ADD_SUBR(subr_read_file_contents);
    ADD_SUBR(subr_insert_file_contents);

    ADD_SUBR_INT(subr_delete_file);
    ADD_SUBR_INT(subr_rename_file);
    ADD_SUBR_INT(subr_copy_file);

    ADD_SUBR(subr_file_readable_p);
    ADD_SUBR(subr_file_writable_p);
    ADD_SUBR(subr_file_exists_p);
    ADD_SUBR(subr_file_regular_p);
    ADD_SUBR(subr_file_directory_p);
    ADD_SUBR(subr_file_symlink_p);
    ADD_SUBR(subr_file_owner_p);
    ADD_SUBR(subr_file_nlinks);
    ADD_SUBR(subr_file_size);
    ADD_SUBR(subr_file_modes);
    ADD_SUBR(subr_set_file_modes);
    ADD_SUBR(subr_file_modes_as_string);
    ADD_SUBR(subr_file_modtime);
    ADD_SUBR(subr_directory_files);

    ADD_SUBR(subr_stdin_file);
    ADD_SUBR(subr_stdout_file);
    ADD_SUBR(subr_stderr_file);
    ADD_SUBR(subr_make_temp_name);
}

void
files_kill(void)
{
    Lisp_File *lf = file_list;
    while(lf)
    {
	Lisp_File *nxt = lf->next;
	if(LOCAL_FILE_P(VAL(lf)) && !(lf->car & LFF_DONT_CLOSE))
	    fclose(lf->file.fh);
	FREE_OBJECT(lf);
	lf = nxt;
    }
    file_list = NULL;
}
