/* repint.h -- Main include file for library internal objects
   Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef REPINT_H
#define REPINT_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* Maximum/minimum macros. Don't use when X or Y have side-effects! */
#define MAX(x,y) (((x) > (y)) ? (x) : (y))
#define MIN(x,y) (((x) < (y)) ? (x) : (y))

#include "rep.h"

#ifdef rep_HAVE_UNIX
# include "unix_defs.h"
#else
# error "Need an operating system definition"
#endif

#include "repint_subrs.h"
#include <build.h>

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
    op_write_buffer_contents,		/* these three for jade */
    op_read_file_contents,
    op_insert_file_contents,
    op_delete_file,
    op_rename_file,
    op_make_directory,
    op_delete_directory,
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
    op_read_symlink,
    op_make_symlink,

    op_MAX
};

struct blocked_op {
    struct blocked_op *next;
    repv handler;
};

extern struct blocked_op *rep_blocked_ops[op_MAX];

#endif /* REPINT_H */
