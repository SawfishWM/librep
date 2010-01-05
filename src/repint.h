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
#ifdef __OpenBSD__
    /* MAX and MIN these are defined in <sys/param.h> on OpenBSD
     * We include that here as sometimes it's included in other
     * places and sometimes not - this ensures we don't redefine
     * these two macros */
# include <sys/param.h>
#else
# define MAX(x,y) (((x) > (y)) ? (x) : (y))
# define MIN(x,y) (((x) < (y)) ? (x) : (y))
#endif
#define POS(x)   MAX(x, 0)
#define ABS(x)   MAX(x, -(x))

#define rep_INTERNAL 1
#include "rep.h"

#ifndef ENABLE_BROKEN_DUMPING
  /* No point incurring the overhead if it's unnecessary */
# undef rep_CONS_WRITABLE_P
# define rep_CONS_WRITABLE_P(x) rep_TRUE
#endif

#ifdef rep_HAVE_UNIX
# include "unix_defs.h"
#else
# error "Need an operating system definition"
#endif

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
    op_file_executable_p,
    op_file_exists_p,
    op_file_regular_p,
    op_file_directory_p,
    op_file_symlink_p,
    op_file_owner_p,
    op_file_gid_p,
    op_file_uid_p,
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


/* module system */

typedef struct rep_struct_node_struct rep_struct_node;
struct rep_struct_node_struct {
    rep_struct_node *next;
    repv symbol;
    repv binding;
    unsigned int is_constant : 1;
    unsigned int is_exported : 1;
};

/* structure encapsulating a single namespace */
typedef struct rep_struct_struct rep_struct;
struct rep_struct_struct {
    repv car;
    rep_struct *next;
    repv name;
    repv inherited;	/* exported symbols that have no local binding */
    int total_buckets, total_bindings;
    rep_struct_node **buckets;
    repv imports;
    repv accessible;

    /* A list of the special variables that may be accessed in this
       environment, or Qt to denote all specials. */
    repv special_env;

    /* Bytecode interpreter to use when calling functions defined here.
       If null, call rep_apply_bytecode  */
    repv (*apply_bytecode) (repv subr, int nargs, repv *args);
};

extern int rep_structure_type;

#define rep_STRUCTUREP(v) rep_CELL16_TYPEP(v, rep_structure_type)
#define rep_STRUCTURE(v)  ((rep_struct *) rep_PTR(v))

/* If set, currently recursively searching this module for a binding */
#define rep_STF_EXCLUSION	(1 << (rep_CELL16_TYPE_BITS + 0))

/* If set, all (local) bindings are exported by default. */
#define rep_STF_EXPORT_ALL	(1 << (rep_CELL16_TYPE_BITS + 1))

/* If set, bindings can be created by setq et al. */
#define rep_STF_SET_BINDS	(1 << (rep_CELL16_TYPE_BITS + 2))

#define rep_SPECIAL_ENV   (rep_STRUCTURE(rep_structure)->special_env)

#define rep_STRUCT_HASH(x,n) (((x) >> 3) % (n))


/* binding tracking */

#define rep_MARK_LEX_BINDING(x)		(x + (1 << rep_VALUE_INT_SHIFT))
#define rep_MARK_SPEC_BINDING(x)	(x + (1 << (16 + rep_VALUE_INT_SHIFT)))
#define rep_LEX_BINDINGS(x)		(rep_INT(x) & 0xffff)
#define rep_SPEC_BINDINGS(x)		(rep_INT(x) >> 16)
#define rep_NEW_FRAME			rep_MAKE_INT(0)

#define rep_USE_FUNARG(f)				\
    do {						\
	rep_env = rep_FUNARG(f)->env;			\
	rep_structure = rep_FUNARG(f)->structure;	\
    } while (0)

#define rep_USE_DEFAULT_ENV			\
    do {					\
	rep_env = Qnil;				\
	rep_structure = rep_default_structure;	\
    } while (0)


/* call history */

/* Keeps a backtrace of all lisp functions called. */
struct rep_Call {
    struct rep_Call *next;
    repv fun;
    repv args;
    repv current_form;			/* used for debugging, set by progn */
    repv saved_env;
    repv saved_structure;
};

#define rep_PUSH_CALL(lc)		\
    do {				\
	(lc).current_form = rep_NULL;	\
	(lc).saved_env = rep_env;	\
	(lc).saved_structure = rep_structure; \
	(lc).next = rep_call_stack;	\
	rep_call_stack = &(lc);		\
    } while (0)

#define rep_POP_CALL(lc)		\
    do {				\
	rep_env = (lc).saved_env;	\
	rep_structure = (lc).saved_structure; \
	rep_call_stack = (lc).next;	\
    } while (0)


/* guardians */

typedef struct rep_guardian_struct {
    repv car;
    struct rep_guardian_struct *next;
    repv accessible;
    repv inaccessible;
} rep_guardian;


/* cons' */

#define rep_CONSBLK_SIZE	1022		/* ~8k */

/* Structure of cons allocation blocks */
typedef struct rep_cons_block_struct {
    union {
	struct rep_cons_block_struct *p;
	/* ensure that the following cons cell is aligned to at
	   least sizeof (rep_cons) (for the dcache) */
	rep_cons dummy;
    } next;
    rep_cons cons[rep_CONSBLK_SIZE];
} rep_cons_block;


/* prototypes */

#include "repint_subrs.h"

/* If using GCC, make inline_Fcons be Fcons that only takes a procedure
   call when the heap needs to grow. */

#if defined __GNUC__ && defined __OPTIMIZE__
extern __inline__ repv inline_Fcons (repv x, repv y);
extern __inline__ repv
inline_Fcons (repv x, repv y)
{
    rep_cons *c = rep_cons_freelist;
    if (c == 0)
	c = rep_allocate_cons ();
    rep_cons_freelist = rep_CONS (c->cdr);
    rep_used_cons++;
    rep_data_after_gc += sizeof(rep_cons);

    c->car = (x);
    c->cdr = (y);
    return rep_CONS_VAL (c);
}
#else
# define inline_Fcons Fcons
#endif

#endif /* REPINT_H */
