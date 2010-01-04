/* repint_subrs.h -- library-local prototypes
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

#ifndef REPINT_SUBRS_H
#define REPINT_SUBRS_H

/* from continuations.c */
extern void rep_continuations_init (void);

/* from datums.c */
extern void rep_pre_datums_init (void);
extern void rep_datums_init (void);

/* from files.c */
extern void rep_files_init(void);
extern void rep_files_kill(void);

/* from find.c */
extern struct rep_saved_regexp_data *rep_saved_matches;
extern void rep_string_modified (repv string);
extern void rep_mark_regexp_data(void);
extern void rep_find_init(void);
extern void rep_find_kill(void);

/* from fluids.c */
extern void rep_fluids_init (void);

/* from lisp.c */
extern repv rep_scm_t, rep_scm_f;
extern repv rep_readl(repv, int *);
extern repv rep_eval (repv form, repv tail_posn);
extern void rep_lisp_prin(repv, repv);
extern void rep_string_princ(repv, repv);
extern void rep_string_print(repv, repv);
extern repv rep_copy_list(repv);
extern rep_bool rep_compare_error(repv error, repv handler);
extern void rep_lisp_init(void);
extern rep_bool rep_single_step_flag;

/* from lispcmds.c */
extern rep_xsubr Slambda;
extern repv Qload_filename;
extern repv Fcall_with_exception_handler (repv, repv);
extern void rep_lispcmds_init(void);
extern repv Flist_star (int argc, repv *argv);
extern repv Fnconc_ (int argc, repv *argv);
extern repv Fappend (int argc, repv *argv);
extern repv Fvector (int argc, repv *argv);
extern repv Fconcat (int, repv *);
extern repv Fnum_eq (int, repv *);
extern repv Fnum_noteq (int, repv *);
extern repv Fgtthan (int, repv *);
extern repv Fgethan (int, repv *);
extern repv Fltthan (int, repv *);
extern repv Flethan (int, repv *);

/* from lispmach.c */
extern repv Qbytecode_error;
extern repv Frun_byte_code(repv code, repv consts, repv stkreq);
extern repv rep_apply_bytecode (repv subr, int nargs, repv *args);
extern void rep_lispmach_init(void);
extern void rep_lispmach_kill(void);

/* from main.c */
extern char *rep_stack_bottom;
extern void rep_deprecated (rep_bool *seen, const char *desc);

/* from macros.c */
extern void rep_macros_before_gc (void);
extern void rep_macros_clear_history (void);
extern void rep_macros_init (void);

/* from misc.c */
#ifndef HAVE_STPCPY
extern char *stpcpy(char *, const char *);
#endif
#ifndef HAVE_STRNCASECMP
extern int strncasecmp (const char *s1, const char *s2, size_t n);
#endif
extern void rep_misc_init(void);

/* from numbers.c */
extern repv rep_parse_number (char *buf, unsigned int len, unsigned int radix,
			      int sign, unsigned int type);
extern void rep_numbers_init (void);
extern repv Fplus(int, repv *);
extern repv Fminus(int, repv *);
extern repv Fproduct(int, repv *);
extern repv Fdivide(int, repv *);
extern repv Flogior(int, repv *);
extern repv Flogxor(int, repv *);
extern repv Flogand(int, repv *);
extern repv Fmax(int, repv *);
extern repv Fmin(int, repv *);
extern repv Fgcd (int, repv *);

/* from origin.c */
extern rep_bool rep_record_origins;
extern void rep_record_origin (repv form, repv stream, long start_line);
extern repv Flexical_origin (repv form);
extern void rep_mark_origins (void);
extern void rep_origin_init (void);

/* from regsub.c */
extern void rep_default_regsub(int, rep_regsubs *, char *, char *, void *);
extern int rep_default_regsublen(int, rep_regsubs *, char *, void *);

/* from streams.c */
extern void rep_streams_init(void);

/* from structures.c */
extern repv rep_default_structure, rep_specials_structure;
extern repv Qfeatures, Q_structures, Q_meta, Qrep, Q_specials,
    Q_user_structure, Qrep_structures, Qrep_lang_interpreter,
    Qrep_vm_interpreter, Qexternal, Qinternal;
extern rep_struct_node *rep_search_imports (rep_struct *s, repv var);
extern repv Fmake_structure (repv, repv, repv, repv);
extern repv F_structure_ref (repv, repv);
extern repv Fstructure_set (repv, repv, repv);
extern repv Fstructure_define (repv, repv, repv);
extern repv Fstructure_bound_p (repv, repv);
extern repv Fexternal_structure_ref (repv, repv);
extern repv Fintern_structure (repv);
extern repv Fget_structure (repv);
extern repv Fexport_binding (repv var);
extern repv rep_get_initial_special_value (repv sym);
extern repv rep_documentation_property (repv structure);
extern void rep_pre_structures_init (void);
extern void rep_structures_init (void);

/* from symbols.c */
extern repv rep_keyword_obarray;
extern int rep_pre_symbols_init(void);
extern void rep_symbols_init(void);
extern int rep_allocated_funargs, rep_used_funargs;
extern repv Freal_set (repv var, repv value);
extern repv rep_bind_special (repv oldList, repv symbol, repv newVal);

/* from tuples.c */
extern int rep_allocated_tuples, rep_used_tuples;
extern void rep_sweep_tuples (void);
extern void rep_tuples_kill(void);

/* from values.c */
extern int rep_type_cmp(repv, repv);
extern int rep_ptr_cmp(repv, repv);
extern rep_cons_block *rep_cons_block_chain;
extern rep_cons *rep_cons_freelist;
extern int rep_allocated_cons, rep_used_cons;
extern rep_cons *rep_allocate_cons (void);
extern void rep_cons_free(repv);
extern void rep_pre_values_init (void);
extern void rep_values_init(void);
extern void rep_values_kill (void);
extern void rep_dumped_init(char *file);

/* from weak-refs.c */
extern repv Fmake_weak_ref (repv value);
extern repv Fweak_ref (repv ref);
extern repv Fweak_ref_set (repv ref, repv value);
extern void rep_scan_weak_refs (void);
extern void rep_weak_refs_init (void);

#ifdef rep_HAVE_UNIX

/* from unix_dl.c */
extern repv rep_open_dl_library(repv file_name);
extern void rep_mark_dl_data(void);
extern void rep_kill_dl_libraries(void);
extern int rep_intern_dl_library (repv file_name);
extern void *rep_lookup_dl_symbol (int idx, const char *name);

/* from unix_files.c */
extern repv rep_file_name_absolute_p(repv file);
extern repv rep_expand_file_name(repv file);
extern repv rep_canonical_file_name(repv file);
extern repv rep_file_name_nondirectory(repv file);
extern repv rep_file_name_directory(repv file);
extern repv rep_file_name_as_directory(repv file);
extern repv rep_directory_file_name(repv file);
extern repv rep_delete_file(repv file);
extern repv rep_rename_file(repv old, repv new_);
extern repv rep_make_directory(repv dir);
extern repv rep_delete_directory(repv dir);
extern repv rep_copy_file(repv src, repv dst);
extern repv rep_file_readable_p(repv file);
extern repv rep_file_writable_p(repv file);
extern repv rep_file_executable_p(repv file);
extern repv rep_file_exists_p(repv file);
extern repv rep_file_regular_p(repv file);
extern repv rep_file_directory_p(repv file);
extern repv rep_file_symlink_p(repv file);
extern repv rep_file_owner_p(repv file);
extern repv rep_file_gid_p(repv file);
extern repv rep_file_uid_p(repv file);
extern repv rep_file_nlinks(repv file);
extern repv rep_file_size(repv file);
extern repv rep_file_modes(repv file);
extern repv rep_set_file_modes(repv file, repv modes);
extern repv rep_file_modes_as_string(repv file);
extern repv rep_file_modtime(repv file);
extern repv rep_directory_files(repv dir_name);
extern repv rep_read_symlink (repv file);
extern repv rep_make_symlink (repv file, repv contents);
extern repv rep_getpwd(void);
extern repv rep_structure_file (repv in);

/* from unix_main.c */
extern repv rep_user_login_name(void);
extern repv rep_user_full_name(void);
extern repv rep_user_home_directory(repv user);
extern repv rep_system_name(void);
extern void rep_pre_sys_os_init(void);
extern void rep_sys_os_init(void);
extern void rep_sys_os_kill(void);

/* from unix_processes.c */
extern repv rep_system(char *command);
extern void rep_proc_init(void);
extern void rep_proc_kill(void);

#ifndef HAVE_REALPATH
/* from realpath.c */
extern char *realpath (const char *name, char *resolved);
#endif

#endif /* rep_HAVE_UNIX */

#endif /* REPINT_SUBRS_H */
