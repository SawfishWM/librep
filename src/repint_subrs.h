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
extern void rep_datums_init (void);

/* from files.c */
extern repv rep_get_fh_env (void);
extern void rep_files_init(void);
extern void rep_files_kill(void);

/* from find.c */
extern struct rep_saved_regexp_data *rep_saved_matches;
extern void rep_mark_regexp_data(void);
extern void rep_find_init(void);
extern void rep_find_kill(void);

/* from fluids.c */
extern void rep_fluids_init (void);

/* from lisp.c */
extern repv rep_scm_t, rep_scm_f;
extern repv rep_readl(repv, int *);
extern repv rep_bind_lambda_list_1 (repv lambdaList, repv *args, int nargs);
extern repv rep_bind_lambda_list(repv lambdaList, repv argList,
				 rep_bool eval_args, rep_bool eval_in_env);
extern repv rep_eval_lambda(repv, repv, rep_bool, rep_bool);
extern void rep_lisp_prin(repv, repv);
extern void rep_string_princ(repv, repv);
extern void rep_string_print(repv, repv);
extern int rep_list_length(repv);
extern repv rep_copy_list(repv);
extern rep_bool rep_compare_error(repv error, repv handler);
extern void rep_lisp_init(void);
extern rep_bool rep_single_step_flag;

/* from lispcmds.c */
extern void rep_lispcmds_init(void);

/* from lispmach.c */
extern void rep_lispmach_init(void);
extern void rep_lispmach_kill(void);

/* from main.c */
extern char *rep_stack_bottom;

/* from macros.c */
extern void rep_macros_before_gc (void);
extern void rep_macros_clear_history (void);
extern void rep_macros_init (void);

/* from misc.c */
#ifndef HAVE_STPCPY
extern char *stpcpy(char *, const char *);
#endif
extern void rep_misc_init(void);

/* from numbers.c */
extern repv rep_parse_number (char *buf, u_int len, u_int radix,
			      int sign, u_int type);
extern void rep_numbers_init (void);

/* from regsub.c */
extern void rep_default_regsub(int, rep_regsubs *, char *, char *, void *);
extern int rep_default_regsublen(int, rep_regsubs *, char *, void *);

/* from streams.c */
extern void rep_streams_init(void);

/* from structures.c */
extern repv rep_default_structure, rep_specials_structure;
extern repv F_make_structure (repv, repv, repv, repv);
extern repv F_structure_ref (repv, repv);
extern repv F_structure_set (repv, repv, repv);
extern repv F_structure_bound_p (repv, repv);
extern repv F_external_structure_ref (repv, repv);
extern repv rep_get_initial_special_value (repv sym);
extern void rep_pre_structures_init (void);
extern void rep_structures_init (void);

/* from symbols.c */
extern int rep_pre_symbols_init(void);
extern void rep_symbols_init(void);
extern void rep_symbols_kill(void);
extern int rep_allocated_symbols, rep_used_symbols;
extern int rep_allocated_funargs, rep_used_funargs;

/* from values.c */
extern int rep_type_cmp(repv, repv);
extern int rep_ptr_cmp(repv, repv);
extern void rep_cons_free(repv);
extern void rep_pre_values_init (void);
extern void rep_values_init(void);
extern void rep_values_kill (void);
extern void rep_dumped_init(char *file);

#ifdef rep_HAVE_UNIX

/* from unix_dl.c */
extern repv rep_open_dl_library(repv file_name);
extern void rep_mark_dl_data(void);
extern void rep_kill_dl_libraries(void);

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
