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

/* from files.c */
extern void rep_files_init(void);
extern void rep_files_kill(void);

/* from find.c */
extern void rep_mark_regexp_data(void);
extern void rep_find_init(void);
extern void rep_find_kill(void);

/* from lisp.c */
extern repv rep_readl(repv, int *);
extern repv rep_bind_lambda_list(repv lambdaList, repv argList,
				  rep_bool eval_args);
extern repv rep_eval_lambda(repv, repv, rep_bool eval_args);
extern void rep_lisp_prin(repv, repv);
extern void rep_string_princ(repv, repv);
extern void rep_string_print(repv, repv);
extern int rep_list_length(repv);
extern repv rep_copy_list(repv);
extern rep_bool rep_compare_error(repv error, repv handler);
extern void rep_lisp_init(void);
extern rep_bool rep_single_step_flag;
extern struct rep_Call *rep_call_stack;

/* from lispcmds.c */
extern void rep_lispcmds_init(void);

/* from lispmach.c */
extern void rep_lispmach_init(void);
extern void rep_lispmach_kill(void);

/* from misc.c */
#ifndef HAVE_STPCPY
extern char *stpcpy(char *, const char *);
#endif
extern void rep_misc_init(void);

/* from regsub.c */
extern void rep_default_regsub(int, rep_regsubs *, char *, char *, void *);
extern int rep_default_regsublen(int, rep_regsubs *, char *, void *);

/* from streams.c */
extern void rep_streams_init(void);

/* from symbols.c */
extern int rep_pre_symbols_init(void);
extern void rep_symbols_init(void);
extern void rep_symbols_kill(void);
extern int rep_allocated_symbols, rep_used_symbols;

/* from values.c */
extern int rep_type_cmp(repv, repv);
extern int rep_ptr_cmp(repv, repv);
extern void rep_cons_free(repv);
extern void rep_pre_values_init (void);
extern void rep_values_init(void);
extern void rep_values_kill (void);
extern void rep_dumped_init(void);

#ifdef rep_HAVE_UNIX

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
