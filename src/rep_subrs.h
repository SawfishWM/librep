/* rep_subrs.h -- mostly LISP subr declarations
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

#ifndef REP_SUBRS_H
#define REP_SUBRS_H

#include <stdarg.h>

/* from debug-buffer.c */
extern void *rep_db_alloc(char *name, int size);
extern void rep_db_free(void *db);
extern void rep_db_vprintf(void *_db, char *fmt, va_list args);
extern void rep_db_printf(void *db, char *fmt, ...);
extern void rep_db_print_backtrace(void *_db, char *fun);
extern void *rep_db_return_address(void);
extern void rep_db_spew(void *_db);
extern void rep_db_spew_all(void);
extern void rep_db_kill(void);

/* from files.c */
extern repv Qdefault_directory;
extern repv Qstart, Qend;
extern repv Qread, Qwrite, Qappend;
extern repv rep_fh_env;
extern int rep_file_type;
extern int rep_op_write_buffer_contents;
extern int rep_op_read_file_contents;
extern int rep_op_insert_file_contents;
extern repv rep_signal_file_error(repv cdr);
extern repv rep_unbound_file_error(repv file);
extern repv rep_get_file_handler(repv file_name, int op);
extern repv rep_call_file_handler(repv handler, int op,
				   repv sym, int nargs, ...);
extern repv rep_get_handler_from_file_or_name(repv *filep, int op);
extern repv rep_expand_and_get_handler(repv *file_namep, int op);
extern repv rep_localise_and_get_handler(repv *file_namep, int op);
extern rep_bool rep_file_newer_than(repv name1, repv name2);
extern repv Ffile_name_absolute_p(repv file);
extern repv Fexpand_file_name(repv, repv);
extern repv Flocal_file_name(repv);
extern repv Fcanonical_file_name(repv);
extern repv Ffile_name_nondirectory(repv);
extern repv Ffile_name_directory(repv);
extern repv Ffile_name_as_directory(repv);
extern repv Fdirectory_file_name(repv);
extern repv Ffilep(repv arg);
extern repv Ffile_binding(repv file);
extern repv Ffile_bound_stream(repv file);
extern repv Ffile_handler_data(repv);
extern repv Fset_file_handler_data(repv, repv);
extern repv Fopen_file(repv, repv);
extern repv Fmake_file_from_stream(repv, repv, repv);
extern repv Fclose_file(repv);
extern repv Fflush_file(repv file);
extern repv Fseek_file(repv file, repv offset, repv where);
extern repv Fdelete_file(repv);
extern repv Frename_file(repv, repv);
extern repv Fmake_directory(repv);
extern repv Fdelete_directory(repv);
extern repv Fcopy_file(repv, repv);
extern repv Ffile_readable_p(repv file);
extern repv Ffile_writable_p(repv file);
extern repv Ffile_exists_p(repv file);
extern repv Ffile_regular_p(repv file);
extern repv Ffile_directory_p(repv file);
extern repv Ffile_symlink_p(repv file);
extern repv Ffile_owner_p(repv file);
extern repv Ffile_nlinks(repv file);
extern repv Ffile_size(repv file);
extern repv Ffile_modes(repv file);
extern repv Fset_file_modes(repv file, repv modes);
extern repv Ffile_modes_as_string(repv file);
extern repv Ffile_modtime(repv file);
extern repv Fdirectory_files(repv dir);
extern repv Fread_symlink(repv file);
extern repv Fmake_symlink(repv file, repv contents);
extern repv Fstdin_file(void);
extern repv Fstdout_file(void);
extern repv Fstderr_file(void);
extern repv Fmake_temp_name(void);

/* from find.c */
extern rep_regexp *rep_compile_regexp(repv re);
extern void rep_push_regexp_data(struct rep_saved_regexp_data *sd);
extern void rep_pop_regexp_data(void);
extern void rep_update_last_match(repv data, rep_regexp *prog);
extern void rep_set_string_match(repv obj, repv start, repv end);
extern void (*rep_regsub_fun)(int, rep_regsubs *, char *, char *, void *);
extern int (*rep_regsublen_fun)(int, rep_regsubs *, char *, void *);
extern repv Qregexp_error;
extern repv Fstring_match(repv re, repv str, repv start, repv nocasep);
extern repv Fstring_looking_at(repv re, repv string,
				repv start, repv nocasep);
extern repv Fexpand_last_match(repv template);
extern repv Fmatch_start(repv exp);
extern repv Fmatch_end(repv exp);
extern repv Fquote_regexp(repv str);
extern repv Fregexp_cache_control(repv limit);
extern void rep_regerror(char *err);

/* from lisp.c */
extern repv rep_load_autoload(repv);
extern repv rep_funcall(repv fun, repv arglist, rep_bool eval_args);
extern repv rep_call_lisp0(repv);
extern repv rep_call_lisp1(repv, repv);
extern repv rep_call_lisp2(repv, repv, repv);
extern repv rep_call_lisp3(repv, repv, repv, repv);
extern repv rep_call_lisp4(repv, repv, repv, repv, repv);
extern repv rep_handle_var_int(repv, int *);
extern repv rep_handle_var_long_int(repv, long *);
extern void rep_handle_error(repv, repv);
extern repv rep_signal_arg_error(repv, int);
extern repv rep_signal_missing_arg(int argnum);
extern repv rep_mem_error(void);
extern repv Qdebug_entry, Qdebug_exit, Qdebug_error_entry;
extern repv Qquote, Qlambda, Qmacro, Qautoload, Qfunction;
extern repv Qstandard_input, Qstandard_output;
extern repv Qamp_optional, Qamp_rest, Qamp_aux;
extern volatile repv rep_throw_value;
extern repv rep_int_cell, rep_term_cell;
extern repv Qerror, Qerror_message, Qinvalid_function;
extern repv Qvoid_value, Qbad_arg, Qinvalid_read_syntax;
extern repv Qend_of_stream, Qinvalid_lambda_list, Qmissing_arg;
extern repv Qinvalid_macro, Qinvalid_autoload, Qno_catcher;
extern repv Qfile_error;
extern repv Qinvalid_stream, Qsetting_constant, Qprocess_error;
extern repv Qno_memory, Quser_interrupt, Qarith_error;
extern repv Qterm_interrupt;
extern repv Qstack_error;
extern repv Qprint_escape, Qprint_length, Qprint_level, Qnewlines;
extern repv rep_env, rep_fenv, rep_special_env;
extern struct rep_Call *rep_call_stack;
extern int rep_test_int_counter;
extern int rep_test_int_period;
extern void (*rep_test_int_fun)(void);
extern repv Ffuncall(repv);
extern repv Feval(repv);
extern repv Fprogn(repv);
extern repv Fbreak(void);
extern repv Fstep(repv);
extern repv Fmacroexpand(repv, repv);
extern repv Fsignal(repv error, repv data);
extern repv Fcondition_case(repv args);
extern repv Fbacktrace(repv strm);
extern repv Vmax_lisp_depth(repv val);

/* from lispcmds.c */
extern repv Qor, Qand;
extern repv Qload_path, Qafter_load_alist, Qlisp_lib_directory;
extern repv Qdl_load_path, Qdl_load_reloc_now, Qprovide;
extern repv Qsite_lisp_directory, Qdocumentation_file, Qdocumentation_files;
extern repv Fquote(repv);
extern repv Fdefmacro(repv);
extern repv Fdefun(repv);
extern repv Fdefvar(repv);
extern repv Fdefconst(repv);
extern repv Fcar(repv);
extern repv Fcdr(repv);
extern repv Flist(repv);
extern repv Flist_star(repv);
extern repv Fmake_list(repv, repv);
extern repv Fappend(repv);
extern repv Fnconc(repv);
extern repv Frplaca(repv, repv);
extern repv Frplacd(repv, repv);
extern repv Freverse(repv);
extern repv Fnreverse(repv);
extern repv Fassoc(repv, repv);
extern repv Fassq(repv, repv);
extern repv Frassoc(repv, repv);
extern repv Frassq(repv, repv);
extern repv Fnth(repv, repv);
extern repv Fnthcdr(repv index, repv list);
extern repv Flast(repv);
extern repv Fmapcar(repv, repv);
extern repv Fmapc(repv, repv);
extern repv Ffilter(repv pred, repv list);
extern repv Fmember(repv, repv);
extern repv Fmemq(repv, repv);
extern repv Fdelete(repv, repv);
extern repv Fdelq(repv, repv);
extern repv Fdelete_if(repv, repv);
extern repv Fdelete_if_not(repv, repv);
extern repv Fvector(repv);
extern repv Fmake_vector(repv, repv);
extern repv Farrayp(repv);
extern repv Faset(repv, repv, repv);
extern repv Faref(repv, repv);
extern repv Fmake_string(repv, repv);
extern repv Fsubstring(repv string, repv start, repv end);
extern repv Fconcat(repv);
extern repv Flength(repv);
extern repv Fcopy_sequence(repv);
extern repv Felt(repv, repv);
extern repv Fprog1(repv);
extern repv Fprog2(repv);
extern repv Fwhile(repv);
extern repv Fcond(repv);
extern repv Fif(repv args);
extern repv Fand(repv);
extern repv For(repv);
extern repv Fapply(repv);
extern repv Fload(repv file, repv noerr_p, repv nopath_p,
		  repv nosuf_p, repv in_env);
extern repv Fplus(repv);
extern repv Fminus(repv);
extern repv Fproduct(repv);
extern repv Fdivide(repv);
extern repv Fremainder(repv n1, repv n2);
extern repv Fmod(repv n1, repv n2);
extern repv Flognot(repv);
extern repv Fnot(repv);
extern repv Flogior(repv);
extern repv Flogxor(repv);
extern repv Flogand(repv);
extern repv Fequal(repv, repv);
extern repv Feq(repv, repv);
extern repv Feql(repv arg1, repv arg2);
extern repv Fstring_head_eq(repv, repv);
extern repv Fnum_eq(repv num1, repv num2);
extern repv Fnum_noteq(repv num1, repv num2);
extern repv Fgtthan(repv);
extern repv Fgethan(repv);
extern repv Fltthan(repv);
extern repv Flethan(repv);
extern repv Fmax(repv);
extern repv Fmin(repv);
extern repv Fplus1(repv);
extern repv Fsub1(repv);
extern repv Flsh(repv, repv);
extern repv Fash(repv, repv);
extern repv Fzerop(repv);
extern repv Fnull(repv);
extern repv Fatom(repv);
extern repv Fconsp(repv);
extern repv Flistp(repv);
extern repv Fnumberp(repv);
extern repv Fintegerp(repv);
extern repv Fstringp(repv);
extern repv Fvectorp(repv);
extern repv Fbytecodep(repv);
extern repv Ffunctionp(repv);
extern repv Fmacrop(repv);
extern repv Fspecial_form_p(repv);
extern repv Fsubrp(repv arg);
extern repv Fsequencep(repv arg);
extern repv FSdocumentation(repv subr, repv useVar);
extern repv FSname(repv subr, repv useVar);
extern repv Fcall_hook(repv hook, repv arg_list, repv type);
extern repv Fcatch(repv);
extern repv Fthrow(repv, repv);
extern repv Funwind_protect(repv);
extern repv Ffeaturep(repv);
extern repv Fprovide(repv);
extern repv Frequire(repv, repv);

/* from lispmach.c */
extern void rep_unbind_object(repv item);
extern repv rep_bind_object(repv obj);
extern repv Qbytecode_error, Qjade_byte_code;
extern repv Fjade_byte_code(repv code, repv consts, repv stkreq);
extern repv Fvalidate_byte_code(repv bc_major, repv bc_minor);
extern repv Fmake_byte_code_subr(repv args);

/* from main.c */
extern void rep_init(char *prog_name, int *argc, char ***argv,
		     void (*sys_symbols)(void), void (*sys_usage)(void));
extern void rep_kill(void);
extern rep_bool rep_get_option (char *option, repv *argp);
extern rep_bool rep_on_idle(long since_last_event);
extern rep_bool rep_handle_input_exception(repv *result_p);
extern void *rep_common_db;
extern int rep_recurse_depth;
extern rep_bool (*rep_on_idle_fun)(int since_last);
extern repv (*rep_event_loop_fun)(void);
extern repv Qidle_hook;
extern void (*rep_on_termination_fun)(void);
extern repv Qexit, Qquit, Qtop_level, Qcommand_line_args;
extern repv Qbatch_mode, Qinterpreted_mode, Qprogram_name;
extern repv Qerror_mode, Qinterrupt_mode;
extern repv Frecursive_edit(void);
extern repv Frecursion_depth(void);
extern repv Fget_command_line_option (repv, repv);

/* from message.c */
enum rep_message {
    rep_messagen = 0,
    rep_message,
    rep_messagef,
    rep_save_message,
    rep_append_message,
    rep_reset_message,
    rep_restore_message,
    rep_redisplay_message
};
void (*rep_message_fun)(enum rep_message fn, ...);

/* from misc.c */
extern u_char *rep_str_dupn(const u_char *old, int len);
extern void (*rep_beep_fun)(void);
extern repv Qoperating_system, Qwindow_system, Qprocess_environment;
extern repv Qbuild_id_string;
extern repv Qupcase_table, Qdowncase_table, Qflatten_table;
extern repv Fbeep(void);
extern repv Fcomplete_string(repv existing, repv arg_list, repv fold);
extern repv Fcurrent_time(void);
extern repv Ffix_time(repv time);
extern repv Fcurrent_time_string(repv time, repv format);
extern repv Ftime_later_p(repv t1, repv t2);
extern repv Fsleep_for(repv secs, repv msecs);
extern repv Fsit_for(repv secs, repv msecs);
extern repv Fuser_login_name(void);
extern repv Fuser_full_name(repv arg);
extern repv Fuser_home_directory(repv user);
extern repv Fsystem_name(void);
extern repv Fmessage(repv string, repv now);
extern repv Frandom(repv arg);
extern repv Ftranslate_string(repv string, repv table);
extern repv Falpha_char_p(repv);
extern repv Fupper_case_p(repv);
extern repv Flower_case_p(repv);
extern repv Fdigit_char_p(repv);
extern repv Falphanumericp(repv);
extern repv Fspace_char_p(repv);
extern repv Fchar_upcase(repv);
extern repv Fchar_downcase(repv);

/* from streams.c */
extern repv Qformat_hooks_alist;
extern int rep_stream_getc(repv);
extern int rep_stream_ungetc(repv, int);
extern int rep_stream_putc(repv, int);
extern int rep_stream_puts(repv, void *, int, rep_bool);
extern int rep_stream_read_esc(repv, int *);
extern repv Fwrite(repv stream, repv data, repv len);
extern repv Fread_char(repv stream);
extern repv Fread_chars(repv stream, repv count);
extern repv Fread_line(repv stream);
extern repv Fcopy_stream(repv source, repv dest);
extern repv Fread(repv);
extern repv Fprint(repv, repv);
extern repv Fprin1(repv, repv);
extern repv Fprinc(repv, repv);
extern repv Fformat(repv);
extern repv Fmake_string_input_stream(repv string, repv start);
extern repv Fmake_string_output_stream(void);
extern repv Fget_output_stream_string(repv strm);
extern repv Fstreamp(repv arg);

/* from symbols.c */
extern repv (*rep_deref_local_symbol_fun)(repv sym);
extern repv (*rep_set_local_symbol_fun)(repv sym, repv val);
extern repv rep_add_subr(rep_xsubr *);
extern repv rep_add_const_num(repv, long);
extern void rep_intern_static(repv *, repv);
extern repv rep_bind_symbol(repv, repv, repv);
extern void rep_unbind_symbols(repv);
extern repv rep_obarray;
extern repv Qnil, Qt;
extern repv Qvariable_documentation, Qpermanent_local;
extern repv rep_void_value;
extern rep_bool rep_warn_shadowing;
extern repv Fmake_symbol(repv);
extern repv Fmake_obarray(repv);
extern repv Ffind_symbol(repv, repv);
extern repv Fintern_symbol(repv, repv);
extern repv Fintern(repv, repv);
extern repv Funintern(repv, repv);
extern repv Fmake_closure (repv function, repv name);
extern repv Fclosure_function (repv funarg);
extern repv Fset_closure_function (repv funarg, repv fun);
extern repv Fclosurep (repv arg);
extern repv Fsymbol_value(repv, repv);
extern repv Fset(repv, repv);
extern repv Fsetplist(repv, repv);
extern repv Fsymbol_name(repv);
extern repv Fdefault_value(repv, repv);
extern repv Fdefault_boundp(repv);
extern repv Fset_default(repv, repv);
extern repv Fboundp(repv);
extern repv Fsymbol_plist(repv);
extern repv Fgensym(void);
extern repv Fsymbolp(repv);
extern repv Fsetq(repv);
extern repv Fsetq_default(repv);
extern repv Fmakunbound(repv);
extern repv Flet(repv);
extern repv Fletstar(repv);
extern repv Fget(repv, repv);
extern repv Fput(repv, repv, repv);
extern repv Fapropos(repv, repv, repv);
extern repv Fset_const_variable(repv sym, repv stat);
extern repv Fconst_variable_p(repv sym);
extern repv Fspecial_variable_p(repv sym);
extern repv Ftrace(repv sym);
extern repv Funtrace(repv sym);
extern repv Vobarray(repv val);

/* from values.c */
extern void rep_register_type(u_int code, char *name,
			      int (*compare)(repv, repv),
			      void (*princ)(repv, repv),
			      void (*print)(repv, repv),
			      void (*sweep)(void),
			      void (*mark)(repv),
			      void (*mark_type)(void),
			      int (*getc)(repv), int (*ungetc)(repv, int),
			      int (*putc)(repv, int),
			      int (*puts)(repv, void *, int, rep_bool),
			      repv (*bind)(repv), void (*unbind)(repv));
extern u_int rep_register_new_type(char *name,
				   int (*compare)(repv, repv),
				   void (*princ)(repv, repv),
				   void (*print)(repv, repv),
				   void (*sweep)(void),
				   void (*mark)(repv),
				   void (*mark_type)(void),
				   int (*getc)(repv),
				   int (*ungetc)(repv, int),
				   int (*putc)(repv, int),
				   int (*puts)(repv, void *, int, rep_bool),
				   repv (*bind)(repv),
				   void (*unbind)(repv));
extern rep_type *rep_get_data_type(u_int code);
extern int rep_value_cmp(repv, repv);
extern void rep_princ_val(repv, repv);
extern void rep_print_val(repv, repv);
extern repv rep_null_string(void);
extern repv rep_make_string(long);
extern repv rep_string_dupn(const u_char *, long);
extern repv rep_string_dup(const u_char *);
extern repv rep_concat2(u_char *, u_char *);
extern repv rep_concat3(u_char *, u_char *, u_char *);
extern repv rep_concat4(u_char *s1, u_char *s2, u_char *s3, u_char *s4);
extern rep_bool rep_set_string_len(repv, long);
extern repv rep_list_1(repv);
extern repv rep_list_2(repv, repv);
extern repv rep_list_3(repv, repv, repv);
extern repv rep_list_4(repv, repv, repv, repv);
extern repv rep_list_5(repv, repv, repv, repv, repv);
extern repv rep_make_vector(int);
extern void rep_mark_static(repv *);
extern void rep_mark_value(repv);
extern repv Fcons(repv, repv);
extern rep_GC_root *rep_gc_root_stack;
extern rep_GC_n_roots *rep_gc_n_roots_stack;
extern repv Vgarbage_threshold(repv val);
extern repv Vidle_garbage_threshold(repv val);
extern repv Fgarbage_collect(repv noStats);
extern int rep_data_after_gc, rep_gc_threshold, rep_idle_gc_threshold;
extern rep_bool rep_in_gc;

#ifdef rep_HAVE_UNIX

/* from unix_dl.c */
extern rep_bool rep_find_c_symbol(void *, char **, void **);
extern void *rep_open_dl_library(repv file_name);
extern void rep_mark_dl_data(void);
extern void rep_kill_dl_libraries(void);
extern void *rep_find_dl_symbol (repv feature, char *symbol);

/* from unix_files.c */
extern repv rep_lookup_errno(void);
extern u_long rep_file_length(repv file);
extern repv rep_file_name_absolute_p(repv file);
extern repv rep_expand_file_name(repv file);
extern repv rep_canonical_file_name(repv file);
extern repv rep_file_name_nondirectory(repv file);
extern repv rep_file_name_directory(repv file);
extern repv rep_file_name_as_directory(repv file);
extern repv rep_directory_file_name(repv file);
extern repv rep_delete_file(repv file);
extern repv rep_rename_file(repv old, repv new);
extern repv rep_make_directory(repv dir);
extern repv rep_delete_directory(repv dir);
extern repv rep_copy_file(repv src, repv dst);
extern repv rep_file_readable_p(repv file);
extern repv rep_file_writable_p(repv file);
extern repv rep_file_exists_p(repv file);
extern repv rep_file_regular_p(repv file);
extern repv rep_file_directory_p(repv file);
extern repv rep_file_symlink_p(repv file);
extern repv rep_file_owner_p(repv file);
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

/* from unix_main.c */
extern u_long rep_time(void);
extern void (*rep_register_input_fd_fun)(int fd, void (*callback)(int fd));
extern void (*rep_deregister_input_fd_fun)(int fd);
extern void rep_sleep_for(long secs, long msecs);
extern void rep_register_input_fd(int fd, void (*callback)(int fd));
extern void rep_deregister_input_fd(int fd);
extern void rep_map_inputs (void (*fun)(int fd, void (*callback)(int)));
extern void rep_mark_input_pending(int fd);
extern void rep_unix_set_fd_nonblocking(int fd);
extern void rep_unix_set_fd_blocking(int fd);
extern void rep_unix_set_fd_cloexec(int fd);
extern void rep_sig_restart(int sig, rep_bool flag);
extern repv rep_event_loop(void);
extern repv rep_sit_for(u_long timeout_msecs);
extern repv rep_accept_input(u_long timeout_msecs, void *callback);
extern rep_bool rep_poll_input(int fd);

#ifdef DEBUG_SYS_ALLOC
extern void *rep_alloc(u_int length);
extern void *rep_realloc(void *ptr, u_int length);
extern void rep_free(void *ptr);
extern void rep_print_allocations(void);
#else
# include <stdlib.h>
# define rep_alloc(n) malloc(n)
# define rep_realloc(p,n) realloc(p,n)
# define rep_free(p) free(p)
#endif

extern void (*rep_redisplay_fun)(void);
extern int rep_input_timeout_secs;
extern repv Funix_print_allocations(void);

/* from unix_processes.c */
extern repv Qpipe, Qpty;
extern void (*rep_sigchld_fun) (void);
extern rep_bool rep_proc_periodically(void);
extern repv Fmake_process(repv stream, repv fun, repv dir,
			   repv prog, repv args);
extern repv Fstart_process(repv arg_list);
extern repv Fcall_process(repv arg_list);
extern repv Finterrupt_process(repv proc, repv grp);
extern repv Fkill_process(repv proc, repv grp);
extern repv Fstop_process(repv proc, repv grp);
extern repv Fcontinue_process(repv proc, repv grp);
extern repv Fprocess_exit_status(repv proc);
extern repv Fprocess_exit_value(repv proc);
extern repv Fprocess_id(repv proc);
extern repv Fprocess_running_p(repv proc);
extern repv Fprocess_stopped_p(repv proc);
extern repv Fprocess_in_use_p(repv proc);
extern repv Fprocessp(repv arg);
extern repv Fprocess_prog(repv proc);
extern repv Fset_process_prog(repv proc, repv prog);
extern repv Fprocess_args(repv proc);
extern repv Fset_process_args(repv proc, repv args);
extern repv Fprocess_output_stream(repv proc);
extern repv Fset_process_output_stream(repv proc, repv stream);
extern repv Fprocess_error_stream(repv proc);
extern repv Fset_process_error_stream(repv proc, repv stream);
extern repv Fprocess_function(repv proc);
extern repv Fset_process_function(repv proc, repv fn);
extern repv Fprocess_dir(repv proc);
extern repv Fset_process_dir(repv proc, repv dir);
extern repv Fprocess_connection_type(repv proc);
extern repv Fset_process_connection_type(repv proc, repv type);
extern repv Factive_processes(void);
extern repv Faccept_process_output(repv secs, repv msecs);

#endif /* rep_HAVE_UNIX */

#endif /* REP_SUBRS_H */
