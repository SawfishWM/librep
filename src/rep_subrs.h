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

#ifndef inline
#define inline
#endif

/* from continuations.c */
extern int rep_thread_lock;
extern rep_bool rep_pending_thread_yield;
extern repv rep_call_with_barrier (repv (*callback)(repv), repv arg,
				   rep_bool closed, void (*in)(void *),
				   void (*out)(void *), void *data);
extern repv Fcall_cc (repv thunk);
extern repv Fcontinuation_callable_p (repv cont);
extern repv Fcall_with_object (repv arg, repv thunk);
extern repv Fcall_with_dynamic_root (repv thunk);
extern repv Fcall_with_barrier (repv thunk, repv closed, repv in, repv out);
extern repv Fmake_thread (repv thunk, repv name);
extern repv Fthread_yield (void);
extern repv Fthread_delete (repv thread);
extern repv Fthread_suspend (repv thread, repv msecs);
extern repv Fthread_wake (repv thread);
extern repv Ftheadp (repv arg);
extern repv Fthread_suspended_p (repv thread);
extern repv Fthread_exited_p (repv thread);
extern repv Fcurrent_thread (repv depth);
extern repv Fall_threads (repv depth);
extern repv Fthread_forbid (void);
extern repv Fthread_permit (void);
extern repv Fthread_name (repv th);
extern unsigned long rep_max_sleep_for (void);

/* from datums.c */
extern repv Fmake_datum (repv, repv);
extern repv Fdefine_datum_printer (repv, repv);
extern repv Fdatum_ref (repv, repv);
extern repv Fdatum_set (repv, repv, repv);
extern repv Fhas_type_p (repv, repv);

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
extern repv Ffile_executable_p(repv file);
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
extern repv rep_file_fdopen (int fd, char *mode);

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
extern repv Fexpand_last_match(repv template_);
extern repv Fmatch_start(repv exp);
extern repv Fmatch_end(repv exp);
extern repv Fquote_regexp(repv str);
extern repv Fregexp_cache_control(repv limit);
extern void rep_regerror(char *err);

/* from fluids.c */
extern repv Fmake_fluid (repv);
extern repv Ffluid_ref (repv);
extern repv Ffluid_set (repv, repv);
extern repv Fwith_fluids (repv, repv, repv);

/* from lisp.c */
extern repv rep_load_autoload(repv);
extern repv rep_funcall(repv fun, repv arglist, rep_bool eval_args);
extern repv rep_apply (repv, repv);
extern repv rep_call_lisp0(repv);
extern repv rep_call_lisp1(repv, repv);
extern repv rep_call_lisp2(repv, repv, repv);
extern repv rep_call_lisp3(repv, repv, repv, repv);
extern repv rep_call_lisp4(repv, repv, repv, repv, repv);
extern repv rep_call_lispn (repv fun, int argc, repv *argv);
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
extern repv rep_env, rep_fenv, rep_special_bindings;
extern struct rep_Call *rep_call_stack;
extern int rep_lisp_depth, rep_max_lisp_depth;
extern int rep_test_int_counter;
extern int rep_test_int_period;
extern void (*rep_test_int_fun)(void);
extern repv Ffuncall(repv);
extern repv Feval(repv);
extern repv Fprogn(repv, repv);
extern repv Fbreak(void);
extern repv Fstep(repv);
extern repv Fsignal(repv error, repv data);
extern repv Fbacktrace(repv strm);
extern repv Vmax_lisp_depth(repv val);
extern int rep_list_length(repv);
extern rep_bool rep_assign_args (repv list, int required, int total, ...);

/* from lispcmds.c */
extern repv Qor, Qand;
extern repv Qload_path, Qafter_load_alist, Qlisp_lib_directory;
extern repv Qdl_load_path, Qdl_load_reloc_now, Qprovide, Qfeatures;
extern repv Qsite_lisp_directory, Qdocumentation_file, Qdocumentation_files;
extern repv Fquote(repv, repv);
extern repv Fcar(repv);
extern repv Fcdr(repv);
extern repv Fmake_list(repv, repv);
extern repv Fnconc(repv args);
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
extern repv Fmemql(repv, repv);
extern repv Fdelete(repv, repv);
extern repv Fdelq(repv, repv);
extern repv Fdelete_if(repv, repv);
extern repv Fdelete_if_not(repv, repv);
extern repv Fmake_vector(repv, repv);
extern repv Farrayp(repv);
extern repv Faset(repv, repv, repv);
extern repv Faref(repv, repv);
extern repv Fmake_string(repv, repv);
extern repv Fsubstring(repv string, repv start, repv end);
extern repv Flength(repv);
extern repv Fcopy_sequence(repv);
extern repv Felt(repv, repv);
extern repv Fcond(repv, repv);
extern repv Fapply(repv);
extern repv Fload(repv file, repv noerr_p, repv nopath_p,
		  repv nosuf_p, repv in_env);
extern repv Fequal(repv, repv);
extern repv Feq(repv, repv);
extern repv Fstring_head_eq(repv, repv);
extern repv Fnull(repv);
extern repv Fatom(repv);
extern repv Fconsp(repv);
extern repv Flistp(repv);
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
extern repv Fthrow(repv, repv);

/* from lispmach.c */
extern repv Qbytecode_error;
extern repv Fvalidate_byte_code(repv bc_major, repv bc_minor);
extern repv Fmake_byte_code_subr(repv args);

/* from macros.c */
extern repv Fmacroexpand(repv, repv);

/* from main.c */
extern void rep_init(char *prog_name, int *argc, char ***argv,
		     void (*sys_symbols)(void), void (*sys_usage)(void));
extern void rep_init_from_dump(char *prog_name, int *argc, char ***argv,
			       void (*sys_symbols)(void),
			       void (*sys_usage)(void),
			       char *dump_file);
extern repv rep_load_environment (repv file);
extern void rep_kill(void);
extern rep_bool rep_get_option (char *option, repv *argp);
extern rep_bool rep_on_idle(long since_last_event);
extern rep_bool rep_handle_input_exception(repv *result_p);
extern int rep_top_level_exit (void);
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
extern repv rep_top_level_recursive_edit (void);
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
extern void (*rep_message_fun)(enum rep_message fn, ...);

/* from misc.c */
extern char *rep_str_dupn(const char *old, int len);
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

/* from numbers.c */
extern repv rep_make_long_uint (unsigned long in);
extern repv rep_make_long_int (long in);
extern unsigned long rep_get_long_uint (repv in);
extern long rep_get_long_int (repv in);
extern repv rep_make_longlong_int (rep_long_long in);
extern rep_long_long rep_get_longlong_int (repv in);
extern repv rep_make_float (double in, rep_bool force);
extern double rep_get_float (repv in);
extern int rep_compare_numbers (repv n1, repv n2);
extern char *rep_print_number_to_string (repv obj, int radix, int prec);
extern repv rep_number_foldl (repv args, repv (*op)(repv, repv));
extern repv rep_integer_foldl (repv args, repv (*op)(repv, repv));
extern repv rep_foldl (repv args, repv (*op)(repv, repv));
extern repv rep_number_add (repv x, repv y);
extern repv rep_number_neg (repv x);
extern repv rep_number_sub (repv x, repv y);
extern repv rep_number_mul (repv x, repv y);
extern repv rep_number_div (repv x, repv y);
extern repv rep_number_lognot (repv x);
extern repv rep_number_logior (repv x, repv y);
extern repv rep_number_logxor (repv x, repv y);
extern repv rep_number_logand (repv x, repv y);
extern repv rep_number_max (repv x, repv y);
extern repv rep_number_min (repv x, repv y);
extern repv rep_integer_gcd (repv x, repv y);
extern repv Feql(repv arg1, repv arg2);
extern repv Fremainder(repv n1, repv n2);
extern repv Fmod(repv n1, repv n2);
extern repv Fquotient(repv n1, repv n2);
extern repv Flognot(repv);
extern repv Fnot(repv);
extern repv Fplus1(repv);
extern repv Fsub1(repv);
extern repv Fash(repv, repv);
extern repv Ffloor (repv);
extern repv Fceiling (repv);
extern repv Ftruncate (repv);
extern repv Fround (repv);
extern repv Fexp (repv);
extern repv Flog (repv);
extern repv Fsin (repv);
extern repv Fcos (repv);
extern repv Ftan (repv);
extern repv Fasin (repv);
extern repv Facos (repv);
extern repv Fatan (repv, repv);
extern repv Fsqrt (repv);
extern repv Fexpt (repv, repv);
extern repv Fzerop(repv);
extern repv Fnumberp(repv);
extern repv Fintegerp(repv);
extern repv Frationalp(repv);
extern repv Frealp(repv);
extern repv Fexactp(repv);
extern repv Finexactp(repv);
extern repv Fexact_to_inexact(repv);
extern repv Finexact_to_exact(repv);
extern repv Fnumerator(repv);
extern repv Fdenominator(repv);

/* from streams.c */
extern repv Qformat_hooks_alist;
extern int rep_stream_getc(repv);
extern int rep_stream_ungetc(repv, int);
extern int rep_stream_putc(repv, int);
extern int rep_stream_puts(repv, void *, int, rep_bool);
extern int rep_stream_read_esc(repv, int *);
extern repv Fwrite(repv stream, repv data, repv len);
extern repv Fread_char(repv stream);
extern repv Fpeek_char(repv stream);
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
extern repv Finput_stream_p(repv arg);
extern repv Foutput_stream_p(repv arg);

/* from symbols.c */
extern repv rep_undefined_value;
extern repv (*rep_deref_local_symbol_fun)(repv sym);
extern repv (*rep_set_local_symbol_fun)(repv sym, repv val);
extern void rep_intern_static(repv *, repv);
extern repv rep_call_with_closure (repv closure,
				   repv (*fun)(repv arg), repv arg);
extern repv rep_bind_symbol(repv, repv, repv);
extern int rep_unbind_symbols(repv);
extern repv rep_add_binding_to_env (repv env, repv sym, repv value);
extern repv rep_obarray;
extern repv Qt;
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
extern repv Fdefvar(repv, repv);
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
extern repv Fsetq(repv, repv);
extern repv Fmakunbound(repv);
extern repv Fget(repv, repv);
extern repv Fput(repv, repv, repv);
extern repv Fapropos(repv, repv, repv);
extern repv Fmake_variable_special (repv sym);
extern repv Fspecial_variable_p(repv sym);
extern repv Ftrace(repv sym);
extern repv Funtrace(repv sym);
extern repv Vobarray(repv val);
extern repv Fmake_keyword (repv in);
extern repv Fkeywordp (repv arg);

/* from structures.c */
extern repv rep_structure;
extern repv Fmake_binding_immutable (repv);
extern repv Fbinding_immutable_p (repv, repv);
extern repv Fexport_bindings (repv list);
extern repv Ffeaturep(repv);
extern repv Fprovide(repv);
extern repv Frequire(repv);
extern repv rep_push_structure_name (repv name);
extern repv rep_push_structure (const char *name);
extern repv rep_pop_structure (repv old);
extern void rep_alias_structure (const char *name);
extern repv rep_bootstrap_structure (const char *s);
extern repv rep_add_subr(rep_xsubr *, rep_bool);
extern void rep_structure_exports_all (repv s, rep_bool status);
extern void rep_structure_set_binds (repv s, rep_bool status);

/* from tuples.c */
extern repv rep_make_tuple (repv car, repv a, repv b);
extern void rep_mark_tuple (repv t);

/* from values.c */
extern repv Qafter_gc_hook;
extern rep_cons *rep_dumped_cons_start, *rep_dumped_cons_end;
extern rep_symbol *rep_dumped_symbols_start, *rep_dumped_symbols_end;
extern repv rep_dumped_non_constants;
extern int rep_guardian_type;
extern repv rep_box_pointer (void *p);
void *rep_unbox_pointer (repv v);
extern void rep_register_type(unsigned int code, char *name,
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
extern unsigned int rep_register_new_type(char *name,
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
extern rep_type *rep_get_data_type(unsigned int code);
extern int rep_value_cmp(repv, repv);
extern void rep_princ_val(repv, repv);
extern void rep_print_val(repv, repv);
extern repv rep_null_string(void);
extern repv rep_box_string (char *ptr, long len);
extern repv rep_make_string(long);
extern repv rep_string_dupn(const char *, long);
extern repv rep_string_dup(const char *);
extern repv rep_concat2(char *, char *);
extern repv rep_concat3(char *, char *, char *);
extern repv rep_concat4(char *s1, char *s2, char *s3, char *s4);
extern rep_bool rep_set_string_len(repv, long);
extern repv rep_list_1(repv);
extern repv rep_list_2(repv, repv);
extern repv rep_list_3(repv, repv, repv);
extern repv rep_list_4(repv, repv, repv, repv);
extern repv rep_list_5(repv, repv, repv, repv, repv);
extern repv rep_make_vector(int);
extern repv Fmake_primitive_guardian (void);
extern repv Fprimitive_guardian_push (repv g, repv obj);
extern repv Fprimitive_guardian_pop (repv g);
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
extern void *rep_find_dl_symbol (repv feature, char *symbol);

/* from unix_files.c */
extern repv rep_lookup_errno(void);
extern unsigned long rep_file_length(repv file);

/* from unix_main.c */
extern unsigned long rep_time(void);
extern rep_long_long rep_utime (void);
extern unsigned long rep_getpid (void);
extern void (*rep_register_input_fd_fun)(int fd, void (*callback)(int fd));
extern void (*rep_deregister_input_fd_fun)(int fd);
extern void rep_add_event_loop_callback (rep_bool (*callback)(void));
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
extern repv rep_sit_for(unsigned long timeout_msecs);
extern repv rep_accept_input_for_callbacks (unsigned long timeout_msecs,
					    int ncallbacks,
					    void (**callbacks)(int));
extern repv rep_accept_input_for_fds (unsigned long timeout_msecs,
				      int nfds, int *fds);
extern repv rep_accept_input(unsigned long timeout_msecs, void (*callback)(int));
extern rep_bool rep_poll_input(int fd);

#ifdef DEBUG_SYS_ALLOC
extern void *rep_alloc(unsigned int length);
extern void *rep_realloc(void *ptr, unsigned int length);
extern void rep_free(void *ptr);
extern void rep_print_allocations(void);
#else
# include <stdlib.h>
# define rep_alloc(n) malloc(n)
# define rep_realloc(p,n) realloc(p,n)
# define rep_free(p) free(p)
#endif

extern void (*rep_redisplay_fun)(void);
extern long (*rep_wait_for_input_fun)(void *inputs, unsigned long timeout_msecs);
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
void rep_register_process_input_handler (void (*handler)(int));

#endif /* rep_HAVE_UNIX */

/* in plugins */
extern repv rep_dl_init (void);
extern void rep_dl_kill (void);

#endif /* REP_SUBRS_H */
