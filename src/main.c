/* main.c -- Entry point for Jade
   Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
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

#include "repint.h"
#include <string.h>
#include <limits.h>

void *rep_common_db;

int rep_recurse_depth = -1;

rep_bool (*rep_on_idle_fun)(int since_last);
DEFSYM(idle_hook, "idle-hook"); /*
::doc:Vidle-hook::
This hook gets evaluated every second while the editor is idle. Don't depend
on how regularly this gets called, any events from the window-system will
delay it. Also, auto-saving files and garbage-collection take precedence
when there's idle time available. Use this hook sparingly, or for short
periods only!
::end:: */

/* Called when we get a termination signal. */
void (*rep_on_termination_fun)(void);

DEFSYM(exit, "exit");
DEFSYM(quit, "quit");
DEFSYM(top_level, "top-level");
DEFSYM(command_line_args, "command-line-args");
DEFSYM(batch_mode, "batch-mode");
DEFSYM(interpreted_mode, "interpreted-mode");

#ifndef INIT_SCRIPT
# define INIT_SCRIPT "init"
#endif
static u_char *init_script = INIT_SCRIPT;

static void rep_main_init(void);

static void
usage(char *prog_name, void (*sys_usage)(void))
{
    if (sys_usage != 0)
    {
	fprintf(stderr, "usage: %s [REP-OPTIONS] [SYSTEM-OPTIONS] [LISP-OPTIONS]\n", prog_name);
    }
    else
	fprintf(stderr, "usage: %s [REP-OPTIONS] [LISP-OPTIONS]\n", prog_name);

    fputs ("\nREP-OPTIONS include:\n"
	   "    --init FILE  use FILE instead of `init.jl' to boot from\n"
	   "    -v           print version/revision details\n"
	   "    --batch      don't open any windows; process args and exit\n"
	   "    --interp     don't load compiled Lisp files\n",
	   stderr);

    if (sys_usage != 0)
	(*sys_usage)();

    fputs ("\nand LISP-OPTIONS are:\n"
	   "    --no-rc      don't load .jaderc or site-init files\n"
	   "    -f FUNCTION  call the Lisp function FUNCTION\n"
	   "    -l FILE      load the file of Lisp forms called FILE\n"
	   "    -q           quit\n"
	   "    FILE         load FILE into a new buffer\n"
	   "\nNote that ordering _is_ important.\n",
	   stderr);
}

static int
get_main_options(char *prog_name, int *argc_p,
		 char ***argv_p, void (*sys_usage)(void))
{
    int argc = *argc_p;
    char **argv = *argv_p;
    repv head, *last;
    while(argc && (**argv == '-'))
    {
	if((argc >= 2) && !strcmp("--init", *argv))
	{
	    init_script = *(++argv);
	    argc--;
	}
	else if(!strcmp("-v", *argv))
	{
	    fputs("rep version " rep_VERSION "\n", stdout);
	    return rep_FALSE;
	}
	else if(!strcmp("--batch", *argv))
	    rep_SYM(Qbatch_mode)->value = Qt;
	else if(!strcmp("--interp", *argv))
	    rep_SYM(Qinterpreted_mode)->value = Qt;
	else if(!strcmp("-?", *argv) || !strcmp("--help", *argv))
	{
	    usage(prog_name, sys_usage);
	    return rep_FALSE;
	}
	else
	    break;
	argc--;
	argv++;
    }
    /* any command line args left now get made into a list of strings
       in symbol "command-line-args".  */
    head = Qnil;
    last = &head;
    while(argc > 0)
    {
	*last = Fcons(rep_string_dup(*argv), Qnil);
	last = &rep_CDR(*last);
	argc--;
	argv++;
    }
    rep_SYM(Qcommand_line_args)->value = head;
    *argc_p = argc;
    *argv_p = argv;
    return rep_TRUE;
}

void
rep_init(char *prog_name, int *argc, char ***argv,
	 void (*sys_symbols)(void), void (*sys_usage)(void))
{
    if(sizeof(rep_PTR_SIZED_INT) != sizeof(void *))
    {
	fputs("sizeof(rep_PTR_SIZED_INT) != sizeof(void *); aborting\n",
	      stderr);
	exit(10);
    }
    if(rep_PTR_SIZED_INT_BITS != sizeof(rep_PTR_SIZED_INT) * CHAR_BIT)
    {
	fputs("rep_PTR_SIZED_INT_BITS incorrectly defined; aborting\n",
	      stderr);
	exit(10);
    }

    if(!sys_memory_init())
	exit(10);

    rep_common_db = rep_db_alloc("common", 4096);

    rep_pre_values_init();
    rep_pre_sys_os_init();
    if(rep_pre_symbols_init())
    {
#ifdef rep_DUMPED
	/* Must initialise dumped out symbols before interning _any_
	   symbols by hand. */
	rep_dumped_init();
#endif
	rep_symbols_init();

	rep_values_init();
	rep_lisp_init();
	rep_lispcmds_init();
	rep_lispmach_init();
	rep_find_init();
	rep_main_init();
	rep_misc_init();
	rep_streams_init();
	rep_files_init();
	rep_sys_os_init();

	if (sys_symbols != 0)
	    (*sys_symbols)();

	if(get_main_options(prog_name, argc, argv, sys_usage))
	{
	    repv arg, res;
	    if((arg = rep_string_dup(init_script))
	       && (res = Fload(arg, Qnil, Qnil, Qnil)))
	    {
		return;
	    }
	}
    }

    if(rep_throw_value && rep_CAR(rep_throw_value) == Qerror)
    {
	/* If quitting due to an error, print the error cell if
	   at all possible. */
	repv stream = Fstderr_file();
	repv old_tv = rep_throw_value;
	rep_GC_root gc_old_tv;
	rep_PUSHGC(gc_old_tv, old_tv);
	rep_throw_value = rep_NULL;
	if(stream && rep_FILEP(stream))
	{
	    fputs("error--> ", stderr);
	    Fprin1(rep_CDR(old_tv), stream);
	    fputc('\n', stderr);
	}
	else
	    fputs("jade: error in initialisation\n", stderr);
	rep_throw_value = old_tv;
	rep_POPGC;
    }

    exit (10);
}

void
rep_kill(void)
{
#ifdef HAVE_DYNAMIC_LOADING
    rep_kill_dl_libraries();
#endif

    rep_sys_os_kill();
    rep_find_kill();
    rep_files_kill();
    rep_lispmach_kill();
    rep_db_kill();
    rep_symbols_kill();
    rep_values_kill();
    sys_memory_kill();
}

/* This function gets called when we have idle time available. The
   single argument is the number of seconds since we weren't idle.
   The first idle period after a non-idle period should pass zero.
   Returns rep_TRUE if the display should be refreshed. */
rep_bool
rep_on_idle(long since_last_event)
{
    static rep_bool called_hook;
    static int depth;
    rep_bool res = rep_FALSE;

    depth++;

    /* A timeout; do one of:
	* Remove messages in minibuffers
	* Print the current key-prefix
	* Auto-save a buffer
	* GC if enough data allocated
	* Run the `idle-hook' (only once per idle-period)  */

    if(since_last_event == 0)
	called_hook = rep_FALSE;

    if(rep_on_idle_fun != 0 && (*rep_on_idle_fun)(since_last_event))
	res = rep_TRUE;
    else if(rep_data_after_gc > rep_idle_gc_threshold)
	/* nothing was saved so try a GC */
	Fgarbage_collect(Qt);
    else if(!called_hook && depth == 1)
    {
	repv hook = Fsymbol_value(Qidle_hook, Qt);
	if(!rep_VOIDP(hook) && !rep_NILP(hook))
	{
	    Fcall_hook(hook, Qnil, Qnil);
	    res = rep_TRUE;
	}
	called_hook = rep_TRUE;
    }

    depth--;
    return res;
}

/* The input loop should call this function when rep_throw_value == rep_NULL.
   It returns rep_TRUE when the input loop should exit, returning whatever
   is stored in *RESULT-P. */
rep_bool
rep_handle_input_exception(repv *result_p)
{
    repv tv = rep_throw_value;
    repv car = rep_CAR(tv);
    rep_throw_value = rep_NULL;
    *result_p = rep_NULL;
    
    if(car == Qexit)
    {
	*result_p = rep_CDR(tv);
	if(rep_recurse_depth > 0)
	    return rep_TRUE;
    }
    else if((car == Qtop_level) && (rep_recurse_depth == 0))
	*result_p = rep_CDR(tv);
    else if(car == Qquit)
	return rep_TRUE;
    else if(car == Quser_interrupt)
	rep_handle_error(car, Qnil);
    else if(car == Qterm_interrupt)
    {
	if(rep_recurse_depth == 0 && rep_on_termination_fun != 0)
	    (*rep_on_termination_fun)();
	return rep_TRUE;
    }
    else if(car == Qerror)
	rep_handle_error(rep_CAR(rep_CDR(tv)), rep_CDR(rep_CDR(tv)));
    else if(rep_recurse_depth == 0)
	rep_handle_error(Qno_catcher, rep_LIST_1(car));
    else
    {
	rep_throw_value = tv;
	return rep_TRUE;
    }
    return rep_FALSE;
}

DEFUN_INT("recursive-edit", Frecursive_edit, Srecursive_edit, (void), rep_Subr0, "") /*
::doc:Srecursive-edit::
recursive-edit

Enter a new recursive-edit.
::end:: */
{
    return rep_event_loop();
}

DEFUN("recursion-depth", Frecursion_depth, Srecursion_depth, (void), rep_Subr0) /*
::doc:Srecursion-depth::
recursion-depth

Returns the number of recursive-edit's deep we are, zero signifies the
original level.
::end:: */
{
    return rep_MAKE_INT(rep_recurse_depth);
}

static void
rep_main_init(void)
{
    rep_ADD_SUBR_INT(Srecursive_edit);
    rep_ADD_SUBR(Srecursion_depth);
    rep_INTERN(quit);
    rep_INTERN(exit);
    rep_INTERN(top_level);
    rep_INTERN(command_line_args);
    rep_INTERN(idle_hook);
    rep_INTERN(batch_mode);
    rep_SYM(Qbatch_mode)->value = Qnil;
    rep_INTERN(interpreted_mode);
    rep_SYM(Qinterpreted_mode)->value = Qnil;
}
