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

#include "jade.h"
#include "jade_protos.h"
#include "revision.h"

#include <string.h>

_PR int main(int, char **);
_PR int inner_main(int, char **);
_PR bool on_idle(long since_last_event);
_PR bool handle_input_exception(VALUE *result_p);
_PR void main_init(void);

_PR StrMem main_strmem;
StrMem main_strmem;

_PR int recurse_depth;
int recurse_depth = -1;

_PR VALUE sym_exit, sym_quit, sym_top_level, sym_command_line_args;
DEFSYM(exit, "exit");
DEFSYM(quit, "quit");
DEFSYM(top_level, "top-level");
DEFSYM(command_line_args, "command-line-args");

#ifndef INIT_SCRIPT
# define INIT_SCRIPT "init"
#endif
static u_char *init_script = INIT_SCRIPT;

static void
usage(void)
{
    fputs("usage: jade [SYSTEM-OPTIONS] [STANDARD-OPTIONS] [LISP-OPTIONS]\n", stderr);
    sys_usage();
    fputs("STANDARD-OPTIONS are,\n"
	"    -rc FILE     use FILE instead of `init.jl' to boot from\n"
	"    -v           print version/revision details\n"
	"    -log-msgs    print all messages to standard-error as well\n"
	"and LISP-OPTIONS are,\n"
	"    -no-rc       don't load .jaderc or site-init files\n"
	"    -f FUNCTION  call the Lisp function FUNCTION\n"
	"    -l FILE      load the file of Lisp forms called FILE\n"
	"    -q           quit\n"
	"    FILE         load FILE into a new buffer\n"
	, stderr);
}

static int
get_main_options(int *argc_p, char ***argv_p)
{
    int argc = *argc_p;
    char **argv = *argv_p;
    VALUE head, *last;
    while(argc && (**argv == '-'))
    {
	if((argc >= 2) && !strcmp("-rc", *argv))
	{
	    init_script = *(++argv);
	    argc--;
	}
	else if(!strcmp("-v", *argv))
	{
	    fputs(VERSSTRING "\n", stdout);
	    return FALSE;
	}
	else if(!strcmp("-log-msgs", *argv))
	    log_messages = TRUE;
	else if(!strcmp("-?", *argv) || !strcmp("-help", *argv))
	{
	    usage();
	    return FALSE;
	}
	else
	    break;
	argc--;
	argv++;
    }
    /* any command line args left now get made into a list of strings
       in symbol "command-line-args".  */
    head = sym_nil;
    last = &head;
    while(argc > 0)
    {
	*last = cmd_cons(string_dup(*argv), sym_nil);
	last = &VCDR(*last);
	argc--;
	argv++;
    }
    VSYM(sym_command_line_args)->value = head;
    *argc_p = argc;
    *argv_p = argv;
    return TRUE;
}

int
main(int argc, char **argv)
{
    int rc;

    if(sizeof(PTR_SIZED_INT) != sizeof(void *))
    {
	fputs("jade: sizeof(PTR_SIZED_INT) != sizeof(void *); aborting\n",
	      stderr);
	return 100;
    }

    if(!sys_memory_init())
	return 10;
    sm_init(&main_strmem);

    pre_values_init();
    pre_sys_init();
    if(pre_symbols_init())
    {
#ifdef DUMPED
	/* Must initialise dumped out symbols before interning _any_
	   symbols by hand. */
	dumped_init();
#endif
	symbols_init();
	rc = sys_init(argc, argv);
	symbols_kill();
    }
    else
	rc = 5;
    values_kill();

    sm_kill(&main_strmem);
    sys_memory_kill();

    return rc;
}

/* This function is called from sys_init(), it completes the initialisation
   process and calls the top-level event loop.  sys_init() must advance
   ARGC and ARGV so they point to the next unused argument.  */
int
inner_main(int argc, char **argv)
{
    int rc = 5;

    values_init();
    lisp_init();
    lispcmds_init();
    lispmach_init();
    buffers_init();
    commands_init();
    edit_init();
    find_init();
    glyphs_init();
    keys_init();
    main_init();
    misc_init();
    movement_init();
    redisplay_init();
    streams_init();
    files_init();
    undo_init();
    views_init();
    windows_init();
    server_init();
    sys_misc_init();
    sys_windows_init();
#ifdef HAVE_SUBPROCESSES
    proc_init();
#endif
    if(get_main_options(&argc, &argv) && first_buffer())
    {
	VALUE arg, res;
	if((arg = string_dup(init_script))
	   && (res = cmd_load(arg, sym_nil, sym_nil, sym_nil)))
	{
	    rc = 0;
	    res = sys_event_loop();
	}
	else if(throw_value && VCAR(throw_value) == sym_quit)
	{
	    if(INTP(VCDR(throw_value)))
		rc = VINT(VCDR(throw_value));
	    else
		rc = 0;
	}
	else
	{
	    /* If quitting due to an error, print the error cell if
	       at all possible. */
	    VALUE stream;
	    VALUE old_tv = throw_value;
	    throw_value = LISP_NULL;
	    if(old_tv && VCAR(old_tv) == sym_error
	       && (stream = cmd_stderr_file())
	       && FILEP(stream))
	    {
		fputs("error--> ", stderr);
		cmd_prin1(VCDR(old_tv), stream);
		fputc('\n', stderr);
	    }
	    else
		fputs("jade: error in initialisation script\n", stderr);
	    throw_value = old_tv;
	}
#ifdef HAVE_SUBPROCESSES
        proc_kill();
#endif
	server_kill();
	windows_kill();
	views_kill();
	buffers_kill();
	find_kill();
	glyphs_kill();
	files_kill();
    }
    db_kill();
    return rc;
}

/* This function gets called when we have idle time available. The
   single argument is the number of seconds since we weren't idle.
   The first idle period after a non-idle period should pass zero.
   Returns TRUE if the display should be refreshed. */
bool
on_idle(long since_last_event)
{
    bool res = FALSE;

    /* A timeout; do one of:
	* Remove messages in minibuffers
	* Print the current key-prefix
	* Auto-save a buffer
	* GC if enough data allocated
	* Run the `idle-hook'  */

    if(remove_all_messages(TRUE)
       || print_event_prefix()
       || auto_save_buffers(FALSE))
	res = TRUE;
    else if(data_after_gc > idle_gc_threshold)
	/* nothing was saved so try a GC */
	cmd_garbage_collect(sym_t);
    else
    {
	VALUE hook = cmd_symbol_value(sym_idle_hook, sym_t);
	if(!VOIDP(hook) && !NILP(hook))
	{
	    cmd_call_hook(sym_idle_hook, sym_nil, sym_nil);
	    res = TRUE;
	}
    }
    return res;
}

/* The input loop should call this function when throw_value == LISP_NULL.
   It returns TRUE when the input loop should exit, returning whatever
   is stored in *RESULT-P. */
bool
handle_input_exception(VALUE *result_p)
{
    VALUE tv = throw_value;
    VALUE car = VCAR(tv);
    throw_value = LISP_NULL;
    *result_p = LISP_NULL;
    
    if(car == sym_exit)
    {
	*result_p = VCDR(tv);
	if(recurse_depth > 0)
	    return TRUE;
    }
    else if((car == sym_top_level) && (recurse_depth == 0))
	*result_p = VCDR(tv);
    else if(car == sym_quit)
	return TRUE;
    else if(car == sym_user_interrupt)
	handle_error(car, sym_nil);
    else if(car == sym_term_interrupt)
    {
	if(recurse_depth == 0)
	{
	    /* Autosave all buffers */
	    while(auto_save_buffers(TRUE) > 0)
		;
	}
	return TRUE;
    }
    else if(car == sym_error)
	handle_error(VCAR(VCDR(tv)), VCDR(VCDR(tv)));
    else if(recurse_depth == 0)
	handle_error(sym_no_catcher, LIST_1(car));
    else
    {
	throw_value = tv;
	return TRUE;
    }
    return FALSE;
}

_PR VALUE cmd_recursive_edit(void);
DEFUN_INT("recursive-edit", cmd_recursive_edit, subr_recursive_edit, (void), V_Subr0, DOC_recursive_edit, "") /*
::doc:recursive_edit::
recursive-edit

Enter a new recursive-edit.
::end:: */
{
    return sys_event_loop();
}

_PR VALUE cmd_recursion_depth(void);
DEFUN("recursion-depth", cmd_recursion_depth, subr_recursion_depth, (void), V_Subr0, DOC_recursion_depth) /*
::doc:recursion_depth::
recursion-depth

Returns the number of recursive-edit's deep we are, zero signifies the
original level.
::end:: */
{
    return MAKE_INT(recurse_depth);
}

void
main_init(void)
{
    ADD_SUBR_INT(subr_recursive_edit);
    ADD_SUBR(subr_recursion_depth);
    INTERN(quit);
    INTERN(exit);
    INTERN(top_level);
    INTERN(command_line_args);
}
