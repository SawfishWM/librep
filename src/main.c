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

#define INIT_SCR "init"

_PR int main(int, char **);
_PR int inner_main(int, char **);
_PR void doconmsg(u_char *);
_PR void main_init(void);

_PR bool input_lock;
    bool input_lock;
_PR StrMem main_strmem;
    StrMem main_strmem;

_PR int recurse_depth;
int recurse_depth = -1;

_PR VALUE sym_exit, sym_quit, sym_top_level, sym_command_line_args;
VALUE sym_exit, sym_quit, sym_top_level, sym_command_line_args;

static u_char *init_script = INIT_SCR;

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
	    doconmsg(VERSSTRING "\n");
	    return(FALSE);
	}
	else if(!strcmp("-log-msgs", *argv))
	    log_messages = TRUE;
	else if(!strcmp("-?", *argv) || !strcmp("-help", *argv))
	{
	    usage();
	    return(FALSE);
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
    VSYM(sym_command_line_args)->sym_Value = head;
    *argc_p = argc;
    *argv_p = argv;
    return(TRUE);
}

int
main(int argc, char **argv)
{
    int rc;
    if(!initmem())
	return(10);
#if 0
    main_strmem.sm_FreesBeforeFlush = 3;	/* reasonable? */
#endif
    sm_init(&main_strmem);
    values_init();
    if(symbols_init())
    {
	rc = sys_init(argc, argv);
	symbols_kill();
    }
    else
	rc = 5;
    values_kill();
    sm_kill(&main_strmem);
    killmem();
    return(rc);
}

/* This function is called from sys_init(), it completes the initialisation
   process and calls the top-level event loop.  sys_init() must advance
   ARGC and ARGV so they point to the next unused argument.  */
int
inner_main(int argc, char **argv)
{
    int rc = 5;

    values_init2();
    lisp_init();
    lispcmds_init();
    lispmach_init();
    buffers_init();
    commands_init();
    edit_init();
    editrect_init();
    find_init();
    glyphs_init();
    io_init();
    keys_init();
    main_init();
    misc_init();
    movement_init();
    refresh_init();
    streams_init();
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
	    cursor(curr_vw, CURS_ON);
	    res = event_loop();
	}
	else if(throw_value && VCAR(throw_value) == sym_quit)
	{
	    if(NUMBERP(VCDR(throw_value)))
		rc = VNUM(VCDR(throw_value));
	    else
		rc = 0;
	}
	else
	    doconmsg("jade: error in initialisation script\n");
#ifdef HAVE_SUBPROCESSES
        proc_kill();
#endif
	server_kill();
	windows_kill();
	views_kill();
	buffers_kill();
	find_kill();
	glyphs_kill();
	streams_kill();
    }
    db_kill();
    return(rc);
}

_PR VALUE cmd_recursive_edit(void);
DEFUN_INT("recursive-edit", cmd_recursive_edit, subr_recursive_edit, (void), V_Subr0, DOC_recursive_edit, "") /*
::doc:recursive_edit::
recursive-edit

Enter a new recursive-edit.
::end:: */
{
    VALUE res;
    cursor(curr_vw, CURS_ON);
    res = event_loop();
    if(curr_vw)
	cursor(curr_vw, CURS_OFF);
    return(res);
}

_PR VALUE cmd_recursion_depth(void);
DEFUN("recursion-depth", cmd_recursion_depth, subr_recursion_depth, (void), V_Subr0, DOC_recursion_depth) /*
::doc:recursion_depth::
recursion-depth

Returns the number of recursive-edit's deep we are, zero signifies the
original level.
::end:: */
{
    return(make_number(recurse_depth));
}

_PR VALUE cmd_input_lock(VALUE status);
DEFUN("input-lock", cmd_input_lock, subr_input_lock, (VALUE args), V_SubrN, DOC_input_lock) /*
::doc:input_lock::
input-lock [STATUS]

Sets or returns the status of the input lock. When this value is non-zero
no user input is accepted, only messages from ARexx can get through.
::end:: */
{
    if(CONSP(args))
    {
	args = VCAR(args);
	if(NILP(args))
	    input_lock = FALSE;
	else
	    input_lock = TRUE;
    }
    if(input_lock)
	return(sym_t);
    return(sym_nil);
}

void
main_init(void)
{
    ADD_SUBR(subr_recursive_edit);
    ADD_SUBR(subr_recursion_depth);
    ADD_SUBR(subr_input_lock);
    INTERN(sym_quit, "quit");
    INTERN(sym_exit, "exit");
    INTERN(sym_top_level, "top-level");
    INTERN(sym_command_line_args, "command-line-args");
}
