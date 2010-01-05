/* record-profile.c -- very basic Lisp profiler

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   $Id$

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Commentary:

   Hook into the interrupt-checking code to record the current
   backtrace statistics. Uses SIGPROF to tell the lisp system when it
   should interrupt (can't run the profiler off the signal itself,
   since data would need to be allocated from the signal handler) */

#define _GNU_SOURCE

/* AIX requires this to be the first thing in the file.  */
#include <config.h>
#ifdef __GNUC__
# define alloca __builtin_alloca
#else
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include "repint.h"
#include <signal.h>
#include <time.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

static repv profile_table;
static rep_bool profiling;

static void (*chained_test_interrupt)(void);

static int profile_interval = 10;		/* microseconds */


/* SIGPROF handling */

#ifdef HAVE_SETITIMER
static RETSIGTYPE
sigprof_handler (int unused)
{
    /* force an interrupt */
    rep_test_int_counter = rep_test_int_period;
}
#endif

static void
set_timer (void)
{
#ifdef HAVE_SETITIMER
    struct itimerval it, tem;
    it.it_interval.tv_usec = 0;
    it.it_interval.tv_sec = 0;
    it.it_value.tv_usec = profile_interval % 1000000;
    it.it_value.tv_sec = profile_interval / 1000000;
    setitimer (ITIMER_PROF, &it, &tem);
    signal (SIGPROF, sigprof_handler);
#endif
}

static void
clear_timer (void)
{
#ifdef HAVE_SETITIMER
    signal (SIGPROF, SIG_IGN);
#endif
}


/* profile recording */

static void
test_interrupt (void)
{
    if (profiling)
    {
	repv *seen = alloca (rep_max_lisp_depth * sizeof (repv));
	struct rep_Call *c;
	int seen_i = 0;
	for (c = rep_call_stack; c != 0 && c->fun != Qnil; c = c->next)
	{
	    repv name;
	    switch (rep_TYPE (c->fun))
	    {
	    case rep_Subr0: case rep_Subr1: case rep_Subr2: case rep_Subr3:
	    case rep_Subr4: case rep_Subr5: case rep_SubrN:
		name = rep_XSUBR (c->fun)->name;
		break;

	    case rep_Funarg:
		name = rep_FUNARG (c->fun)->name;
		break;

	    default:
		continue;
	    }
	    if (rep_STRINGP (name))
	    {
		repv tem;
		int j;

		name = Fintern (name, Qnil);
		for (j = 0; j < seen_i; j++)
		{
		    if (seen[j] == name)
			goto skip;
		}

		tem = F_structure_ref (profile_table, name);
		if (rep_VOIDP (tem))
		    tem = Fcons (rep_MAKE_INT (0), rep_MAKE_INT (0));
		if (c == rep_call_stack)
		    rep_CAR (tem) = rep_MAKE_INT (rep_INT (rep_CAR (tem)) + 1);
		rep_CDR (tem) = rep_MAKE_INT (rep_INT (rep_CDR (tem)) + 1);
		Fstructure_define (profile_table, name, tem);

		seen[seen_i++] = name;
	    }
	skip: {}
	}
	set_timer ();
    }
    (*chained_test_interrupt) ();
}


/* interface */

DEFUN ("start-profiler", Fstart_profiler, Sstart_profiler, (void), rep_Subr0)
{
    profile_table = Fmake_structure (Qnil, Qnil, Qnil, Qnil);
    profiling = rep_TRUE;
    set_timer ();
    return Qt;
}

DEFUN ("stop-profiler", Fstop_profiler, Sstop_profiler, (void), rep_Subr0)
{
    profiling = rep_FALSE;
    clear_timer ();
    return Qt;
}

DEFUN ("fetch-profile", Ffetch_profile, Sfetch_profile, (void), rep_Subr0)
{
    return profile_table ? profile_table : Qnil;
}

DEFUN ("profile-interval", Fprofile_interval,
       Sprofile_interval, (repv arg), rep_Subr1)
{
    repv ret = rep_MAKE_INT (profile_interval);
    if (rep_INTP (arg) && rep_INT (arg) > 0)
	profile_interval = rep_INT (arg);
    return ret;
}


/* init */

repv
rep_dl_init (void)
{
    repv tem = rep_push_structure ("rep.lang.record-profile");

    rep_ADD_SUBR (Sstart_profiler);
    rep_ADD_SUBR (Sstop_profiler);
    rep_ADD_SUBR (Sfetch_profile);
    rep_ADD_SUBR (Sprofile_interval);
    rep_mark_static (&profile_table);

#ifdef HAVE_SETITIMER
    signal (SIGPROF, SIG_IGN);
#endif

    chained_test_interrupt = rep_test_int_fun;
    rep_test_int_fun = test_interrupt;

    return rep_pop_structure (tem);
}
