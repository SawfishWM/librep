/* misc.c -- Miscellaneous functions
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
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include <lib/jade_protos.h>
#include "revision.h"
#include "build.h"

#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>

_PR void misc_init(void);

DEFSTRING(vers_string, VERSSTRING);
DEFSTRING(build_id_string,
	  BUILD_DATE " by " BUILD_USER "@" BUILD_HOST ", for " HOST_TYPE ".");

_PR VALUE sym_operating_system, sym_window_system, sym_process_environment;
DEFSYM(operating_system, "operating-system");
DEFSYM(window_system, "window-system");
DEFSYM(process_environment, "process-environment"); /*
::doc:operating_system::
A symbol defining the type of operating system that Jade is running
under. Currently this is always the symbol `unix'.
::end::
::doc:window_system::
A symbol defining the window system that Jade is running under. The only
current possibility is `x11'.
::end::
::doc:process_environment::
A list of all environment variables (as strings "NAME=VALUE") passed
to Jade. Also used to specify the environment of all subprocesses.
::end:: */
#ifdef HAVE_X11
DEFSYM(x11, "x11");
#endif
#ifdef HAVE_UNIX
DEFSYM(unix, "unix");
#endif

#ifndef HAVE_STPCPY
/*
 * copy src to dst, returning pointer to terminating '\0' of dst.
 * Although this has a prototype in my <string.h> it doesn't seem to be
 * in the actual library??
 */
char *
stpcpy(register char *dst, register const char *src)
{
    while((*dst++ = *src++) != 0)
	;
    return(dst - 1);
}
#endif /* !HAVE_STPCPY */

#ifndef HAVE_MEMCHR
void *
memchr(const void *mem, int c, size_t len)
{
    register char *tmp = (char *)mem;
    while(len-- > 0)
    {
	if(*tmp++ != c)
	    continue;
	return((void *)(tmp - 1));
    }
    return(NULL);
}
#endif /* !HAVE_MEMCHR */

_PR VALUE cmd_beep(void);
DEFUN_INT("beep", cmd_beep, subr_beep, (void), V_Subr0, DOC_beep, "") /*
::doc:beep::
beep

Rings a bell.
::end:: */
{
    beep(curr_vw);
    return(sym_t);
}

_PR VALUE cmd_complete_string(VALUE existing, VALUE arg_list, VALUE fold);
DEFUN("complete-string", cmd_complete_string, subr_complete_string,
      (VALUE existing, VALUE arg_list, VALUE fold), V_Subr3,
      DOC_complete_string) /*
::doc:complete_string::
complete-string TEMPLATE LIST [FOLD-CASE]

Return a string whose beginning matches the string TEMPLATE, and is unique
in the set of all strings in LIST which also match TEMPLATE. If FOLD-CASE
is t, all matching ignores character case.
::end:: */
{
    u_char *orig, *match = NULL;
    int matchlen = 0, origlen;

    DECLARE1(existing, STRINGP);
    DECLARE2(arg_list, LISTP);

    orig = VSTR(existing);
    origlen = STRING_LEN(existing);

    while(CONSP(arg_list))
    {
	VALUE arg = VCAR(arg_list);
	if(STRINGP(arg))
	{
	    u_char *tmp = VSTR(arg);
	    if((NILP(fold) ? strncmp : strncasecmp)(orig, tmp, origlen) == 0)
	    {
		if(match)
		{
		    u_char *tmp2 = match + origlen;
		    tmp += origlen;
		    while(*tmp2 && *tmp)
		    {
			if(NILP(fold)
			   ? (*tmp2 != *tmp)
			   : (tolower(*tmp2) != tolower(*tmp)))
			{
			    break;
			}
			tmp2++; tmp++;
		    }
		    if((tmp2 - match) < matchlen)
			matchlen = tmp2 - match;
		}
		else
		{
		    match = tmp;
		    matchlen = strlen(tmp);
		}
	    }
	}
	arg_list = VCDR(arg_list);
    }
    if(match)
	return string_dupn(match, matchlen);
    else
	return sym_nil;
}

_PR VALUE cmd_current_time(void);
DEFUN("current-time", cmd_current_time, subr_current_time, (void), V_Subr0, DOC_current_time) /*
::doc:current_time::
current-time

Return a value denoting the current system time. This will be a cons cell
containing (DAYS . SECONDS), the number of DAYS since the epoch, and the
number of seconds since the start of the day (universal time).
::end:: */
{
    u_long time = sys_time();
    return MAKE_TIME(time);
}

_PR VALUE cmd_fix_time(VALUE time);
DEFUN("fix-time", cmd_fix_time, subr_fix_time, (VALUE time), V_Subr1, DOC_fix_time) /*
::doc:fix_time::
fix-time TIMESTAMP

Ensure that the two parts of TIMESTAMP are mutually consistent. If not
TIMESTAMP is altered. Returns TIMESTAMP.
::end:: */
{
    u_long timestamp;
    DECLARE1(time, TIMEP);
    timestamp = GET_TIME(time);
    VCAR(time) = MAKE_INT(timestamp / 86400);
    VCDR(time) = MAKE_INT(timestamp % 86400);
    return time;
}

_PR VALUE cmd_current_time_string(VALUE time, VALUE format);
DEFUN("current-time-string", cmd_current_time_string,
      subr_current_time_string, (VALUE time, VALUE format), V_Subr2,
      DOC_current_time_string) /*
::doc:current_time_string::
current-time-string [TIME] [FORMAT]

Returns a human-readable string defining the current date and time, or if
specified, that defining TIME.

If defined, FORMAT is a string defining how to create the string. It has
the same conventions as the template to the C library's strftime function.
::end:: */
{
    time_t timestamp;
    if(TIMEP(time))
	timestamp = GET_TIME(time);
    else
	timestamp = sys_time();
    if(STRINGP(format))
    {
	struct tm *loctime = localtime(&timestamp);
	char buf[256];
	int len = strftime(buf, sizeof(buf), VSTR(format), loctime);
	if(len > 0)
	    return string_dupn(buf, len);
    }
    else
    {
	char *str = ctime(&timestamp);
	if(str)
	    return(string_dupn(str, strlen(str) - 1));
    }
    return LISP_NULL;
}

_PR VALUE cmd_time_later_p(VALUE t1, VALUE t2);
DEFUN("time-later-p", cmd_time_later_p, subr_time_later_p, (VALUE t1, VALUE t2), V_Subr2, DOC_time_later_p) /*
::doc:time_later_p::
time-later-p TIME-STAMP1 TIME-STAMP2

Returns t when TIME-STAMP1 refers to a later time than TIME-STAMP2.
::end:: */
{
    u_long time1, time2;
    DECLARE1(t1, TIMEP);
    DECLARE2(t2, TIMEP);
    time1 = GET_TIME(t1);
    time2 = GET_TIME(t2);
    return time1 > time2 ? sym_t : sym_nil;
}

_PR VALUE cmd_sleep_for(VALUE secs, VALUE msecs);
DEFUN("sleep-for", cmd_sleep_for, subr_sleep_for, (VALUE secs, VALUE msecs),
      V_Subr2, DOC_sleep_for) /*
::doc:sleep_for::
sleep-for SECONDS [MILLISECONDS]

Pause for SECONDS (plus the optional MILLISECOND component) length of time.
::end:: */
{
    DECLARE1(secs, INTP);
    sys_sleep_for(VINT(secs), INTP(msecs) ? VINT(msecs) : 0);
    return sym_t;
}

_PR VALUE cmd_sit_for(VALUE secs, VALUE msecs);
DEFUN("sit-for", cmd_sit_for, subr_sit_for, (VALUE secs, VALUE msecs),
      V_Subr2, DOC_sit_for) /*
::doc:sit_for::
sit-for [SECONDS] [MILLISECONDS]

Wait for input to arrive and be processed. No more than SECONDS seconds plus
MILLISECONDS milliseconds will be waited. If at the end of this time no
input has arrived, return t. Otherwise return nil if input was found.

If neither SECONDS nor MILLISECONDS is defined the command will return
immediately, using a null timeout.
::end:: */
{
    return sys_sit_for(((INTP(secs) ? VINT(secs) : 0) * 1000)
		       + (INTP(msecs) ? VINT(msecs) : 0));
}

_PR VALUE cmd_user_login_name(void);
DEFUN("user-login-name", cmd_user_login_name, subr_user_login_name, (void), V_Subr0, DOC_user_login_name) /*
::doc:user_login_name::
user-login-name

Returns the login name of the user (a string).
On the Amiga this is taken from the environment variable `USERNAME'.
::end:: */
{
    return sys_user_login_name();
}

_PR VALUE cmd_user_full_name(void);
DEFUN("user-full-name", cmd_user_full_name, subr_user_full_name, (void), V_Subr0, DOC_user_full_name) /*
::doc:user_full_name::
user-full-name

Returns the real name of the user (a string).
On the Amiga this is taken from the environment variable `REALNAME'.
::end:: */
{
    return sys_user_full_name();
}

_PR VALUE cmd_user_home_directory(VALUE user);
DEFUN("user-home-directory", cmd_user_home_directory,
      subr_user_home_directory, (VALUE user), V_Subr1,
      DOC_user_home_directory) /*
::doc:user_home_directory::
user-home-directory [USER]

Return the path to USER's home directory (a string). When USER is undefined
the directory of the user who executed Jade is found.
::end:: */
{
    if(!NILP(user))
	DECLARE1(user, STRINGP);
    return sys_user_home_directory(user);
}

_PR VALUE cmd_system_name(void);
DEFUN("system-name", cmd_system_name, subr_system_name, (void), V_Subr0,  DOC_system_name) /*
::doc:system_name::
system-name

Returns the name of the host which the editor is running on.
::end:: */
{
    return sys_system_name();
}

_PR VALUE cmd_major_version_number(void);
DEFUN("major-version-number", cmd_major_version_number, subr_major_version_number, (void), V_Subr0, DOC_major_version_number) /*
::doc:major_version_number::
major-version-number
::end:: */
{
    return MAKE_INT(MAJOR);
}

_PR VALUE cmd_minor_version_number(void);
DEFUN("minor-version-number", cmd_minor_version_number, subr_minor_version_number, (void), V_Subr0, DOC_minor_version_number) /*
::doc:minor_version_number::
minor-version-number
::end:: */
{
    return MAKE_INT(MINOR);
}

_PR VALUE cmd_version_string(void);
DEFUN("version-string", cmd_version_string, subr_version_string, (void), V_Subr0, DOC_version_string) /*
::doc:version_string::
version-string

Return a string identifying the current version of Jade.
::end:: */
{
    return VAL(&vers_string);
}

_PR VALUE cmd_build_id_string(void);
DEFUN("build-id-string", cmd_build_id_string, subr_build_id_string, (void), V_Subr0, DOC_build_id_string) /*
::doc:build_id_string::
build-id-string

Returns a string describing when, where, and by who the running version of
Jade was built.
::end:: */
{
    return VAL(&build_id_string);
}

/* Try to work out how many bits of randomness rand() will give.. */
#ifdef HAVE_LRAND48
# define RAND_BITS 31
# define rand lrand48
# define srand srand48
#else
# if RAND_MAX == 32768
#  define RAND_BITS 15
# elif RAND_MAX == 2147483647
#  define RAND_BITS 31
# else
#  define RAND_BITS 63
# endif
#endif

_PR VALUE cmd_random(VALUE arg);
DEFUN("random", cmd_random, subr_random, (VALUE arg), V_Subr1, DOC_random) /*
::doc:random::
random [LIMIT]

Produce a pseudo-random number between zero and LIMIT (or the largest positive
integer representable). If LIMIT is the symbol `t' the generator is seeded
with the current time of day.
::end:: */
{
    long limit, divisor, val;
    if(arg == sym_t)
    {
	srand(time(0));
	return sym_t;
    }

    if(INTP(arg))
	limit = VINT(arg);
    else
	limit = LISP_MAX_INT;
    divisor = LISP_MAX_INT / limit;
    do {
	val = rand();
#if LISP_INT_BITS > RAND_BITS
	val = (val << RAND_BITS) | rand();
# if LISP_INT_BITS > 2*RAND_BITS
	val = (val << RAND_BITS) | rand();
#  if LISP_INT_BITS > 3*RAND_BITS
	val = (val << RAND_BITS) | rand();
#   if LISP_INT_BITS > 4*RAND_BITS
	val = (val << RAND_BITS) | rand();
#   endif
#  endif
# endif
#endif
	val /= divisor;
    } while(val >= limit);

    return MAKE_INT(val);
}

void
misc_init(void)
{
    INTERN(operating_system); DOC(operating_system);
#ifdef HAVE_UNIX
    INTERN(unix);
    VSYM(sym_operating_system)->value = sym_unix;
#endif
    INTERN(window_system); DOC(window_system);
#ifdef HAVE_X11
    INTERN(x11);
    VSYM(sym_window_system)->value = sym_x11;
#endif

    INTERN(process_environment); DOC(process_environment);
    VSYM(sym_process_environment)->value = sym_nil;

    ADD_SUBR_INT(subr_beep);
    ADD_SUBR(subr_complete_string);
    ADD_SUBR(subr_current_time);
    ADD_SUBR(subr_fix_time);
    ADD_SUBR(subr_current_time_string);
    ADD_SUBR(subr_time_later_p);
    ADD_SUBR(subr_sleep_for);
    ADD_SUBR(subr_sit_for);

    ADD_SUBR(subr_user_login_name);
    ADD_SUBR(subr_user_full_name);
    ADD_SUBR(subr_user_home_directory);
    ADD_SUBR(subr_system_name);

    ADD_SUBR(subr_major_version_number);
    ADD_SUBR(subr_minor_version_number);
    ADD_SUBR(subr_version_string);
    ADD_SUBR(subr_build_id_string);

    ADD_SUBR(subr_random);
}
