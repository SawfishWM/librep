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

#include "repint.h"
#include "build.h"

#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>

void (*rep_beep_fun)(void);

DEFSTRING(build_id_string,
	  BUILD_DATE " by " BUILD_USER "@" BUILD_HOST ", for " HOST_TYPE ".");
DEFSTRING(rep_version_string, REP_VERSION);

DEFSYM(operating_system, "operating-system");
DEFSYM(process_environment, "process-environment");
DEFSYM(rep_version, "rep-version");
DEFSYM(rep_build_id, "rep-build-id"); /*
::doc:Voperating-system::
A symbol defining the type of operating system that Jade is running
under. Currently this is always the symbol `unix'.
::end::
::doc:Vprocess-environment::
A list of all environment variables (as strings "NAME=VALUE") passed
to the interpreter. Also used to specify the environment of subprocesses.
::end::
::doc:Vrep-version::
A string defining the current version of the REP interpreter.
::end::
::doc:Vrep-build-id::
A string describing when, where, and by who the running version of the
LISP interpreter was built.
::end:: */

#ifdef rep_HAVE_UNIX
DEFSYM(unix, "unix");
#endif

DEFSYM(upcase_table, "upcase-table");
DEFSYM(downcase_table, "downcase-table");
DEFSYM(flatten_table, "flatten-table");
/* Some doc strings
::doc:Vupcase-table::
256-byte string holding translations to turn each character into its
upper-case equivalent.
::end::
::doc:Vdowncase-table::
256-byte string holding translations to turn each character into its
lower-case equivalent.
::end::
::doc:Vflatten-table::
Translation table to convert newline characters to spaces.
::end:: */

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

u_char *
rep_str_dupn(const u_char *old, int len)
{
    char *new = rep_alloc(len + 1);
    if(new)
    {
	memcpy(new, old, len);
	new[len] = 0;
    }
    return new;
}

DEFUN_INT("beep", Fbeep, Sbeep, (void), rep_Subr0, "") /*
::doc:Sbeep::
beep

Rings a bell.
::end:: */
{
    if (rep_beep_fun != 0)
	(*rep_beep_fun)();
    return Qt;
}

DEFUN("complete-string", Fcomplete_string, Scomplete_string,
      (repv existing, repv arg_list, repv fold), rep_Subr3) /*
::doc:Scomplete-string::
complete-string TEMPLATE LIST [FOLD-CASE]

Return a string whose beginning matches the string TEMPLATE, and is unique
in the set of all strings in LIST which also match TEMPLATE. If FOLD-CASE
is t, all matching ignores character case.
::end:: */
{
    u_char *orig, *match = NULL;
    int matchlen = 0, origlen;

    rep_DECLARE1(existing, rep_STRINGP);
    rep_DECLARE2(arg_list, rep_LISTP);

    orig = rep_STR(existing);
    origlen = rep_STRING_LEN(existing);

    while(rep_CONSP(arg_list))
    {
	repv arg = rep_CAR(arg_list);
	if(rep_STRINGP(arg))
	{
	    u_char *tmp = rep_STR(arg);
	    if((rep_NILP(fold) ? strncmp : strncasecmp)(orig, tmp, origlen) == 0)
	    {
		if(match)
		{
		    u_char *tmp2 = match + origlen;
		    tmp += origlen;
		    while(*tmp2 && *tmp)
		    {
			if(rep_NILP(fold)
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
	arg_list = rep_CDR(arg_list);
    }
    if(match)
	return rep_string_dupn(match, matchlen);
    else
	return Qnil;
}

DEFUN("current-time", Fcurrent_time, Scurrent_time, (void), rep_Subr0) /*
::doc:Scurrent-time::
current-time

Return a value denoting the current system time. This will be a cons cell
containing (DAYS . SECONDS), the number of DAYS since the epoch, and the
number of seconds since the start of the day (universal time).
::end:: */
{
    u_long time = rep_time();
    return rep_MAKE_TIME(time);
}

DEFUN("fix-time", Ffix_time, Sfix_time, (repv time), rep_Subr1) /*
::doc:Sfix-time::
fix-time TIMESTAMP

Ensure that the two parts of TIMESTAMP are mutually consistent. If not
TIMESTAMP is altered. Returns TIMESTAMP.
::end:: */
{
    u_long timestamp;
    rep_DECLARE1(time, rep_TIMEP);
    timestamp = rep_GET_TIME(time);
    rep_CAR(time) = rep_MAKE_INT(timestamp / 86400);
    rep_CDR(time) = rep_MAKE_INT(timestamp % 86400);
    return time;
}

DEFUN("current-time-string", Fcurrent_time_string,
      Scurrent_time_string, (repv time, repv format), rep_Subr2) /*
::doc:Scurrent-time-string::
current-time-string [TIME] [FORMAT]

Returns a human-readable string defining the current date and time, or if
specified, that defining TIME.

If defined, FORMAT is a string defining how to create the string. It has
the same conventions as the template to the C library's strftime function.
::end:: */
{
    time_t timestamp;
    if(rep_TIMEP(time))
	timestamp = rep_GET_TIME(time);
    else
	timestamp = rep_time();
    if(rep_STRINGP(format))
    {
	struct tm *loctime = localtime(&timestamp);
	char buf[256];
	int len = strftime(buf, sizeof(buf), rep_STR(format), loctime);
	if(len > 0)
	    return rep_string_dupn(buf, len);
    }
    else
    {
	char *str = ctime(&timestamp);
	if(str)
	    return(rep_string_dupn(str, strlen(str) - 1));
    }
    return rep_NULL;
}

DEFUN("time-later-p", Ftime_later_p, Stime_later_p, (repv t1, repv t2), rep_Subr2) /*
::doc:Stime-later-p::
time-later-p TIME-STAMP1 TIME-STAMP2

Returns t when TIME-STAMP1 refers to a later time than TIME-STAMP2.
::end:: */
{
    u_long time1, time2;
    rep_DECLARE1(t1, rep_TIMEP);
    rep_DECLARE2(t2, rep_TIMEP);
    time1 = rep_GET_TIME(t1);
    time2 = rep_GET_TIME(t2);
    return time1 > time2 ? Qt : Qnil;
}

DEFUN("sleep-for", Fsleep_for, Ssleep_for, (repv secs, repv msecs),
      rep_Subr2) /*
::doc:Ssleep-for::
sleep-for SECONDS [MILLISECONDS]

Pause for SECONDS (plus the optional MILLISECOND component) length of time.
::end:: */
{
    rep_DECLARE1(secs, rep_INTP);
    rep_sleep_for(rep_INT(secs), rep_INTP(msecs) ? rep_INT(msecs) : 0);
    return Qt;
}

DEFUN("sit-for", Fsit_for, Ssit_for, (repv secs, repv msecs),
      rep_Subr2) /*
::doc:Ssit-for::
sit-for [SECONDS] [MILLISECONDS]

Wait for input to arrive and be processed. No more than SECONDS seconds plus
MILLISECONDS milliseconds will be waited. If at the end of this time no
input has arrived, return t. Otherwise return nil if input was found.

If neither SECONDS nor MILLISECONDS is defined the command will return
immediately, using a null timeout.
::end:: */
{
    return rep_sit_for(((rep_INTP(secs) ? rep_INT(secs) : 0) * 1000)
		       + (rep_INTP(msecs) ? rep_INT(msecs) : 0));
}

DEFUN("user-login-name", Fuser_login_name, Suser_login_name, (void), rep_Subr0) /*
::doc:Suser-login-name::
user-login-name

Returns the login name of the user (a string).
::end:: */
{
    return rep_user_login_name();
}

DEFUN("user-full-name", Fuser_full_name, Suser_full_name, (repv arg), rep_Subr1) /*
::doc:Suser-full-name::
user-full-name [REAL-NAME]

Returns the real name of the user (a string). If REAL-NAME is non-nil, it's
the name to return in subsequent calls.
::end:: */
{
    static repv saved_name;
    if(rep_STRINGP(arg))
    {
	if(!saved_name)
	    rep_mark_static(&saved_name);
	saved_name = arg;
    }
    return saved_name ? saved_name : rep_user_full_name();
}

DEFUN("user-home-directory", Fuser_home_directory,
      Suser_home_directory, (repv user), rep_Subr1) /*
::doc:Suser-home-directory::
user-home-directory [USER]

Return the path to USER's home directory (a string). When USER is undefined
the directory of the user who executed Jade is found.
::end:: */
{
    if(!rep_NILP(user))
	rep_DECLARE1(user, rep_STRINGP);
    return rep_user_home_directory(user);
}

DEFUN("system-name", Fsystem_name, Ssystem_name, (void), rep_Subr0) /*
::doc:Ssystem-name::
system-name

Returns the name of the host which the editor is running on.
::end:: */
{
    return rep_system_name();
}

DEFUN("message", Fmessage, Smessage, (repv string, repv now), rep_Subr2) /*
::doc:Smessage::
message STRING [DISPLAY-NOW]

Temporarily sets the status display to STRING, this may not happen until the
next complete redisplay, unless DISPLAY-NOW is non-nil.
::end:: */
{
    rep_DECLARE1(string, rep_STRINGP);
    if (rep_message_fun != 0)
    {
	(*rep_message_fun)(rep_message, rep_STR(string));
	if(!rep_NILP(now))
	    (*rep_message_fun)(rep_redisplay_message);
    }
    return string;
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

DEFUN("random", Frandom, Srandom, (repv arg), rep_Subr1) /*
::doc:Srandom::
random [LIMIT]

Produce a pseudo-random number between zero and LIMIT (or the largest positive
integer representable). If LIMIT is the symbol `t' the generator is seeded
with the current time of day.
::end:: */
{
    long limit, divisor, val;
    if(arg == Qt)
    {
	srand(time(0));
	return Qt;
    }

    if(rep_INTP(arg))
	limit = rep_INT(arg);
    else
	limit = rep_LISP_MAX_INT;
    divisor = rep_LISP_MAX_INT / limit;
    do {
	val = rand();
#if rep_LISP_INT_BITS > RAND_BITS
	val = (val << RAND_BITS) | rand();
# if rep_LISP_INT_BITS > 2*RAND_BITS
	val = (val << RAND_BITS) | rand();
#  if rep_LISP_INT_BITS > 3*RAND_BITS
	val = (val << RAND_BITS) | rand();
#   if rep_LISP_INT_BITS > 4*RAND_BITS
	val = (val << RAND_BITS) | rand();
#   endif
#  endif
# endif
#endif
	val /= divisor;
    } while(val >= limit);

    return rep_MAKE_INT(val);
}

DEFUN("translate-string", Ftranslate_string, Stranslate_string, (repv string, repv table), rep_Subr2) /*
::doc:Stranslate-string:
translate-string STRING TRANSLATION-TABLE

Applies the TRANSLATION-TABLE to each character in the string STRING.
TRANSLATION-TABLE is a string, each character represents the translation
for an ascii character of that characters position in the string. If the
string is less than 256 chars long any undefined characters will remain
unchanged.
Note that the STRING really is modified, no copy is made!
::end:: */
{
    int tablen, slen;
    register u_char *str;
    rep_DECLARE1(string, rep_STRINGP);
    rep_DECLARE2(table, rep_STRINGP);
    tablen = rep_STRING_LEN(table);
    if(!rep_STRING_WRITABLE_P(string))
	return(rep_signal_arg_error(string, 1));
    str = rep_STR(string);
    slen = rep_STRING_LEN(string);
    while(slen-- > 0)
    {
	register u_char c = *str;
	*str++ = (c < tablen) ? rep_STR(table)[c] : c;
    }
    return(string);
}

DEFUN("alpha-char-p", Falpha_char_p, Salpha_char_p, (repv ch), rep_Subr1) /*
::doc:Salpha-char-p::
alpha-char-p CHAR

Returns t if CHAR is an alphabetic character.
::end:: */
{
    return (rep_INTP(ch) && isalpha(rep_INT(ch))) ? Qt : Qnil;
}

DEFUN("upper-case-p", Fupper_case_p, Supper_case_p, (repv ch), rep_Subr1) /*
::doc:Supper-case-p::
upper-case-p CHAR

Returns t if CHAR is upper case.
::end:: */
{
    return (rep_INTP(ch) && isupper(rep_INT(ch))) ? Qt : Qnil;
}

DEFUN("lower-case-p", Flower_case_p, Slower_case_p, (repv ch), rep_Subr1) /*
::doc:Slower-case-p::
lower-case-p CHAR

Returns t if CHAR is lower case.
::end:: */
{
    return (rep_INTP(ch) && islower(rep_INT(ch))) ? Qt : Qnil;
}

DEFUN("digit-char-p", Fdigit_char_p, Sdigit_char_p, (repv ch), rep_Subr1) /*
::doc:Sdigit-char-p::
digit-char-p CHAR

Returns t if CHAR is a digit.
::end:: */
{
    return (rep_INTP(ch) && isdigit(rep_INT(ch))) ? Qt : Qnil;
}

DEFUN("alphanumericp", Falphanumericp, Salphanumericp, (repv ch), rep_Subr1) /*
::doc:Salphanumericp::
alphanumericp CHAR

Returns t if CHAR is alpha-numeric.
::end:: */
{
    return (rep_INTP(ch) && isalnum(rep_INT(ch))) ? Qt : Qnil;
}

DEFUN("space-char-p", Fspace_char_p, Sspace_char_p, (repv ch), rep_Subr1) /*
::doc:Sspace-char-p::
space-char-p CHAR

Returns t if CHAR is whitespace.
::end:: */
{
    return (rep_INTP(ch) && isspace(rep_INT(ch))) ? Qt : Qnil;
}

DEFUN("char-upcase", Fchar_upcase, Schar_upcase, (repv ch), rep_Subr1) /*
::doc:Schar-upcase::
char-upcase CHAR

Returns the upper-case equivalent of CHAR.
::end:: */
{
    rep_DECLARE1(ch, rep_INTP);
    return rep_MAKE_INT(toupper(rep_INT(ch)));
}

DEFUN("char-downcase", Fchar_downcase, Schar_downcase, (repv ch), rep_Subr1) /*
::doc:Schar-downcase::
char-downcase CHAR

Returns the lower-case equivalent of CHAR.
::end:: */
{
    rep_DECLARE1(ch, rep_INTP);
    return rep_MAKE_INT(tolower(rep_INT(ch)));
}

void
rep_misc_init(void)
{
    int i;

    rep_INTERN(operating_system);
#ifdef rep_HAVE_UNIX
    rep_INTERN(unix);
    rep_SYM(Qoperating_system)->value = Qunix;
#endif

    rep_INTERN(process_environment);
    rep_SYM(Qprocess_environment)->value = Qnil;

    rep_INTERN(rep_version);
    rep_SYM(Qrep_version)->value = rep_VAL(&rep_version_string);
    rep_INTERN(rep_build_id);
    rep_SYM(Qrep_build_id)->value = rep_VAL(&build_id_string);

    rep_ADD_SUBR_INT(Sbeep);
    rep_ADD_SUBR(Scomplete_string);
    rep_ADD_SUBR(Scurrent_time);
    rep_ADD_SUBR(Sfix_time);
    rep_ADD_SUBR(Scurrent_time_string);
    rep_ADD_SUBR(Stime_later_p);
    rep_ADD_SUBR(Ssleep_for);
    rep_ADD_SUBR(Ssit_for);

    rep_ADD_SUBR(Suser_login_name);
    rep_ADD_SUBR(Suser_full_name);
    rep_ADD_SUBR(Suser_home_directory);
    rep_ADD_SUBR(Ssystem_name);
    rep_ADD_SUBR(Smessage);
    rep_ADD_SUBR(Srandom);
    rep_ADD_SUBR(Stranslate_string);
    rep_ADD_SUBR(Salpha_char_p);
    rep_ADD_SUBR(Supper_case_p);
    rep_ADD_SUBR(Slower_case_p);
    rep_ADD_SUBR(Sdigit_char_p);
    rep_ADD_SUBR(Salphanumericp);
    rep_ADD_SUBR(Sspace_char_p);
    rep_ADD_SUBR(Schar_upcase);
    rep_ADD_SUBR(Schar_downcase);

    rep_INTERN(upcase_table);
    rep_SYM(Qupcase_table)->value = rep_make_string(257);
    rep_INTERN(downcase_table);
    rep_SYM(Qdowncase_table)->value = rep_make_string(257);
    for(i = 0; i < 256; i++)
    {
	rep_STR(rep_SYM(Qupcase_table)->value)[i] = toupper(i);
	rep_STR(rep_SYM(Qdowncase_table)->value)[i] = tolower(i);
    }
    rep_STR(rep_SYM(Qupcase_table)->value)[256] = 0;
    rep_STR(rep_SYM(Qdowncase_table)->value)[256] = 0;

    rep_INTERN(flatten_table);
    rep_SYM(Qflatten_table)->value = rep_make_string(12);
    for(i = 0; i < 10; i++)
	rep_STR(rep_SYM(Qflatten_table)->value)[i] = i;
    rep_STR(rep_SYM(Qflatten_table)->value)[10] = ' ';
    rep_STR(rep_SYM(Qflatten_table)->value)[11] = 0;
}
