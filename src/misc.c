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

#define _GNU_SOURCE

#include "repint.h"
#include "build.h"

#include <string.h>
#include <strings.h>		/* needed for strncasecmp () on UnixWare */
#include <ctype.h>
#include <stdlib.h>
#include <time.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

void (*rep_beep_fun)(void);

DEFSTRING(build_id_string,
	  BUILD_DATE " by " BUILD_USER "@" BUILD_HOST ", for " HOST_TYPE ".");
DEFSTRING(rep_version_string, REP_VERSION);

DEFSYM(operating_system, "operating-system");
DEFSYM(process_environment, "process-environment");
DEFSYM(rep_version, "rep-version");
DEFSYM(rep_interface_id, "rep-interface-id");
DEFSYM(rep_build_id, "rep-build-id"); /*
::doc:rep.system#operating-system::
A symbol defining the type of operating system that Jade is running
under. Currently this is always the symbol `unix'.
::end::
::doc:process-environment::
A list of all environment variables (as strings "NAME=VALUE") passed
to the interpreter. Also used to specify the environment of subprocesses.
::end::
::doc:rep.system#rep-version::
A string defining the current version of the REP interpreter.
::end::
::doc:rep.system#rep-build-id::
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
::doc:rep.data#upcase-table::
256-byte string holding translations to turn each character into its
upper-case equivalent.
::end::
::doc:rep.data#downcase-table::
256-byte string holding translations to turn each character into its
lower-case equivalent.
::end::
::doc:rep.data#flatten-table::
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

#ifndef HAVE_STRNCASECMP
/* Compare no more than N characters of S1 and S2,
   ignoring case, returning less than, equal to or
   greater than zero if S1 is lexicographically less
   than, equal to or greater than S2. (from glibc)  */
int
strncasecmp (const char *s1, const char *s2, size_t n)
{
  const unsigned char *p1 = (const unsigned char *) s1;
  const unsigned char *p2 = (const unsigned char *) s2;
  unsigned char c1, c2;

  if (p1 == p2 || n == 0)
    return 0;

  do
    {
      c1 = tolower (*p1++);
      c2 = tolower (*p2++);
      if (c1 == '\0' || c1 != c2)
        return c1 - c2;
    } while (--n > 0);

  return c1 - c2;
}
#endif

char *
rep_str_dupn(const char *old, int len)
{
    char *new = rep_alloc(len + 1);
    if(new)
    {
	memcpy(new, old, len);
	new[len] = 0;
    }
    return new;
}

static void
default_beep (void)
{
    fputc (7, stdout);
    fflush (stdout);
}

DEFUN_INT("beep", Fbeep, Sbeep, (void), rep_Subr0, "") /*
::doc:rep.system#beep::
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
::doc:rep.data#complete-string::
complete-string TEMPLATE LIST [FOLD-CASE]

Return a string whose beginning matches the string TEMPLATE, and is unique
in the set of all strings in LIST which also match TEMPLATE. If FOLD-CASE
is t, all matching ignores character case.
::end:: */
{
    char *orig, *match = NULL;
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
	    char *tmp = rep_STR(arg);
	    if((rep_NILP(fold)
		? strncmp (orig, tmp, origlen)
		: strncasecmp (orig, tmp, origlen)) == 0)
	    {
		if(match)
		{
		    char *tmp2 = match + origlen;
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
::doc:rep.system#current-time::
current-time

Return a value denoting the current system time. This will be a cons cell
containing (DAYS . SECONDS), the number of DAYS since the epoch, and the
number of seconds since the start of the day (universal time).
::end:: */
{
    unsigned long time = rep_time();
    return rep_MAKE_TIME(time);
}

DEFUN("current-utime", Fcurrent_utime, Scurrent_utime, (void), rep_Subr0) /*
::doc:rep.system#current-utime::
current-utime

Return the current time in microseconds.
::end:: */
{
    rep_long_long time = rep_utime ();
    return rep_make_longlong_int (time);
}

DEFUN("fix-time", Ffix_time, Sfix_time, (repv time), rep_Subr1) /*
::doc:rep.system#fix-time::
fix-time TIMESTAMP

Ensure that the two parts of TIMESTAMP are mutually consistent. If not
TIMESTAMP is altered. Returns TIMESTAMP.
::end:: */
{
    unsigned long timestamp;
    rep_DECLARE1(time, rep_TIMEP);
    timestamp = rep_GET_TIME(time);
    rep_CAR(time) = rep_MAKE_INT(timestamp / 86400);
    rep_CDR(time) = rep_MAKE_INT(timestamp % 86400);
    return time;
}

DEFUN("current-time-string", Fcurrent_time_string,
      Scurrent_time_string, (repv time, repv format), rep_Subr2) /*
::doc:rep.system#current-time-string::
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
	else
	    return rep_null_string ();
    }
    else
    {
	char *str = ctime(&timestamp);
	if(str != 0)
	    return rep_string_dupn(str, strlen(str) - 1);
	else
	    return rep_null_string ();
    }
}

DEFUN("time-later-p", Ftime_later_p, Stime_later_p, (repv t1, repv t2), rep_Subr2) /*
::doc:rep.system#time-later-p::
time-later-p TIME-STAMP1 TIME-STAMP2

Returns t when TIME-STAMP1 refers to a later time than TIME-STAMP2.
::end:: */
{
    unsigned long time1, time2;
    rep_DECLARE1(t1, rep_TIMEP);
    rep_DECLARE2(t2, rep_TIMEP);
    time1 = rep_GET_TIME(t1);
    time2 = rep_GET_TIME(t2);
    return time1 > time2 ? Qt : Qnil;
}

DEFUN("sleep-for", Fsleep_for, Ssleep_for, (repv secs, repv msecs),
      rep_Subr2) /*
::doc:rep.system#sleep-for::
sleep-for SECONDS [MILLISECONDS]

Pause for SECONDS (plus the optional MILLISECOND component) length of time.
::end:: */
{
    rep_DECLARE1(secs, rep_NUMERICP);
    rep_DECLARE2_OPT(msecs, rep_NUMERICP);
    rep_sleep_for(rep_get_long_int (secs), rep_get_long_int (msecs));
    return Qt;
}

DEFUN("sit-for", Fsit_for, Ssit_for, (repv secs, repv msecs),
      rep_Subr2) /*
::doc:rep.system#sit-for::
sit-for [SECONDS] [MILLISECONDS]

Wait for input to arrive and be processed. No more than SECONDS seconds plus
MILLISECONDS milliseconds will be waited. If at the end of this time no
input has arrived, return t. Otherwise return nil if input was found.

If neither SECONDS nor MILLISECONDS is defined the command will return
immediately, using a null timeout.
::end:: */
{
    rep_DECLARE1_OPT(secs, rep_NUMERICP);
    rep_DECLARE2_OPT(msecs, rep_NUMERICP);
    return rep_sit_for(((rep_get_long_int (secs)) * 1000)
		       + rep_get_long_int (msecs));
}

DEFUN("user-login-name", Fuser_login_name, Suser_login_name, (void), rep_Subr0) /*
::doc:rep.system#user-login-name::
user-login-name

Returns the login name of the user (a string).
::end:: */
{
    return rep_user_login_name();
}

DEFUN("user-full-name", Fuser_full_name, Suser_full_name, (repv arg), rep_Subr1) /*
::doc:rep.system#user-full-name::
user-full-name [REAL-NAME]

Returns the real name of the user (a string). If REAL-NAME is non-nil, it's
the name to return in subsequent calls.
::end:: */
{
    static repv saved_name;
    rep_DECLARE1_OPT (arg, rep_STRINGP);
    if(arg != Qnil)
    {
	if(!saved_name)
	    rep_mark_static(&saved_name);
	saved_name = arg;
    }
    return saved_name ? saved_name : rep_user_full_name();
}

DEFUN("user-home-directory", Fuser_home_directory,
      Suser_home_directory, (repv user), rep_Subr1) /*
::doc:rep.system#user-home-directory::
user-home-directory [USER]

Return the path to USER's home directory (a string). When USER is undefined
the directory of the user who executed Jade is found.
::end:: */
{
    rep_DECLARE1_OPT(user, rep_STRINGP);
    return rep_user_home_directory(user);
}

DEFUN("system-name", Fsystem_name, Ssystem_name, (void), rep_Subr0) /*
::doc:rep.system#system-name::
system-name

Returns the name of the host which the editor is running on.
::end:: */
{
    return rep_system_name();
}

DEFUN("message", Fmessage, Smessage, (repv string, repv now), rep_Subr2) /*
::doc:rep.system#message::
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

DEFUN("translate-string", Ftranslate_string, Stranslate_string, (repv string, repv table), rep_Subr2) /*
::doc:rep.data#translate-string:
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
    register unsigned char *str;
    rep_DECLARE1(string, rep_STRINGP);
    rep_DECLARE2(table, rep_STRINGP);
    tablen = rep_STRING_LEN(table);
    if(!rep_STRING_WRITABLE_P(string))
	return(rep_signal_arg_error(string, 1));
    str = (unsigned char *)rep_STR(string);
    slen = rep_STRING_LEN(string);
    while(slen-- > 0)
    {
	register unsigned char c = *str;
	*str++ = (c < tablen) ? ((unsigned char *)rep_STR(table))[c] : c;
    }
    rep_string_modified (string);
    return(string);
}

DEFUN("alpha-char-p", Falpha_char_p, Salpha_char_p, (repv ch), rep_Subr1) /*
::doc:rep.data#alpha-char-p::
alpha-char-p CHAR

Returns t if CHAR is an alphabetic character.
::end:: */
{
    return (rep_INTP(ch) && isalpha(rep_INT(ch))) ? Qt : Qnil;
}

DEFUN("upper-case-p", Fupper_case_p, Supper_case_p, (repv ch), rep_Subr1) /*
::doc:rep.data#upper-case-p::
upper-case-p CHAR

Returns t if CHAR is upper case.
::end:: */
{
    return (rep_INTP(ch) && isupper(rep_INT(ch))) ? Qt : Qnil;
}

DEFUN("lower-case-p", Flower_case_p, Slower_case_p, (repv ch), rep_Subr1) /*
::doc:rep.data#lower-case-p::
lower-case-p CHAR

Returns t if CHAR is lower case.
::end:: */
{
    return (rep_INTP(ch) && islower(rep_INT(ch))) ? Qt : Qnil;
}

DEFUN("digit-char-p", Fdigit_char_p, Sdigit_char_p, (repv ch), rep_Subr1) /*
::doc:rep.data#digit-char-p::
digit-char-p CHAR

Returns t if CHAR is a digit.
::end:: */
{
    return (rep_INTP(ch) && isdigit(rep_INT(ch))) ? Qt : Qnil;
}

DEFUN("alphanumericp", Falphanumericp, Salphanumericp, (repv ch), rep_Subr1) /*
::doc:rep.data#alphanumericp::
alphanumericp CHAR

Returns t if CHAR is alpha-numeric.
::end:: */
{
    return (rep_INTP(ch) && isalnum(rep_INT(ch))) ? Qt : Qnil;
}

DEFUN("space-char-p", Fspace_char_p, Sspace_char_p, (repv ch), rep_Subr1) /*
::doc:rep.data#space-char-p::
space-char-p CHAR

Returns t if CHAR is whitespace.
::end:: */
{
    return (rep_INTP(ch) && isspace(rep_INT(ch))) ? Qt : Qnil;
}

DEFUN("char-upcase", Fchar_upcase, Schar_upcase, (repv ch), rep_Subr1) /*
::doc:rep.data#char-upcase::
char-upcase CHAR

Returns the upper-case equivalent of CHAR.
::end:: */
{
    rep_DECLARE1(ch, rep_INTP);
    return rep_MAKE_INT(toupper(rep_INT(ch)));
}

DEFUN("char-downcase", Fchar_downcase, Schar_downcase, (repv ch), rep_Subr1) /*
::doc:rep.data#char-downcase::
char-downcase CHAR

Returns the lower-case equivalent of CHAR.
::end:: */
{
    rep_DECLARE1(ch, rep_INTP);
    return rep_MAKE_INT(tolower(rep_INT(ch)));
}

DEFUN_INT("system", Fsystem, Ssystem, (repv command), rep_Subr1, "sShell command:") /*
::doc:rep.system#system::
system SHELL-COMMAND

Synchronously execute the shell command string SHELL-COMMAND. Returns the
exit status of the command, or signals an error if the shell couldn't
be started.

Note that the exit status is _not_ the same as the return code. It
depends on the operating system, but under unix the return code may be
found by right-shifting the exit status by eight bits. Low non-zero
values represent that the process was killed by a signal.
::end:: */
{
    rep_DECLARE1(command, rep_STRINGP);
    return rep_system (rep_STR (command));
}

DEFUN("get-command-line-option", Fget_command_line_option,
      Sget_command_line_option, (repv opt, repv arg), rep_Subr2) /*
::doc:rep.system#get-command-line-option::
get-command-line-option OPTION [REQUIRES-ARGUMENT]

Returns t if OPTION was specified on the command line (OPTION is typically
a word beginning with `--'). If REQUIRES-ARGUMENT is non-nil, this option
requires a parameter, the value of which is returned. If a parameters isn't
supplied an error is signalled.
::end:: */
{
    repv param = Qt;
    rep_DECLARE1(opt, rep_STRINGP);
    if (rep_get_option (rep_STR(opt), (arg == Qnil) ? 0 : &param))
	return param;
    else
	return Qnil;
}

DEFUN ("crypt", Fcrypt, Scrypt, (repv key, repv salt), rep_Subr2) /*
::doc:rep.system#crypt::
crypt KEY SALT

The `crypt' function takes a password, KEY, as a string, and a SALT
character array, and returns a printable ASCII string which starts with
another salt.  It is believed that, given the output of the function,
the best way to find a KEY that will produce that output is to guess
values of KEY until the original value of KEY is found.

See crypt(3) for more information.
::end:: */
{
    const char *output;

    rep_DECLARE1 (key, rep_STRINGP);
    rep_DECLARE2 (salt, rep_STRINGP);

#ifdef HAVE_CRYPT
    output = crypt (rep_STR (key), rep_STR (salt));
    return rep_string_dup (output);
#else
    { DEFSTRING (err, "crypt () isn't supported on this system");
      return Fsignal (Qerror, rep_LIST_1 (rep_VAL (&err))); }
#endif    
}

void
rep_misc_init(void)
{
    int i;
    repv tem;

    if (rep_beep_fun == 0)
	rep_beep_fun = default_beep;

    tem = rep_push_structure ("rep.system");

    rep_INTERN(operating_system);
#ifdef rep_HAVE_UNIX
    rep_INTERN(unix);
    Fset (Qoperating_system, Qunix);
#endif

    rep_INTERN_SPECIAL(process_environment);
    Fset (Qprocess_environment, Qnil);

    rep_INTERN(rep_version);
    Fset (Qrep_version, rep_VAL(&rep_version_string));
    rep_INTERN(rep_interface_id);
    Fset (Qrep_interface_id, rep_VAL(rep_MAKE_INT(rep_INTERFACE)));
    rep_INTERN(rep_build_id);
    Fset (Qrep_build_id, rep_VAL(&build_id_string));

    rep_ADD_SUBR_INT(Sbeep);
    rep_ADD_SUBR(Scurrent_time);
    rep_ADD_SUBR(Scurrent_utime);
    rep_ADD_SUBR(Sfix_time);
    rep_ADD_SUBR(Scurrent_time_string);
    rep_ADD_SUBR(Stime_later_p);
    rep_ADD_SUBR(Ssleep_for);
    rep_ADD_SUBR(Ssit_for);
    rep_ADD_SUBR(Sget_command_line_option);
    rep_ADD_SUBR(Scrypt);
    rep_ADD_SUBR_INT(Ssystem);
    rep_ADD_SUBR(Suser_login_name);
    rep_ADD_SUBR(Suser_full_name);
    rep_ADD_SUBR(Suser_home_directory);
    rep_ADD_SUBR(Ssystem_name);
    rep_ADD_SUBR(Smessage);

    rep_pop_structure (tem);

    tem = rep_push_structure ("rep.data");
    rep_ADD_SUBR(Stranslate_string);
    rep_ADD_SUBR(Salpha_char_p);
    rep_ADD_SUBR(Supper_case_p);
    rep_ADD_SUBR(Slower_case_p);
    rep_ADD_SUBR(Sdigit_char_p);
    rep_ADD_SUBR(Salphanumericp);
    rep_ADD_SUBR(Sspace_char_p);
    rep_ADD_SUBR(Schar_upcase);
    rep_ADD_SUBR(Schar_downcase);
    rep_ADD_SUBR(Scomplete_string);
    {
	repv up = rep_make_string (257);
	repv down = rep_make_string (257);

	for(i = 0; i < 256; i++)
	{
	    ((unsigned char *)rep_STR(up))[i] = toupper(i);
	    ((unsigned char *)rep_STR(down))[i] = tolower(i);
	}
	rep_STR(up)[256] = 0;
	rep_STR(down)[256] = 0;

	rep_INTERN(upcase_table);
	rep_INTERN(downcase_table);
	Fset (Qupcase_table, up);
	Fset (Qdowncase_table, down);
    }

    {
	repv flatten = rep_make_string (12);

	for(i = 0; i < 10; i++)
	    ((unsigned char *)rep_STR(flatten))[i] = i;
	rep_STR(flatten)[10] = ' ';
	rep_STR(flatten)[11] = 0;

	rep_INTERN(flatten_table);
	Fset (Qflatten_table, flatten);
    }
    rep_pop_structure (tem);
}
