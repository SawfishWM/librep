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
#include "jade_protos.h"
#include "revision.h"

#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>

_PR VALUE concat2(u_char *, u_char *);
_PR VALUE concat3(u_char *, u_char *, u_char *);
_PR void misc_init(void);

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

VALUE
concat2(u_char *s1, u_char *s2)
{
    int len = strlen(s1) + strlen(s2);
    VALUE res = make_string(len + 1);
    stpcpy(stpcpy(VSTR(res), s1), s2);
    return(res);
}
VALUE
concat3(u_char *s1, u_char *s2, u_char *s3)
{
    int len = strlen(s1) + strlen(s2) + strlen(s3);
    VALUE res = make_string(len + 1);
    stpcpy(stpcpy(stpcpy(VSTR(res), s1), s2), s3);
    return(res);
}

_PR VALUE cmd_file_name_concat(VALUE args);
DEFUN("file-name-concat", cmd_file_name_concat, subr_file_name_concat, (VALUE args), V_SubrN, DOC_file_name_concat) /*
::doc:file_name_concat::
file-name-concat PARTS...

Returns a string made from all the PARTS, each of which is one component of
a file name. Add's `/' characters between each PART if necessary.
::end:: */
{
    if(CONSP(args) && STRINGP(VCAR(args)))
    {
	u_char buf[512];
	strcpy(buf, VSTR(VCAR(args)));
	args = VCDR(args);
	while(CONSP(args) && STRINGP(VCAR(args)))
	{
	    if(add_file_part(buf, VSTR(VCAR(args)), 512) == 0)
		return(NULL);
	    args = VCDR(args);
	}
	return(string_dup(buf));
    }
    return(null_string);
}

_PR VALUE cmd_expand_file_name(VALUE name, VALUE full);
DEFUN("expand-file-name", cmd_expand_file_name, subr_expand_file_name, (VALUE name, VALUE full), V_Subr2, DOC_expand_file_name) /*
::doc:expand_file_name::
expand-file-name FILENAME [FULLY-QUALIFY]

Returns a valid file name from the string FILENAME. Currently this just expands
any `~' characters it finds (if we're running on Unix) to the user's home
directory.

If FULLY-QUALIFY is t then the FILENAME is made absolute
::end:: */
{
    DECLARE1(name, STRINGP);
    name = sys_expand_file_name(name);
    if(!NILP(full))
	name = sys_fully_qualify_file_name(name);
    return(name);
}

_PR VALUE cmd_system(VALUE command);
DEFUN_INT("system", cmd_system, subr_system, (VALUE command), V_Subr1, DOC_system, "sShell command:") /*
::doc:system::
system SHELL-COMMAND

Tells the operating-system to execute SHELL-COMMAND, returns the exit code
of that command.
::end:: */
{
    DECLARE1(command, STRINGP);
    return(make_number(system(VSTR(command))));
}

_PR VALUE cmd_substring(VALUE string, VALUE start, VALUE end);
DEFUN("substring", cmd_substring, subr_substring, (VALUE string, VALUE start, VALUE end), V_Subr3, DOC_substring) /*
::doc:substring::
substring STRING START [END]

Returns the portion of STRING starting at character number START and ending
at the character before END (or the end of the string is END is not given).
All indices start at zero.
::end:: */
{
    int slen;
    DECLARE1(string, STRINGP);
    DECLARE2(start, NUMBERP);
    slen = STRING_LEN(string);
    if(VNUM(start) > slen)
	return(signal_arg_error(start, 2));
    if(NUMBERP(end))
    {
	if((VNUM(end) > slen) || (VNUM(end) < VNUM(start)))
	    return(signal_arg_error(end, 3));
	return(string_dupn(VSTR(string) + VNUM(start), VNUM(end) - VNUM(start)));
    }
    else
	return(string_dupn(VSTR(string) + VNUM(start), slen - VNUM(start)));
}

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

_PR VALUE cmd_file_name_nondirectory(VALUE file);
DEFUN("file-name-nondirectory", cmd_file_name_nondirectory, subr_file_name_nondirectory, (VALUE file), V_Subr1, DOC_file_name_nondirectory) /*
::doc:file_name_nondirectory::
file-name-nondirectory FILE-NAME

Returns the nondirectory part of FILE-NAME.
::end:: */
{
    DECLARE1(file, STRINGP);
    return(string_dup(file_part(VSTR(file))));
}

_PR VALUE cmd_file_name_directory(VALUE file);
DEFUN("file-name-directory", cmd_file_name_directory, subr_file_name_directory, (VALUE file), V_Subr1, DOC_file_name_directory) /*
::doc:file_name_directory::
file-name-directory FILE-NAME

Returns the directory part of FILE-NAME.
::end:: */
{
    int len;
    DECLARE1(file, STRINGP);
    len = file_part(VSTR(file)) - VSTR(file);
    return(string_dupn(VSTR(file), len));
}

_PR VALUE cmd_balance_brackets(VALUE open, VALUE close, VALUE string);
DEFUN("balance-brackets", cmd_balance_brackets, subr_balance_brackets, (VALUE open, VALUE close, VALUE string), V_Subr3, DOC_balance_brackets) /*
::doc:balance_brackets::
balance-brackets OPEN-STRING CLOSE-STRING STRING
::end:: */
{
    int cnt = 0;
    u_char *s;
    DECLARE1(open, STRINGP);
    DECLARE2(close, STRINGP);
    DECLARE3(string, STRINGP);
    s = VSTR(string) - 1;
    while((s = strpbrk(s + 1, VSTR(open))))
	cnt++;
    s = VSTR(string) - 1;
    while((s = strpbrk(s + 1, VSTR(close))))
	cnt--;
    return(make_number(cnt));
}

_PR VALUE cmd_strtoc(VALUE string);
DEFUN("strtoc", cmd_strtoc, subr_strtoc, (VALUE string), V_Subr1, DOC_strtoc) /*
::doc:strtoc::
strtoc STRING

Returns the first character of STRING.
::end:: */
{
    DECLARE1(string, STRINGP);
    return(make_number((long)*VSTR(string)));
}

_PR VALUE cmd_ctostr(VALUE ch);
DEFUN("ctostr", cmd_ctostr, subr_ctostr, (VALUE ch), V_Subr1, DOC_ctostr) /*
::doc:ctostr::
ctostr CHAR

Returns a one-character string containing CHAR.
::end:: */
{
    u_char tmp[2];
    DECLARE1(ch, CHARP);
    tmp[0] = (u_char)VCHAR(ch);
    tmp[1] = 0;
    return(string_dup(tmp));
}

_PR VALUE cmd_amiga_p(void);
DEFUN("amiga-p", cmd_amiga_p, subr_amiga_p, (void), V_Subr0, DOC_amiga_p) /*
::doc:amiga_p::
amiga-p

t if running on an Amiga.
::end:: */
{
#ifdef HAVE_AMIGA
    return(sym_t);
#else
    return(sym_nil);
#endif
}
_PR VALUE cmd_x11_p(void);
DEFUN("x11-p", cmd_x11_p, subr_x11_p, (void), V_Subr0, DOC_x11_p) /*
::doc:x11_p::
x11-p

t if running on the X Window System V11.
::end:: */
{
#ifdef HAVE_X11
    return(sym_t);
#else
    return(sym_nil);
#endif
}
_PR VALUE cmd_unix_p(void);
DEFUN("unix-p", cmd_unix_p, subr_unix_p, (void), V_Subr0, DOC_unix_p) /*
::doc:unix_p::
unix-p

t if running under some flavour of unix.
::end:: */
{
#ifdef HAVE_UNIX
    return(sym_t);
#else
    return(sym_nil);
#endif
}

_PR VALUE cmd_tmp_file_name(void);
DEFUN("tmp-file-name", cmd_tmp_file_name, subr_tmp_file_name, (void), V_Subr0, DOC_tmp_file_name) /*
::doc:tmp_file_name::
tmp-file-name

Returns the name of a unique file.
::end:: */
{
    return(string_dup(tmpnam(NULL)));
}

_PR VALUE cmd_make_completion_string(VALUE args);
DEFUN("make-completion-string", cmd_make_completion_string, subr_make_completion_string, (VALUE args), V_SubrN, DOC_make_completion_string) /*
::doc:make_completion_string::
make-completion-string EXISTING [POSSIBLE | POSIIBLE...]
::end:: */
{
    u_char *orig, *match = NULL;
    int matchlen = 0, origlen;
    VALUE arg;
    if(!CONSP(args))
	return(NULL);
    arg = VCAR(args);
    DECLARE1(arg, STRINGP);
    orig = VSTR(arg);
    origlen = strlen(orig);
    arg = ARG2;
    switch(VTYPE(arg))
    {
    case V_Cons:
	args = arg;
	break;
    case V_StaticString:
    case V_DynamicString:
	args = VCDR(args);
	break;
    default:
	return(sym_nil);
    }
    while(CONSP(args))
    {
	arg = VCAR(args);
	if(STRINGP(arg))
	{
	    u_char *tmp = VSTR(arg);
	    if(mystrcmp(orig, tmp))
	    {
		if(match)
		{
		    u_char *tmp2 = match + origlen;
		    tmp += origlen;
		    while(*tmp2 && *tmp)
		    {
			if(*tmp2++ != *tmp++)
			{
			    tmp2--;
			    break;
			}
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
	args = VCDR(args);
    }
    if(match)
	return(string_dupn(match, matchlen));
    return(sym_nil);
}

_PR VALUE cmd_current_time(void);
DEFUN("current-time", cmd_current_time, subr_current_time, (void), V_Subr0, DOC_current_time) /*
::doc:current_time::
current-time

Return some number denoting the current system time.
::end:: */
{
    return(make_number(sys_time()));
}

_PR VALUE cmd_current_time_string(void);
DEFUN("current-time-string", cmd_current_time_string, subr_current_time_string, (void), V_Subr0, DOC_current_time_string) /*
::doc:current_time_string::
current-time-string

Returns a human-readable string defining the current date and time.
::end:: */
{
    char *str;
    time_t tim;
    time(&tim);
    str = ctime(&tim);
    if(str)
	return(string_dupn(str, strlen(str) - 1));
    return(NULL);
}

_PR VALUE cmd_major_version_number(void);
DEFUN("major-version-number", cmd_major_version_number, subr_major_version_number, (void), V_Subr0, DOC_major_version_number) /*
::doc:major_version_number::
major-version-number
::end:: */
{
    static VALUE major_version_number;
    if(!major_version_number)
    {
	major_version_number = make_number(MAJOR);
	mark_static(&major_version_number);
    }
    return(major_version_number);
}

_PR VALUE cmd_minor_version_number(void);
DEFUN("minor-version-number", cmd_minor_version_number, subr_minor_version_number, (void), V_Subr0, DOC_minor_version_number) /*
::doc:minor_version_number::
minor-version-number
::end:: */
{
    static VALUE minor_version_number;
    if(!minor_version_number)
    {
	minor_version_number = make_number(MINOR);
	mark_static(&minor_version_number);
    }
    return(minor_version_number);
}

_PR VALUE cmd_getenv(VALUE name);
DEFUN("getenv", cmd_getenv, subr_getenv, (VALUE name), V_Subr1, DOC_getenv) /*
::doc:getenv::
getenv NAME

Returns the value of environment variable NAME as a string, or nil if it is
undefined.
::end:: */
{
    char *val;
    DECLARE1(name, STRINGP);
    val = getenv(VSTR(name));
    if(val)
	return(string_dup(val));
    return(sym_nil);
}

void
misc_init(void)
{
    ADD_SUBR(subr_file_name_concat);
    ADD_SUBR(subr_expand_file_name);
    ADD_SUBR(subr_system);
    ADD_SUBR(subr_substring);
    ADD_SUBR(subr_beep);
    ADD_SUBR(subr_file_name_nondirectory);
    ADD_SUBR(subr_file_name_directory);
    ADD_SUBR(subr_balance_brackets);
    ADD_SUBR(subr_strtoc);
    ADD_SUBR(subr_ctostr);
    ADD_SUBR(subr_amiga_p);
    ADD_SUBR(subr_x11_p);
    ADD_SUBR(subr_unix_p);
    ADD_SUBR(subr_tmp_file_name);
    ADD_SUBR(subr_make_completion_string);
    ADD_SUBR(subr_current_time);
    ADD_SUBR(subr_current_time_string);
    ADD_SUBR(subr_major_version_number);
    ADD_SUBR(subr_minor_version_number);
    ADD_SUBR(subr_getenv);
}
