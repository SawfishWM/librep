/* gettext.c -- wrap some i18n functions when available
   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define _GNU_SOURCE

#include <config.h>
#include <rep.h>

#ifdef LIBC_GETTEXT
# define gnu_gettext gettext
# define gnu_textdomain textdomain
# define gnu_bindtextdomain bindtextdomain
#endif

extern char *gnu_gettext (const char *msgid);
extern char *gnu_textdomain (const char *domainname);
extern char *gnu_bindtextdomain (const char *domainname, const char *dirname);

DEFSYM(gettext, "gettext");

DEFUN("_", Fgettext, Sgettext, (repv in), rep_Subr1)
{
    char *out;
    rep_DECLARE1(in, rep_STRINGP);

    out = gnu_gettext (rep_STR(in));
    if (out == 0 || (u_char *) out == rep_STR(in))
	return in;
    else
	return rep_string_dup (out);
}

DEFUN("bindtextdomain", Fbindtextdomain,
      Sbindtextdomain, (repv dom, repv dir), rep_Subr2)
{
    char *domainname = 0, *dirname = 0, *out;

    if (rep_STRINGP(dom))
	domainname = rep_STR(dom);
    if (rep_STRINGP(dir))
	dirname = rep_STR(dir);

    out = gnu_bindtextdomain (domainname, dirname);
    return out ? rep_string_dup (out) : Qnil;
}

DEFUN("textdomain", Ftextdomain, Stextdomain, (repv dom), rep_Subr1)
{
    char *domainname = 0, *out;

    if (rep_STRINGP(dom))
	domainname = rep_STR(dom);

    out = gnu_textdomain (domainname);
    return out ? rep_string_dup (out) : Qnil;
}



/* DL hooks */

repv
rep_dl_init(void)
{
    rep_ADD_SUBR(Sgettext);
    rep_ADD_SUBR(Sbindtextdomain);
    rep_ADD_SUBR(Stextdomain);

    rep_INTERN(gettext);
    return Qgettext;
}
