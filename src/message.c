/* message.c -- 
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "rep.h"
#include <stdio.h>

static void
default_message (enum rep_message fn, ...)
{
    va_list args;
    va_start (args, fn);
    switch (fn)
    {
	int len;
	u_char *msg;
	u_long *old_lenp;
	u_char **old_msgp;

    case rep_messagen:
	msg = (u_char *)va_arg(args, u_char *);
	len = (int)va_arg(args, int);
	fwrite(msg, 1, len, stderr);
	fputc('\n', stderr);
	break;

    case rep_message:
	msg = (u_char *)va_arg(args, u_char *);
	fputs (msg, stderr);
	fputc ('\n', stderr);
	break;

    case rep_messagef:
	msg = (u_char *)va_arg(args, u_char *);
	vfprintf (stderr, msg, args);
	fputc ('\n', stderr);
	break;

    case rep_save_message:
	old_msgp = (u_char **)va_arg(args, u_char **);
	old_lenp = (u_long *)va_arg(args, u_long *);
	*old_msgp = ""; *old_lenp = 0;
	break;

    case rep_append_message:
	msg = (u_char *)va_arg(args, u_char *);
	len = (int)va_arg(args, int);
	fwrite(msg, len, 1, stderr);
	fputc('\n', stderr);
	break;

    case rep_reset_message: 		/* (void) */
    case rep_restore_message:		/* (u_char *msg, u_long len) */
    case rep_redisplay_message:		/* (void) */
    }
}

void (*rep_message_fun)(enum rep_message fn, ...) = default_message;
