/* repdoc.c -- Program to strip doc-strings from C source
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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include <string.h>
#include <gdbm.h>
#include <fcntl.h>
#include <stdlib.h>

#ifndef GDBM_NOLOCK
# define GDBM_NOLOCK 0
#endif

static void
usage(void)
{
    fputs("usage: repdoc doc-file [src-files...]\n", stderr);
    exit(1);
}

static void
scanfile(FILE *src, GDBM_FILE sdbm)
{
    char buf[512];
    while(fgets(buf, 512, src))
    {
	char *start = strstr(buf, "::doc:");
	if(start)
	{
	    datum key, value;
	    char buf[16384];		/* so lazy.. */
	    char *out = buf;

	    char *id = start + 6;
	    start = strstr (id, "::");
	    if (start == 0)
		continue;
	    *start = 0;

	    while(fgets(out, sizeof (buf) - (out - buf), src))
	    {
		char *end = strstr (out, "::end::");
		if (end != 0)
		    break;
		out += strlen(out);
	    }
	    /* ignore trailing newline */
	    if (out > buf)
		out--;
	    *out = 0;

	    key.dptr = id;
	    key.dsize = strlen(id);
            value.dptr = buf;
	    value.dsize = strlen(buf);
	    if (gdbm_store (sdbm, key, value, GDBM_REPLACE) < 0)
		perror ("gdbm_store");
	}
    }
}

int
main(int ac, char **av)
{
    GDBM_FILE docdbm;
    ac--;
    av++;
    if(ac < 2)
	usage();
    docdbm = gdbm_open(*av++, 0, GDBM_WRCREAT | GDBM_NOLOCK, 0666, 0);
    ac--;
    if(docdbm == 0)
    {
	fprintf(stderr, "can't open output files.\n");
	exit(2);
    }
    if(!ac)
	scanfile(stdin, docdbm);
    else
    {
	while(ac)
	{
	    FILE *file = fopen(*av, "r");
	    if(file)
	    {
		scanfile(file, docdbm);
		fclose(file);
	    }
	    ac--;
	    av++;
	}
    }
    return 0;
}
