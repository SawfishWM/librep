/* unix_files.c -- Built-in file handler functions for Unix-like files
   Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
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

#define _GNU_SOURCE

#include "repint.h"

#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#if HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif

#ifndef PATH_MAX
# define PATH_MAX 256
#endif

#ifndef S_ISLNK
#define S_ISLNK(mode)  (((mode) & S_IFMT) == S_IFLNK)
#endif

#ifndef S_ISSOCK
#define S_ISSOCK(mode)  (((mode) & S_IFMT) == S_IFSOCK)
#endif


/* Support functions */

DEFSTRING(dot, ".");

static inline char *
file_part(char *name)
{
    char *tmp = strrchr(name, '/');
    return tmp != 0 ? tmp + 1 : name;
}

static struct stat *
stat_file(repv file)
{
    static struct stat statbuf;
    if(stat(rep_STR(file), &statbuf) == 0)
	return &statbuf;
    else
	return 0;
}

unsigned long
rep_file_length(repv file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return st->st_size;
    else
	return 0;
}


/* File ops */

repv
rep_file_name_absolute_p(repv file)
{
    return (((rep_STR(file)[0] == '/') || (rep_STR(file)[0] == '~'))
	    ? Qt : Qnil);
}

repv
rep_expand_file_name(repv file)
{
    char buf[PATH_MAX];
    char *optr = buf;
    char *iptr = rep_STR(file);

    while(*iptr != 0)
    {
	char *end;

	if(iptr[0] == '.')
	{
	    if(iptr[1] == '/')
	    {
		iptr += 1;
		goto strip;
	    }
	    else if(iptr[1] == 0)
	    {
		if(optr == buf)
		    /* Only character in string. Must preserve the dot. */
		    *optr++ = '.';
		iptr++;
		continue;
	    }
	    else if(iptr[1] == '.' && (iptr[2] == '/' || iptr[2] == 0))
	    {
		/* `XXX/..[/]' Try to back up over the parent directory */

		char *back = optr;
		rep_bool all_dots = rep_TRUE;
		char *end;

		/* Step over any contiguous `/' characters */
		while(back > buf && back[-1] == '/')
		    back--;
		end = back;

		/* Step over any non-`/' characters */
		while(back > buf && back[-1] != '/')
		{
		    back--;
		    if (back[0] != '.')
			all_dots = rep_FALSE;
		}

		if(back < optr && back >= buf && *back != '/'
		   /* Don't allow `../..' -> `' */
		   && (!all_dots || end - back != 2))
		{
		    /* Reset the output ptr to the end of the parent */
		    optr = back;
		}
		/* Check for `/..' */
		else if (all_dots && end == back
			 && back == buf && optr > buf
			 && buf[0] == '/' && optr - end == 1)
		{
		    optr = back + 1;
		}
		else
		{
		    /* Can't move up; leave the .. in the file name */
		    *optr++ = '.';
		    *optr++ = '.';
		    if(iptr[2] == '/')
			*optr++ = '/';
		}
		iptr += (iptr[2] == 0) ? 2 : 3;
		goto strip;
	    }
	}
	end = strchr(iptr, '/');
	if(end == 0)
	    end = iptr + strlen(iptr);
	memcpy(optr, iptr, end - iptr);
	optr += end - iptr;
	iptr = end;

	if(*iptr == '/')
	    *optr++ = *iptr++;

    strip:
	/* merge multiple slashes into one */
	while (*iptr && *iptr == '/')
	    iptr++;
    }

    /* Don't allow a fully-empty string to be returned */
    if (optr - buf == 0)
	*optr++ = '.';

    if(optr - buf != rep_STRING_LEN(file)
       || memcmp(rep_STR(file), buf, optr - buf) != 0)
	return rep_string_dupn(buf, optr - buf);
    else
	return file;
}

repv
rep_canonical_file_name(repv file)
{
    char buf[PATH_MAX];
    int len;

    if(realpath(rep_STR(file), buf) == 0)
    {
	/* realpath () failed; copy the source */
	strncpy (buf, rep_STR (file), sizeof (buf));
    }

    len = strlen(buf);
    while (len > 0 && buf[len - 1] == '/')
    {
	buf[len - 1] = 0;
	len--;
    }
    return rep_string_dupn(buf, len);
}

repv
rep_file_name_nondirectory(repv file)
{
    char *tem = file_part(rep_STR(file));
    return tem == rep_STR(file) ? file : rep_string_dup(tem);
}

repv
rep_file_name_directory(repv file)
{
    int len = file_part(rep_STR(file)) - rep_STR(file);
    return rep_string_dupn(rep_STR(file), len);
}

repv
rep_file_name_as_directory(repv file)
{
    int len = rep_STRING_LEN(file);
    if(file_part(rep_STR(file)) == rep_STR(file) + len)
    {
	/* It's already a directory */
	return file;
    }
    else
    {
	repv new = rep_string_dupn(rep_STR(file), len + 1);
	if(new)
	{
	    rep_STR(new)[len] = '/';
	    rep_STR(new)[len+1] = 0;
	}
	return new;
    }
}

repv
rep_directory_file_name(repv file)
{
    int len = rep_STRING_LEN(file);
    if(file_part(rep_STR(file)) != rep_STR(file) + len)
    {
	/* There's a file part. Just return the initial string? */
	return file;
    }
    else
    {
	if(len == 0)
	    return rep_VAL(&dot);
	else if(len == 1)
	    return file;
	else
	    /* Chop the trailing "/" */
	    return rep_string_dupn(rep_STR(file), len - 1);
    }
}

repv
rep_delete_file(repv file)
{
    if(unlink(rep_STR(file)) == 0)
	return Qt;
    else
	return rep_signal_file_error(file);
}

repv
rep_rename_file(repv old, repv new)
{
    if(rename(rep_STR(old), rep_STR(new)) != -1)
	return Qt;
    else
	return rep_signal_file_error(rep_list_2(old, new));
}

repv
rep_make_directory(repv dir)
{
    int len = rep_STRING_LEN(dir);

    /* Trim trailing '/' to mkdir(2) since some OSes fail the call otherwise */
    if (*(rep_STR(dir) + len - 1) == '/')
	dir = rep_string_dupn(rep_STR(dir), len - 1);

    if(mkdir(rep_STR(dir), S_IRWXU | S_IRWXG | S_IRWXO) == 0)
	return Qt;
    else
	return rep_signal_file_error(dir);
}

repv
rep_delete_directory(repv dir)
{
    if(rmdir(rep_STR(dir)) == 0)
	return Qt;
    else
	return rep_signal_file_error(dir);
}

repv
rep_copy_file(repv src, repv dst)
{
    repv res = Qt;
    int srcf;
    srcf = open(rep_STR(src), O_RDONLY);
    if(srcf != -1)
    {
	int dstf = open(rep_STR(dst), O_WRONLY | O_CREAT | O_TRUNC, 0666);
	if(dstf != -1)
	{
	    struct stat statb;
	    int rd;
	    if(fstat(srcf, &statb) == 0)
		chmod(rep_STR(dst), statb.st_mode);
	    do {
		char buf[BUFSIZ];
		int wr;
		rd = read(srcf, buf, BUFSIZ);
		if(rd < 0)
		{
		    res = rep_signal_file_error(src);
		    break;
		}
		wr = write(dstf, buf, rd);
		if(wr != rd)
		{
		    res = rep_signal_file_error(dst);
		    break;
		}
	    } while(rd != 0);
	    close(dstf);
	}
	else
	    res = rep_signal_file_error(dst);
	close(srcf);
    }
    else
	res = rep_signal_file_error(src);
    return res;
}

repv
rep_file_readable_p(repv file)
{
    return access(rep_STR(file), R_OK) == 0 ? Qt : Qnil;
}

repv
rep_file_writable_p(repv file)
{
    return access(rep_STR(file), W_OK) == 0 ? Qt : Qnil;
}

repv
rep_file_executable_p(repv file)
{
    return access(rep_STR(file), X_OK) == 0 ? Qt : Qnil;
}

repv
rep_file_exists_p(repv file)
{
    return access(rep_STR(file), F_OK) == 0 ? Qt : Qnil;
}

repv
rep_file_regular_p(repv file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return S_ISREG(st->st_mode) ? Qt : Qnil;
    else
	return Qnil;
}

repv
rep_file_directory_p(repv file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return S_ISDIR(st->st_mode) ? Qt : Qnil;
    else
	return Qnil;
}

repv
rep_file_symlink_p(repv file)
{
    struct stat st;
    if(lstat(rep_STR(file), &st) == 0)
	return S_ISLNK(st.st_mode) ? Qt : Qnil;
    else
	return Qnil;
}

repv
rep_file_owner_p(repv file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return ((st->st_uid == geteuid() && st->st_gid == getegid())
		? Qt : Qnil);
    else
	return Qnil;
}

repv
rep_file_gid_p(repv file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return rep_MAKE_INT(st->st_gid & 07777); 
    else
	return Qnil;
}

repv
rep_file_uid_p(repv file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	 return rep_MAKE_INT(st->st_uid & 07777);
    else
	 return Qnil;
}

repv
rep_file_nlinks(repv file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return rep_MAKE_INT(st->st_nlink);
    else
	return Qnil;
}

repv
rep_file_size(repv file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return rep_make_long_uint(st->st_size);
    else
	return Qnil;
}

repv
rep_file_modes(repv file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return rep_MAKE_INT(st->st_mode & 07777);
    else
	return Qnil;
}

repv
rep_set_file_modes(repv file, repv modes)
{
    rep_DECLARE2(modes, rep_INTP);
    if(chmod(rep_STR(file), rep_INT(modes)) == 0)
	return modes;
    else
	return rep_signal_file_error(file);
}

repv
rep_file_modes_as_string(repv file)
{
    struct stat *st = stat_file(file);
    repv string = Fmake_string(rep_MAKE_INT(10), rep_MAKE_INT('-'));
    if(st != 0 && string && rep_STRINGP(string))
    {
	unsigned long perms = st->st_mode;
	int i;
	char c = '-';
	if(S_ISDIR(perms))	    c = 'd';
	else if(S_ISLNK(perms))	    c = 'l';
	else if(S_ISBLK(perms))	    c = 'b';
	else if(S_ISCHR(perms))	    c = 'c';
	else if(S_ISFIFO(perms))    c = 'p';
	else if(S_ISSOCK(perms))    c = 's';
	rep_STR(string)[0] = c;
	for(i = 0; i < 3; i++)
	{
	    unsigned long xperms = perms >> ((2 - i) * 3);
	    if(xperms & 4)
		rep_STR(string)[1+i*3] = 'r';
	    if(xperms & 2)
		rep_STR(string)[2+i*3] = 'w';
	    c = (xperms & 1) ? 'x' : 0;
	    if(perms & (04000 >> i))
	    {
		static char extra_bits[3] = { 'S', 'S', 'T' };
		/* Rampant abuse of ASCII knowledge :-) */
		c = extra_bits[i] | (c & 0x20);
	    }
	    if(c != 0)
		rep_STR(string)[3+i*3] = c;
	}
    }
    return string;
}

repv
rep_file_modtime(repv file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return rep_MAKE_TIME(st->st_mtime);
    else
	/* Really this should return nil */
	return rep_MAKE_TIME(0);
}

repv
rep_directory_files(repv dir_name)
{
    DIR *dir;
    if(*rep_STR(dir_name) == 0)
	dir_name = rep_VAL(&dot);
    dir = opendir(rep_STR(dir_name));
    if(dir)
    {
	repv list = Qnil;
	struct dirent *de;
	while((de = readdir(dir)))
	{
	    repv name = rep_string_dupn(de->d_name, NAMLEN(de));
	    list = Fcons(name, list);
	    if(name == rep_NULL || list == rep_NULL)
	    {
		rep_mem_error();
		closedir(dir);
		return rep_NULL;
	    }
	}
	closedir(dir);
	return list;
    }
    return Fsignal(Qfile_error, rep_list_2(rep_lookup_errno(), dir_name));
}

repv
rep_read_symlink (repv file)
{
    char buf[PATH_MAX];
    int len = readlink (rep_STR(file), buf, sizeof (buf));
    if (len == -1)
	return rep_signal_file_error (file);
    else
	return rep_string_dupn (buf, len);
}

repv
rep_make_symlink (repv file, repv contents)
{
    if (symlink (rep_STR (contents), rep_STR (file)) == 0)
	return Qt;
    else
	return rep_signal_file_error (file);
}

repv
rep_getpwd(void)
{
    char buf[PATH_MAX];
#ifdef HAVE_GETCWD
    if(!getcwd(buf, PATH_MAX))
#else
    if(!getwd(buf))
#endif
	return rep_signal_file_error(Qnil);
    else
    {
	/* Ensure that it ends with "/" */
	int len = strlen(buf);
	if(len < (PATH_MAX - 1) && buf[len] != '/')
	{
	    buf[len++] = '/';
	    buf[len] = 0;
	}
	return rep_string_dupn(buf, len);
    }
}


/* module name conversion */

repv
rep_structure_file (repv in)
{
    /* Convert dots to slashes.  XXX escape meta chars? */

    char *ptr = strchr (rep_STR (in), '.');
    if (ptr == 0)
	return in;
    else
    {
	repv copy = rep_string_dupn (rep_STR (in), rep_STRING_LEN (in));
	for (ptr = rep_STR (copy); *ptr != 0; ptr++)
	{
	    if (*ptr == '.')
		*ptr = '/';
	}
	return copy;
    }
}
