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

#include "jade.h"
#include <lib/jade_protos.h>

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

_PR u_long sys_file_length(VALUE file);
_PR VALUE sys_file_name_absolute_p(VALUE file);
_PR VALUE sys_expand_file_name(VALUE file);
_PR VALUE sys_canonical_file_name(VALUE file);
_PR VALUE sys_file_name_nondirectory(VALUE file);
_PR VALUE sys_file_name_directory(VALUE file);
_PR VALUE sys_file_name_as_directory(VALUE file);
_PR VALUE sys_directory_file_name(VALUE file);
_PR VALUE sys_delete_file(VALUE file);
_PR VALUE sys_rename_file(VALUE old, VALUE new);
_PR VALUE sys_make_directory(VALUE dir);
_PR VALUE sys_delete_directory(VALUE dir);
_PR VALUE sys_copy_file(VALUE src, VALUE dst);
_PR VALUE sys_file_readable_p(VALUE file);
_PR VALUE sys_file_writable_p(VALUE file);
_PR VALUE sys_file_exists_p(VALUE file);
_PR VALUE sys_file_regular_p(VALUE file);
_PR VALUE sys_file_directory_p(VALUE file);
_PR VALUE sys_file_symlink_p(VALUE file);
_PR VALUE sys_file_owner_p(VALUE file);
_PR VALUE sys_file_nlinks(VALUE file);
_PR VALUE sys_file_size(VALUE file);
_PR VALUE sys_file_modes(VALUE file);
_PR VALUE sys_set_file_modes(VALUE file, VALUE modes);
_PR VALUE sys_file_modes_as_string(VALUE file);
_PR VALUE sys_file_modtime(VALUE file);
_PR VALUE sys_directory_files(VALUE dir_name);
_PR VALUE sys_getpwd(void);


/* Support functions */

DEFSTRING(dot, ".");

static inline u_char *
file_part(char *name)
{
    char *tmp = strrchr(name, '/');
    return tmp != 0 ? tmp + 1 : name;
}

static struct stat *
stat_file(VALUE file)
{
    static struct stat statbuf;
    if(stat(VSTR(file), &statbuf) == 0)
	return &statbuf;
    else
	return 0;
}

u_long
sys_file_length(VALUE file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return st->st_size;
    else
	return 0;
}


/* File ops */

VALUE
sys_file_name_absolute_p(VALUE file)
{
    return (((VSTR(file)[0] == '/') || (VSTR(file)[0] == '~'))
	    ? sym_t : sym_nil);
}

VALUE
sys_expand_file_name(VALUE file)
{
    char buf[PATH_MAX];
    char *optr = buf;
    char *iptr = VSTR(file);
    while(*iptr != 0)
    {
	char *end;

	if(iptr[0] == '.')
	{
	    if(iptr[1] == '/')
	    {
		iptr += 2;
		continue;
	    }
	    else if(iptr[1] == 0)
	    {
		if(optr == buf)
		    /* Only character in string. Must preserve the dot. */
		    *optr++ = '.';
		iptr++;
		continue;
	    }
	    else if(iptr[1] == '.')
	    {
		if(iptr[2] == '/' || iptr[2] == 0)
		{
		    char *back = optr;
		    if(back > buf && back[-1] == '/')
			back--;
		    while(back > buf && back[-1] != '/')
			back--;
		    if(back >= buf && *back != '/')
			optr = back;
		    else
		    {
			/* Can't move up; leave the .. in the file name */
			*optr++ = '.';
			*optr++ = '.';
			if(iptr[2] == '/')
			    *optr++ = '/';
		    }
		    iptr += (iptr[2] == 0) ? 2 : 3;
		    continue;
		}
	    }
	}
	else if(*iptr == '/')
	{
	    /* Must be a root */
	    optr = buf;
	    *optr++ = *iptr++;
	    continue;
	}

	end = strchr(iptr, '/');
	if(end == 0)
	    end = iptr + strlen(iptr);
	memcpy(optr, iptr, end - iptr);
	optr += end - iptr;
	iptr = end;
	if(*iptr == '/')
	    *optr++ = *iptr++;
    }

    if(optr - buf != STRING_LEN(file)
       || memcmp(VSTR(file), buf, optr - buf) != 0)
	return string_dupn(buf, optr - buf);
    else
	return file;
}

VALUE
sys_canonical_file_name(VALUE file)
{
    char buf[PATH_MAX];
    if(realpath(VSTR(file), buf) != 0)
	return string_dup(buf);
    else
	/* Bail out */
	return file;
}

VALUE
sys_file_name_nondirectory(VALUE file)
{
    u_char *tem = file_part(VSTR(file));
    return tem == VSTR(file) ? file : string_dup(tem);
}

VALUE
sys_file_name_directory(VALUE file)
{
    int len = file_part(VSTR(file)) - VSTR(file);
    return string_dupn(VSTR(file), len);
}

VALUE
sys_file_name_as_directory(VALUE file)
{
    int len = STRING_LEN(file);
    if(file_part(VSTR(file)) == VSTR(file) + len)
    {
	/* It's already a directory */
	return file;
    }
    else
    {
	VALUE new = string_dupn(VSTR(file), len + 1);
	if(new)
	{
	    VSTR(new)[len] = '/';
	    VSTR(new)[len+1] = 0;
	}
	return new;
    }
}

VALUE
sys_directory_file_name(VALUE file)
{
    int len = STRING_LEN(file);
    if(file_part(VSTR(file)) != VSTR(file) + len)
    {
	/* There's a file part. Just return the initial string? */
	return file;
    }
    else
    {
	if(len == 0)
	    return VAL(&dot);
	else if(len == 1)
	    return file;
	else
	    /* Chop the trailing "/" */
	    return string_dupn(VSTR(file), len - 1);
    }
}

VALUE
sys_delete_file(VALUE file)
{
    if(unlink(VSTR(file)) == 0)
	return sym_t;
    else
	return signal_file_error(file);
}

VALUE
sys_rename_file(VALUE old, VALUE new)
{
    if(rename(VSTR(old), VSTR(new)) != -1)
	return sym_t;
    else
	return signal_file_error(list_2(old, new));
}

VALUE
sys_make_directory(VALUE dir)
{
    if(mkdir(VSTR(dir), S_IRWXU | S_IRWXG | S_IRWXO) == 0)
	return sym_t;
    else
	return signal_file_error(dir);
}

VALUE
sys_delete_directory(VALUE dir)
{
    if(rmdir(VSTR(dir)) == 0)
	return sym_t;
    else
	return signal_file_error(dir);
}

VALUE
sys_copy_file(VALUE src, VALUE dst)
{
    VALUE res = sym_t;
    int srcf;
    srcf = open(VSTR(src), O_RDONLY);
    if(srcf != -1)
    {
	int dstf = open(VSTR(dst), O_WRONLY | O_CREAT | O_TRUNC, 0666);
	if(dstf != -1)
	{
	    struct stat statb;
	    int rd;
	    if(fstat(srcf, &statb) == 0)
		chmod(VSTR(dst), statb.st_mode);
	    do {
		u_char buf[BUFSIZ];
		int wr;
		rd = read(srcf, buf, BUFSIZ);
		if(rd < 0)
		{
		    res = signal_file_error(src);
		    break;
		}
		wr = write(dstf, buf, rd);
		if(wr != rd)
		{
		    res = signal_file_error(dst);
		    break;
		}
	    } while(rd != 0);
	    close(dstf);
	}
	else
	    res = signal_file_error(dst);
	close(srcf);
    }
    else
	res = signal_file_error(src);
    return res;
}

VALUE
sys_file_readable_p(VALUE file)
{
    return access(VSTR(file), R_OK) == 0 ? sym_t : sym_nil;
}

VALUE
sys_file_writable_p(VALUE file)
{
    return access(VSTR(file), W_OK) == 0 ? sym_t : sym_nil;
}

VALUE
sys_file_exists_p(VALUE file)
{
    return access(VSTR(file), F_OK) == 0 ? sym_t : sym_nil;
}

VALUE
sys_file_regular_p(VALUE file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return S_ISREG(st->st_mode) ? sym_t : sym_nil;
    else
	return sym_nil;
}

VALUE
sys_file_directory_p(VALUE file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return S_ISDIR(st->st_mode) ? sym_t : sym_nil;
    else
	return sym_nil;
}

VALUE
sys_file_symlink_p(VALUE file)
{
    struct stat st;
    if(lstat(VSTR(file), &st) == 0)
	return S_ISLNK(st.st_mode) ? sym_t : sym_nil;
    else
	return sym_nil;
}

VALUE
sys_file_owner_p(VALUE file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return ((st->st_uid == geteuid() && st->st_gid == getegid())
		? sym_t : sym_nil);
    else
	return sym_nil;
}

VALUE
sys_file_nlinks(VALUE file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return MAKE_INT(st->st_nlink);
    else
	return sym_nil;
}

VALUE
sys_file_size(VALUE file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return MAKE_INT(st->st_size);
    else
	return sym_nil;
}

VALUE
sys_file_modes(VALUE file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return MAKE_INT(st->st_mode & 07777);
    else
	return sym_nil;
}

VALUE
sys_set_file_modes(VALUE file, VALUE modes)
{
    DECLARE2(modes, INTP);
    if(chmod(VSTR(file), VINT(modes)) == 0)
	return modes;
    else
	return signal_file_error(file);
}

VALUE
sys_file_modes_as_string(VALUE file)
{
    struct stat *st = stat_file(file);
    VALUE string = cmd_make_string(MAKE_INT(10), MAKE_INT('-'));
    if(st != 0 && string && STRINGP(string))
    {
	ulong perms = st->st_mode;
	int i;
	char c = '-';
	if(S_ISDIR(perms))	    c = 'd';
	else if(S_ISLNK(perms))	    c = 'l';
	else if(S_ISBLK(perms))	    c = 'b';
	else if(S_ISCHR(perms))	    c = 'c';
	else if(S_ISFIFO(perms))    c = 'p';
	else if(S_ISSOCK(perms))    c = 's';
	VSTR(string)[0] = c;
	for(i = 0; i < 3; i++)
	{
	    u_long xperms = perms >> ((2 - i) * 3);
	    if(xperms & 4)
		VSTR(string)[1+i*3] = 'r';
	    if(xperms & 2)
		VSTR(string)[2+i*3] = 'w';
	    c = (xperms & 1) ? 'x' : 0;
	    if(perms & (04000 >> i))
	    {
		static char extra_bits[3] = { 'S', 'S', 'T' };
		/* Rampant abuse of ASCII knowledge :-) */
		c = extra_bits[i] | (c & 0x20);
	    }
	    if(c != 0)
		VSTR(string)[3+i*3] = c;
	}
    }
    return string;
}

VALUE
sys_file_modtime(VALUE file)
{
    struct stat *st = stat_file(file);
    if(st != 0)
	return MAKE_TIME(st->st_mtime);
    else
	/* Really this should return nil */
	return MAKE_TIME(0);
}

VALUE
sys_directory_files(VALUE dir_name)
{
    DIR *dir;
    if(*VSTR(dir_name) == 0)
	dir_name = VAL(&dot);
    dir = opendir(VSTR(dir_name));
    if(dir)
    {
	VALUE list = sym_nil;
	struct dirent *de;
	while((de = readdir(dir)))
	{
	    VALUE name = string_dupn(de->d_name, NAMLEN(de));
	    list = cmd_cons(name, list);
	    if(name == LISP_NULL || list == LISP_NULL)
	    {
		mem_error();
		closedir(dir);
		return LISP_NULL;
	    }
	}
	closedir(dir);
	return list;
    }
    return cmd_signal(sym_file_error, list_2(lookup_errno(), dir_name));
}

VALUE
sys_getpwd(void)
{
    char buf[PATH_MAX];
#ifdef HAVE_GETCWD
    if(!getcwd(buf, PATH_MAX))
#else
    if(!getwd(buf))
#endif
	return signal_file_error(sym_nil);
    else
    {
	/* Ensure that it ends with "/" */
	int len = strlen(buf);
	if(len < (PATH_MAX - 1) && buf[len] != '/')
	{
	    buf[len++] = '/';
	    buf[len] = 0;
	}
	return string_dupn(buf, len);
    }
}
