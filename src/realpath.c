/* Return the canonical absolute name of a given file.
   Copyright (C) 1996 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* I've hacked this file to compile with Jade --jsh */

#include <config.h>

#ifndef HAVE_REALPATH

/* AIX requires this to be the first thing in the file.  */
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

#include <stdlib.h>
#include <string.h>
#ifdef NEED_MEMORY_H
# include <memory.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#include <limits.h>
#include <sys/param.h>
#include <sys/stat.h>

#ifndef S_ISLNK
#define S_ISLNK(mode)  (((mode) & S_IFMT) == S_IFLNK)
#endif

#ifndef S_ISSOCK
#define S_ISSOCK(mode)  (((mode) & S_IFMT) == S_IFSOCK)
#endif

/* Return the canonical absolute name of file NAME.  A canonical name
   does not contain any `.', `..' components nor any repeated path
   separators ('/') or symlinks.  All path components must exist.  If
   RESOLVED is null, the result is malloc'd; otherwise, if the
   canonical name is PATH_MAX chars or more, returns null with `errno'
   set to ENAMETOOLONG; if the name fits in fewer than PATH_MAX chars,
   returns the name in RESOLVED.  If the name cannot be resolved and
   RESOLVED is non-NULL, it contains the path of the first component
   that cannot be resolved.  If the path can be resolved, RESOLVED
   holds the same value as the value returned.  */

/* I'll never test errno, so ignore all attempts to set it, in the
   interests of portability.. */
#define __set_errno(err)

char *
realpath (const char *name, char *resolved)
{
  char *rpath, *dest, *extra_buf = NULL;
  const char *start, *end, *rpath_limit;
  long int path_max;
  int num_links = 0;

#ifdef PATH_MAX
  path_max = PATH_MAX;
#else
  path_max = pathconf (name, _PC_PATH_MAX);
  if (path_max <= 0)
    path_max = 1024;
#endif

  rpath = resolved ? alloca (path_max) : malloc (path_max);
  rpath_limit = rpath + path_max;

  if (name[0] != '/')
    {
#ifdef HAVE_GETCWD
      if (!getcwd (rpath, path_max))
#else
      if (!getwd (rpath))
#endif
	goto error;
      dest = strchr (rpath, '\0');
    }
  else
    {
      rpath[0] = '/';
      dest = rpath + 1;
    }

  for (start = end = name; *start; start = end)
    {
      struct stat st;
      int n;

      /* skip sequence of multiple path-separators: */
      while (*start == '/') ++start;

      /* find end of path component: */
      for (end = start; *end && *end != '/'; ++end);

      if (end - start == 0)
	break;
      else if (strncmp (start, ".", end - start) == 0)
	/* nothing */;
      else if (strncmp (start, "..", end - start) == 0) {
	/* back up to previous component, ignore if at root already: */
	if (dest > rpath + 1)
	  while ((--dest)[-1] != '/');
      } else
	{
	  size_t new_size;

	  if (dest[-1] != '/')
	    *dest++ = '/';

	  if (dest + (end - start) >= rpath_limit)
	    {
	      if (resolved)
		{
		  __set_errno (ENAMETOOLONG);
		  goto error;
		}
	      new_size = rpath_limit - rpath;
	      if (end - start + 1 > path_max)
		new_size += end - start + 1;
	      else
		new_size += path_max;
	      rpath = realloc (rpath, new_size);
	      rpath_limit = rpath + new_size;
	      if (!rpath)
		return NULL;
	    }

	  memcpy (dest, start, end - start);
	  dest += end - start;
	  *dest = '\0';

	  if (lstat (rpath, &st) < 0)
	    goto error;

	  if (S_ISLNK (st.st_mode))
	    {
	      char *buf = alloca (path_max);

	      if (++num_links > MAXSYMLINKS)
		{
		  __set_errno (ELOOP);
		  goto error;
		}

	      n = readlink (rpath, buf, path_max);
	      if (n < 0)
		goto error;
	      buf[n] = '\0';

	      if (!extra_buf)
		extra_buf = alloca (path_max);

	      if ((long int) (n + strlen (end)) >= path_max)
		{
		  __set_errno (ENAMETOOLONG);
		  goto error;
		}

	      /* careful here, end may be a pointer into extra_buf... */
	      strcat (buf, end);
	      strcpy (extra_buf, buf);
	      name = end = extra_buf;

	      if (buf[0] == '/')
		dest = rpath + 1;	/* it's an absolute symlink */
	      else
		/* back up to previous component, ignore if at root already: */
		if (dest > rpath + 1)
		  while ((--dest)[-1] != '/');
	    }
	  else
	    num_links = 0;
	}
    }
  if (dest > rpath + 1 && dest[-1] == '/')
    --dest;
  *dest = '\0';

  return resolved ? strcpy (resolved, rpath) : rpath;

error:
  if (resolved)
    strcpy (resolved, rpath);
  else
    free (rpath);
  return NULL;
}

#endif /* !HAVE_REALPATH */
