/* unix_defs.h -- Declarations for Unix
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

#ifndef _UNIX_DEFS_H
#define _UNIX_DEFS_H

#include "build.h"
#include <stdlib.h>

#define HAVE_SUBPROCESSES

/* default directory to look for scripts in */
#define LISP_LIB_DIR_SUFFIX "/" VERSID "/lisp/"

/* Site scripts directory */
#define SITE_LISP_DIR_SUFFIX "/site-lisp/"

/* file containing doc-strings */
#define DOC_FILE_SUFFIX "/" VERSID "/DOC"

/* These are related to the definition of sys_alloc() in unix_main.c */
#ifndef DEBUG_SYS_ALLOC
# define sys_alloc(n) malloc(n)
# define sys_realloc(p,n) realloc(p,n)
# define sys_free(p) free(p)
# define sys_memory_kill()
#else
  extern void sys_free(void *p);
# define sys_memory_kill() sys_print_allocations()
#endif
#define sys_memory_init() (1)

/* For the client/server stuff. %s is host name */
#define JADE_SOCK_NAME ".jade-%s"

enum server_request {
    req_find_file = 0,
    req_eval,
    req_end_of_session
};

#ifndef HAVE_REALPATH
extern char *realpath (const char *name, char *resolved);
#endif

#endif /* _UNIX_DEFS_H */
