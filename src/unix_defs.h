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

#include "lists.h"

#define HAVE_SUBPROCESSES

/* default directory to look for scripts in */
#define LISP_LIB_DIR QUOTE(JADE_DIR) "/lisp/"

/* file containing doc-strings */
#define DOC_FILE QUOTE(JADE_DIR) "/DOC"

/* From `unix_memory.c'  */
#define myfree(p) do { if(p) free(p); } while(0)
#define initmem() (1)
#define killmem()

/* For the client/server stuff. */
#define JADE_SOCK_NAME ".Jade_rendezvous"

#endif /* _UNIX_DEFS_H */
