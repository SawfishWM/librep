/* repint.h -- Main include file for library internal objects
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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef REPINT_H
#define REPINT_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* Maximum/minimum macros. Don't use when X or Y have side-effects! */
#define MAX(x,y) (((x) > (y)) ? (x) : (y))
#define MIN(x,y) (((x) < (y)) ? (x) : (y))

#include "rep.h"

#ifdef rep_HAVE_UNIX
# include "unix_defs.h"
#else
# error "Need an operating system definition"
#endif

#include "repint_subrs.h"
#include <build.h>

#endif /* REPINT_H */
