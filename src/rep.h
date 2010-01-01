/* rep.h -- Public include file, brings in all the rest
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

#ifndef REP_H
#define REP_H

#include <sys/types.h>
#include <limits.h>

typedef int rep_bool;
#define rep_TRUE 1
#define rep_FALSE 0

#ifndef NULL
#define NULL 0
#endif

#include "rep_config.h"

#ifdef rep_HAVE_LONG_LONG
# define rep_long_long long long
#else
# define rep_long_long long
#endif

#include "rep_lisp.h"
#include "rep_regexp.h"
#include "rep_subrs.h"

#endif /* REP_H */
