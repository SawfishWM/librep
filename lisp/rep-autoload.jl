#| rep-autoload.jl -- autoloads for %default-structure

   $Id$

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;; ::autoload-start::
(autoload 'sort "sort")
(autoload 'getenv "environ")
(autoload 'setenv "environ")
(autoload 'unsetenv "environ")
(autoload-macro 'define "define")
(autoload-macro 'with-internal-definitions "define")
(autoload-macro 'with-threads-blocked "threads")
(autoload 'string-upper-case-p "string-util")
(autoload 'string-lower-case-p "string-util")
(autoload 'string-capitalized-p "string-util")
(autoload 'string-upcase "string-util")
(autoload 'string-downcase "string-util")
(autoload 'capitalize-string "string-util")
(autoload 'repl "rep-repl")
;;; ::autoload-end::
