#| rep.io.streams bootstrap

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

(declare (in-module rep.io.streams))

(open-structures '(rep.lang.symbols
		   rep.data
		   rep.io.files))

;; Setup format-hooks-alist to a few default'ish things
(defvar format-hooks-alist (list (cons #\D file-name-directory)
				 (cons #\F file-name-nondirectory)))

(defun prin1-to-string (arg)
  "Return a string representing ARG."
  (format nil "%S" arg))

(defun read-from-string (string #!optional start)
  "Reads an object from STRING, starting at character number START (default
is 0)."
  (read (make-string-input-stream string start)))

(defun streamp (arg)
  "Returns true if ARG is some sort of I/O stream."
  (or (input-stream-p arg) (output-stream-p arg)))

(export-bindings '(prin1-to-string read-from-string streamp))
