#| rep.regexp bootstrap

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

(declare (in-module rep.regexp))

(open-structures '(rep.data))

(defun string-replace (regexp template string)
  "Return the string created by replacing all matches of REGEXP in STRING
with the result of expanding TEMPLATE using the `expand-last-match'
function."
  (let loop ((point 0)
	     (out '()))
    (if (string-match regexp string point)
	(loop (match-end)
	      (cons (expand-last-match template)
		    (cons (substring string point (match-start)) out)))
      (if (null out)
	  string
	(apply concat (nreverse (cons (substring string point) out)))))))

(export-bindings '(string-replace))
