#| rep.structures bootstrap

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

(declare (in-module rep.structures))

(open-structures '(rep.lang.symbols
		   rep.data))

(make-structure nil nil nil '%interfaces)

(defun make-interface (name sig)
  (structure-set (get-structure '%interfaces) name sig))

(defun parse-interface (sig)
  (cond ((null sig) '())
	((eq (car sig) 'export)
	 (cdr sig))
	((eq (car sig) 'compound-interface)
	 (apply append (mapcar parse-interface (cdr sig))))
	((eq (car sig) 'structure-interface)
	 (structure-interface (intern-structure (cadr sig))))
	((symbolp sig)
	 (let ((interfaces (get-structure '%interfaces)))
	   (or (structure-bound-p interfaces sig)
	       (error "No such interface: %s" sig))
	   (%structure-ref interfaces sig)))))

(defun alias-structure (from to)
  (name-structure (get-structure from) to))

(export-bindings '(make-interface parse-interface alias-structure))
