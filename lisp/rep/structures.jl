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
  "Create an interface called NAME exporting the list of symbols SIG."
  (structure-define (get-structure '%interfaces) name sig))

(defun parse-interface (sig)
  "Return the list of symbols described by the module interface SIG."
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
  "Create an alias of the structure called FROM as the name TO."
  (name-structure (get-structure from) to))

(defun locate-binding (var imported)
  "Return the name of the structure binding VAR, using the list of module
names IMPORTED as the search start points."
  (when imported
    (let ((tem (structure-exports-p (get-structure (car imported)) var)))
      (cond ((null tem)
	     (locate-binding var (cdr imported)))
	    ((eq tem 'external)
	     ;; this module exports it, but it doesn't define
	     ;; it, so search its imports
	     (locate-binding var (structure-imports
				  (get-structure (car imported)))))
	    (t (car imported))))))

(export-bindings '(make-interface parse-interface
		   alias-structure locate-binding))
