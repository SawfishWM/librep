#| compat-doc.jl -- the old documentation interfaces

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

(define-structure rep.lang.compat-doc

    (export describe-lambda-list
	    describe-value
	    doc-file-ref
	    doc-file-set
	    documentation
	    document-var
	    add-documentation
	    add-documentation-params)

    (open rep
	  rep.structures
	  rep.lang.doc)

  ;; make this appear as the old module 'lisp-doc
  (define-structure-alias lisp-doc rep.lang.compat-doc)

  (define (infer-structure sym)
    (locate-binding sym (list *user-structure*)))

  (define (documentation symbol #!optional value)
    (rep.lang.doc#documentation symbol (infer-structure symbol) value))

  (define (document-var symbol string)
    (document-variable symbol (infer-structure symbol) string))

  (define (add-documentation symbol string)
    (rep.lang.doc#add-documentation symbol nil string))

  (define (add-documentation-params name param-list)
    (rep.lang.doc#add-documentation-params name nil param-list)))
