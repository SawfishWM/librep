#| rep.lang.error-helper -- give hints about what's causing common lisp errors

   $Id$

   Copyright (C) 2001 John Harper <jsh@pixelslut.com>

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
   the Free Software Foundation, 51 Franklin Street, Fifth Floor, 
   Boston, MA 02110-1301 USA
|#

(define-structure rep.lang.error-helper

    (export error-helper)

    (open rep
	  rep.regexp
	  rep.data.tables
	  rep.structures)

  ;; map error symbols to helper functions
  (define helper-table (make-table eq-hash eq))

  (define output-stream (make-fluid standard-error))

  (define (define-helper name function) (table-set helper-table name function))
  (define (helper-ref name) (table-ref helper-table name))

  (define (for-each-structure fun)
    (fun 'rep (get-structure 'rep))
    (structure-walk (lambda (name struct)
		      (unless (or (not struct)
				  (eq name 'rep)
				  (string-match "^%" (symbol-name name)))
			(fun name struct)))
		    (get-structure '%structures)))

  (define (output fmt . args)
    (write (fluid output-stream) #\()
    (apply format (fluid output-stream) fmt args)
    (write (fluid output-stream) "\)\n"))

  (define (void-value-helper symbol)
    (case symbol
      ((export compound-interface structure-interface)
       (output
"You may have the interface clause (`export', etc) of a module declaration
in the wrong position."))

      ((open access)
       (output
"You may have the configuration clause (`open', etc) of a module declaration
in the wrong position."))

      (t (let ((structs '()))
	   (for-each-structure
	    (lambda (name struct)
	      (when (structure-exports-p struct symbol)
		(setq structs (cons name structs)))))
	   (cond ((null structs)
		  (output "You're accessing an undefined variable or function `%s'"
			  symbol))
		 ((null (cdr structs))
		  (output
		   "You probably need to open the module `%s'" (car structs)))
		 (t (output "You probably need to open one of the modules %s"
			    (mapconcat (lambda (x)
					 (format nil "`%s'" x))
				       (nreverse structs) ", "))))))))

  (define-helper 'void-value void-value-helper)

  (define (error-helper error-symbol data)
    (let ((helper (helper-ref error-symbol)))
      (when helper
	(apply helper data)))))
