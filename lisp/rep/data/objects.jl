#| objects.jl -- very basic OO system

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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure rep.data.objects

    (export object)

    (open rep)

  ;; Commentary:

  ;; This module provides an extremely simple message-passing object
  ;; implementation, with support for single inheritance. The `object'
  ;; form expands to a lambda expression, hence it captures local
  ;; bindings for the method implementations.

  ;; Any unknown methods are passed off to BASE-OBJECT, or if that is
  ;; nil, an `unknown-method' error is signalled.

  ;; PARAM-LIST isn't the full lambda spec, just a list of symbols. The
  ;; list can be dotted to a symbol to make a &rest parameter. All
  ;; parameters are optional (i.e. default to nil)

  ;; (object BASE-OBJECT
  ;;   ((METHOD-NAME . PARAM-LIST) BODY...)
  ;;   ((METHOD-NAME . PARAM-LIST) BODY...))

  (define (make-let-bindings spec args-var)
    (let loop ((rest spec)
	       (i 0)
	       (out '()))
      (cond ((null rest) (nreverse out))
	    ((atom rest)
	     (loop '() (1+ i) (cons `(,rest (nthcdr ,i ,args-var)) out)))
	    (t (loop (cdr rest) (1+ i)
		     (cons `(,(car rest) (nth ,i ,args-var)) out))))))

  (defmacro object (base-object . methods)
    (let ((op (gensym))
	  (args (gensym)))
      `(lambda (,op . ,args)
	 (case ,op
	   ,@(mapcar (lambda (method)
		       `((,(caar method))
			 (let ,(make-let-bindings (cdar method) args)
			   ,@(cdr method)))) methods)
	   (t ,(if base-object
		   `(apply ,base-object ,op ,args)
		 `(signal 'unknown-method (list ,op))))))))

  (put 'unknown-method 'error-message "Unknown method call"))
