;; define.jl -- Scheme define syntax
;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

;; $Id$

;; This file is part of librep.

;; librep is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; librep is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with librep; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'define)

;; Commentary:

;; This is a half-hearted attempt at supporting scheme-like block-
;; structured definitions. It scans inner defines from within outer
;; defines, but it _doesn't_ scan inner defines from let, etc.. (see
;; the provided lambda* macro that does this for lambda)

;; It would be hard to change this since let, etc, are all special
;; forms, and therefore handled specially by the compiler (except for
;; letrec which is a macro). [ recursively scanning the define body for
;; these forms isn't possible since we don't know the macro environment ]

;; Note that the rep interpreter/compiler support scheme-like lambda
;; lists natively, so things like (define (foo . bar) ..) should work
;; okay

;; returns (SYM . DEF)
(defun define-parse (args)
  (let
      (sym def)
    (if (consp (car args))
	;; (define (foo args...) body...)
	(progn
	  (setq sym (caar args))
	  (setq def `(lambda ,(cdar args)
		       ,(define-scan-internals (cdr args)))))
      (setq sym (car args))
      (setq def (cadr args)))
    (cons sym def)))

(defun define-scan-internals (body)
  (let
      ((defs (mapcar (lambda (form)
		       (define-parse (cdr form)))
		     (filter (lambda (form)
			       (eq (car form) 'define)) body))))
    (if defs
	(list* 'letrec
	       (mapcar (lambda (def)
			 (list (car def) (cdr def))) defs)
	       (filter (lambda (form)
			 (not (eq (car form) 'define))) body))
      (cons 'progn body))))

(defmacro define (&rest args)
  (let
      ((def (define-parse args)))
    (if (eq (cadr def) 'lambda)
	(list 'defun (car def) (caddr def) (car (cdddr def)))
      (list 'define-value (list 'quote (car def)) (cdr def)))))

(defmacro lambda* (spec &rest body)
  `(lambda ,spec ,(define-scan-internals body)))
