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

(declare (unsafe-for-call/cc))

(declare (in-module rep.lang.interpreter))

(open-structures '(rep.lang.backquote))

;; Commentary:

;; This attempts to implement Scheme's elegant block-structured
;; function definitions. It will scan leading `define' forms from all
;; `define', `let', `let*', and `lambda' special forms (and from any
;; macros in terms of these special forms)

;; Note that the rep interpreter and compiler support scheme-like
;; lambda lists natively, so things like (define (foo . bar) ..) will
;; work correctly

;; Note^2 that this doesn't work quite like Scheme define, in that the
;; outermost define always affects the global environment (unless
;; within a with-internal-definitions block) [the reason for this
;; ugliness is to avoid redefining lambda]

;; List of currently bound variables. Used to avoid expanding macros
;; that have been rebound locally
(%define define-bound-vars (make-fluid '()))

;; returns (SYM DEF [DOC])
(defun define-parse (args)
  (if (consp (car args))
      (define-parse `(,(caar args) (lambda ,(cdar args) ,@(cdr args))))
    (list* (car args) (define-scan-form (cadr args))
	   (and (stringp (caddr args)) (list (caddr args))))))

(defun define-scan-internals (body)
  (let
      (defs)
    (while (eq (caar body) 'define)
      (setq defs (cons (define-parse (cdar body)) defs))
      (setq body (cdr body)))
    (if defs
	(list* 'letrec
	       (mapcar (lambda (def)
			 (list (car def) (cadr def))) (nreverse defs))
	       (define-scan-body body))
      (let ((new-body (define-scan-body body)))
	(if (null (cdr new-body))
	    (car new-body)
	  (cons 'progn new-body))))))

(defun define-scan-body (body)
  (let ((new (mapcar define-scan-form body)))
    (if (equal new body) body new)))

(defun define-macroexpand-1 (form)
  (if (memq (car form) (fluid define-bound-vars))
      form
    (macroexpand-1 form macro-environment)))

;; This needs to handle all special forms. It also needs to handle any
;; macros that the compiler wants to see without being expanded..
(defun define-scan-form (form)
  (if (atom form)
      form
    (case (if (memq (car form) (fluid define-bound-vars)) '() (car form))
      ((let)
       (if (and (eq (car form) 'let) (cadr form) (symbolp (cadr form)))
	   ;; named let, expand
	   (define-scan-form (define-macroexpand-1 form))
	 (let loop ((rest (cadr form))
		    (vars '())
		    (clauses '()))
	   (cond ((null rest)
		  (list 'let (nreverse clauses)
			(let-fluids ((define-bound-vars
				      (nconc vars (fluid define-bound-vars))))
			  (define-scan-internals (cddr form)))))
		 ((consp (car rest))
		  (loop (cdr rest)
			(cons (caar rest) vars)
			(cons (cons (caar rest)
				    (define-scan-body (cdar rest))) clauses)))
		 (t (loop (cdr rest)
			  (cons (car rest) vars)
			  (cons (car rest) clauses)))))))

      ((let*)
       (let-fluids ((define-bound-vars (fluid define-bound-vars)))
	 (let loop ((rest (cadr form))
		    (clauses '()))
	   (cond ((null rest)
		  (list 'let* (nreverse clauses)
			(define-scan-internals (cddr form))))
		 ((consp (car rest))
		  (fluid-set define-bound-vars
			     (cons (caar rest) (fluid define-bound-vars)))
		  (loop (cdr rest)
			(cons (cons (caar rest)
				    (define-scan-body (cdar rest))) clauses)))
		 (t
		  (fluid-set define-bound-vars
			     (cons (car rest) (fluid define-bound-vars)))
		  (loop (cdr rest)
			(cons (car rest) clauses)))))))

      ((letrec)
       (let-fluids ((define-bound-vars
		     (nconc (mapcar (lambda (x) (or (car x) x)) (cadr form))
			    (fluid define-bound-vars))))
	 (list 'letrec
	       (mapcar (lambda (x)
			 (if (consp x)
			     (cons (car x) (define-scan-body (cdr x)))
			   x)) (cadr form))
	       (define-scan-internals (cddr form)))))

      ((let-fluids)
       (list 'let-fluids
	     (mapcar (lambda (x)
		       (if (consp x)
			   (cons (car x) (define-scan-body (cdr x)))
			 x)) (cadr form))
	     (define-scan-internals (cddr form))))

      ((setq)
       (let loop ((rest (cdr form))
		  (out nil))
	 (if rest
	     (loop (cddr rest)
		   (cons (list (car rest)
			       (define-scan-form (cadr rest))) out))
	   (cons (car form) (apply nconc (nreverse out))))))

      ((cond)
       (cons 'cond (mapcar (lambda (clause)
			     (define-scan-body clause)) (cdr form))))

      ((case)
       (list* 'case
	      (define-scan-form (nth 1 form))
	      (mapcar (lambda (clause)
			(cons (car clause) (define-scan-body (cdr clause))))
		      (nthcdr 2 form))))

      ((condition-case)
       (let ((var (if (eq (cadr form) 'nil) nil (cadr form))))
	 (let-fluids ((define-bound-vars (cons var (fluid define-bound-vars))))
	   (list* 'condition-case (cadr form)
		  (define-scan-body (cddr form))))))

      ((catch unwind-protect progn)
       (cons (car form) (define-scan-body (cdr form))))

      ((quote structure-ref) form)

      ((lambda)
       (let ((vars (let loop ((rest (cadr form))
			      (vars '()))
		     (cond ((null rest) vars)
			   ((memq (or (caar rest) (car rest))
				  '(#!optional #!key #!rest &optional &rest))
			    (loop (cdr rest) vars))
			   (t (loop (cdr rest) (cons (or (caar rest)
							 (car rest)) vars))))))
	     (body (nthcdr 2 form))
	     (header nil))
	 ;; skip doc strings and interactive decls..
	 (while (or (stringp (car body)) (eq (caar body) 'interactive))
	   (setq header (cons (car body) header))
	   (setq body (cdr body)))
	 `(lambda ,(cadr form)
	    ,@(nreverse header)
	    ,(let-fluids ((define-bound-vars
			   (nconc vars (fluid define-bound-vars))))
	       (define-scan-internals body)))))

      ((defvar)
       (list* 'defvar (nth 1 form) (define-scan-form (nth 2 form))
	      (nthcdr 3 form)))

      ((structure define-structure declare) form)

      (t (let ((expansion (define-macroexpand-1 form)))
	   (if (eq expansion form)
	       (define-scan-body form)
	     (define-scan-form expansion)))))))

;;;###autoload
(defmacro define (#!rest args)
  (let ((def (define-parse args)))
    (let ((var (car def))
	  (value (cadr def))
	  (doc (caddr def)))
      (if (eq (car value) 'lambda)
	  `(defun ,var ,(cadr value)
	     ,@(and doc (list doc))
	     ,@(let ((body (cddr value)))
		 (if (and (eq (car body) 'progn) (null (cdr body)))
		     (cdar body)
		   body)))
	(cons '%define def)))))

;;;###autoload
(defmacro define-macro (#!rest args)
  (let ((def (define-parse args)))
    (let ((var (car def))
	  (value (cadr def))
	  (doc (caddr def)))
      (if (eq (car value) 'lambda)
	  `(defmacro ,var ,(cadr value)
	     ,@(and doc (list doc))
	     ,@(let ((body (cddr value)))
		 (if (and (eq (car body) 'progn) (null (cdr body)))
		     (cdar body)
		   body)))
	;; can only expand to defmacro forms (for the compiler's sake)
	(error "Macros must be constant lambdas: %s" (car def))))))

;;;###autoload
(defmacro with-internal-definitions (#!rest body)
  (define-scan-internals body))
