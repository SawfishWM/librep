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

(defmacro define-scan-body (body)
  `(mapcar (lambda (f)
	     (define-scan-form f)) ,body))

;; This needs to handle all special forms. It also needs to handle any
;; macros that the compiler wants to see without being expanded..
(defun define-scan-form (form)
  (if (atom form)
      form
    (case (car form)
      ((let let* letrec let-fluids)
       (if (and (eq (car form) 'let) (cadr form) (symbolp (cadr form)))
	   ;; named let, expand
	   (define-scan-form (macroexpand-1 form macro-environment))
	 (let* ((type (car form))
		fun values body)
	   (setq form (cdr form))
	   (when (and (eq type 'let) (car form) (symbolp (car form)))
	     (setq fun (car form))
	     (setq form (cdr form)))
	   (setq values (mapcar (lambda (lst)
				  (if (consp lst)
				      (cons (car lst) (define-scan-body
						       (cdr lst)))
				    lst))
				(car form)))
	   (setq body (define-scan-internals (cdr form)))
	   (if fun
	       (list type fun values body)
	     (list type values body)))))

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
       (list* 'condition-case (nth 1 form) (define-scan-body (nthcdr 2 form))))

      ((catch unwind-protect progn)
       (cons (car form) (define-scan-body (cdr form))))

      ((quote structure-ref) form)

      ((lambda)
       (let ((body (nthcdr 2 form))
	     (header nil))
	 ;; skip doc strings and interactive decls..
	 (while (or (stringp (car body)) (eq (caar body) 'interactive))
	   (setq header (cons (car body) header))
	   (setq body (cdr body)))
	 `(lambda ,(cadr form)
	    ,@(nreverse header)
	    ,(define-scan-internals body))))

      ((defvar)
       (list* 'defvar (nth 1 form) (define-scan-form (nth 2 form))
	      (nthcdr 3 form)))

      (t (let ((expansion (macroexpand-1 form macro-environment)))
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
