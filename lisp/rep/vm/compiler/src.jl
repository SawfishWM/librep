#| src.jl -- source code program transforms

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
   the Free Software Foundation, 51 Franklin Street, Fifth Floor, 
   Boston, MA 02110-1301 USA
|#

(declare (unsafe-for-call/cc))

(define-structure rep.vm.compiler.src

    (export coalesce-constants
	    mash-constants
	    source-code-transform)

    (open rep
	  rep.vm.compiler.utils
	  rep.vm.compiler.modules
	  rep.vm.compiler.lap
	  rep.vm.compiler.bindings
	  rep.vm.bytecodes)

;;; Constant folding

  (defun foldablep (name)
    (unless (has-local-binding-p name)
      (let ((fun (get-procedure-handler name 'compiler-foldablep)))
	(and fun (fun name)))))

  (defun quote-constant (value)
    (if (or (symbolp value) (consp value))
	(list 'quote value)
      value))

  ;; This assumes that FORM is a list, and its car is one of the functions
  ;; in the comp-constant-functions list
  (defun fold-constants (form)
    (catch 'exit
      (let
	  ((args (mapcar (lambda (arg)
			   (when (consp arg)
			     (setq arg (compiler-macroexpand arg)))
			   (when (and (consp arg) (foldablep (car arg)))
			     (setq arg (fold-constants arg)))
			   (if (compiler-constant-p arg)
			       (compiler-constant-value arg)
			     ;; Not a constant, abort, abort
			     (throw 'exit form)))
			 (cdr form))))
	;; Now we have ARGS, the constant [folded] arguments from FORM
	(quote-constant (apply (compiler-symbol-value (car form)) args)))))

  (defun coalesce-constants (folder forms)
    (when forms
      (let loop ((result '())
		 (first (car forms))
		 (rest (cdr forms)))
	(cond ((null rest) (nreverse (cons first result)))
	      ((and (compiler-constant-p first)
		    rest (compiler-constant-p (car rest)))
	       (loop result
		     (quote-constant
		      (folder (compiler-constant-value first)
			      (compiler-constant-value (car rest))))
		     (cdr rest)))
	      (t (loop (cons first result) (car rest) (cdr rest)))))))

  (defun mash-constants (folder forms)
    (let ((consts (filter compiler-constant-p forms))
	  (non-consts (filter (lambda (x)
				(not (compiler-constant-p x))) forms)))
      (if consts
	  (cons (quote-constant
		 (apply folder (mapcar compiler-constant-value consts)))
		non-consts)
	non-consts)))

;;; Entry point

  (defun source-code-transform (form)
    (let (tem)
      ;; first try constant folding
      (when (and (consp form) (foldablep (car form)))
	(setq form (fold-constants form)))

      ;; then look for a specific tranformer
      (when (and (symbolp (car form))
		 (setq tem (get-procedure-handler
			    (car form) 'compiler-transform-property)))
	(setq form (tem form)))

      form)))
