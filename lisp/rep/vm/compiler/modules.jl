#| compiler-modules.jl -- module handling for the compiler

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

(define-structure compiler-modules (export comp-variable-p
					   comp-locate-var
					   comp-symbol-value
					   comp-binding-from-rep-p
					   comp-binding-immutable-p
					   comp-get-procedure-handler
					   comp-macroexpand
					   comp-macroexpand-1
					   comp-compile-module-body
					   comp-note-require
					   comp-note-macro
					   compile-structure
					   compile-define-structure
					   compile-structure-ref)
  (open rep
	structure-internals
	compiler-basic
	compiler-const
	compiler-utils
	compiler-lap
	compiler-vars
	bytecodes)

;;; module environment of form being compiled

  ;; the name of the module being compiled in
  (defvar comp-current-module *root-structure*)

  ;; if non-nil, the namespace of the module being compiled in; only
  ;; set when compiling code outside a module definition
  (defvar comp-current-structure (%get-structure comp-current-module))

  ;; the names of the currently open and accessed modules
  (defvar comp-open-modules (and comp-current-structure
				 (%structure-imports comp-current-structure)))
  (defvar comp-accessed-modules (and comp-current-structure
				     (%structure-accessible
				      comp-current-structure)))

;;; functions

  ;; return t if the module called STRUCT exports a variable called VAR
  (defun module-exports-p (struct var)
    (and (symbolp var) (%structure-exports-p (%intern-structure struct) var)))

  ;; return t if ARG is a structure reference form
  (defun structure-ref-p (arg)
    (and (eq (car arg) 'structure-ref)
	 (memq (comp-locate-var 'structure-ref) '(rep module-system))))

  ;; return t if ARG refers to a variable
  (defun comp-variable-p (arg)
    (or (symbolp arg) (structure-ref-p arg)))

  ;; return the name of the structure exporting VAR to the current
  ;; structure, or nil
  (defun comp-locate-var (var)
    (if (structure-ref-p var)
	(nth 1 var)
      (let loop ((rest comp-open-modules))
	(if rest
	    (if (module-exports-p (car rest) var)
		(car rest)
	      (loop (cdr rest)))
	  ;; it's not exported by any opened modules, if we have a handle
	  ;; on the current module (i.e. we're compiling code not in
	  ;; a module definition) try looking in that
	  (if (and (symbolp var) comp-current-structure
		   (%structure-bound-p comp-current-structure var))
	      comp-current-module
	    nil)))))

  ;; if possible, return the value of variable VAR, else return nil
  (defun comp-symbol-value (var)
    (cond ((and (symbolp var) (special-variable-p var))
	   (symbol-value var))
	  ((and (symbolp var) comp-current-structure
		(%structure-bound-p comp-current-structure var))
	   (%structure-ref comp-current-structure var))
	  (t
	   (let ((struct (comp-locate-var var)))
	     (and struct (%structure-ref (%intern-structure struct)
					 (if (consp var)
					     (nth 2 var)	;structure-ref
					   var)))))))

  ;; return t if the binding of VAR comes from the rep (built-ins) module
  (defun comp-binding-from-rep-p (var)
    (if (structure-ref-p var)
	(eq (nth 1 var) 'rep)
      (and (not (or (memq var comp-spec-bindings)
		    (memq var comp-lex-bindings)))
	   (eq (comp-locate-var var) 'rep))))

  ;; return t if the binding of VAR is a known constant
  ;; (not including those in comp-constant-env)
  (defun comp-binding-immutable-p (var)
    (and (not (or (memq var comp-spec-bindings)
		  (memq var comp-lex-bindings)))
	 (let
	     ((struct (comp-locate-var var)))
	   (and struct (binding-immutable-p
			(if (consp var)
			    (nth 2 var)	;structure-ref
			  var)
			(%intern-structure struct))))))

  (defun comp-get-procedure-handler (name prop-name)
    (unless (or (memq name comp-spec-bindings)
		(memq name comp-lex-bindings))
      (let*
	  ((struct (comp-locate-var name))
	   (prop (and struct (get struct prop-name))))
	(if (and prop (symbolp prop))
	    (get (if (consp name)
		     (nth 2 name)	;structure-ref
		   name) prop)
	  prop))))

  (defun comp-macroexpand-1 (form)
    (when (consp form)
      (let* ((def (assq (car form) comp-macro-env))
	     ;; make #<subr macroexpand> pass us any inner expansions
	     (macro-environment comp-macroexpand))
	(if def
	    (setq form (apply (cdr def) (cdr form)))
	  (setq def (comp-symbol-value (car form)))
	  (when (and (eq (car def) 'macro) (functionp (cdr def)))
	    (setq form (apply (cdr def) (cdr form)))))))
    form)

  (defun comp-macroexpand (form &optional pred)
    (let loop ((in form))
      (let
	  ((out (comp-macroexpand-1 in)))
	;;(format standard-error "in: %S, out: %S\n" in out)
	(if ((or pred eq) in out)
	    out
	  (loop out)))))

  ;; if OPENED or ACCESSED are `t', the current values are used
  (defun comp-compile-module-body (body name opened accessed)
    (let
	((comp-current-module (or name comp-current-module))
	 (comp-current-structure (if name nil comp-current-structure))
	 (comp-open-modules (if (eq opened t)
				comp-open-modules
			      opened))
	 (comp-accessed-modules (if (eq accessed t)
				    comp-accessed-modules
				  accessed))
	 (comp-macro-env comp-default-macro-env)
	 (comp-const-env nil)
	 (comp-inline-env nil)
	 (comp-defuns nil)
	 (comp-defvars comp-defvars)
	 (comp-defines nil)
	 (comp-lexically-pure t)
	 (comp-output-stream nil)
	 pass-1 pass-2
	 next-body)

      ;; find language pass-1 and pass-2 compilers; scan all opened
      ;; modules, then scan any they import, ..
      (catch 'out
	(let ((tocheck (list (list comp-current-module) comp-open-modules)))
	  (while tocheck
	    (mapc (lambda (struct)
		    (if (get struct 'compiler-module)
			(progn
			  (%intern-structure (get struct 'compiler-module))
			  (setq pass-1 (get struct 'compiler-pass-1))
			  (setq pass-2 (get struct 'compiler-pass-2))
			  (throw 'out))
		      (when (%get-structure struct)
			(setq tocheck (nconc tocheck
					     (list (%structure-imports
						    (%get-structure struct))))))))
		  (car tocheck))
	    (setq tocheck (cdr tocheck)))
	  (comp-warning "unknown language dialect for module"
			comp-current-module)))

      ;; pass 1. remember definitions in the body for pass 2
      (when pass-1
	(mapc (lambda (form)
		(setq next-body (cons (pass-1 form) next-body))) body)
	(setq body next-body)
	(setq next-body nil))

      ;; pass 2. the actual compilation
      (when pass-2
	(mapc (lambda (form)
		(setq next-body (cons (pass-2 form) next-body))) body)
	(setq body next-body)
	(setq next-body nil))

      ;; return the compiled representation of the body
      body))

  (defun comp-note-require (feature)
    (unless (memq feature comp-open-modules)
      (cond ((%get-structure feature)
	     (setq comp-open-modules (cons feature comp-open-modules)))
	    (comp-current-structure
	     (unless (%eval-in-structure
		      `(featurep ',feature) comp-current-structure)
	       (%eval-in-structure
		`(require ',feature) comp-current-structure)
	       (when (%get-structure feature)
		 (setq comp-open-modules (cons feature comp-open-modules)))))
	    (t
	     ;; XXX this doesn't work, no alternative..?
	     (require feature)
	     (when (%get-structure feature)
	       (setq comp-open-modules (cons feature comp-open-modules)))))))

  ;; XXX enclose macro defs in the *root-structure*, this is different
  ;; to with interpreted code
  (defun comp-note-macro (name body)
    (setq comp-macro-env (cons (cons name
				     (%make-closure-in-structure
				      body (%get-structure *root-structure*)))
			       comp-macro-env)))


;;; declarations

  ;; (declare (in-module STRUCT))

  (defun declare-in-module (form)
    (setq comp-current-module (cadr form))
    (setq comp-current-structure (%get-structure comp-current-module)))
  (put 'in-module 'compiler-decl-fun declare-in-module)


;;; module compilers

  (defun compile-structure (form)
    (compile-structure-def nil (cadr form) (cddr form)))

  (defun compile-define-structure (form)
    (compile-structure-def (cadr form) (caddr form) (cdddr form)))

  (defun compile-structure-def (name sig body)
    (let
	((opened '(module-system))
	 (accessed '())
	 header)

      (while (memq (caar body) '(open access))
	(let
	    ((clause (car body)))
	  (case (car clause)
	    ((open)
	     (setq opened (nconc (reverse (cdr clause)) opened))
	     (setq header (cons `(%open-structures ',(cdr clause)) header)))

	    ((access)
	     (setq accessed (nconc (reverse (cdr clause)) accessed))
	     (setq header (cons `(%access-structures ',(cdr clause)) header))))

	  (setq body (cdr body))))
      (setq header (cons '(%open-structures '(module-system))
			 (nreverse header)))

      (setq body (comp-compile-module-body body name opened accessed))
      (comp-compile-form '%make-structure)
      (comp-compile-form `(%parse-interface ',sig))
      (if header
	  (progn
	    (comp-compile-constant `(lambda () ,@header))
	    (comp-write-op (bytecode enclose)))
	(comp-compile-constant nil))
      (if body
	  (progn
	    (comp-compile-constant `(lambda () ,@body))
	    (comp-write-op (bytecode enclose)))
	(comp-compile-constant nil))
      (when name
	(comp-compile-constant name))
      (comp-write-op (bytecode call) (if name 4 3))
      (comp-dec-stack (if name 4 3))))

  (defun parse-interface (sig)
    (cond ((eq (car sig) 'export)
	   (cdr sig))

	  ((eq (car sig) 'compound-interface)
	   (apply 'append (mapcar parse-interface (cdr sig))))

	  ((symbolp sig)
	   (if (boundp sig)
	       (symbol-value sig)
	     (comp-error "Don't know this interface: %s" sig)))))

  (defun compile-structure-ref (form)
    (let
	((struct (nth 1 form))
	 (var (nth 2 form)))
      (or (memq struct comp-accessed-modules)
	  (comp-error "Referencing non-accessible structure" struct))
      (or (module-exports-p struct var)
	  (comp-error "Referencing non-exported variable" struct var))
      (comp-compile-constant struct)
      (comp-compile-constant var)
      (comp-write-op (bytecode structure-ref))
      (comp-dec-stack))))
