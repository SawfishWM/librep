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

(define-structure compiler-modules (export macro-env
					   variable-ref-p
					   locate-variable
					   compiler-symbol-value
					   compiler-binding-from-rep-p
					   compiler-binding-immutable-p
					   get-procedure-handler
					   get-language-property
					   compiler-macroexpand
					   compiler-macroexpand-1
					   compile-module-body
					   note-require
					   note-macro-def
					   compile-structure
					   compile-define-structure
					   compile-structure-ref
					   call-with-module-declared
					   compile-module)
  (open rep
	structure-internals
	compiler
	compiler-basic
	compiler-bindings
	compiler-const
	compiler-utils
	compiler-lap
	bytecodes)

  (define macro-env (make-fluid '()))		;alist of (NAME . MACRO-DEF)
  (define default-macro-env (make-fluid '()))

;;; module environment of form being compiled

  ;; the name of the module being compiled in
  (define current-module (make-fluid *root-structure*))

  ;; if non-nil, the namespace of the module being compiled in; only
  ;; set when compiling code outside a module definition
  (define current-structure (make-fluid
			     (%get-structure (fluid current-module))))

  (define current-language (make-fluid 'rep))

  ;; the names of the currently open and accessed modules
  (define open-modules (make-fluid (and (fluid current-structure)
					(%structure-imports
					 (fluid current-structure)))))
  (define accessed-modules (make-fluid (and (fluid current-structure)
					    (%structure-accessible
					     (fluid current-structure)))))

;;; functions

  ;; return t if the module called STRUCT exports a variable called VAR
  (defun module-exports-p (struct var)
    (and (symbolp var) (%structure-exports-p (%intern-structure struct) var)))

  ;; return t if ARG is a structure reference form
  (defun structure-ref-p (arg)
    (and (eq (car arg) 'structure-ref)
	 (memq (locate-variable 'structure-ref) '(rep module-system))))

  ;; return t if ARG refers to a variable
  (defun variable-ref-p (arg)
    (or (symbolp arg) (structure-ref-p arg)))

  ;; return the name of the structure exporting VAR to the current
  ;; structure, or nil
  (defun locate-variable (var)
    (if (structure-ref-p var)
	(nth 1 var)
      (let loop ((rest (fluid open-modules)))
	(if rest
	    (if (module-exports-p (car rest) var)
		(car rest)
	      (loop (cdr rest)))
	  ;; it's not exported by any opened modules, if we have a handle
	  ;; on the current module (i.e. we're compiling code not in
	  ;; a module definition) try looking in that
	  (if (and (symbolp var) (fluid current-structure)
		   (%structure-bound-p (fluid current-structure) var))
	      (fluid current-module)
	    nil)))))

  (defun symbol-value-1 (var)
    (cond ((and (symbolp var) (special-variable-p var))
	   (symbol-value var))
	  ((and (symbolp var) (fluid current-structure)
		(%structure-bound-p (fluid current-structure) var))
	   (%structure-ref (fluid current-structure) var))
	  (t
	   (let ((struct (locate-variable var)))
	     (and struct (%structure-ref (%intern-structure struct)
					 (if (consp var)
					     (nth 2 var)	;structure-ref
					   var)))))))

  ;; if possible, return the value of variable VAR, else return nil
  (defun compiler-symbol-value (var)
    (let ((value (symbol-value-1 var)))
      ;; if the value is an autoload, try to load it
      (if (and (closurep value)
	       (eq (car (closure-function value)) 'autoload))
	  (%load-autoload value)
	value)))

  ;; return t if the binding of VAR comes from the rep (built-ins) module
  (defun compiler-binding-from-rep-p (var)
    (if (structure-ref-p var)
	(eq (nth 1 var) 'rep)
      (and (not (or (memq var (fluid spec-bindings))
		    (assq var (fluid lex-bindings))))
	   (eq (locate-variable var) 'rep))))

  ;; return t if the binding of VAR is a known constant
  ;; (not including those in comp-constant-env)
  (defun compiler-binding-immutable-p (var)
    (and (not (or (memq var (fluid spec-bindings))
		  (assq var (fluid lex-bindings))))
	 (let
	     ((struct (locate-variable var)))
	   (and struct (binding-immutable-p
			(if (consp var)
			    (nth 2 var)	;structure-ref
			  var)
			(%intern-structure struct))))))

  (defun get-language-property (prop)
    (and (fluid current-language) (get (fluid current-language) prop)))

  (defun get-procedure-handler (name prop-name)
    (unless (or (memq name (fluid spec-bindings))
		(assq name (fluid lex-bindings)))
      (let*
	  ((struct (locate-variable name))
	   (prop (and struct (get struct prop-name))))
	(if (and prop (symbolp prop))
	    (get (if (consp name)
		     (nth 2 name)	;structure-ref
		   name) prop)
	  prop))))

  (defun compiler-macroexpand-1 (form)
    (when (consp form)
      (let* ((def (assq (car form) (fluid macro-env)))
	     ;; make #<subr macroexpand> pass us any inner expansions
	     (macro-environment compiler-macroexpand))
	(if def
	    (setq form (apply (cdr def) (cdr form)))
	  (setq def (compiler-symbol-value (car form)))
	  (when (and (eq (car def) 'macro) (functionp (cdr def)))
	    (setq form (apply (cdr def) (cdr form)))))))
    form)

  (defun compiler-macroexpand (form &optional pred)
    (let loop ((in form))
      (let
	  ((out (compiler-macroexpand-1 in)))
	;;(format standard-error "in: %S, out: %S\n" in out)
	(if ((or pred eq) in out)
	    out
	  (loop out)))))

  ;; if OPENED or ACCESSED are `t', the current values are used
  (defun compile-module-body (body name opened accessed)
    (fluid-let ((macro-env (fluid default-macro-env))
		(current-module (or name (fluid current-module)))
		(current-structure (if name nil (fluid current-structure)))
		(current-language (fluid current-language))
		(open-modules (if (eq opened t)
				  (fluid open-modules)
				opened))
		(accessed-modules (if (eq accessed t)
				      (fluid accessed-modules)
				    accessed))
		(const-env nil)
		(inline-env nil)
		(defuns nil)
		(defvars (fluid defvars))
		(defines nil)
		(lexically-pure t)
		(output-stream nil))

    (find-language-module)
    (let
	;; find language pass-1 and pass-2 compilers
	((pass-1 (get-language-property 'compiler-pass-1))
	 (pass-2 (get-language-property 'compiler-pass-2))
	 next-body)

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
      body)))

  (defun note-require (feature)
    (unless (memq feature (fluid open-modules))
      (cond ((%get-structure feature)
	     (fluid-set open-modules (cons feature (fluid open-modules))))
	    ((fluid current-structure)
	     (unless (eval `(featurep ',feature) (fluid current-structure))
	       (eval `(require ',feature) (fluid current-structure))
	       (when (%get-structure feature)
		 (fluid-set open-modules
			    (cons feature (fluid open-modules))))))
	    (t
	     ;; XXX this doesn't work, no alternative..?
	     (require feature)
	     (when (%get-structure feature)
	       (fluid-set open-modules (cons feature
					     (fluid open-modules))))))))

  ;; XXX enclose macro defs in the *root-structure*, this is different
  ;; to with interpreted code
  (defun note-macro-def (name body)
    (fluid-set macro-env (cons (cons name
				     (%make-closure-in-structure
				      body (%get-structure *root-structure*)))
			       (fluid macro-env))))

  (defun call-with-module-declared (struct thunk)
    (fluid-let ((current-module (%structure-name struct))
		(current-structure struct)
		(current-language nil))
      (fluid-let ((open-modules (and (fluid current-structure)
				     (%structure-imports
				      (fluid current-structure))))
		  (accessed-modules (and (fluid current-structure)
					 (%structure-accessible
					  (fluid current-structure)))))
	(find-language-module)
	(thunk))))

  (defun find-language-module ()
    ;; scan all opened modules, then scan any they import, ..
    (catch 'out
      (let ((tocheck (list (list (fluid current-module))
			   (fluid open-modules))))
	(while tocheck
	  (mapc (lambda (struct)
		  (if (get struct 'compiler-module)
		      (progn
			(%intern-structure (get struct 'compiler-module))
			(fluid-set current-language struct)
			(throw 'out))
		    (when (%get-structure struct)
		      (setq tocheck (nconc tocheck
					   (list (%structure-imports
						  (%get-structure struct))))))))
		(car tocheck))
	  (setq tocheck (cdr tocheck)))
	(compiler-warning
	 "unknown language dialect for module" (fluid current-module)))))



;;; declarations

  ;; (declare (in-module STRUCT))

  (defun declare-in-module (form)
    (fluid-set current-module (cadr form))
    (fluid-set current-structure (%get-structure (fluid current-module))))
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
	 (config (car body))
	 header)

      (setq body (cdr body))
      (unless (listp (car config))
	(setq config (list config)))
      (mapc (lambda (clause)
	      (case (car clause)
		((open)
		 (setq opened (nconc (reverse (cdr clause)) opened))
		 (setq header (cons `(%open-structures ',(cdr clause))
				    header)))

		((access)
		 (setq accessed (nconc (reverse (cdr clause)) accessed))
		 (setq header (cons `(%access-structures ',(cdr clause))
				    header)))))
	    config)
      (setq header (cons '(%open-structures '(module-system))
			 (nreverse header)))

      (setq body (compile-module-body body name opened accessed))
      (compile-form-1 '%make-structure)
      (compile-form-1 `(%parse-interface ',sig))
      (if header
	  (progn
	    (compile-constant `(lambda () ,@header))
	    (emit-insn (bytecode enclose))
	    (note-closure-made))
	(compile-constant nil))
      (if body
	  (progn
	    (compile-constant `(lambda () ,@body))
	    (emit-insn (bytecode enclose))
	    (note-closure-made))
	(compile-constant nil))
      (when name
	(compile-constant name))
      (emit-insn (bytecode call) (if name 4 3))
      (decrement-stack (if name 4 3))))

  (defun parse-interface (sig)
    (cond ((null sig) '())

	  ((eq (car sig) 'export)
	   (cdr sig))

	  ((eq (car sig) 'compound-interface)
	   (apply 'append (mapcar parse-interface (cdr sig))))

	  ((symbolp sig)
	   (if (boundp sig)
	       (symbol-value sig)
	     (compiler-error "Don't know this interface: %s" sig)))))

  (defun compile-structure-ref (form)
    (let
	((struct (nth 1 form))
	 (var (nth 2 form)))
      (or (memq struct (fluid accessed-modules))
	  (compiler-error "Referencing non-accessible structure" struct))
      (or (module-exports-p struct var)
	  (compiler-error "Referencing non-exported variable" struct var))
      (compile-constant struct)
      (compile-constant var)
      (emit-insn (bytecode structure-ref))
      (decrement-stack)))


;;; exported top-level functions

  (defun compile-module (struct)
    "Compiles all function bindings in the module named STRUCT."
    (interactive "SModule name:")
    (let ((struct (%intern-structure struct)))
      (when struct
	(%structure-walk (lambda (var value)
			   (when (closurep value)
			     (compile-function value))) struct)))))
