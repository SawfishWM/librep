#| modules.jl -- module handling for the compiler

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

(declare (unsafe-for-call/cc))

(define-structure rep.vm.compiler.modules

    (export current-module
	    macro-env
	    variable-ref-p
	    locate-variable
	    compiler-symbol-value
	    compiler-boundp
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
	    compile-top-level-structure
	    compile-top-level-define-structure
	    compile-structure-ref
	    compile-function
	    compile-module)

    (open rep
	  rep.structures
	  rep.vm.compiler.basic
	  rep.vm.compiler.bindings
	  rep.vm.compiler.utils
	  rep.vm.compiler.lap)

  (define macro-env (make-fluid '()))		;alist of (NAME . MACRO-DEF)
  (define default-macro-env (make-fluid '()))

;;; module environment of form being compiled

  ;; the name of the module being compiled in
  (define current-module (make-fluid *user-structure*))

  ;; if true, the namespace of the module being compiled in; only
  ;; set when compiling code outside a module definition
  (define current-structure (make-fluid
			     (get-structure (fluid current-module))))

  (define current-language (make-fluid 'rep))

  ;; the names of the currently open and accessed modules
  (define open-modules (make-fluid (and (fluid current-structure)
					(structure-imports
					 (fluid current-structure)))))
  (define accessed-modules (make-fluid (and (fluid current-structure)
					    (structure-accessible
					     (fluid current-structure)))))

;;; functions

  (define (find-structure name)
    (condition-case nil
	(intern-structure name)
      (file-error nil)))

  ;; return t if the module called STRUCT exports a variable called VAR
  (defun module-exports-p (struct var)
    (and (symbolp var)
	 (cond ((symbolp struct)
		(let ((tem (find-structure struct)))
		  (and tem (structure-exports-p tem var))))
	       ((structurep struct)
		(structure-exports-p struct var)))))

  ;; return t if ARG is a structure reference form
  (defun structure-ref-p (arg)
    (and (eq (car arg) 'structure-ref)
	 (memq (locate-variable 'structure-ref) '(rep rep.module-system))))

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
		   (structure-bound-p (fluid current-structure) var))
	      (fluid current-module)
	    nil)))))

  (defun variable-stem (var)
    (if (consp var)
	(nth 2 var)			;structure-ref
      var))

  (defun symbol-value-1 (var)
    (cond ((and (symbolp var) (special-variable-p var))
	   (symbol-value var))
	  ((and (symbolp var) (fluid current-structure)
		(structure-bound-p (fluid current-structure) var))
	   (%structure-ref (fluid current-structure) var))
	  ((has-local-binding-p var) nil)
	  (t
	   (let* ((struct (locate-variable var))
		  (module (and struct (find-structure struct))))
	     (and module
		  (structure-bound-p module (variable-stem var))
		  (%structure-ref module (variable-stem var)))))))

  ;; if possible, return the value of variable VAR, else return nil
  (defun compiler-symbol-value (var)
    (let ((value (symbol-value-1 var)))
      ;; if the value is an autoload, try to load it
      (if (and (closurep value)
	       (eq (car (closure-function value)) 'autoload))
	  (load-autoload value)
	value)))

  (defun compiler-boundp (var)
    (and (symbolp var)
	 (or (locate-variable var)
	     (and (special-variable-p var) (boundp var)))))

  ;; return t if the binding of VAR comes from the rep (built-ins) module
  (defun compiler-binding-from-rep-p (var)
    (if (structure-ref-p var)
	(eq (nth 1 var) 'rep)
      (and (not (has-local-binding-p var))
	   (eq (locate-variable var) 'rep))))

  ;; return t if the binding of VAR is a known constant
  ;; (not including those in comp-constant-env)
  (defun compiler-binding-immutable-p (var)
    (and (not (has-local-binding-p var))
	 (let ((struct (locate-variable var)))
	   (and struct (binding-immutable-p (variable-stem var)
					    (find-structure struct))))))

  (defun get-language-property (prop)
    (and (fluid current-language) (get (fluid current-language) prop)))

  (defun get-procedure-handler (name prop-name)
    (unless (has-local-binding-p name)
      (let*
	  ((struct (locate-variable name))
	   (prop (and struct (get struct prop-name))))
	(if (and prop (symbolp prop))
	    (get (variable-stem name) prop)
	  prop))))

  (defun compiler-macroexpand-1 (form)
    (when (and (consp form)
	       (symbolp (car form))
	       (not (has-local-binding-p (car form))))
      (let* ((def (assq (car form) (fluid macro-env)))
	     ;; make #<subr macroexpand> pass us any inner expansions
	     (macro-environment compiler-macroexpand-1))
	(if def
	    (setq form (apply (cdr def) (cdr form)))
	  (setq def (compiler-symbol-value (car form)))
	  (when (and (eq (car def) 'macro) (functionp (cdr def)))
	    (when (and (closurep (cdr def))
		       (eq (car (closure-function (cdr def))) 'autoload))
	      (setq def (load-autoload (cdr def))))
	    (setq form (apply (cdr def) (cdr form)))))))
    form)

  (defun compiler-macroexpand (form #!optional pred)
    (let loop ((in form))
      (let
	  ((out (compiler-macroexpand-1 in)))
	;;(format standard-error "in: %S, out: %S\n" in out)
	(if ((or pred eq) in out)
	    out
	  (loop out)))))

  ;; if OPENED or ACCESSED are `t', the current values are used
  (defun call-with-module-env (thunk opened accessed)
    (let-fluids ((macro-env (fluid default-macro-env))
		 (current-module (fluid current-module))
		 (current-structure (fluid current-structure))
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
      (thunk)))

  (defun compile-module-body-1 (body)
    (find-language-module)
    (let
	;; find language pass-1 and pass-2 compilers
	((pass-1 (get-language-property 'compiler-pass-1))
	 (pass-2 (get-language-property 'compiler-pass-2)))

      ;; pass 1. remember definitions in the body for pass 2
      (when pass-1
	(setq body (pass-1 body)))

      ;; pass 2. the actual compilation
      (when pass-2
	(setq body (pass-2 body)))

      ;; return the compiled representation of the body
      body))

  (defun compile-module-body (body opened accessed)
    (call-with-module-env
     (lambda () (compile-module-body-1 body))
     opened accessed))

  (defun note-require (feature)
    (unless (or (memq feature (fluid open-modules))
		(and (fluid current-structure)
		     (eval `(featurep ',feature) (fluid current-structure))))
      ;; XXX this is broken; there's no way to tell if we're trying
      ;; XXX to load a module, or a bare file.
      (cond ((get-structure feature)
	     ;; structure already loaded..
	     (fluid-set open-modules (cons feature (fluid open-modules))))

	    ((fluid current-structure)
	     ;; try to require it..
	     (eval `(require ',feature) (fluid current-structure))
	     (when (get-structure feature)
	       (fluid-set open-modules (cons feature (fluid open-modules)))))

	    ;; no current structure, try to load the file
	    ;; as a module..
	    ((intern-structure feature)
	     (fluid-set open-modules (cons feature (fluid open-modules))))

	    (t (compiler-warning "unable to require `%s'" feature)))))

  ;; XXX enclose macro defs in the *user-structure*, this is different
  ;; to with interpreted code
  (defun note-macro-def (name body)
    (fluid-set macro-env
	       (cons (cons name
			   (let ((closure (make-closure body)))
			     (set-closure-structure
			      closure (get-structure *user-structure*))
			     closure))
		     (fluid macro-env))))

  (defun call-with-structure (thunk struct)
    (let-fluids ((current-module (structure-name struct))
		 (current-structure struct)
		 (current-language nil))
      (let-fluids ((open-modules (and (fluid current-structure)
				      (structure-imports
				       (fluid current-structure))))
		   (accessed-modules (and (fluid current-structure)
					  (structure-accessible
					   (fluid current-structure)))))
	(find-language-module)
	(thunk))))

  (defun find-language-module ()
    ;; scan all opened modules for a known language
    (catch 'out
      (mapc (lambda (struct)
	      (if (get struct 'compiler-module)
		  (progn
		    (or (intern-structure (get struct 'compiler-module))
			(compiler-error "unable to load module `%s'"
					(get struct 'compiler-module)))
		    (fluid-set current-language struct)
		    (throw 'out))))
	    (fluid open-modules))
      (fluid-set current-language 'no-lang)))


;;; declarations

  ;; (declare (in-module STRUCT))

  (defun declare-in-module (form)
    (fluid-set current-module (cadr form))
    (fluid-set current-structure (intern-structure (fluid current-module))))
  (put 'in-module 'compiler-decl-fun declare-in-module)

  ;; (declare (language LANG))

  (defun declare-language (form)
    (fluid-set current-language (cadr form)))
  (put 'language 'compiler-decl-fun declare-language)


;;; module compilers

  (defun compile-structure (form)
    (compile-structure-def nil (cadr form) (cddr form)))

  (defun compile-define-structure (form)
    (compile-structure-def (cadr form) (caddr form) (cdddr form)))

  (defun compile-top-level-structure (form)
    (compile-structure-def nil (cadr form) (cddr form) t))

  (defun compile-top-level-define-structure (form)
    (compile-structure-def (cadr form) (caddr form) (cdddr form) t))

  (defun compile-structure-def (name sig body #!optional top-level)
    (let
	((opened '(rep.module-system))
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
		 (setq header (cons clause header)))

		((access)
		 (setq accessed (nconc (reverse (cdr clause)) accessed))
		 (setq header (cons clause header)))

		(t (setq header (cons clause header)))))
	    config)
      (setq header (cons '(open rep.module-system) (nreverse header)))

      (let-fluids ((current-structure nil)
		   (current-module name))
	(call-with-module-env
	 (lambda ()
	   (setq body (compile-module-body-1 body))

	   (if top-level
	       (if name
		   `(define-structure ,name ,sig ,config ,@body)
		 `(structure ,sig ,config ,@body))
	     (compile-form-1 '%make-structure)
	     (compile-form-1 `(%parse-interface ',sig))
	     (if header
		 (progn
		   (compile-constant `(lambda () ,@header))
		   (emit-insn '(enclose)))
	       (compile-constant nil))
	     (if body
		 ;; compile non-top-level structure bodies, so that
		 ;; they can access the active bindings
		 (compile-lambda-constant `(lambda () ,@body))
	       (compile-constant nil))
	     (when name
	       (compile-constant name))
	     (emit-insn `(call ,(if name 4 3)))
	     (note-function-call-made)
	     (decrement-stack (if name 4 3))))
	 opened accessed))))

  (defun compile-structure-ref (form)
    (let
	((struct (nth 1 form))
	 (var (nth 2 form)))
      (or (memq struct (fluid accessed-modules))
	  (memq struct (fluid open-modules))
	  (compiler-error
	   "referencing non-accessible structure `%s'" struct))
      (or (module-exports-p struct var)
	  (compiler-error
	   "referencing private variable `%s#%s'" struct var))
      (compile-constant struct)
      (compile-constant var)
      (emit-insn '(structure-ref))
      (decrement-stack)))


;;; exported top-level functions

  (defun compile-function (function #!optional name)
    "Compiles the body of the function FUNCTION."
    (interactive "aFunction to compile:")
    (let-fluids ((defuns nil)
		 (defvars nil)
		 (defines nil)
		 (current-fun function)
		 (output-stream nil))
      (let ((body (closure-function function)))
	(unless (bytecodep body)
	  (call-with-structure
	   (lambda ()
	     (set-closure-function function (compile-lambda body name)))
	   (closure-structure function)))
	function)))

  (defun compile-module (struct)
    "Compiles all function bindings in the module named STRUCT."
    (interactive "SModule name:")
    (let ((struct (intern-structure struct)))
      (when struct
	(structure-walk (lambda (var value)
			  (when (closurep value)
			    (compile-function value var))) struct)))))
