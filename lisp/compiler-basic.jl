#| compiler-forms.jl -- basic compilation

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

(define-structure compiler-basic (export current-file
					 current-fun
					 lambda-name
					 lambda-args
					 lambda-bindings
					 compile-form-1
					 compile-body
					 compile-lambda)
  (open rep
	lisp-doc
	compiler
	compiler-utils
	compiler-bindings
	compiler-modules
	compiler-const
	compiler-src
	compiler-inline
	compiler-lap
	bytecodes)

  (define current-file (make-fluid))		;the file being compiled
  (define current-fun (make-fluid))		;the function being compiled

  (define lambda-name (make-fluid))		;name of current lambda exp
  (define lambda-args (make-fluid))		;arg spec of current lambda
  (define lambda-bindings (make-fluid))	;value of c-l-b at top of lambda

  ;; Compile one form so that its value ends up on the stack when interpreted
  (defun compile-form-1 (form &optional return-follows)
    (cond
     ((eq form nil)
      (emit-insn (bytecode nil))
      (increment-stack))
     ((eq form t)
      (emit-insn (bytecode t))
      (increment-stack))

     ((variable-ref-p form)
      ;; A variable reference
      (let
	  (val)
	(test-variable-ref form)
	(cond
	 ((setq val (assq form (fluid const-env)))
	  ;; A constant from this file
	  (compile-constant (cdr val)))
	 ((compiler-binding-immutable-p form)
	  ;; A known constant
	  (compile-constant (compiler-symbol-value form)))
	 (t
	  ;; Not a constant
	  (if (spec-bound-p form)
	      ;; Specially bound
	      (emit-insn (bytecode refq) (add-constant form))
	    (let
		((lex-addr (binding-lexical-addr form)))
	      (if lex-addr
		  ;; We know the lexical address, so use it
		  (emit-insn (bytecode refn) lex-addr)
		;; It's not bound, so just update the global value
		(emit-insn (bytecode refg) (add-constant form)))))
	  (increment-stack)))))

     ((consp form)
      (let ((new (source-code-transform form)))
	(if (consp new)
	    (setq form new)
	  (compile-form-1 new)
	  (setq form nil)))
      (unless (null form)
	;; A subroutine application of some sort
	(let
	    (fun)
	  (cond
	   ;; Check if there's a special handler for this function
	   ((and (variable-ref-p (car form))
		 (setq fun (get-procedure-handler
			    (car form) 'compiler-handler-property)))
	    (fun form return-follows))

	   ;; Is it a function to be inlined?
	   ;; XXX broken for module system
	   ((and (symbolp (car form)) (get (car form) 'compile-inline))
	    (test-function-call (car form) (length (cdr form)))
	    (compile-inline-function form))

	   (t
	    ;; Expand macros
	    (test-function-call (car form) (length (cdr form)))
	    (if (not (eq (setq fun (compiler-macroexpand form)) form))
		;; The macro did something, so start again
		(compile-form-1 fun return-follows)
	      ;; No special handler, so do it ourselves
	      (setq fun (car form))
	      (cond
	       ;; XXX assumes usual rep binding of `lambda'
	       ((and (consp fun) (eq (car fun) 'lambda))
		;; An inline lambda expression
		(compile-lambda-inline (car form) (cdr form)
				       nil return-follows))
	       ((and (symbolp fun) (assq fun (fluid inline-env)))
		;; A call to a function that should be open-coded
		(compile-lambda-inline (cdr (assq fun (fluid inline-env)))
				       (cdr form) nil return-follows))
	       (t
		;; Assume a normal function call
		(if (and return-follows
			 (fluid lexically-pure)
			 (eq fun (fluid lambda-name))
			 (not (binding-modified-p fun)))
		    (progn
		      (compile-tail-call (fluid lambda-args) (cdr form))
		      ;; fake it, the next caller will pop the (non-existant)
		      ;; return value
		      (increment-stack))
		  (compile-form-1 fun)
		  (setq form (cdr form))
		  (let
		      ((i 0))
		    (while (consp form)
		      (compile-form-1 (car form))
		      (setq i (1+ i)
			    form (cdr form)))
		    (emit-insn (bytecode call) i)
		    (decrement-stack i)))))))))))
     (t
      ;; Not a variable reference or a function call; so what is it?
      (compile-constant form))))

  ;; Compile a list of forms, the last form's evaluated value is left on
  ;; the stack. If the list is empty nil is pushed.
  (defun compile-body (body &optional return-follows name)
    (if (null body)
	(progn
	  (emit-insn (bytecode nil))
	  (increment-stack))
      (while (consp body)
	(if (and (null (cdr body)) (constant-function-p (car body)) name)
	    ;; handle named lambdas specially so we track name of current fun
	    (progn
	      (compile-constant
	       (compile-lambda (constant-function-value (car body)) name))
	      (emit-insn (bytecode enclose))
	      (note-closure-made))
	  (compile-form-1 (car body) (if (cdr body) nil return-follows)))
	(when (cdr body)
	  (emit-insn (bytecode pop))
	  (decrement-stack))
	(setq body (cdr body)))))

  (defun compile-lambda-spec (in vars)
    (if (filter spec-bound-p vars)
	in
      ;; encode the parameter spec to an integer if it
      ;; will only create lexical bindings
      (let ((min-args 0)
	    (max-args 0)
	    (rest-arg nil))
	(while (and (consp in) (not (memq (car in) '(&optional &rest))))
	  (setq min-args (1+ min-args))
	  (setq in (cdr in)))
	(when (eq (car in) '&optional)
	  (setq in (cdr in))
	  (while (and (consp in) (not (eq (car in) '&rest)))
	    (setq max-args (1+ max-args))
	    (setq in (cdr in)))
	  (when (eq (car in) '&optional)
	    (setq in (cdr in))))
	(when (or (eq (car in) '&rest) (and in (not (consp in))))
	  (setq rest-arg t))
	(logior min-args (ash max-args 12) (ash (if rest-arg 1 0) 24)))))

  ;; From LST, `(lambda (ARGS) [DOC-STRING] BODY ...)' returns a byte-code
  ;; vector
  (defun compile-lambda (lst &optional name)
    (let
	((args (nth 1 lst))
	 (body (nthcdr 2 lst))
	 doc interactive form)
      (when (stringp (car body))
	(setq doc (car body))
	(setq body (cdr body)))
      (when (eq (car (car body)) 'interactive)
	;; If we have (interactive), set the interactive spec to t
	;; so that it's not ignored
	(setq interactive (or (car (cdr (car body))) t)
	      body (cdr body))
	;; See if it might be a good idea to compile the interactive decl
	(when (consp interactive)
	  (setq interactive (compile-form interactive))))
      (when (and name doc)
	(setq doc (format nil "\(%s%s\)\n\n%s"
			  name (describe-lambda-list args) doc)))
      (when (and *compiler-write-docs* doc name)
	(add-documentation name doc))
      (let
	  ((vars (get-lambda-vars args)))
	(mapc test-variable-bind vars)
	(when (setq form (let-fluids
			     ((spec-bindings (fluid spec-bindings))
			      (lex-bindings (fluid lex-bindings))
			      (lexically-pure t)
			      (lambda-name name)
			      (lambda-args args)
			      (lambda-bindings nil))
			   (note-bindings (reverse vars))
			   (fluid-set lambda-bindings (fluid lex-bindings))
			   (compile-form (cons (get-language-property
						'compiler-sequencer) body))))
	  (make-byte-code-subr (compile-lambda-spec args vars)
			       (nth 1 form) (nth 2 form) (nth 3 form)
			       (and (not *compiler-write-docs*) doc)
			       interactive))))))
