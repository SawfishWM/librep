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

(define-structure compiler-forms (export comp-compile-form
					 comp-compile-body
					 comp-compile-lambda)
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
	compiler-vars
	bytecodes)

  ;; Compile one form so that its value ends up on the stack when interpreted
  (defun comp-compile-form (form &optional return-follows)
    (cond
     ((eq form nil)
      (comp-write-op (bytecode nil))
      (comp-inc-stack))
     ((eq form t)
      (comp-write-op (bytecode t))
      (comp-inc-stack))
     ((comp-variable-p form)
      ;; A variable reference
      (let
	  (val)
	(comp-test-varref form)
	(cond
	 ((setq val (assq form comp-const-env))
	  ;; A constant from this file
	  (comp-compile-constant (cdr val)))
	 ((comp-binding-immutable-p form)
	  ;; A known constant
	  (comp-compile-constant (comp-symbol-value form)))
	 (t
	  ;; Not a constant
	  (if (comp-spec-bound-p form)
	      ;; Specially bound
	      (comp-write-op (bytecode refq) (comp-add-constant form))
	    (let
		((lex-addr (comp-binding-lexical-addr form)))
	      (if lex-addr
		  ;; We know the lexical address, so use it
		  (comp-write-op (bytecode refn) lex-addr)
		;; It's not bound, so just update the global value
		(comp-write-op (bytecode refg) (comp-add-constant form)))))
	  (comp-inc-stack)))))
     ((consp form)
      (let ((new (comp-src-transform form)))
	(if (consp new)
	    (setq form new)
	  (comp-compile-form new)
	  (setq form nil)))
      (unless (null form)
	;; A subroutine application of some sort
	(comp-test-funcall (car form) (length (cdr form)))
	(let
	    (fun)
	  (cond
	   ;; Check if there's a special handler for this function
	   ((and (comp-variable-p (car form))
		 (setq fun (comp-get-procedure-handler
			    (car form) 'compiler-handler-property)))
	    (fun form return-follows))

	   ;; Is it a function to be inlined?
	   ;; XXX broken for module system
	   ((and (symbolp (car form)) (get (car form) 'compile-inline))
	    (comp-compile-inline-function form))

	   (t
	    ;; Expand macros
	    (if (not (eq (setq fun (comp-macroexpand form)) form))
		;; The macro did something, so start again
		(comp-compile-form fun return-follows)
	      ;; No special handler, so do it ourselves
	      (setq fun (car form))
	      (cond
	       ;; XXX assumes usual rep binding of `lambda'
	       ((and (consp fun) (eq (car fun) 'lambda))
		;; An inline lambda expression
		(comp-compile-lambda-inline (car form) (cdr form)))
	       ((and (symbolp fun) (assq fun comp-inline-env))
		;; A call to a function that should be open-coded
		(comp-compile-lambda-inline (cdr (assq fun comp-inline-env))
					    (cdr form)))
	       (t
		;; Assume a normal function call
		(if (and return-follows
			 comp-lexically-pure
			 (eq fun comp-lambda-name))
		    (progn
		      (comp-do-tail-call comp-lambda-args (cdr form))
		      ;; fake it, the next caller will pop the (non-existant)
		      ;; return value
		      (comp-inc-stack))
		  (comp-compile-form fun)
		  (setq form (cdr form))
		  (let
		      ((i 0))
		    (while (consp form)
		      (comp-compile-form (car form))
		      (setq i (1+ i)
			    form (cdr form)))
		    (comp-write-op (bytecode call) i)
		    (comp-dec-stack i)))))))))))
     (t
      ;; Not a variable reference or a function call; so what is it?
      (comp-compile-constant form))))

  ;; Compile a list of forms, the last form's evaluated value is left on
  ;; the stack. If the list is empty nil is pushed.
  (defun comp-compile-body (body &optional return-follows name)
    (if (null body)
	(progn
	  (comp-write-op (bytecode nil))
	  (comp-inc-stack))
      (while (consp body)
	(if (and (null (cdr body)) (comp-constant-function-p (car body)) name)
	    ;; handle named lambdas specially so we track name of current fun
	    (progn
	      (comp-compile-constant
	       (comp-compile-lambda
		(comp-constant-function-value (car body)) name))
	      (comp-write-op (bytecode enclose)))
	  (comp-compile-form (car body) (if (cdr body) nil return-follows)))
	(when (cdr body)
	  (comp-write-op (bytecode pop))
	  (comp-dec-stack))
	(setq body (cdr body)))))

  ;; From LST, `(lambda (ARGS) [DOC-STRING] BODY ...)' returns a byte-code
  ;; vector
  (defun comp-compile-lambda (lst &optional name sequencer)
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
      (when (and comp-write-docs doc name)
	(add-documentation name doc))
      (let
	  ((vars (comp-get-lambda-vars args)))
	(mapc comp-test-varbind vars)
	(when (setq form (let
			     ((comp-spec-bindings comp-spec-bindings)
			      (comp-lex-bindings comp-lex-bindings)
			      (comp-lexically-pure t)
			      (comp-lambda-name name)
			      (comp-lambda-args args))
			   (comp-note-bindings vars)
			   (compile-form (cons (or sequencer 'progn) body))))
	  (make-byte-code-subr args (nth 1 form) (nth 2 form) (nth 3 form)
			       (and (not comp-write-docs) doc) interactive))))))