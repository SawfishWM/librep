#| compiler-utils.jl -- 

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

(define-structure compiler-utils (export comp-message
					 comp-error
					 comp-warning
					 comp-remember-fun
					 comp-remember-var
					 comp-remember-lexical-var
					 comp-test-varref
					 comp-test-varbind
					 comp-test-funcall
					 comp-inc-stack
					 comp-dec-stack
					 comp-get-lambda-vars
					 comp-constant-p
					 comp-constant-value
					 comp-constant-function-p
					 comp-constant-function-value)
  (open rep
	compiler
	compiler-modules
	compiler-vars
	bytecodes)


;;; Message output

  (defun comp-message (fmt &rest args)
    (when (null comp-output-stream)
      (if (or batch-mode (not (featurep 'jade)))
	  (setq comp-output-stream (stdout-file))
	(setq comp-output-stream (open-buffer "*compilation-output*"))))
    (when (and (featurep 'jade)
	       (bufferp comp-output-stream)
	       (not (eq (current-buffer) comp-output-stream)))
      (goto-buffer comp-output-stream)
      (goto (end-of-buffer)))
    (when comp-current-fun
      (format comp-output-stream "%s:" comp-current-fun))
    (apply format comp-output-stream fmt args))

  (put 'compile-error 'error-message "Compilation mishap")
  (defun comp-error (&rest data)
    (signal 'compile-error data))

  (defun comp-warning (fmt &rest args)
    (apply comp-message fmt args)
    (write comp-output-stream "\n"))


;;; Code to handle warning tests

  ;; Note that there's a function or macro NAME with lambda-list ARGS
  ;; in the current file
  (defun comp-remember-fun (name args)
    (if (assq name comp-defuns)
	(comp-warning "Multiply defined function or macro: %s" name)
      (let
	  ((required 0)
	   (optional nil)
	   (rest nil)
	   (state 'required))
	;; Scan the lambda-list for the number of required and optional
	;; arguments, and whether there's a &rest clause
	(while args
	  (if (symbolp args)
	      ;; (foo . bar)
	      (setq rest t)
	    (if (memq (car args) '(&optional &rest &aux))
		(cond
		 ((eq (car args) '&optional)
		  (setq state 'optional)
		  (setq optional 0))
		 ((eq (car args) '&rest)
		  (setq args nil)
		  (setq rest t))
		 ((eq (car args) '&aux)
		  (setq args nil)))
	      (set state (1+ (symbol-value state)))))
	  (setq args (cdr args)))
	(setq comp-defuns (cons (list name required optional rest)
				comp-defuns)))))

  ;; Similar for variables
  (defun comp-remember-var (name)
    (cond ((memq name comp-defines)
	   (comp-error "Variable %s was previously declared lexically" name))
	  ((memq name comp-defvars)
	   (comp-warning "Multiply defined variable: %s" name))
	  (t
	   (setq comp-defvars (cons name comp-defvars)))))

  (defun comp-remember-lexical-var (name)
    (cond ((memq name comp-defvars)
	   (comp-error "Variable %s was previously declared special" name))
	  ((memq name comp-defines)
	   (comp-warning "Multiply defined lexical variable: %s" name))
	  (t
	   (setq comp-defines (cons name comp-defines)))))

  ;; Test that a reference to variable NAME appears valid
  (defun comp-test-varref (name)
    (when (and (symbolp name)
	       (null (memq name comp-defvars))
	       (null (memq name comp-defines))
	       (null (memq name comp-spec-bindings))
	       (null (memq name comp-lex-bindings))
	       (null (assq name comp-defuns))
	       (not (special-variable-p name))
	       (not (boundp name))
	       (not (comp-locate-var name)))
      (comp-warning "Reference to undeclared free variable: %s" name)))

  ;; Test that binding to variable NAME appears valid
  (defun comp-test-varbind (name)
    (cond ((assq name comp-defuns)
	   (comp-warning "Binding to %s shadows function" name))
	  ;((memq name comp-defvars)
	  ; (comp-warning "Binding to %s shadows special variable" name))
	  ((or (memq name comp-spec-bindings)
	       (memq name comp-lex-bindings))
	   (comp-warning "Binding to %s shadows earlier binding" name))
	  ((and (boundp name) (functionp (symbol-value name)))
	   (comp-warning "Binding to %s shadows pre-defined value" name))))

  ;; Test a call to NAME with NARGS arguments
  ;; XXX functions in comp-fun-bindings aren't type-checked
  (defun comp-test-funcall (name nargs)
    (when (symbolp name)
      (catch 'return
	(let
	    ((decl (assq name comp-defuns)))
	  (when (and (null decl) (or (boundp name)
				     (assq name comp-inline-env)))
	    (setq decl (or (cdr (assq name comp-inline-env))
			   (symbol-value name)))
	    (when (or (subrp decl)
		      (and (closurep decl)
			   (eq (car (closure-function decl)) 'autoload)))
	      (throw 'return))
	    (when (eq (car decl) 'macro)
	      (setq decl (cdr decl)))
	    (when (closurep decl)
	      (setq decl (closure-function decl)))
	    (if (bytecodep decl)
		(comp-remember-fun name (aref decl 0))
	      (comp-remember-fun name (nth 1 decl)))
	    (setq decl (assq name comp-defuns)))
	  (if (null decl)
	      (unless (or (memq name comp-spec-bindings)
			  (memq name comp-lex-bindings)
			  (memq name comp-defvars)
			  (memq name comp-defines)
			  (comp-locate-var name))
		(comp-warning "Call to undeclared function: %s" name))
	    (let
		((required (nth 1 decl))
		 (optional (nth 2 decl))
		 (rest (nth 3 decl)))
	      (if (< nargs required)
		  (comp-warning "%d arguments required by %s; %d supplied"
				required name nargs)
		(when (and (null rest) (> nargs (+ required (or optional 0))))
		  (comp-warning "Too many arguments to %s (%d given, %d used)"
				name nargs
				(+ required (or optional 0)))))))))))


;;; stack handling

  ;; Increment the current stack size, setting the maximum stack size if
  ;; necessary
  (defmacro comp-inc-stack (&optional n)
    (list 'when (list '> (list 'setq 'comp-current-stack
			       (if n
				   (list '+ 'comp-current-stack n)
				 (list '1+ 'comp-current-stack)))
			 'comp-max-stack)
	  '(setq comp-max-stack comp-current-stack)))

  ;; Decrement the current stack usage
  (defmacro comp-dec-stack (&optional n)
    (list 'setq 'comp-current-stack 
	  (if n
	      (list '- 'comp-current-stack n)
	    (list '1- 'comp-current-stack))))



  ;; Remove all keywords from a lambda list ARGS, returning the list of
  ;; variables that would be bound (in the order they would be bound)
  (defun comp-get-lambda-vars (args)
    (let
	(vars)
      (while args
	(if (symbolp args)
	    (setq vars (cons args vars))
	  (unless (memq (car args) '(&optional &rest &aux))
	    (setq vars (cons (car args) vars))))
	(setq args (cdr args)))
      (nreverse vars)))


;;; constant forms

;; Return t if FORM is a constant
(defun comp-constant-p (form)
  (cond
   ((or (integerp form) (stringp form)
	(vectorp form) (bytecodep form)
	(eq form t) (eq form nil)))
   ((consp form)
    (and (eq (car form) 'quote) (comp-binding-from-rep-p 'quote)))
   ((symbolp form)
    (or (assq form comp-const-env)
	(comp-binding-immutable-p form)))
   ;; What other constant forms have I missed..?
   (t
    nil)))

;; If FORM is a constant, return its value
(defun comp-constant-value (form)
  (cond
   ((or (integerp form) (stringp form)
	(vectorp form) (bytecodep form)
	(eq form t) (eq form nil))
    ;; Self-evaluating types
    form)
   ((consp form)
    ;; only quote
    (nth 1 form))
   ((symbolp form)
    (if (comp-binding-immutable-p form)
	(comp-symbol-value form)
      (cdr (assq form comp-const-env))))))

(defun comp-constant-function-p (form)
  (setq form (comp-macroexpand form))
  (and (memq (car form) '(lambda function))
       (comp-binding-from-rep-p (car form))))

(defun comp-constant-function-value (form)
  (setq form (comp-macroexpand form))
  (cond ((eq (car form) 'lambda)
	 form)
	((eq (car form) 'function)
	 (nth 1 form))))

)
