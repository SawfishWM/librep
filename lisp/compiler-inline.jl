#| compiler-inline.jl -- function inlining

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

(define-structure compiler-inline (export comp-push-args
					  comp-pop-args
					  comp-compile-lambda-inline
					  comp-compile-inline-function
					  comp-do-tail-call)
  (open rep
	compiler
	compiler-utils
	compiler-basic
	compiler-vars
	compiler-lap
	compiler-bindings
	bytecodes)

  (defun comp-push-args (lambda-list args &optional pushed-args-already tester)
    (let
	((arg-count 0))
      (if (not pushed-args-already)
	  ;; First of all, evaluate each argument onto the stack
	  (while (consp args)
	    (comp-compile-form (car args))
	    (setq args (cdr args)
		  arg-count (1+ arg-count)))
	;; Args already on stack
	(setq args nil
	      arg-count pushed-args-already))
      ;; Now the interesting bit. The args are on the stack, in
      ;; reverse order. So now we have to scan the lambda-list to
      ;; see what they should be bound to.
      (let
	  ((state 'required)
	   (args-left arg-count)
	   (bind-stack '()))
	(mapc tester (comp-get-lambda-vars lambda-list))
	(while lambda-list
	  (cond
	   ((symbolp lambda-list)
	    (setq bind-stack (cons (cons lambda-list args-left) bind-stack))
	    (setq args-left 0))
	   ((consp lambda-list)
	    (if (memq (car lambda-list) '(&optional &rest &aux))
		(setq state (car lambda-list))
	      (cond
	       ((eq state 'required)
		(if (zerop args-left)
		    (comp-error "Required arg missing" (car lambda-list))
		  (setq bind-stack (cons (car lambda-list) bind-stack)
			args-left (1- args-left))))
	       ((eq state '&optional)
		(if (zerop args-left)
		    (progn
		      (comp-write-op (bytecode nil))
		      (comp-inc-stack))
		  (setq args-left (1- args-left)))
		(setq bind-stack (cons (car lambda-list) bind-stack)))
	       ((eq state '&rest)
		(setq bind-stack (cons (cons (car lambda-list) args-left)
				       bind-stack)
		      args-left 0
		      state '&aux))
	       ((eq state '&aux)
		(setq bind-stack (cons (cons (car lambda-list) nil)
				       bind-stack)))))))
	  (setq lambda-list (cdr lambda-list)))
	(when (> args-left 0)
	  (comp-warning "%d unused parameters to lambda expression" args-left))
	(cons args-left bind-stack))))

  (defun comp-pop-args (bind-stack args-left setter)
    ;; Bind all variables
    (while bind-stack
      (if (consp (car bind-stack))
	  (progn
	    (if (null (cdr (car bind-stack)))
		(progn
		  (comp-write-op (bytecode nil))
		  (comp-inc-stack))
	      (comp-write-op (bytecode list) (cdr (car bind-stack)))
	      (comp-dec-stack (cdr (car bind-stack)))
	      (comp-inc-stack))
	    (setter (car (car bind-stack))))
	(setter (car bind-stack)))
      (comp-dec-stack)
      (setq bind-stack (cdr bind-stack)))
    ;; Then pop any args that weren't used.
    (while (> args-left 0)
      (comp-write-op (bytecode pop))
      (comp-dec-stack)
      (setq args-left (1- args-left))))

  ;; This compiles an inline lambda, i.e. FUN is something like
  ;; (lambda (LAMBDA-LIST...) BODY...)
  ;; If PUSHED-ARGS-ALREADY is non-nil it should be a count of the number
  ;; of arguments pushed onto the stack (in reverse order). In this case,
  ;; ARGS is ignored
  (defun comp-compile-lambda-inline (fun args &optional pushed-args-already)
    (when (>= (setq comp-inline-depth (1+ comp-inline-depth))
	      comp-max-inline-depth)
      (setq comp-inline-depth 0)
      (comp-error (format nil "Won't inline more than %d nested functions"
			  comp-max-inline-depth)))
    (unless (eq (car fun) 'lambda)
      (comp-error "Invalid function to inline: %s, %s" fun args))
    (let*
	((lambda-list (nth 1 fun))
	 (body (nthcdr 2 fun))
	 (out (comp-push-args
	       lambda-list args pushed-args-already comp-test-varbind))
	 (args-left (car out))
	 (bind-stack (cdr out))
	 (comp-spec-bindings comp-spec-bindings)
	 (comp-lex-bindings comp-lex-bindings)
	 (comp-lexically-pure comp-lexically-pure)
	 (comp-lambda-name comp-lambda-name))

      ;; Set up the body for compiling, skip any interactive form or
      ;; doc string
      (while (and (consp body) (or (stringp (car body))
				   (and (consp (car body))
					(eq (car (car body)) 'interactive))))
	(setq body (cdr body)))
    
      ;; Now we have a list of things to bind to, in the same order
      ;; as the stack of evaluated arguments. The list has items
      ;; SYMBOL, (SYMBOL . ARGS-TO-BIND), or (SYMBOL . nil)
      (if bind-stack
	  (progn
	    (comp-write-op (bytecode init-bind))
	    (comp-pop-args bind-stack args-left comp-emit-binding)
	    (comp-compile-body body)
	    (comp-write-op (bytecode unbind)))
	;; Nothing to bind to. Just pop the evaluated args and
	;; evaluate the body
	(while (> args-left 0)
	  (comp-write-op (bytecode pop))
	  (comp-dec-stack)
	  (setq args-left (1- args-left)))
	(comp-compile-body body)))
    (setq comp-inline-depth (1- comp-inline-depth)))
  
  ;; The defsubst form stores the defun in the compile-inline property
  ;; of all defsubst declared functions
  (defun comp-compile-inline-function (form)
    (comp-compile-lambda-inline (get (car form) 'compile-inline) (cdr form)))

  (defun comp-do-tail-call (arg-spec args)
    (let*
	((out (comp-push-args arg-spec args nil comp-test-varref))
	 (args-left (car out))
	 (bind-stack (cdr out))
	 (comp-spec-bindings comp-spec-bindings)
	 (comp-lex-bindings comp-lex-bindings)
	 (comp-lexically-pure comp-lexically-pure)
	 (comp-lambda-name comp-lambda-name))
      (comp-write-op (bytecode unbindall-0))
      (comp-write-op (bytecode init-bind))
      (comp-pop-args bind-stack args-left comp-emit-binding)
      (when (> comp-current-stack 0)
	(comp-write-op (bytecode pop-all)))
      (comp-write-op (bytecode jmp) (comp-start-label)))))
