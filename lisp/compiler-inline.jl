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

(define-structure compiler-inline (export compile-lambda-inline
					  compile-tail-call)
  (open rep
	compiler
	compiler-utils
	compiler-basic
	compiler-modules
	compiler-lap
	compiler-bindings
	bytecodes)

  (define inline-depth (make-fluid 0))		;depth of lambda-inlining
  (defconst max-inline-depth 8)

  (defun push-inline-args (lambda-list args &optional pushed-args-already tester)
    (let
	((arg-count 0))
      (if (not pushed-args-already)
	  ;; First of all, evaluate each argument onto the stack
	  (while (consp args)
	    (compile-form-1 (car args))
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
	(mapc tester (get-lambda-vars lambda-list))
	(while lambda-list
	  (cond
	   ((symbolp lambda-list)
	    (setq bind-stack (cons (cons lambda-list args-left) bind-stack))
	    (setq args-left 0))
	   ((consp lambda-list)
	    (if (memq (car lambda-list) '(&optional &rest))
		(setq state (car lambda-list))
	      (cond
	       ((eq state 'required)
		(if (zerop args-left)
		    (compiler-error "Required arg missing" (car lambda-list))
		  (setq bind-stack (cons (car lambda-list) bind-stack)
			args-left (1- args-left))))
	       ((eq state '&optional)
		(if (zerop args-left)
		    (progn
		      (emit-insn (bytecode nil))
		      (increment-stack))
		  (setq args-left (1- args-left)))
		(setq bind-stack (cons (car lambda-list) bind-stack)))
	       ((eq state '&rest)
		(setq bind-stack (cons (cons (car lambda-list) args-left)
				       bind-stack)
		      args-left 0
		      state '*done*))))))
	  (setq lambda-list (cdr lambda-list)))
	(when (> args-left 0)
	  (compiler-warning
	   "%d unused parameters to lambda expression" args-left))
	(cons args-left bind-stack))))

  (defun pop-inline-args (bind-stack args-left setter)
    ;; Bind all variables
    (while bind-stack
      (if (consp (car bind-stack))
	  (progn
	    (if (null (cdr (car bind-stack)))
		(progn
		  (emit-insn (bytecode nil))
		  (increment-stack))
	      (emit-insn (bytecode list) (cdr (car bind-stack)))
	      (decrement-stack (cdr (car bind-stack)))
	      (increment-stack))
	    (setter (car (car bind-stack))))
	(setter (car bind-stack)))
      (decrement-stack)
      (setq bind-stack (cdr bind-stack)))
    ;; Then pop any args that weren't used.
    (while (> args-left 0)
      (emit-insn (bytecode pop))
      (decrement-stack)
      (setq args-left (1- args-left))))

  ;; This compiles an inline lambda, i.e. FUN is something like
  ;; (lambda (LAMBDA-LIST...) BODY...)
  ;; If PUSHED-ARGS-ALREADY is non-nil it should be a count of the number
  ;; of arguments pushed onto the stack (in reverse order). In this case,
  ;; ARGS is ignored
  (defun compile-lambda-inline (fun args &optional pushed-args-already
				return-follows)
    (setq fun (compiler-macroexpand fun))
    (when (>= (fluid-set inline-depth (1+ (fluid inline-depth)))
	      max-inline-depth)
      (fluid-set inline-depth 0)
      (compiler-error (format nil "Won't inline more than %d nested functions"
			      max-inline-depth)))
    (let*
	((lambda-list (nth 1 fun))
	 (body (nthcdr 2 fun))
	 (out (push-inline-args
	       lambda-list args pushed-args-already test-variable-bind))
	 (args-left (car out))
	 (bind-stack (cdr out)))

      (let-fluids ((spec-bindings (fluid spec-bindings))
		   (lex-bindings (fluid lex-bindings))
		   (lexically-pure (fluid lexically-pure))
		   (lambda-name (fluid lambda-name)))

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
	      (emit-insn (bytecode init-bind))
	      (increment-b-stack)
	      (pop-inline-args bind-stack args-left emit-binding)
	      (compile-body body return-follows)
	      (emit-insn (bytecode unbind))
	      (decrement-b-stack))
	  ;; Nothing to bind to. Just pop the evaluated args and
	  ;; evaluate the body
	  (while (> args-left 0)
	    (emit-insn (bytecode pop))
	    (decrement-stack)
	    (setq args-left (1- args-left)))
	  (compile-body body return-follows)))
      (fluid-set inline-depth (1- (fluid inline-depth)))))
  
  (defun compile-tail-call (arg-spec args)
    (let*
	((out (push-inline-args arg-spec args nil test-variable-ref))
	 (args-left (car out))
	 (bind-stack (cdr out)))
      (let-fluids ((spec-bindings (fluid spec-bindings))
		   (lex-bindings (fluid lex-bindings))
		   (lexically-pure (fluid lexically-pure))
		   (lambda-name (fluid lambda-name)))
	(if (catch 'foo
	      (mapc (lambda (var)
		      (when (binding-captured-p var)
			(throw 'foo t)))
		    (get-lambda-vars arg-spec))
	      nil)
	    ;; some of the parameter bindings have been captured,
	    ;; so rebind all of them
	    (progn
	      (emit-insn (bytecode unbindall-0))
	      (emit-insn (bytecode init-bind))
	      (pop-inline-args bind-stack args-left emit-binding))
	  ;; none of the bindings are captured, so just modify them
	  (pop-inline-args bind-stack args-left emit-varset)
	  (unless (eq (fluid lambda-bindings) (fluid lex-bindings))
	    (emit-insn (bytecode unbindall))))
	(when (> (fluid current-stack) 0)
	  (emit-insn (bytecode pop-all)))
	(emit-insn (bytecode jmp) (get-start-label))))))
