#| inline.jl -- function inlining

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

(define-structure rep.vm.compiler.inline

    (export compile-lambda-inline
	    compile-tail-call)

    (open rep
	  rep.vm.compiler.utils
	  rep.vm.compiler.basic
	  rep.vm.compiler.modules
	  rep.vm.compiler.lap
	  rep.vm.compiler.bindings)

  (define inline-depth (make-fluid 0))		;depth of lambda-inlining
  (defconst max-inline-depth 64)

  (defun push-inline-args (lambda-list args #!optional pushed-args-already tester)
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
	    (case (car lambda-list)
	      ((#!optional &optional) (setq state 'optional))
	      ((#!rest &rest) (setq state 'rest))
	      ;; XXX implement keyword params
	      ((#!key) (compiler-error "can't inline `#!key' parameters"))
	      (t (case state
		   ((required)
		    (if (zerop args-left)
			(compiler-error "required arg `%s' missing"
					(car lambda-list))
		      (setq bind-stack (cons (car lambda-list) bind-stack)
			    args-left (1- args-left))))
		   ((optional)
		    (if (zerop args-left)
			(let ((def (cdar lambda-list)))
			  (if def
			      (compile-form-1 (car def))
			    (emit-insn '(push ())))
			  (increment-stack))
		      (setq args-left (1- args-left)))
		    (setq bind-stack (cons (or (caar lambda-list)
					       (car lambda-list)) bind-stack)))
		   ((rest)
		    (setq bind-stack (cons (cons (car lambda-list) args-left)
					   bind-stack)
			  args-left 0
			  state '*done*)))))))
	  (setq lambda-list (cdr lambda-list)))
	(when (> args-left 0)
	  (compiler-warning 'parameters
	   "%d unused %s to lambda expression"
	   args-left (if (= args-left 1) "parameter" "parameters")))
	(cons args-left bind-stack))))

  (defun pop-inline-args (bind-stack args-left setter)
    ;; Bind all variables
    (while bind-stack
      (if (consp (car bind-stack))
	  (progn
	    (compile-constant '())
	    (unless (null (cdr (car bind-stack)))
	      (do ((i 0 (1+ i)))
		  ((= i (cdr (car bind-stack))))
		(emit-insn '(cons))
		(decrement-stack)))
	    (setter (car (car bind-stack))))
	(setter (car bind-stack)))
      (decrement-stack)
      (setq bind-stack (cdr bind-stack)))
    ;; Then pop any args that weren't used.
    (while (> args-left 0)
      (emit-insn '(pop))
      (decrement-stack)
      (setq args-left (1- args-left))))

  ;; This compiles an inline lambda, i.e. FUN is something like
  ;; (lambda (LAMBDA-LIST...) BODY...)
  ;; If PUSHED-ARGS-ALREADY is true it should be a count of the number
  ;; of arguments pushed onto the stack (in reverse order). In this case,
  ;; ARGS is ignored
  (defun compile-lambda-inline (fun args #!optional pushed-args-already
				return-follows name)
    (setq fun (compiler-macroexpand fun))
    (when (>= (fluid-set inline-depth (1+ (fluid inline-depth)))
	      max-inline-depth)
      (fluid-set inline-depth 0)
      (compiler-error "can't inline more than %d nested functions"
		      max-inline-depth))
    (let*
	((lambda-list (nth 1 fun))
	 (body (nthcdr 2 fun))
	 (out (push-inline-args
	       lambda-list args pushed-args-already test-variable-bind))
	 (args-left (car out))
	 (bind-stack (cdr out)))

      (call-with-frame
       (lambda ()
	 ;; Set up the body for compiling, skip any interactive form or
	 ;; doc string
	 (while (and (consp body)
		     (or (stringp (car body))
			 (and (consp (car body))
			      (eq (car (car body)) 'interactive))))
	   (setq body (cdr body)))
    
	 ;; Now we have a list of things to bind to, in the same order
	 ;; as the stack of evaluated arguments. The list has items
	 ;; SYMBOL, (SYMBOL . ARGS-TO-BIND), or (SYMBOL . nil)
	 (if bind-stack
	     (progn
	       (emit-insn '(init-bind))
	       (increment-b-stack)
	       (pop-inline-args bind-stack args-left (lambda (x)
						       (note-binding x)
						       (emit-binding x)))
	       (call-with-lambda-record name lambda-list 0
		(lambda ()
		  (fix-label (lambda-label (current-lambda)))
		  (set-lambda-inlined (current-lambda) t)
		  (compile-body body return-follows)))
	       (emit-insn '(unbind))
	       (decrement-b-stack))
	   ;; Nothing to bind to. Just pop the evaluated args and
	   ;; evaluate the body
	   (while (> args-left 0)
	     (emit-insn '(pop))
	     (decrement-stack)
	     (setq args-left (1- args-left)))
	   (call-with-lambda-record name lambda-list 0
	    (lambda ()
	      (fix-label (lambda-label (current-lambda)))
	      (set-lambda-inlined (current-lambda) t)
	      (compile-body body return-follows))))))
      (fluid-set inline-depth (1- (fluid inline-depth)))))

  (define (pop-between top bottom)
    (or (and (>= top bottom) (>= bottom 0))
	(break)
	(error "Invalid stack pointers: %d, %d" top bottom))
    (when (/= top bottom)
      (if (= bottom 0)
	  (emit-insn '(pop-all))
	(do ((sp top (1- sp)))
	    ((= sp bottom))
	  (emit-insn '(pop))))))

  (define (unbind-between top bottom)
    (cond ((= bottom -1) (emit-insn '(unbindall-0)))
	  ((= bottom 0)
	   (unless (<= top bottom)
	     (emit-insn '(unbindall))))
	  (t (do ((bp top (1- bp)))
		 ((<= bp bottom))
	       (emit-insn '(unbind))))))

  (defun compile-tail-call (lambda-record args)
    (let* ((out (push-inline-args (lambda-args lambda-record)
				  args nil test-variable-ref))
	   (args-left (car out))
	   (bind-stack (cdr out)))
      (call-with-frame
       (lambda ()
	 (if (catch 'foo
	       (mapc (lambda (var)
		       (when (binding-enclosed-p var)
			 (throw 'foo t)))
		     (get-lambda-vars (lambda-args lambda-record)))
	       nil)
	     ;; some of the parameter bindings may have been captured,
	     ;; so rebind all of them
	     (progn
	       (unbind-between (fluid current-b-stack)
			       ;; the 1- is so that the frame of
			       ;; the function itself is also removed
			       (1- (lambda-bp lambda-record)))
	       (emit-insn '(init-bind))
	       (pop-inline-args bind-stack args-left emit-binding))
	   ;; none of the bindings are captured, so just modify them
	   (pop-inline-args bind-stack args-left emit-varset)
	   (unbind-between (fluid current-b-stack)
			   (lambda-bp lambda-record)))
	 ;; force the stack pointer to what it should be
	 (pop-between (fluid current-stack) (lambda-sp lambda-record))
	 (emit-insn `(jmp ,(lambda-label lambda-record))))))))
