#| basic.jl -- basic compilation

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

(define-structure rep.vm.compiler.basic

    (export current-file
	    current-fun
	    current-form
	    lambda-records
	    lambda-name lambda-args lambda-depth lambda-bindings
	    lambda-bp lambda-sp
	    lambda-label
	    lambda-stack
	    lambda-inlined set-lambda-inlined
	    current-lambda
	    call-with-lambda-record
	    assembly-code assembly-code-set
	    assembly-slots assembly-slots-set
	    compile-constant compile-form-1 compile-body
	    compile-lambda compile-lambda-constant
	    compile-form
	    assemble-assembly-to-form assemble-assembly-to-subr)

    (open rep
	  rep.lang.doc
	  rep.data.records
	  rep.vm.compiler.utils
	  rep.vm.compiler.bindings
	  rep.vm.compiler.modules
	  rep.vm.compiler.src
	  rep.vm.compiler.inline
	  rep.vm.compiler.lap
	  rep.vm.peephole
	  rep.vm.assembler)

  (defvar *compiler-write-docs* nil
    "When t all doc-strings are appended to the doc file and replaced with
their position in that file.")

  (defvar *compiler-no-low-level-optimisations* nil)

  (defvar *compiler-debug* nil)

  (define current-file (make-fluid))		;the file being compiled
  (define current-fun (make-fluid))		;the function being compiled
  (define current-form (make-fluid))		;the current cons-like form

  (define-record-type :assembly
    (make-assembly code max-stack max-b-stack slots)
    assemblyp
    (code assembly-code assembly-code-set)
    (max-stack assembly-max-stack assembly-max-stack-set)
    (max-b-stack assembly-max-b-stack assembly-max-b-stack-set)
    (slots assembly-slots assembly-slots-set))

  (define-record-type :lambda-record
    (make-lambda-record name args depth sp bp label)
    lambda-record-p
    (name lambda-name)			;name of the lambda exp or ()
    (args lambda-args)			;arg spec of the lambda
    (depth lambda-depth)		;depth of physical bytecode
    (sp lambda-sp)			;value of current-stack at top
    (bp lambda-bp)			;value of current-b-stack at top
    (label lambda-label)		;label for code (after binding init)
    ;; t when inlined (from a letrec)
    (inlined lambda-inlined set-lambda-inlined))

  ;; list of lambda records
  (define lambda-stack (make-fluid))


;;; lambda management

  (define (find-lambda name)
    (let loop ((rest (fluid lambda-stack)))
      (cond ((null rest) nil)
	    ((eq (lambda-name (car rest)) name) (car rest))
	    (t (loop (cdr rest))))))

  (define (call-with-lambda-record name args depth-delta thunk)
    (let* ((label (make-label))
	   (depth (if (fluid lambda-stack)
		      (+ (lambda-depth (current-lambda)) depth-delta)
		    0))
	   (lr (make-lambda-record name args depth
				   (fluid current-stack)
				   (fluid current-b-stack) label)))
      (let-fluids ((lambda-stack (cons lr (fluid lambda-stack))))
	(thunk))))

  (define (current-lambda)
    (or (car (fluid lambda-stack)) (error "No current lambda!")))


;;; driver function

  ;; stop macroexpanding if we come across a function with a special handler
  (defun macroexpand-pred (in out)
    (or (eq in out)
	(and (variable-ref-p (car out))
	     (get-procedure-handler
	      (car out) 'compiler-handler-property))))

  (define (compile-constant value)
    (emit-insn `(push ,value))
    (increment-stack))

  (define (inlinable-call-p fun return-follows)
    (let ((tem (find-lambda fun)))
      (and tem
	   (or (lambda-inlined tem)
	       (and (fluid lexically-pure)
		    return-follows
		    (not (binding-modified-p fun))))
	   (= (lambda-depth tem) (lambda-depth (current-lambda)))
	   (lambda-label tem))))

  ;; Compile one form so that its value ends up on the stack when interpreted
  (defun compile-form-1 (form #!key return-follows in-tail-slot)
    (cond
     ((eq form '())
      (emit-insn '(push ()))
      (increment-stack))
     ((eq form t)
      (emit-insn '(push t))
      (increment-stack))

     ((symbolp form)
      ;; A variable reference
      (let (val)
	(test-variable-ref form)
	(cond
	 ((keywordp form)
	  (compile-constant form))
	 ((setq val (assq form (fluid const-env)))
	  ;; A constant from this file
	  (compile-constant (cdr val)))
	 ((compiler-binding-immutable-p form)
	  ;; A known constant
	  (compile-constant (compiler-symbol-value form)))
	 (t
	  ;; Not a constant
	  (emit-varref form in-tail-slot)
	  (increment-stack)))))

       ((consp form)
	(let-fluids ((current-form form))
	  (let ((new (source-code-transform form)))
	    (if (consp new)
		(setq form new)
	      (compile-form-1 new)
	      (setq form nil)))
	  (unless (null form)
	    ;; A subroutine application of some sort
	    (let (fun)
	      (cond
	       ;; Check if there's a special handler for this function
	       ((and (variable-ref-p (car form))
		     (setq fun (get-procedure-handler
				(car form) 'compiler-handler-property)))
		(fun form return-follows))

	       (t
		;; Expand macros
		(test-function-call (car form) (length (cdr form)))
		(if (not (eq (setq fun (compiler-macroexpand
					form macroexpand-pred)) form))
		    ;; The macro did something, so start again
		    (compile-form-1 fun #:return-follows return-follows)
		  ;; No special handler, so do it ourselves
		  (setq fun (car form))
		  (cond
		   ;; XXX assumes usual rep binding of `lambda'
		   ((and (consp fun) (eq (car fun) 'lambda))
		    ;; An inline lambda expression
		    (compile-lambda-inline (car form) (cdr form)
					   nil return-follows))

		   ;; Assume a normal function call

		   ((inlinable-call-p fun return-follows)
		    ;; an inlinable tail call
		    (note-binding-referenced fun t)
		    (compile-tail-call (find-lambda fun) (cdr form))
		    ;; fake it, the next caller will pop the (non-existant)
		    ;; return value
		    (increment-stack))

		   ((and (symbolp fun)
			 (cdr (assq fun (fluid inline-env)))
			 (not (find-lambda fun)))
		    ;; A call to a function that should be open-coded
		    (compile-lambda-inline (cdr (assq fun (fluid inline-env)))
					   (cdr form) nil return-follows fun))
		   (t
		    (compile-form-1
		     fun #:in-tail-slot (inlinable-call-p fun return-follows))
		    (setq form (cdr form))
		    (let ((i 0))
		      (while (consp form)
			(compile-form-1 (car form))
			(setq i (1+ i)
			      form (cdr form)))
		      (emit-insn `(call ,i))
		      (note-function-call-made)
		      (decrement-stack i)))))))))))
       (t
	;; Not a variable reference or a function call; so what is it?
	(compile-constant form))))

  ;; Compile a list of forms, the last form's evaluated value is left on
  ;; the stack. If the list is empty nil is pushed.
  (defun compile-body (body #!optional return-follows name)
    (if (null body)
	(progn
	  (emit-insn '(push ()))
	  (increment-stack))
      (while (consp body)
	(if (and (null (cdr body)) (constant-function-p (car body)) name)
	    ;; handle named lambdas specially so we track name of current fun
	    (compile-lambda-constant (constant-function-value (car body)) name)
	  (compile-form-1
	   (car body) #:return-follows (if (cdr body) nil return-follows)))
	(when (cdr body)
	  (emit-insn '(pop))
	  (decrement-stack))
	(setq body (cdr body)))))


;;; creating assembly code

  (define (call-with-initial-env thunk)
    (let-fluids ((current-stack 0)
		 (max-stack 0)
		 (current-b-stack 0)
		 (max-b-stack 0)
		 (intermediate-code '())
		 (lexically-pure t))
      (thunk)))

  (define (get-assembly)
    (let ((asm (make-assembly (nreverse (fluid intermediate-code))
			      (fluid max-stack) (fluid max-b-stack) 0)))
      asm))

  ;; returns (ASM MAX-STACK MAX-B-STACK)
  (define (compile-form-to-asm form #!optional start-label)
    (call-with-initial-env
     (lambda ()
       ;; Do the high-level compilation
       (when start-label
	 (fix-label start-label))
       (compile-form-1 form #:return-follows t)
       (emit-insn '(return))
       (let ((asm (get-assembly)))
	 (allocate-bindings asm)
	 asm))))

  (defun compile-lambda-spec (in)
    (let loop ((rest in)
	       (state 'required)
	       (vars '()))
      (cond ((null rest)
	     ;; emit the bindings now
	     (do ((rest vars (cdr rest)))
		 ((null rest))
	       (note-binding (car rest))
	       (emit-binding (car rest))
	       (decrement-stack)))
	    ((symbolp rest)
	     (test-variable-bind rest)
	     (emit-insn '(rest-arg))
	     (increment-stack)
	     (loop '() nil (cons rest vars)))
	    (t (case (car rest)
		 ((#!optional) (loop (cdr rest) 'optional vars))
		 ((#!key) (loop (cdr rest) 'key vars))
		 ((#!rest) (loop (cdr rest) 'rest vars))
		 ((&optional)
		  (compiler-deprecated '&optional "&optional in lambda list")
		  (loop (cdr rest) 'optional vars))
		 ((&rest)
		  (compiler-deprecated '&rest "&rest in lambda list")
		  (loop (cdr rest) 'rest vars))
		 (t (let ((var (or (caar rest) (car rest)))
			  (default (cdar rest))
			  (pushed 0))
		      (test-variable-bind var)
		      (when (eq state 'key)
			(compile-constant (make-keyword var))
			(setq pushed (1+ pushed)))
		      (emit-insn (case state
				   ((required) '(required-arg))
				   ((optional) (if default
						   '(optional-arg*)
						 '(optional-arg)))
				   ((key) (if default
					      '(keyword-arg*)
					    '(keyword-arg)))
				   ((rest) '(rest-arg))))
		      (if (and (memq state '(optional key)) default)
			  (progn
			    (increment-stack (- 2 pushed))
			    (let ((label (make-label)))
			      (emit-insn `(jt ,label))
			      (decrement-stack 2)
			      (compile-form-1 (car default))
			      (fix-label label)))
			(decrement-stack pushed)
			(increment-stack))
		      (loop (cdr rest) state (cons var vars)))))))))

  ;; From LST, `(lambda (ARGS) BODY ...)' returns an assembly code object
  (defun compile-lambda-to-asm (lst #!optional name)
    (let ((args (nth 1 lst))
	  (body (nthcdr 2 lst)))
      (call-with-initial-env
       (lambda ()
	 (call-with-lambda-record name args +1
	  (lambda ()
	    (call-with-frame
	     (lambda ()
	       (compile-lambda-spec args)
	       (fix-label (lambda-label (current-lambda)))
	       (compile-body body t)
	       (emit-insn '(return))
	       (get-assembly)))))))))

  (define (optimize-assembly asm)
    (when *compiler-debug*
      (format standard-error "lap-0 code: %S\n\n" (assembly-code asm)))
    ;; Unless disabled, run the peephole optimiser
    (unless *compiler-no-low-level-optimisations*
      (let ((tem (peephole-optimizer (assembly-code asm))))
	(assembly-code-set asm (car tem))
	(assembly-max-stack-set asm (+ (assembly-max-stack asm) (cdr tem)))))
    (when *compiler-debug*
      (format standard-error "lap-1 code: %S\n\n" (assembly-code asm))))

  (define (assemble-assembly-to-form asm)
    (optimize-assembly asm)
    (let ((object-code (assemble (assembly-code asm)))) ;(CODE . CONSTS)
      (list 'run-byte-code
	    (car object-code) (cdr object-code)
	    (+ (assembly-max-stack asm)
	       (ash (assembly-max-b-stack asm) 10)
	       (ash (assembly-slots asm) 20)))))

  (define (assemble-assembly-to-subr asm #!optional doc interactive)
    (optimize-assembly asm)
    (let ((object-code (assemble (assembly-code asm)))) ;(CODE . CONSTS)
      (make-byte-code-subr (car object-code) (cdr object-code)
			   (+ (assembly-max-stack asm)
			      (ash (assembly-max-b-stack asm) 10)
			      (ash (assembly-slots asm) 20))
			   (and (not *compiler-write-docs*) doc)
			   interactive)))

  (define (compile-form form)
    "Compile the Lisp form FORM into a byte code form."
    (call-with-lambda-record nil '() +1
     (lambda ()
       (assemble-assembly-to-form
	(compile-form-to-asm form (lambda-label (current-lambda)))))))


;;; compiling lambdas

  ;; From LST, `(lambda (ARGS) [DOC-STRING] BODY ...)' returns a byte-code
  ;; vector
  (defun compile-lambda (lst #!optional name)
    (let ((args (nth 1 lst))
	  (body (nthcdr 2 lst))
	  doc interactive)
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
      (when (and *compiler-write-docs* doc name)
	(add-documentation name (fluid current-module) doc)
	(add-documentation-params name (fluid current-module) args))

      (let ((asm (compile-lambda-to-asm `(lambda ,args ,@body) name)))
	(allocate-bindings asm)
	(assemble-assembly-to-subr asm doc interactive))))

  (defun compile-lambda-constant (lst #!optional name)
    (let ((args (nth 1 lst))
	  (body (nthcdr 2 lst))
	  doc interactive)
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
      (when (and *compiler-write-docs* doc name)
	(add-documentation name (fluid current-module) doc)
	(add-documentation-params name (fluid current-module) args))

      ;; push a pseudo instruction. All details of the bindings may
      ;; not yet be known. So allocate-bindings function will recursively
      ;; call itself for pushed bytecode
      (emit-insn `(push-bytecode
		   ,(compile-lambda-to-asm `(lambda ,args ,@body) name)
		   ,(fluid lex-bindings) ,doc ,interactive))
      (emit-insn '(enclose))
      (increment-stack)
      (note-closure-made))))
