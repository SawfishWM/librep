#| compiler-rep.jl -- inliners for many rep language features

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

(define-structure compiler-rep (export)

  (open rep
	lisp-doc
	compiler
	compiler-modules
	compiler-utils
	compiler-basic
	compiler-const
	compiler-inline
	compiler-lap
	compiler-bindings
	bytecodes)

  ;; List of side-effect-free functions. They should always return the
  ;; same value when given the same inputs. Used when constant folding.
  (define constant-functions
    '(+ - * / % mod max min 1+ 1- car cdr assoc assq rassoc rassq nth nthcdr
      last member memq arrayp aref substring concat length elt lognot not
      logior logxor logand equal = /= > < >= <= ash zerop null atom consp
      listp numberp integerp stringp vectorp bytecodep functionp macrop
      special-form-p subrp sequencep string-head-eq string-equal
      string-lessp string-match string-looking-at quote-regexp
      complete-string time-later-p alpha-char-p upper-case-p lower-case-p
      digit-char-p alphanumericp space-char-p char-upcase char-downcase
      quotient floor ceiling truncate round exp log sin cos tan asin acos
      atan sqrt expt prin1-to-string read-from-string assoc-regexp
      string= string< nop identity caar cdar cadr cddr caaar cdaar
      cadar cddar caadr cdadr caddr cdddr positivep negativep oddp
      evenp abs lcm % modulo lsh string-upper-case-p string-lower-case-p
      string-capitalized-p))

  ;; List of symbols, when the name of the function called by a top-level
  ;; form is one of these that form is compiled.
  (define top-level-compiled
    '(if cond when unless let let* letrec catch unwind-protect condition-case
      progn prog1 prog2 while and or case define-structure structure))

  ;; List of symbols, when the car of a top-level form is a member of this
  ;; list, don't macroexpand the form before compiling.
  (define top-level-unexpanded
    '(defun defmacro defvar defconst defsubst require define-value
      declare eval-when-compile define-structure structure))

  ;; setup properties to tell the compiler where to look for symbols
  ;; in the `rep'  package
  (put 'rep 'compiler-handler-property 'rep-compile-fun)
  (put 'rep 'compiler-transform-property 'rep-compile-transform)

  (put 'rep 'compiler-sequencer 'progn)


;;; pass 1 support

  (defun pass-1 (forms)
    ;; merge adjacent progn forms
    (let loop ((rest (mapcar do-pass-1 forms))
	       (out '()))
      (cond ((null rest)
	     (nreverse out))

	    ((and (eq (caar out) 'progn) (eq (caar rest) 'progn))
	     (rplaca out (nconc (car out) (cdar rest)))
	     (loop (cdr rest) out))

	    (t (loop (cdr rest) (cons (car rest) out))))))

  (defun do-pass-1 (form)
    (unless (or (memq (car form) top-level-unexpanded)
		(memq (car form) top-level-compiled))
      (setq form (compiler-macroexpand
		  form (lambda (in out)
			 (or (eq in out)
			     (memq (car out) top-level-unexpanded)
			     (memq (car out) top-level-compiled))))))
    (case (car form)
      ((defun)
       (remember-function (nth 1 form) (nth 2 form)))

      ((defmacro)
       (remember-function (nth 1 form) (nth 2 form))
       (note-macro-def (nth 1 form) (cons 'lambda (nthcdr 2 form))))

      ((defsubst)
       (fluid-set inline-env (cons (cons (nth 1 form)
					 (cons 'lambda (nthcdr 2 form)))
				   (fluid inline-env))))

      ((defvar)
       (remember-variable (nth 1 form)))

      ((defconst)
       (remember-variable (nth 1 form))
       (fluid-set const-env (cons (cons (nth 1 form) (nth 2 form))
				  (fluid const-env))))

      ((define-value)
       (let
	   ((sym (nth 1 form)))
	 (when (compiler-constant-p sym)
	   (remember-lexical-variable
	    (compiler-constant-value sym)))))

      ((require)
       (if (compiler-constant-p (cadr form))
	   (note-require (compiler-constant-value (cadr form)))
	 ;; hmm..
	 (eval form)))

      ((declare)
       (note-declaration (cdr form)))

      ((eval-when-compile)
       (if (and (eq (car (nth 1 form)) 'require)
		(compiler-constant-p (cadr (nth 1 form))))
	   (note-require (compiler-constant-value (cadr (nth 1 form))))
	 (eval (nth 1 form))))

      ((progn)
       (setq form (cons 'progn (pass-1 (cdr form)))))

      ;; put bare forms into progns so they can be merged in pass-1
      (t (unless (memq (car form) top-level-unexpanded)
	   (setq form (list 'progn form)))))

    form)

  (put 'rep 'compiler-pass-1 pass-1)


;;; pass 2 support

  (defun pass-2 (forms)
    (let loop ((rest forms)
	       (out '()))
      (if (null rest)
	  (nreverse out)
	(loop (cdr rest) (cons (do-pass-2 (car rest)) out)))))

  (defun do-pass-2 (form)
    (case (car form)
      ((defun)
       (let ((tmp (assq (nth 1 form) (fluid macro-env))))
	 (let-fluids ((current-fun (nth 1 form)))
	   (when tmp
	     (rplaca tmp nil)
	     (rplacd tmp nil))
	   (list 'defun (nth 1 form)
		 (compile-lambda (cons 'lambda (nthcdr 2 form))
				 (nth 1 form))))))

      ((defmacro)
       (let
	   ((code (compile-lambda (cons 'lambda (nthcdr 2 form)) (nth 1 form)))
	    (tmp (assq (nth 1 form) (fluid macro-env))))
	 (let-fluids ((current-fun (nth 1 form)))
	   (if tmp
	       (rplacd tmp (make-closure code))
	     (compiler-error
	      "Compiled macro wasn't in environment" (nth 1 form)))
	   (list 'defmacro (nth 1 form) code))))

      ((defsubst)
       (when *compiler-write-docs*
	 (cond
	  ((stringp (nth 3 form))
	   (add-documentation (nth 1 form) (nth 3 form))
	   (setq form (delq (nth 3 form) form)))
	  ((stringp (nth 4 form))
	   (add-documentation (nth 1 form) (nth 4 form))
	   (setq form (delq (nth 4 form) form)))))
       (unless (assq (nth 1 form) (fluid inline-env))
	 (compiler-error "Inline function wasn't in environment" (nth 1 form)))
       form)

      ((defconst)
       (let
	   ((value (eval (nth 2 form)))
	    (doc (nth 3 form)))
	 (when (and *compiler-write-docs* (stringp doc))
	   (add-documentation (nth 1 form) doc)
	   (setq form (delq (nth 3 form) form)))
	 (unless (memq (nth 1 form) (fluid defvars))
	   (remember-variable (nth 1 form)))
	 (unless (assq (nth 1 form) (fluid const-env))
	   (compiler-warning "Constant wasn't in environment" (nth 1 form))))
       form)

      ((defvar)
       (let
	   ((value (nth 2 form))
	    (doc (nth 3 form)))
	 (when (and (listp value)
		    (not (compiler-constant-p value)))
	   ;; Compile the definition. A good idea?
	   (rplaca (nthcdr 2 form) (compile-form (nth 2 form))))
	 (when (and *compiler-write-docs* (stringp doc))
	   (add-documentation (nth 1 form) doc)
	   (setq form (delq (nth 3 form) form)))
	 (unless (memq (nth 1 form) (fluid defvars))
	   (remember-variable (nth 1 form))))
       form)

      ((define-value)
       (let
	   ((sym (nth 1 form))
	    (value (nth 2 form)))
	 (when (compiler-constant-p sym)
	   (setq sym (compiler-constant-value sym))
	   (unless (memq sym (fluid defines))
	     (remember-lexical-variable (compiler-constant-value sym))))
	 (when (and (listp value) (not (compiler-constant-p value)))
	   ;; Compile the definition. A good idea?
	   (rplaca (nthcdr 2 form) (compile-form (nth 2 form))))
	 form))

      ((define-structure)
       (compile-top-level-define-structure form))

      ((structure)
       (compile-top-level-structure form))

      ((eval-when-compile) nil)

      (t (if (memq (car form) top-level-compiled)
	     (compile-form form)
	   form))))

  (put 'rep 'compiler-pass-2 pass-2)


;;; Source code transformations. These are basically macros that are only
;;; used at compile-time.

  ;; tells the constant-folder which functions can be removed
  (defun foldablep (name)
    (memq name constant-functions))
  (put 'rep 'compiler-foldablep foldablep)

  (defun trans-setq (form)
    (let
	(lst)
      (setq form (cdr form))
      (while form
	(unless (consp (cdr form))
	  (compiler-error "Odd number of args to setq"))
	(setq lst (cons `(set ',(car form) ,(nth 1 form)) lst))
	(setq form (nthcdr 2 form)))
      (cons 'progn (nreverse lst))))
  (put 'setq 'rep-compile-transform trans-setq)

  (defun trans-defvar (form)
    (let
	((name (nth 1 form))
	 (value (nth 2 form))
	 (doc (nth 3 form)))
      (remember-variable name)
      `(progn
	 (when ,doc
	   (put ',name 'variable-documentation ,doc))
	 (unless (boundp ',name)
	   (setq ,name ,value)))))
  (put 'defvar 'rep-compile-transform trans-defvar)

  (defun trans-require (form)
    (let
	((feature (nth 1 form)))
      (when (compiler-constant-p feature)
	(note-require (compiler-constant-value feature)))
      ;; Must transform to something other than (require FEATURE) to
      ;; prevent infinite regress
      `(funcall require ,feature)))
  (put 'require 'rep-compile-transform trans-require)

  (defun trans-/= (form)
    `(not (= ,@(cdr form))))
  (put '/= 'rep-compile-transform trans-/=)


;;; Functions which compile non-standard functions (ie special-forms)

  ;; module compilers from compiler-modules
  (put 'structure 'rep-compile-fun compile-structure)
  (put 'define-structure 'rep-compile-fun compile-define-structure)
  (put 'structure-ref 'rep-compile-fun compile-structure-ref)

  (defun compile-declare (form)
    (note-declaration (cdr form)))
  (put 'declare 'rep-compile-fun compile-declare)

  (defun compile-quote (form)
    (compile-constant (car (cdr form))))
  (put 'quote 'rep-compile-fun compile-quote)

  (defun compile-function (form)
    (compile-form-1 (cadr form)))
  (put 'function 'rep-compile-fun compile-function)

  (defun compile-lambda-form (form)
    (compile-constant (compile-lambda form))
    (emit-insn (bytecode enclose))
    (note-closure-made))
  (put 'lambda 'rep-compile-fun compile-lambda-form)

  (defun compile-while (form)
    (let
	((top-label (make-label))
	 (test-label (make-label)))
      (emit-jmp-insn (bytecode jmp) test-label)
      (fix-label top-label)
      (compile-body (nthcdr 2 form))
      (emit-insn (bytecode pop))
      (decrement-stack)
      (fix-label test-label)
      (compile-form-1 (nth 1 form))
      (emit-jmp-insn (bytecode jpt) top-label)))
  (put 'while 'rep-compile-fun compile-while)

  ;; Compile mapc specially if we can open code the function call
  (defun compile-mapc (form)
    (let
	((fun (nth 1 form))
	 (lst (nth 2 form)))
      (if (constant-function-p fun)
	  ;; We can open code the function
	  (let
	      ((top-label (make-label))
	       (test-label (make-label)))
	    (setq fun (constant-function-value fun))
	    (compile-form-1 lst)
	    (emit-jmp-insn (bytecode jmp) test-label)
	    (fix-label top-label)
	    (emit-insn (bytecode dup))
	    (increment-stack)
	    (emit-insn (bytecode car))
	    (compile-lambda-inline fun nil 1)
	    (emit-insn (bytecode pop))
	    (decrement-stack)
	    (emit-insn (bytecode cdr))
	    (fix-label test-label)
	    ;; I don't have a jump-if-t-but-never-pop instruction, so
	    ;; make one out of "jpt TOP; nil". If I ever get a peep hole
	    ;; optimiser working, the nil should be fodder for it..
	    (emit-jmp-insn (bytecode jtp) top-label)
	    (emit-insn (bytecode nil)))
	;; The function must be called, so just use the mapc opcode
	(compile-form-1 fun)
	(compile-form-1 lst)
	(emit-insn (bytecode mapc))
	(decrement-stack))))
  (put 'mapc 'rep-compile-fun compile-mapc)

  (defun compile-progn (form &optional return-follows)
    (compile-body (cdr form) return-follows))
  (put 'progn 'rep-compile-fun compile-progn)

  (defun compile-prog1 (form)
    (compile-form-1 (nth 1 form))
    (compile-body (nthcdr 2 form))
    (emit-insn (bytecode pop))
    (decrement-stack))
  (put 'prog1 'rep-compile-fun compile-prog1)

  (defun compile-set (form)
    (let
	((fun (car form))
	 (sym (nth 1 form))
	 (val (nth 2 form)))
      (if (compiler-constant-p sym)
	  ;; use setq
	  (progn
	    (setq sym (compiler-constant-value sym))
	    (compile-form-1 val)
	    (emit-insn (bytecode dup))
	    (increment-stack)
	    (emit-varset sym)
	    (decrement-stack))
	(compile-form-1 sym)
	(compile-form-1 val)
	(emit-insn (bytecode set))
	(decrement-stack))))
  (put 'set 'rep-compile-fun compile-set)

  ;; compile let* specially to coalesce all bindings into a single frame
  (defun compile-let* (form &optional return-follows)
    (let
	((lst (car (cdr form))))
      (let-fluids ((spec-bindings (fluid spec-bindings))
		   (lex-bindings (fluid lex-bindings))
		   (lexically-pure (fluid lexically-pure))
		   (lambda-name (fluid lambda-name)))
	(emit-insn (bytecode init-bind))
	(increment-b-stack)
	(while (consp lst)
	  (cond
	   ((consp (car lst))
	    (let
		((tmp (car lst)))
	      (compile-body (cdr tmp))
	      (test-variable-bind (car tmp))
	      (emit-binding (car tmp))))
	   (t
	    (emit-insn (bytecode nil))
	    (increment-stack)
	    (test-variable-bind (car lst))
	    (emit-binding (car lst))))
	  (decrement-stack)
	  (setq lst (cdr lst)))
	(compile-body (nthcdr 2 form) return-follows)
	(emit-insn (bytecode unbind))
	(decrement-b-stack))))
  (put 'let* 'rep-compile-fun compile-let*)

  ;; let can be compiled straight from its macro definition

  ;; compile letrec specially to handle tail recursion elimination
  (defun compile-letrec (form &optional return-follows)
    (let
	((bindings (car (cdr form))))
      (let-fluids ((spec-bindings (fluid spec-bindings))
		   (lex-bindings (fluid lex-bindings))
		   (lexically-pure (fluid lexically-pure))
		   (lambda-name (fluid lambda-name)))
	(emit-insn (bytecode init-bind))
	(increment-b-stack)
	;; create the bindings, should really be to void values, but use nil..
	(mapc (lambda (cell)
		(let
		    ((var (or (car cell) cell)))
		  (test-variable-bind var)
		  (compile-constant nil)
		  (emit-binding var)
		  (decrement-stack))) bindings)
	;; then set them to their values
	(mapc (lambda (cell)
		(let
		    ((var (or (car cell) cell)))
		  (compile-body (cdr cell) nil var)
		  (emit-varset var)
		  (decrement-stack))) bindings)
	(compile-body (nthcdr 2 form) return-follows)
	(emit-insn (bytecode unbind))
	(decrement-b-stack))))
  (put 'letrec 'rep-compile-fun compile-letrec)

  (defun compile-let-fluids (form)
    (let ((bindings (cadr form))
	  (body (cddr form)))
      (let-fluids ((lexically-pure nil))
	;; compile each fluid, value pair onto the stack
	(mapc (lambda (cell)
		(compile-form-1 (car cell))
		(compile-body (cdr cell))) bindings)
	(emit-insn (bytecode init-bind))
	(increment-b-stack)
	(mapc (lambda (unused)
		(emit-insn (bytecode fluid-bind))
		(decrement-stack 2)) bindings)
	(compile-body body)
	(emit-insn (bytecode unbind))
	(decrement-b-stack))))
  (put 'let-fluids 'rep-compile-fun compile-let-fluids)

  (defun compile-defun (form)
    (remember-function (nth 1 form) (nth 2 form))
    (compile-constant
     (compile-lambda (cons 'lambda (nthcdr 2 form)) (nth 1 form)))
    (emit-insn (bytecode enclose))
    (emit-insn (bytecode dup))
    (increment-stack)
    (emit-varset (nth 1 form))
    (decrement-stack))
  (put 'defun 'rep-compile-fun compile-defun)

  (defun compile-defmacro (form)
    (remember-function (nth 1 form) (nth 2 form))
    (compile-constant
     (cons 'macro
	   (compile-lambda (cons 'lambda (nthcdr 2 form)) (nth 1 form))))
    (emit-insn (bytecode enclose))
    (emit-insn (bytecode dup))
    (increment-stack)
    (emit-varset (nth 1 form))
    (decrement-stack))
  (put 'defmacro 'rep-compile-fun compile-defmacro)

  (defun compile-cond (form &optional return-follows)
    (let
	((end-label (make-label))
	 (need-trailing-nil t))
      (setq form (cdr form))
      (while (consp form)
	(let*
	    ((subl (car form))
	     (condition (car subl))
	     (next-label (make-label)))
	  ;; See if we can squash a constant condition to t or nil
	  (when (compiler-constant-p condition)
	    (setq condition (not (not (compiler-constant-value condition)))))
	  (cond
	   ((eq condition t)
	    ;; condition t -- always taken
	    (if (consp (cdr subl))
		;; There's something besides the condition
		(progn
		  (compile-body (cdr subl) return-follows)
		  (decrement-stack))
	      (if (eq condition (car subl))
		  (emit-insn (bytecode t))
		(compile-form-1 (car subl) return-follows)
		(decrement-stack)))
	    (when (consp (cdr form))
	      (compiler-warning
	       "Unreachable conditions after t in cond statement")
	      ;; Ignore the rest of the statement
	      (setq form nil))
	    (setq need-trailing-nil nil))
	   ((eq condition nil)
	    ;; condition nil -- never taken
	    (when (cdr subl)
	      (compiler-warning
	       "Unreachable forms after nil in cond statement")))
	   (t
	    ;; non t-or-nil condition
	    (compile-form-1 (car subl))
	    (decrement-stack)
	    (if (consp (cdr subl))
		;; Something besides the condition
		(if (cdr form)
		    ;; This isn't the last condition list
		    (progn
		      (emit-jmp-insn (bytecode jn) next-label)
		      (compile-body (cdr subl) return-follows)
		      (decrement-stack)
		      (emit-jmp-insn (bytecode jmp) end-label)
		      (fix-label next-label))
		  ;; It is the last condition list, use the result
		  ;; of this condition for the return value when it's
		  ;; nil
		  (emit-jmp-insn (bytecode jnp) end-label)
		  (compile-body (cdr subl) return-follows)
		  (decrement-stack)
		  (setq need-trailing-nil nil))
	      ;; No action to take
	      (if (cdr form)
		  ;; This isn't the last condition list
		  (emit-jmp-insn (bytecode jtp) end-label)
		;; This is the last condition list, since there's no
		;; action to take, just fall out the bottom, with the
		;; condition as value.
		(setq need-trailing-nil nil))))))
	(setq form (cdr form)))
      (when need-trailing-nil
	(emit-insn (bytecode nil)))
      (increment-stack)
      (fix-label end-label)))
  (put 'cond 'rep-compile-fun compile-cond)

  (defun compile-case (form &optional return-follows)
    (let
	((end-label (make-label)))
      (setq form (cdr form))
      (unless form
	(compiler-error "No key value in case statement"))
      ;; XXX if key is constant optimise case away..
      (compile-form-1 (car form))
      (setq form (cdr form))
      (while (consp form)
	(unless (consp form)
	  (compiler-error "Badly formed clause in case statement"))
	(let
	    ((cases (caar form))
	     (forms (cdar form))
	     (next-label (make-label)))
	  (cond ((consp cases)
		 (emit-insn (bytecode dup))
		 (increment-stack)
		 (if (consp (cdr cases))
		     ;; >1 possible case
		     (progn
		       (compile-constant cases)
		       (emit-insn (bytecode memql)))
		   ;; only one case, use eql
		   (compile-constant (car cases))
		   (emit-insn (bytecode eql)))
		 (decrement-stack)
		 (emit-jmp-insn (bytecode jn) next-label)
		 (decrement-stack))
		((not (eq cases t))
		 (compiler-error "Badly formed clause in case statement")))
	  (compile-body forms return-follows)
	  (decrement-stack)
	  (emit-jmp-insn (bytecode jmp) end-label)
	  (fix-label next-label)
	  (setq form (cdr form))))
      (increment-stack)
      (fix-label end-label)
      (emit-insn (bytecode swap))
      (emit-insn (bytecode pop))))
  (put 'case 'rep-compile-fun compile-case)

  (defun compile-catch (form)
    (let
	((catch-label (make-label))
	 (start-label (make-label))
	 (end-label (make-label)))
    (let-fluids ((lexically-pure nil))

      ;;		jmp start
      (emit-jmp-insn (bytecode jmp) start-label)

      ;; catch:
      ;;		catch TAG
      ;;		ejmp end
      (increment-stack)			;enter with one arg on stack
      (fix-label catch-label)
      (compile-form-1 (nth 1 form))
      (emit-insn (bytecode catch))
      (decrement-stack)
      (emit-jmp-insn (bytecode ejmp) end-label)
      (decrement-stack)

      ;; start:
      ;;		push #catch
      ;;		binderr
      ;;		FORMS...
      ;;		unbind
      ;; end:
      (fix-label start-label)
      (push-label-addr catch-label)
      (emit-insn (bytecode binderr))
      (increment-b-stack)
      (decrement-stack)
      (compile-body (nthcdr 2 form))
      (emit-insn (bytecode unbind))
      (decrement-b-stack)
      (fix-label end-label))))
  (put 'catch 'rep-compile-fun compile-catch)

  (defun compile-unwind-pro (form)
    (let
	((cleanup-label (make-label))
	 (start-label (make-label))
	 (end-label (make-label)))
    (let-fluids ((lexically-pure nil))

      ;;		jmp start
      (emit-jmp-insn (bytecode jmp) start-label)

      ;; cleanup:
      ;;		CLEANUP-FORMS
      ;;		pop
      ;;		ejmp end
      ;; [overall, stack +1]
      (increment-stack 2)
      (fix-label cleanup-label)
      (compile-body (nthcdr 2 form))
      (emit-insn (bytecode pop))
      (emit-jmp-insn (bytecode ejmp) end-label)
      (decrement-stack 2)

      ;; start:
      ;;		push #cleanup
      ;;		binderr
      ;;		FORM
      ;;		unbind
      ;;		nil
      ;;		jmp cleanup
      ;; [overall, stack +2]
      (fix-label start-label)
      (push-label-addr cleanup-label)
      (emit-insn (bytecode binderr))
      (increment-b-stack)
      (decrement-stack)
      (compile-form-1 (nth 1 form))
      (emit-insn (bytecode unbind))
      (decrement-b-stack)
      (emit-insn (bytecode nil))
      (decrement-stack)
      (emit-jmp-insn (bytecode jmp) cleanup-label)

      ;; end:
      (fix-label end-label))))
  (put 'unwind-protect 'rep-compile-fun compile-unwind-pro)

  (defun compile-condition-case (form)
    (let
	((cleanup-label (make-label))
	 (start-label (make-label))
	 (end-label (make-label))
	 (handlers (nthcdr 3 form)))
    (let-fluids ((lexically-pure nil))

      ;;		jmp start
      ;; cleanup:
      (emit-jmp-insn (bytecode jmp) start-label)
      (fix-label cleanup-label)

      (increment-stack)		;reach here with one item on stack
      (if (consp handlers)
	  (let-fluids ((spec-bindings (fluid spec-bindings))
		       (lex-bindings (fluid lex-bindings))
		       (lambda-name (fluid lambda-name)))
	    (if (nth 1 form)
		(let ((var (nth 1 form)))
		  (when (spec-bound-p var)
		    (compiler-error
		     "condition-case can only bind lexically: %s" var))
		  (test-variable-bind var)
		  (note-binding var))
	      ;; something always gets bound
	      (note-binding (gensym)))
	    ;; Loop over all but the last handler
	    (while (consp (cdr handlers))
	      (if (consp (car handlers))
		  (let
		      ((next-label (make-label)))
		    ;;		push CONDITIONS
		    ;;		errorpro
		    ;;		jtp next
		    ;;		HANDLER
		    ;;		jmp end
		    ;; next:
		    (compile-constant (car (car handlers)))
		    (emit-insn (bytecode errorpro))
		    (decrement-stack)
		    (emit-jmp-insn (bytecode jtp) next-label)
		    (decrement-stack)
		    (compile-body (cdr (car handlers)))
		    (emit-jmp-insn (bytecode jmp) end-label)
		    (fix-label next-label))
		(compiler-error "Badly formed condition-case handler"))
	      (setq handlers (cdr handlers)))
	    ;; The last handler
	    (if (consp (car handlers))
		(let
		    ((pc-label (make-label)))
		  ;;		push CONDITIONS
		  ;;		errorpro
		  ;;		ejmp pc
		  ;; pc:	HANDLER
		  ;;		jmp end
		  (compile-constant (car (car handlers)))
		  (emit-insn (bytecode errorpro))
		  (decrement-stack)
		  (emit-jmp-insn (bytecode ejmp) pc-label)
		  (fix-label pc-label)
		  (decrement-stack)
		  (compile-body (cdr (car handlers)))
		  (emit-jmp-insn (bytecode jmp) end-label))
	      (compiler-error "Badly formed condition-case handler")))
	(compiler-error "No handlers in condition-case"))
      (decrement-stack)

      ;; start:
      ;;		push cleanup
      ;;		binderr
      ;;		FORM
      (fix-label start-label)
      (push-label-addr cleanup-label)
      (emit-insn (bytecode binderr))
      (increment-b-stack)
      (decrement-stack)
      (compile-form-1 (nth 2 form))

      ;; end:
      ;;		unbind			;unbind error handler or VAR
      (fix-label end-label)
      (emit-insn (bytecode unbind))
      (decrement-b-stack))))
  (put 'condition-case 'rep-compile-fun compile-condition-case)

  (defun compile-list (form)
    (let
	((count 0))
      (setq form (cdr form))
      (while (consp form)
	(compile-form-1 (car form))
	(setq
	 count (1+ count)
	 form (cdr form)))
      (emit-insn (bytecode list) count)
      (decrement-stack (1- count))))
  (put 'list 'rep-compile-fun compile-list)

  ;; Funcall normally translates to a single call instruction. However,
  ;; if the function being called is a constant lambda expression, open
  ;; code it.
  (defun compile-funcall (form &optional return-follows)
    (let*
	((fun (nth 1 form))
	 (args (nthcdr 2 form))
	 (arg-count 0)
	 (open-code (constant-function-p fun)))
      (unless open-code
	(compile-form-1 fun))
      (while args
	(compile-form-1 (car args))
	(setq args (cdr args)
	      arg-count (1+ arg-count)))
      (if open-code
	  (progn
	    (compile-lambda-inline
	     (constant-function-value fun) nil arg-count return-follows)
	    ;; We push one less value than when using (bytecode call)
	    (if (zerop arg-count)
		(increment-stack)
	      (decrement-stack (1- arg-count))))
	(emit-insn (bytecode call) arg-count)
	(decrement-stack arg-count))))
  (put 'funcall 'rep-compile-fun compile-funcall)

  (defun compile-nth (form)
    (let
	((insn (cdr (assq (nth 1 form) byte-nth-insns))))
      (if insn
	  (progn
	    (compile-form-1 (nth 2 form))
	    (emit-insn insn))
	(compile-2-args form))))
  (put 'nth 'rep-compile-fun compile-nth)
  (put 'nth 'rep-compile-opcode (bytecode nth))

  (defun compile-nthcdr (form)
    (let
	((insn (assq (nth 1 form) byte-nthcdr-insns)))
      (if insn
	  (progn
	    (compile-form-1 (nth 2 form))
	    (when (cdr insn)
	      (emit-insn (cdr insn))))
	(compile-2-args form))))
  (put 'nthcdr 'rep-compile-fun compile-nthcdr)
  (put 'nthcdr 'rep-compile-opcode (bytecode nthcdr))

  (defun compile-minus (form)
    (if (/= (length form) 2)
	(compile-binary-op form)
      (compile-form-1 (car (cdr form)))
      (emit-insn (bytecode neg))))
  (put '- 'rep-compile-fun compile-minus)
  (put '- 'rep-compile-opcode (bytecode sub))

  (defun compile-make-closure (form)
    (when (nthcdr 3 form)
      (compiler-warning
       "More than two parameters to %s; rest ignored" (car form)))
    (compile-form-1 (nth 1 form))
    (compile-form-1 (nth 2 form))
    (emit-insn (bytecode make-closure))
    (note-closure-made)
    (decrement-stack))
  (put 'make-closure 'rep-compile-fun compile-make-closure)

  (defun get-form-opcode (form)
    (cond ((symbolp form) (get form 'rep-compile-opcode))
	  ;; must be a structure-ref
	  ((eq (car form) 'structure-ref)
	   (get (caddr form) 'rep-compile-opcode))
	  (t (compiler-error "unknown opcode for form: %s" form))))

  ;; Instruction with no arguments
  (defun compile-0-args (form)
    (when (cdr form)
      (compiler-warning "All parameters to %s ignored" (car form)))
    (emit-insn (get-form-opcode (car form)))
    (increment-stack))

  ;; Instruction taking 1 arg on the stack
  (defun compile-1-args (form)
    (when (nthcdr 2 form)
      (compiler-warning
       "More than one parameter to %s; rest ignored" (car form)))
    (compile-form-1 (nth 1 form))
    (emit-insn (get-form-opcode (car form))))

  ;; Instruction taking 2 args on the stack
  (defun compile-2-args (form)
    (when (nthcdr 3 form)
      (compiler-warning
       "More than two parameters to %s; rest ignored" (car form)))
    (compile-form-1 (nth 1 form))
    (compile-form-1 (nth 2 form))
    (emit-insn (get-form-opcode (car form)))
    (decrement-stack))

  ;; Instruction taking 3 args on the stack
  (defun compile-3-args (form)
    (when (nthcdr 4 form)
      (compiler-warning
       "More than three parameters to %s; rest ignored" (car form)))
    (compile-form-1 (nth 1 form))
    (compile-form-1 (nth 2 form))
    (compile-form-1 (nth 3 form))
    (emit-insn (get-form-opcode (car form)))
    (decrement-stack 2))

  ;; Compile a form `(OP ARG1 ARG2 ARG3 ...)' into as many two argument
  ;; instructions as needed (PUSH ARG1; PUSH ARG2; OP; PUSH ARG3; OP; ...)
  (defun compile-binary-op (form)
    (let
	((opcode (get-form-opcode (car form))))
      (setq form (cdr form))
      (unless (>= (length form) 2)
	(compiler-error "Too few args to binary operator" form))
      (compile-form-1 (car form))
      (setq form (cdr form))
      (while (consp form)
	(compile-form-1 (car form))
	(emit-insn opcode)
	(decrement-stack)
	(setq form (cdr form)))))

  ;; Used for >, >=, < and <=
  (defun compile-transitive-relation (form)
    (cond
     ((<= (length form) 2)
      (compiler-error "Too few args to relation" form))
     ((= (length form) 3)
      (let
	  ((opcode (get-form-opcode (car form))))
	;; Simple case, only two arguments, i.e. `(OP ARG1 ARG2)' into:
	;;  PUSH ARG1; PUSH ARG2; OP;
	(compile-form-1 (nth 1 form))
	(compile-form-1 (nth 2 form))
	(emit-insn opcode)
	(decrement-stack)))
     (t
      ;; Tricky case, >2 args,

      ;; Originally I did `(OP ARG1 ARG2 ARG3... ARGN)' as:

      ;;  PUSH ARG1; PUSH ARG2; DUP; SWAP2; OP; JNP Fail;
      ;;  PUSH ARG3; DUP; SWAP2; OP; JNP Fail;
      ;;  ...
      ;;  PUSH ARGN; OP; JMP End;
      ;; Fail:
      ;;  SWAP; POP;
      ;; End:

      ;; But that doesn't always evaluate all arguments..
      (compile-funcall (cons 'funcall form)))))


;;; Opcode properties for the generic instructions, in a progn for compiled
;;; speed

  (progn
    (put 'cons 'rep-compile-fun compile-2-args)
    (put 'cons 'rep-compile-opcode (bytecode cons))
    (put 'car 'rep-compile-fun compile-1-args)
    (put 'car 'rep-compile-opcode (bytecode car))
    (put 'cdr 'rep-compile-fun compile-1-args)
    (put 'cdr 'rep-compile-opcode (bytecode cdr))
    (put 'rplaca 'rep-compile-fun compile-2-args)
    (put 'rplaca 'rep-compile-opcode (bytecode rplaca))
    (put 'rplacd 'rep-compile-fun compile-2-args)
    (put 'rplacd 'rep-compile-opcode (bytecode rplacd))
    (put 'aset 'rep-compile-fun compile-3-args)
    (put 'aset 'rep-compile-opcode (bytecode aset))
    (put 'aref 'rep-compile-fun compile-2-args)
    (put 'aref 'rep-compile-opcode (bytecode aref))
    (put 'length 'rep-compile-fun compile-1-args)
    (put 'length 'rep-compile-opcode (bytecode length))
    (put '+ 'rep-compile-fun compile-binary-op)
    (put '+ 'rep-compile-opcode (bytecode add))
    (put '* 'rep-compile-fun compile-binary-op)
    (put '* 'rep-compile-opcode (bytecode mul))
    (put '/ 'rep-compile-fun compile-binary-op)
    (put '/ 'rep-compile-opcode (bytecode div))
    (put 'remainder 'rep-compile-fun compile-2-args)
    (put 'remainder 'rep-compile-opcode (bytecode rem))
    (put 'mod 'rep-compile-fun compile-2-args)
    (put 'mod 'rep-compile-opcode (bytecode mod))
    (put 'lognot 'rep-compile-fun compile-1-args)
    (put 'lognot 'rep-compile-opcode (bytecode lnot))
    (put 'not 'rep-compile-fun compile-1-args)
    (put 'not 'rep-compile-opcode (bytecode not))
    (put 'logior 'rep-compile-fun compile-binary-op)
    (put 'logior 'rep-compile-opcode (bytecode lor))
    (put 'logxor 'rep-compile-fun compile-binary-op)
    (put 'logxor 'rep-compile-opcode (bytecode lxor))
    (put 'logand 'rep-compile-fun compile-binary-op)
    (put 'logand 'rep-compile-opcode (bytecode land))
    (put 'ash 'rep-compile-fun compile-2-args)
    (put 'ash 'rep-compile-opcode (bytecode ash))
    (put 'equal 'rep-compile-fun compile-2-args)
    (put 'equal 'rep-compile-opcode (bytecode equal))
    (put 'eq 'rep-compile-fun compile-2-args)
    (put 'eq 'rep-compile-opcode (bytecode eq))
    (put '= 'rep-compile-fun compile-transitive-relation)
    (put '= 'rep-compile-opcode (bytecode num-eq))
    (put '> 'rep-compile-fun compile-transitive-relation)
    (put '> 'rep-compile-opcode (bytecode gt))
    (put '< 'rep-compile-fun compile-transitive-relation)
    (put '< 'rep-compile-opcode (bytecode lt))
    (put '>= 'rep-compile-fun compile-transitive-relation)
    (put '>= 'rep-compile-opcode (bytecode ge))
    (put '<= 'rep-compile-fun compile-transitive-relation)
    (put '<= 'rep-compile-opcode (bytecode le))
    (put '1+ 'rep-compile-fun compile-1-args)
    (put '1+ 'rep-compile-opcode (bytecode inc))
    (put '1- 'rep-compile-fun compile-1-args)
    (put '1- 'rep-compile-opcode (bytecode dec))
    (put 'zerop 'rep-compile-fun compile-1-args)
    (put 'zerop 'rep-compile-opcode (bytecode zerop))
    (put 'null 'rep-compile-fun compile-1-args)
    (put 'null 'rep-compile-opcode (bytecode not))
    (put 'atom 'rep-compile-fun compile-1-args)
    (put 'atom 'rep-compile-opcode (bytecode atom))
    (put 'consp 'rep-compile-fun compile-1-args)
    (put 'consp 'rep-compile-opcode (bytecode consp))
    (put 'listp 'rep-compile-fun compile-1-args)
    (put 'listp 'rep-compile-opcode (bytecode listp))
    (put 'numberp 'rep-compile-fun compile-1-args)
    (put 'numberp 'rep-compile-opcode (bytecode numberp))
    (put 'stringp 'rep-compile-fun compile-1-args)
    (put 'stringp 'rep-compile-opcode (bytecode stringp))
    (put 'vectorp 'rep-compile-fun compile-1-args)
    (put 'vectorp 'rep-compile-opcode (bytecode vectorp))
    (put 'throw 'rep-compile-fun compile-2-args)
    (put 'throw 'rep-compile-opcode (bytecode throw))
    (put 'boundp 'rep-compile-fun compile-1-args)
    (put 'boundp 'rep-compile-opcode (bytecode boundp))
    (put 'symbolp 'rep-compile-fun compile-1-args)
    (put 'symbolp 'rep-compile-opcode (bytecode symbolp))
    (put 'get 'rep-compile-fun compile-2-args)
    (put 'get 'rep-compile-opcode (bytecode get))
    (put 'put 'rep-compile-fun compile-3-args)
    (put 'put 'rep-compile-opcode (bytecode put))
    (put 'signal 'rep-compile-fun compile-2-args)
    (put 'signal 'rep-compile-opcode (bytecode signal))
    (put 'quotient 'rep-compile-fun compile-2-args)
    (put 'quotient 'rep-compile-opcode (bytecode quotient))
    (put 'reverse 'rep-compile-fun compile-1-args) ; new 12/7/94
    (put 'reverse 'rep-compile-opcode (bytecode reverse))
    (put 'nreverse 'rep-compile-fun compile-1-args)
    (put 'nreverse 'rep-compile-opcode (bytecode nreverse))
    (put 'assoc 'rep-compile-fun compile-2-args)
    (put 'assoc 'rep-compile-opcode (bytecode assoc))
    (put 'assq 'rep-compile-fun compile-2-args)
    (put 'assq 'rep-compile-opcode (bytecode assq))
    (put 'rassoc 'rep-compile-fun compile-2-args)
    (put 'rassoc 'rep-compile-opcode (bytecode rassoc))
    (put 'rassq 'rep-compile-fun compile-2-args)
    (put 'rassq 'rep-compile-opcode (bytecode rassq))
    (put 'last 'rep-compile-fun compile-1-args)
    (put 'last 'rep-compile-opcode (bytecode last))
    (put 'mapcar 'rep-compile-fun compile-2-args)
    (put 'mapcar 'rep-compile-opcode (bytecode mapcar))
    (put 'member 'rep-compile-fun compile-2-args)
    (put 'member 'rep-compile-opcode (bytecode member))
    (put 'memq 'rep-compile-fun compile-2-args)
    (put 'memq 'rep-compile-opcode (bytecode memq))
    (put 'delete 'rep-compile-fun compile-2-args)
    (put 'delete 'rep-compile-opcode (bytecode delete))
    (put 'delq 'rep-compile-fun compile-2-args)
    (put 'delq 'rep-compile-opcode (bytecode delq))
    (put 'delete-if 'rep-compile-fun compile-2-args)
    (put 'delete-if 'rep-compile-opcode (bytecode delete-if))
    (put 'delete-if-not 'rep-compile-fun compile-2-args)
    (put 'delete-if-not 'rep-compile-opcode (bytecode delete-if-not))
    (put 'copy-sequence 'rep-compile-fun compile-1-args)
    (put 'copy-sequence 'rep-compile-opcode (bytecode copy-sequence))
    (put 'sequencep 'rep-compile-fun compile-1-args)
    (put 'sequencep 'rep-compile-opcode (bytecode sequencep))
    (put 'functionp 'rep-compile-fun compile-1-args)
    (put 'functionp 'rep-compile-opcode (bytecode functionp))
    (put 'special-form-p 'rep-compile-fun compile-1-args)
    (put 'special-form-p 'rep-compile-opcode (bytecode special-form-p))
    (put 'subrp 'rep-compile-fun compile-1-args)
    (put 'subrp 'rep-compile-opcode (bytecode subrp))
    (put 'eql 'rep-compile-fun compile-2-args)
    (put 'eql 'rep-compile-opcode (bytecode eql))
    (put 'max 'rep-compile-fun compile-binary-op)
    (put 'max 'rep-compile-opcode (bytecode max))
    (put 'min 'rep-compile-fun compile-binary-op)
    (put 'min 'rep-compile-opcode (bytecode min))
    (put 'filter 'rep-compile-fun compile-2-args)
    (put 'filter 'rep-compile-opcode (bytecode filter))
    (put 'macrop 'rep-compile-fun compile-1-args)
    (put 'macrop 'rep-compile-opcode (bytecode macrop))
    (put 'bytecodep 'rep-compile-fun compile-1-args)
    (put 'bytecodep 'rep-compile-opcode (bytecode bytecodep))
    (put 'closurep 'rep-compile-fun compile-1-args)
    (put 'closurep 'rep-compile-opcode (bytecode closurep))
    (put 'thread-forbid 'rep-compile-fun compile-0-args)
    (put 'thread-forbid 'rep-compile-opcode (bytecode forbid))
    (put 'thread-permit 'rep-compile-fun compile-0-args)
    (put 'thread-permit 'rep-compile-opcode (bytecode permit))
    (put 'fluid 'rep-compile-fun compile-1-args)
    (put 'fluid 'rep-compile-opcode (bytecode fluid-ref))
    (put 'fluid-set 'rep-compile-fun compile-2-args)
    (put 'fluid-set 'rep-compile-opcode (bytecode fluid-set))

    (put 'caar 'rep-compile-fun compile-1-args)
    (put 'caar 'rep-compile-opcode (bytecode caar))
    (put 'cadr 'rep-compile-fun compile-1-args)
    (put 'cadr 'rep-compile-opcode (bytecode cadr))
    (put 'cdar 'rep-compile-fun compile-1-args)
    (put 'cdar 'rep-compile-opcode (bytecode cdar))
    (put 'cddr 'rep-compile-fun compile-1-args)
    (put 'cddr 'rep-compile-opcode (bytecode cddr))
    (put 'caddr 'rep-compile-fun compile-1-args)
    (put 'caddr 'rep-compile-opcode (bytecode caddr))

    (put 'floor 'rep-compile-fun compile-1-args)
    (put 'floor 'rep-compile-opcode (bytecode floor))
    (put 'ceiling 'rep-compile-fun compile-1-args)
    (put 'ceiling 'rep-compile-opcode (bytecode ceiling))
    (put 'truncate 'rep-compile-fun compile-1-args)
    (put 'truncate 'rep-compile-opcode (bytecode truncate))
    (put 'round 'rep-compile-fun compile-1-args)
    (put 'round 'rep-compile-opcode (bytecode round))
    (put 'exp 'rep-compile-fun compile-1-args)
    (put 'exp 'rep-compile-opcode (bytecode exp))
    (put 'log 'rep-compile-fun compile-1-args)
    (put 'log 'rep-compile-opcode (bytecode log))
    (put 'sin 'rep-compile-fun compile-1-args)
    (put 'sin 'rep-compile-opcode (bytecode sin))
    (put 'cos 'rep-compile-fun compile-1-args)
    (put 'cos 'rep-compile-opcode (bytecode cos))
    (put 'tan 'rep-compile-fun compile-1-args)
    (put 'tan 'rep-compile-opcode (bytecode tan))
    (put 'sqrt 'rep-compile-fun compile-1-args)
    (put 'sqrt 'rep-compile-opcode (bytecode sqrt))
    (put 'expt 'rep-compile-fun compile-2-args)
    (put 'expt 'rep-compile-opcode (bytecode expt))

    ;; some pseudonyms
    (put 'setcar 'rep-compile-fun compile-2-args)
    (put 'setcar 'rep-compile-opcode (bytecode rplaca))
    (put 'setcdr 'rep-compile-fun compile-2-args)
    (put 'setcdr 'rep-compile-opcode (bytecode rplacd))
    (put 'string= 'rep-compile-fun compile-2-args)
    (put 'string= 'rep-compile-opcode (bytecode equal))
    (put 'string< 'rep-compile-fun compile-transitive-relation)
    (put 'string< 'rep-compile-opcode (bytecode lt))
    (put '% 'rep-compile-fun compile-2-args)
    (put '% 'rep-compile-opcode (bytecode rem))
    (put 'modulo 'rep-compile-fun compile-2-args)
    (put 'modulo 'rep-compile-opcode (bytecode mod))
    (put 'lsh 'rep-compile-fun compile-2-args)
    (put 'lsh 'rep-compile-opcode (bytecode ash))))
