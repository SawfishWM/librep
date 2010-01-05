#| rep.jl -- inliners for many rep language features

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

(define-structure rep.vm.compiler.rep ()

    (open rep
	  rep.lang.doc
	  rep.vm.bytecodes
	  rep.vm.compiler.modules
	  rep.vm.compiler.utils
	  rep.vm.compiler.basic
	  rep.vm.compiler.inline
	  rep.vm.compiler.lap
	  rep.vm.compiler.bindings)

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
    '(defun defmacro defvar defconst defsubst %define require
      declare eval-when-compile define-structure structure))

;;; pass 1 support

  (defun pass-1 (forms) (add-progns (pass-1* forms)))

  (defun pass-1* (forms) (lift-progns (mapcar do-pass-1 forms)))

  ;; flatten progn forms into their container
  (defun lift-progns (forms)
    (let loop ((rest (reverse forms))
	       (out '()))
      (cond ((null rest) out)
	    ((eq (caar rest) 'progn)
	     (loop (cdr rest) (append (cdar rest) out)))
	    (t (loop (cdr rest) (cons (car rest) out))))))

  ;; merge `non-top-level' forms into progn blocks. These will then
  ;; get compiled into single run-byte-code forms
  (defun add-progns (forms)
    (let loop ((rest forms))
      (cond ((null rest) forms)
	    ((memq (caar rest) top-level-unexpanded) (loop (cdr rest)))
	    (t (unless (eq (caar rest) 'progn)
		 (rplaca rest (list 'progn (car rest))))
	       (if (and (cadr rest)
			(not (memq (caadr rest) top-level-unexpanded)))
		   (progn
		     (rplaca rest (nconc (car rest) (list (cadr rest))))
		     (rplacd rest (cddr rest))
		     (loop rest))
		 (loop (cdr rest)))))))

  (defun do-pass-1 (form)
    (let-fluids ((current-form form))
      (unless (or (memq (car form) top-level-unexpanded)
		  (memq (car form) top-level-compiled))
	(setq form (compiler-macroexpand
		    form (lambda (in out)
			   (or (eq in out)
			       (memq (car out) top-level-unexpanded)
			       (memq (car out) top-level-compiled))))))
      (case (car form)
	((defun)
	 (remember-function (nth 1 form) (nth 2 form) (nthcdr 3 form)))

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

	((%define) (remember-lexical-variable (nth 1 form)))

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
	 (setq form (cons 'progn (pass-1* (cdr form)))))

	;; put bare forms into progns so they can be merged in pass-1
	(t (unless (memq (car form) top-level-unexpanded)
	     (setq form (list 'progn form)))))

      form))

;;; pass 2 support

  (defun pass-2 (forms)
    (let loop ((rest forms)
	       (out '()))
      (if (null rest)
	  (nreverse out)
	(loop (cdr rest) (cons (do-pass-2 (car rest)) out)))))

  (defun do-pass-2 (form)
    (let-fluids ((current-form form))
      (case (car form)
	((defun defsubst)
	 (let ((tmp (assq (nth 1 form) (fluid macro-env))))
	   (let-fluids ((current-fun (nth 1 form)))
	     ;;(format standard-error "[%s]\n" (fluid current-fun))
	     (when tmp
	       (rplaca tmp nil)
	       (rplacd tmp nil))
	     (list 'defun (nth 1 form)
		   (compile-lambda (cons 'lambda (nthcdr 2 form))
				   (nth 1 form))))))

	((defmacro)
	 (let ((code (compile-lambda (cons 'lambda (nthcdr 2 form))
				     (nth 1 form)))
	       (tmp (assq (nth 1 form) (fluid macro-env))))
	   (let-fluids ((current-fun (nth 1 form)))
	     (if tmp
		 (rplacd tmp (make-closure code))
	       (compiler-error
		"compiled macro `%s' wasn't in environment" (nth 1 form)))
	     (list 'defmacro (nth 1 form) code))))

	((defconst)
	 (let ((doc (nth 3 form)))
	   (when (and *compiler-write-docs* (stringp doc))
	     (add-documentation (nth 1 form) (fluid current-module) doc)
	     (setq form (delq doc form)))
	   (unless (memq (nth 1 form) (fluid defvars))
	     (remember-variable (nth 1 form)))
	   (unless (assq (nth 1 form) (fluid const-env))
	     (compiler-warning
	      'bindings "unknown constant `%s'" (nth 1 form))))
	 form)

	((defvar)
	 (let ((value (nth 2 form))
	       (doc (nth 3 form)))
	   (when (and (listp value)
		      (not (compiler-constant-p value)))
	     ;; Compile the definition. A good idea?
	     (rplaca (nthcdr 2 form) (compile-form (nth 2 form))))
	   (when (and *compiler-write-docs* (stringp doc))
	     (add-documentation (nth 1 form) nil doc)
	     (setq form (delq (nth 3 form) form)))
	   (unless (memq (nth 1 form) (fluid defvars))
	     (remember-variable (nth 1 form))))
	 form)

	((%define)
	 (let ((sym (nth 1 form))
	       (value (nth 2 form))
	       (doc (nth 3 form)))
	   (unless (memq sym (fluid defines))
	     (remember-lexical-variable (compiler-constant-value sym)))
	   (when (and *compiler-write-docs* (stringp doc))
	     (add-documentation sym (fluid current-module) doc)
	     (setq form (delq doc form)))
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
	     form)))))

;;; Source code transformations. These are basically macros that are only
;;; used at compile-time.

  ;; tells the constant-folder which functions can be removed
  (defun foldablep (name)
    (memq name constant-functions))

  (defun trans-setq (form)
    (let
	(lst)
      (setq form (cdr form))
      (while form
	(unless (consp (cdr form))
	  (compiler-error "odd number of args to setq"))
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
      (when (and (compiler-constant-p doc)
		 (stringp (compiler-constant-value doc))
		 *compiler-write-docs*)
	(add-documentation name nil (compiler-constant-value doc))
	(setq doc nil))
      `(progn
	 ,@(and doc (list `(put ',name 'documentation ,doc)))
	 (make-variable-special ',name)
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
    (note-declaration (cdr form))
    (compile-constant nil))
  (put 'declare 'rep-compile-fun compile-declare)

  (defun compile-quote (form)
    (compile-constant (car (cdr form))))
  (put 'quote 'rep-compile-fun compile-quote)

  (defun compile-function (form)
    (compile-form-1 (cadr form)))
  (put 'function 'rep-compile-fun compile-function)

  (defun compile-lambda-form (form)
    (compile-lambda-constant form))
  (put 'lambda 'rep-compile-fun compile-lambda-form)

  (defun compile-while (form)
    (let
	((top-label (make-label))
	 (test-label (make-label)))
      (emit-insn `(jmp ,test-label))
      (fix-label top-label)
      (compile-body (nthcdr 2 form))
      (emit-insn '(pop))
      (decrement-stack)
      (fix-label test-label)
      (compile-form-1 (nth 1 form))
      (emit-insn `(jpt ,top-label))))
  (put 'while 'rep-compile-fun compile-while)

  (defun compile-%define (form)
    (compile-constant (nth 1 form))
    (compile-form-1 (nth 2 form))
    (emit-insn '(%define))
    (decrement-stack))
  (put '%define 'rep-compile-fun compile-%define)

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
	    (emit-insn `(jmp ,test-label))
	    (fix-label top-label)
	    (emit-insn '(dup))
	    (increment-stack)
	    (emit-insn '(car))
	    (compile-lambda-inline fun nil 1)
	    (emit-insn '(pop))
	    (decrement-stack)
	    (emit-insn '(cdr))
	    (fix-label test-label)
	    ;; I don't have a jump-if-t-but-never-pop instruction, so
	    ;; make one out of "jpt TOP; nil". If I ever get a peep hole
	    ;; optimiser working, the nil should be fodder for it..
	    (emit-insn `(jtp ,top-label))
	    (emit-insn '(push ())))
	;; The function must be called, so just use the mapc opcode
	(compile-form-1 fun)
	(compile-form-1 lst)
	(emit-insn '(mapc))
	(decrement-stack))))
  (put 'mapc 'rep-compile-fun compile-mapc)

  (defun compile-progn (form #!optional return-follows)
    (compile-body (cdr form) return-follows))
  (put 'progn 'rep-compile-fun compile-progn)

  (defun compile-prog1 (form)
    (compile-form-1 (nth 1 form))
    (compile-body (nthcdr 2 form))
    (emit-insn '(pop))
    (decrement-stack))
  (put 'prog1 'rep-compile-fun compile-prog1)

  (defun compile-set (form)
    (let ((sym (nth 1 form))
	  (val (nth 2 form)))
      (if (compiler-constant-p sym)
	  ;; use setq
	  (progn
	    (setq sym (compiler-constant-value sym))
	    (unless (symbolp sym)
	      (compiler-error "trying to set value of a non-symbol: %s" sym))
	    (compile-form-1 val)
	    (emit-insn '(dup))
	    (increment-stack)
	    (emit-varset sym)
	    (note-binding-modified sym)
	    (decrement-stack))
	;; need to preserve left-right evaluation order
	(compile-form-1 sym)
	(compile-form-1 val)
	(emit-insn '(set))
	(decrement-stack))))
  (put 'set 'rep-compile-fun compile-set)

  ;; compile let* specially to coalesce all bindings into a single frame
  (defun compile-let* (form #!optional return-follows)
    (let
	((lst (car (cdr form))))
      (call-with-frame
       (lambda ()
	 (emit-insn '(init-bind))
	 (increment-b-stack)
	 (while (consp lst)
	   (cond ((consp (car lst))
		  (let ((tmp (car lst)))
		    (compile-body (cdr tmp))
		    (test-variable-bind (car tmp))
		    (note-binding (car tmp))
		    (emit-binding (car tmp))))
		 (t (emit-insn '(push ()))
		    (increment-stack)
		    (test-variable-bind (car lst))
		    (note-binding (car lst))
		    (emit-binding (car lst))))
	   (decrement-stack)
	   (setq lst (cdr lst)))
	 (compile-body (nthcdr 2 form) return-follows)
	 (emit-insn '(unbind))
	 (decrement-b-stack)))))
  (put 'let* 'rep-compile-fun compile-let*)

  ;; let can be compiled straight from its macro definition

  ;; compile letrec specially to handle tail recursion elimination
  (defun compile-letrec (form #!optional return-follows)
    (let ((bindings (car (cdr form))))
      (call-with-frame
       (lambda ()
	 (push-state)
	 (emit-insn '(init-bind))
	 (increment-b-stack)

	 ;; create the bindings, should really be to void values, but use nil..
	 (mapc (lambda (cell)
		 (let ((var (or (car cell) cell)))
		   (test-variable-bind var)
		   (compile-constant nil)
		   (note-binding var)
		   (emit-binding var)
		   (decrement-stack))) bindings)
	 ;; then set them to their values
	 (mapc (lambda (cell)
		 (let ((var (or (car cell) cell)))
		   (compile-body (cdr cell) nil var)
		   (emit-varset var)
		   (decrement-stack))) bindings)

	 ;; Test if we can inline it away.
	 ;; Look for forms like (letrec ((foo (lambda (..) body..))) (foo ..))
	 ;; where `foo' only appears in inlinable tail calls in body
	 (when (catch 'no
		 (unless (= (length bindings) 1)
		   (throw 'no t))
		 (let ((var (or (caar bindings) (car bindings)))
		       (value (cdar bindings)))
		   (unless (and (binding-tail-call-only-p var)
				value (not (cdr value))
				(eq (caar value) 'lambda))
		     (throw 'no t))
		   (setq value (car value))
		   (let ((body (nthcdr 2 form)))
		     (unless (= (length body) 1)
		       (throw 'no t))
		     (setq body (car body))
		     (when (and (eq (car body) (get-language-property
						'compiler-sequencer))
				(= (length body) 2))
		       (setq body (cadr body)))
		     (unless (eq (car body) var)
		       (throw 'no t))

		     ;; okay, let's go
		     (let-fluids ((silence-compiler t))
		       (reload-state)
		       ;; XXX what if this clashes?
		       (remember-function var (cadr value))
		       (compile-lambda-inline value (cdr body)
					      nil return-follows var)
		       (forget-function var)
		       nil))))

	   ;; no, keep on the usual track
	   (compile-body (nthcdr 2 form) return-follows)
	   (emit-insn '(unbind))
	   (decrement-b-stack))
	 (pop-state)))))
  (put 'letrec 'rep-compile-fun compile-letrec)

  (defun compile-let-fluids (form)
    (let ((bindings (cadr form))
	  (body (cddr form)))
      (call-with-frame
       (lambda ()
	 (fluid-set lexically-pure nil)
	 ;; compile each fluid, value pair onto the stack
	 (mapc (lambda (cell)
		 (compile-form-1 (car cell))
		 (compile-body (cdr cell))) bindings)
	 (emit-insn '(init-bind))
	 (increment-b-stack)
	 (mapc (lambda (unused)
		 (declare (unused unused))
		 (emit-insn '(fluid-bind))
		 (decrement-stack 2)) bindings)
	 (compile-body body)
	 (emit-insn '(unbind))
	 (decrement-b-stack)))))
  (put 'let-fluids 'rep-compile-fun compile-let-fluids)

  (defun compile-defun (form)
    (remember-function (nth 1 form) (nth 2 form))
    (compile-constant (nth 1 form))
    (compile-lambda-constant (cons 'lambda (nthcdr 2 form)) (nth 1 form))
    (emit-insn '(%define))
    (decrement-stack))
  (put 'defun 'rep-compile-fun compile-defun)

  (defun compile-defmacro (form)
    (remember-function (nth 1 form) (nth 2 form))
    (compile-constant (nth 1 form))
    (compile-constant 'macro)
    (compile-lambda-constant (cons 'lambda (nthcdr 2 form)) (nth 1 form))
    (emit-insn '(cons))
    (emit-insn '(%define))
    (decrement-stack))
  (put 'defmacro 'rep-compile-fun compile-defmacro)

  (defun compile-cond (form #!optional return-follows)
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
		  (emit-insn '(push t))
		(compile-form-1 (car subl) #:return-follows return-follows)
		(decrement-stack)))
	    (when (consp (cdr form))
	      ;;(compiler-warning
	      ;; 'misc "unreachable conditions after t in cond statement")
	      ;; Ignore the rest of the statement
	      (setq form nil))
	    (setq need-trailing-nil nil))
	   ((eq condition nil)
	    ;; condition nil -- never taken
	    (when (cdr subl)
	      ;;(compiler-warning
	      ;; 'misc "unreachable forms after nil in cond statement")
	      ))
	   (t
	    ;; non t-or-nil condition
	    (compile-form-1 (car subl)
			    #:return-follows (and return-follows
						  (null (cdr subl))
						  (null (cdr form))))
	    (decrement-stack)
	    (if (consp (cdr subl))
		;; Something besides the condition
		(if (cdr form)
		    ;; This isn't the last condition list
		    (progn
		      (emit-insn `(jn ,next-label))
		      (compile-body (cdr subl) return-follows)
		      (decrement-stack)
		      (emit-insn `(jmp ,end-label))
		      (fix-label next-label))
		  ;; It is the last condition list, use the result
		  ;; of this condition for the return value when it's
		  ;; nil
		  (emit-insn `(jnp ,end-label))
		  (compile-body (cdr subl) return-follows)
		  (decrement-stack)
		  (setq need-trailing-nil nil))
	      ;; No action to take
	      (if (cdr form)
		  ;; This isn't the last condition list
		  (emit-insn `(jtp ,end-label))
		;; This is the last condition list, since there's no
		;; action to take, just fall out the bottom, with the
		;; condition as value.
		(setq need-trailing-nil nil))))))
	(setq form (cdr form)))
      (when need-trailing-nil
	(emit-insn '(push ())))
      (increment-stack)
      (fix-label end-label)))
  (put 'cond 'rep-compile-fun compile-cond)

  (defun compile-case (form #!optional return-follows)
    (let
	((end-label (make-label))
	 (had-default nil))
      (setq form (cdr form))
      (unless form
	(compiler-error "no key value in case statement"))
      ;; XXX if key is constant optimise case away..
      (compile-form-1 (car form))
      (setq form (cdr form))
      (while (consp form)
	(unless (consp form)
	  (compiler-error "badly formed clause in case statement"))
	(let
	    ((cases (caar form))
	     (forms (cdar form))
	     (next-label (make-label)))
	  (cond ((consp cases)
		 (emit-insn '(dup))
		 (increment-stack)
		 (if (consp (cdr cases))
		     ;; >1 possible case
		     (progn
		       (compile-constant cases)
		       (emit-insn '(memql)))
		   ;; only one case, use eql
		   (compile-constant (car cases))
		   (emit-insn '(eql)))
		 (decrement-stack)
		 (emit-insn `(jn ,next-label))
		 (decrement-stack))
		((eq cases t) (setq had-default t))
		(t (compiler-error
		    "badly formed clause in case statement" #:form cases)))
	  (compile-body forms return-follows)
	  (decrement-stack)
	  (emit-insn `(jmp ,end-label))
	  (fix-label next-label)
	  (setq form (cdr form))))
      (unless had-default
	(emit-insn '(push ())))
      (increment-stack)
      (fix-label end-label)
      (emit-insn '(swap))
      (emit-insn '(pop))))
  (put 'case 'rep-compile-fun compile-case)

  (defun compile-catch (form)
    (let
	((catch-label (make-label))
	 (start-label (make-label))
	 (end-label (make-label)))
    (let-fluids ((lexically-pure nil))

      ;;		jmp start
      (emit-insn `(jmp ,start-label))

      ;; catch:
      ;;		catch TAG
      ;;		ejmp end
      (increment-stack)			;enter with one arg on stack
      (fix-label catch-label)
      (compile-form-1 (nth 1 form))
      (emit-insn '(catch))
      (decrement-stack)
      (emit-insn `(ejmp ,end-label))
      (decrement-stack)

      ;; start:
      ;;		push #catch
      ;;		binderr
      ;;		FORMS...
      ;;		unbind
      ;; end:
      (fix-label start-label)
      (push-label-addr catch-label)
      (emit-insn '(binderr))
      (increment-b-stack)
      (decrement-stack)
      (compile-body (nthcdr 2 form))
      (emit-insn '(unbind))
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
      (emit-insn `(jmp ,start-label))

      ;; cleanup:
      ;;		CLEANUP-FORMS
      ;;		pop
      ;;		ejmp end
      ;; [overall, stack +1]
      (increment-stack 2)
      (fix-label cleanup-label)
      (compile-body (nthcdr 2 form))
      (emit-insn '(pop))
      (emit-insn `(ejmp ,end-label))
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
      (emit-insn '(binderr))
      (increment-b-stack)
      (decrement-stack)
      (compile-form-1 (nth 1 form))
      (emit-insn '(unbind))
      (decrement-b-stack)
      (emit-insn '(push ()))
      (decrement-stack)
      (emit-insn `(jmp ,cleanup-label))

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
      (emit-insn `(jmp ,start-label))
      (fix-label cleanup-label)

      (increment-stack)		;reach here with one item on stack
      (if (consp handlers)
	  (call-with-frame
	   (lambda ()
	     (if (and (nth 1 form) (not (eq (nth 1 form) 'nil)))
		 (let ((var (nth 1 form)))
		   (when (spec-bound-p var)
		     (compiler-error
		      "condition-case can't bind to special variable `%s'" var))
		   (test-variable-bind var)
		   (note-binding var)
		   ;; XXX errorpro instruction always heap binds..
		   (tag-binding var 'heap-allocated))
	       ;; something always gets bound
	       (let ((tem (gensym)))
		 (note-binding tem)
		 (tag-binding tem 'heap-allocated)
		 ;; avoid `unused variable' warnings
		 (note-binding-referenced tem)))
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
		     (emit-insn '(errorpro))
		     (decrement-stack)
		     (emit-insn `(jtp ,next-label))
		     (decrement-stack)
		     (compile-body (cdr (car handlers)))
		     (emit-insn `(jmp ,end-label))
		     (fix-label next-label))
		 (compiler-error
		  "badly formed condition-case handler: `%s'"
		  (car handlers) #:form handlers))
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
		   (emit-insn '(errorpro))
		   (decrement-stack)
		   (emit-insn `(ejmp ,pc-label))
		   (fix-label pc-label)
		   (decrement-stack)
		   (compile-body (cdr (car handlers)))
		   (emit-insn `(jmp ,end-label)))
	       (compiler-error
		"badly formed condition-case handler: `%s'"
		(car handlers) #:form (car handlers)))))
	(compiler-error "no handlers in condition-case"))
      (decrement-stack)

      ;; start:
      ;;		push cleanup
      ;;		binderr
      ;;		FORM
      (fix-label start-label)
      (push-label-addr cleanup-label)
      (emit-insn '(binderr))
      (increment-b-stack)
      (decrement-stack)
      (compile-form-1 (nth 2 form))

      ;; end:
      ;;		unbind			;unbind error handler or VAR
      (fix-label end-label)
      (emit-insn '(unbind))
      (decrement-b-stack))))
  (put 'condition-case 'rep-compile-fun compile-condition-case)

  (defun compile-list (form)
    (do ((args (cdr form) (cdr args))
	 (count 0 (1+ count)))
	((null args)
	 ;; merge the arguments into a single list
	 (compile-constant '())
	 (do ((i 0 (1+ i)))
	     ((= i count))
	   (emit-insn '(cons))
	   (decrement-stack)))
      (compile-form-1 (car args))))
  (put 'list 'rep-compile-fun compile-list)

  (defun compile-list* (form)
    (do ((args (cdr form) (cdr args))
	 (count 0 (1+ count)))
	((null args)
	 ;; merge the arguments into a single list
	 (do ((i 0 (1+ i)))
	     ((>= i (1- count)))
	   (emit-insn '(cons))
	   (decrement-stack)))
      (compile-form-1 (car args))))
  (put 'list* 'rep-compile-fun compile-list*)

  ;; Funcall normally translates to a single call instruction. However,
  ;; if the function being called is a constant lambda expression, open
  ;; code it.
  (defun compile-funcall (form #!optional return-follows)
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
	    ;; We push one less value than when using 'call
	    (if (zerop arg-count)
		(increment-stack)
	      (decrement-stack (1- arg-count))))
	(emit-insn `(call ,arg-count))
	(note-function-call-made)
	(decrement-stack arg-count))))
  (put 'funcall 'rep-compile-fun compile-funcall)

  (defun compile-apply (form)
    (compile-form-1 (nth 1 form))
    (do ((args (nthcdr 2 form) (cdr args))
	 (count 0 (1+ count)))
	((null args)
	 ;; merge the arguments into a single list
	 (do ((i 0 (1+ i)))
	     ((>= i (1- count)))
	   (emit-insn '(cons))
	   (decrement-stack)))
      (compile-form-1 (car args)))
    (emit-insn '(apply))
    (decrement-stack))
  (put 'apply 'rep-compile-fun compile-apply)

  (defun compile-nth (form)
    (let
	((insn (cdr (assq (nth 1 form) byte-nth-insns))))
      (if insn
	  (progn
	    (compile-form-1 (nth 2 form))
	    (emit-insn (list insn)))
	(compile-2-args form))))
  (put 'nth 'rep-compile-fun compile-nth)
  (put 'nth 'rep-compile-opcode 'nth)

  (defun compile-nthcdr (form)
    (let
	((insn (assq (nth 1 form) byte-nthcdr-insns)))
      (if insn
	  (progn
	    (compile-form-1 (nth 2 form))
	    (when (cdr insn)
	      (emit-insn (list (cdr insn)))))
	(compile-2-args form))))
  (put 'nthcdr 'rep-compile-fun compile-nthcdr)
  (put 'nthcdr 'rep-compile-opcode 'nthcdr)

  (defun compile-minus (form)
    (if (/= (length form) 2)
	(compile-binary-op form)
      (compile-form-1 (car (cdr form)))
      (emit-insn '(neg))))
  (put '- 'rep-compile-fun compile-minus)
  (put '- 'rep-compile-opcode 'sub)

  (defun compile-make-closure (form)
    (when (nthcdr 3 form)
      (compiler-warning
       'parameters "more than two parameters to `%s'; rest ignored"
       (car form)))
    (compile-form-1 (nth 1 form))
    (compile-form-1 (nth 2 form))
    (emit-insn '(make-closure))
    (note-closure-made)
    (decrement-stack))
  (put 'make-closure 'rep-compile-fun compile-make-closure)

  (defun compile-log (form)
    (cond ((nthcdr 3 form)
	   (compiler-warning
	    'parameters "more than two parameters to `log'; rest ignored"))
	  ((nthcdr 2 form)
	   ;; dual argument form of log. compiles to
	   (compile-form-1 (nth 1 form))
	   (emit-insn '(log))
	   (compile-form-1 (nth 2 form))
	   (emit-insn '(log))
	   (emit-insn '(div))
	   (decrement-stack))
	  ((nthcdr 1 form)
	   ;; single argument form
	   (compile-form-1 (nth 1 form))
	   (emit-insn '(log)))
	  (t (compiler-warning 'parameters "too few parameters to `log'"))))
  (put 'log 'rep-compile-fun compile-log)

  (defun get-form-opcode (form)
    (cond ((symbolp form) (get form 'rep-compile-opcode))
	  ;; must be a structure-ref
	  ((eq (car form) 'structure-ref)
	   (get (caddr form) 'rep-compile-opcode))
	  (t (compiler-error "don't know opcode for `%s'" form))))

  ;; Instruction with no arguments
  (defun compile-0-args (form)
    (when (cdr form)
      (compiler-warning
       'parameters "all parameters to `%s' ignored" (car form)))
    (emit-insn (list (get-form-opcode (car form))))
    (increment-stack))

  ;; Instruction taking 1 arg on the stack
  (defun compile-1-args (form)
    (when (nthcdr 2 form)
      (compiler-warning
       'parameters "more than one parameter to `%s'; rest ignored" (car form)))
    (compile-form-1 (nth 1 form))
    (emit-insn (list (get-form-opcode (car form)))))

  ;; Instruction taking 2 args on the stack
  (defun compile-2-args (form)
    (when (nthcdr 3 form)
      (compiler-warning
       'parameters "more than two parameters to `%s'; rest ignored"
       (car form)))
    (compile-form-1 (nth 1 form))
    (compile-form-1 (nth 2 form))
    (emit-insn (list (get-form-opcode (car form))))
    (decrement-stack))

  ;; Instruction taking 3 args on the stack
  (defun compile-3-args (form)
    (when (nthcdr 4 form)
      (compiler-warning
       'parameters "More than three parameters to `%s'; rest ignored"
       (car form)))
    (compile-form-1 (nth 1 form))
    (compile-form-1 (nth 2 form))
    (compile-form-1 (nth 3 form))
    (emit-insn (list (get-form-opcode (car form))))
    (decrement-stack 2))

  ;; Compile a form `(OP ARG1 ARG2 ARG3 ...)' into as many two argument
  ;; instructions as needed (PUSH ARG1; PUSH ARG2; OP; PUSH ARG3; OP; ...)
  (defun compile-binary-op (form)
    (let
	((opcode (get-form-opcode (car form))))
      (setq form (cdr form))
      (unless (>= (length form) 2)
	(compiler-error
	 "too few arguments to binary operator `%s'" (car form)))
      (compile-form-1 (car form))
      (setq form (cdr form))
      (while (consp form)
	(compile-form-1 (car form))
	(emit-insn (list opcode))
	(decrement-stack)
	(setq form (cdr form)))))

  ;; Used for >, >=, < and <=
  (defun compile-transitive-relation (form)
    (cond
     ((<= (length form) 2)
      (compiler-error "too few args to relation `%s'" (car form)))
     ((= (length form) 3)
      (let
	  ((opcode (get-form-opcode (car form))))
	;; Simple case, only two arguments, i.e. `(OP ARG1 ARG2)' into:
	;;  PUSH ARG1; PUSH ARG2; OP;
	(compile-form-1 (nth 1 form))
	(compile-form-1 (nth 2 form))
	(emit-insn (list opcode))
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
    (put 'cons 'rep-compile-opcode 'cons)
    (put 'car 'rep-compile-fun compile-1-args)
    (put 'car 'rep-compile-opcode 'car)
    (put 'cdr 'rep-compile-fun compile-1-args)
    (put 'cdr 'rep-compile-opcode 'cdr)
    (put 'rplaca 'rep-compile-fun compile-2-args)
    (put 'rplaca 'rep-compile-opcode 'rplaca)
    (put 'rplacd 'rep-compile-fun compile-2-args)
    (put 'rplacd 'rep-compile-opcode 'rplacd)
    (put 'aset 'rep-compile-fun compile-3-args)
    (put 'aset 'rep-compile-opcode 'aset)
    (put 'aref 'rep-compile-fun compile-2-args)
    (put 'aref 'rep-compile-opcode 'aref)
    (put 'length 'rep-compile-fun compile-1-args)
    (put 'length 'rep-compile-opcode 'length)
    (put '+ 'rep-compile-fun compile-binary-op)
    (put '+ 'rep-compile-opcode 'add)
    (put '* 'rep-compile-fun compile-binary-op)
    (put '* 'rep-compile-opcode 'mul)
    (put '/ 'rep-compile-fun compile-binary-op)
    (put '/ 'rep-compile-opcode 'div)
    (put 'remainder 'rep-compile-fun compile-2-args)
    (put 'remainder 'rep-compile-opcode 'rem)
    (put 'mod 'rep-compile-fun compile-2-args)
    (put 'mod 'rep-compile-opcode 'mod)
    (put 'lognot 'rep-compile-fun compile-1-args)
    (put 'lognot 'rep-compile-opcode 'lnot)
    (put 'not 'rep-compile-fun compile-1-args)
    (put 'not 'rep-compile-opcode 'not)
    (put 'logior 'rep-compile-fun compile-binary-op)
    (put 'logior 'rep-compile-opcode 'lor)
    (put 'logxor 'rep-compile-fun compile-binary-op)
    (put 'logxor 'rep-compile-opcode 'lxor)
    (put 'logand 'rep-compile-fun compile-binary-op)
    (put 'logand 'rep-compile-opcode 'land)
    (put 'ash 'rep-compile-fun compile-2-args)
    (put 'ash 'rep-compile-opcode 'ash)
    (put 'equal 'rep-compile-fun compile-2-args)
    (put 'equal 'rep-compile-opcode 'equal)
    (put 'eq 'rep-compile-fun compile-2-args)
    (put 'eq 'rep-compile-opcode 'eq)
    (put '= 'rep-compile-fun compile-transitive-relation)
    (put '= 'rep-compile-opcode 'num-eq)
    (put '> 'rep-compile-fun compile-transitive-relation)
    (put '> 'rep-compile-opcode 'gt)
    (put '< 'rep-compile-fun compile-transitive-relation)
    (put '< 'rep-compile-opcode 'lt)
    (put '>= 'rep-compile-fun compile-transitive-relation)
    (put '>= 'rep-compile-opcode 'ge)
    (put '<= 'rep-compile-fun compile-transitive-relation)
    (put '<= 'rep-compile-opcode 'le)
    (put '1+ 'rep-compile-fun compile-1-args)
    (put '1+ 'rep-compile-opcode 'inc)
    (put '1- 'rep-compile-fun compile-1-args)
    (put '1- 'rep-compile-opcode 'dec)
    (put 'zerop 'rep-compile-fun compile-1-args)
    (put 'zerop 'rep-compile-opcode 'zerop)
    (put 'null 'rep-compile-fun compile-1-args)
    (put 'null 'rep-compile-opcode 'not)
    (put 'atom 'rep-compile-fun compile-1-args)
    (put 'atom 'rep-compile-opcode 'atom)
    (put 'consp 'rep-compile-fun compile-1-args)
    (put 'consp 'rep-compile-opcode 'consp)
    (put 'listp 'rep-compile-fun compile-1-args)
    (put 'listp 'rep-compile-opcode 'listp)
    (put 'numberp 'rep-compile-fun compile-1-args)
    (put 'numberp 'rep-compile-opcode 'numberp)
    (put 'stringp 'rep-compile-fun compile-1-args)
    (put 'stringp 'rep-compile-opcode 'stringp)
    (put 'vectorp 'rep-compile-fun compile-1-args)
    (put 'vectorp 'rep-compile-opcode 'vectorp)
    (put 'throw 'rep-compile-fun compile-2-args)
    (put 'throw 'rep-compile-opcode 'throw)
    (put 'boundp 'rep-compile-fun compile-1-args)
    (put 'boundp 'rep-compile-opcode 'boundp)
    (put 'symbolp 'rep-compile-fun compile-1-args)
    (put 'symbolp 'rep-compile-opcode 'symbolp)
    (put 'get 'rep-compile-fun compile-2-args)
    (put 'get 'rep-compile-opcode 'get)
    (put 'put 'rep-compile-fun compile-3-args)
    (put 'put 'rep-compile-opcode 'put)
    (put 'signal 'rep-compile-fun compile-2-args)
    (put 'signal 'rep-compile-opcode 'signal)
    (put 'quotient 'rep-compile-fun compile-2-args)
    (put 'quotient 'rep-compile-opcode 'quotient)
    (put 'reverse 'rep-compile-fun compile-1-args) ; new 12/7/94
    (put 'reverse 'rep-compile-opcode 'reverse)
    (put 'nreverse 'rep-compile-fun compile-1-args)
    (put 'nreverse 'rep-compile-opcode 'nreverse)
    (put 'assoc 'rep-compile-fun compile-2-args)
    (put 'assoc 'rep-compile-opcode 'assoc)
    (put 'assq 'rep-compile-fun compile-2-args)
    (put 'assq 'rep-compile-opcode 'assq)
    (put 'rassoc 'rep-compile-fun compile-2-args)
    (put 'rassoc 'rep-compile-opcode 'rassoc)
    (put 'rassq 'rep-compile-fun compile-2-args)
    (put 'rassq 'rep-compile-opcode 'rassq)
    (put 'last 'rep-compile-fun compile-1-args)
    (put 'last 'rep-compile-opcode 'last)
    (put 'mapcar 'rep-compile-fun compile-2-args)
    (put 'mapcar 'rep-compile-opcode 'mapcar)
    (put 'member 'rep-compile-fun compile-2-args)
    (put 'member 'rep-compile-opcode 'member)
    (put 'memq 'rep-compile-fun compile-2-args)
    (put 'memq 'rep-compile-opcode 'memq)
    (put 'delete 'rep-compile-fun compile-2-args)
    (put 'delete 'rep-compile-opcode 'delete)
    (put 'delq 'rep-compile-fun compile-2-args)
    (put 'delq 'rep-compile-opcode 'delq)
    (put 'delete-if 'rep-compile-fun compile-2-args)
    (put 'delete-if 'rep-compile-opcode 'delete-if)
    (put 'delete-if-not 'rep-compile-fun compile-2-args)
    (put 'delete-if-not 'rep-compile-opcode 'delete-if-not)
    (put 'copy-sequence 'rep-compile-fun compile-1-args)
    (put 'copy-sequence 'rep-compile-opcode 'copy-sequence)
    (put 'sequencep 'rep-compile-fun compile-1-args)
    (put 'sequencep 'rep-compile-opcode 'sequencep)
    (put 'functionp 'rep-compile-fun compile-1-args)
    (put 'functionp 'rep-compile-opcode 'functionp)
    (put 'special-form-p 'rep-compile-fun compile-1-args)
    (put 'special-form-p 'rep-compile-opcode 'special-form-p)
    (put 'subrp 'rep-compile-fun compile-1-args)
    (put 'subrp 'rep-compile-opcode 'subrp)
    (put 'eql 'rep-compile-fun compile-2-args)
    (put 'eql 'rep-compile-opcode 'eql)
    (put 'max 'rep-compile-fun compile-binary-op)
    (put 'max 'rep-compile-opcode 'max)
    (put 'min 'rep-compile-fun compile-binary-op)
    (put 'min 'rep-compile-opcode 'min)
    (put 'filter 'rep-compile-fun compile-2-args)
    (put 'filter 'rep-compile-opcode 'filter)
    (put 'macrop 'rep-compile-fun compile-1-args)
    (put 'macrop 'rep-compile-opcode 'macrop)
    (put 'bytecodep 'rep-compile-fun compile-1-args)
    (put 'bytecodep 'rep-compile-opcode 'bytecodep)
    (put 'closurep 'rep-compile-fun compile-1-args)
    (put 'closurep 'rep-compile-opcode 'closurep)
    (put 'thread-forbid 'rep-compile-fun compile-0-args)
    (put 'thread-forbid 'rep-compile-opcode 'forbid)
    (put 'thread-permit 'rep-compile-fun compile-0-args)
    (put 'thread-permit 'rep-compile-opcode 'permit)
    (put 'fluid 'rep-compile-fun compile-1-args)
    (put 'fluid 'rep-compile-opcode 'fluid-ref)
    (put 'fluid-set 'rep-compile-fun compile-2-args)
    (put 'fluid-set 'rep-compile-opcode 'fluid-set)

    (put 'caar 'rep-compile-fun compile-1-args)
    (put 'caar 'rep-compile-opcode 'caar)
    (put 'cadr 'rep-compile-fun compile-1-args)
    (put 'cadr 'rep-compile-opcode 'cadr)
    (put 'cdar 'rep-compile-fun compile-1-args)
    (put 'cdar 'rep-compile-opcode 'cdar)
    (put 'cddr 'rep-compile-fun compile-1-args)
    (put 'cddr 'rep-compile-opcode 'cddr)
    (put 'caddr 'rep-compile-fun compile-1-args)
    (put 'caddr 'rep-compile-opcode 'caddr)
    (put 'cadddr 'rep-compile-fun compile-1-args)
    (put 'cadddr 'rep-compile-opcode 'cadddr)

    (put 'floor 'rep-compile-fun compile-1-args)
    (put 'floor 'rep-compile-opcode 'floor)
    (put 'ceiling 'rep-compile-fun compile-1-args)
    (put 'ceiling 'rep-compile-opcode 'ceiling)
    (put 'truncate 'rep-compile-fun compile-1-args)
    (put 'truncate 'rep-compile-opcode 'truncate)
    (put 'round 'rep-compile-fun compile-1-args)
    (put 'round 'rep-compile-opcode 'round)
    (put 'exp 'rep-compile-fun compile-1-args)
    (put 'exp 'rep-compile-opcode 'exp)
    (put 'sin 'rep-compile-fun compile-1-args)
    (put 'sin 'rep-compile-opcode 'sin)
    (put 'cos 'rep-compile-fun compile-1-args)
    (put 'cos 'rep-compile-opcode 'cos)
    (put 'tan 'rep-compile-fun compile-1-args)
    (put 'tan 'rep-compile-opcode 'tan)
    (put 'sqrt 'rep-compile-fun compile-1-args)
    (put 'sqrt 'rep-compile-opcode 'sqrt)
    (put 'expt 'rep-compile-fun compile-2-args)
    (put 'expt 'rep-compile-opcode 'expt)

    ;; some pseudonyms
    (put 'string= 'rep-compile-fun compile-2-args)
    (put 'string= 'rep-compile-opcode 'equal)
    (put 'string< 'rep-compile-fun compile-transitive-relation)
    (put 'string< 'rep-compile-opcode 'lt)
    (put '% 'rep-compile-fun compile-2-args)
    (put '% 'rep-compile-opcode 'rem)
    (put 'modulo 'rep-compile-fun compile-2-args)
    (put 'modulo 'rep-compile-opcode 'mod)
    (put 'lsh 'rep-compile-fun compile-2-args)
    (put 'lsh 'rep-compile-opcode 'ash))

  ;; setup properties to tell the compiler where to look for symbols
  ;; in the `rep'  package
  (unless (get 'rep 'compiler-handler-property)
    (put 'rep 'compiler-handler-property 'rep-compile-fun)
    (put 'rep 'compiler-transform-property 'rep-compile-transform)
    (put 'rep 'compiler-sequencer 'progn)
    (put 'rep 'compiler-pass-1 pass-1)
    (put 'rep 'compiler-pass-2 pass-2)
    (put 'rep 'compiler-foldablep foldablep)))
