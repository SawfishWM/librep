#| scheme.jl -- inliners for compiling Scheme code

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

;; XXX this is pretty much untested..

(declare (unsafe-for-call/cc))

(define-structure rep.vm.compiler.scheme ()

    (open rep
	  rep.lang.doc
	  rep.vm.compiler.modules
	  rep.vm.compiler.utils
	  rep.vm.compiler.basic
	  rep.vm.compiler.inline
	  rep.vm.compiler.lap
	  rep.vm.compiler.bindings
	  rep.vm.compiler.rep
	  rep.vm.bytecodes)

  ;; List of side-effect-free functions. They should always return the
  ;; same value when given the same inputs. Used when constant folding.
  (define constant-functions
    '(not eqv? eq? equal? boolean? pair? car cdr caar cadr cdar cddr caaar
      caadr cadar caddr cdaar cdadr cddar cdddr null? list? length?
      list-tail list-ref memq memv member assq assv assoc symbol?
      symbol->string string->symbol number? complex? real? rational?
      integer? exact? inexact? = < > <= >= zero? positive? negative?
      odd? even? max min + * - / abs quotient remainder modulo gcd
      lcm numerator denominator floor ceiling truncate round rationalize
      exp log sin cos tan asin acos atan sqrt expt exact->inexact
      inexact->exact string->number number->string char=? char<? char>?
      char<=? char>=? char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
      char-alphabetic? char-numeric? char-whitespace? char-upper-case?
      char-lower-case? char->integer integer->char char-upcase char-downcase
      string? string-length string-ref string=? string-ci=? string<?
      string>? string<=? string>=? string-ci<? string-ci>? string-ci<=?
      string-ci>=? vector? vector-length vector-ref procedure?))

  ;; List of symbols, when the name of the function called by a top-level
  ;; form is one of these that form is compiled.
  (define top-level-compiled
    '(if cond when unless let let* letrec begin and or case))


;;; pass 1 support

  (defun pass-1 (forms)
    (let loop ((rest forms)
	       (out '()))
      (if (null rest)
	  (nreverse out)
	(loop (cdr rest) (cons (do-pass-1 (car rest)) out)))))

  (defun do-pass-1 (form)
    (unless (or (eq (car form) 'define) (memq (car form) top-level-compiled))
      (setq form (compiler-macroexpand
		  form (lambda (in out)
			 (or (eq in out) (memq (car out) '(define begin))
			     (memq (car out) top-level-compiled))))))
    (case (car form)
      ((define)
       (let ((name (cadr form)))
	 (cond ((symbolp name)
		(remember-lexical-variable (compiler-constant-value name)))
	       ((and (consp name) (symbolp (car name)))
		(remember-function (car name) (cdr name)))
	       (t (compiler-error "Invalid define statement" form)))))

      ((begin)
       (setq form (cons 'begin (mapcar do-pass-1 (cdr form))))))

    form)


;;; pass 2 support

  (defun pass-2 (forms)
    (let loop ((rest forms)
	       (out '()))
      (if (null rest)
	  (nreverse out)
	(loop (cdr rest) (cons (do-pass-2 (car rest)) out)))))

  (defun do-pass-2 (form)
    (cond ((eq (car form) 'define)
	   (setq form (compile-define form)))
	  ((eq (car form) 'begin)
	   (cons 'begin (mapcar do-pass-2 (cdr form))))
	  ((memq (car form) top-level-compiled)
	   (setq form (compile-form form))))
    form)

  ;; XXX this is broken, e.g.: (define ((foo a) b) (+ a b))
  (defun compile-define (form)
    (let ((name (cadr form)))
      (if (symbolp name)
	  `(define ,name
	     ,(if (memq (car (nth 2 form)) top-level-compiled)
		  (compile-form (nth 2 form))
		(nth 2 form)))
	`(define ,(car name)
	   (make-closure
	    ,(compile-lambda `(lambda ,(cdr name) ,@(cddr form)) (car name))
	    ',(car name))))))


;;; source code transformations

  ;; tells the constant-folder which functions can be removed
  (defun foldablep (name)
    (memq name constant-functions))


;;; special compilers

  ;; module compilers from compiler-modules
  (put 'structure 'scheme-compile-fun compile-structure)
  (put 'define-structure 'scheme-compile-fun compile-define-structure)
  (put 'structure-ref 'scheme-compile-fun compile-structure-ref)

  (put 'quote 'scheme-compile-fun (get 'quote 'rep-compile-fun))
  (put '\#lambda 'scheme-compile-fun (get 'lambda 'rep-compile-fun))
  (put '\#progn 'scheme-compile-fun (get 'progn 'rep-compile-fun))

  (defun compile-set! (form)
    (let ((sym (nth 1 form))
	  (val (nth 2 form)))
      (compile-form-1 val)
      (emit-insn '(dup))
      (increment-stack)
      (emit-varset sym)
      (note-binding-modified sym)
      (decrement-stack)))
  (put 'set! 'scheme-compile-fun compile-set!)

  (put '\#define 'unscheme-compile-fun (get '%define 'rep-compile-fun))

  (defun compile-\#test (form)
    (compile-form-1 (cadr form))
    (emit-insn '(scm-test)))
  (put '\#test 'scheme-compile-fun compile-\#test)

  ;; compile let* specially to coalesce all bindings into a single frame
  (put 'let* 'scheme-compile-fun (get 'let* 'rep-compile-fun))

  ;; let can be compiled straight from its macro definition

  ;; compile letrec specially to handle tail recursion elimination
  (put 'letrec 'scheme-compile-fun (get 'letrec 'rep-compile-fun))

  (put '\#cond 'scheme-compile-fun (get 'cond 'rep-compile-fun))

  ;; adapted from rep.vm.compiler.rep
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
		((not (eq cases 'else))
		 (compiler-error "Badly formed clause in case statement")))
	  (compile-body forms return-follows)
	  (decrement-stack)
	  (emit-insn `(jmp ,end-label))
	  (fix-label next-label)
	  (setq form (cdr form))))
      (increment-stack)
      (fix-label end-label)
      (emit-insn '(swap))
      (emit-insn '(pop))))
  (put 'case 'scheme-compile-fun compile-case)

  (defun do-predicate (form)
    (let* ((rep-fun (or (get (car form) 'scheme-compile-rep) (car form)))
	   (rep-compiler (get rep-fun 'rep-compile-fun)))
      (rep-compiler (cons rep-fun (cdr form)))))

  (defun compile-predicate (form)
    (do-predicate form)
    (emit-insn '(test-scm)))

  (defun compile-nil-predicate (form)
    (do-predicate form)
    (emit-insn '(test-scm-f)))

  ;; set properties of scheme functions that are pseudonyms of rep fns
  (mapc (lambda (cell)
	  (if (symbolp cell)
	      (put cell 'scheme-compile-fun (get cell 'rep-compile-fun))
	    (put (car cell) 'scheme-compile-fun
		 (get (cdr cell) 'rep-compile-fun))))
	'(list list* cons apply
	  (set-car! . rplaca)
	  (set-cdr! . rplacd)
	  (string-set! . aset)
	  (vector-set! . aset)
	  (string-ref . aref)
	  (vector-ref . aref)
	  length
	  (string-length . length)
	  (vector-length . length)
	  - + * / remainder modulo quotient max min floor ceiling
	  truncate round exp log sin cos tan sqrt expt
	  (string-copy . copy-sequence)
	  (vector-copy . copy-sequence)))

  ;; set properties of scheme predicates that are just rep fns with
  ;; booleans mapped from rep->scheme
  (mapc (lambda (cell)
	  (if (symbolp cell)
	      (put cell 'scheme-compile-fun compile-predicate)
	    (put (car cell) 'scheme-compile-fun compile-predicate)
	    (put (car cell) 'scheme-compile-rep (cdr cell))))
	'((eqv? . eql)
	  (eq? . eq)
	  (equal? . equal)
	  (pair? . consp)
	  (null? . null)
	  (symbol? . symbolp)
	  (number? . numberp)
	  = < > <= >=
	  (zero? . zerop)
	  (char=? . =)
	  (char<? . <)
	  (char>? . >)
	  (char<=? . <=)
	  (char>=? . >=)
	  (string? . stringp)
	  (string=? . =)
	  (string<? . <)
	  (string>? . >)
	  (string<=? . <=)
	  (string>=? . >=)
	  (vector? . vectorp)
	  (procedure? . functionp)))

  ;; set properties of scheme predicates that are just rep fns with
  ;; nil mapped to #f
  (mapc (lambda (cell)
	  (if (symbolp cell)
	      (put cell 'scheme-compile-fun compile-nil-predicate)
	    (put (car cell) 'scheme-compile-fun compile-nil-predicate)
	    (put (car cell) 'scheme-compile-rep (cdr cell))))
	'(memq memv member assq assoc))

  ;; setup properties to tell the compiler where to look for symbols
  ;; in the `scheme'  package
  (unless (get 'scheme 'compiler-handler-property)
    (put 'scheme 'compiler-handler-property 'scheme-compile-fun)
    (put 'scheme 'compiler-transform-property 'scheme-compile-transform)
    (put 'scheme 'compiler-sequencer 'begin)
    (put 'scheme 'compiler-pass-1 pass-1)
    (put 'scheme 'compiler-pass-2 pass-2)
    (put 'scheme 'compiler-foldablep foldablep)))
