#| compiler-scheme.jl -- inliners for compiling Scheme code

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

(define-structure compiler-scheme (export)

  (open rep
	lisp-doc
	compiler
	compiler-modules
	compiler-utils
	compiler-basic
	compiler-const
	compiler-inline
	compiler-lap
	compiler-vars
	compiler-bindings
	compiler-rep
	bytecodes)

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

  ;; setup properties to tell the compiler where to look for symbols
  ;; in the `scheme'  package
  (put 'scheme 'compiler-handler-property 'scheme-compile-fun)
  (put 'scheme 'compiler-transform-property 'scheme-compile-transform)


;;; pass 1 support

  (defun pass-1 (form)
    (unless (or (eq (car form) 'define) (memq (car form) top-level-compiled))
      (setq form (comp-macroexpand
		  form (lambda (in out)
			 (or (eq in out) (eq (car out) 'define)
			     (memq (car out) top-level-compiled))))))
    (when (eq (car form) 'define)
      (let ((name (cadr form)))
	(cond ((symbolp name)
	       (comp-remember-lexical-var (comp-constant-value name)))
	      ((and (consp name) (symbolp (car name)))
	       (comp-remember-fun (car name) (cdr name)))
	      (t (comp-error "Invalid define statement" form)))))

    form)

  (put 'scheme 'compiler-pass-1 pass-1)


;;; pass 2 support

  (defun pass-2 (form)
    (cond ((eq (car form) 'define)
	   (setq form (compile-define form)))
	  ((memq (car form) top-level-compiled)
	   (setq form (compile-form form))))
    form)

  (put 'scheme 'compiler-pass-2 pass-2)

  (defun compile-define (form)
    (let ((name (cadr form)))
      (if (symbolp name)
	  `(define ,name
	     ,(if (memq (car (nth 2 form)) top-level-compiled)
		  (compile-form (nth 2 form))
		(nth 2 form)))
	`(define ,(car name)
	   (make-closure
	    ,(comp-compile-lambda `(lambda ,(cdr name) ,@(cddr form))
				  (car name) 'begin)
	    ',(car name))))))


;;; source code transformations

  ;; tells the constant-folder which functions can be removed
;  (defun foldablep (name)
;    (memq name constant-functions))
;  (put 'scheme 'compiler-foldablep foldablep)

  ;; XXX the above doesn't work, the functions will be called from
  ;; XXX the wrong structure


;;; special compilers

  ;; module compilers from compiler-modules
  (put 'structure 'rep-compile-fun compile-structure)
  (put 'define-structure 'rep-compile-fun compile-define-structure)
  (put 'structure-ref 'rep-compile-fun compile-structure-ref)

  (put 'quote 'scheme-compile-fun (get 'quote 'rep-compile-fun))
  (put 'lambda 'scheme-compile-fun (get 'lambda 'rep-compile-fun))
  (put '%while 'scheme-compile-fun (get 'while 'rep-compile-fun))
  (put '%progn 'scheme-compile-fun (get 'progn 'rep-compile-fun))

  (defun compile-set! (form)
    (let ((sym (nth 1 form))
	  (val (nth 2 form)))
      (comp-compile-form val)
      (comp-write-op (bytecode dup))
      (comp-inc-stack)
      (comp-emit-varset sym)
      (comp-dec-stack)))
  (put 'set! 'scheme-compile-fun compile-set!)

  (defun compile-%test (form)
    (comp-compile-form (cadr form))
    (comp-write-op (bytecode scm-test)))
  (put '%test 'scheme-compile-fun compile-%test)

  ;; compile let* specially to coalesce all bindings into a single frame
  (put 'let* 'scheme-compile-fun (get 'let* 'rep-compile-fun))

  ;; let can be compiled straight from its macro definition

  ;; compile letrec specially to handle tail recursion elimination
  (put 'letrec 'scheme-compile-fun (get 'letrec 'rep-compile-fun))

  (put '%cond 'scheme-compile-fun (get 'cond 'rep-compile-fun))
  (put '%case 'scheme-compile-fun (get 'case 'rep-compile-fun))

  (mapc (lambda (cell)
	  (if (symbolp cell)
	      (put cell 'scheme-compile-fun (get cell 'rep-compile-fun))
	    (put (car cell) 'scheme-compile-fun
		 (get (cdr cell) 'rep-compile-fun))))
	'(list cons
	  (set-car! . rplaca)
	  (set-cdr! . rplacd)
	  (string-set! . aset)
	  (vector-set! . aset)
	  (string-ref . aref)
	  (vector-ref . aref)
	  length
	  (string-length . length)
	  (vector-length . length)
	  - + * remainder modulo quotient max min floor ceiling
	  truncate round exp log sin cos tan sqrt expt
	  (string-copy . copy-sequence)
	  (vector-copy . copy-sequence))))
