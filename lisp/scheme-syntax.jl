#| scheme-syntax.jl -- syntax macros

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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;; XXX add support for internal definitions (scanning lambda?)

;; ugh! rep's macros really suck when used across module boundaries..

(define-structure scheme-syntax (export quote lambda if set! cond case
					and or let let* letrec begin do
					delay define)
  ((open rep scheme-utils)
   (access rep))

;;; syntax

  ;; lambda is the usual rep special form (for now..)

  (defmacro if (test consequent . alternative)
    (rep#cond
     (alternative
      `(%cond ((%test ,test) ,consequent)
		  ('t ,@alternative)))
     (t `(%cond ((%test ,test) ,consequent)))))

  (defmacro set! (variable expression)
    `(%setq ,variable ,expression))

  (defmacro cond clauses
    (cons '%cond
	  (mapcar (lambda (x)
		    (rep#cond
		     ((eq (car x) 'else) `('t ,@(cdr x)))
		     (t `((%test ,(car x)) ,@(cdr x))))) clauses)))

  (defmacro case (key . clauses)
    (cons '%case
	  (mapcar (lambda (x)
		    (rep#cond
		     ((eq (car x) 'else) `('t ,@(cdr x)))
		     (t x))) clauses)))

  (defmacro or args
    (rep#cond
     ((null args) #f)
     ((null (cdr args)) (car args))
     (t ((lambda (tem)
	   `((lambda (,tem)
	       (if ,tem ,tem (or ,@(cdr args))))
	     ,(car args)))
	 (gensym)))))

  (defmacro and args
    (rep#cond
     ((null (cdr args)) (car args))
     (t `(cond (,(car args) (and ,@(cdr args))) (else #f)))))

  (defmacro let args
    ((lambda (fun vars values)
       (rep#cond
	((symbolp (car args))
	 ;; named let
	 (setq fun (car args))
	 (setq args (cdr args))))
       (setq vars (mapcar (lambda (x)
			    (rep#cond
			     ((consp x) (car x))
			     (t x)))
			  (car args)))
       (setq values (mapcar (lambda (x)
			      (rep#cond
			       ((consp x) (cons 'begin (cdr x)))
			       (t #f)))
			    (car args)))
       (rep#cond
	(fun
	 ;; use the progn so the compiler notices the inner letrec
	 ;; (else it will get macroexpanded away too soon)
	 (list 'begin
	       (list 'letrec
		     (list (list fun (list* 'lambda vars (cdr args))))
		     (cons fun values))))
	(t (cons (list* 'lambda vars (cdr args)) values))))
     nil nil nil))

  (defmacro let* (bindings . body)
    (rep#cond
     ((null bindings) (cons 'begin body))
     (t `((lambda (,(caar bindings)) (let* ,(cdr bindings) ,@body))
	  ,(cadar bindings)))))

  (defmacro letrec (bindings . body)
    ((lambda (vars setters)
       (list* 'let vars (nconc setters body)))
     (mapcar (lambda (x)
	       (rep#cond
		((consp x) (car x))
		(t x)))
	     bindings)
     (mapcar (lambda (x)
	       (rep#cond
		((consp x) (cons 'set! x))
		(t (list 'set! x nil))))
	     bindings)))

  (defmacro begin forms
    (cons '%progn forms))

  ;; I could have written this using named let, but since rep currently
  ;; doesn't have a tail-recursive interpreter (only a compiler), using
  ;; `while' may be more useful..
  (defmacro do (vars test . body)
    `(let ,(mapcar (lambda (var)
		     (list (car var) (cadr var))) vars)
       (%while (%test (not ,(car test)))
	 ,@body
	 ,@(mapcar (lambda (var)
		     `(set! ,(car var) ,(rep#cond
					 ((cddr var) (caddr var))
					 (t (car var))))) vars))
       ,@(cdr test)))

  (defmacro delay (expression)
    `(%make-promise (lambda () ,expression)))

  (defmacro define args
    (rep#cond
     ((symbolp (car args)) (cons 'set! args))
     (t `(set! ,(caar args) (lambda ,(cdar args) ,@(cdr args)))))))
