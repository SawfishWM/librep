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

;; ugh! rep's macros really suck when used across module boundaries..

(define-structure scheme-syntax (export quote lambda if set! cond case
					and or let let* letrec begin do
					delay define)
  ((open rep scheme-utils)
   (access rep))

;;; syntax

  (defmacro lambda (vars . body)
    ((rep#lambda (header)
       (while (eq (caar body) 'define)
	 (setq header (cons (parse-define (car body)) header))
	 (setq body (cdr body)))
     (rep#cond
      (header `(%lambda ,vars (letrec ,(nreverse header) ,@body)))
      (t `(%lambda ,vars ,@body))))
     nil))

  (defmacro if (test consequent . alternative)
    (rep#cond
     ((cdr alternative)
      (error "Scheme `if' only takes one else form"))
     (alternative
      `(%cond ((%test ,test) ,consequent)
	      ('t ,(car alternative))))
     (t `(%cond ((%test ,test) ,consequent)))))

  (defmacro set! (variable expression)
    `(%setq ,variable ,expression))

  (defmacro cond args
    ((rep#lambda (first rest)
      (rep#cond ((null args) #f)
		((eq (car first) 'else)
		 `(begin ,@(cdr first)))
		((eq (cadr first) '=>)
		 ((rep#lambda (tem)
		   `(let ((,tem ,(car first)))
		      (if ,tem
			  (,(caddr first) ,tem)
			,@(rep#cond (rest `((cond ,@rest)))))))
		  (gensym)))
		(t `(if ,(car first)
			(begin ,@(cdr first))
		      ,@(rep#cond (rest `((cond ,@rest))))))))
     (car args) (cdr args)))

  (defmacro case (key . clauses)
    (list* '%case key
	  (mapcar (rep#lambda (x)
		    (rep#cond
		     ((eq (car x) 'else) `(t ,@(cdr x)))
		     (t x))) clauses)))

  (defmacro or args
    (rep#cond
     ((null args) #f)
     ((null (cdr args)) (car args))
     (t ((rep#lambda (tem)
	   `((lambda (,tem)
	       (if ,tem ,tem (or ,@(cdr args))))
	     ,(car args)))
	 (gensym)))))

  (defmacro and args
    (rep#cond
     ((null args) '#t)
     ((null (cdr args)) (car args))
     (t `(cond (,(car args) (and ,@(cdr args))) (else #f)))))

  (defmacro let args
    ((rep#lambda (fun vars values)
       (rep#cond
	((rep#cond ((car args) (symbolp (car args))))	;and expanded
	 ;; named let
	 (setq fun (car args))
	 (setq args (cdr args))))
       (setq vars (mapcar (rep#lambda (x)
			    (rep#cond
			     ((consp x) (car x))
			     (t x)))
			  (car args)))
       (setq values (mapcar (rep#lambda (x)
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
    ((rep#lambda (vars setters)
       (list* 'let vars (nconc setters body)))
     (mapcar (rep#lambda (x)
	       (rep#cond
		((consp x) (car x))
		(t x)))
	     bindings)
     (mapcar (rep#lambda (x)
	       (rep#cond
		((consp x) (cons 'set! x))
		(t (list 'set! x nil))))
	     bindings)))

  (defmacro begin forms
    (cons '%progn forms))

  ;; initially I wrote this using while, to avoid the non-tail-recursive
  ;; interpreter, but that doesn't create fresh bindings for each
  ;; iteration
  (defmacro do (vars test . body)
    ((rep#lambda (tem)
       `(let ,tem ,(mapcar (rep#lambda (var)
			     (list (car var) (cadr var))) vars)
	  (if ,(car test)
	      (begin ,@(cdr test))
	    (begin
	     ,@body
	     (,tem ,@(mapcar (rep#lambda (var)
			       (rep#cond
			        ((cddr var) (caddr var))
			        (t (car var))))
			     vars))))))
     (gensym)))

  (defmacro delay (expression)
    `(%make-promise (lambda () ,expression)))

  (defmacro define args
    (rep#cond
     ((symbolp (car args)) (cons 'set! args))
     (t `(define ,(caar args) (lambda ,(cdar args) ,@(cdr args)))))))
