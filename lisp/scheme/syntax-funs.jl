#| syntax-funs.jl -- syntax expansion functions

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

(define-structure scheme.syntax-funs

    (export expand-lambda
	    expand-if
	    expand-set!
	    expand-cond
	    expand-case
	    expand-and
	    expand-or
	    expand-let
	    expand-let*
	    expand-letrec
	    expand-do
	    expand-delay
	    expand-define)

    (open rep scheme.utils)

;;; syntax

  ;; returns (VAR BODY) suitable for putting in a letrec
  (define (parse-define form)
    (let loop ((name (cadr form))
	       (body (caddr form)))
      (if (symbolp name)
	  `(,name ,body)
	(loop (car name) `(lambda ,(cdr name) ,body)))))

  (define (expand-lambda vars . body)
    (let (header)
      (while (eq (caar body) 'define)
	(setq header (cons (parse-define (car body)) header))
	(setq body (cdr body)))
     (if header
	 `(\#lambda ,vars (letrec ,(nreverse header) ,@body))
       `(\#lambda ,vars ,@body))))

  (define (expand-if test consequent . alternative)
    (cond ((cdr alternative)
	   (error "Scheme `if' only takes one else form"))
	  (alternative
	   `(\#cond ((\#test ,test) ,consequent)
		   ('t ,(car alternative))))
	  (t `(\#cond ((\#test ,test) ,consequent)))))

  (define (expand-set! variable expression)
    `(\#setq ,variable ,expression))

  (define (expand-cond . args)
    (let ((first (car args))
	  (rest (cdr args)))
      (cond ((null args) #f)
	    ((eq (car first) 'else)
	     `(begin ,@(cdr first)))
	    ((eq (cadr first) '=>)
	     (let ((tem (gensym)))
	       `(let ((,tem ,(car first)))
		  (if ,tem
		      (,(caddr first) ,tem)
		    ,@(and rest `((cond ,@rest)))))))
	    (t `(if ,(car first)
		    (begin ,@(cdr first))
		  ,@(and rest `((cond ,@rest))))))))

  (define (expand-case key . clauses)
    (let ((tem (gensym)))
      (let loop ((body nil)
		 (rest clauses))
	(if rest
	    (let ((this (car rest)))
	      (loop (cons (cond ((eq (car this) 'else) `(else ,@(cdr this)))
				((cdar this)
				 `((memv ,tem ',(car this)) ,@(cdr this)))
				(t `((eqv? ,tem ',(caar this)) ,@(cdr this))))
			  body)
		    (cdr rest)))
	  `(let ((,tem ,key))
	     (cond ,@(nreverse body)))))))

  (define (expand-or . args)
    (cond
     ((null args) #f)
     ((null (cdr args)) (car args))
     (t (let ((tem (gensym)))
	  `((lambda (,tem)
	      (if ,tem ,tem (or ,@(cdr args))))
	    ,(car args))))))

  (define (expand-and . args)
    (cond
     ((null args) '#t)
     ((null (cdr args)) (car args))
     (t `(cond (,(car args) (and ,@(cdr args))) (else #f)))))

  (define (expand-let . args)
    (let (fun vars values)
      (when (and (car args) (symbolp (car args)))
	;; named let
	(setq fun (car args))
	(setq args (cdr args)))
      (setq vars (mapcar car (car args)))
      (setq values (mapcar cadr (car args)))
      (if fun
	  ;; use the progn so the compiler notices the inner letrec
	  ;; (else it will get macroexpanded away too soon)
	  `(begin (letrec
		      ((,fun (lambda ,vars ,@(cdr args))))
		    (,fun ,@values)))
	`((lambda ,vars ,@(cdr args)) ,@values))))

  (define (expand-let* bindings . body)
    (if (null bindings)
	`((lambda () ,@body))
      `((lambda (,(caar bindings))
	  (let* ,(cdr bindings) ,@body))
	,(cadar bindings))))

  (define (expand-letrec bindings . body)
    (let ((vars (mapcar car bindings))
	  (setters (mapcar (lambda (x) `(set! ,@x)) bindings))
	  (initial (make-list (length bindings) ''nil)))
      `((lambda ,vars ,@setters ,@body) ,@initial)))

  (define (expand-do vars test . body)
    (let ((tem (gensym)))
      `(let ,tem ,(mapcar (lambda (var)
			    (list (car var) (cadr var))) vars)
	 (if ,(car test)
	     (begin ,@(cdr test))
	   (begin
	    ,@body
	    (,tem ,@(mapcar (lambda (var)
			      (if (cddr var)
				  (caddr var)
				(car var))) vars)))))))

  (define (expand-delay expression)
    `(\#make-promise (lambda () ,expression)))

  (define (expand-define . args)
    (if (symbolp (car args))
	(cons 'set! args)
      `(define ,(caar args) (lambda ,(cdar args) ,@(cdr args))))))
