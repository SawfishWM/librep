#| data.jl -- data type functions

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

(define-structure unscheme.data

    (export #f #t not eqv? eq? equal? boolean?

	    pair? cons car cdr set-car! set-cdr!
	    caar cadr cdar cddr
	    caaar caadr cadar caddr
	    cdaar cdadr cddar cdddr
	    caaaar caaadr caadar caaddr
	    cadaar cadadr caddar cadddr
	    cdaaar cdaadr cdadar cdaddr
	    cddaar cddadr cdddar cddddr

	    null? list? list length append reverse
	    list-tail list-ref memq memv member
	    assq assv assoc

	    symbol? symbol->string string->symbol
	    
	    number? complex? real? rational? integer?
	    exact? inexact? = < > <= >= zero? positive?
	    negative? odd? even? max min + * - /
	    abs quotient remainder modulo gcd lcm
	    numerator denominator floor ceiling
	    truncate round rationalize exp log sin cos
	    tan asin acos atan sqrt expt exact->inexact
	    inexact->exact string->number number->string
	    
	    char? char=? char<? char>? char<=? char>=?
	    char-ci=? char-ci<? char-ci>? char-ci<=?
	    char-ci>=? char-alphabetic? char-numeric?
	    char-whitespace? char-upper-case?
	    char-lower-case? char->integer integer->char
	    char-upcase char-downcase
	    
	    string? make-string string string-length
	    string-ref string-set! string=? string-ci=?
	    string<? string>? string<=? string>=?
	    string-ci<? string-ci>? string-ci<=?
	    string-ci>=? substring string-append
	    string->list list->string string-copy
	    string-fill!
	    
	    vector? make-vector vector vector-length
	    vector-ref vector-set! vector->list
	    list->vector vector-fill!)

    ((open rep)
     (access rep))

  (defconst #f ())
  (defconst #t t)

;;; equivalence

  (define eqv? eql)
  (define eq? eq)
  (define equal? equal)

  (define (boolean? obj) (and (memq obj '(() t #f #t)) #t))

;;; pairs (cons cells)

  (define pair? consp)

  (define set-car! rplaca)
  (define set-cdr! rplacd)

;;; lists

  (define null? null)

  (define (list? x)
    (let loop ((slow x)
	       (fast (cdr x)))
      (cond ((null slow) #t)
	    ((not (consp slow)) #f)
	    ((eq slow fast) #f)
	    (t (loop (cdr slow) (cddr fast))))))

  ;; XXX return nil if I > (length LST)
  (define (list-tail lst i) (nthcdr i lst))
  (define (list-ref lst i) (nth i lst))

  (define memv memql)

  (define (assv obj alist)
    (let loop ((rest alist))
      (cond ((null rest) #f)
	    ((eql (caar rest) obj) (car rest))
	    (t (loop (cdr rest))))))

;;; symbols

  (define (symbol? arg)
    (cond ((memq arg '(#f #t)) #f)
	  ((symbolp arg) #t)
	  (t #f)))
    
  (define symbol->string symbol-name)

  (define (string->symbol name)
    (if (string= name "nil")
	'nil
      (or (find-symbol name)
	  ;; The copying is needed to pass test.scm..
	  (intern (copy-sequence name)))))

;;; numbers

  (define number? numberp)
  (define (complex? obj)
    (declare (unused obj))
    #t)
  (define real? realp)
  (define rational? rationalp)
  (define integer? integerp)

  (define exact? exactp)
  (define inexact? inexactp)

  (define zero? zerop)
  (define positive? positivep)
  (define negative? negativep)
  (define odd? oddp)
  (define even? evenp)

  (define (rationalize x y)
    (declare (unused x y))
    (error "rationalize is unimplemented"))

;;; characters

  (define char? fixnump)

  (define char=? =)
  (define char<? <)
  (define char>? >)
  (define char<=? <=)
  (define char>=? >=)

  ;; XXX slow...
  (define (char-ci=? . args) (apply char=? (mapcar char-upcase args)))
  (define (char-ci<? . args) (apply char<? (mapcar char-upcase args)))
  (define (char-ci>? . args) (apply char>? (mapcar char-upcase args)))
  (define (char-ci<=? . args) (apply char<=? (mapcar char-upcase args)))
  (define (char-ci>=? . args) (apply char>=? (mapcar char-upcase args)))

  (define char-alphabetic? alpha-char-p)
  (define char-numeric? digit-char-p)
  (define char-whitespace? space-char-p)
  (define char-upper-case? upper-case-p)
  (define char-lower-case? lower-case-p)

  (define char->integer identity)
  (define integer->char identity)

;;; strings

  (define string? stringp)

  (define string concat)
  (define string-length length)
  (define string-ref aref)
  (define string-set! aset)

  (define string=? =)
  (define string<? <)
  (define string>? >)
  (define string<=? <=)
  (define string>=? >=)

  (define string-ci=? string-equal)
  (define string-ci<? string-lessp)
  (define string-ci>? (lambda args
			(not (or (apply string-equal args)
				 (apply string-lessp args)))))
  (define string-ci<=? (lambda args
			 (or (apply string-lessp args)
			     (apply string-equal args))))
  (define string-ci>=? (lambda args
			 (not (apply string-lessp args))))

  (define string-append concat)

  (define (string->list string)
    (let loop ((i (1- (length string)))
	       (out nil))
      (if (>= i 0)
	  (loop (1- i) (cons (aref string i) out))
	out)))

  (define list->string concat)

  (define string-copy copy-sequence)

  (define (string-fill! string char)
    (let loop ((i (1- (length string))))
      (cond ((>= i 0)
	     (aset string i char)
	     (loop (1- i))))))

;;; vectors

  (define vector? vectorp)

  (define vector-length length)
  (define vector-ref aref)
  (define vector-set! aset)

  (define (vector->list vect)
    (let loop ((i (1- (length vect)))
	       (out nil))
      (if (>= i 0)
	  (loop (1- i) (cons (aref vect i) out))
	out)))

  (define (list->vector lst)
    (apply vector lst))

  (define (vector-fill! vect char)
    (let loop ((i (1- (length vect))))
      (cond ((>= i 0)
	     (aset vect i char)
	     (loop (1- i)))))))
