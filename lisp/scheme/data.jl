#| scheme-data.jl -- data type functions

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

(define-structure scheme-data
    (export not eqv? eq? equal? boolean?

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

  ((open rep scheme-utils)
   (access rep))

;;; equivalence

  (define (not obj)
    (if (%test obj) #f #t))

  (define eqv? (make-predicate eql))
  (define eq? (make-predicate eq))
  (define equal? (make-predicate equal))

  (define boolean? (make-predicate (lambda (obj) (memq obj '(#f #t)))))

;;; pairs (cons cells)

  (define pair? (make-predicate consp))

  (define (car pair)
    (if (consp pair)
	(rep#car pair)
      (error "taking car of non-pair object")))
    
  (define (cdr pair)
    (if (consp pair)
	(rep#cdr pair)
      (error "taking cdr of non-pair object")))

  (define set-car! rplaca)
  (define set-cdr! rplacd)

  (define (caar x) (car (car x)))
  (define (cdar x) (cdr (car x)))
  (define (cadr x) (car (cdr x)))
  (define (cddr x) (cdr (cdr x)))
  
  (define (caaar x) (car (caar x)))
  (define (cdaar x) (cdr (caar x)))
  (define (cadar x) (car (cdar x)))
  (define (cddar x) (cdr (cdar x)))
  (define (caadr x) (car (cadr x)))
  (define (cdadr x) (cdr (cadr x)))
  (define (caddr x) (car (cddr x)))
  (define (cdddr x) (cdr (cddr x)))
  
  (define (caaaar x) (caar (caar x)))
  (define (cadaar x) (cadr (caar x)))
  (define (caadar x) (caar (cdar x)))
  (define (caddar x) (cadr (cdar x)))
  (define (caaadr x) (caar (cadr x)))
  (define (cadadr x) (cadr (cadr x)))
  (define (caaddr x) (caar (cddr x)))
  (define (cadddr x) (cadr (cddr x)))
  (define (cdaaar x) (cdar (caar x)))
  (define (cddaar x) (cddr (caar x)))
  (define (cdadar x) (cdar (cdar x)))
  (define (cdddar x) (cddr (cdar x)))
  (define (cdaadr x) (cdar (cadr x)))
  (define (cddadr x) (cddr (cadr x)))
  (define (cdaddr x) (cdar (cddr x)))
  (define (cddddr x) (cddr (cddr x)))

;;; lists

  (define null? (make-predicate null))
  (define list? (make-predicate listp))

  ;; XXX return nil if I > (length LST)
  (define (list-tail lst i) (nthcdr i lst))
  (define (list-ref lst i) (nth i lst))

  (define memq (make-nil-predicate rep#memq))
  (define memv (make-nil-predicate rep#memql))
  (define member (make-nil-predicate rep#member))

  (define assq (make-nil-predicate rep#assq))

  (define (assv obj alist)
    (let loop ((rest alist))
      (cond ((null rest) #f)
	    ((eql (caar rest) obj) (car rest))
	    (t (loop (cdr rest))))))

  (define assoc (make-nil-predicate rep#assoc))

;;; symbols

  ;; XXX this fails the spec clause "(symbol? '()) => #f"
  (define symbol? (make-predicate symbolp))

  (define symbol->string symbol-name)
  (define string->symbol intern)

;;; numbers

  (define number? (make-predicate numberp))
  (define (complex? obj) #t)
  (define real? (make-predicate realp))
  (define rational? (make-predicate rationalp))
  (define integer? (make-predicate integerp))

  (define exact? (make-predicate exactp))
  (define inexact? (make-predicate inexactp))

  (define = (make-predicate =))
  (define < (make-predicate <))
  (define > (make-predicate >))
  (define <= (make-predicate <=))
  (define >= (make-predicate >=))

  (define zero? (make-predicate zerop))
  (define positive? (make-predicate positivep))
  (define negative? (make-predicate negativep))
  (define odd? (make-predicate oddp))
  (define even? (make-predicate evenp))

  (define (/ . args)
    (cond ((cdr args) (apply rep#/ args))
	  (t (rep#/ 1 (car args)))))

  ;; XXX rep's gcd and lcm only take two parameters..
  
  (define (rationalize x y)
    (error "rationalize is unimplemented"))

  (define string->number (make-nil-predicate rep#string->number))

;;; characters

  (define char? (make-predicate fixnump))

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

  (define char-alphabetic? (make-predicate alpha-char-p))
  (define char-numeric? (make-predicate digit-char-p))
  (define char-whitespace? (make-predicate space-char-p))
  (define char-upper-case? (make-predicate upper-case-p))
  (define char-lower-case? (make-predicate lower-case-p))

  (define char->integer identity)
  (define integer->char identity)

;;; strings

  (define string? (make-predicate stringp))

  (define string concat)
  (define string-length length)
  (define string-ref aref)
  (define string-set! aset)

  (define string=? =)
  (define string<? <)
  (define string>? >)
  (define string<=? <=)
  (define string>=? >=)

  (define string-ci=? (make-predicate string-equal))
  (define string-ci<? (make-predicate string-lessp))
  (define string-ci>? (make-predicate
		       (lambda args
			 (or (apply string-equal args)
			     (not (apply string-lessp args))))))
  (define string-ci<=? (make-predicate
			(lambda args
			  (or (apply string-lessp args)
			      (apply string-equal args)))))
  (define string-ci>=? (make-predicate
			(lambda args
			  (not (apply string-lessp args)))))

  (define string-append concat)

  (define (string->list string)
    (let loop ((i (1- (length string)))
	       (out nil))
      (if (rep#>= i 0)
	  (loop (1- i) (cons (aref string i) out))
	out)))

  (define list->string concat)

  (define string-copy copy-sequence)

  (define (string-fill! string char)
    (let loop ((i (1- (length string))))
      (cond ((rep#>= i 0)
	     (aset string i char)
	     (loop (1- i))))))

;;; vectors

  (define vector? (make-predicate vectorp))

  (define vector-length length)
  (define vector-ref aref)
  (define vector-set! aset)

  (define (vector->list vect)
    (let loop ((i (1- (length vect)))
	       (out nil))
      (if (rep#>= i 0)
	  (loop (1- i) (cons (aref vect i) out))
	out)))

  (define (list->vector lst)
    (apply vector lst))

  (define (vector-fill! vect char)
    (let loop ((i (1- (length vect))))
      (cond ((rep#>= i 0)
	     (aset vect i char)
	     (loop (1- i)))))))
