#| scheme.jl -- foolish attempt to run Scheme code

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

;; the only things really missing from r5rs are macros and multiple
;; values, r4rs should be reasonably okay

;; other warts:

;;   (eq? 'FOO 'foo) => #f, guile also fails here
;;   `(..) => (backquote ..) not (quasiquote ..)
;;   no separate character type
;;   reader doesn't handle numbers like: 15#

;; to test this, run the repl, then: ",new test" and ",open scheme"

(define-structure scheme (export

			  #f #t

			  ;; syntax

			  quote lambda if set! cond case and or
			  let let* letrec begin do delay define

			  ;; built-in procedures

			  not eqv? eq? equal? boolean?

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
			  list->vector vector-fill!

			  procedure? apply map for-each force
			  \#make-promise call-with-current-continuation
			  call/cc dynamic-wind eval
			  scheme-report-environment null-environment
			  interaction-environment

			  call-with-input-file call-with-output-file
			  input-port? output-port? current-input-port
			  current-output-port with-input-from-file
			  with-output-to-file open-input-file
			  open-output-file close-input-port
			  close-output-port

			  read read-char peek-char eof-object?
			  write display newline write-char load

			  ;; exported local kludges
			  \#cond \#setq \#progn \#test \#lambda
			  backquote list* %load-suffixes)
  ((access rep)
   (open scheme.utils
	 scheme.syntax
	 scheme.data
	 scheme.misc))

  (rep#setq \#cond rep#cond)
  (rep#setq \#setq rep#setq)
  (rep#setq \#progn rep#progn)
  (rep#setq \#lambda rep#lambda)
  (rep#setq backquote rep#backquote)
  (rep#setq list* rep#list*))
