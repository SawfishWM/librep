#| utils.jl -- misc functions

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

(define-structure scheme.utils

    (export #f #t \#test
	    make-predicate
	    make-nil-predicate)

    (open rep)

  (defconst #f #f)
  (defconst #t #t)

  ;; given a scheme boolean, convert to a rep boolean
  (define (\#test value) (not (eq value #f)))

  ;; create a scheme predicate from the rep predicate PRED
  (define (make-predicate pred)
    (lambda args
      (if (apply pred args) #t #f)))

  ;; create a scheme `pseudo-predicate' from the rep `pseudo-predicate'
  ;; PRED. `pseudo-predicate' means that it returns false or an
  ;; interesting non-false value
  (define (make-nil-predicate pred)
    (lambda args
      (cond ((apply pred args)) (t #f)))))
