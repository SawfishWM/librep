#| rep.lang.math bootstrap

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

(declare (in-module rep.lang.math))

(open-structures '(rep.lang.symbols
		   rep.data))

;; numeric functions

(defun realp (x)
  "Return t if X is a real number."
  (numberp x))

(defun rationalp (x)
  "Return t if X is a (possibly inexact) rational number."
  (numberp x))

(defun inexactp (x)
  "Return t if X is an inexact number."
  (and (numberp x) (not (exactp x))))

(defun positivep (x)
  "Return t if X is greater than zero."
  (> x 0))

(defun negativep (x)
  "Return t if X is less than zero."
  (< x 0))

(defun oddp (x)
  "Return t if X is odd, i.e. (/= (mod X 2) 0)."
  (not (zerop (mod x 2))))

(defun evenp (x)
  "Return t if X is odd, i.e. (= (mod X 2) 0)."
  (zerop (mod x 2)))

(defun abs (x)
  "Return the absolute value of X, i.e. (max X (- X))."
  (max x (- x)))

(defun lcm args
  "Return the least common multiple of integers A and B."
  (if (null args)
      1
    (quotient (apply * (mapcar abs args)) (apply gcd args))))

(%define % remainder)
(%define modulo mod)
(%define lsh ash)

;; exports

(export-bindings '(realp rationalp inexactp positivep negativep
		   oddp evenp abs lcm % modulo lsh))
