;; string-util.jl -- some more string functions
;; $Id$

;;  Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of librep.

;; librep is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; librep is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with librep; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;###autoload
(defun string-upper-case-p (x)
  "Return t if string X is upper case (contains no lower case characters)."
  (letrec
      ((iter (lambda (point)
	       (cond ((>= point (length x)) t)
		     ((lower-case-p (aref x point)) nil)
		     (t (iter (1+ point)))))))
    (iter 0)))

;;;###autoload
(defun string-lower-case-p (x)
  "Return t if string X is lower case (contains no upper case characters)."
  (letrec
      ((iter (lambda (point)
	       (cond ((>= point (length x)) t)
		     ((upper-case-p (aref x point)) nil)
		     (t (iter (1+ point)))))))
    (iter 0)))

;;;###autoload
(defun string-capitalized-p (x)
  "Returns t if string X is capitalized (first character is upper case)."
  (upper-case-p (aref x 0)))

;;;###autoload
(defun string-upcase (x)
  "Return a new string, an upper case copy of string X."
  (translate-string (copy-sequence x) upcase-table))

;;;###autoload
(defun string-downcase (x)
  "Return a new string, a lower case copy of string X."
  (translate-string (copy-sequence x) downcase-table))

;;;###autoload
(defun capitalize-string (x)
  "Return a new string, a copy of X with its first character in upper case."
  (let
      ((new (copy-sequence x)))
    (aset new 0 (char-upcase (aref new 0)))
    new))
