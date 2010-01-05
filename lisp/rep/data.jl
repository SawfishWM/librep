#| rep.data bootstrap

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

(declare (in-module rep.data))

(open-structures '(rep.regexp
		   rep.io.files))

(defun assoc-regexp (input alist #!optional fold-case)
  "Scan ALIST for an element whose car is a regular expression matching the
string INPUT."
  (catch 'return
    (mapc (lambda (cell)
	    (when (string-match (car cell) input nil fold-case)
	      (throw 'return cell))) alist)))

(defun setcar (cell x) (rplaca cell x) x)
(defun setcdr (cell x) (rplacd cell x) x)

;; Some function pseudonyms
(%define string= equal)
(%define string< <)

(defun member-if (fun lst)
  "Similar to the `member' function, except that the function FUN is
called to test the elements for matches. If `(FUN ELT)' returns true,
then the sublist starting with ELT is returned."
  (cond ((null lst) '())
	((fun (car lst)) lst)
	(t (member-if fun (cdr lst)))))

(defun remove-if (pred lst)
  "Returns a new copy of LST with any elements removed for which (PRED ELT)
returns true."
  (let loop ((rest lst)
	     (out '()))
    (cond ((null rest) (nreverse out))
	  ((pred (car rest)) (loop (cdr rest) out))
	  (t (loop (cdr rest) (cons (car rest) out))))))

(defun remove-if-not (fun lst)
  "Returns a new copy of LST with any elements removed for which (PRED ELT)
returns false."
  (remove-if (lambda (x) (not (fun x))) lst))

(defun remove (elt lst)
  "Returns a new copy of LST with all elements `equal' to ELT discarded."
  (remove-if (lambda (x) (equal x elt)) lst))

(defun remq (elt lst)
  "Returns a new copy of LST with all elements `eq' to ELT discarded."
  (remove-if (lambda (x) (eq x elt)) lst))

(export-bindings '(assoc-regexp setcar setcdr string= string<
		   member-if remove-if remove-if-not remove remq))


;; cons accessors

(defun caar (x) (car (car x)))
(defun cdar (x) (cdr (car x)))
(defun cadr (x) (car (cdr x)))
(defun cddr (x) (cdr (cdr x)))

(defun caaar (x) (car (caar x)))
(defun cdaar (x) (cdr (caar x)))
(defun cadar (x) (car (cdar x)))
(defun cddar (x) (cdr (cdar x)))
(defun caadr (x) (car (cadr x)))
(defun cdadr (x) (cdr (cadr x)))
(defun caddr (x) (car (cddr x)))
(defun cdddr (x) (cdr (cddr x)))

(defun caaaar (x) (caar (caar x)))
(defun cadaar (x) (cadr (caar x)))
(defun caadar (x) (caar (cdar x)))
(defun caddar (x) (cadr (cdar x)))
(defun caaadr (x) (caar (cadr x)))
(defun cadadr (x) (cadr (cadr x)))
(defun caaddr (x) (caar (cddr x)))
(defun cadddr (x) (cadr (cddr x)))
(defun cdaaar (x) (cdar (caar x)))
(defun cddaar (x) (cddr (caar x)))
(defun cdadar (x) (cdar (cdar x)))
(defun cdddar (x) (cddr (cdar x)))
(defun cdaadr (x) (cdar (cadr x)))
(defun cddadr (x) (cddr (cadr x)))
(defun cdaddr (x) (cdar (cddr x)))
(defun cddddr (x) (cddr (cddr x)))

(export-bindings '(caar cdar cadr cddr caaar cdaar cadar cddar caadr
		   cdadr caddr cdddr caaaar cadaar caadar caddar
		   caaadr cadadr caaddr cadddr cdaaar cddaar cdadar
		   cdddar cdaadr cddadr cdaddr cddddr))


;; vector utils

(defun vector->list (vec)
  (do ((i 0 (1+ i))
       (out '() (cons (aref vec i) out)))
      ((= i (length vec)) (nreverse out))))

(defun list->vector (lst)
  (apply vector lst))

(export-bindings '(vector->list list->vector))


;; guardian wrapper

(defun make-guardian ()
  "Create a new guardian. Guardians provide a means of protecting data
objects from deallocation when they have no extant references.

`make-guardian' returns a function representing a single guardian.
Calling this function with a single argument adds that value to the
list of objects protected by the guardian. Calling the function with no
arguments returns one of the objects that would otherwise have been
deallocated by the garbage collector, or false if no such objects
exist that have not already been returned."
  (let ((g (make-primitive-guardian)))
    (lambda args
      (if args
	  (primitive-guardian-push g (car args))
	(primitive-guardian-pop g)))))

(export-bindings '(make-guardian))


;; autoloads

(autoload 'string-upper-case-p "rep/data/string-util")
(autoload 'string-lower-case-p "rep/data/string-util")
(autoload 'string-capitalized-p "rep/data/string-util")
(autoload 'string-upcase "rep/data/string-util")
(autoload 'string-downcase "rep/data/string-util")
(autoload 'capitalize-string "rep/data/string-util")
(autoload 'mapconcat "rep/data/string-util")
(autoload 'sort "rep/data/sort")

(export-bindings '(string-upper-case-p string-lower-case-p string-capitalized-p
		   string-upcase string-downcase capitalize-string
		   mapconcat sort upcase-table downcase-table flatten-table))
