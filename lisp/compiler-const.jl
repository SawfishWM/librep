#| compiler-const.jl -- compiling constants

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

(define-structure compiler-const (export make-constant-vector
					 comp-compile-constant
					 comp-add-constant)
  (open rep
	compiler-lap
	compiler-utils
	compiler-vars
	bytecodes)

  ;; Turn the alist of constants into a vector
  (defun make-constant-vector ()
    (let
	((vec (make-vector comp-constant-index)))
      (mapc (lambda (cell)
	      (aset vec (cdr cell) (car cell))) comp-constant-alist)
      vec))

  ;; Push a constant onto the stack
  (defun comp-compile-constant (form)
    (cond
     ((eq form nil)
      (comp-write-op (bytecode nil)))
     ((eq form t)
      (comp-write-op (bytecode t)))
     ((and (integerp form) (<= form 65535) (>= form -65535))
      ;; use one of the pushi instructions
      (cond ((zerop form)
	     (comp-write-op (bytecode pushi-0)))
	    ((= form 1)
	     (comp-write-op (bytecode pushi-1)))
	    ((= form 2)
	     (comp-write-op (bytecode pushi-2)))
	    ((= form -1)
	     (comp-write-op (bytecode pushi-minus-1)))
	    ((= form -2)
	     (comp-write-op (bytecode pushi-minus-2)))
	    ((and (<= form 127) (>= form -128))
	     (comp-write-op (bytecode pushi) (logand form 255)))
	    ((and (< form 0) (>= form -65535))
	     (comp-write-op (bytecode pushi-pair-neg) (- form)))
	    (t
	     (comp-write-op (bytecode pushi-pair-pos) form))))
     (t
      (comp-write-op (bytecode push) (comp-add-constant form))))
    (comp-inc-stack))

  ;; Put a constant into the alist of constants, returning its index number.
  ;; It won't be added twice if it's already there.
  (defun comp-add-constant (const)
    (or (cdr (assoc const comp-constant-alist))
	(progn
	  (setq comp-constant-alist (cons (cons const comp-constant-index)
					  comp-constant-alist)
		comp-constant-index (1+ comp-constant-index))
	  (1- comp-constant-index)))))
