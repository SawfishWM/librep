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
					 compile-constant
					 add-constant)
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
  (defun compile-constant (form)
    (cond
     ((eq form nil)
      (emit-insn (bytecode nil)))
     ((eq form t)
      (emit-insn (bytecode t)))
     ((and (integerp form) (<= form 65535) (>= form -65535))
      ;; use one of the pushi instructions
      (cond ((zerop form)
	     (emit-insn (bytecode pushi-0)))
	    ((= form 1)
	     (emit-insn (bytecode pushi-1)))
	    ((= form 2)
	     (emit-insn (bytecode pushi-2)))
	    ((= form -1)
	     (emit-insn (bytecode pushi-minus-1)))
	    ((= form -2)
	     (emit-insn (bytecode pushi-minus-2)))
	    ((and (<= form 127) (>= form -128))
	     (emit-insn (bytecode pushi) (logand form 255)))
	    ((and (< form 0) (>= form -65535))
	     (emit-insn (bytecode pushi-pair-neg) (- form)))
	    (t
	     (emit-insn (bytecode pushi-pair-pos) form))))
     (t
      (emit-insn (bytecode push) (add-constant form))))
    (increment-stack))

  ;; Put a constant into the alist of constants, returning its index number.
  ;; It won't be added twice if it's already there.
  (defun add-constant (const)
    (or (cdr (assoc const comp-constant-alist))
	(progn
	  (setq comp-constant-alist (cons (cons const comp-constant-index)
					  comp-constant-alist)
		comp-constant-index (1+ comp-constant-index))
	  (1- comp-constant-index)))))
