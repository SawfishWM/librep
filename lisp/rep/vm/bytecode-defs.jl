#| bytecode-defs.jl -- low-level details of vm bytecodes

   $Id$

   Copyright (C) 1993, 1994, 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of Librep.

   Librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure rep.vm.bytecode-defs

    (export bytecode-major
	    bytecode-minor
	    bytecode
	    bytecode-ref
	    byte-max-1-byte-arg
	    byte-max-2-byte-arg
	    byte-max-3-byte-arg
	    byte-insn-stack-delta)

    (open rep)

  ;; Instruction set version
  (defconst bytecode-major 11)
  (defconst bytecode-minor 0)

  ;; macro to get a named bytecode
  (defmacro bytecode (name)
    (cdr (assq name bytecode-alist)))

  (define (bytecode-ref name)
    (or (cdr (assq name bytecode-alist))
	(error "No such instruction: %s" name)))

  (define bytecode-alist
    '((slot-ref . #x00)
      (call . #x08)			;call (stk[n] stk[n-1] ... stk[0])
					; pops n values, replacing the
					; function with the result.
      (push . #x10)			;pushes constant # n
      (refg . #x18)			;pushes val of symbol n (in c-v)
      (setg . #x20)			;sets sym n (in c-v) to stk[0]; pop
      (setn . #x28)
      (slot-set . #x30)
      (refn . #x38)

      (last-with-args . #x3f)

      (ref . #x40)			;replace symbol with it's value
      (%set . #x41)
      (fluid-ref . #x42)
      (enclose . #x43)
      (init-bind . #x44)		;initialise a new set of bindings
      (unbind . #x45)			;unbind all bindings in the top set
      (dup . #x46)			;duplicate top of stack
      (swap . #x47)			;swap top two values on stack
      (pop . #x48)			;pops the stack

      (nil . #x49)			;pushes ()
      (t . #x4a)			;pushes t
      (cons . #x4b)
      (car . #x4c)
      (cdr . #x4d)
      (rplaca . #x4e)
      (rplacd . #x4f)
      (nth . #x50)
      (nthcdr . #x51)
      (aset . #x52)
      (aref . #x53)
      (length . #x54)
      (bind . #x55)
      (add . #x56)			;adds the top two values
      (neg . #x57)
      (sub . #x58)
      (mul . #x59)
      (div . #x5a)
      (rem . #x5b)
      (lnot . #x5c)
      (not . #x5d)
      (lor . #x5e)
      (land . #x5f)
      (equal . #x60)
      (eq . #x61)
      (structure-ref . #x62)
      (scm-test . #x63)
      (gt . #x64)
      (ge . #x65)
      (lt . #x66)
      (le . #x67)
      (inc . #x68)
      (dec . #x69)
      (ash . #x6a)
      (zerop . #x6b)
      (null . #x6c)
      (atom . #x6d)
      (consp . #x6e)
      (listp . #x6f)
      (numberp . #x70)
      (stringp . #x71)
      (vectorp . #x72)
      (catch . #x73)
      (throw . #x74)
      (binderr . #x75)
      (return . #x76)
      (unbindall . #x77)
      (boundp . #x78)
      (symbolp . #x79)
      (get . #x7a)
      (put . #x7b)
      (errorpro . #x7c)
      (signal . #x7d)
      (quotient . #x7e)
      (reverse . #x7f)
      (nreverse . #x80)
      (assoc . #x81)
      (assq . #x82)
      (rassoc . #x83)
      (rassq . #x84)
      (last . #x85)
      (mapcar . #x86)
      (mapc . #x87)
      (member . #x88)
      (memq . #x89)
      (delete . #x8a)
      (delq . #x8b)
      (delete-if . #x8c)
      (delete-if-not . #x8d)
      (copy-sequence . #x8e)
      (sequencep . #x8f)
      (functionp . #x90)
      (special-form-p . #x91)
      (subrp . #x92)
      (eql . #x93)
      (lxor . #x94)
      (max . #x95)
      (min . #x96)
      (filter . #x97)
      (macrop . #x98)
      (bytecodep . #x99)

      (pushi-0 . #x9a)
      (pushi-1 . #x9b)
      (pushi-2 . #x9c)
      (pushi-minus-1 . #x9d)
      (pushi-minus-2 . #x9e)
      (pushi . #x9f)
      (pushi-pair-neg . #xa0)
      (pushi-pair-pos . #xa1)

      (caar . #xa2)
      (cadr . #xa3)
      (cdar . #xa4)
      (cddr . #xa5)

      (caddr . #xa6)
      (cadddr . #xa7)
      (caddddr . #xa8)
      (cadddddr . #xa9)
      (caddddddr . #xaa)
      (cadddddddr . #xab)

      (floor . #xac)
      (ceiling . #xad)
      (truncate . #xae)
      (round . #xaf)

      (apply . #xb0)
      (forbid . #xb1)
      (permit . #xb2)

      (exp . #xb3)
      (log . #xb4)
      (sin . #xb5)
      (cos . #xb6)
      (tan . #xb7)
      (sqrt . #xb8)
      (expt . #xb9)

      (swap2 . #xba)
      (mod . #xbb)

      (make-closure . #xbc)
      (unbindall-0 . #xbd)
      (closurep . #xbe)
      (pop-all . #xbf)
      (fluid-set . #xc0)
      (fluid-bind . #xc1)
      (memql . #xc2)
      (num-eq . #xc3)

      (test-scm . #xc4)
      (test-scm-f . #xc5)
      (%define . #xc6)
      (spec-bind . #xc7)
      (set . #xc8)

      (required-arg . #xc9)
      (optional-arg . #xca)
      (rest-arg . #xcb)

      (not-zero-p . #xcc)

      (keyword-arg . #xcd)
      (optional-arg* . #xce)
      (keyword-arg* . #xcf)

      (last-before-jmps . #xf7)

;;; All jmps take two-byte arguments

      (ejmp . #xf8)			;if (pop[1]) goto error-handler,
					; else jmp x
      (jpn . #xf9)			;if stk[0] nil, pop and jmp x
      (jpt . #xfa)			;if stk[0] t, pop and jmp x
      (jmp . #xfb)			;jmp to x
      (jn . #xfc)			;pop the stack, if nil, jmp x
      (jt . #xfd)			;pop the stack, if t, jmp x
      (jnp . #xfe)			;if stk[0] nil, jmp x, else pop
      (jtp . #xff)))			;if stk[0] t, jmp x, else pop

  ;; maximum argument value in 1,2,3 byte instructions
  (defconst byte-max-1-byte-arg 5)
  (defconst byte-max-2-byte-arg #xff)
  (defconst byte-max-3-byte-arg #xffff)

  ;; maps from each instruction to the effect they have on the stack
  ;; pointer. i.e. +1 means the instruction always increases the net
  ;; stack position by one
  (define byte-insn-stack-delta
    [+1  nil nil nil nil nil nil nil	;#x00
     nil nil nil nil nil nil nil nil
     +1  nil nil nil nil nil nil nil	;#x10
     +1  nil nil nil nil nil nil nil
     -1  nil nil nil nil nil nil nil	;#x20
     -1  nil nil nil nil nil nil nil
     -1  nil nil nil nil nil nil nil	;#x30
     +1  nil nil nil nil nil nil nil
     0   -1  0   0   0   0   +1  0	;#x40
     -1  +1  +1  -1  0   0   -1  -1
     -1  -1  -1  -1  0   -1  -1  0	;#x50
     -1  -1  -1  -1  0   0   -1  -1
     -1  -1  -1  0   -1  -1  -1  -1	;#x60
     0   0   -1  0   0   0   0   0
     0   0   0   nil -1  -1  -1  0	;#x70
     0   0   -1  -2  -1  -1  -1  0
     0   -1  -1  -1  -1  0   -1  -1	;#x80
     -1  -1  -1  -1  -1  -1  0   0
     0   0   0   -1  -1  -1  -1  -1	;#x90
     0   0   +1  +1  +1  +1  +1  +1
     +1  +1  0   0   0   0   0   0	;#xa0
     0   0   0   0   0   0   0   0
     -1  0   0   0   0   0   0   0	;#xb0
     0   -1  0   -1  -1  0   0   nil
     -1  -2  -1  -1  0   0   -1  -2	;#xc0
     -1  +1  +1  +1  0   0   nil nil
     nil nil nil nil nil nil nil nil	;#xd0
     nil nil nil nil nil nil nil nil
     -1  nil nil nil nil nil nil nil	;#xe0
     -1  nil nil nil nil nil nil nil
     nil nil nil nil nil nil nil nil	;#xf0
     -1  nil nil 0   -1  -1  nil nil]))
