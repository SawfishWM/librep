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

(define-structure bytecode-defs (export bytecode-major
					bytecode-minor
					bytecode
					bytecode-alist
					byte-max-1-byte-arg
					byte-max-2-byte-arg
					byte-max-3-byte-arg
					byte-insn-stack-delta)
  (open rep)

  ;; Instruction set version
  (defconst bytecode-major 10)
  (defconst bytecode-minor 0)

  ;; macro to get a named bytecode
  (defmacro bytecode (name)
    (cdr (assq name bytecode-alist)))

  (define bytecode-alist
    '((call . 0x08)			;call (stk[n] stk[n-1] ... stk[0])
					; pops n values, replacing the
					; function with the result.
      (push . 0x10)			;pushes constant # n
      (refq . 0x18)			;pushes val of symbol n (in c-v)
      (setq . 0x20)			;sets sym n (in c-v) to stk[0]; pop
      (list . 0x28)			;makes top n items into a list
      (bind . 0x30)			;bind constant n to stk[0], pops stk
      (refn . 0x38)
      (setn . 0xe8)
      (refg . 0xe0)
      (setg . 0xd8)
      (bindspec . 0xd0)

      (last-with-args . 0x3f)

      (first-with-args-2 . 0xd0)
      (last-with-args-2 . 0xef)

      (ref . 0x40)			;replace symbol with it's value
      (set . 0x41)
      (dset . 0x42)
      (enclose . 0x43)
      (init-bind . 0x44)		;initialise a new set of bindings
      (unbind . 0x45)			;unbind all bindings in the top set
      (dup . 0x46)			;duplicate top of stack
      (swap . 0x47)			;swap top two values on stack
      (pop . 0x48)			;pops the stack

      (nil . 0x49)			;pushes nil
      (t . 0x4a)			;pushes t
      (cons . 0x4b)
      (car . 0x4c)
      (cdr . 0x4d)
      (rplaca . 0x4e)
      (rplacd . 0x4f)
      (nth . 0x50)
      (nthcdr . 0x51)
      (aset . 0x52)
      (aref . 0x53)
      (length . 0x54)
      (eval . 0x55)
      (add . 0x56)			;adds the top two values
      (neg . 0x57)
      (sub . 0x58)
      (mul . 0x59)
      (div . 0x5a)
      (rem . 0x5b)
      (lnot . 0x5c)
      (not . 0x5d)
      (lor . 0x5e)
      (land . 0x5f)
      (equal . 0x60)
      (eq . 0x61)
      (structure-ref . 0x62)
      (scm-test . 0x63)
      (gt . 0x64)
      (ge . 0x65)
      (lt . 0x66)
      (le . 0x67)
      (inc . 0x68)
      (dec . 0x69)
      (ash . 0x6a)
      (zerop . 0x6b)
      (null . 0x6c)
      (atom . 0x6d)
      (consp . 0x6e)
      (listp . 0x6f)
      (numberp . 0x70)
      (stringp . 0x71)
      (vectorp . 0x72)
      (catch . 0x73)
      (throw . 0x74)
      (binderr . 0x75)
      (return . 0x76)
      (unbindall . 0x77)
      (boundp . 0x78)
      (symbolp . 0x79)
      (get . 0x7a)
      (put . 0x7b)
      (errorpro . 0x7c)
      (signal . 0x7d)
      (quotient . 0x7e)
      (reverse . 0x7f)
      (nreverse . 0x80)
      (assoc . 0x81)
      (assq . 0x82)
      (rassoc . 0x83)
      (rassq . 0x84)
      (last . 0x85)
      (mapcar . 0x86)
      (mapc . 0x87)
      (member . 0x88)
      (memq . 0x89)
      (delete . 0x8a)
      (delq . 0x8b)
      (delete-if . 0x8c)
      (delete-if-not . 0x8d)
      (copy-sequence . 0x8e)
      (sequencep . 0x8f)
      (functionp . 0x90)
      (special-form-p . 0x91)
      (subrp . 0x92)
      (eql . 0x93)
      (lxor . 0x94)
      (max . 0x95)
      (min . 0x96)
      (filter . 0x97)
      (macrop . 0x98)
      (bytecodep . 0x99)

      (pushi-0 . 0x9a)
      (pushi-1 . 0x9b)
      (pushi-2 . 0x9c)
      (pushi-minus-1 . 0x9d)
      (pushi-minus-2 . 0x9e)
      (pushi . 0x9f)
      (pushi-pair-neg . 0xa0)
      (pushi-pair-pos . 0xa1)

      (caar . 0xa2)
      (cadr . 0xa3)
      (cdar . 0xa4)
      (cddr . 0xa5)

      (caddr . 0xa6)
      (cadddr . 0xa7)
      (caddddr . 0xa8)
      (cadddddr . 0xa9)
      (caddddddr . 0xaa)
      (cadddddddr . 0xab)

      (floor . 0xac)
      (ceiling . 0xad)
      (truncate . 0xae)
      (round . 0xaf)

      (bindobj . 0xb0)
      (forbid . 0xb1)
      (permit . 0xb2)

      (exp . 0xb3)
      (log . 0xb4)
      (sin . 0xb5)
      (cos . 0xb6)
      (tan . 0xb7)
      (sqrt . 0xb8)
      (expt . 0xb9)

      (swap2 . 0xba)
      (mod . 0xbb)

      (make-closure . 0xbc)
      (unbindall-0 . 0xbd)
      (closurep . 0xbe)
      (bindenv . 0xbf)
      (pop-all . 0xc0)

      (last-before-jmps . 0xf7)

;;; All jmps take two-byte arguments

      (ejmp . 0xf8)			;if (pop[1]) goto error-handler,
					; else jmp x
      (jpn . 0xf9)			;if stk[0] nil, pop and jmp x
      (jpt . 0xfa)			;if stk[0] t, pop and jmp x
      (jmp . 0xfb)			;jmp to x
      (jn . 0xfc)			;pop the stack, if nil, jmp x
      (jt . 0xfd)			;pop the stack, if t, jmp x
      (jnp . 0xfe)			;if stk[0] nil, jmp x, else pop
      (jtp . 0xff)))			;if stk[0] t, jmp x, else pop

  ;; maximum argument value in 1,2,3 byte instructions
  (defconst byte-max-1-byte-arg 5)
  (defconst byte-max-2-byte-arg 0xff)
  (defconst byte-max-3-byte-arg 0xffff)

  ;; maps from each instruction to the effect they have on the stack
  ;; pointer. i.e. +1 means the instruction always increases the net
  ;; stack position by one
  (define byte-insn-stack-delta
    [nil nil nil nil nil nil nil nil	;0x00
     nil nil nil nil nil nil nil nil
     +1  nil nil nil nil nil nil nil	;0x10
     +1  nil nil nil nil nil nil nil
     -1  nil nil nil nil nil nil nil	;0x20
     nil nil nil nil nil nil nil nil
     -1  nil nil nil nil nil nil nil	;0x30
     +1  nil nil nil nil nil nil nil
     0   -1  -1  0   0   0   +1  0	;0x40
     -1  +1  +1  -1  0   0   -1  -1
     -1  -1  -1  -1  0   0   -1  0	;0x50
     -1  -1  -1  -1  0   0   -1  -1
     -1  -1  -1  0   -1  -1  -1  -1	;0x60
     0   0   -1  0   0   0   0   0
     0   0   0   nil -1  -1  -1  0	;0x70
     0   0   -1  -2  -1  -1  -1  0
     0   -1  -1  -1  -1  0   -1  -1	;0x80
     -1  -1  -1  -1  -1  -1  0   0
     0   0   0   -1  -1  -1  -1  -1	;0x90
     0   0   +1  +1  +1  +1  +1  +1
     +1  +1  0   0   0   0   0   0	;0xa0
     0   0   0   0   0   0   0   0
     -1  0   0   0   0   0   0   0	;0xb0
     0   -1  0   -1  -1  0   0   0
     nil nil nil nil nil nil nil nil	;0xc0
     nil nil nil nil nil nil nil nil
     -1  nil nil nil nil nil nil nil	;0xd0
     -1  nil nil nil nil nil nil nil
     +1  nil nil nil nil nil nil nil	;0xe0
     -1  nil nil nil nil nil nil nil
     nil nil nil nil nil nil nil nil	;0xf0
     -1  nil nil 0   -1  -1  nil nil]))
