;;;; bytecodes.jl -- Bytecodes for lispmach virtual machine
;;;  Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
;;;  $Id$

;;; This file is part of Jade.

;;; Jade is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; Jade is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with Jade; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'bytecodes)

;;; Notes:
;;;
;;; Instruction Encoding
;;; ====================
;;; Instructions which get an argument (with opcodes of zero up to
;;; `op-last-with-args') encode the type of argument in the low 3 bits
;;; of their opcode (this is why these instructions take up 8 opcodes).
;;; A value of 0 to 5 (inclusive) is the literal argument, value of
;;; 6 means the next byte holds the argument, or a value of 7 says
;;; that the next two bytes are used to encode the argument (in big-
;;; endian form, i.e. first extra byte has the high 8 bits)
;;;
;;; All instructions greater than the `op-last-before-jmps' are branches,
;;; currently only absolute destinations are supported, all branch
;;; instructions encode their destination in the following two bytes (also
;;; in big-endian form).
;;;
;;; Any opcode between `op-last-with-args' and `op-last-before-jmps' is
;;; a straightforward single-byte instruction.
;;;
;;; The machine simulated by lispmach.c is a simple stack-machine, each
;;; call to the byte-code interpreter gets its own stack; the size of
;;; stack needed is calculated by the compiler.

;; Instruction set version
(defconst bytecode-major 2)
(defconst bytecode-minor 2)

;; Opcodes
(defconst op-call 0x08)			;call (stk[n] stk[n-1] ... stk[0])
					; pops n values, replacing the
					; function with the result.
(defconst op-push 0x10)			;pushes constant # n
(defconst op-refq 0x18)			;pushes val of symbol n (in c-v)
(defconst op-setq 0x20)			;sets symbol n (in c-v) to stk[0]
(defconst op-list 0x28)			;makes top n items into a list
(defconst op-bind 0x30)			;bind constant n to stk[0], pops stk

(defconst op-last-with-args 0x37)

(defconst op-ref 0x40)			;replace symbol with it's value
(defconst op-set 0x41)			;set (sym)stk[0]=stk[1], pops symbol
(defconst op-fref 0x42)			;similar to vref/vset, but for
(defconst op-fset 0x43)			; function value.
(defconst op-init-bind 0x44)		;initialise a new set of bindings
(defconst op-unbind 0x45)		;unbind all bindings in the top set
(defconst op-dup 0x46)			;duplicate top of stack
(defconst op-swap 0x47)			;swap top two values on stack
(defconst op-pop 0x48)			;pops the stack

(defconst op-nil 0x49)			;pushes nil
(defconst op-t 0x4a)			;pushes t
(defconst op-cons 0x4b)
(defconst op-car 0x4c)
(defconst op-cdr 0x4d)
(defconst op-rplaca 0x4e)
(defconst op-rplacd 0x4f)
(defconst op-nth 0x50)
(defconst op-nthcdr 0x51)
(defconst op-aset 0x52)
(defconst op-aref 0x53)
(defconst op-length 0x54)
(defconst op-eval 0x55)
(defconst op-add 0x56)			;adds the top two values
(defconst op-neg 0x57)
(defconst op-sub 0x58)
(defconst op-mul 0x59)
(defconst op-div 0x5a)
(defconst op-rem 0x5b)
(defconst op-lnot 0x5c)
(defconst op-not 0x5d)
(defconst op-lor 0x5e)
(defconst op-land 0x5f)
(defconst op-equal 0x60)
(defconst op-eq 0x61)
(defconst op-num-eq 0x62)
(defconst op-num-noteq 0x63)
(defconst op-gt 0x64)
(defconst op-ge 0x65)
(defconst op-lt 0x66)
(defconst op-le 0x67)
(defconst op-inc 0x68)
(defconst op-dec 0x69)
(defconst op-lsh 0x6a)
(defconst op-zerop 0x6b)
(defconst op-null 0x6c)
(defconst op-atom 0x6d)
(defconst op-consp 0x6e)
(defconst op-listp 0x6f)
(defconst op-numberp 0x70)
(defconst op-stringp 0x71)
(defconst op-vectorp 0x72)
(defconst op-catch-kludge 0x73)
(defconst op-throw 0x74)
(defconst op-unwind-pro 0x75)
;(defconst op-un-unwind-pro 0x76)
(defconst op-fboundp 0x77)
(defconst op-boundp 0x78)
(defconst op-symbolp 0x79)
(defconst op-get 0x7a)
(defconst op-put 0x7b)
(defconst op-error-pro 0x7c)
(defconst op-signal 0x7d)
(defconst op-return 0x7e)
(defconst op-reverse 0x7f)		;new 12/7/94
(defconst op-nreverse 0x80)
(defconst op-assoc 0x81)
(defconst op-assq 0x82)
(defconst op-rassoc 0x83)
(defconst op-rassq 0x84)
(defconst op-last 0x85)
(defconst op-mapcar 0x86)
(defconst op-mapc 0x87)
(defconst op-member 0x88)
(defconst op-memq 0x89)
(defconst op-delete 0x8a)
(defconst op-delq 0x8b)
(defconst op-delete-if 0x8c)
(defconst op-delete-if-not 0x8d)
(defconst op-copy-sequence 0x8e)
(defconst op-sequencep 0x8f)
(defconst op-functionp 0x90)
(defconst op-special-form-p 0x91)
(defconst op-subrp 0x92)
(defconst op-eql 0x93)
(defconst op-lxor 0x94)			;new 23-8-94

(defconst op-set-current-buffer 0xb0)
(defconst op-bind-buffer 0xb1)
(defconst op-current-buffer 0xb2)
(defconst op-bufferp 0xb3)
(defconst op-markp 0xb4)
(defconst op-windowp 0xb5)
(defconst op-bind-window 0xb6)

(defconst op-viewp 0xb7)		;new 26-8-97
(defconst op-bind-view 0xb8)
(defconst op-current-view 0xb9)
(defconst op-swap2 0xba)		;s[0]=s[1], s[1]=s[2], s[2]=s[0]

(defconst op-mod 0xbb)			;new 21-11-97

(defconst op-last-before-jmps 0xfa)

;; All jmps take two-byte arguments
(defconst op-jmp 0xfb)			;jmp to x
(defconst op-jn 0xfc)			;pop the stack, if nil, jmp x
(defconst op-jt 0xfd)			;pop the stack, if t, jmp x
(defconst op-jnp 0xfe)			;if stk[0] nil, jmp x, else pop
(defconst op-jtp 0xff)			;if stk[0] t, jmp x, else pop

(defconst comp-max-1-byte-arg 5)	;max arg held in 1-byte instruction
(defconst comp-max-2-byte-arg 0xff)	;max arg held in 2-byte instruction
(defconst comp-max-3-byte-arg 0xffff)	;max arg help in 3-byte instruction
