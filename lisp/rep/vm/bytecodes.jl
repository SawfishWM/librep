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
(defconst bytecode-major 7)
(defconst bytecode-minor 0)

;; Opcodes
(defconst op-call 0x08)			;call (stk[n] stk[n-1] ... stk[0])
					; pops n values, replacing the
					; function with the result.
(defconst op-push 0x10)			;pushes constant # n
(defconst op-refq 0x18)			;pushes val of symbol n (in c-v)
(defconst op-setq 0x20)			;sets sym n (in c-v) to stk[0]; pop
(defconst op-list 0x28)			;makes top n items into a list
(defconst op-bind 0x30)			;bind constant n to stk[0], pops stk

(defconst op-last-with-args 0x37)

(defconst op-ref 0x40)			;replace symbol with it's value
(defconst op-set 0x41)
(defconst op-fref 0x42)			;similar to ref for function slot
(defconst op-fset 0x43)
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
(defconst op-catch 0x73)
(defconst op-throw 0x74)
(defconst op-binderr 0x75)
(defconst op-unused1 0x76)
(defconst op-fboundp 0x77)
(defconst op-boundp 0x78)
(defconst op-symbolp 0x79)
(defconst op-get 0x7a)
(defconst op-put 0x7b)
(defconst op-errorpro 0x7c)
(defconst op-signal 0x7d)
(defconst op-unused2 0x7e)
(defconst op-reverse 0x7f)
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
(defconst op-lxor 0x94)
(defconst op-max 0x95)
(defconst op-min 0x96)
(defconst op-filter 0x97)
(defconst op-macrop 0x98)
(defconst op-bytecodep 0x99)

(defconst op-pushi-0 0x9a)
(defconst op-pushi-1 0x9b)
(defconst op-pushi-2 0x9c)
(defconst op-pushi-minus-1 0x9d)
(defconst op-pushi-minus-2 0x9e)
(defconst op-pushi 0x9f)
(defconst op-pushi-pair-neg 0xa0)
(defconst op-pushi-pair-pos 0xa1)

(defconst op-bindobj 0xb0)
(defconst op-swap2 0xba)
(defconst op-mod 0xbb)

(defconst op-make-closure 0xbc)
(defconst op-fbind 0xbd)
(defconst op-closurep 0xbe)
(defconst op-bindenv 0xbf)

(defconst op-last-before-jmps 0xf7)

;; All jmps take two-byte arguments
(defconst op-ejmp 0xf8)			;if (pop[1]) goto error-handler,
					; else jmp x
(defconst op-jpn 0xf9)			;if stk[0] nil, pop and jmp x
(defconst op-jpt 0xfa)			;if stk[0] t, pop and jmp x
(defconst op-jmp 0xfb)			;jmp to x
(defconst op-jn 0xfc)			;pop the stack, if nil, jmp x
(defconst op-jt 0xfd)			;pop the stack, if t, jmp x
(defconst op-jnp 0xfe)			;if stk[0] nil, jmp x, else pop
(defconst op-jtp 0xff)			;if stk[0] t, jmp x, else pop

(defconst comp-max-1-byte-arg 5)	;max arg held in 1-byte instruction
(defconst comp-max-2-byte-arg 0xff)	;max arg held in 2-byte instruction
(defconst comp-max-3-byte-arg 0xffff)	;max arg help in 3-byte instruction


;; Description of instruction set for when optimising

;; list of instructions that always have a 1-byte argument following them
(defvar comp-two-byte-insns (list op-pushi))

;; list of instructions that always have a 2-byte argument following them
(defvar comp-three-byte-insns (list op-pushi-pair-neg op-pushi-pair-pos
				    op-ejmp op-jpn op-jpt op-jmp op-jn op-jt
				    op-jnp op-jtp))

;; maps from each instruction to the effect they have on the stack pointer.
;; i.e. +1 means the instruction always increases the net stack position
;; by one
(defvar comp-insn-stack-delta
  [nil nil nil nil nil nil nil nil	;0x00
   nil nil nil nil nil nil nil nil
   +1  nil nil nil nil nil nil nil	;0x10
   +1  nil nil nil nil nil nil nil
   -1   nil nil nil nil nil nil nil	;0x20
   nil nil nil nil nil nil nil nil
   -1  nil nil nil nil nil nil nil	;0x30
   nil nil nil nil nil nil nil nil
   0   -1  0   -1  0   0   +1  0	;0x40
   -1  +1  +1  -1  0   0   -1  -1
   -1  -1  -1  -1  0   0   -1  0	;0x50
   -1  -1  -1  -1  0   0   -1  -1
   -1  -1  -1  -1  -1  -1  -1  -1	;0x60
   0   0   -1  0   0   0   0   0
   0   0   0   nil -1  -1  nil 0	;0x70
   0   0   -1  -2  -1  -1  nil 0
   0   -1  -1  -1  -1  0   -1  -1	;0x80
   -1  -1  -1  -1  -1  -1  0   0
   0   0   0   -1  -1  -1  -1  -1	;0x90
   0   0   +1  +1  +1  +1  +1  +1
   +1  +1 nil nil nil nil nil nil	;0xa0
   nil nil nil nil nil nil nil nil
   -1  nil nil nil nil nil nil nil	;0xb0
   nil nil  0  -1   0  -2   0   0
   nil nil nil nil nil nil nil nil	;0xc0
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil	;0xd0
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil	;0xe0
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil	;0xf0
   -1  nil nil 0   -1  -1  nil nil])

;; list of instructions pushing a single constant onto the stack
(defvar comp-constant-insns
  (list op-push op-nil op-t op-pushi-0 op-pushi-1 op-pushi-2
	op-pushi-minus-1 op-pushi-minus-2 op-pushi
	op-pushi-pair-neg op-pushi-pair-pos))

;; list of instructions that are both side-effect free and don't reference
;; any variables. Also none of these may ever raise exceptions
(defvar comp-varref-free-insns
  (list* op-dup op-cons op-car op-cdr op-eq op-equal op-zerop op-null
	 op-atom op-consp op-listp op-numberp op-stringp op-vectorp
	 op-symbolp op-sequencep op-functionp op-special-form-p
	 op-subrp op-eql op-macrop op-bytecodep
	 comp-constant-insns))

;; list of instructions that can be safely deleted if their result
;; isn't actually required
(defvar comp-side-effect-free-insns
  (list* op-refq op-ref op-fref op-nth op-nthcdr op-aref op-length op-add
	 op-neg op-sub op-mul op-div op-rem op-lnot op-not op-lor
	 op-land op-num-eq op-num-noteq op-gt op-ge op-lt op-le op-inc
	 op-dec op-lsh op-fboundp op-boundp op-get op-reverse op-assoc
	 op-assq op-rassoc op-rassq op-last op-copy-sequence op-lxor
	 op-max op-min op-mod op-make-closure
	 comp-varref-free-insns))

;; list of all conditional jumps
(defvar comp-conditional-jmp-insns
  (list op-jpn op-jpt op-jn op-jt op-jnp op-jtp))

;; list of all jump instructions
(defvar comp-jmp-insns (cons op-jmp comp-conditional-jmp-insns))

;; list of instructions that reference the vector of constants
(defvar comp-insns-with-constants (list op-push op-refq op-setq op-bind))
