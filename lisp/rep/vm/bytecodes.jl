#| bytecodes.jl -- Bytecodes for rep virtual machine

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

(define-structure rep.vm.bytecodes

    (export bytecode-major bytecode-minor bytecode bytecode-ref
	    byte-max-1-byte-arg byte-max-2-byte-arg byte-max-3-byte-arg
	    byte-two-byte-insns byte-three-byte-insns
	    byte-insn-stack-delta byte-constant-insns
	    byte-varref-free-insns byte-side-effect-free-insns
	    byte-conditional-jmp-insns byte-jmp-insns
	    byte-insns-with-constants byte-varref-insns
	    byte-varset-insns byte-varbind-insns
	    byte-nth-insns byte-nthcdr-insns)

    (open rep rep.vm.bytecode-defs)

;;; Description of instruction set for when optimising

  ;; list of instructions that always have a 1-byte argument following them
  (define byte-two-byte-insns (list (bytecode pushi)))

  ;; list of instructions that always have a 2-byte argument following them
  (define byte-three-byte-insns
    (list (bytecode pushi-pair-neg)
	  (bytecode pushi-pair-pos)
	  (bytecode ejmp)
	  (bytecode jpn)
	  (bytecode jpt)
	  (bytecode jmp)
	  (bytecode jn)
	  (bytecode jt)
	  (bytecode jnp)
	  (bytecode jtp)))

  ;; list of instructions pushing a single constant onto the stack
  (define byte-constant-insns
    (list (bytecode push)
	  (bytecode nil)
	  (bytecode t)
	  (bytecode pushi-0)
	  (bytecode pushi-1)
	  (bytecode pushi-2)
	  (bytecode pushi-minus-1)
	  (bytecode pushi-minus-2)
	  (bytecode pushi)
	  (bytecode pushi-pair-neg)
	  (bytecode pushi-pair-pos)))

  ;; list of instructions that are both side-effect free and don't
  ;; reference any variables. Also none of these may ever raise exceptions
  (define byte-varref-free-insns
    (list* (bytecode dup)
	   (bytecode cons)
	   (bytecode car)
	   (bytecode cdr)
	   (bytecode eq)
	   (bytecode equal)
	   (bytecode zerop)
	   (bytecode null)
	   (bytecode atom)
	   (bytecode consp)
	   (bytecode listp)
	   (bytecode numberp)
	   (bytecode stringp)
	   (bytecode vectorp)
	   (bytecode symbolp)
	   (bytecode sequencep)
	   (bytecode functionp)
	   (bytecode special-form-p)
	   (bytecode subrp)
	   (bytecode eql)
	   (bytecode macrop)
	   (bytecode bytecodep)
	   (bytecode caar)
	   (bytecode cadr)
	   (bytecode cdar)
	   (bytecode cadddr)
	   (bytecode caddddr)
	   (bytecode cadddddr)
	   (bytecode caddddddr)
	   (bytecode cadddddddr)
	   (bytecode scm-test)
	   (bytecode test-scm)
	   (bytecode test-scm-f)
	   byte-constant-insns))

  ;; list of instructions that can be safely deleted if their result
  ;; isn't actually required
  (define byte-side-effect-free-insns
    (list* (bytecode refq)
	   (bytecode refn)
	   (bytecode slot-ref)
	   (bytecode refg)
	   (bytecode ref)
	   (bytecode nth)
	   (bytecode nthcdr)
	   (bytecode aref)
	   (bytecode length)
	   (bytecode add)
	   (bytecode neg)
	   (bytecode sub)
	   (bytecode mul)
	   (bytecode div)
	   (bytecode rem)
	   (bytecode lnot)
	   (bytecode not)
	   (bytecode lor)
	   (bytecode land)
	   (bytecode gt)
	   (bytecode ge)
	   (bytecode lt)
	   (bytecode le)
	   (bytecode inc)
	   (bytecode dec)
	   (bytecode ash)
	   (bytecode boundp)
	   (bytecode get)
	   (bytecode reverse)
	   (bytecode assoc)
	   (bytecode assq)
	   (bytecode rassoc)
	   (bytecode rassq)
	   (bytecode last)
	   (bytecode copy-sequence)
	   (bytecode lxor)
	   (bytecode max)
	   (bytecode min)
	   (bytecode mod)
	   (bytecode make-closure)
	   (bytecode enclose)
	   (bytecode quotient)
	   (bytecode floor)
	   (bytecode ceiling)
	   (bytecode truncate)
	   (bytecode round)
	   (bytecode exp)
	   (bytecode log)
	   (bytecode sin)
	   (bytecode cos)
	   (bytecode tan)
	   (bytecode sqrt)
	   (bytecode expt)
	   (bytecode structure-ref)
	   byte-varref-free-insns))

  ;; list of all conditional jumps
  (define byte-conditional-jmp-insns
    (list (bytecode jpn)
	  (bytecode jpt)
	  (bytecode jn)
	  (bytecode jt)
	  (bytecode jnp)
	  (bytecode jtp)))

  ;; list of all jump instructions
  (define byte-jmp-insns
    (list* (bytecode jmp)
	   byte-conditional-jmp-insns))

  ;; list of instructions that reference the vector of constants
  (define byte-insns-with-constants
    (list (bytecode push)
	  (bytecode refq)
	  (bytecode setq)
	  (bytecode refg)
	  (bytecode setg)
	  (bytecode bindspec)))

  ;; list of all varref instructions
  (define byte-varref-insns
    (list (bytecode refq)
	  (bytecode refn)
	  (bytecode refg)
	  (bytecode slot-ref)))

  ;; list of all varset instructions
  (define byte-varset-insns
    (list (bytecode setq)
	  (bytecode setn)
	  (bytecode setg)
	  (bytecode slot-set)))

  ;; list of all varbind instructions
  (define byte-varbind-insns
    (list (bytecode bind)
	  (bytecode bindspec)))
      
  (define byte-nth-insns (list (cons 0 (bytecode car))
			       (cons 1 (bytecode cadr))
			       (cons 2 (bytecode caddr))
			       (cons 3 (bytecode cadddr))
			       (cons 4 (bytecode caddddr))
			       (cons 5 (bytecode cadddddr))
			       (cons 6 (bytecode caddddddr))
			       (cons 7 (bytecode cadddddddr))))

  (define byte-nthcdr-insns (list (cons 0 nil)
				  (cons 1 (bytecode cdr))
				  (cons 2 (bytecode cddr)))))
