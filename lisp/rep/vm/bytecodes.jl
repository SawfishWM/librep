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
	    byte-opcodes-with-constants byte-varref-insns
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

  ;; list of instructions that are both side-effect free and don't
  ;; reference any variables. Also none of these may ever raise exceptions
  (define byte-varref-free-insns
    '(dup push cons car cdr eq equal zerop not-zero-p null atom consp
      listp numberp stringp vectorp symbolp sequencep functionp
      special-form-p subrp eql macrop bytecodep caar cadr cdar
      cadddr caddddr cadddddr caddddddr cadddddddr scm-test
      test-scm test-scm-f))


  ;; list of instructions that can be safely deleted if their result
  ;; isn't actually required
  (define byte-side-effect-free-insns
    (append '(refn refg slot-ref ref nth nthcdr aref length add neg
	      sub mul div rem lnot not lor land gt ge lt le inc dec ash
	      boundp get reverse assoc assq rassoc rassq last copy-sequence
	      lxor max min mod make-closure enclose quotient floor ceiling
	      truncate round exp log sin cos tan sqrt expt structure-ref)
           byte-varref-free-insns))

  ;; list of all conditional jumps
  (define byte-conditional-jmp-insns '(jpn jpt jn jt jnp jtp))

  ;; list of all jump instructions
  (define byte-jmp-insns (list* 'jmp 'ejmp byte-conditional-jmp-insns))

  ;; list of all varref instructions
  (define byte-varref-insns '(refn refg slot-ref))

  ;; list of all varset instructions
  (define byte-varset-insns '(setn setg slot-set))

  ;; list of all varbind instructions
  (define byte-varbind-insns '(bind))

  (define byte-nth-insns '((0 . car)
                          (1 . cadr)
                          (2 . caddr)
                          (3 . cadddr)
                          (4 . caddddr)
                          (5 . cadddddr)
                          (6 . caddddddr)
                          (7 . cadddddddr)))

  (define byte-nthcdr-insns '((0 . ())
                             (1 . cdr)
                             (2 . cddr)))

  ;; list of instructions that reference the vector of constants
  (define byte-opcodes-with-constants
    (list (bytecode push)
	  (bytecode refq)
	  (bytecode setq)
	  (bytecode refg)
	  (bytecode setg)
	  (bytecode bindspec))))
