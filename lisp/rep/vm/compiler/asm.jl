#| asm.jl -- assemble intermediate form to bytecodes

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

(define-structure rep.vm.compiler.asm

    (export assemble-bytecodes)

    (open rep
	  rep.vm.compiler.utils
	  rep.vm.bytecodes)

  (define (assemble-bytecodes lap-code)
    (let
	((output nil)
	 (output-pc 0))

      ;; Output one byte
      (define (byte-out byte)
	(setq output (cons (cons byte output-pc) output))
	(setq output-pc (1+ output-pc)))

      (define (insn-out insn)
	(let
	    ((opcode (car insn))
	     (arg (cdr insn)))
	  (cond
	   ((eq opcode 'label)
	    ;; backpatch already output instructions referrring to this label
	    (mapc (lambda (addr)
		    (setq output (cons (cons (ash output-pc -8) addr)
				       output))
		    (setq output (cons (cons (logand output-pc 255)
					     (1+ addr)) output)))
		  arg)
	    ;; set the address of the label
	    (rplacd insn output-pc))

	   ((and (>= opcode (bytecode last-with-args))
		 (not (and (>= opcode (bytecode first-with-args-2))
			   (< opcode (bytecode last-with-args-2)))))
	    ;; ``normal'' one-byte insn encoding
	    (byte-out opcode)
	    (when arg
	      (when (and (eq (car arg) 'label) (numberp (cdr arg)))
		;; label whose address is already known
		(setq arg (cdr arg)))
	      (cond ((eq (car arg) 'label)
		     ;; label whose address isn't yet known
		     ;; add the address for backpatching
		     (rplacd arg (cons output-pc (cdr arg)))
		     ;; step over waiting slot
		     (setq output-pc (+ output-pc 2)))

		    ((memq opcode byte-two-byte-insns)
		     (if (< arg 256)
			 (byte-out arg)
		       (compiler-error
			"Argument overflow in two-byte insn: %d" opcode)))

		    ((memq opcode byte-three-byte-insns)
		     (if (< arg 65536)
			 (progn
			   (byte-out (ash arg -8))
			   (byte-out (logand arg 255)))
		       (compiler-error
			"Argument overflow in three-byte insn: %d" opcode)))

		    (t (compiler-error
			"Spurious argument given to insn: %d" opcode)))))

	   (t					; insn with encoded argument
	    (cond ((<= arg byte-max-1-byte-arg)
		   (byte-out (+ opcode arg)))
		  ((<= arg byte-max-2-byte-arg)
		   (byte-out (+ opcode 6))
		   (byte-out arg))
		  ((<= arg byte-max-3-byte-arg)
		   (byte-out (+ opcode 7))
		   (byte-out (ash arg -8))
		   (byte-out (logand arg 255)))
		  (t
		   (compiler-error
		    "Argument overflow in insn: %d" opcode)))))))

      ;; assemble to alist of bytes
      (mapc insn-out lap-code)

      ;; then turn the alist into a string
      (let
	  ((code-string (make-string output-pc ?*)))
	(mapc (lambda (cell)
		(aset code-string (cdr cell) (car cell))) output)

	code-string))))
