#| assembler.jl -- higher-level assembler

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

;; The plan is to use in the compiler at some point to remove the ugly
;; lap code representation, instead compile to assembly language, then
;; assemble that.. (with the peephole pass inbetween)

(define-structure rep.vm.assembler

    (export assemble)

    (open rep
	  rep.vm.bytecodes
	  rep.data.tables
	  rep.data.records)

  (define-record-type :label
    (make-label name)
    labelp
    (name label-name label-name-set)
    (address label-address label-address-set)
    (forwards label-forwards label-forwards-set))

  ;; Syntax of INSNS is a list of `(INSN [ARG])' or `LABEL'. One pseudo
  ;; insn: `(push-label LABEL)'

  ;; Example:

  ;; ((push 0)
  ;;  foo
  ;;  (push 1)
  ;;  (add)
  ;;  (jmp foo))

  ;; Returns (BYTE-CODE-VECTOR . CONSTANT-VECTOR)

  (define (assemble insns #!optional start)
    (let ((code '())
	  (pc (or start 0))
	  (labels (make-table symbol-hash eq))
	  (constants '())
	  (next-const-id 0))

      (define (get-label name)
	(or (table-ref labels name)
	    (let ((l (make-label name)))
	      (table-set labels name l)
	      l)))

      (define (get-const-id value)
	(or (cdr (assoc value constants))
	    (prog1 next-const-id
	      (setq constants (cons (cons value next-const-id) constants))
	      (setq next-const-id (1+ next-const-id)))))

      (define (emit-byte-at byte addr)
	(setq code (cons (cons byte addr) code)))

      (define (emit-byte byte)
	(emit-byte-at byte pc)
	(setq pc (1+ pc)))

      (define (emit-address-at addr pc)
	(emit-byte-at (ash addr -8) pc)
	(emit-byte-at (logand addr 255) (1+ pc)))

      (define (emit-address addr)
	(emit-address-at addr pc)
	(setq pc (+ pc 2)))

      (define (emit-label-addr label)
	(if (label-address label)
	    (emit-address (label-address label))
	  (label-forwards-set label (cons pc (label-forwards label)))
	  (setq pc (+ pc 2))))

      (define (emit-insn insn #!optional arg)
	(let ((op (bytecode-ref insn)))
	  (if (>= op (bytecode last-with-args))
	      (progn
		;; ``normal'' one-byte insn encoding
		(emit-byte op)
		(when arg
		  (cond ((memq op byte-two-byte-insns)
			 (if (< arg 256)
			     (emit-byte arg)
			   (error
			    "Argument overflow in two-byte insn: %s" insn)))

			((memq op byte-three-byte-insns)
			 (if (< arg 65536)
			     (progn
			       (emit-byte (ash arg -8))
			       (emit-byte (logand arg 255)))
			   (error
			    "Argument overflow in three-byte insn: %s" insn)))

			(t (error "Spurious argument to insn: %s" insn)))))

	    ;; insn with embedded argument
	    (cond ((<= arg byte-max-1-byte-arg)
		   (emit-byte (+ op arg)))

		  ((<= arg byte-max-2-byte-arg)
		   (emit-byte (+ op 6))
		   (emit-byte arg))

		  ((<= arg byte-max-3-byte-arg)
		   (emit-byte (+ op 7))
		   (emit-byte (ash arg -8))
		   (emit-byte (logand arg 255)))

		  (t (error "Argument overflow in insn: %s" insn))))))

      (define (emit-jmp insn dest)
	(emit-byte (bytecode-ref insn))
	(emit-label-addr (get-label dest)))

      (define (emit-push arg)
	(cond ((and (fixnump arg) (<= arg 65535) (>= arg -65535))
	       (cond ((zerop arg)
		      (emit-insn 'pushi-0))

		     ((= arg 1)
		      (emit-insn 'pushi-1))

		     ((= arg 2)
		      (emit-insn 'pushi-2))

		     ((= arg -1)
		      (emit-insn 'pushi-minus-1))

		     ((= arg -2)
		      (emit-insn 'pushi-minus-2))

		     ((and (<= arg 127) (>= arg -128))
		      (emit-insn 'pushi (logand arg 255)))

		     ((and (< arg 0) (>= arg -65535))
		      (emit-insn 'pushi-pair-neg (- arg)))

		     (t (emit-insn 'pushi-pair-pos arg))))

	      ((eq arg '()) (emit-insn 'nil))
	      ((eq arg 't) (emit-insn 't))

	      (t (emit-insn 'push (get-const-id arg)))))

      (define (emit-push-label arg)
	;; push address of label
	(emit-byte (bytecode pushi-pair-pos))
	(emit-label-addr (get-label arg)))
	      
      (define (emit-label name)
	(let ((label (get-label name)))
	  (and (label-address label)
	       (error "Multiply-defined label: %s, %s" name insns))
	  (label-address-set label pc)
	  ;; backpatch forward references
	  (do ((refs (label-forwards label) (cdr refs)))
	      ((null refs) (label-forwards-set label '()))
	    (emit-byte-at (ash pc -8) (car refs))
	    (emit-byte-at (logand pc 255) (1+ (car refs))))))
			  
      (let loop ((rest insns))
	(when rest
	  (let ((insn (car rest)))
	    (cond ((symbolp insn) (emit-label insn))

		  ((eq (car insn) 'push) (emit-push (cadr insn)))

		  ((eq (car insn) 'push-label) (emit-push-label (cadr insn)))

		  ((memq (car insn) '(refg setg))
		   ;; instruction with constant
		   (emit-insn (car insn) (get-const-id (cadr insn))))

		  ((memq (car insn) byte-jmp-insns)
		   (emit-jmp (car insn) (cadr insn)))

		  (t (apply emit-insn insn)))
	    (loop (cdr rest)))))

      (let ((byte-vec (make-string pc))
	    (const-vec (make-vector next-const-id)))
	(do ((rest code (cdr rest)))
	    ((null rest))
	  (aset byte-vec (cdar rest) (caar rest)))
	(do ((rest constants (cdr rest)))
	    ((null rest))
	  (aset const-vec (cdar rest) (caar rest)))

	(cons byte-vec const-vec)))))
