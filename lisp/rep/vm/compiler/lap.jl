#| compiler-lap.jl -- intermediate code management

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

(define-structure compiler-lap (export intermediate-code
				       emit-insn
				       make-label
				       push-label-addr
				       emit-jmp-insn
				       fix-label
				       get-start-label)
  (open rep
	compiler-utils
	bytecodes)

  ;; list of (INSN . [ARG]), (TAG . REFS)
  (define intermediate-code (make-fluid '()))

  ;; Output one opcode and its optional argument
  (defmacro emit-insn (opcode &optional arg)
    `(fluid-set intermediate-code (cons (cons ,opcode ,arg)
					(fluid intermediate-code))))

  ;; Create a new label
  (defmacro make-label ()
    ;; a label is either (label . nil) or (label . (CODE-REFS...))
    ;; or (label BYTE-ADDRESS)
    `(cons 'label nil))

  ;; Arrange for the address of LABEL to be pushed onto the stack
  (defmacro push-label-addr (label)
    `(progn
       (emit-insn (bytecode pushi-pair-pos) ,label)
       (increment-stack)))

  (defun emit-jmp-insn (opcode label)
    (emit-insn opcode label))

  ;; Set the address of the label LABEL to the current pc
  (defmacro fix-label (label)
    `(fluid-set intermediate-code (cons ,label (fluid intermediate-code))))

  ;; return the label marking the start of the bytecode sequence
  (defun get-start-label ()
    (let
	((label (last (fluid intermediate-code))))
      (unless (eq (car label) 'label)
      (setq label (make-label))
	(fluid-set intermediate-code (nconc (fluid intermediate-code)
					    (list label))))
      label)))
