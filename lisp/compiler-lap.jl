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

(define-structure compiler-lap (export comp-write-op
				       comp-make-label
				       comp-push-label-addr
				       comp-compile-jmp
				       comp-set-label
				       comp-start-label)
  (open rep
	compiler-utils
	compiler-vars
	bytecodes)

  ;; Output one opcode and its optional argument
  (defmacro comp-write-op (opcode &optional arg)
    `(setq comp-intermediate-code (cons (cons ,opcode ,arg)
					comp-intermediate-code)))

  ;; Create a new label
  (defmacro comp-make-label ()
    ;; a label is either (label . nil) or (label . (CODE-REFS...))
    ;; or (label BYTE-ADDRESS)
    `(cons 'label nil))

  ;; Arrange for the address of LABEL to be pushed onto the stack
  (defmacro comp-push-label-addr (label)
    `(progn
       (comp-write-op (bytecode pushi-pair-pos) ,label)
       (comp-inc-stack)))

  (defun comp-compile-jmp (opcode label)
    (comp-write-op opcode label))

  ;; Set the address of the label LABEL to the current pc
  (defmacro comp-set-label (label)
    `(setq comp-intermediate-code (cons ,label comp-intermediate-code)))

  ;; return the label marking the start of the bytecode sequence
  (defun comp-start-label ()
    (let
	((label (last comp-intermediate-code)))
      (unless (eq (car label) 'label)
      (setq label (comp-make-label))
	(setq comp-intermediate-code (nconc comp-intermediate-code
					    (list label))))
      label)))
