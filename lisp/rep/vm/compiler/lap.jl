#| lap.jl -- intermediate code management

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

(declare (unsafe-for-call/cc))

(define-structure rep.vm.compiler.lap

    (export intermediate-code
	    emit-insn
	    make-label
	    push-label-addr
	    fix-label
	    prefix-label
	    push-state
	    pop-state
	    reload-state
	    saved-state)

    (open rep
	  rep.vm.compiler.utils
	  rep.vm.compiler.bindings)

  (define saved-state (make-fluid))

  ;; list of (INSN . [ARG]), (TAG . REFS)
  (define intermediate-code (make-fluid '()))

  ;; Output one opcode and its optional argument
  (define (emit-insn insn)
    (when (consp insn)
      ;; so the peepholer can safely modify code
      (setq insn (copy-sequence insn)))
    (fluid-set intermediate-code (cons insn (fluid intermediate-code))))

  ;; Create a new label
  (define make-label gensym)

  ;; Arrange for the address of LABEL to be pushed onto the stack
  (define (push-label-addr label)
    (emit-insn `(push-label ,label))
    (increment-stack))

  ;; Set the address of the label LABEL to the current pc
  (define fix-label emit-insn)

  (define (prefix-label label)
    (fluid-set intermediate-code (nconc (list label)
					(fluid intermediate-code))))

  (define (push-state)
    (fluid-set saved-state
	       (cons (list (cons intermediate-code (fluid intermediate-code))
			   (cons spec-bindings (fluid spec-bindings))
			   (cons lex-bindings
				 (mapcar (lambda (x)
					   (copy-sequence x))
					 (fluid lex-bindings)))
			   (cons lexically-pure (fluid lexically-pure))
			   (cons current-stack (fluid current-stack))
			   (cons max-stack (fluid max-stack))
			   (cons current-b-stack (fluid current-b-stack))
			   (cons max-b-stack (fluid max-b-stack)))
		     (fluid saved-state))))

  (define (pop-state)
    (fluid-set saved-state (cdr (fluid saved-state))))

  ;; reload lex-bindings value, preserving eq-ness of cells
  (define (reload-lex-bindings saved)
    (let loop ((rest (fluid lex-bindings)))
      (if (eq (caar rest) (caar saved))
	  (progn
	    (fluid-set lex-bindings rest)
	    (do ((old rest (cdr old))
		 (new saved (cdr new)))
		((null old))
	      (rplacd (car old) (cdr (car new)))))
	(loop (cdr rest)))))

  (define (reload-state)
    (mapc (lambda (cell)
	    (if (eq (car cell) lex-bindings)
		(reload-lex-bindings (cdr cell))
	      (fluid-set (car cell) (cdr cell))))
	  (car (fluid saved-state)))))
