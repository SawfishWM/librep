#| peephole.jl -- peephole optimizer for rep assembly code

   $Id$

   Copyright (C) 1999, 2000 John Harper <john@dcs.warwick.ac.uk>

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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;; Most of the optimisation patterns in the peephole optimiser were
;; lifted from jwz's byte-optimize.el (XEmacs)

(declare (unsafe-for-call/cc))

(define-structure rep.vm.peephole

    (export peephole-optimizer)

    (open rep
	  rep.vm.bytecodes)

  ;; todo:

  ;; c{dd..d}r; car --> ca{dd..d}r
  ;; c{dd..d}r; cdr --> cd{dd..d}r

  ;; shift the instruction window
  (defmacro shift ()
    '(progn
       (setq point (cdr point))
       (setq insn0 insn1)
       (setq insn1 insn2)
       (setq insn2 (nth 3 point))))

  ;; refill the window
  (defmacro refill ()
    '(progn
       (setq insn0 (nth 1 point))
       (setq insn1 (nth 2 point))
       (setq insn2 (nth 3 point))))

  ;; delete the first instruction in the window
  (defmacro del-0 ()
    '(progn
       (rplacd point (nthcdr 2 point))
       (setq insn0 insn1)
       (setq insn1 insn2)
       (setq insn2 (nth 3 point))))

  ;; delete the second instruction in the window
  (defmacro del-1 ()
    '(progn
       (rplacd (cdr point) (nthcdr 3 point))
       (setq insn1 insn2)
       (setq insn2 (nth 3 point))))

  ;; delete the third instruction in the window
  (defmacro del-2 ()
    '(progn
       (rplacd (nthcdr 2 point) (nthcdr 4 point))
       (setq insn2 (nth 3 point))))

  ;; delete the first two instructions in the window
  (defmacro del-0-1 ()
    '(progn
       (rplacd point (nthcdr 3 point))
       (setq insn0 insn2)
       (setq insn1 (nth 2 point))
       (setq insn2 (nth 3 point))))

  ;; delete the second two instructions in the window
  (defmacro del-1-2 ()
    '(progn
       (rplacd (cdr point) (nthcdr 4 point))
       (setq insn1 (nth 2 point))
       (setq insn2 (nth 3 point))))

  ;; delete all instructions in the window
  (defmacro del-0-1-2 ()
    '(progn
       (rplacd point (nthcdr 4 point))
       (refill)))

  ;; debugging
  (defmacro before ()
    `(format standard-error "before: [%S %S %S]\n"
	     (nth 1 point) (nth 2 point) (nth 3 point)))
  (defmacro after ()
    `(format standard-error "after: [%S %S %S]\n"
	     (nth 1 point) (nth 2 point) (nth 3 point)))

  ;; run the optimiser over CODE-STRING, modifying and returning it
  ;; returns (CODE . EXTRA-STACK)
  (defun peephole-optimizer (code-string)
    (let ((keep-going t)
	  (extra-stack 0)
	  point insn0 insn1 insn2 tem)
      ;; add an extra cons cell so we can always refer to the
      ;; cdr of the intsruction _before_ insn0, this makes it
      ;; easy to delete instructions
      (setq code-string (cons 'start code-string))
      (while keep-going
	(setq keep-going nil)
	(setq point code-string)
	(refill)
	(while insn0
	  ;;(format standard-error "iter: %S\n\n" code-string)
	  (cond
	   ;; <side-effect-free w/ stack+1>; pop --> <deleted>
	   ;; <side-effect-free w/ stack+0>; pop --> pop
	   ;; <side-effect-free w/ stack-1>; pop --> pop; pop
	   ((and (eq (car insn1) 'pop)
		 (memq (car insn0) byte-side-effect-free-insns))
	    (setq tem (aref byte-insn-stack-delta (bytecode-ref (car insn0))))
	    (cond ((= tem 1)
		   (del-0-1)
		   (setq keep-going t))
		  ((= tem 0)
		   (del-0)
		   (setq keep-going t))
		  ((= tem -1)
		   (rplaca insn0 'pop)
		   (rplacd insn0 nil)
		   (setq keep-going t))))

	   ;; {push,dup}; setn #X; refn #X
	   ;;    --> {push,dup}; setn #X; {push, dup}
	   ;; {push,dup}; bind X; refn #0
	   ;;    --> {push,dup}; bind X; {push, dup}
	   ;; {push,dup}; slot-set #X; slot-ref #X
	   ;;    --> {push,dup}; slot-set #X; {push, dup}
	   ((and (or (and (eq (car insn1) 'setn) (eq (car insn2) 'refn)
			  (eq (cadr insn1) (cadr insn2)))
		     (and (eq (car insn1) 'bind) (eq (car insn2) 'refn)
			  (eq (cadr insn2) 0))
		     (and (eq (car insn1) 'slot-set) (eq (car insn2) 'slot-ref)
			  (eq (cadr insn1) (cadr insn2))))
		 (or (eq (car insn0) 'dup) (eq (car insn0) 'push)))
	    (rplaca insn2 (car insn0))
	    (rplacd insn2 (cdr insn0))
	    (setq keep-going t))

	   ;; setn #X; refn #X --> dup; setn #X
	   ;; bind; refn #0 --> dup; bind
	   ;; slot-set #X; slot-ref #X --> dup; slot-set #X
	   ((or (and (eq (car insn0) 'setn)
		     (eq (car insn1) 'refn)
		     (eq (cadr insn0) (cadr insn1)))
		(and (eq (car insn0) 'bind)
		     (eq (car insn1) 'refn)
		     (eql (cadr insn1) 0))
		(and (eq (car insn0) 'slot-set)
		     (eq (car insn1) 'slot-ref)
		     (eq (cadr insn0) (cadr insn1))))
	    (rplaca insn1 (car insn0))
	    (rplacd insn1 (cdr insn0))
	    (rplaca insn0 'dup)
	    (rplacd insn0 nil)
	    ;; this might require extra stack space
	    (setq extra-stack 1)
	    (setq keep-going t))

	   ;; dup; {<varset>,<varbind>} X; pop --> {<varset>,<varbind>} X
	   ((and (eq (car insn0) 'dup)
		 (or (memq (car insn1) byte-varset-insns)
		     (memq (car insn1) byte-varbind-insns))
		 (eq (car insn2) 'pop))
	    (rplaca insn2 (car insn1))
	    (rplacd insn2 (cdr insn1))
	    (del-0-1)
	    (setq keep-going t))

	   ;; <varref> X; <varref> X --> <varref> X; dup
	   ((and (memq (car insn0) byte-varref-insns)
		 (eq (car insn1) (car insn0))
		 (eq (cadr insn0) (cadr insn1)))
	    (rplaca insn1 'dup)
	    (rplacd insn1 nil)
	    (setq keep-going t))

	   ;; <varref> X; <varset> X --> deleted
	   ((or (and (eq (car insn0) 'refn)
		     (eq (car insn1) 'setn)
		     (eql (cadr insn0) (cadr insn1)))
		(and (eq (car insn0) 'refg)
		     (eq (car insn1) 'setg)
		     (eq (cadr insn0) (cadr insn1)))
		(and (eq (car insn0) 'slot-ref)
		     (eq (car insn1) 'slot-set)
		     (eq (cadr insn0) (cadr insn1))))
	    (del-0-1)
	    (setq keep-going t))

	   ;; c?r; c?r --> c??r
	   ((and (memq (car insn0) '(car cdr))
		 (memq (car insn1) '(car cdr)))
	    (rplaca insn1 (if (eq (car insn0) 'car)
			      (if (eq (car insn1) 'car) 'caar 'cdar)
			    (if (eq (car insn1) 'car) 'cadr 'cddr)))
	    (del-0)
	    (setq keep-going t))

	   ;; test-scm; scm-test --> deleted
	   ;; test-scm-f; scm-test --> deleted
	   ;; [ these are only possible because scm-test is only used
	   ;;   for `cond' tests, not for its actual value ]
	   ((and (memq (car insn0) '(test-scm test-scm-f))
		 (eq (car insn1) 'scm-test))
	    (del-0-1)
	    (setq keep-going t))

	   ;; push 1; sub --> dec
	   ;; push -1; sub --> inc
	   ;; push 1; add --> inc
	   ;; push -1; add --> dec
	   ;; [ XXX these and more should be handled at a higher level ]
	   ((and (eq (car insn0) 'push)
		 (memq (car insn1) '(sub add))
		 (memql (cadr insn0) '(1 -1)))
	    (let ((new (if (eql (cadr insn0) 1)
			   (if (eq (car insn1) 'sub) 'dec 'inc)
			 (if (eq (car insn1) 'sub) 'inc 'dec))))
	      (rplaca insn1 new)
	      (del-0)
	      (setq keep-going t)))

	   ;; push 0; {add,sub} --> <deleted>
	   ((and (equal insn0 '(push 0)) (memq (car insn1) '(add sub)))
	    (del-0-1)
	    (setq keep-going t))

	   ;; push 0; num-eq --> zerop
	   ((and (equal insn0 '(push 0)) (eq (car insn1) 'num-eq))
	    (rplaca insn1 'zerop)
	    (del-0)
	    (setq keep-going t))

	   ;; zerop; not --> not-zero-p
	   ((and (eq (car insn0) 'zerop) (eq (car insn1) 'not))
	    (rplaca insn1 'not-zero-p)
	    (del-0)
	    (setq keep-going t))

	   ;; jmp X; X: --> X:
	   ((and (eq (car insn0) 'jmp) (eq (cadr insn0) insn1))
	    (del-0)
	    (setq keep-going t))

	   ;; {jn,jt} X; X: --> pop; X:
	   ((and (memq (car insn0) '(jn jt)) (eq (cadr insn0) insn1))
	    (rplaca insn0 'pop)
	    (rplacd insn0 nil)
	    (setq keep-going t))

	   ;; {jpt,jpn} X; pop --> {jt,jn} X
	   ((and (memq (car insn0) '(jpt jpn)) (eq (car insn1) 'pop))
	    (rplaca insn0 (if (eq (car insn0) 'jpt) 'jt 'jn))
	    (del-1)
	    (setq keep-going t))

	   ;; not; {jn,jt} X --> {jt,jn} X
	   ((and (eq (car insn0) 'not)
		 (memq (car insn1) '(jn jt)))
	    (rplaca insn1 (if (eq (car insn1) 'jn) 'jt 'jn))
	    (del-0)
	    (setq keep-going t))

	   ;; jt X; (push ()) --> jpt X
	   ((and (eq (car insn0) 'jt) (equal insn1 '(push ())))
	    (rplaca insn0 'jpt)
	    (del-1)
	    (setq keep-going t))

	   ;; {jn,jt} X; jmp Y; X: --> {jt,jn} Y; X:
	   ((and (memq (car insn0) '(jn jt))
		 (eq (car insn1) 'jmp)
		 (eq (cadr insn0) insn2))
	    (rplaca insn1 (if (eq (car insn0) 'jn) 'jt 'jn))
	    (del-0)
	    (setq keep-going t))

	   ;; (push X); <cond. jump> X; --> whatever
	   ((and (eq (car insn0) 'push)
		 (memq (car insn1) byte-conditional-jmp-insns))
	    (let*
		;; only way to get a nil constant is through `(push ())'
		((is-nil (equal insn0 '(push ())))
		 (is-t (not is-nil)))
	      (cond ((or (and is-nil (eq (car insn1) 'jn))
			 (and is-t (eq (car insn1) 'jt))
			 (and is-nil (eq (car insn1) 'jpn))
			 (and is-t (eq (car insn1) 'jpt)))
		     ;; nil; jn X --> jmp X
		     ;; t; jt X --> jmp X
		     ;; nil; jpn X --> jmp X
		     ;; t; jpt X --> jmp X
		     (rplaca insn1 'jmp)
		     (del-0))
		    ((or (and is-nil (eq (car insn1) 'jt))
			 (and is-t (eq (car insn1) 'jn))
			 (and is-t (eq (car insn1) 'jnp))
			 (and is-nil (eq (car insn1) 'jtp)))
		     ;; nil; jt X --> <deleted>
		     ;; t; jn X --> <deleted>
		     ;; t; jnp X --> <deleted>
		     ;; nil; jtp X --> <deleted>
		     (del-0-1))
		    ((or (and is-nil (eq (car insn1) 'jnp))
			 (and is-t (eq (car insn1) 'jtp)))
		     ;; nil; jnp X --> nil; jmp X
		     ;; t; jtp X --> t; jmp X
		     (rplaca insn1 'jmp))
		    ((or (and is-t (eq (car insn1) 'jpn))
			 (and is-nil (eq (car insn1) 'jpt)))
		     ;; t; jpn X --> t
		     ;; nil; jpt X --> nil
		     (del-1))
		    (t (error "Unhandled contional jump case")))
	      (setq keep-going t)))

	   ;; <varref-and-error-free-op>; unbind ---> unbind; op
	   ((and (eq (car insn1) 'unbind)
		 (memq (car insn0) byte-varref-free-insns))
	    (let
		((op (car insn0))
		 (arg (cdr insn0)))
	      (rplaca insn0 (car insn1))
	      (rplacd insn0 (cdr insn1))
	      (rplaca insn1 op)
	      (rplacd insn1 arg)
	      (setq keep-going t)))

	   ;; <varbind> X; unbind --> pop; unbind
	   ((and (memq (car insn0) byte-varbind-insns)
		 (eq (car insn1) 'unbind))
	    (rplaca insn0 'pop)
	    (rplacd insn0 nil)
	    (setq keep-going t))

	   ;; init-bind; unbind --> deleted
	   ((and (eq (car insn0) 'init-bind) (eq (car insn1) 'unbind))
	    (del-0-1)
	    (setq keep-going t))

	   ;; init-bind; {return,unbindall} --> {return,unbindall}
	   ((and (eq (car insn0) 'init-bind)
		 (memq (car insn1) '(return unbindall)))
	    (del-0)
	    (setq keep-going t))

	   ;; unbind; return --> return
	   ((and (eq (car insn0) 'unbind) (eq (car insn1) 'return))
	    (del-0)
	    (setq keep-going t))

	   ;; <varref> X; dup... ; <varref> X --> <varref> X; dup...; dup
	   ((and (memq (car insn0) byte-varref-insns)
		 (eq (car insn1) 'dup))
	    (let
		((tem (nthcdr 2 point)))
	      (while (eq (car (car tem)) 'dup)
		(setq tem (cdr tem)))
	      (when (equal (car tem) insn0)
		(rplaca (car tem) 'dup)
		(rplacd (car tem) nil)
		(setq keep-going t))))

	   ;; X: Y: --> X:  [s/X/Y/]
	   ((and (symbolp insn0) (symbolp insn1))
	    (let loop ((rest (cdr code-string)))
	      (when rest
		(when (and (eq (cadar rest) insn1)
			   (or (memq (caar rest) byte-jmp-insns)
			       (eq (caar rest) 'push-label)))
		  (rplaca (cdar rest) insn0))
		(loop (cdr rest))))
	    (del-1)
	    (setq keep-going t))

	   ;; [unused] X: --> deleted
	   ((and (symbolp insn0)
		 (let loop ((rest (cdr code-string)))
		   (cond ((null rest) t)
			 ((and (eq (cadar rest) insn0)
			       (or (memq (caar rest) byte-jmp-insns)
				   (eq (caar rest) 'push-label))) nil)
			 (t (loop (cdr rest))))))
	    (del-0)
	    (setq keep-going t))

	   ;; jmp X; ... Y: --> jmp X; Y:
	   ;; return; ... Y: --> return; Y:
	   ((and (memq (car insn0) '(jmp ejmp return))
		 insn1 (not (symbolp insn1)))
	    (setq tem (nthcdr 2 point))
	    (while (and tem (not (symbolp (car tem))))
	      (setq tem (cdr tem)))
	    (unless (eq tem (nthcdr 2 point))
	      (rplacd (cdr point) tem)
	      (refill)
	      (setq keep-going t)))

	   ;; j* X; ... X: jmp Y --> j* Y; ... X: jmp Y
	   ((and (memq (car insn0) byte-jmp-insns)
		 (setq tem (or (memq (cadr insn0) (cdr code-string))
			       (error "Can't find jump destination: %s, %s"
				      insn0 (cdr code-string))))
		 (setq tem (car (cdr tem)))
		 (eq (car tem) 'jmp)
		 (not (eq (cadr insn0) (cadr tem))))
	    (rplacd insn0 (cdr tem))
	    (setq keep-going t))

	   ;; jmp X; ... X: return --> return; ... X: return
	   ((and (eq (car insn0) 'jmp)
		 (setq tem (or (memq (cadr insn0) (cdr code-string))
			       (error "Can't find jump destination: %s, %s"
				      insn0 (cdr code-string))))
		 (setq tem (car (cdr tem)))
		 (eq (car tem) 'return))
	    (rplaca insn0 'return)
	    (rplacd insn0 nil)
	    (setq keep-going t))

	   ;; {jnp,jtp} X; ... X: <cond. jmp> Y --> whatever
	   ((and (memq (car insn0) '(jnp jtp))
		 (setq tem (cdr (or (memq (cadr insn0) (cdr code-string))
				    (error "Can't find jump destination: %s, %s"
					   insn0 (cdr code-string)))))
		 (car tem)
		 (memq (car (car tem)) byte-conditional-jmp-insns))
	    (let
		((jmp (car tem))
		 need-new-label)
	      (if (eq (car insn0) 'jtp)
		  (cond
		   ((memq (car jmp) '(jpt jt))
		    ;; jtp X; ... X: jpt Y --> jt Y; ...
		    ;; jtp X; ... X: jt Y --> jt Y; ...
		    (rplaca insn0 'jt))
		   ((eq (car jmp) 'jpn)
		    ;; jtp X; ... X: jpn Y --> jpt Z; ... X: jpn Y; Z:
		    (rplaca insn0 'jpt)
		    (setq need-new-label t))
		   ((memq (car jmp) '(jn jnp))
		    ;; jtp X; ... X: jn Y --> jt Z; ... X: jpn Y; Z:
		    ;; jtp X; ... X: jnp Y --> jt Z; ... X: jpn Y; Z:
		    (rplaca insn0 'jt)
		    (setq need-new-label t))
		   ((eq (car jmp) 'jtp)
		    ;; jtp X; ... X: jtp Y --> jtp Y; ...
		    (rplaca insn0 'jtp)))
		(cond
		 ((eq (car jmp) 'jpt)
		  ;; jnp X; ... X: jpt Y --> jn Z; ... X: jpt Y; Z:
		  (rplaca insn0 'jnp)
		  (setq need-new-label t))
		 ((memq (car jmp) '(jpn jn))
		  ;; jnp X; ... X: jpn Y --> jn Y ...
		  ;; jnp X; ... X: jn Y --> jn Y ...
		  (rplaca insn0 'jn))
		 ((memq (car jmp) '(jt jtp))
		  ;; jnp X; ... X: jt Y --> jn Z; ... X: jt Y; Z:
		  ;; jnp X; ... X: jtp Y --> jn Z; ... X: jt Y; Z:
		  (rplaca insn0 'jn)
		  (setq need-new-label t))
		 ((eq (car jmp) 'jnp)
		  ;; jnp X; ... X: jnp Y --> jnp Y ...
		  (rplaca insn0 'jnp))))
	      (if (not need-new-label)
		  (rplaca (cdr insn0) (cadr jmp))
		;; add label `Z:' following the second jump
		(let ((label (cons (gensym) (cdr tem))))
		  (rplaca (cdr insn0) (car label))
		  (rplacd tem label)))
	      (setq keep-going t)))

	   ;; {jpt,jpn} X; jmp Y; X: --> {jnp,jtp} Y; X:
	   ;; {jtp,jnp} X; jmp Y; X: --> {jpn,jpt} Y; X:
	   ((and (eq (car insn1) 'jmp)
		 (memq (car insn0) '(jpt jpn jtp jnp))
		 (eq (cadr insn0) insn2))
	    (rplaca insn1 (case (car insn0)
			    ((jpt) 'jnp)
			    ((jpn) 'jtp)
			    ((jtp) 'jpn)
			    ((jnp) 'jpt)))
	    (del-0)
	    (setq keep-going t))

	   ;; <const>; jmp X; ... X: <cond. jmp> Y --> whatever
	   ;;
	   ;; [ this should be handled already, by (1) changing the
	   ;;   first jump, then by (2) dereferencing the constant ]

	   ;; jmp X: Y: ... X: <cond. jmp> Y --> ???

	   )
	  ;; shift in the next instruction
	  (shift)))

      ;; now do one last pass, looking for simple things
      (setq point code-string)
      (refill)
      (while insn0
	(cond
	 ;; push X; {<varset>,<varbind>} Y; push X
	 ;;   --> push X; dup; {<varset>,<varbind>} Y
	 ((and (eq (car insn0) 'push)
	       (or (memq (car insn1) byte-varset-insns)
		   (memq (car insn1) byte-varbind-insns))
	       (equal insn0 insn2))
	  (rplaca insn2 (car insn1))
	  (rplacd insn2 (cdr insn1))
	  (rplaca insn1 'dup)
	  (rplacd insn1 nil)
	  (setq extra-stack 1)
	  (setq keep-going t))

	 ;; push X; {dup,push X}... --> push X; dup...
	 ;; <varref> X; {dup,<varref> X}... --> <varref> X; dup...
	 ((or (eq (car insn0) 'push)
	      (memq (car insn0) byte-varref-insns))
	  (setq tem (nthcdr 2 point))
	  (while (or (eq (caar tem) 'dup)
		     (equal (car tem) insn0))
	    (rplaca (car tem) 'dup)
	    (rplacd (car tem) nil)
	    (setq tem (cdr tem)))))
	(shift))

      ;; drop the extra cons we added
      (cons (cdr code-string) extra-stack))))
