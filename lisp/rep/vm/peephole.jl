#| compiler-opt.jl -- low-level compiler optimisations

   $Id$

   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure compiler-opt (export peephole-optimizer
				       constant-optimizer)
  (open rep
	compiler
	compiler-lap
	compiler-const
	compiler-utils
	bytecodes)


;; Peephole optimiser

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

;; run the optimiser over CODE-STRING, modifying and returning it
;; this assumes it's being called from somewhere inside the compiler;
;; it may modify (fluid max-stack)
(defun peephole-optimizer (code-string)
  (let
      ((keep-going t)
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
	(cond
	 ;; <side-effect-free w/ stack+1>; pop --> <deleted>
	 ;; <side-effect-free w/ stack+0>; pop --> pop
	 ;; <side-effect-free w/ stack-1>; pop --> pop; pop
	 ((and (eq (car insn1) (bytecode pop))
	       (memq (car insn0) byte-side-effect-free-insns))
	  (setq tem (aref byte-insn-stack-delta (car insn0)))
	  (cond ((= tem 1)
		 (del-0-1)
		 (setq keep-going t))
		((= tem 0)
		 (del-0)
		 (setq keep-going t))
		((= tem -1)
		 (rplaca insn0 (bytecode pop))
		 (rplacd insn0 nil)
		 (setq keep-going t))))

	 ;; {<const>,dup}; {setq,bind} X; refq X
	 ;;    --> {<const>,dup}; {setq,bind} X; {<const>,dup}
	 ;; {<const>,dup}; setn #X; refn #X
	 ;;    --> {<const>,dup}; setn #X; {<const>, dup}
	 ;; {<const>,dup}; bind X; refn #0
	 ;;    --> {<const>,dup}; bind X; {<const>, dup}
	 ((and (or (and (or (eq (car insn1) (bytecode setq))
			    (eq (car insn1) (bytecode bindspec)))
			(eq (car insn2) (bytecode refq))
			(eq (cdr insn1) (cdr insn2)))
		   (and (eq (car insn1) (bytecode setn))
			(eq (car insn2) (bytecode refn))
			(eq (cdr insn1) (cdr insn2)))
		   (and (eq (car insn1) (bytecode bind))
			(eq (car insn2) (bytecode refn))
			(eq (cdr insn2) 0)))
	       (or (eq (car insn0) (bytecode dup))
		   (memq (car insn0) byte-constant-insns)))
	  (rplaca insn2 (car insn0))
	  (rplacd insn2 (cdr insn0))
	  (setq keep-going t))

	 ;; {setq,bindspec} X; refq X --> dup; {setq,bindspec} X
	 ;; setn #X; refn #X --> dup; setn #X
	 ;; bind X; refn #0 --> dup; bind X
	 ((or (and (or (eq (car insn0) (bytecode setq))
		       (eq (car insn0) (bytecode bindspec)))
		   (eq (car insn1) (bytecode refq))
		   (eq (cdr insn0) (cdr insn1)))
	      (and (eq (car insn0) (bytecode setn))
		   (eq (car insn1) (bytecode refn))
		   (eq (cdr insn0) (cdr insn1)))
	      (and (eq (car insn0) (bytecode bind))
		   (eq (car insn1) (bytecode refn))
		   (eq (cdr insn1) 0)))
	  (rplaca insn1 (car insn0))
	  (rplacd insn1 (cdr insn0))
	  (rplaca insn0 (bytecode dup))
	  (rplacd insn0 nil)
	  ;; this might require extra stack space
	  (setq extra-stack 1)
	  (setq keep-going t))

	 ;; dup; {<varset>,<varbind>} X; pop --> {<varset>,<varbind>} X
	 ((and (eq (car insn0) (bytecode dup))
	       (or (memq (car insn1) byte-varset-insns)
		   (memq (car insn1) byte-varbind-insns))
	       (eq (car insn2) (bytecode pop)))
	  (rplaca insn2 (car insn1))
	  (rplacd insn2 (cdr insn1))
	  (del-0-1)
	  (setq keep-going t))

	 ;; <varref> X; <varref> X --> refq X; dup
	 ((and (memq (car insn0) byte-varref-insns)
	       (eq (car insn1) (car insn0))
	       (eq (cdr insn0) (cdr insn1)))
	  (rplaca insn1 (bytecode dup))
	  (rplacd insn1 nil)
	  (setq keep-going t))

	 ;; <varref> X; <varset> X --> deleted
	 ((or (and (eq (car insn0) (bytecode refn))
		   (eq (car insn1) (bytecode setn))
		   (equal (cdr insn0) (cdr insn1)))
	      (and (eq (car insn0) (bytecode refg))
		   (eq (car insn1) (bytecode setg))
		   (equal (cdr insn0) (cdr insn1)))
	      (and (eq (car insn0) (bytecode refq))
		   (eq (car insn1) (bytecode setq))
		   (equal (cdr insn0) (cdr insn1))))
	  (del-0-1)
	  (setq keep-going t))

	 ;; c?r; c?r --> c??r
	 ((and (or (eq (car insn0) (bytecode car))
		   (eq (car insn0) (bytecode cdr)))
	       (or (eq (car insn1) (bytecode car))
		   (eq (car insn1) (bytecode cdr))))
	  (rplaca insn1 (if (eq (car insn0) (bytecode car))
			    (if (eq (car insn1) (bytecode car))
				(bytecode caar)
			      (bytecode cdar))
			  (if (eq (car insn1) (bytecode car))
			      (bytecode cadr)
			    (bytecode cddr))))
	  (del-0)
	  (setq keep-going t))

	 ;; test-scm; scm-test --> deleted
	 ;; test-scm-f; scm-test --> deleted
	 ((and (or (eq (car insn0) (bytecode test-scm))
		   (eq (car insn0) (bytecode test-scm-f)))
	       (eq (car insn1) (bytecode scm-test)))
	  (del-0-1)
	  (setq keep-going t))

	 ;; pushi-1; sub --> dec
	 ;; pushi-minus-1; sub --> inc
	 ;; pushi-1; add --> inc
	 ;; pushi-minus-1; add --> dec
	 ;; [ XXX these and more should be handled at a higher level ]
	 ((and (or (eq (car insn0) (bytecode pushi-1))
		   (eq (car insn0) (bytecode pushi-minus-1)))
	       (or (eq (car insn1) (bytecode sub))
		   (eq (car insn1) (bytecode add))))
	  (let ((new (if (eq (car insn0) (bytecode pushi-1))
			 (if (eq (car insn1) (bytecode sub))
			     (bytecode dec)
			   (bytecode inc))
		       (if (eq (car insn1) (bytecode sub))
			   (bytecode inc)
			 (bytecode dec)))))
	    (rplaca insn1 new)
	    (del-0)
	    (setq keep-going t)))

	 ;; jmp X; X: --> X:
	 ((and (eq (car insn0) (bytecode jmp))
	       (eq (cdr insn0) insn1))
	  (del-0)
	  (setq keep-going t))

	 ;; {jn,jt} X; X: --> pop; X:
	 ((and (or (eq (car insn0) (bytecode jn))
		   (eq (car insn0) (bytecode jt)))
	       (eq (cdr insn0) insn1))
	  (rplaca insn0 (bytecode pop))
	  (rplacd insn0 nil)
	  (setq keep-going t))

	 ;; {jpt,jpn} X; pop --> {jt,jn} X
	 ((and (or (eq (car insn0) (bytecode jpt))
		   (eq (car insn0) (bytecode jpn)))
	       (eq (car insn1) (bytecode pop)))
	  (rplaca insn0 (if (eq (car insn0) (bytecode jpt))
			    (bytecode jt)
			  (bytecode jn)))
	  (del-1)
	  (setq keep-going t))

	 ;; not; {jn,jt} X --> {jt,jn} X
	 ((and (eq (car insn0) (bytecode not))
	       (or (eq (car insn1) (bytecode jn))
		   (eq (car insn1) (bytecode jt)))
	       (memq (car insn1) byte-conditional-jmp-insns))
	  (rplaca insn1 (if (eq (car insn1) (bytecode jn))
			    (bytecode jt)
			  (bytecode jn)))
	  (del-0)
	  (setq keep-going t))

	 ;; jt X; nil --> jpt X
	 ((and (eq (car insn0) (bytecode jt))
	       (eq (car insn1) (bytecode nil)))
	  (rplaca insn0 (bytecode jpt))
	  (del-1)
	  (setq keep-going t))

	 ;; {jn,jt} X; jmp Y; X: --> {jt,jn} Y; X:
	 ((and (or (eq (car insn0) (bytecode jn))
		   (eq (car insn0) (bytecode jt)))
	       (eq (car insn1) (bytecode jmp))
	       (eq (cdr insn0) insn2))
	  (rplaca insn1 (if (eq (car insn0) (bytecode jn))
			    (bytecode jt)
			  (bytecode jn)))
	  (del-0)
	  (setq keep-going t))

	 ;; <const>; <cond. jump> X; --> whatever
	 ((and (memq (car insn0) byte-constant-insns)
	       (memq (car insn1) byte-conditional-jmp-insns))
	  (let*
	      ;; only way to get a nil constant is through (bytecode nil)
	      ((is-nil (eq (car insn0) (bytecode nil)))
	       (is-t (not is-nil)))
	    (cond ((or (and is-nil (eq (car insn1) (bytecode jn)))
		       (and is-t (eq (car insn1) (bytecode jt)))
		       (and is-nil (eq (car insn1) (bytecode jpn)))
		       (and is-t (eq (car insn1) (bytecode jpt))))
		   ;; nil; jn X --> jmp X
		   ;; t; jt X --> jmp X
		   ;; nil; jpn X --> jmp X
		   ;; t; jpt X --> jmp X
		   (rplaca insn1 (bytecode jmp))
		   (del-0))
		  ((or (and is-nil (eq (car insn1) (bytecode jt)))
		       (and is-t (eq (car insn1) (bytecode jn)))
		       (and is-t (eq (car insn1) (bytecode jnp)))
		       (and is-nil (eq (car insn1) (bytecode jtp))))
		   ;; nil; jt X --> <deleted>
		   ;; t; jn X --> <deleted>
		   ;; t; jnp X --> <deleted>
		   ;; nil; jtp X --> <deleted>
		   (del-0-1))
		  ((or (and is-nil (eq (car insn1) (bytecode jnp)))
		       (and is-t (eq (car insn1) (bytecode jtp))))
		   ;; nil; jnp X --> nil; jmp X
		   ;; t; jpt X --> t; jmp X
		   (rplaca insn1 (bytecode jmp)))
		  ((or (and is-t (eq (car insn1) (bytecode jpn)))
		       (and is-nil (eq (car insn1) (bytecode jpt))))
		   ;; t; jpn X --> t
		   ;; nil; jpt X --> nil
		   (del-1))
		  (t
		   (error "Unhandled contional jump case")))
	    (setq keep-going t)))

	 ;; <varref-and-error-free-op>; unbind ---> unbind; op
	 ((and (eq (car insn1) (bytecode unbind))
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
	       (eq (car insn1) (bytecode unbind)))
	  (rplaca insn0 (bytecode pop))
	  (rplacd insn0 nil)
	  (setq keep-going t))

	 ;; init-bind; unbind --> deleted
	 ((and (eq (car insn0) (bytecode init-bind))
	       (eq (car insn1) (bytecode unbind)))
	  (del-0-1)
	  (setq keep-going t))

	 ;; init-bind; {return,unbindall} --> {return,unbindall}
	 ((and (eq (car insn0) (bytecode init-bind))
	       (or (eq (car insn1) (bytecode return))
		   (eq (car insn1) (bytecode unbindall))))
	  (del-0)
	  (setq keep-going t))

	 ;; unbind; return --> return
	 ((and (eq (car insn0) (bytecode unbind))
	       (eq (car insn1) (bytecode return)))
	  (del-0)
	  (setq keep-going t))

	 ;; <varref> X; dup... ; <varref> X --> <varref> X; dup...; dup
	 ((and (memq (car insn0) byte-varref-insns)
	       (eq (car insn1) (bytecode dup)))
	  (let
	      ((tem (nthcdr 2 point)))
	    (while (eq (car (car tem)) (bytecode dup))
	      (setq tem (cdr tem)))
	    (when (and (eq (car (car tem)) (car insn0))
		       (eq (cdr (car tem)) (cdr insn0)))
	      (rplaca (car tem) (bytecode dup))
	      (rplacd (car tem) nil)
	      (setq keep-going t))))

	 ;; X: Y: --> X:  [s/X/Y/]
	 ((and (eq (car insn0) 'label)
	       (eq (car insn1) 'label))
	  (while (setq tem (rassq insn1 code-string))
	    (rplacd tem insn0))
	  (del-1)
	  (setq keep-going t))

	 ;; [unused] X: --> deleted
	 ((and (eq (car insn0) 'label)
	       (not (rassq insn0 code-string)))
	  (del-0)
	  (setq keep-going t))

	 ;; jmp X; ... Y: --> jmp X; Y:
	 ;; return; ... Y: --> return; Y:
	 ((and (or (eq (car insn0) (bytecode jmp))
		   (eq (car insn0) (bytecode return)))
	       insn1 (not (eq (car insn1) 'label)))
	  (setq tem (nthcdr 2 point))
	  (while (and tem (not (eq (car (car tem)) 'label)))
	    (setq tem (cdr tem)))
	  (unless (eq tem (nthcdr 2 point))
	    (rplacd (cdr point) tem)
	    (refill)
	    (setq keep-going t)))

	 ;; j* X; ... X: jmp Y --> j* Y; ... X: jmp Y
	 ((and (memq (car insn0) byte-jmp-insns)
	       (setq tem (or (memq (cdr insn0) (cdr code-string))
			     (error "Can't find jump destination")))
	       (setq tem (car (cdr tem)))
	       (eq (car tem) (bytecode jmp)))
	  (rplacd insn0 (cdr tem))
	  (setq keep-going t))

	 ;; jmp X; ... X: return --> return; ... X: return
	 ((and (eq (car insn0) (bytecode jmp))
	       (setq tem (or (memq (cdr insn0) (cdr code-string))
			     (error "Can't find jump destination")))
	       (setq tem (car (cdr tem)))
	       (eq (car tem) (bytecode return)))
	  (rplaca insn0 (bytecode return))
	  (rplacd insn0 nil)
	  (setq keep-going t))

	 ;; {jnp,jtp} X; ... X: <cond. jmp> Y --> whatever
	 ((and (or (eq (car insn0) (bytecode jnp))
		   (eq (car insn0) (bytecode jtp)))
	       (setq tem (cdr (or (memq (cdr insn0) (cdr code-string))
				  (error "Can't find jump destination"))))
	       (car tem)
	       (memq (car (car tem)) byte-conditional-jmp-insns))
	  (let
	      ((jmp (car tem))
	       need-new-label)
	    (if (eq (car insn0) (bytecode jtp))
		(cond
		 ((or (eq (car jmp) (bytecode jpt))
		      (eq (car jmp) (bytecode jt)))
		  ;; jtp X; ... X: jpt Y --> jt Y; ...
		  ;; jtp X; ... X: jt Y --> jt Y; ...
		  (rplaca insn0 (bytecode jt)))
		 ((eq (car jmp) (bytecode jpn))
		  ;; jtp X; ... X: jpn Y --> jpt Z; ... X: jpn Y; Z:
		  (rplaca insn0 (bytecode jpt))
		  (setq need-new-label t))
		 ((or (eq (car jmp) (bytecode jn))
		      (eq (car jmp) (bytecode jnp)))
		  ;; jtp X; ... X: jn Y --> jt Z; ... X: jpn Y; Z:
		  ;; jtp X; ... X: jnp Y --> jt Z; ... X: jpn Y; Z:
		  (rplaca insn0 (bytecode jt))
		  (setq need-new-label t))
		 ((eq (car jmp) (bytecode jtp))
		  ;; jtp X; ... X: jtp Y --> jtp Y; ...
		  (rplaca insn0 (bytecode jtp))))
	      (cond
	       ((eq (car jmp) (bytecode jpt))
		;; jnp X; ... X: jpt Y --> jn Z; ... X: jpt Y; Z:
		(rplaca insn0 (bytecode jnp))
		(setq need-new-label t))
	       ((or (eq (car jmp) (bytecode jpn))
		    (eq (car jmp) (bytecode jn)))
		;; jnp X; ... X: jpn Y --> jn Y ...
		;; jnp X; ... X: jn Y --> jn Y ...
		(rplaca insn0 (bytecode jn)))
	       ((or (eq (car jmp) (bytecode jt))
		    (eq (car jmp) (bytecode jtp)))
		;; jnp X; ... X: jt Y --> jn Z; ... X: jt Y; Z:
		;; jnp X; ... X: jtp Y --> jn Z; ... X: jt Y; Z:
		(rplaca insn0 (bytecode jn))
		(setq need-new-label t))
	       ((eq (car jmp) (bytecode jnp))
		;; jnp X; ... X: jnp Y --> jnp Y ...
		(rplaca insn0 (bytecode jnp)))))
	    (if (not need-new-label)
		(rplacd insn0 (cdr jmp))
	      ;; add label `Z:' following the second jump
	      (let
		  ((label (cons (make-label) (cdr tem))))
		(rplacd insn0 (car label))
		(rplacd tem label)))
	    (setq keep-going t)))

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
       ;; <const> X; {<varref>,<varbind>} Y; <const X>
       ;;   --> <const X>; dup; {<varref>,<varbind>} Y
       ((and (or (memq (car insn1) byte-varset-insns)
		 (memq (car insn1) byte-varbind-insns))
	     (memq (car insn0) byte-constant-insns)
	     (eq insn0 insn2))
	(rplaca insn2 (car insn1))
	(rplacd insn2 (cdr insn1))
	(rplaca insn1 (bytecode dup))
	(rplacd insn1 nil)
	(setq extra-stack 1)
	(setq keep-going t))

       ;; <const> X; {dup,<const> X}... --> <const> X; dup...
       ;; <varref> X; {dup,<varref> X}... --> <varref> X; dup...
       ((or (memq (car insn0) byte-constant-insns)
	    (memq (car insn0) byte-varref-insns))
	(setq tem (nthcdr 2 point))
	(while (or (eq (car (car tem)) (bytecode dup))
		   (equal (car tem) insn0))
	  (rplaca (car tem) (bytecode dup))
	  (rplacd (car tem) nil)
	  (setq tem (cdr tem)))))
      (shift))

    (fluid-set max-stack (+ (fluid max-stack) extra-stack))
    ;; drop the extra cons we added
    (cdr code-string)))


;; Optimisation of the constant vector

;; All this does is to delete any unused constants, and reorders the
;; indices such that the most commonly used values get the smallest
;; indices. This will probably decrease the overall code size (using
;; 1-byte instructions instead of 2-byte, or 2 instead of 3)

;; modifies the constant-alist fluid, returns the new code string
(defun constant-optimizer (code-string)
  (let
      ((comp-constant-usage (make-vector (fluid constant-index) 0)))
    ;; first count how many times each constant is used
    (mapc (lambda (insn)
	    (when (memq (car insn) byte-insns-with-constants)
	      (aset comp-constant-usage (cdr insn)
		    (1+ (aref comp-constant-usage (cdr insn))))))
	  code-string)
    ;; now sort by usage, minimum to maximum
    (fluid-set constant-alist
	       (sort (fluid constant-alist)
		     (lambda (x y)
		       (< (aref comp-constant-usage (cdr x))
			  (aref comp-constant-usage (cdr y))))))
    ;; delete any unused constants at the head of the list
    (while (and (fluid constant-alist)
		(zerop (aref comp-constant-usage
			     (cdr (car (fluid constant-alist))))))
      (fluid-set constant-alist (cdr (fluid constant-alist))))
    ;; reverse the list to get most-used-first
    (fluid-set constant-alist (nreverse (fluid constant-alist)))
    ;; then assign new indices, based on current list position
    ;; reuse comp-constant-usage to map from old to new positions
    (let
	((i 0))
      (mapc (lambda (c)
	      (aset comp-constant-usage (cdr c) i)
	      (rplacd c i)
	      (setq i (1+ i))) (fluid constant-alist)))
    ;; now update the code string
    (mapc (lambda (insn)
	    (when (memq (car insn) byte-insns-with-constants)
	      (rplacd insn (aref comp-constant-usage (cdr insn)))))
	  code-string)
    code-string))
)
