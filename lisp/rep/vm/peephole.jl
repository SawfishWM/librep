;;;; compiler-opt.jl -- low-level compiler optimisations
;;;  Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
;;;  $Id$

;;; This file is part of librep.

;;; librep is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; librep is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with Jade; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Most of the optimisation patterns in the peephole optimiser were
;; lifted from jwz's byte-optimize.el (XEmacs)

(require 'compiler)
(require 'bytecodes)
(provide 'compiler-opt)


;; Peephole optimiser

;; todo:

;; c{dd..d}r; car --> ca{dd..d}r
;; c{dd..d}r; cdr --> cd{dd..d}r

;; shift the instruction window
(defmacro comp-peep-shift ()
  '(progn
     (setq point (cdr point))
     (setq insn0 insn1)
     (setq insn1 insn2)
     (setq insn2 (nth 3 point))))

;; refill the window
(defmacro comp-peep-refill ()
  '(progn
     (setq insn0 (nth 1 point))
     (setq insn1 (nth 2 point))
     (setq insn2 (nth 3 point))))

;; delete the first instruction in the window
(defmacro comp-peep-del-0 ()
  '(progn
     (rplacd point (nthcdr 2 point))
     (setq insn0 insn1)
     (setq insn1 insn2)
     (setq insn2 (nth 3 point))))

;; delete the second instruction in the window
(defmacro comp-peep-del-1 ()
  '(progn
     (rplacd (cdr point) (nthcdr 3 point))
     (setq insn1 insn2)
     (setq insn2 (nth 3 point))))

;; delete the third instruction in the window
(defmacro comp-peep-del-2 ()
  '(progn
     (rplacd (nthcdr 2 point) (nthcdr 4 point))
     (setq insn2 (nth 3 point))))

;; delete the first two instructions in the window
(defmacro comp-peep-del-0-1 ()
  '(progn
     (rplacd point (nthcdr 3 point))
     (setq insn0 insn2)
     (setq insn1 (nth 2 point))
     (setq insn2 (nth 3 point))))

;; delete the second two instructions in the window
(defmacro comp-peep-del-1-2 ()
  '(progn
     (rplacd (cdr point) (nthcdr 4 point))
     (setq insn1 (nth 2 point))
     (setq insn2 (nth 3 point))))

;; delete all instructions in the window
(defmacro comp-peep-del-0-1-2 ()
  '(progn
     (rplacd point (nthcdr 4 point))
     (comp-peep-refill)))

;; run the optimiser over CODE-STRING, modifying and returning it
;; this assumes it's being called from somewhere inside the compiler;
;; it may modify comp-max-stack
(defun comp-peephole-opt (code-string)
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
      (comp-peep-refill)
      (while insn0
	(cond
	 ;; <side-effect-free w/ stack+1>; pop --> <deleted>
	 ;; <side-effect-free w/ stack+0>; pop --> pop
	 ;; <side-effect-free w/ stack-1>; pop --> pop; pop
	 ((and (eq (car insn1) op-pop)
	       (memq (car insn0) comp-side-effect-free-insns))
	  (setq tem (aref comp-insn-stack-delta (car insn0)))
	  (cond ((= tem 1)
		 (comp-peep-del-0-1)
		 (setq keep-going t))
		((= tem 0)
		 (comp-peep-del-0)
		 (setq keep-going t))
		((= tem -1)
		 (rplaca insn0 op-pop)
		 (rplacd insn0 nil)
		 (setq keep-going t))))

	 ;; {<const>,dup}; {setq,bind} X; refq X
	 ;;    --> {<const>,dup}; {setq,bind} X; {<const>,dup}
	 ;; {<const>,dup}; setn #X; refn #X
	 ;;    --> {<const>,dup}; setn #X; {<const>, dup}
	 ;; {<const>,dup}; bind X; refn #0
	 ;;    --> {<const>,dup}; bind X; {<const>, dup}
	 ((and (or (and (or (eq (car insn1) op-setq)
			    (eq (car insn1) op-bindspec))
			(eq (car insn2) op-refq)
			(eq (cdr insn1) (cdr insn2)))
		   (and (eq (car insn1) op-setn)
			(eq (car insn2) op-refn)
			(eq (cdr insn1) (cdr insn2)))
		   (and (eq (car insn1) op-bind)
			(eq (car insn2) op-refn)
			(eq (cdr insn2) 0)))
	       (or (eq (car insn0) op-dup)
		   (memq (car insn0) comp-constant-insns)))
	  (rplaca insn2 (car insn0))
	  (rplacd insn2 (cdr insn0))
	  (setq keep-going t))

	 ;; {setq,bindspec} X; refq X --> dup; {setq,bindspec} X
	 ;; setn #X; refn #X --> dup; setn #X
	 ;; bind X; refn #0 --> dup; bind X
	 ((or (and (or (eq (car insn0) op-setq)
		       (eq (car insn0) op-bindspec))
		   (eq (car insn1) op-refq)
		   (eq (cdr insn0) (cdr insn1)))
	      (and (eq (car insn0) op-setn)
		   (eq (car insn1) op-refn)
		   (eq (cdr insn0) (cdr insn1)))
	      (and (eq (car insn0) op-bind)
		   (eq (car insn1) op-refn)
		   (eq (cdr insn1) 0)))
	  (rplaca insn1 (car insn0))
	  (rplacd insn1 (cdr insn0))
	  (rplaca insn0 op-dup)
	  (rplacd insn0 nil)
	  ;; this might require extra stack space
	  (setq extra-stack 1)
	  (setq keep-going t))

	 ;; dup; {<varset>,<varbind>} X; pop --> {<varset>,<varbind>} X
	 ((and (eq (car insn0) op-dup)
	       (or (memq (car insn1) comp-varset-insns)
		   (memq (car insn1) comp-varbind-insns))
	       (eq (car insn2) op-pop))
	  (rplaca insn2 (car insn1))
	  (rplacd insn2 (cdr insn1))
	  (comp-peep-del-0-1)
	  (setq keep-going t))

	 ;; <varref> X; <varref> X --> refq X; dup
	 ((and (memq (car insn0) comp-varref-insns)
	       (eq (car insn1) (car insn0))
	       (eq (cdr insn0) (cdr insn1)))
	  (rplaca insn1 op-dup)
	  (rplacd insn1 nil)
	  (setq keep-going t))

	 ;; c?r; c?r --> c??r
	 ((and (or (eq (car insn0) op-car)
		   (eq (car insn0) op-cdr))
	       (or (eq (car insn1) op-car)
		   (eq (car insn1) op-cdr)))
	  (rplaca insn1 (if (eq (car insn0) op-car)
			    (if (eq (car insn1) op-car)
				op-caar
			      op-cdar)
			  (if (eq (car insn1) op-car)
			      op-cadr
			    op-cddr)))
	  (comp-peep-del-0)
	  (setq keep-going t))

	 ;; jmp X; X: --> X:
	 ((and (eq (car insn0) op-jmp)
	       (eq (cdr insn0) insn1))
	  (comp-peep-del-0)
	  (setq keep-going t))

	 ;; {jn,jt} X; X: --> pop; X:
	 ((and (or (eq (car insn0) op-jn) (eq (car insn0) op-jt))
	       (eq (cdr insn0) insn1))
	  (rplaca insn0 op-pop)
	  (rplacd insn0 nil)
	  (setq keep-going t))

	 ;; {jpt,jpn} X; pop --> {jt,jn} X
	 ((and (or (eq (car insn0) op-jpt)
		   (eq (car insn0) op-jpn))
	       (eq (car insn1) op-pop))
	  (rplaca insn0 (if (eq (car insn0) op-jpt) op-jt op-jn))
	  (comp-peep-del-1)
	  (setq keep-going t))

	 ;; not; {jn,jt} X --> {jt,jn} X
	 ((and (eq (car insn0) op-not)
	       (or (eq (car insn1) op-jn)
		   (eq (car insn1) op-jt))
	       (memq (car insn1) comp-conditional-jmp-insns))
	  (rplaca insn1 (if (eq (car insn1) op-jn) op-jt op-jn))
	  (comp-peep-del-0)
	  (setq keep-going t))

	 ;; jt X; nil --> jpt X
	 ((and (eq (car insn0) op-jt)
	       (eq (car insn1) op-nil))
	  (rplaca insn0 op-jtp)
	  (comp-peep-del-1)
	  (setq keep-going t))

	 ;; {jn,jt} X; jmp Y; X: --> {jt,jn} Y; X:
	 ((and (or (eq (car insn1) op-jn)
		   (eq (car insn1) op-jt))
	       (eq (car insn1) op-jmp)
	       (eq (car insn2) (cdr insn0)))
	  (rplaca insn1 (if (eq (car insn0) op-jn) op-jt op-jn))
	  (comp-peep-del-0)
	  (setq keep-going t))

	 ;; <const>; <cond. jump> X; --> whatever
	 ((and (memq (car insn0) comp-constant-insns)
	       (memq (car insn1) comp-conditional-jmp-insns))
	  (let*
	      ;; only way to get a nil constant is through op-nil
	      ((is-nil (eq (car insn0) op-nil))
	       (is-t (not is-nil)))
	    (cond ((or (and is-nil (eq (car insn1) op-jn))
		       (and is-t (eq (car insn1) op-jt))
		       (and is-nil (eq (car insn1) op-jpn))
		       (and is-t (eq (car insn1) op-jpt)))
		   ;; nil; jn X --> jmp X
		   ;; t; jt X --> jmp X
		   ;; nil; jpn X --> jmp X
		   ;; t; jpt X --> jmp X
		   (rplaca insn1 op-jmp)
		   (comp-peep-del-0))
		  ((or (and is-nil (eq (car insn1) op-jt))
		       (and is-t (eq (car insn1) op-jn))
		       (and is-t (eq (car insn1) op-jnp))
		       (and is-nil (eq (car insn1) op-jtp)))
		   ;; nil; jt X --> <deleted>
		   ;; t; jn X --> <deleted>
		   ;; t; jnp X --> <deleted>
		   ;; nil; jtp X --> <deleted>
		   (comp-peep-del-0-1))
		  ((or (and is-nil (eq (car insn1) op-jnp))
		       (and is-t (eq (car insn1) op-jtp)))
		   ;; nil; jnp X --> nil; jmp X
		   ;; t; jpt X --> t; jmp X
		   (rplaca insn1 op-jmp))
		  ((or (and is-t (eq (car insn1) op-jpn))
		       (and is-nil (eq (car insn1) op-jpt)))
		   ;; t; jpn X --> t
		   ;; nil; jpt X --> nil
		   (comp-peep-del-1))
		  (t
		   (error "Unhandled contional jump case")))
	    (setq keep-going t)))

	 ;; <varref-and-error-free-op>; unbind ---> unbind; op
	 ((and (eq (car insn1) op-unbind)
	       (memq (car insn0) comp-varref-free-insns))
	  (let
	      ((op (car insn0))
	       (arg (cdr insn0)))
	    (rplaca insn0 (car insn1))
	    (rplacd insn0 (cdr insn1))
	    (rplaca insn1 op)
	    (rplacd insn1 arg)
	    (setq keep-going t)))

	 ;; <varbind> X; unbind --> pop; unbind
	 ((and (memq (car insn0) comp-varbind-insns)
	       (eq (car insn1) op-unbind))
	  (rplaca insn0 op-pop)
	  (rplacd insn0 nil)
	  (setq keep-going t))

	 ;; init-bind; unbind --> deleted
	 ((and (eq (car insn0) op-init-bind)
	       (eq (car insn1) op-unbind))
	  (comp-peep-del-0-1)
	  (setq keep-going t))

	 ;; unbind; return --> return
	 ((and (eq (car insn0) op-unbind)
	       (eq (car insn1) op-return))
	  (comp-peep-del-0)
	  (setq keep-going t))

	 ;; <varref> X; dup... ; <varref> X --> <varref> X; dup...; dup
	 ((and (memq (car insn0) comp-varref-insns)
	       (eq (car insn1) op-dup))
	  (let
	      ((tem (nthcdr 2 point)))
	    (while (eq (car (car tem)) op-dup)
	      (setq tem (cdr tem)))
	    (when (and (eq (car (car tem)) (car insn0))
		       (eq (cdr (car tem)) (cdr insn0)))
	      (rplaca (car tem) op-dup)
	      (rplacd (car tem) nil)
	      (setq keep-going t))))

	 ;; X: Y: --> X:  [s/X/Y/]
	 ((and (eq (car insn0) 'label)
	       (eq (car insn1) 'label))
	  (while (setq tem (rassq insn1 code-string))
	    (rplacd tem insn0))
	  (comp-peep-del-1)
	  (setq keep-going t))

	 ;; [unused] X: --> deleted
	 ((and (eq (car insn0) 'label)
	       (not (rassq insn0 code-string)))
	  (comp-peep-del-0)
	  (setq keep-going t))

	 ;; jmp X; ... Y: --> jmp X; Y:
	 ((and (eq (car insn0) op-jmp)
	       (not (eq (car insn1) 'label)))
	  (setq tem (nthcdr 2 point))
	  (while (and tem (not (eq (car (car tem)) 'label)))
	    (setq tem (cdr tem)))
	  (when (eq (cdr insn0) (car tem))
	    (rplacd (cdr point) tem)
	    (comp-peep-refill)
	    (setq keep-going t)))

	 ;; j* X; ... X: jmp Y --> j* Y; ... X: jmp Y
	 ((and (memq (car insn0) comp-jmp-insns)
	       (setq tem (or (memq (cdr insn0) (cdr code-string))
			     (error "Can't find jump destination")))
	       (setq tem (car (cdr tem)))
	       (eq (car tem) op-jmp))
	  (rplacd insn0 (cdr tem))
	  (setq keep-going t))

	 ;; jmp X; ... X: return --> return; ... X: return
	 ((and (eq (car insn0) op-jmp)
	       (setq tem (or (memq (cdr insn0) (cdr code-string))
			     (error "Can't find jump destination")))
	       (setq tem (car (cdr tem)))
	       (eq (car tem) op-return))
	  (rplaca insn0 op-return)
	  (rplacd insn0 nil)
	  (setq keep-going t))

	 ;; {jnp,jtp} X; ... X: <cond. jmp> Y --> whatever
	 ((and (or (eq (car insn0) op-jnp)
		   (eq (car insn0) op-jtp))
	       (setq tem (cdr (or (memq (cdr insn0) (cdr code-string))
				  (error "Can't find jump destination"))))
	       (car tem)
	       (memq (car (car tem)) comp-conditional-jmp-insns))
	  (let
	      ((jmp (car tem))
	       need-new-label)
	    (if (eq (car insn0) op-jtp)
		(cond
		 ((or (eq (car jmp) op-jpt)
		      (eq (car jmp) op-jt))
		  ;; jtp X; ... X: jpt Y --> jt Y; ...
		  ;; jtp X; ... X: jt Y --> jt Y; ...
		  (rplaca insn0 op-jt))
		 ((eq (car jmp) op-jpn)
		  ;; jtp X; ... X: jpn Y --> jpt Z; ... X: jpn Y; Z:
		  (rplaca insn0 op-jpt)
		  (setq need-new-label t))
		 ((or (eq (car jmp) op-jn)
		      (eq (car jmp) op-jnp))
		  ;; jtp X; ... X: jn Y --> jt Z; ... X: jpn Y; Z:
		  ;; jtp X; ... X: jnp Y --> jt Z; ... X: jpn Y; Z:
		  (rplaca insn0 op-jt)
		  (setq need-new-label t))
		 ((eq (car jmp) op-jtp)
		  ;; jtp X; ... X: jtp Y --> jtp Y; ...
		  (rplaca insn0 op-jtp)))
	      (cond
	       ((eq (car jmp) op-jpt)
		;; jnp X; ... X: jpt Y --> jn Z; ... X: jpt Y; Z:
		(rplaca insn0 op-jnp)
		(setq need-new-label t))
	       ((or (eq (car jmp) op-jpn)
		    (eq (car jmp) op-jn))
		;; jnp X; ... X: jpn Y --> jn Y ...
		;; jnp X; ... X: jn Y --> jn Y ...
		(rplaca insn0 op-jn))
	       ((or (eq (car jmp) op-jt)
		    (eq (car jmp) op-jtp))
		;; jnp X; ... X: jt Y --> jn Z; ... X: jt Y; Z:
		;; jnp X; ... X: jtp Y --> jn Z; ... X: jt Y; Z:
		(rplaca insn0 op-jn)
		(setq need-new-label t))
	       ((eq (car jmp) op-jnp)
		;; jnp X; ... X: jnp Y --> jnp Y ...
		(rplaca insn0 op-jnp))))
	    (if (not need-new-label)
		(rplacd insn0 (cdr jmp))
	      ;; add label `Z:' following the second jump
	      (let
		  ((label (cons (comp-make-label) (cdr tem))))
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
	(comp-peep-shift)))

    ;; now do one last pass, looking for simple things
    (setq point code-string)
    (comp-peep-refill)
    (while insn0
      (cond
       ;; <const> X; {<varref>,<varbind>} Y; <const X>
       ;;   --> <const X>; dup; {<varref>,<varbind>} Y
       ((and (or (memq (car insn1) comp-varset-insns)
		 (memq (car insn1) comp-varbind-insns))
	     (memq (car insn0) comp-constant-insns)
	     (eq insn0 insn2))
	(rplaca insn2 (car insn1))
	(rplacd insn2 (cdr insn1))
	(rplaca insn1 op-dup)
	(rplacd insn1 nil)
	(setq extra-stack 1)
	(setq keep-going t))

       ;; <const> X; {dup,<const> X}... --> <const> X; dup...
       ;; <varref> X; {dup,<varref> X}... --> <varref> X; dup...
       ((or (memq (car insn0) comp-constant-insns)
	    (memq (car insn0) comp-varref-insns))
	(setq tem (nthcdr 2 point))
	(while (or (eq (car (car tem)) op-dup)
		   (equal (car tem) insn0))
	  (rplaca (car tem) op-dup)
	  (rplacd (car tem) nil)
	  (setq tem (cdr tem)))))
      (comp-peep-shift))

    (setq comp-max-stack (+ comp-max-stack extra-stack))
    ;; drop the extra cons we added
    (cdr code-string)))


;; Optimisation of the constant vector

;; All this does is to delete any unused constants, and reorders the
;; indices such that the most commonly used values get the smallest
;; indices. This will probably decrease the overall code size (using
;; 1-byte instructions instead of 2-byte, or 2 instead of 3)

;; modifies the comp-constant-alist variable, returns the new code string
(defun comp-optimise-constants (code-string)
  (let
      ((comp-constant-usage (make-vector comp-constant-index 0)))
    ;; first count how many times each constant is used
    (mapc (lambda (insn)
	    (when (memq (car insn) comp-insns-with-constants)
	      (aset comp-constant-usage (cdr insn)
		    (1+ (aref comp-constant-usage (cdr insn))))))
	  code-string)
    ;; now sort by usage, minimum to maximum
    (setq comp-constant-alist
	  (sort comp-constant-alist
		(lambda (x y)
		  (< (aref comp-constant-usage (cdr x))
		     (aref comp-constant-usage (cdr y))))))
    ;; delete any unused constants at the head of the list
    (while (and comp-constant-alist
		(zerop (aref comp-constant-usage
			     (cdr (car comp-constant-alist)))))
      (setq comp-constant-alist (cdr comp-constant-alist)))
    ;; reverse the list to get most-used-first
    (setq comp-constant-alist (nreverse comp-constant-alist))
    ;; then assign new indices, based on current list position
    ;; reuse comp-constant-usage to map from old to new positions
    (let
	((i 0))
      (mapc (lambda (c)
	      (aset comp-constant-usage (cdr c) i)
	      (rplacd c i)
	      (setq i (1+ i))) comp-constant-alist))
    ;; now update the code string
    (mapc (lambda (insn)
	    (when (memq (car insn) comp-insns-with-constants)
	      (rplacd insn (aref comp-constant-usage (cdr insn)))))
	  code-string)
    code-string))
