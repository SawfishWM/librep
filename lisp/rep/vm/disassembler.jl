;; disassembler.jl -- Disassembles compiled Lisp functions

;; $Id$

;; Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of Jade.

;; Jade is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Jade is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Jade; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(declare (unsafe-for-call/cc))

(define-structure rep.vm.disassembler

    (export disassemble
	    disassemble-1)

    (open rep
	  rep.vm.bytecodes)

  (define-structure-alias disassembler rep.vm.disassembler)

  ;; Lookup table of strings naming instructions
  (define disassembler-opcodes
   [ "slot-ref" nil nil nil nil nil nil nil	; #x00
     "call" nil nil nil nil nil nil nil
     "push" nil nil nil nil nil nil nil	; #x10
     "refg" nil nil nil nil nil nil nil
     "setg" nil nil nil nil nil nil nil	; #x20
     "setn" nil nil nil nil nil nil nil
     "slot-set" nil nil nil nil nil nil nil	; #x30
     "refn" nil nil nil nil nil nil nil
     "ref" "%set" "fluid-ref" "enclose"
     "init-bind" "unbind" "dup" "swap"	; #x40
     "pop" "push\t()" "push\tt" "cons"
     "car" "cdr" "rplaca" "rplacd"
     "nth" "nthcdr" "aset" "aref"
     "length" "bind" "add" "neg" "sub"	; #x50
     "mul" "div" "rem" "lnot" "not" "lor" "land"
     "equal" "eq" "structure-ref" "scm-test"
     "gt" "ge" "lt" "le"		; #x60
     "inc" "dec" "ash" "zerop" "null" "atom" "consp" "listp"
     "numberp" "stringp" "vectorp" "catch"
     "throw" "binderr" "return" "unbindall"	; #x70
     "boundp" "symbolp" "get" "put"
     "errorpro" "signal" "quotient" "reverse"
     "nreverse" "assoc" "assq" "rassoc"
     "rassq" "last" "mapcar" "mapc"	; #x80
     "member" "memq" "delete" "delq"
     "delete-if" "delete-if-not" "copy-sequence" "sequencep"
     "functionp" "special-form-p" "subrp" "eql"
     "lxor" "max" "min" "filter"	; #x90
     "macrop" "bytecodep" "pushi\t0" "pushi\t1"
     "pushi\t2" "pushi\t-1" "pushi\t-2" "pushi\t%d"
     "pushi\t%d" "pushi\t%d" "caar" "cadr"
     "cdar" "cddr" "caddr" "cadddr"	; #xa0
     "caddddr" "cadddddr" "caddddddr" "cadddddddr"
     "floor" "ceiling" "truncate" "round"
     "apply" "forbid" "permit" "exp"
     "log" "sin" "cos" "tan"		; #xb0
     "sqrt" "expt" "swap2" "mod"
     "make-closure" "unbindall-0" "closurep" "pop-all"
     "fluid-set" "fluid-bind" "memql" "num-eq"
     "test-scm" "test-scm-f" "%define" "spec-bind"	; #xc0
     "set" "required-arg" "optional-arg" "rest-arg"
     "not-zero-p" "keyword-arg" "optional-arg*" "keyword-arg*"
     nil nil nil nil nil nil nil nil	; #xd0
     nil nil nil nil nil nil nil nil
     nil nil nil nil nil nil nil nil	; #xe0
     nil nil nil nil nil nil nil nil
     nil nil nil nil nil nil nil nil	; #xf0
     "ejmp\t%d" "jpn\t%d" "jpt\t%d" "jmp\t%d" "jn\t%d" "jt\t%d" "jnp\t%d" "jtp\t%d" ])

  (defun disassemble-1 (code-string consts stream #!optional depth)
    (unless depth (setq depth 0))
    (let
	((i 0)
	 (indent (make-string depth))
	 c arg op)
      (while (< i (length code-string))
	(setq c (aref code-string i))
	(format stream "\n%s%d\t\t" indent i)
	(cond
	 ((< c (bytecode last-with-args))
	  (setq op (logand c #xf8))
	  (cond
	   ((< (logand c #x07) 6)
	    (setq arg (logand c #x07)))
	   ((= (logand c #x07) 6)
	    (setq i (1+ i)
		  arg (aref code-string i)))
	   (t
	    (setq arg (logior (ash (aref code-string (1+ i)) 8)
			      (aref code-string (+ i 2)))
		  i (+ i 2))))
	  (cond
	   ((= op (bytecode call))
	    (format stream "call\t#%d" arg))
	   ((= op (bytecode push))
	    (let
		((argobj (aref consts arg)))
	      (if (or (and (consp argobj) (eq (car argobj) 'byte-code))
		      (bytecodep argobj))
		  (progn
		    (format stream "push\t[%d] bytecode...\n" arg)
		    (disassemble argobj stream (1+ depth)))
		(format stream "push\t[%d] %S" arg (aref consts arg)))))
	   ((= op (bytecode bind))
	    (format stream "bind\t[%d] %S" arg (aref consts arg)))
	   ((= op (bytecode refn))
	    (format stream "refn\t#%d" arg))
	   ((= op (bytecode setn))
	    (format stream "setn\t#%d" arg))
	   ((= op (bytecode slot-ref))
	    (format stream "slot-ref #%d" arg))
	   ((= op (bytecode slot-set))
	    (format stream "slot-set #%d" arg))
	   ((= op (bytecode refg))
	    (format stream "refg\t[%d] %S" arg (aref consts arg)))
	   ((= op (bytecode setg))
	    (format stream "setg\t[%d] %S" arg (aref consts arg)))))
	 ((> c (bytecode last-before-jmps))
	  (setq arg (logior (ash (aref code-string (1+ i)) 8)
			    (aref code-string (+ i 2)))
		op c
		i (+ i 2))
	  (format stream (aref disassembler-opcodes op) arg))
	 ((= c (bytecode pushi))
	  (setq arg (aref code-string (1+ i)))
	  (setq i (1+ i))
	  (when (>= arg 128)
	    (setq arg (- (- 256 arg))))
	  (format stream (aref disassembler-opcodes c) arg))
	 ((or (= c (bytecode pushi-pair-neg))
	      (= c (bytecode pushi-pair-pos)))
	  (setq arg (logior (ash (aref code-string (1+ i)) 8)
			    (aref code-string (+ i 2))))
	  (setq i (+ i 2))
	  (when (= c (bytecode pushi-pair-neg))
	    (setq arg (- arg)))
	  (format stream (aref disassembler-opcodes c) arg))
	 (t
	  (if (setq op (aref disassembler-opcodes c))
	      (write stream op)
	    (format stream "<unknown opcode %d>" c))))
	(setq i (1+ i)))
      (write stream ?\n)))

  ;;;###autoload
  (defun disassemble (arg #!optional stream depth)
    "Dissasembles ARG, with output to STREAM, or the *disassembly* buffer."
    (interactive "aFunction to disassemble:")
    (let
	(code-string consts stack
	 (print-escape t))
      (unless stream
	(if (featurep 'jade)
	    (progn
	      (declare (bound open-buffer clear-buffer goto-other-view
			      goto-buffer insert start-of-buffer goto))
	      (setq stream (open-buffer "*disassembly*"))
	      (clear-buffer stream)
	      (goto-other-view)
	      (goto-buffer stream)
	      (insert "\n" stream)
	      (goto (start-of-buffer))
	      (setq stream (cons stream t)))
	  (setq stream standard-output)))
      (unless depth
	(setq depth 0))
      (when (zerop depth)
	(if (symbolp arg)
	    (progn
	      (format stream "Disassembly of function %s:\n\n" arg)
	      (setq arg (symbol-value arg)))
	  (format stream "Disassembly of %S:\n\n" arg)))
      (when (closurep arg)
	(setq arg (closure-function arg)))
      (cond
       ((and (consp arg) (eq (car arg) 'run-byte-code))
	(setq code-string (nth 1 arg)
	      consts (nth 2 arg)
	      stack (nth 3 arg)))
       (t
	(setq code-string (aref arg 0)
	      consts (aref arg 1))
	(when (zerop depth)
	  (let ((spec (and (> (length arg) 4) (aref arg 4)))
		(doc (and (> (length arg) 3) (aref arg 3))))
	    (when spec
	      (format stream "Interactive spec: %S\n" spec))
	    (when doc
	      (format stream "Doc string: %S\n" doc)))
	  (setq stack (aref arg 2)))))
      (when (zerop depth)
	(format stream "%d bytes, %d constants, and (%d,%d,%d) stack slots\n"
		(length code-string) (length consts)
		(logand stack #x3ff) (logand (ash stack -10) #x3ff)
		(ash stack -20)))
      (disassemble-1 code-string consts stream depth))))
