;;;; disassembler.jl -- Disassembles compiled Lisp functions
;;;  Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
;;;  $Id$

;;; This file is part of Jade.

;;; Jade is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; Jade is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with Jade; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; need this for the opcode constants
(require 'bytecodes)
(provide 'disassembler)

;; Lookup table of strings naming instructions
(defvar disassembler-opcodes
 [ nil nil nil nil nil nil nil nil	 ; 0x00
   "call" nil nil nil nil nil nil nil
   "push" nil nil nil nil nil nil nil	 ; 0x10
   "refq" nil nil nil nil nil nil nil
   "setq" nil nil nil nil nil nil nil	 ; 0x20
   "list" nil nil nil nil nil nil nil
   "bind" nil nil nil nil nil nil nil	 ; 0x30
   nil nil nil nil nil nil nil nil
   "ref" "set" "fref" "fset" "init-bind" "unbind" "dup" "swap"	; 0x40
   "pop" "push\tnil" "push\tt" "cons" "car" "cdr" "rplaca" "rplacd"
   "nth" "nthcdr" "aset" "aref" "length" "eval" "add" "neg" "sub"	; 0x50
   "mul" "div" "rem" "lnot" "not" "lor" "land"
   "equal" "eq" "num-eq" "num-not-eq" "gt" "ge" "lt" "le"	; 0x60
   "inc" "dec" "lsh" "zerop" "null" "atom" "consp" "listp"
   "numberp" "stringp" "vectorp" "catch" "throw" "binderr" nil "fboundp"	; 0x70
   "boundp" "symbolp" "get" "put" "errorpro" "signal" nil "reverse"
   "nreverse" "assoc" "assq" "rassoc" "rassq" "last" "mapcar" "mapc" ; 0x80
   "member" "memq" "delete" "delq" "delete-if" "delete-if-not" "copy-sequence" "sequencep"
   "functionp" "special-form-p" "subrp" "eql" "lxor" "max" "min" "filter" ; 0x90
   "macrop" "bytecodep" "pushi\t0" "pushi\t1" "pushi\t2" "pushi\t-1" "pushi\t-2" "pushi\t%d"
   "pushi\t%d" nil nil nil nil nil nil nil	 ; 0xa0
   nil nil nil nil nil nil nil nil
   "bindobj" nil nil nil nil nil nil nil	 ; 0xb0
   nil nil "swap2" "mod" nil nil nil nil
   nil nil nil nil nil nil nil nil	 ; 0xc0
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil	 ; 0xd0
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil	 ; 0xe0
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil	 ; 0xf0
   "ejmp\t%d" "jpn\t%d" "jpt\t%d" "jmp\t%d" "jn\t%d" "jt\t%d" "jnp\t%d" "jtp\t%d" ])

;;;###autoload
(defun disassemble (arg &optional stream depth)
  "Dissasembles ARG, with output to STREAM, or the *disassembly* buffer."
  (interactive "aFunction to disassemble:")
  (let
      (code-string consts stack
       (print-escape 'newlines))
    (unless stream
      (if (featurep 'jade)
	  (progn
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
	    (setq arg (symbol-function arg)))
	(format stream "Disassembly of form %S:\n\n" arg)))
    (cond
     ((and (consp arg) (eq (car arg) 'jade-byte-code))
      (setq code-string (nth 1 arg)
	    consts (nth 2 arg)
	    stack (nth 3 arg)))
     (t
      (setq code-string (aref arg 1)
	    consts (aref arg 2))
      (when (zerop depth)
	(format stream "Arguments: %S\nInteractive spec: %S\nDoc string: %S\n"
		(aref arg 0)
		(and (> (length arg) 5)
		     (aref arg 5))
		(and (> (length arg) 4)
		     (aref arg 4)))
	(if (zerop (logand (aref arg 3) 0x10000))
	    ;; Not a macro
	    (setq stack (aref arg 3))
	  (format stream "[This is a macro definition]\n")
	  (setq stack (logand (aref arg 3) 0xffff))))))
    (when (zerop depth)
      (format stream "[Uses %d code bytes, %d constants, and %d stack slots]\n
Disassembly:\n"
	      (length code-string) (length consts) stack))
    (let
	((i 0)
	 (indent (make-string depth))
	 c arg op)
      (while (< i (length code-string))
	(setq c (aref code-string i))
	(format stream "\n%s%d\t\t" indent i)
	(cond
	 ((< c op-last-with-args)
	  (setq op (logand c 0xf8))
	  (cond
	   ((< (logand c 0x07) 6)
	    (setq arg (logand c 0x07)))
	   ((= (logand c 0x07) 6)
	    (setq i (1+ i)
		  arg (aref code-string i)))
	   (t
	    (setq arg (logior (lsh (aref code-string (1+ i)) 8)
			      (aref code-string (+ i 2)))
		  i (+ i 2))))
	  (cond
	   ((= op op-call)
	    (format stream "call\t#%d" arg))
	   ((= op op-push)
	    (let
		((argobj (aref consts arg)))
	      (if (or (and (consp argobj) (eq (car argobj) 'jade-byte-code))
		      (bytecodep argobj))
		  (progn
		    (format stream "push\t[%d] %S\n\n%sbytecode:"
			    arg argobj indent)
		    (disassemble argobj stream (1+ depth)))
		(format stream "push\t[%d] %S" arg (aref consts arg)))))
	   ((= op op-refq)
	    (format stream "refq\t[%d] %S" arg (aref consts arg)))
	   ((= op op-setq)
	    (format stream "setq\t[%d] %S" arg (aref consts arg)))
	   ((= op op-list)
	    (format stream "list\t#%d" arg))
	   ((= op op-bind)
	    (format stream "bind\t[%d] %S" arg (aref consts arg)))))
	 ((> c op-last-before-jmps)
	  (setq arg (logior (lsh (aref code-string (1+ i)) 8)
			    (aref code-string (+ i 2)))
		op c
		i (+ i 2))
	  (format stream (aref disassembler-opcodes op) arg))
	 ((= c op-pushi)
	  (setq arg (aref code-string (1+ i)))
	  (setq i (1+ i))
	  (when (>= arg 128)
	    (setq arg (- (- 256 arg))))
	  (format stream (aref disassembler-opcodes c) arg))
	 ((= c op-pushi-pair)
	  (setq arg (logior (lsh (aref code-string (1+ i)) 8)
			    (aref code-string (+ i 2))))
	  (setq i (+ i 2))
	  (when (>= arg 32768)
	    (setq arg (- (- 65536 arg))))
	  (format stream (aref disassembler-opcodes c) arg))
	 (t
	  (if (setq op (aref disassembler-opcodes c))
	      (write stream op)
	    (format stream "<unknown opcode %d>" c))))
	(setq i (1+ i)))
      (write stream ?\n))))
