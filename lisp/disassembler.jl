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
(defvar dis-opcode-vector
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
   "numberp" "stringp" "vectorp" "catch-kludge" "throw" "unwind-pro" "<obsolete>" "fboundp"	; 0x70
   "boundp" "symbolp" "get" "put" "error-pro" "signal" "return" "reverse"
   "nreverse" "assoc" "assq" "rassoc" "rassq" "last" "mapcar" "mapc" ; 0x80
   "member" "memq" "delete" "delq" "delete-if" "delete-if-not" "copy-sequence" "sequencep"
   "functionp" "special-form-p" "subrp" "eql" "lxor" nil nil nil ; 0x90
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil	 ; 0xa0
   nil nil nil nil nil nil nil nil
   "set-current-buffer" "bind-buffer" "current-buffer" "bufferp" "markp" "windowp" "bind-window" "viewp"
   "bind-view" "current-view" "swap2" "mod" nil nil nil nil
   nil nil nil nil nil nil nil nil	 ; 0xc0
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil	 ; 0xd0
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil	 ; 0xe0
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil	 ; 0xf0
   nil "jpn\t%d" "jpt\t%d" "jmp\t%d" "jn\t%d" "jt\t%d" "jnp\t%d" "jtp\t%d" ])

;;;###autoload
(defun disassemble-fun (fun &optional stream)
  "Disassembles the byte code form which is the function value of FUN. If
STREAM is given all output goes to that stream."
  (interactive "aFunction to disassemble:")
  (when (symbolp fun)
    (setq fun (symbol-function fun)))
  (if (eq (car fun) 'macro)
      (setq fun (nthcdr 3 fun))
    (setq fun (nthcdr 2 fun)))
  (when (or (stringp (car fun)) (numberp (car fun)))
    ;; doc-string
    (setq fun (cdr fun)))
  (when (and (consp (car fun)) (eq (car (car fun)) 'interactive))
    ;; interactive decl
    (setq fun (cdr fun)))
  (disassemble (car fun)) stream)

;; Disassembles the FORM, output goes to STREAM
(defun disassemble (form &optional stream)
  (let
      ((code-string (nth 1 form))
       (consts (nth 2 form))
       (i 0)
       c arg op)
    (unless stream
      (setq stream standard-output))
    (while (setq c (aref code-string i))
      (format stream "\n%d:\t" i)
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
	    (if (and (consp argobj) (eq (car argobj) 'jade-byte-code))
		(progn
		  (format stream "push\t[%d] %S\n<byte-code" arg argobj)
		  (disassemble argobj stream)
		  (write stream "\n>"))
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
	(format stream (aref dis-opcode-vector op) arg))
       (t
	(if (setq op (aref dis-opcode-vector c))
	    (write stream op)
	  (format stream "<unknown opcode %d>" c))))
      (setq i (1+ i)))
    t))
