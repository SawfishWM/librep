;;;; dump.jl -- dumping of Lisp forms to assembler code
;;;  Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
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

(provide 'dump)

;;;;Commentary

;;; Lisp modules generally consist almost wholly of constant
;;; definitions, that is, definitions whose effect can be known upon
;;; static analysis. For example, the defun special form always sets
;;; the function cell of its first argument to the specified function,
;;; both name and definition are static.

;;; These constant definitions can therefore be moved out of the Lisp
;;; file and into the text segment of the program binary. In general,
;;; only non-constant forms are left in the Lisp file, it can be loaded
;;; as normal with exactly the same effect.

;;; The function dump-lisp-forms scans a file of (compiled) Lisp code
;;; for the constant definitions, resolving them into the different
;;; types of constant (cons, vectors, symbols, bytecodes, etc), along
;;; with a unique label for each object. For compound objects the
;;; sub-objects are treated in the same manner, and the reference to
;;; the sub-object is replaced by its label, thereby creating a kind
;;; of dependency, or more accurately, reference graph.

;;; By outputting the collected information as assembler code, the constant
;;; objects can be generated in the text segment of the binary, with very
;;; little modification needed to the Lisp system itself.

;;; Currently, the following forms are recognised as [possibly] containing
;;; static definitions: defun, defmacro, defvar, defconst,
;;; make-variable-buffer-local, put.

(defvar dump-verbosely t
  "When t output the reference graph as text to a file dump-verbosely-file
for the entire set of dumped files.")

(defvar dump-verbosely-file "dump.out"
  "File for verbose output.")

(defvar dump-section-alist '((symbol . data)
			     (string . text)
			     (cons . text)
			     (vector . text)
			     (bytecode . text))
  "List of (TYPE . TEXT-OR-DATA). Specifies which section constant objects
go in, the (read-only) text segment, or the (read-write) data segment.
Don't touch this unless you know what you're doing!")

(defvar dump-asm-format '((value . "\t.long %s\n")
			  (align . "\t.align %s\n")
			  (label . "%s:\n")
			  (string . "\t.asciz %S\n")
			  (text . "\t.text\n")
			  (data . "\t.data\n")
			  (global . "\t.globl %s\n")
			  (comment . "/* %s */\n"))
  "List of (TAG . FORMAT-STRING) specifying the syntax of the target
assembler's various pseudo-operations.")

(defvar dump-inline-strings t
  "Must mirror the INLINE_STATIC_STRINGS definition in lisp.h")

;; Special vars
(defvar dump-non-constant-forms nil)
(defvar dump-string-constants nil)
(defvar dump-cons-constants nil)
(defvar dump-symbol-constants nil)
(defvar dump-vector-constants nil)
(defvar dump-bytecode-constants nil)


;; Top level entrypoints

;; Call Jade something like:
;;
;;	jade -l dump -f dump-batch [OPTIONS...] SRCS... -q
;;
;; where OPTIONS may be any of:
;;
;;	-o OUTPUT-FILE			Specify the output file
;;	--enable-inline-strings		Dump for inlined string constants
;;	--disable-inline-strings	Dump for non-inline strings

(defun dump-batch ()
  (let
      (files output)
    (while (and (consp command-line-args)
		(not (equal (car command-line-args) "-q")))
      (cond
       ((equal (car command-line-args) "-o")
	(setq output (car (cdr command-line-args))
	      command-line-args (cdr command-line-args)))
       ((equal (car command-line-args) "--enable-inline-strings")
	(setq dump-inline-strings t))
       ((equal (car command-line-args) "--disable-inline-strings")
	(setq dump-inline-strings nil))
       (t
	(setq files (cons (car command-line-args) files))))
      (setq command-line-args (cdr command-line-args)))
    (setq files (nreverse files))
    (format (stdout-file) "Dumping %S to %S\n" files output)
    (dump files output)))

(defun dump (file-list output-file)
  "Dump each compiled Lisp file named in FILE-LIST, to the assembler
file OUTPUT-FILE. Note that each input file will be loaded from
the lisp-lib-dir with .jlc as its suffix."
  (let
      (dump-vector-constants
       dump-string-constants
       dump-symbol-constants
       dump-cons-constants
       dump-bytecode-constants
       output-stream input-stream
       file-full-name
       (list-head file-list))
    (when (setq output-stream (open-file output-file "w"))
      (unwind-protect
	  (progn
	    (dump-output-comment
	     output-stream (format nil "%s, dumped %s by %s@%s"
				   output-file (current-time-string)
				   (user-login-name) (system-name)))
	    (dump-output-comment
	     output-stream (format nil "From: %S" file-list))
	    ;; Ensure that `nil' gets added as the first symbol, note that
	    ;; since the lists are consed up bottom first, this means that
	    ;; it's actually the last entry in assembler code for the
	    ;; generated symbols
	    (dump-add-constant nil)
	    (while (consp list-head)
	      (setq file-full-name (file-name-concat lisp-lib-dir
						     (concat (car list-head)
							     ".jlc")))
	      (unless (setq input-stream (open-file file-full-name "r"))
		(error "Dump: can't open %s" file-full-name))
	      (unwind-protect
		  (let
		      (dump-non-constant-forms func)
		    (condition-case nil
			(while (setq form (read input-stream))
			  (if (setq func (get (car form) 'dump-function))
			      (funcall func form)
			    (dump-add-non-constant form)))
		      (end-of-stream))
		    (setq dump-non-constant-forms
			  (nreverse dump-non-constant-forms))
		    (dump-output-non-consts file-full-name
					    (file-name-concat
					     lisp-lib-dir
					     (concat (car list-head) ".jld"))))
		(close-file input-stream))
	      (setq list-head (cdr list-head)))
	    ;; Set the variable dumped-lisp-libraries to the list of
	    ;; files (less directory and suffix) that were dumped.
	    (dump-add-state (dump-add-constant 'dumped-lisp-libraries)
			    'value
			    (dump-get-label
			     (dump-add-constant (copy-sequence file-list))))
	    ;; For all symbols with a plist property, add it as a constant
	    (dump-fix-plists dump-symbol-constants)
	    ;; Generate the assembler code
	    (dump-output-assembler output-stream)
	    (when dump-verbosely
	      ;; And the debugging output
	      (dump-output-readably file-list)))
	(close-file output-stream)))))

;; Output a LIST of objects to STREAM. Each object printed will be
;; preceded by INDENT (default is two spaces)
(defun dump-output-list (list stream &optional indent)
  (while (consp list)
    (format stream "\n%s%S" (or indent "  ") (car list))
    (setq list (cdr list))))

;; Output the list of non-constant forms in free variable
;; dump-non-constant-forms to FILE-NAME. INPUT-FILE is the
;; name of the file they came from
(defun dump-output-non-consts (input-file file-name)
  (let
      ((file (open-file file-name "w")))
    (when file
      (unwind-protect
	  (progn
	    (format file ";; Dumped version of %s\n;; Dumped on %s by %s@%s\n"
		    input-file (current-time-string)
		    (user-login-name) (system-name))
	    (dump-output-list dump-non-constant-forms file "")
	    (write file "\n"))
	(close-file file)))))

;; Dump all dump-X-constants lists to the file dump-verbosely-file. The
;; definitions came from the list of files INPUT-FILES
(defun dump-output-readably (input-files)
  (let
      ((file (open-file dump-verbosely-file "w"))
       (print-escape t))
    (when file
      (unwind-protect
	  (progn
	    (format file ";; Dump output from %S" input-files)
	    (write file "\n\n;; String constants\n")
	    (dump-output-list dump-string-constants file)
	    (write file "\n\n;; Cons constants\n")
	    (dump-output-list dump-cons-constants file)
	    (write file "\n\n;; Symbol constants\n")
	    (dump-output-list dump-symbol-constants file)
	    (write file "\n\n;; Vector constants\n")
	    (dump-output-list dump-vector-constants file)
	    (write file "\n\n;; Bytecode constants\n")
	    (dump-output-list dump-bytecode-constants file)
	    (write file "\n\n;; End\n"))
	(close-file file)))))
	  

;; Structure of cells (constant objects, and their related state)

;; Create a new constant cell
(defmacro dump-new-cell (object &rest state)
  (cons 'list (cons object (cons '(gensym) state))))

;; For CELL, return its constant data object
(defmacro dump-get-object (cell)
  (list 'car cell))

;; Return the symbol whose name is the label of CELL
(defmacro dump-get-label (cell)
  (list 'nth 1 cell))

;; Get the X from the pair (TAG . X) associated with CELL. Returns nil
;; if no such pair exists
(defun dump-get-state (cell tag)
  (when (> (length cell) 2)
    (cdr (assoc tag (nthcdr 2 cell)))))

;; Returns t if a pair (TAG . X) is associated with CELL. Actually
;; returns the pair itself if it exists
(defun dump-has-state-p (cell tag)
  (when (> (length cell) 2)
    (assoc tag (nthcdr 2 cell))))  

;; Add a pair (TAG . VALUE) to be associated with CELL
(defun dump-add-state (cell tag value &aux pair)
  (if (setq pair (assoc tag (nthcdr 2 cell)))
      (rplacd pair value)
    (rplacd (nthcdr 1 cell) (cons (cons tag value) (nthcdr 2 cell)))))

;; Add a pair (PROP . VALUE) to the plist state of CELL. Currently this
;; doesn't handle PROP already being in the list, it just pushes another
;; pair on the head
(defun dump-state-put (cell prop value &aux plist tem)
  (dump-add-state cell 'plist
		  (cons prop (cons value (dump-get-state cell 'plist)))))

;; Add the constant OBJECT, returning the cell representing it
(defun dump-add-constant (object)
  (let
      (cell list-var)
    (cond
     ((stringp object)
      (setq list-var 'dump-string-constants))
     ((vectorp object)
      (setq list-var 'dump-vector-constants))
     ((symbolp object)
      (setq list-var 'dump-symbol-constants))
     ((consp object)
      (setq list-var 'dump-cons-constants))
     ((bytecodep object)
      (setq list-var 'dump-bytecode-constants))
     (t
      (error "Unknown type of constant: %S" object)))

    ;; Walk through all sub-objects of this object that have visible
    ;; constants, replacing all but integers (stored in the pointer)
    ;; with the labels of the objects referred to.
    (cond
     ((or (vectorp object) (bytecodep object))
      (let
	  ((i 0)
	   (size (length object)))
	(while (< i size)
	  (unless (integerp (aref object i))
	    (aset object i (dump-get-label
			    (dump-add-constant (aref object i)))))
	  (setq i (1+ i)))))
     ((consp object)
      (unless (integerp (car object))
	(rplaca object (dump-get-label (dump-add-constant (car object)))))
      (unless (integerp (cdr object))
	(rplacd object (dump-get-label (dump-add-constant (cdr object)))))))

    ;; Get the (OBJECT LABEL STATE...) cell that represents this
    ;; object. The LABEL will be used in the resulting assembler/C code
    ;; as the start of the object. This is done after resolving inner
    ;; constants so that we can reuse objects.
    ;; Any extra state needing to be recorded is appended after the
    ;; LABEL as an alist (e.g. a symbol's value and function-value
    ;; cells)
    (unless (setq cell (assoc object (symbol-value list-var)))
      (setq cell (dump-new-cell object))
      (set list-var (cons cell (symbol-value list-var))))

    ;; If it's a symbol, add its name as a string constant
    (when (symbolp object)
      (dump-add-state
       cell 'name (dump-get-label (dump-add-constant (symbol-name object)))))

    ;; Return the complete cell
    cell))

;; Add the non-constant FORM
(defmacro dump-add-non-constant (form)
  (list 'setq 'dump-non-constant-forms
	(list 'cons form 'dump-non-constant-forms)))

;; Return t if FORM is constant
(defun dump-constant-p (form)
  (cond
   ((or (integerp form) (stringp form)
	(vectorp form) (bytecodep form)
	(eq form t) (eq form nil)))
   ((consp form)
    (memq (car form) '(quote function)))
   ;; What other constant forms have I missed..?
   (t
    nil)))

;; Return the Lisp object that is the value of the constant FORM
(defun dump-get-constant (form)
  (cond
   ((or (integerp form) (stringp form)
	(vectorp form) (bytecodep form)
	(eq form t) (eq form nil))
    ;; Self-evaluating types
    form)
   ((consp form)
    ;; only quote or function
    (nth 1 form))))

;; Return the label or integer that is the constant value of FORM (given
;; that FORM is a constant)
(defun dump-constant-value (form)
  (setq form (dump-get-constant form))
  (if (integerp form)
      form
    (dump-get-label (dump-add-constant form))))

;; For all symbol cells in LIST that have a plist property, add its value
;; as a constant
(defun dump-fix-plists (list)
  (mapc #'(lambda (x &aux plist)
	    (when (setq plist (dump-has-state-p x 'plist))
	      (rplacd plist (dump-get-label (dump-add-constant (cdr plist))))))
	list))


;; Handlers for supported top-level forms

(defun dump-defun (form)
  (let
      ((sym (dump-add-constant (nth 1 form)))
       (func (nth 2 form)))
    (when (consp func)
      (setq func (cons 'lambda (nthcdr 2 form))))
    (dump-add-state sym 'function (dump-get-label (dump-add-constant func)))))

(defun dump-defsubst (form)
  (let
      ((sym (dump-add-constant (nth 1 form)))
       (func (nth 2 form)))
    (when (consp func)
      (setq func (cons 'lambda (nthcdr 2 form))))
    (dump-add-state sym 'function (dump-get-label (dump-add-constant func)))
    (dump-state-put sym 'compile-fun 'comp-compile-inline-function)))  

(defun dump-defmacro (form)
  (let
      ((sym (dump-add-constant (nth 1 form)))
       (func (nth 2 form)))
    (when (consp func)
      (setq func (cons 'macro (cons 'lambda (nthcdr 2 func)))))
    (dump-add-state sym 'function (dump-get-label (dump-add-constant func)))))

(defun dump-defvar (form)
  (let
      ((sym (nth 1 form))
       (value (nth 2 form)))
    (if (not (dump-constant-p value))
	(dump-add-non-constant form)
      (setq sym (dump-add-constant sym))
      (dump-add-state sym 'value (dump-constant-value value))
      (when (nth 3 form)
	(dump-state-put sym 'variable-documentation (nth 3 form))))))

(defun dump-defconst (form)
  (let
      ((sym (nth 1 form))
       (value (nth 2 form)))
    (if (not (dump-constant-p value))
	(dump-add-non-constant form)
      (setq sym (dump-add-constant sym))
      (dump-add-state sym 'value (dump-constant-value value))
      (dump-add-state sym 'constant t)
      (when (nth 3 form)
	(dump-state-put sym 'variable-documentation (nth 3 form))))))

(defun dump-make-variable-buffer-local (form)
  (let
      ((sym (nth 1 form)))
    (if (not (dump-constant-p sym))
	(dump-add-non-constant form)
      (setq sym (dump-add-constant (dump-get-constant sym)))
      (dump-add-state sym 'buffer-local t))))

(defun dump-put (form)
  (let
      ((sym (nth 1 form))
       (prop (nth 2 form))
       (value (nth 3 form)))
    (if (not (and (dump-constant-p sym)
		  (dump-constant-p prop)
		  (dump-constant-p value)))
	(dump-add-non-constant form)
      (setq sym (dump-add-constant (dump-get-constant sym)))
      (dump-state-put sym (dump-get-constant prop)
		      (dump-get-constant value)))))

(put 'defun 'dump-function 'dump-defun)
(put 'defsubst 'dump-function 'dump-defsubst)
(put 'defmacro 'dump-function 'dump-defmacro)
(put 'defvar 'dump-function 'dump-defvar)
(put 'defconst 'dump-function 'dump-defconst)
(put 'make-variable-buffer-local 'dump-function
     'dump-make-variable-buffer-local)
(put 'put 'dump-function 'dump-put)


;; Assembler output

;; Output to STREAM the assembler op TAG, using arguments ARGS
(defmacro dump-output (stream tag &rest args)
  (cons 'format
	(cons 'stream
	      (cons (list 'cdr (list 'assq tag 'dump-asm-format)) args))))

;; Output a comment TEXT to STREAM
(defun dump-output-comment (stream text)
  (dump-output stream 'comment text))

;; Output a directive to align to the next cell boundary
(defmacro dump-output-align-cell (stream)
  (list 'dump-output stream ''align 4))

;; Output a long value representing constant VALUE (i.e. a label or
;; an integer)
(defmacro dump-output-object (stream value)
  (list 'dump-output stream ''value (list 'if (list 'integerp value)
					  (list 'logior (list 'lsh value 2) 2)
					  value)))

;; Return the section that an object of TYPE should be put in, either text
;; or data
(defmacro dump-get-section (type)
  (list 'cdr (list 'assq type 'dump-section-alist)))

;; Output to STREAM all string cells in the list HEAD
(defun dump-output-strings (stream head)
  (let
      (obj)
    (write stream "\n")
    (dump-output stream (dump-get-section 'string))
    (dump-output-align-cell stream)
    (dump-output stream 'global "dumped_strings_start")
    (dump-output stream 'label "dumped_strings_start")
    (while (consp head)
      (dump-output stream 'label (dump-get-label (car head)))
      (setq obj (dump-get-object (car head)))
      (dump-output stream 'value (logior (lsh (length obj) 8) 0x45))
      (if dump-inline-strings
	  (dump-output stream 'string obj)
	(let
	    ((data-label (gensym)))
	  (dump-output stream 'value data-label)
	  (dump-output stream 'label data-label)
	  (dump-output stream 'string obj)))
      (dump-output-align-cell stream)
      (setq head (cdr head)))
    (dump-output stream 'global "dumped_strings_end")
    (dump-output stream 'label "dumped_strings_end")))
    
;; Output to STREAM all cons cells in the list HEAD
(defun dump-output-cons (stream head)
  (let
      (obj)
    (write stream "\n")
    (dump-output stream (dump-get-section 'cons))
    (dump-output-align-cell stream)
    (dump-output stream 'global "dumped_cons_start")
    (dump-output stream 'label "dumped_cons_start")
    (while (consp head)
      (dump-output stream 'label (dump-get-label (car head)))
      (setq obj (dump-get-object (car head)))
      (dump-output-object stream (car obj))
      (dump-output-object stream (cdr obj))
      (setq head (cdr head)))
    (dump-output stream 'global "dumped_cons_end")
    (dump-output stream 'label "dumped_cons_end")))

;; Output to STREAM all symbol cells in the list HEAD
(defun dump-output-symbols (stream head)
  (let
      (cell tem)
    (write stream "\n")
    (dump-output stream (dump-get-section 'symbol))
    (dump-output-align-cell stream)
    (dump-output stream 'global "dumped_symbols_start")
    (dump-output stream 'label "dumped_symbols_start")
    (while (consp head)
      (dump-output stream 'label (dump-get-label (car head)))
      (setq cell (car head))
      (dump-output stream 'value
		   (logior (if (dump-get-state cell 'constant) 0x100 0)
			   (if (dump-get-state cell 'buffer-local) 0x600 0)
			   0x41))
      (dump-output stream 'value 0)
      (dump-output stream 'value (dump-get-state cell 'name))
      (if (dump-has-state-p cell 'value)
	  (dump-output-object stream (dump-get-state cell 'value))
	(dump-output stream 'value 0))
      (if (dump-has-state-p cell 'function)
	  (dump-output-object stream (dump-get-state cell 'function))
	(dump-output stream 'value 0))
      (if (dump-has-state-p cell 'plist)
	  (dump-output-object stream (dump-get-state cell 'plist))
	(dump-output stream 'value 0))
      (setq head (cdr head)))
    (dump-output stream 'global "dumped_symbols_end")
    (dump-output stream 'label "dumped_symbols_end")))

;; Output to STREAM all vector cells in the list HEAD, TYPE should be
;; either vector or bytecode
(defun dump-output-vectors (stream head type)
  (let
      ((type-value (if (eq type 'vector) 0x43 0x47))
       (type-start (if (eq type 'vector)
		       "dumped_vectors_start"
		     "dumped_bytecode_start"))
       (type-end (if (eq type 'vector)
		     "dumped_vectors_end"
		   "dumped_bytecode_end"))
       obj i len)
    (write stream "\n")
    (dump-output stream (dump-get-section type))
    (dump-output-align-cell stream)
    (dump-output stream 'global type-start)
    (dump-output stream 'label type-start)
    (while (consp head)
      (dump-output stream 'label (dump-get-label (car head)))
      (setq obj (dump-get-object (car head))
	    len (length obj)
	    i 0)
      (dump-output stream 'value (logior (lsh len 8) type-value))
      (dump-output stream 'value 0)
      (while (< i len)
	(dump-output-object stream (aref obj i))
	(setq i (1+ i)))
      (setq head (cdr head)))
    (dump-output stream 'global type-end)
    (dump-output stream 'label type-end)))

;; Output all assembler code to STREAM
(defun dump-output-assembler (stream)
  (let
      ((print-escape t))
    ;; Prelude
    (write stream "\n\n")
    (dump-output stream 'text)
    (dump-output stream 'global "dumped_text_start")
    (dump-output stream 'label "dumped_text_start")
    (dump-output stream 'data)
    (dump-output stream 'global "dumped_data_start")
    (dump-output stream 'label "dumped_data_start")
    (write stream "\n\n")

    ;; Data itself
    (dump-output-strings stream dump-string-constants)
    (dump-output-cons stream dump-cons-constants)
    (dump-output-symbols stream dump-symbol-constants)
    (dump-output-vectors stream dump-vector-constants 'vector)
    (dump-output-vectors stream dump-bytecode-constants 'bytecode)

    ;; Postlude?
    (write stream "\n\n")
    (dump-output stream 'text)
    (dump-output stream 'global "dumped_text_end")
    (dump-output stream 'label "dumped_text_end")
    (dump-output stream 'data)
    (dump-output stream 'global "dumped_data_end")
    (dump-output stream 'label "dumped_data_end")))
