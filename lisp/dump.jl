;; dump.jl -- dumping of Lisp forms to C code
;;  $Id$

;;  Copyright (C) 1998,1999 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of librep.

;; librep is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; librep is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with librep; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'dump)

;; Commentary:

;; Lisp modules generally consist almost wholly of constant
;; definitions, that is, definitions whose effect can be known upon
;; static analysis. For example, the defun special form always sets the
;; function cell of its first argument to the specified function, both
;; name and definition are static.

;; These constant definitions can therefore be moved out of the Lisp
;; file and into the text segment of the program binary. In general,
;; only non-constant forms are left in the Lisp file, it can be loaded
;; as normal with exactly the same effect.

;; The function dump-lisp-forms scans a file of (compiled) Lisp code
;; for the constant definitions, resolving them into the different
;; types of constant (cons, vectors, symbols, bytecodes, etc), along
;; with a unique label for each object. For compound objects the
;; sub-objects are treated in the same manner, and the reference to the
;; sub-object is replaced by its label, thereby creating a kind of
;; dependency, or more accurately, reference graph.

;; By outputting the collected information as C code, the constant
;; objects can be generated in the text segment of the binary, with
;; very little modification needed to the Lisp system itself.

;; Currently, the following forms are recognised as [possibly]
;; containing static definitions: defun, defsubst, defmacro, defvar,
;; defconst, put, require, provide.

;; Note that the data-layout of the main Lisp data types (integers,
;; cons cells, symbols and vectors) is hard coded at the end of this
;; file.

;; [ I think this "dumping" is usually called "freezing"? if you
;; reinvent something you accidentally give it a new name.. :-) ]


;; Configuration

(defvar dump-verbosely t
  "When t output the reference graph as text to a file dump-verbosely-file
for the entire set of dumped files.")

(defvar dump-verbosely-file "dump.out"
  "File for verbose output.")

;; Special vars
(defvar dump-non-constant-forms nil)
(defvar dump-string-constants nil)
(defvar dump-cons-constants nil)
(defvar dump-symbol-constants nil)
(defvar dump-vector-constants nil)
(defvar dump-bytecode-constants nil)
(defvar dump-funarg-constants nil)
(defvar dump-feature-constants nil)


;; Top level entrypoints

;; Call something like:

;;	rep --batch dump -f dump-batch [OPTIONS...] SRCS... -q

;; where OPTIONS may be any of:

;;	-o OUTPUT-FILE			Specify the output file

(defun dump-batch ()
  (let
      (files output)
    (while (and (consp command-line-args)
		(not (equal (car command-line-args) "-q")))
      (cond
       ((equal (car command-line-args) "-o")
	(setq output (car (cdr command-line-args))
	      command-line-args (cdr command-line-args)))
       (t
	(setq files (cons (car command-line-args) files))))
      (setq command-line-args (cdr command-line-args)))
    (setq files (nreverse files))
    (format (stdout-file) "Dumping %S to %S\n" files output)
    (dump files output)))

(defun find-lisp-file (filename)
  (catch 'out
    (mapc (lambda (dir)
	    (let
		((full (expand-file-name (concat filename ".jlc") dir)))
	      (when (file-exists-p full)
		(throw 'out full)))) load-path)
    (error "Can't find file: %s\n" filename)))

(defun dump (file-list output-file)
  "Dump each compiled Lisp file named in FILE-LIST, to the C file OUTPUT-FILE.
Note that each input file will be loaded from the lisp-lib-directory with
.jlc as its suffix."
  (let
      (dump-vector-constants
       dump-string-constants
       dump-symbol-constants
       dump-cons-constants
       dump-bytecode-constants
       dump-funarg-constants
       dump-non-constant-forms
       dump-feature-constants
       standard-output input-stream
       file-full-name
       (list-head file-list))
    (when (setq standard-output (open-file output-file 'write))
      (unwind-protect
	  (progn
	    (dump-output-comment output-file)
	    (dump-output-comment (format nil "From: %S" file-list))
	    ;; Ensure that `nil' gets added as the first symbol, note that
	    ;; since the lists are consed up bottom first, this means that
	    ;; it's actually the last entry in C code for the symbols
	    (dump-add-constant nil)
	    (dump-add-constant t)
	    (while (consp list-head)
	      (setq file-full-name (find-lisp-file (car list-head)))
	      (unless (setq input-stream (open-file file-full-name 'read))
		(error "Dump: can't open %s" file-full-name))
	      (unwind-protect
		  (let
		      (func form)
		    (condition-case nil
			(while (setq form (read input-stream))
			  (if (setq func (get (car form) 'dump-function))
			      (func form)
			    (dump-add-non-constant form)))
		      (end-of-stream)))
		(close-file input-stream))
	      (setq list-head (cdr list-head)))

	    ;; Output the features variable
	    (dump-defvar `(defvar features
			    ',(copy-sequence dump-feature-constants)))

	    ;; reverse list of forms to evaluate
	    (setq dump-non-constant-forms
		  (cons 'progn (nreverse dump-non-constant-forms)))
	    (setq dump-non-constant-forms
		  (dump-get-label (dump-add-constant dump-non-constant-forms)))

	    ;; For all symbols with a plist property, add it as a constant
	    (dump-fix-plists dump-symbol-constants)

	    ;; Generate the C code
	    (dump-output-code)
	    (when dump-verbosely
	      ;; And the debugging output
	      (dump-output-readably file-list)))
	(close-file standard-output)))))

;; Output a LIST of objects to STREAM. Each object printed will be
;; preceded by INDENT (default is two spaces)
(defun dump-output-list (lst stream &optional indent)
  (while (consp lst)
    (format stream "\n%s%S" (or indent "  ") (car lst))
    (setq lst (cdr lst))))

;; Dump all dump-X-constants lists to the file dump-verbosely-file. The
;; definitions came from the list of files INPUT-FILES
(defun dump-output-readably (input-files)
  (let
      ((file (open-file dump-verbosely-file 'write))
       (print-escape t))
    (when file
      (unwind-protect
	  (progn
	    (format file ";; Dump output from %S" input-files)
	    (write file "\n\n;; Features\n")
	    (dump-output-list dump-feature-constants file)
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
	    (write file "\n\n;; Funarg constants\n")
	    (dump-output-list dump-funarg-constants file)
	    (write file "\n\n;; Non constant forms\n")
	    (dump-output-list dump-non-constant-forms file)
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
    ;; object. The LABEL will be used in the resulting C code
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
    (eq (car form) 'quote))
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
    ;; only quote
    (nth 1 form))))

;; Return the label or integer that is the constant value of FORM (given
;; that FORM is a constant)
(defun dump-constant-value (form)
  (setq form (dump-get-constant form))
  (if (integerp form)
      form
    (dump-get-label (dump-add-constant form))))

(defun dump-add-funarg (fun name)
  (let*
      ((env (dump-add-constant t))
       (special-env (dump-add-constant (cons nil t)))
       (fh-env (dump-add-constant t))
       (object (vector (dump-get-label (dump-add-constant fun))
		       (dump-get-label (dump-add-constant name))
		       (dump-get-label env)
		       (dump-get-label special-env)
		       (dump-get-label fh-env)))
       (cell (assoc object dump-funarg-constants)))
    (unless cell
      (setq cell (dump-new-cell object))
      (setq dump-funarg-constants (cons cell dump-funarg-constants)))
    cell))

;; For all symbol cells in LIST that have a plist property, add its value
;; as a constant
(defun dump-fix-plists (lst)
  (mapc (lambda (x &aux plist)
	  (when (setq plist (dump-has-state-p x 'plist))
	    (rplacd plist (dump-get-label (dump-add-constant (cdr plist))))))
	lst))


;; Handlers for supported top-level forms

(defun dump-defun (form)
  (let
      ((sym (dump-add-constant (nth 1 form)))
       (func (nth 2 form)))
    (when (consp func)
      (setq func (cons 'lambda (nthcdr 2 form))))
    (dump-add-state sym 'value (dump-get-label
				(dump-add-funarg func (symbol-name
						       (nth 1 form)))))
    (dump-add-state sym 'defined t)))

(defun dump-defsubst (form)
  (let
      ((sym (dump-add-constant (nth 1 form)))
       (func (nth 2 form)))
    (when (consp func)
      (setq func (cons 'lambda (nthcdr 2 form))))
    (dump-add-state sym 'value (dump-get-label
				(dump-add-funarg func (symbol-name
						       (nth 1 form)))))
    (dump-add-state sym 'defined t)
    (dump-state-put sym 'compile-inline t)))

(defun dump-defmacro (form)
  (let
      ((sym (dump-add-constant (nth 1 form)))
       (func (nth 2 form))
       funarg)
    (when (consp func)
      (setq func (cons 'lambda (nthcdr 2 func))))
    (setq funarg (dump-get-label (dump-add-funarg
				  func (symbol-name (nth 1 form)))))
    (setq func (dump-add-constant (cons 'macro nil)))
    (rplacd (dump-get-object func) funarg)
    (dump-add-state sym 'value (dump-get-label func))
    (dump-add-state sym 'defined t)))

(defun dump-defvar (form)
  (let
      ((sym (nth 1 form))
       (value (nth 2 form)))
    (if (not (dump-constant-p value))
	(dump-add-non-constant form)
      (setq sym (dump-add-constant sym))
      (dump-add-state sym 'value (dump-constant-value value))
      (dump-add-state sym 'defined t)
      (dump-add-state sym 'special t)
      (when (nth 3 form)
	(dump-state-put sym 'documentation (nth 3 form))))))

(defun dump-defconst (form)
  (let
      ((sym (nth 1 form))
       (value (nth 2 form)))
    (if (not (dump-constant-p value))
	(dump-add-non-constant form)
      (setq sym (dump-add-constant sym))
      (dump-add-state sym 'value (dump-constant-value value))
      (dump-add-state sym 'defined t)
      (dump-add-state sym 'special t)
      (dump-add-state sym 'constant t)
      (when (nth 3 form)
	(dump-state-put sym 'documentation (nth 3 form))))))

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

(defun dump-provide (form)
  (if (dump-constant-p (nth 1 form))
      (let
	  ((const (dump-get-constant (nth 1 form))))
	(unless (memq const dump-feature-constants)
	  (setq dump-feature-constants (cons const dump-feature-constants))))
    (dump-add-non-constant form)))

(defun dump-require (form)
  (if (dump-constant-p (nth 1 form))
      (let
	  ((const (dump-get-constant (nth 1 form))))
	(unless (memq const dump-feature-constants)
	  (dump-add-non-constant form)))
    (dump-add-non-constant form)))

(put 'defun 'dump-function dump-defun)
(put 'defsubst 'dump-function dump-defsubst)
(put 'defmacro 'dump-function dump-defmacro)
(put 'defvar 'dump-function dump-defvar)
(put 'defconst 'dump-function dump-defconst)
(put 'put 'dump-function dump-put)
(put 'provide 'dump-function dump-provide)
(put 'require 'dump-function dump-require)


;; Code output

;; We make the gruesome assumption that sizeof(void*) == sizeof(repv).
;; (This is checked for validity at run-time.) It seems that compilers
;; don't think pointers are constants, and so won't put them in the
;; read-only section of the library. repv's (which are integers) don't
;; have that problem..

(defmacro @ (&rest args)
  (list* 'format 'standard-output args))

(defun dump-output-structs ()
  (@ "\ntypedef struct {\n")
  (@ "  repv car;\n")
  (@ "  repv next;\n")
  (@ "  repv data;\n")
  (@ "} const_rep_string;\n")
  (@ "\ntypedef struct {\n")
  (@ "  repv car;\n")
  (@ "  repv next;\n")
  (@ "  repv fun;\n")
  (@ "  repv name;\n")
  (@ "  repv env;\n")
  (@ "  repv special_env;\n")
  (@ "  repv fh_env;\n")
  (@ "} const_rep_funarg;\n"))

(defun dump-output-object (obj)
  (cond ((integerp obj)
	 (format nil "%d" (logior (ash obj 2) 2)))
	(obj
	 (format nil "rep_VAL(&%s)" obj))
	(t
	 "rep_NULL")))

;; Output a comment TEXT
(defun dump-output-comment (text)
  (@ "/* %s */\n" text))

(defun dump-output-string-protos (head)
  (@ "\n")
  (mapc (lambda (cell)
	  (@ "extern const rep_string %s;\n" (dump-get-label cell))) head))

;; Output all string cells in the list HEAD
(defun dump-output-strings (head)
  (@ "\n\f\n/* Constant strings */\n\n")
  (mapc (lambda (cell)
	  (let
	      ((string (dump-get-object cell))
	       (label (dump-get-label cell)))
	    (@ "const u_char %s_data[] = %S;\n" label string)
	    (@ "const const_rep_string %s = {\n" label)
	    (@ "  0x%x,\n" (logior (ash (length string) 8) 0x45))
	    (@ "  0,\n")
	    (@ "  rep_VAL(%s_data)\n" label)
	    (@ "};\n\n"))) head))
    
(defun dump-output-cons-protos (head)
  (@ "\n")
  (mapc (lambda (cell)
	  (@ "extern const rep_cons %s;\n" (dump-get-label cell))) head))

;; Output all cons cells in the list HEAD
(defun dump-output-cons (head)
  (@ "\n\f\n/* Constant cons cells */\n\n")
  (mapc (lambda (cell)
	  (let
	      ((pair (dump-get-object cell))
	       (label (dump-get-label cell)))
	    (@ "const rep_cons %s = {\n" label)
	    (@ "  %s, %s\n"
	       (dump-output-object (car pair)) (dump-output-object (cdr pair)))
	    (@ "};\n"))) head))

(defun dump-output-symbol-protos (head)
  (@ "\n")
  (mapc (lambda (cell)
	  (@ "extern rep_symbol %s;\n" (dump-get-label cell))) head))

;; Output all symbol cells in the list HEAD
(defun dump-output-symbols (head)
  (@ "\n\f\n/* Symbols */\n\n")
  (mapc (lambda (cell)
	  (let*
	      ((symbol (dump-get-object cell))
	       (label (dump-get-label cell))
	       (is-constant (dump-get-state cell 'constant))
	       (is-defined (dump-get-state cell 'defined))
	       (is-special (dump-get-state cell 'special))
	       (name (dump-get-state cell 'name))
	       (value (dump-get-state cell 'value))
	       (plist (dump-get-state cell 'plist)))
	    (@ "/* %s */\n" symbol)
	    (@ "rep_symbol %s = {\n" label)
	    (@ "  0x%x,\n" (logior 0x41
				   (if is-constant (ash 1 (+ 8 0)) 0)
				   (if is-defined  (ash 1 (+ 8 7)) 0)
				   (if is-special  (ash 1 (+ 8 4)) 0)))
	    (@ "  0,\n")
	    (@ "  %s,\n" (dump-output-object name))
	    (@ "  %s,\n" (dump-output-object value))
	    (@ "  %s\n" (dump-output-object plist))
	    (@ "};\n\n"))) head))

(let*
    ((done nil)

     (output-struct
      (lambda (size)
	(unless (memq size done)
	  (@ "struct rep_vector_%d {\n" size)
	  (@ "  repv car; repv next; repv array[%d];\n" size)
	  (@ "};\n")
	  (setq done (cons size done))))))

  (defun dump-output-vector-protos (head)
    (@ "\n")
    (mapc (lambda (cell)
	    (let
		((len (length (dump-get-object cell))))
	      (output-struct len)
	      (@ "extern const struct rep_vector_%d %s;\n"
		 len (dump-get-label cell)))) head))

  ;; Output to vector cells in the list HEAD, TYPE should be vector or bytecode
  (defun dump-output-vectors (head type)
    (let
	((type-value (if (eq type 'vector) 0x43 0x47)))
      (@ "\n\f\n/* Constant %ss */\n\n" type)
      (mapc (lambda (cell)
	      (let
		  ((vec (dump-get-object cell))
		   (label (dump-get-label cell))
		   (i 0))
		(output-struct (length vec))
		(@ "const struct rep_vector_%d %s = {\n"
		   (length vec) label)
		(@ "  0x%x,\n" (logior (ash (length vec) 8) type-value))
		(@ "  0,\n")
		(@ "  {\n")
		(while (< i (length vec))
		  (@ "    %s%s\n" (dump-output-object (aref vec i))
		     (if (/= i (1- (length vec))) "," ""))
		  (setq i (1+ i)))
		(@ "  }\n};\n\n"))) head))))

(defun dump-output-funarg-protos (head)
  (@ "\n")
  (mapc (lambda (cell)
	  (@ "extern const const_rep_funarg %s;\n"
	     (dump-get-label cell))) head))

(defun dump-output-funargs (head)
  (@ "\n\f\n/* Constant funargs */\n\n")
  (mapc (lambda (cell)
	  (let
	      ((funarg (dump-get-object cell))
	       (label (dump-get-label cell)))
	    (@ "const const_rep_funarg %s = {\n" label)
	    (@ "  0x5f, 0,\n")
	    (@ "  rep_VAL(&%s),\n" (aref funarg 0))
	    (@ "  rep_VAL(&%s),\n" (aref funarg 1))
	    (@ "  rep_VAL(&%s),\n" (aref funarg 2))
	    (@ "  rep_VAL(&%s),\n" (aref funarg 3))
	    (@ "  rep_VAL(&%s)\n" (aref funarg 4))
	    (@ "};\n\n"))) head))

;; Output all code
(defun dump-output-code ()
  (let
      ((print-escape t))

    (@ "\n#include <rep.h>\n#include <assert.h>\n")

    (dump-output-structs)

    ;;(dump-output-string-protos dump-string-constants)
    (dump-output-cons-protos dump-cons-constants)
    (dump-output-symbol-protos dump-symbol-constants)
    (dump-output-vector-protos dump-vector-constants)
    (dump-output-vector-protos dump-bytecode-constants)
    (dump-output-funarg-protos dump-funarg-constants)

    ;; Data itself
    (dump-output-strings dump-string-constants)
    (dump-output-cons dump-cons-constants)
    (dump-output-symbols dump-symbol-constants)
    (dump-output-vectors dump-vector-constants 'vector)
    (dump-output-vectors dump-bytecode-constants 'bytecode)
    (dump-output-funargs dump-funarg-constants)

    (@ "\f\n/* Initialisation */\n\n")
    (@ "repv\nrep_dl_init (void)\n{\n")
    (@ "  assert (sizeof (repv) == sizeof (void *));\n")
    (@ "  assert (sizeof (const_rep_string) == sizeof (rep_string));\n")
    (@ "  assert (sizeof (const_rep_funarg) == sizeof (rep_funarg));\n\n")
    (@ "  rep_dumped_cons_start = (rep_cons *) &%s;\n"
       (dump-get-label (car dump-cons-constants)))
    (@ "  rep_dumped_cons_end = (rep_cons *) (&%s)+1;\n"
       (dump-get-label (last dump-cons-constants)))
    (@ "  rep_dumped_symbols_start = (rep_symbol *) &%s;\n"
       (dump-get-label (car dump-symbol-constants)))
    (@ "  rep_dumped_symbols_end = (rep_symbol *) (&%s)+1;\n"
       (dump-get-label (last dump-symbol-constants)))
    (@ "  rep_dumped_non_constants = rep_VAL(&%s);\n" dump-non-constant-forms)
    (@ "  return Qt;\n")
    (@ "}\n")))
