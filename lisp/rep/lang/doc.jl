#| lisp-doc.jl -- Accessing LISP doc strings

   $Id$

   Copyright (C) 1993, 1994, 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of Librep.

   Librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(declare (unsafe-for-call/cc))

(define-structure rep.lang.doc

    (export describe-lambda-list
	    describe-value
	    doc-file-value-key
	    doc-file-param-key
	    doc-file-ref
	    doc-file-set
	    documentation
	    document-variable
	    add-documentation
	    add-documentation-params)

    (open rep
	  rep.structures)

  (defun describe-lambda-list (lambda-list)
    (let ((output (make-string-output-stream)))
      ;; Print the arg list (one at a time)
      (while (consp lambda-list)
	(let ((arg-name (symbol-name (or (caar lambda-list)
					 (car lambda-list)))))
	  ;; Unless the it's a lambda-list keyword, print in capitals
	  (unless (memq (car lambda-list)
			'(#!optional #!key #!rest &optional &rest))
	    (setq arg-name (string-upcase arg-name)))
	  (format output " %s" arg-name))
	(setq lambda-list (cdr lambda-list)))
      (when (and lambda-list (symbolp lambda-list))
	(format output " . %s" (string-upcase (symbol-name lambda-list))))
      (get-output-stream-string output)))

  (defun describe-value (value #!optional name structure)
    "Print to standard-output a description of the lisp data object VALUE. If
NAME is true, then it should be the symbol that is associated with VALUE."
    (let*
	((type (cond
		((special-form-p value) "Special Form")
		((macrop value)
		 ;; macros are stored as `(macro . FUNCTION)'
		 (setq value (cdr value))
		 "Macro")
		((subrp value) "Native Function")
		((closurep value) "Function")
		(t "Variable"))))
      (when (closurep value)
	(unless structure
	  (let ((tem (closure-structure value)))
	    (when (structure-name tem)
	      (setq structure (structure-name tem)))))
	(setq value (closure-function value)))
      ;; Check if it's been compiled.
      (when (bytecodep value)
	(setq type (concat "Compiled " type)))
      (when (and name structure (not (special-variable-p name))
		 (binding-immutable-p name (get-structure structure)))
	(setq type (concat "Constant " type)))
      (when (and name (special-variable-p name))
	(setq type (concat "Special " type)))
		       
      (format standard-output "%s: " type)
      (let ((arg-doc (cond ((eq (car value) 'lambda)
			    (describe-lambda-list (cadr value)))
			   ((symbolp name)
			    (or (and structure
				     (doc-file-ref (doc-file-param-key
						    name structure)))
				(doc-file-ref (doc-file-param-key name)))))))
	(if arg-doc
	    (format standard-output "\(%s%s\)\n" (or name value) arg-doc)
	  (format standard-output "%s\n" (or name value))))))


;;; GDBM doc-file access

  (defun make-key (prefix name #!optional structure)
    (if structure
	(concat prefix (symbol-name structure) #\# (symbol-name name))
      (concat prefix (symbol-name name))))

  (defun doc-file-value-key (name #!optional structure)
    (make-key nil name structure))
  (defun doc-file-param-key (name #!optional structure)
    (make-key 0 name structure))

  (defun doc-file-ref (key)
    (require 'rep.io.db.gdbm)
    (catch 'done
      (mapc (lambda (file)
	      ;; turn off read-locking -- DOC files are normally
	      ;; created before being installed, and reportedly
	      ;; AFS often prevents normal users gaining locks
	      (let ((db (gdbm-open file 'read nil '(no-lock))))
		(when db
		  (unwind-protect
		      (let ((value (gdbm-fetch db key)))
			(when value
			  (throw 'done value)))
		    (gdbm-close db))))) documentation-files)
      nil))

  (defun doc-file-set (key value)
    (require 'rep.io.db.gdbm)
    ;; XXX I'm not convinced that turning off locking is wise..
    (let ((db (gdbm-open documentation-file 'append nil '(no-lock))))
      (when db
	(unwind-protect
	    (gdbm-store db key value 'replace)
	  (gdbm-close db)))))


;;; Accessing doc strings

  (defun documentation-property (#!optional structure)
    (if structure
	(intern (concat "documentation#" (symbol-name structure)))
      'documentation))

  (defun documentation (symbol #!optional structure value)
    "Returns the documentation-string for SYMBOL."
    (catch 'exit
      (when (and (not structure) (closurep value))
	(let ((tem (closure-structure value)))
	  (when (structure-name tem)
	    (setq structure (structure-name tem)))))

      ;; First check for in-core documentation
      (when value
	(let ((tem value))
	  (when (eq 'macro (car tem))
	    (setq tem (cdr tem)))
	  (when (and (closurep tem)
		     (eq (car (closure-function tem)) 'lambda))
	    (setq tem (nth 2 (closure-function tem)))
	    (when (stringp tem)
	      (throw 'exit tem)))))

      (let ((doc (or (and structure (get symbol
					 (documentation-property
					  structure)))
		     (get symbol (documentation-property)))))
	(when doc
	  (throw 'exit doc)))

      ;; Then for doc strings in the databases
      (or (and structure (doc-file-ref
			  (doc-file-value-key symbol structure)))
	  (doc-file-ref (doc-file-value-key symbol)))))

  (defun document-variable (symbol structure doc-string)
    "Sets the documentation property of SYMBOL to DOC-STRING."
    (put symbol (documentation-property structure) doc-string)
    symbol)

  (defun add-documentation (symbol structure string)
    "Adds a documentation string STRING to the file of such strings."
    (doc-file-set (doc-file-value-key symbol structure) string))

  (defun add-documentation-params (name structure param-list)
    "Records that function NAME (a symbol) has argument list PARAM-LIST."
    (doc-file-set (doc-file-param-key name structure)
		  (describe-lambda-list param-list))))
