;;;; lisp-doc.jl -- Accessing LISP doc strings
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

(define-structure rep.lang.doc

    (export describe-lambda-list
	    describe-value
	    doc-file-ref
	    doc-file-set
	    documentation
	    document-var
	    add-documentation
	    add-documentation-params)

    (open rep)

  (define-structure-alias lisp-doc rep.lang.doc)

  (defun describe-lambda-list (lambda-list)
    (let ((output (make-string-output-stream)))
      ;; Print the arg list (one at a time)
      (while (consp lambda-list)
	(let ((arg-name (symbol-name (car lambda-list))))
	  ;; Unless the argument starts with a `&' print it in capitals
	  (unless (= (aref arg-name 0) ?&)
	    (setq arg-name (string-upcase arg-name)))
	  (format output " %s" arg-name))
	(setq lambda-list (cdr lambda-list)))
      (when (and lambda-list (symbolp lambda-list))
	(format output " . %s" (string-upcase (symbol-name lambda-list))))
      (get-output-stream-string output)))

  (defun describe-value (value &optional name)
    "Print to standard-output a description of the lisp data object VALUE. If
NAME is non-nil, then it should be the symbol that is associated with VALUE."
    (let*
	((type (cond
		((special-form-p value)
		 "Special Form")
		((macrop value)
		 ;; macros are stored as `(macro . FUNCTION)'
		 (setq value (cdr value))
		 "Macro")
		((subrp value)
		 "Built-in Function")
		((closurep value)
		 "Function")
		(t
		 "Variable"))))
      (when (closurep value)
	(setq value (closure-function value)))
      ;; Check if it's been compiled.
      (when (bytecodep value)
	(setq type (concat "Compiled " type)))
      (format standard-output "%s: " type)
      (let ((arg-doc (cond ((eq (car value) 'lambda)
			    (describe-lambda-list (cadr value)))
			   ((bytecodep value)
			    (cond ((listp (aref value 0))
				   (describe-lambda-list (aref value 0)))
				  ((and name (symbolp name))
				   (doc-file-ref
				    (concat 0 (symbol-name name)))))))))
	(format standard-output
		"\(%s%s\)\n" (or name value) (or arg-doc "")))))


;;; GDBM doc-file access

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
    (let ((db (gdbm-open documentation-file 'append)))
      (when db
	(unwind-protect
	    (gdbm-store db key value 'replace)
	  (gdbm-close db)))))


;;; Accessing doc strings

  (defun documentation (symbol &optional value)
    "Returns the documentation-string for SYMBOL."
    (catch 'exit
      (let (doc)
	;; First check for in-core documentation
	(when (setq doc (get symbol 'documentation))
	  (throw 'exit doc))
	(when (boundp symbol)
	  (setq doc (or value (and (boundp symbol) (symbol-value symbol))))
	  (when (eq 'macro (car doc))
	    (setq doc (cdr doc)))
	  (when (and (closurep doc) (eq (car (closure-function doc)) 'lambda))
	    (setq doc (nth 2 (closure-function doc)))
	    (when (stringp doc)
	      (throw 'exit doc))))
	;; Then for doc strings in the databases
	(doc-file-ref (symbol-name symbol)))))
  
  (defun document-var (symbol doc-string)
    "Sets the `documentation' property of SYMBOL to DOC-STRING."
    (put symbol 'documentation doc-string)
    symbol)

  (defun add-documentation (symbol string)
    "Adds a documentation string STRING to the file of such strings."
    (doc-file-set (symbol-name symbol) string))

  (defun add-documentation-params (name param-list)
    "Records that function NAME (a symbol) has argument list PARAM-LIST."
    (doc-file-set (concat 0 (symbol-name name))
		  (describe-lambda-list param-list))))
