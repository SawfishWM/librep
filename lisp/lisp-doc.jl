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

(structure (export describe-lambda-list
		   describe-value
		   documentation
		   document-var
		   add-documentation)
  (open rep)

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
      (if (eq (car value) 'lambda)
	  (format standard-output "\(%s%s\)\n" (or name value)
		  (describe-lambda-list
		   (nth (if (eq (car value) 'macro) 2 1) value)))
	(format standard-output "%s\n" (or name value)))))


;;; Accessing doc strings

  (defun documentation (symbol &optional value)
    "Returns the documentation-string for SYMBOL."
    (catch 'exit
      (let
	  (doc dbm)
	;; First check for in-core documentation
	(when (setq doc (get symbol 'documentation))
	  (throw 'exit doc))
	(when (boundp symbol)
	  (setq doc (or value (symbol-value symbol t)))
	  (when (eq 'macro (car doc))
	    (setq doc (cdr doc)))
	  (when (and (closurep doc) (eq (car (closure-function doc)) 'lambda))
	    (setq doc (nth 2 (closure-function doc)))
	    (when (stringp doc)
	      (throw 'exit doc))))
	;; Then for doc strings in the databases
	(require 'gdbm)
	(mapc (lambda (file)
		(setq dbm (gdbm-open file 'read))
		(when dbm
		  (unwind-protect
		      (setq doc (gdbm-fetch dbm (symbol-name symbol)))
		    (gdbm-close dbm))
		  (when doc
		    (throw 'exit doc))))
	      documentation-files))))
  
  (defun document-var (symbol doc-string)
    "Sets the `documentation' property of SYMBOL to DOC-STRING."
    (put symbol 'documentation doc-string)
    symbol)

  (defun add-documentation (symbol string)
    "Adds a documentation string STRING to the file of such strings."
    (require 'gdbm)
    (let
	((dbm (gdbm-open documentation-file 'append)))
      (unwind-protect
	  (gdbm-store dbm (symbol-name symbol) string 'replace)
	(gdbm-close dbm)))))
