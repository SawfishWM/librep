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

(provide 'lisp-doc)

(defun apropos-output (symbols use-function)
  (let
      ((separator (make-string 72 ?-)))
    (mapc (lambda (sym)
	    (write standard-output separator)
	    (if use-function
		(describe-function-1 sym)
	      (describe-variable-1 sym))
	    (format standard-output "%s\n\n"
		    (or (documentation sym) "Undocumented"))) symbols)))

;;;###autoload
(defun apropos-function (regexp &optional all-functions)
  (interactive "sApropos functions:\nP")
  (format standard-output "Apropos %s `%s':\n\n"
	  (if all-functions "function" "command") regexp)
  (apropos-output (apropos regexp (if (or all-functions
					  (not (boundp 'commandp)))
				      (lambda (s)
					(and (boundp s)
					     (functionp (symbol-value s))))
				    commandp)) t))

;;;###autoload
(defun apropos-variable (regexp)
  (interactive "sApropos variables:")
  (format standard-output "Apropos variable `%s':\n" regexp)
  (apropos-output (apropos regexp boundp) nil))

(defun describe-function-1 (fun)
  (let*
      ((fval (if (symbolp fun)
		 (symbol-value fun)
	       fun))
       (type (cond
	      ((special-form-p fval)
	       "Special Form")
	      ((macrop fval)
	       "Macro")
	      ((subrp fval)
	       "Built-in Function")
	      (t
	       "Function"))))
    (when (closurep fval)
      (setq fval (closure-function fval)))
    ;; Check if it's been compiled.
    (when (or (bytecodep fval)
	      (and (consp fval) (assq 'jade-byte-code fval)))
      (setq type (concat "Compiled " type)))
    (format standard-output "\n%s: %s\n\n" type fun)
    (when (boundp fun)
      (when (or (consp fval) (bytecodep fval))
	;; A Lisp function or macro, print its argument spec.
	(let
	    ((lambda-list (if (consp fval)
			      (nth (if (eq (car fval) 'macro) 2 1) fval)
			    (aref fval 0))))
	  (prin1 fun)
	  ;; Print the arg list (one at a time)
	  (while lambda-list
	    (let
		((arg-name (symbol-name (car lambda-list))))
	      ;; Unless the argument starts with a `&' print it in capitals
	      (unless (= (aref arg-name 0) ?&)
		(setq arg-name (translate-string (copy-sequence arg-name)
						 upcase-table)))
	      (format standard-output " %s" arg-name))
	    (setq lambda-list (cdr lambda-list)))
	  (format standard-output "\n\n"))))))
  
;;;###autoload
(defun describe-function (fun)
  (interactive "aFunction:")
  "Display the documentation of a function, macro or special-form."
  (let
      ((doc (documentation fun)))
    (describe-function-1 fun)
    (write standard-output (or doc "Undocumented."))
    (write standard-output "\n")))

(defun describe-variable-1 (var)
  (format standard-output
	  "\n%s: %s\nCurrent value: %S\n\n"
	  (if (const-variable-p var) "Constant" "Variable")
	  (symbol-name var) (symbol-value var t)))

;;;###autoload
(defun describe-variable (var)
  (interactive "vVariable:")
  (let
      ((doc (documentation var)))
    (describe-variable-1 var)
    (format standard-output "%s\n" (or doc "Undocumented."))))


;; Accessing doc strings

;;;###autoload
(defun documentation (symbol)
  "Returns the documentation-string for SYMBOL."
  (catch 'exit
    (let
	(doc dbm)
      ;; First check for in-core documentation
      (when (setq doc (get symbol 'documentation))
	(throw 'exit doc))
      (when (boundp symbol)
	(setq doc (symbol-value symbol))
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
  
;;;###autoload
(defun document-var (symbol doc-string)
  "Sets the `documentation' property of SYMBOL to DOC-STRING."
  (put symbol 'documentation doc-string)
  symbol)

;;;###autoload
(defun add-documentation (symbol string)
  "Adds a documentation string STRING to the file of such strings."
  (require 'gdbm)
  (let
      ((dbm (gdbm-open documentation-file 'append)))
    (gdbm-store dbm (symbol-name symbol) string 'replace)
    (gdbm-close dbm)))
