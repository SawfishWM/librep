#! /bin/sh
exec rep "$0" "$@"
!#

;; rep-xgettext.jl -- extract i18n strings from lisp scripts
;; $Id$

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

(defvar *current-file* nil)
(defvar *found-strings* nil)		;list of (STRING FILES...)

(defun parse (filename)
  (let
      ((file (open-file filename 'read)))
    (when file
      (unwind-protect
	  (condition-case nil
	      (let
		  ((*current-file* filename))
		(while t
		  (let
		      ((form (read file))
		       (*current-file* filename))
		    (scan form))))
	    (end-of-stream))
	(close-file file)))))

(defmacro scan-list (x)
  `(when (consp ,x)
     (mapc scan ,x)))

(defun scan (form)
  (if (and (consp form) (eq (car form) '_) (stringp (nth 1 form)))
      (output (nth 1 form))

    (when (and (car form) (macrop (car form)))
      (setq form (macroexpand form)))
    (cond ((eq (car form) 'quote))

	  ((memq (car form) '(setq setq-default))
	   (let
	       ((tem (cdr form)))
	     (while (cdr tem)
	       (scan (nth 1 tem))
	       (setq tem (nthcdr 2 tem)))))

	  ((eq (car form) 'let)
	   (mapc (lambda (x)
		   (when (consp x)
		     (scan-list (cdr x)))) (nth 1 form))
	   (scan-list (nthcdr 2 form)))

	  ((eq (car form) 'let*)
	   (mapc (lambda (x)
		   (when (consp x)
		     (scan-list (cdr x)))) (nth 1 form))
	   (scan-list (nthcdr 2 form)))

	  ((eq (car form) 'function)
	   (scan (cdr form)))

	  ((eq (car form) 'cond)
	   (mapc (lambda (f)
		   (scan-list f)) (cdr form)))

	  ((eq (car form) 'condition-case)
	   (mapc (lambda (handler)
		   (scan-list (cdr handler))) (nthcdr 2 form)))

	  ((eq (car (car form)) 'lambda)
	   ;; an inline lambda expression
	   (scan (car form)))

	  ((memq (car form) '(defun defmacro defsubst))
	   (let
	       ((doc (nth 3 form)))
	     (when (stringp doc)
	       (output doc))
	     (scan-list (nthcdr 2 form))))

	  ((memq (car form) '(defvar defconst defcustom))
	   (let
	       ((doc (nth 3 form)))
	     (when (stringp doc)
	       (output doc))
	     (scan-list (nthcdr 2 form))))

	  ((memq (car form) '(defgroup))
	   (let
	       ((doc (nth 2 form)))
	     (when (stringp doc)
	       (output doc))
	     (scan-list (nthcdr 2 form))))

	  ((consp form)
	   (scan-list form)))))

(defun output (string)
  (let
      ((cell (assoc string *found-strings*)))
    (if cell
	(unless (member *current-file* (cdr cell))
	  (rplacd cell (cons *current-file* (cdr cell))))
      (setq *found-strings* (cons (list string *current-file*)
				  *found-strings*)))))

(defun output-all ()
  (mapc (lambda (x)
	  (let ((string (car x))
		(files (cdr x)))
	    (mapc (lambda (f)
		    (format standard-output "#: %s\n" f)) files)
	    (let*
		((print-escape 'newlines)
		 (out (format nil "%S" string))
		 (point 0))
	      (while (and (< point (length out))
			  (string-match "\\\\n" out point))
		(setq out (concat (substring out 0 (match-start)) "\\n\"\n\""
				  (substring out (match-end))))
		(setq point (+ (match-end) 3)))
	      (format standard-output "msgid %s\nmsgstr \"\"\n\n" out))))
	(nreverse *found-strings*)))


;; entry point

(format standard-output "\
# SOME DESCRIPTIVE TITLE.
# Copyright (C) YEAR Free Software Foundation, Inc.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
#, fuzzy
msgid \"\"
msgstr \"\"
\"Project-Id-Version: PACKAGE VERSION\\n\"
\"POT-Creation-Date: %s\\n\"
\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"
\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"
\"Language-Team: LANGUAGE <LL@li.org>\\n\"
\"MIME-Version: 1.0\\n\"
\"Content-Type: text/plain; charset=CHARSET\\n\"
\"Content-Transfer-Encoding: ENCODING\\n\"\n\n"
	(current-time-string nil "%Y-%m-%d %H:%M%z"))

(while command-line-args
  (let
      ((file (car command-line-args)))
    (setq command-line-args (cdr command-line-args))
    (parse file)))
(output-all)

;; Local variables:
;; major-mode: lisp-mode
;; End:
