#| xgettext.jl -- helper functions for writing xgettext programs

   $Id$

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure rep.i18n.xgettext

    (export current-file current-module
	    set-included-definers set-helper
	    register scan scan-list scan-file
	    output-c-file output-pot-file)

    (open rep
	  rep.io.files
	  rep.regexp
	  rep.system)

  (define current-file (make-fluid))
  (define current-module (make-fluid))

  (define found-strings (make-fluid))

  (define included-definers (make-fluid t))
  (define helper (make-fluid))

  (define (set-included-definers lst) (fluid-set included-definers lst))
  (define (set-helper h) (fluid-set helper h))

  (define (register string)
    (let ((cell (assoc string (fluid found-strings))))
      (if cell
	  (unless (member (fluid current-file) (cdr cell))
	    (rplacd cell (cons (fluid current-file) (cdr cell))))
	(fluid-set found-strings (cons (list string (fluid current-file))
				       (fluid found-strings))))))

  (define (includedp name)
    (or (eq (fluid included-definers) t)
	(memq name (fluid included-definers))))

  (define (scan form)

    (if (and (consp form) (eq (car form) '_) (stringp (nth 1 form)))
	(register (nth 1 form))

      (when (and (car form) (macrop (car form)))
	(setq form (macroexpand form)))

      (when (consp form)
	(case (car form)
	  ((quote))

	  ((setq setq-default %define)
	   (do ((tem (cdr form) (cddr tem)))
	       ((null (cdr tem)))
	     (scan (cadr tem))))

	  ((let let* letrec let-fluids)
	   (setq form (cdr form))
	   (when (symbolp (car form))
	     (setq form (cdr form)))
	   (let loop ((vars (car form)))
	     (when vars
	       (scan-list (cdar vars))
	       (loop (cdr vars))))
	   (scan-list (cdr form)))

	  ((function) (scan (cdr form)))

	  ((cond)
	   (mapc (lambda (f)
		   (scan-list f)) (cdr form)))

	  ((lambda) (scan-list (cddr form)))

	  ((defun defmacro defsubst defvar defconst)
	   (when (includedp (car form))
	     (let ((doc (nth 3 form)))
	       (when (stringp doc)
		 (register doc))))
	   (if (memq (car form) '(defun defmacro defsubst))
	       (scan-list (nthcdr 3 form))
	     (scan-list (nthcdr 2 form))))

	  ((define-structure)
	   (let-fluids ((current-module (nth 1 form)))
	     (scan-list (nthcdr 4 form))))

	  ((structure)
	   (scan-list (nthcdr 3 form)))

	  (t (if (fluid helper)
		 ((fluid helper) form)
	       (scan-list form)))))))

  (define (scan-list body)
    (mapc scan body))

  (define (scan-file filename)
    (let ((file (open-file filename 'read)))
      (when file
	(unwind-protect
	    (condition-case nil
		(let-fluids ((current-file filename))
		  (while t
		    (let ((form (read file)))
		      (scan form))))
	      (end-of-stream))
	  (close-file file)))))

  (defun output-strings (c-mode)
    (mapc (lambda (x)
	    (let ((string (car x))
		  (files (cdr x)))
	      (mapc (lambda (f)
		      (format standard-output "%s %s %s\n"
			      (if c-mode "  /*" "#:")
			      f (if c-mode "*/" ""))) files)
	    (let* ((print-escape 'newlines)
		   (out (format nil "%S" string))
		   (point 0))
	      (if c-mode
		  (format standard-output "  _(%s);\n\n" out)
		(while (and (< point (length out))
			    (string-match "\\\\n" out point))
		  (setq out (concat (substring out 0 (match-start)) "\\n\"\n\""
				    (substring out (match-end))))
		  (setq point (+ (match-end) 3)))
		(format standard-output "msgid %s\nmsgstr \"\"\n\n" out)))))
	(nreverse (fluid found-strings))))

  (define (output-c-file)
    (write standard-output "\
/* SOME DESCRIPTIVE TITLE */
/* This file is intended to be parsed by xgettext.
 * It is not intended to be compiled.
 */

#if 0
void some_function_name() {\n\n")
    (output-strings t)
    (write standard-output "\
}
#endif\n"))

  (define (output-pot-file)
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
    (output-strings nil)))
