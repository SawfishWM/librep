#| debug.jl -- Lisp debugger (well, single-stepper anyway)

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

;; XXX extend this to support the structure inspection meta-commands
;; of the top-level repl

(define-structure debug (export)
    (open rep structure-internals readline)

;;; the form stopped on

  (define obj (make-fluid))
  (define depth (make-fluid))
  (define frame-pointer (make-fluid))

  (define last (make-fluid))

;;; the debugger repl

  (defun debug-rep ()
    (let
	((print-escape t))
      (when (fluid depth)
	(format standard-error "<%d> %S\n" (fluid depth) (fluid obj)))
      (while t
	(let
	    ((input (readline "rep-db? "))
	     next-last)
	  (cond ((string-match "^\\s*n" input)
		 (fluid-set last do-next)
		 (do-next))
		((string-match "^\\s*s" input)
		 (fluid-set last do-step)
		 (do-step))
		((string-match "^\\s*c" input)
		 (fluid-set last do-continue)
		 (do-continue))
		((string-match "^\\s*r\\w*\\s+" input)
		 (do-set-result
		  (eval (read-from-string (substring input (match-end))))))
		((string-match "^\\s*u" input)
		 (fluid-set last do-up)
		 (do-up))
		((string-match "^\\s*d" input)
		 (fluid-set last do-down)
		 (do-down))
		((string-match "^\\s*p\\w*\\s+" input)
		 (condition-case data
		     (format standard-error "%S\n"
			     (eval-in-frame
			      (read-from-string
			       (substring input (match-end)))))
		   (error (default-error-handler (car data) (cdr data)))))
		((string-match "^\\s*b" input)
		 (do-backtrace))
		((string-match "^\\s*f" input)
		 (format standard-error "<%d> %S\n" (fluid depth) (fluid obj)))
		((string-match "^\\s*l" input)
		 (print-locals))
		((string-match "^\\s*$" input)
		 (if (fluid last)
		     (progn
		       ((fluid last))
		       (setq next-last (fluid last)))
		   (write standard-error "Nothing to repeat\n")))
		(t
		 (write standard-error "\
commands: `n[ext]', `s[tep]', `c[ontinue]', `r[eturn] FORM',
          `p[rint] FORM', `b[acktrace]', `f[orm], `l[ocals]''\n")))
	  (fluid-set last next-last)))))

;;; local functions

  (defun print-locals ()
    (let
	;; (ENV . STRUCTURE)
	((data (debug-frame-environment (fluid frame-pointer))))
      (when data
	(mapc (lambda (cell)
		(format standard-error "%16s %S\n"
			(symbol-name (car cell)) (cdr cell)))
	      (car data)))))

  (defun eval-in-frame (form)
    (let
	;; (ENV . STRUCTURE)
	((data (debug-frame-environment (fluid frame-pointer))))
      (when data
	(%eval-in-structure form (cdr data) (car data)))))

  (defun entry (debug-obj debug-depth debug-frame-pointer)
    (catch 'debug
      (fluid-let ((obj debug-obj)
		  (depth debug-depth)
		  (frame-pointer debug-frame-pointer))
	(debug-rep))))

  (defun exit (debug-val debug-depth debug-frame-pointer)
    (format standard-error "<%d> => %S\n" debug-depth debug-val))

  (defun error-entry (error-list debug-frame-pointer)
    (default-error-handler (car error-list) (cdr error-list))
    (catch 'debug
      (fluid-let ((frame-pointer debug-frame-pointer))
	(debug-rep)
	nil)))

  (defun do-step ()
    (throw 'debug (cons 1 (fluid obj))))

  (defun do-set-result (value)
    (throw 'debug (cons 4 value)))

  (defun do-next ()
    (throw 'debug (cons 2 (fluid obj))))

  (defun do-continue ()
    (throw 'debug (cons 3 (fluid obj))))

  (defun do-backtrace ()
    (backtrace standard-output)
    (write standard-output #\newline))

  (defun do-up ()
    (when (fluid frame-pointer)
      (let ((fp (debug-inner-frame (fluid frame-pointer))))
	(when fp
	  (fluid-set frame-pointer fp)))))

  (defun do-down ()
    (when (fluid frame-pointer)
      (let ((fp (debug-outer-frame (fluid frame-pointer))))
	(when fp
	  (fluid-set frame-pointer fp)))))

;;; initialize debug hooks (special variables)

  (setq debug-entry entry)
  (setq debug-exit exit)
  (setq debug-error-entry error-entry))
