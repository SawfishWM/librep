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

(define-structure rep.lang.debugger ()

    (open rep
	  rep.system
	  rep.structures
	  rep.regexp
	  rep.io.files
	  rep.io.readline)

  (define emit-emacs-tokens (get-command-line-option "--emacs-debug"))

;;; the form stopped on

  (define obj (make-fluid))
  (define depth (make-fluid))
  (define frame-id (make-fluid))
  (define bottom-frame-id (make-fluid))

  (define last-printed-frame)

  (define last (make-fluid))

;;; stack frame accessors

  (define (stack-frame-function x) (nth 0 x))
  (define (stack-frame-args x) (nth 1 x))
  (define (stack-frame-current-form x) (nth 2 x))
  (define (stack-frame-environment x) (nth 3 x))
  (define (stack-frame-structure x) (nth 4 x))

;;; the debugger repl

  (defun debug-rep ()
    (let ((print-escape t))
      (when (fluid obj)
	(print-form)
	(print-emacs-form (fluid obj)))
      (while t
	(let
	    ((input (readline (format nil "rep-db> ")))
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
		 (setq next-last do-up)
		 (do-up))
		((string-match "^\\s*d" input)
		 (setq next-last do-down)
		 (do-down))
		((string-match "^\\s*p\\w*\\s+" input)
		 (condition-case data
		     (format standard-error "%S\n"
			     (eval-in-frame
			      (read-from-string
			       (substring input (match-end)))))
		   (error (default-error-handler (car data) (cdr data)))))
		((string-match "^\\s*b" input)
		 (print-backtrace))
		((string-match "^\\s*f" input)
		 (setq last-printed-frame t)
		 (print-frame (fluid frame-id))
		 (if (fluid obj)
		     (progn
		       (print-form)
		       (print-emacs-form (fluid obj)))
		   (print-emacs-frame (fluid frame-id))))
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

  (defun print-frame (id)
    (let ((frame (stack-frame-ref id)))
      (if (null frame)
	  (format standard-error "#%-3d #undefined\n" id)
	(unless (equal frame last-printed-frame)
	  (let ((fun (stack-frame-function frame))
		(args (stack-frame-args frame))
		(location (lexical-origin (stack-frame-current-form frame))))
	    (if (null fun)
		(format standard-error "#%-3d #undefined\n" id)
	      (format standard-error "#%-3d %s %S%s\n" id
		      (or (cond ((closurep fun) (closure-name fun))
				((subrp fun) (subr-name fun))
				((eq (car fun) 'lambda)
				 (list 'lambda (cadr fun) '...))) fun)
		      (if (or (eq fun run-byte-code)
			      (eq args #undefined))
			  '...
			args)
		      (if location
			  (format nil " at %s:%d"
				  (file-name-nondirectory (car location))
				  (cdr location))
			""))))))
      (setq last-printed-frame frame)))

  (defun print-backtrace ()
    (do ((i (fluid bottom-frame-id) (1- i)))
	((< i 0))
      (print-frame i)))

  (defun print-form ()
    (let* ((form (if (= (fluid frame-id) (fluid bottom-frame-id))
		     (fluid obj)
		   (stack-frame-current-form
		    (stack-frame-ref (fluid frame-id)))))
	   (location (lexical-origin form)))
      (if location
	  (format standard-error "%d:\t%S\n" (cdr location) form)
	(format standard-error "\t%S\n" form))))

  (defun print-emacs-form (form)
    (when emit-emacs-tokens
      (let ((location (lexical-origin form)))
	(when location
	  (format standard-error "\032\032%s:%d:\n"
		  (local-file-name (car location)) (cdr location))))))

  (defun print-emacs-frame (id)
    (when emit-emacs-tokens
      (let* ((frame (stack-frame-ref id))
	     (location (and frame (lexical-origin
				   (stack-frame-current-form frame)))))
	(when location
	  (print-emacs-form location)))))

  (defun print-locals ()
    (let ((frame (stack-frame-ref (fluid frame-id))))
      (when frame
	(mapc (lambda (cell)
		(format standard-error "%16s %S\n"
			(symbol-name (cadr cell)) (cddr cell)))
	      (stack-frame-environment frame)))))

  (defun eval-in-frame (form)
    (let ((frame (stack-frame-ref (fluid frame-id))))
      (when frame
	(eval form (stack-frame-structure frame)
	      (stack-frame-environment frame)))))

  (defun entry (debug-obj debug-depth debug-frame-id)
    (catch 'debug
      (let-fluids ((obj debug-obj)
		   (depth debug-depth)
		   (frame-id debug-frame-id)
		   (bottom-frame-id debug-frame-id))
	(print-frame debug-frame-id)
	(debug-rep))))

  (defun exit (debug-val debug-depth debug-frame-id)
    (declare (unused debug-frame-id))
    (unless (eq debug-val #undefined)
      (format standard-error "%s-> %S\n"
	      (make-string debug-depth #\-) debug-val)))

  (defun error-entry (error-list debug-frame-id)
    (default-error-handler (car error-list) (cdr error-list))
    (catch 'debug
      (let-fluids ((frame-id debug-frame-id)
		   (bottom-frame-id debug-frame-id))
	(print-frame debug-frame-id)
	(print-emacs-frame debug-frame-id)
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

  (defun do-up ()
    (when (fluid frame-id)
      (fluid-set frame-id (max 0 (1- (fluid frame-id))))
      (print-frame (fluid frame-id))
      (print-emacs-frame (fluid frame-id))))

  (defun do-down ()
    (when (fluid frame-id)
      (fluid-set frame-id (1+ (fluid frame-id)))
      (print-frame (fluid frame-id))
      (print-emacs-frame (fluid frame-id))))

;;; initialize debug hooks (special variables)

  (setq debug-entry entry)
  (setq debug-exit exit)
  (setq debug-error-entry error-entry))
