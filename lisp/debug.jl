;;;; debug.jl -- Lisp debugger (well, single-stepper anyway)
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

(require 'readline)

;; Form stopped on
(defvar debug-obj nil)
(defvar debug-depth nil)
(defvar debug-frame-pointer nil)

(defvar debug-last nil)

(defun debug-rep ()
  (let
      ((print-escape t))
    (format standard-error "<%d> %S\n" debug-depth debug-obj)
    (while t
      (let
	  ((input (readline "rep-db? "))
	   next-last)
	(cond ((string-match "^\\s*n" input)
	       (setq debug-last debug-next)
	       (debug-next))
	      ((string-match "^\\s*s" input)
	       (setq debug-last debug-step)
	       (debug-step))
	      ((string-match "^\\s*c" input)
	       (setq debug-last debug-continue)
	       (debug-continue))
	      ((string-match "^\\s*r\\w*\\s+" input)
	       (debug-set-result
		(eval (read-from-string (substring input (match-end))))))
	      ((string-match "^\\s*p\\w*\\s+" input)
	       (format standard-error "%S\n"
		       (debug-eval-in-frame
			(read-from-string (substring input (match-end))))))
	      ((string-match "^\\s*b" input)
	       (debug-backtrace 0))
	      ((string-match "^\\s*f" input)
	       (format standard-error "<%d> %S\n" debug-depth debug-obj))
	      ((string-match "^\\s*l" input)
	       (debug-print-locals))
	      ((string-match "^\\s*$" input)
	       (if debug-last
		   (progn
		     (debug-last)
		     (setq next-last debug-last))
		 (write standard-error "Nothing to repeat\n")))
	      (t
	       (write standard-error "\
commands: `n[ext]', `s[tep]', `c[ontinue]', `r[eturn] FORM',
          `p[rint] FORM', `b[acktrace]', `f[orm], `l[ocals]''\n")))
	(setq debug-last next-last)))))

(defun debug-print-locals ()
  (let
      ;; (ENV SPECIAL-ENV FH-ENV)
      ((data (debug-frame-environment debug-frame-pointer)))
    (when data
      (mapc (lambda (cell)
	      (format standard-error "%16s %S\n" (car cell) (cdr cell)))
	    (nth 0 data)))))

(defun debug-eval-in-frame (form)
  (let
      ;; (ENV SPECIAL-ENV FH-ENV)
      ((data (debug-frame-environment debug-frame-pointer)))
    (when data
      (eval `(save-environment
	      (set-special-environment ',(nth 1 data))
	      (set-file-handler-environment ',(nth 2 data))
	      (set-environment ',(nth 0 data))
	      ,form)))))

;;;###autoload
(defun debug-entry (debug-obj debug-depth debug-frame-pointer)
  (catch 'debug
    (debug-rep)))

(defun debug-exit (debug-val debug-depth debug-frame-pointer)
  (format standard-error "<%d> => %S\n" debug-depth debug-val))

;;;###autoload
(defun debug-error-entry (error-list debug-frame-pointer)
  (format standard-error "*** Error: %s: %S\n"
	  (or (get (car error-list) 'error-message)
	      (car error-list)) (cdr error-list))
  (debug-backtrace 1)
  (catch 'debug
    (debug-rep)
    nil))

(defun debug-step ()
  (interactive)
  (if (boundp 'debug-obj)
      (throw 'debug (cons 1 debug-obj))
    (beep)))

(defun debug-set-result (value)
  (interactive "XEval:")
  (if (boundp 'debug-obj)
      (throw 'debug (cons 4 value))
    (beep)))

(defun debug-next ()
  (interactive)
  (if (boundp 'debug-obj)
      (throw 'debug (cons 2 debug-obj))
    (beep)))

(defun debug-continue ()
  (interactive)
  (cond
   ((boundp 'debug-obj)
    (throw 'debug (cons 3 debug-obj)))
   ((boundp 'error-list)
    (throw 'debug))
   (t
    (beep))))

;; DEPTH is the number of stack frames to discard
(defun debug-backtrace (depth)
  (backtrace standard-output)
  (write standard-output ?\n))
