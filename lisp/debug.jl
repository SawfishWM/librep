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

(provide 'debug)

;; Form stopped on
(defvar debug-obj nil)

;;;###autoload
(defun debug-entry (debug-obj debug-depth)
  (let
      ((print-escape 'newlines))
    (format standard-output "%s%S\n"
	    (make-string (* 2 debug-depth)) debug-obj))
    (catch 'debug
      (recursive-edit)))

(defun debug-exit (debug-val debug-depth)
  (format standard-output "%s=> %S\n"
	  (make-string (* 2 debug-depth)) debug-val))

;;;###autoload
(defun debug-error-entry (error-list)
  (format standard-output "*** Error: %s: %S\n"
	  (or (get (car error-list) 'error-message)
	      (car error-list)) (cdr error-list))
  (debug-backtrace 3)
  (catch 'debug
    (recursive-edit)
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
