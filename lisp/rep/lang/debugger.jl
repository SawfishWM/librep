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

(defvar debug-buffer (open-buffer "*debugger*")
  "Buffer to use for the Lisp debugger.")

(defvar debug-ctrl-c-keymap 
  (bind-keys (make-sparse-keymap)
    "Ctrl-s" 'debug-step
    "Ctrl-i" '(debug-set-result nil)
    "Ctrl-n" 'debug-next
    "Ctrl-c" 'debug-continue
    "Ctrl-r" 'debug-continue
    "Ctrl-b" '(debug-backtrace 2)
    "Ctrl-x" 'debug-set-result)
  "Keymap for debugger's ctrl-c prefix.")

(defun debug-mode ()
  "Debug Mode:\n
The major mode controlling the Lisp debugger. Commands available within
the debugger are:\n
\\{debug-ctrl-c-keymap,Ctrl-c}"
  (setq ctrl-c-keymap debug-ctrl-c-keymap
	major-mode 'debug-mode
	mode-name "Debug"))

(with-buffer debug-buffer
  (debug-mode)
  (split-line)
  (insert "::Lisp Debugger::\n\n"))



;; Form stopped on
(defvar debug-obj nil)

;;;###autoload
(defun debug-entry (debug-obj debug-depth)
  (with-buffer debug-buffer
    (goto (start-of-line (end-of-buffer)))
    (let
	((print-escape 'newlines))
      (format debug-buffer "%s%S\n" (make-string (* 2 debug-depth)) debug-obj))
    (goto-glyph (forward-line 1 (indent-pos (forward-line -1))))
    (catch 'debug
      (recursive-edit))))

(defun debug-exit (debug-val debug-depth)
  (with-buffer debug-buffer
    (goto (start-of-line (end-of-buffer)))
    (format debug-buffer "%s=> %S\n" (make-string (* 2 debug-depth)) debug-val)))

;;;###autoload
(defun debug-error-entry (error-list)
  (with-buffer debug-buffer
    (goto (start-of-line (end-of-buffer)))
    (format debug-buffer "*** Error: %s: %S\n" (unless (get (car error-list) 'error-message) (car error-list)) (cdr error-list))
    (catch 'debug
      (recursive-edit)
      nil)))

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
  (goto (start-of-line (end-of-buffer)))
  (let
      ((old-pos (cursor-pos)))
    (backtrace debug-buffer)
    (delete-area old-pos (forward-line (1+ depth) old-pos))
    (split-line)))
