;;; rep-debugger.el --- Rep hacks for Emacs' gud.el

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Author: John Harper <jsh@pixelslut.com>
;; Keywords: unix, tools, rep

;; Copyright (C) 1992, 93, 94, 95, 96, 1998 Free Software Foundation, Inc.

;; This file is part of Librep.

;; Librep is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Librep is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Librep; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This file is the perldb portions of gud.el with trivial substitutions
;; to make it work with rep..

(require 'gud)

;; ======================================================================
;; rep functions

;;; History of argument lists passed to rep.
(defvar gud-rep-history nil)

(defun gud-rep-massage-args (file args)
  (cons "--debug" (cons (car args) (cons "--emacs-debug" (cdr args)))))

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defun gud-rep-marker-filter (string)
  (setq gud-marker-acc (concat gud-marker-acc string))
  (let ((output ""))

    ;; Process all the complete markers in this chunk.
    (while (string-match "\032\032\\(\\([a-zA-Z]:\\)?[^:\n]*\\):\\([0-9]*\\):.*\n"
			 gud-marker-acc)
      (setq

       ;; Extract the frame position from the marker.
       gud-last-frame
       (cons (substring gud-marker-acc (match-beginning 1) (match-end 1))
	     (string-to-int (substring gud-marker-acc
				       (match-beginning 3)
				       (match-end 3))))

       ;; Append any text before the marker to the output we're going
       ;; to return - we don't include the marker in this text.
       output (concat output
		      (substring gud-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text.
       gud-marker-acc (substring gud-marker-acc (match-end 0))))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "\032.*\\'" gud-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output.
	  (setq output (concat output (substring gud-marker-acc
						 0 (match-beginning 0))))

	  ;; Everything after, we save, to combine with later input.
	  (setq gud-marker-acc
		(substring gud-marker-acc (match-beginning 0))))

      (setq output (concat output gud-marker-acc)
	    gud-marker-acc ""))

    output))

(defun gud-rep-find-file (f)
  (save-excursion
    (let ((buf (find-file-noselect f)))
      (set-buffer buf)
      (gud-make-debug-menu)
      buf)))

(defcustom gud-rep-command-name "rep"
  "File name for executing rep."
  :type 'string
  :group 'gud)

;;;###autoload
(defun rep-debugger (command-line)
  "Run the rep debugger on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (read-from-minibuffer "Run rep debugger (like this): "
			       (if (consp gud-rep-history)
				   (car gud-rep-history)
				 (concat gud-rep-command-name
					 " "
					 (buffer-file-name)
					 " "))
			       nil nil
			       '(gud-rep-history . 1))))

  (gud-common-init command-line 'gud-rep-massage-args
		   'gud-rep-marker-filter 'gud-rep-find-file)

;  (gud-def gud-break  "b %l"         "\C-b" "Set breakpoint at current line.")
;  (gud-def gud-remove "d %l"         "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "s"            "\C-s" "Step one source line with display.")
  (gud-def gud-next   "n"            "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "c"            "\C-r" "Continue with display.")
;  (gud-def gud-finish "finish"       "\C-f" "Finish executing current function.")
  (gud-def gud-up     "u %p"        "<" "Up N stack frames (numeric arg).")
  (gud-def gud-down   "down %p"      ">" "Down N stack frames (numeric arg).")
  (gud-def gud-print  "p %e"           "\C-p" "Evaluate perl expression at point.")

  (setq comint-prompt-regexp "^rep-db> ")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'rep-debugger-mode-hook))

(provide 'rep-debugger)

;; rep-debugger.el ends here
