;;;; tilde.jl -- File handler for tilde expansion
;;;  Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
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

(defun tilde-expand (file-name)
  (if (string-looking-at "~([^/]*)/?" file-name)
      (concat
       (if (/= (match-start 1) (match-end 1))
	   ;; ~USER/...
	   (user-home-directory (substring file-name
					   (match-start 1)
					   (match-end 1)))
	 ;; ~/..
	 (user-home-directory))
       (substring file-name (match-end)))
    file-name))

(defun tilde-file-handler (op &rest args)
  (cond
   ((eq op 'file-name-absolute-p))	;~FOO always absolute
   ((memq op '(expand-file-name file-name-nondirectory file-name-directory
	       file-name-as-directory directory-file-name))
    ;; Functions of a single file name that we leave alone. By re-calling
    ;; OP the standard action will occur since this handler is now
    ;; blocked for OP.
    (apply op args))
   ((memq op '(local-file-name canonical-file-name open-file
	       write-buffer-contents read-file-contents insert-file-contents
	       delete-file file-exists-p file-regular-p file-readable-p
	       file-writable-p file-directory-p file-symlink-p file-owner-p
	       file-nlinks file-size file-modes file-modes-as-string
	       set-file-modes file-modtime directory-files))
    ;; All functions which only have a single file name (their first
    ;; argument). Expand the tilde expression then re-call OP.)
    (apply op (tilde-expand (car args)) (cdr args)))
   (t
    ;; Anything else shouldn't have happened
    (error "Can't expand ~ for %s" (cons op args)))))

;; Runtime initialisation
(progn
  ;; Install the handler
  (setq file-handler-alist (cons '("^~" . tilde-file-handler)
				 file-handler-alist))
  ;; Fix the initial default-directory; replacing $HOME by ~ if possible
  (when (string-looking-at (concat (quote-regexp (user-home-directory))
				   "?(.*)$") default-directory)
    (setq default-directory (expand-last-match "~/\\1"))))
