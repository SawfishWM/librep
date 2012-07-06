#| tilde.jl -- File handler for tilde expansion

   $Id$

   Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 51 Franklin Street, Fifth Floor, 
   Boston, MA 02110-1301 USA
|#

(define-structure rep.io.file-handlers.tilde ()

    (open rep
	  rep.regexp
	  rep.system
	  rep.io.files
	  rep.io.file-handlers)

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

  (defun tilde-file-handler (op #!rest args)
    (cond
     ((eq op 'file-name-absolute-p))	;~FOO always absolute
     ((eq op 'expand-file-name)
      ;; Slightly tricky. It's necessary to remove the tilde, call
      ;; expand-file-name, then reapply the tilde. This is to ensure
      ;; that things like "~/foo/../bar" expand to "~/bar"
      (let
	  ((file-name (car args)))
	(if (string-looking-at "(~[^/]*/)." file-name)
	    (concat (substring file-name (match-start 1) (match-end 1))
		    (expand-file-name (substring file-name (match-end 1)) "."))
	  file-name)))
     ((memq op '(file-name-nondirectory file-name-directory
		file-name-as-directory directory-file-name))
      ;; Functions of a single file name that we leave alone. By re-calling
      ;; OP the standard action will occur since this handler is now
      ;; blocked for OP.
      (apply (symbol-value op) args))
     ((memq op '(local-file-name canonical-file-name open-file
		 write-buffer-contents read-file-contents insert-file-contents
		 delete-file delete-directory make-directory file-exists-p
		 file-regular-p file-readable-p
		 file-writable-p file-directory-p file-symlink-p file-owner-p
		 file-nlinks file-size file-modes file-modes-as-string
		 set-file-modes file-modtime directory-files
		 read-symlink make-symlink))
      ;; All functions which only have a single file name (their first
      ;; argument). Expand the tilde expression then re-call OP.
      (apply (symbol-value op) (tilde-expand (car args)) (cdr args)))
     ((eq op 'copy-file-to-local-fs)
      (apply copy-file (tilde-expand (car args)) (cdr args)))
     ((eq op 'copy-file-from-local-fs)
      ;; file to expand is second argument
      (copy-file (car args) (tilde-expand (cadr args))))
     ((eq op 'copy-file)
      ;; both names need expanding
      (copy-file (tilde-expand (car args)) (tilde-expand (cadr args))))
    (t
     ;; Anything else shouldn't have happened
     (error "Can't expand ~ in %s" (cons op args)))))

  (define-file-handler 'tilde-file-handler tilde-file-handler)

  ;; Runtime initialisation
  (progn
    ;; Install the handler
    (setq file-handler-alist (cons '("^~" . tilde-file-handler)
				   file-handler-alist))
    ;; Fix the initial default-directory; replacing $HOME by ~ if possible
    (when (string-looking-at (concat (quote-regexp
				      (canonical-file-name
				       (user-home-directory)))
				     "(/(.+))?$")
			     (canonical-file-name default-directory))
      (setq-default default-directory (expand-last-match "~/\\2")))))
