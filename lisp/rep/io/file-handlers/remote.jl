#|  remote.jl -- Remote file access

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
    the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure rep.io.file-handlers.remote ()

    (open rep
	  rep.io.files
	  rep.io.file-handlers
	  rep.io.file-handlers.remote.utils)

;;; Configuration

  ;; A symbol defines a backend type if its `remote-backend' property
  ;; is a function to call as (FUNCTION SPLIT-NAME-OR-NIL OP ARG-LIST)

  (defvar remote-auto-backend-alist nil
    "An alist of (HOST-REGEXP . BACKEND-TYPE) defining how remote files are
accessed on specific hosts.")

  (defvar remote-default-backend 'ftp
    "Backend used for otherwise unspecified hosts.")

;;; Entry point

  (defun remote-file-handler (op #!rest args)
      (cond
       ((filep (car args))
	;; A previously opened file handle. The backend should have stashed
	;; it's handler function in the first slot the file's handler-data
	;; (a vector)
	(let
	    ((split (remote-split-filename (file-binding (car args)))))
	  (funcall (aref (file-handler-data (car args)) 0) split op args)))

       ((eq op 'file-name-absolute-p))	;remote files are absolute?

       ((eq op 'local-file-name)
	;; can't get a local file name
	nil)

       (t
	(let
	    ;; Chop up the file name
	    ((split (remote-split-filename (if (eq op 'copy-file-from-local-fs)
					       ;; remote file is 2nd arg
					       (nth 1 args)
					     (car args)))))
	  (cond
	   ;; Handle all file name manipulations
	   ;; XXX This isn't such a good idea since it presumes that remote
	   ;; XXX systems use the same file naming conventions as locally.
	   ((eq op 'expand-file-name)
	    (remote-join-filename (car split) (nth 1 split)
				  (expand-file-name (nth 2 split) ".")))

	   ((eq op 'file-name-nondirectory)
	    (file-name-nondirectory (nth 2 split)))

	   ((eq op 'file-name-directory)
	    (remote-join-filename (car split) (nth 1 split)
				  (file-name-directory (nth 2 split))))

	   ((eq op 'file-name-as-directory)
	    (remote-join-filename (car split) (nth 1 split)
				  (if (string= (nth 2 split) "")
				      ""
				    (file-name-as-directory (nth 2 split)))))

	   ((eq op 'directory-file-name)
	    (remote-join-filename (car split) (nth 1 split)
				  (directory-file-name (nth 2 split))))

	   (t
	    ;; Anything else, pass off to a backend
	    (let
		((backend (get (or (cdr (assoc-regexp
					 (nth 1 split)
					 remote-auto-backend-alist t))
				   remote-default-backend)
			       'remote-backend)))
	      (funcall (if (symbolp backend)
			   (file-handler-ref backend)
			 backend)
		       split op args))))))))

  (define-file-handler 'remote-file-handler remote-file-handler)

;;; Initialise handler

;;;###autoload (setq file-handler-alist (cons '("^/(([a-zA-Z0-9._-]+)@)?([a-zA-Z0-9._-]+):" . remote-file-handler) file-handler-alist))

;;;###autoload (autoload-file-handler 'remote-file-handler 'rep.io.file-handlers.remote)

)
