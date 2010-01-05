;;;; remote.jl -- Remote file access
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

(declare (unsafe-for-call/cc))

(define-structure rep.io.file-handlers.remote.rcp ()

    (open rep
	  rep.system
	  rep.io.processes
	  rep.io.files
	  rep.io.file-handlers
	  rep.io.file-handlers.remote.utils)

  ;; Notes:

  ;; Don't use this. It needs a lot of work. Use the FTP backend instead.


;; Configuration:

(defvar rcp-program "rcp"
  "The name of the `rcp' program used to copy files from host to host.")


;; Code:

(defun remote-rcp-command (#!rest args)
  (message (format nil "Calling rcp with args: %s... " args) t)
  (let
      ((status (apply call-process nil nil rcp-program args)))
    (write t "done")
    (or (zerop status)
	(error "Couldn't run rcp with args: %s" args))))

(defun remote-rcp-filename (split)
  (concat (and (car split) (concat (car split) ?@))
	  (nth 1 split) ?: (nth 2 split)))

(defun remote-rcp-handler (split-name op args)
  (cond
   ((eq op 'canonical-file-name)
    (car args))
   ((memq op '(read-file-contents insert-file-contents copy-to-local-fs))
    ;; Need to get the file to the local fs
    (let
	((local-name (if (eq op 'copy-to-local-fs)
			 (car args)
		       (make-temp-name))))
      (remote-rcp-command (remote-rcp-filename split-name) local-name)
      (when (memq op '(read-file-contents insert-file-contents))
	(unwind-protect
	    (funcall (symbol-value op) local-name)
	  (delete-file local-name)))
      t))
   ((memq op '(write-buffer-contents copy-from-local-fs))
    ;; Need to get the file off the local fs
    (let
	((local-name (if (eq op 'copy-from-local-fs)
			 (car args)
		       (make-temp-name))))
      (when (eq op 'write-buffer-contents)
	(apply (symbol-value op) local-name (cdr args)))
      (unwind-protect
	  (remote-rcp-command local-name (remote-rcp-filename split-name))
	(when (eq op 'write-buffer-contents)
	  (delete-file local-name)))
      t))
   ;; This is where the laziness sets in...
   ((memq op '(file-exists-p file-regular-p file-readable-p
	       file-writable-p owner-p))
    t)
   ((memq op '(file-directory-p file-symlink-p set-file-modes delete-file
	       rename-file copy-file))
    nil)
   ((eq op 'file-nlinks)
    1)
   ((eq op 'file-size)
    0)
   ((eq op 'file-modes)
    #o644)
   ((eq op 'file-modes-as-string)
    (make-string 10 ?*))
   ((eq op 'file-modtime)
    (cons 0 0))
   ((eq op 'directory-files)
    nil)
   (t
    (error "Unsupported remote-rcp op: %s %s" op args))))

;;;###autoload (put 'rcp 'remote-backend 'remote-rcp-handler)

;;;###autoload (autoload-file-handler 'remote-rcp-handler 'rep.io.file-handelrs.remote.rcp)

(define-file-handler 'remote-rcp-handler remote-rcp-handler))
