#| remote-util.jl -- Remote file access common functions

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

(define-structure rep.io.file-handlers.remote.utils

    (export remote-get-user
	    remote-split-filename
	    remote-join-filename
	    remote-register-file-handle)

    (open rep
	  rep.system
	  rep.regexp
	  rep.io.files)

  (defvar remote-host-user-alist nil
    "Alist of (HOST-REGEXP . USER-NAME) matching host names to usernames.
Only used when no username is given in a filename.")

  (defvar remote-default-user (user-login-name)
    "Default username to use for file-transfer when none is specified, either
explicitly, or by the remote-ftp-host-user-alist variable.")

  ;; Remote filename syntax
  (defconst remote-file-regexp "^/(([a-zA-Z0-9._-]+)@)?([a-zA-Z0-9._-]+):")

  ;; guards remote file handles (closes them if necessary)
  (define remote-fh-guardian (make-guardian))

  (defun remote-get-user (host)
    (or (cdr (assoc-regexp host remote-host-user-alist)) remote-default-user))

  ;; Return (USER-OR-NIL HOST FILE)
  (defun remote-split-filename (filename)
    (unless (string-match remote-file-regexp filename)
      (error "Malformed remote file specification: %s" filename))
    (let
	((host (substring filename (match-start 3) (match-end 3)))
	 (file (substring filename (match-end))))
      (list
       (and (match-start 2)
	    (substring filename (match-start 2) (match-end 2)))
       host file)))

  ;; Create a remote file name. USER may be nil
  (defun remote-join-filename (user host file)
    (concat ?/ (and user (concat user ?@)) host ?: file))

  (defun remote-register-file-handle (fh)
    (remote-fh-guardian fh))

  (defun remote-after-gc ()
    (do ((fh (remote-fh-guardian) (remote-fh-guardian)))
	((not fh))
      (when (file-binding fh)
	(close-file fh))))

  (add-hook 'after-gc-hook remote-after-gc))
