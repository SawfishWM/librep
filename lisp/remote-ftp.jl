;;;; remote-ftp.jl -- Remote file access via FTP
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

(require 'remote)
(require 'maildefs)			;for user-mail-address
(provide 'remote-ftp)


;; Configuration:

(defvar ftp-program "ftp"
  "Program used for FTP sessions.")

(defvar remote-ftp-args '("-v" "-n" "-i" "-g")
  "List of arguments to remote FTP sessions.")

(defvar remote-ftp-show-messages t
  "When t, informational messages from FTP sessions are displayed.")

(defvar remote-ftp-max-message-lines nil
  "When non-nil, the maximum number of FTP message lines to keep.")

(defvar remote-ftp-timeout 30
  "Number of seconds to wait for FTP output before giving up.")

(defvar remote-ftp-max-sessions 5
  "If non-nil, the maximum number of FTP clients that may be running
concurrently.")

(defvar remote-ftp-anon-users "anonymous|ftp"
  "Regular expression matching user names of `anonymous' FTP sessions.")

(defvar remote-ftp-anon-passwd user-mail-address
  "Password sent to anonymous FTP sessions.")

;; XXX Allow this to be set by filename?
(defvar remote-ftp-transfer-type 'binary
  "Mode in which to transfer files, one of the symbols `binary' or `ascii'.")

(defvar remote-ftp-display-progress t
  "When non-nil, show progress of FTP transfers.")

(defvar remote-ftp-echo-output nil
  "When t, echo all output from FTP processes. Use for debugging only.")

(defvar remote-ftp-passwd-alist nil
  "Alist of (USER@HOST . PASSWD) defining all known FTP passwords.")

(defvar remote-ftp-host-user-alist nil
  "Alist of (HOST-REGEXP . USER-NAME) matching FTP session host names to
usernames. Only used when no username is given in a filename.")

(defvar remote-ftp-default-user (user-login-name)
  "Default username to use for FTP sessions when none is specified, either
explicitly, or by the remote-ftp-host-user-alist variable.")

(defvar remote-ftp-ls-format "ls \"-la %s\""
  "FTP command format string to produce an `ls -l' format listing of the
directory substituted for the single %s format specifier.")

(defvar remote-ftp-sessions nil
  "List of FTP structures defining all running FTP sessions.")


;; Output templates, mostly copied from ange-ftp :-)

(defvar remote-ftp-multi-msgs
  "220-|230-|226|25.-|221-|200-|331-|4[25]1-|530-"
  "Regular expression matching the start of a multiline ftp reply.")

(defvar remote-ftp-good-msgs
  "220 |230 |226 |25. |221 |200 |[Hh]ash mark"
  "Regular expression matching ftp \"success\" messages.")

(defvar remote-ftp-bad-msgs "55. |500 |\\?Invalid command"
  "Regular expression matching ftp \"failure\" messages.")

(defvar remote-ftp-skip-msgs
  (concat "200 (PORT|Port) |331 |150 |350 |[0-9]+ bytes |"
	  "Connected |$|Remote system|Using| |"
	  "Data connection |"
	  "local:|Trying|125 |550-|221 .*oodbye")
  "Regular expression matching ftp messages that can be ignored.")

(defvar remote-ftp-fatal-msgs
  (concat "ftp: |Not connected|530 |4[25]1 |rcmd: |"
	  "No control connection|([a-zA-Z0-9.-]+: )?unknown host|"
	  "lost connection")
  "Regular expression matching ftp messages that indicate serious errors.
These mean that the FTP process should (or already has) been killed.")

(defvar remote-ftp-passwd-msgs "[Pp]assword: *"
  "Regular expression matching password prompt.")

(defvar remote-ftp-ls-l-regexp "([a-zA-Z-]+)[ \t]+([0-9]+)[ \t]+([a-zA-Z0-9_]+)[ \t]+([a-zA-Z0-9_]+)[\t ]+([0-9]+)[ \t]+([a-zA-Z]+[ \t]+[0-9]+[ \t]+[0-9:]+)[ \t]+([^/ \t\n]+)"
  "Regexp defining `ls -l' output syntax. Hairy.")

(defvar remote-ftp-ls-l-type-alist '((?- . file) (?d . directory)
				     (?l . symlink) (?p . pipe) (?s . socket)
				     (?b . device) (?c . device))
  "Alist associating characters in the first column of `ls -l' output with
file types.")


;; ftp structure

(defconst remote-ftp-host 0)
(defconst remote-ftp-user 1)
(defconst remote-ftp-process 2)
(defconst remote-ftp-status 3)	;success,failure,busy,nil,dying,dead,timed-out
(defconst remote-ftp-callback 4)
(defconst remote-ftp-cached-dir 5)
(defconst remote-ftp-dircache 6)
(defconst remote-ftp-pending-output 7)
(defconst remote-ftp-struct-size 8)

(defmacro remote-ftp-status-p (session stat)
  `(eq (aref ,session remote-ftp-status) ,stat))

;; Return an ftp structure for HOST and USER, with a running ftp session
(defun remote-ftp-open-host (host &optional user)
  (unless user
    (setq user (remote-ftp-get-user host)))
  (catch 'foo
    (mapc #'(lambda (s)
	      (when (and (string= (aref s remote-ftp-host) host)
			 (string= (aref s remote-ftp-user) user))
		;; Move S to the head of the list
		(setq remote-ftp-sessions
		      (cons s (delq s remote-ftp-sessions)))
		(throw 'foo s)))
	  remote-ftp-sessions)
    ;; Create a new session
    (let*
	((session (make-vector remote-ftp-struct-size))
	 (process (make-process `(lambda (data)
				   (remote-ftp-output-filter ,session data))
				'remote-ftp-sentinel
				nil ftp-program
				(append remote-ftp-args (list host)))))
      (when (and remote-ftp-max-sessions
		 (> (length remote-ftp-sessions) remote-ftp-max-sessions))
	;; Kill the session last used the earliest
	(remote-ftp-close-session (last remote-ftp-sessions)))
      (set-process-connection-type process 'pty)
      (aset session remote-ftp-host host)
      (aset session remote-ftp-user user)
      (aset session remote-ftp-process process)
      (aset session remote-ftp-status 'busy)
      (or (start-process process)
	  (error "Can't start FTP session"))
      (setq remote-ftp-sessions (cons session remote-ftp-sessions))
      (remote-ftp-login session)
      session)))

(defun remote-ftp-close-session (session)
  (when (and (aref session remote-ftp-process)
	     (process-in-use-p (aref session remote-ftp-process)))
    (aset session remote-ftp-status 'dying)
    (kill-process (aref session remote-ftp-process))))

(defun remote-ftp-close-host (host &optional user)
  (unless user
    (setq user (remote-ftp-get-user host)))
  (catch 'foo
    (mapc #'(lambda (s)
	      (when (and (string= (aref s remote-ftp-host) host)
			 (string= (aref s remote-ftp-user) user))
		(remote-ftp-close-session s)
		(throw 'foo t)))
	  remote-ftp-sessions)))

(defun remote-ftp-get-session-by-process (process)
  (catch 'return
    (mapc #'(lambda (s)
	      (and (eq (aref s remote-ftp-process) process)
		   (throw 'return s)))
	  remote-ftp-sessions)))

(defun remote-ftp-get-user (host)
  (or (cdr (assoc-regexp host remote-ftp-host-user-alist))
      remote-ftp-default-user))


;; Communicating with ftp sessions

(defun remote-ftp-write (session format arg-list)
  (when (remote-ftp-status-p session 'dying)
    (error "FTP session is dying"))
  (apply 'format (aref session remote-ftp-process) format arg-list)
  (write (aref session remote-ftp-process) ?\n)
  (aset session remote-ftp-status 'busy))

(defun remote-ftp-while (session status &optional type)
  (when (remote-ftp-status-p session 'dying)
    (error "FTP session is dying"))
  (while (remote-ftp-status-p session status)
    (and (accept-process-output remote-ftp-timeout)
	 (aset session remote-ftp-status 'timed-out)
	 (error "FTP process timed out (%s)" (or type "unknown")))))

(defun remote-ftp-command (session type format &rest args)
  (when remote-ftp-display-progress
    (message (format nil "FTP %s: " type) t))
  (remote-ftp-while session 'busy type)
  (remote-ftp-write session format args)
  (remote-ftp-while session 'busy type)
  (when remote-ftp-display-progress
    (format t " %s" (aref session remote-ftp-status)))
  (eq (aref session remote-ftp-status) 'success))

(defun remote-ftp-output-filter (session output)
  (when remote-ftp-echo-output
    (let
	((print-escape t))
      (format (stderr-file) "FTP output: %S\n" output)))
  (when (aref session remote-ftp-pending-output)
    (setq output (concat (aref session remote-ftp-pending-output) output))
    (aset session remote-ftp-pending-output nil))
  (let
      ((point 0)
       line-end)
    (while (< point (length output))
      ;; Skip any prompts
      (when (string-looking-at "(ftp> *)+" output point)
	(setq point (match-end)))
      ;; Look for `#' progress characters
      (when (string-looking-at "#+" output point)
	(setq point (match-end))
	(write t (substring output (match-start) (match-end)))
	(redisplay))
      (if (string-looking-at remote-ftp-passwd-msgs output point)
	  ;; Send password
	  (progn
	    (remote-ftp-write
	     session "%s\n"
	     (list (if (string-match remote-ftp-anon-users
				     (aref session remote-ftp-user))
		       remote-ftp-anon-passwd
		     (remote-ftp-get-passwd (aref session remote-ftp-user)
					    (aref session remote-ftp-host)))))
	    ;; Can't be anything more?
	    (setq point (length output)))
	(if (string-match "\n" output point)
	    (progn
	      ;; At least one whole line
	      (setq line-end (match-end))
	      (cond
	       ((string-looking-at remote-ftp-skip-msgs output point)
		;; Ignore this line of output
		(setq point line-end))
	       ((string-looking-at remote-ftp-good-msgs output point)
		;; Success!
		(aset session remote-ftp-status 'success)
		(setq point line-end))
	       ((string-looking-at remote-ftp-bad-msgs output point)
		;; Failure!
		(aset session remote-ftp-status 'failure)
		(setq point line-end))
	       ((string-looking-at remote-ftp-multi-msgs output point)
		;; One line of a multi-line message
		(remote-ftp-show-multi output point line-end)
		(setq point line-end))
	       ((string-looking-at remote-ftp-fatal-msgs output point)
		;; Fatal error. Kill the session
		(remote-ftp-close-session session)
		(error "FTP process had fatal error"))
	       (t
		;; Hmm. something else. If one exists invoke the callback
		(if (aref session remote-ftp-callback)
		    (funcall (aref session remote-ftp-callback)
			     session output point line-end))
		(setq point line-end))))
	  ;; A partial line. Store it as pending
	  (aset session remote-ftp-pending-output (substring output point))
	  (setq point (length output)))))))

(defun remote-ftp-sentinel (process)
  (let
      ((session (remote-ftp-get-session-by-process process)))
    (unless (process-in-use-p process)
      (aset session remote-ftp-process nil)
      (aset session remote-ftp-dircache nil)
      (setq remote-ftp-sessions (delq session remote-ftp-sessions))
      (message (format nil "FTP session %s@%s exited"
		       (aref session remote-ftp-user)
		       (aref session remote-ftp-host))))))

(defun remote-ftp-show-multi (string start end)
  (let
      ((buffer (open-buffer "*ftp messages*")))
    (with-buffer buffer
      (goto (end-of-buffer))
      (insert (substring string start end))
      (when (and remote-ftp-max-message-lines
		 (> (pos-line (end-of-buffer)) remote-ftp-max-message-lines))
	(delete-area (start-of-buffer)
		     (backward-line remote-ftp-max-message-lines
				    (end-of-buffer)))))
    (when remote-ftp-show-messages
      (with-view (other-view)
	(goto-buffer buffer)
	(shrink-view-if-larger-than-buffer)))))


;; FTP commands

;; Starts the process structure already defined in SESSION, then
;; logs in as the named user
(defun remote-ftp-login (session)
  (and (remote-ftp-command session 'login "user %s"
			   (aref session remote-ftp-user))
       (remote-ftp-command session 'type "type %s" remote-ftp-transfer-type)
       (and remote-ftp-display-progress
	    (remote-ftp-command session 'hash "hash"))))

(defun remote-ftp-get (session remote-file local-file)
  (remote-ftp-command session 'get "get %s %s" remote-file local-file))

(defun remote-ftp-put (session local-file remote-file)
  (unwind-protect
      (remote-ftp-command session 'put "put %s %s" local-file remote-file)
    (remote-ftp-invalidate-directory
     session (file-name-directory remote-file))))

(defun remote-ftp-rm (session remote-file)
  (unwind-protect
      (remote-ftp-command session 'rm "delete %s" remote-file)
    (remote-ftp-invalidate-directory
     session (file-name-directory remote-file))))

(defun remote-ftp-mv (session old-name new-name)
  (unwind-protect
      (remote-ftp-command session 'mv "rename %s %s" old-name new-name)
    (remote-ftp-invalidate-directory
     session (file-name-directory old-name))
    (remote-ftp-invalidate-directory
     session (file-name-directory new-name))))

(defun remote-ftp-rmdir (session remote-dir)
  (unwind-protect
      (remote-ftp-command session 'rmdir "rmdir %s" remote-dir)
    (remote-ftp-invalidate-directory
     session (file-name-directory remote-dir))))

(defun remote-ftp-mkdir (session remote-dir)
  (unwind-protect
      (remote-ftp-command session 'mkdir "mkdir %s" remote-dir)
    (remote-ftp-invalidate-directory
     session (file-name-directory remote-dir))))

(defun remote-ftp-chmod (session mode file)
  ;; XXX Some FTP clients (i.e. Solaris 2.6) don't have the
  ;; XXX chmod command. Perhaps we could use "quote site chmod .."
  ;; XXX but the Solaris ftpd doesn't support this either..
  (unwind-protect
      (remote-ftp-command session 'chmod "chmod %o %s" mode file)
    (remote-ftp-invalidate-directory
     session (file-name-directory file))))


;; Directory handling/caching

(defconst remote-ftp-file-name 0)
(defconst remote-ftp-file-size 1)
(defconst remote-ftp-file-modtime 2)	;may be an unparsed string
(defconst remote-ftp-file-type 3)
(defconst remote-ftp-file-modes 4)	;nil if mode-string needs parsing
(defconst remote-ftp-file-mode-string 5)
(defconst remote-ftp-file-nlinks 6)
(defconst remote-ftp-file-user 7)
(defconst remote-ftp-file-group 7)
(defconst remote-ftp-file-struct-size 8)

(defun remote-ftp-parse-ls-l (string point)
  (when (string-looking-at remote-ftp-ls-l-regexp string point)
    (let
	((mode-string (substring string (match-start 1) (match-end 1)))
	 (nlinks (read-from-string (substring string
					      (match-start 2) (match-end 2))))
	 (user (substring string (match-start 3) (match-end 3)))
	 (group (substring string (match-start 4) (match-end 4)))
	 (size (read-from-string (substring string
					    (match-start 5) (match-end 5))))
	 (modtime (substring string (match-start 6) (match-end 6)))
	 (name (substring string (match-start 7) (match-end 7))))
      (vector name size modtime (cdr (assq (aref mode-string 0)
					   remote-ftp-ls-l-type-alist))
	      nil mode-string nlinks user group))))

(defun remote-ftp-file-modtime (file-struct)
  (when (stringp (aref file-struct remote-ftp-file-modtime))
    (require 'mail-headers)
    (let
	((date (mail-parse-date (aref file-struct remote-ftp-file-modtime))))
      (when date
	(aset file-struct remote-ftp-file-modtime
	      (aref date mail-date-epoch-time)))))
  (aref file-struct remote-ftp-file-modtime))

(defun remote-ftp-file-modes (file-struct)
  (unless (aref file-struct remote-ftp-file-modes)
    (let*
	((string (aref file-struct remote-ftp-file-mode-string))
	 (tuple-function
	  #'(lambda (point tuple)
	      (+ (ash (+ (if (/= (aref string point) ?-) 4 0)
			 (if (/= (aref string (1+ point)) ?-) 2 0)
			 (if (lower-case-p (aref string (+ point 2))) 1 0))
		      (* tuple 3))
		 (if (memq (aref string (+ point 2)) '(?s ?S ?t ?T))
		     (ash 01000 tuple)
		   0)))))
      (aset file-struct remote-ftp-file-modes
	    (+ (funcall tuple-function 1 2)
	       (funcall tuple-function 4 1)
	       (funcall tuple-function 7 0)))))
  (aref file-struct remote-ftp-file-modes))

(defun remote-ftp-file-owner-p (session file)
  (string= (aref session remote-ftp-user)
	   (aref file remote-ftp-file-user)))

(defun remote-ftp-get-file-details (session filename)
  (let
      ((dir (file-name-directory filename))
       (base (file-name-nondirectory filename)))
    (when (string= base "")
      ;; hack, hack
      (setq base (file-name-nondirectory dir)
	    dir (file-name-directory dir))
      (when (string= base "")
	(setq base ".")))
    (setq dir (directory-file-name dir))
    (unless (and (stringp (aref session remote-ftp-cached-dir))
		 (string= dir (aref session remote-ftp-cached-dir)))
      ;; Cache directory DIR
      (remote-ftp-while session 'busy 'dircache)
      (aset session remote-ftp-cached-dir 'busy)
      (aset session remote-ftp-dircache nil)
      (aset session remote-ftp-callback 'remote-ftp-dircache-callback)
      (remote-ftp-command session 'dircache remote-ftp-ls-format dir)
      (aset session remote-ftp-cached-dir dir)
      (aset session remote-ftp-callback nil))
    (catch 'return
      (mapc #'(lambda (f)
		(when (string= (aref f remote-ftp-file-name) base)
		  (throw 'return f)))
	    (aref session remote-ftp-dircache)))))

(defun remote-ftp-dircache-callback (session output point line-end)
  (let
      ((file-struct (remote-ftp-parse-ls-l output point)))
    (when file-struct
      (aset session remote-ftp-dircache
	    (cons file-struct (aref session remote-ftp-dircache))))))

(defun remote-ftp-invalidate-directory (session directory)
  (when (and (stringp (aref session remote-ftp-cached-dir))
	     (string= (directory-file-name directory)
		      (aref session remote-ftp-cached-dir)))
    (aset session remote-ftp-cached-dir nil)
    (aset session remote-ftp-dircache nil)))


;; Password caching

(defun remote-ftp-get-passwd (user host)
  (let*
      ((joined (concat user ?@ host))
       (cell (assoc joined remote-ftp-passwd-alist)))
    (if cell
	(cdr cell)
      (setq cell (pwd-prompt (concat "Password for " joined ?:)))
      (when cell
	(remote-ftp-add-passwd user host cell)
	cell))))

;;;###autoload
(defun remote-ftp-add-passwd (user host passwd)
  "Add the string PASSWD as the password for FTP session of USER@HOST."
  (interactive "sUsername:\nsHost:\nPassword for %s@%s:")
  (let
      ((joined (concat user ?@ host)))
    (catch 'foo
      (mapc #'(lambda (cell)
		(when (string= (car cell) joined)
		  (setcdr cell passwd)
		  (throw 'foo)))
	    remote-ftp-passwd-alist)
      (setq remote-ftp-passwd-alist (cons (cons joined passwd)
					  remote-ftp-passwd-alist)))))


;; Backend handler

;;;###autoload (put 'ftp 'remote-backend 'remote-ftp-handler)
(put 'ftp 'remote-backend 'remote-ftp-handler)

;;;###autoload
(defun remote-ftp-handler (split-name op args)
  (cond
   ((eq op 'canonical-file-name)
    ;; No feasible way to do this?
    (car args))
   ((memq op '(read-file-contents insert-file-contents copy-file-to-local-fs))
    ;; Need to get the file to the local fs
    (let
	((local-name (if (eq op 'copy-file-to-local-fs)
			 (nth 1 args)
		       (make-temp-name)))
	 (session (remote-ftp-open-host (nth 1 split-name) (car split-name))))
      (remote-ftp-get session (nth 2 split-name) local-name)
      (unless (eq op 'copy-file-to-local-fs)
	(unwind-protect
	    (funcall op local-name)
	  (delete-file local-name)))
      t))
   ((memq op '(write-buffer-contents copy-file-from-local-fs))
    ;; Need to get the file off the local fs
    (let
	((local-name (if (eq op 'copy-file-from-local-fs)
			 (car args)
		       (make-temp-name)))
	 (session (remote-ftp-open-host (nth 1 split-name) (car split-name))))
      (unless (eq op 'copy-file-from-local-fs)
	(apply op local-name (cdr args)))
      (unwind-protect
	  (remote-ftp-put session local-name (nth 2 split-name))
	(unless (eq op 'copy-file-from-local-fs)
	  (delete-file local-name)))
      t))
   ((eq op 'copy-file)
    ;; Copying on the remote fs.
    ;; XXX Is there a way to avoid the double transfer?
    ;; XXX Not for inter-session copies, anyway.
    (let
	((local-file (make-temp-name))
	 (dest-split (remote-split-filename (nth 1 args))))
      (unwind-protect
	  (and (remote-ftp-handler split-name 'copy-file-to-local-fs
				   (list (car args) local-file))
	       (remote-ftp-handler dest-split 'copy-file-from-local-fs
				   (list local-file (nth 1 args))))
	(and (file-exists-p local-file)
	     (delete-file local-file)))))
   ((eq op 'rename-file)
    (let
	((session (remote-ftp-open-host (nth 1 split-name) (car split-name)))
	 (dest-split (remote-split-filename (nth 1 args))))
      (or (and (string= (car dest-split) (car split-name))
	       (string= (nth 1 dest-split) (nth 1 split-name)))
	  (error "Can't rename files across FTP sessions"))
      (remote-ftp-mv session (nth 2 split-name) (nth 2 dest-split))))
   (t
    ;; All functions taking a single argument
    (let
	((session (remote-ftp-open-host (nth 1 split-name) (car split-name)))
	 (file-name (nth 2 split-name)))
      (cond
       ((eq op 'directory-files)
	(remote-ftp-get-file-details
	 session
	 ;; XXX this assumes local/remote have same naming structure!
	 (file-name-as-directory file-name))
	(mapcar #'(lambda (f)
		    (aref f remote-ftp-file-name))
		(aref session remote-ftp-dircache)))
       ((eq op 'delete-file)
	(remote-ftp-rm session file-name))
       ((eq op 'set-file-modes)
	(remote-ftp-chmod session (nth 1 args) file-name))
       (t
	(let
	    ((file (remote-ftp-get-file-details session file-name)))
	  (cond
	   ((eq op 'file-exists-p)
	    file)
	   ((eq op 'file-regular-p)
	    (and file (eq (aref file remote-ftp-file-type) 'file)))
	   ((eq op 'file-directory-p)
	    (and file (eq (aref file remote-ftp-file-type) 'directory)))
	   ((eq op 'file-symlink-p)
	    (and file (eq (aref file remote-ftp-file-type) 'symlink)))
	   ((eq op 'file-size)
	    (and file (aref file remote-ftp-file-size)))
	   ((eq op 'file-modes)
	    (and file (remote-ftp-file-modes file)))
	   ((eq op 'file-modes-as-string)
	    (and file (aref file remote-ftp-file-mode-string)))
	   ((eq op 'file-nlinks)
	    (and file (aref file remote-ftp-file-nlinks)))
	   ((eq op 'file-modtime)
	    (if file (remote-ftp-file-modtime file) (cons 0 0)))
	   ((eq op 'file-owner-p)
	    (and file (remote-ftp-file-owner-p session file)))
	   ((eq op 'file-readable-p)
	    (and file (/= (logand (remote-ftp-file-modes file)
				  (if (remote-ftp-file-owner-p session file)
				      0400 0004)) 0)))
	   ((eq op 'file-writable-p)
	    (and file (/= (logand (remote-ftp-file-modes file)
				  (if (remote-ftp-file-owner-p session file)
				      0200 0002)) 0)))
	   (t
	    (error "Unsupported FTP op: %s %s" op args))))))))))
