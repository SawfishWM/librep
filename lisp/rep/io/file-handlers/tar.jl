;; tar-file-handler.jl -- pretend that tar files are (read-only) directories
;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; $Id$

;; This file is part of librep.

;; librep is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; librep is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with librep; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'tar-file-handler)

;; Commentary:

;; This allows tar files to be handled as directories, append `#tar' to
;; the end of the file name to mark that the tar file should be treated
;; in this way. By default it knows how to deal with .tar, .tar.gz,
;; .tgz, .tar.Z, .taz, .tar.bz2 suffixes

;; This is pretty slow when reading more than one file, since each
;; file is uncompressed separately (i.e. uncompressing the entire tar
;; file each time. It would be better to untar the entire contents
;; somewhere, and then clean up later..

;; It needs to use GNU tar (for the compression options)


;; configuration

(defvar tarfh-gnu-tar-program "tar"
  "Location of GNU tar program.")

(defvar tarfh-alternative-gnu-tar-programs '("gtar"))

;; Initialised to the current tar version
(defvar tarfh-gnu-tar-version nil)

;; alist mapping file suffixes to GNU tar compression options
(defvar tarfh-compression-modes '(("\\.t?gz$" . "--gzip")
				  ("\\.(taz|Z)$" . "--compress")
				  ("\\.bz2" . "--bzip2")))

;; Hairy regexp matching tar `--list --verbose' output
(defvar tarfh-list-regexp (concat "([a-zA-Z-]+)\\s+(\\w+)/(\\w+)\\s+(\\d+)\\s+"
				  "([0-9-]+\\s+[0-9:]+)\\s+([^\n]+)"))

;; Map list file types to symbols
(defvar tarfh-list-type-alist '((?- . file) (?d . directory)
				(?l . symlink) (?p . pipe) (?s . socket)
				(?b . device) (?c . device)))

(defvar tarfh-max-cached-dirs 5
  "Total number of tar listings to cache.")

;; Cached tar listings
(defvar tarfh-dir-cache nil)

;; guards tarfh-created file handles
(defvar tarfh-fh-guardian (make-guardian))


;; Interface to tar program

(defun tarfh-check-tar-program ()
  (catch 'out
    (mapc (lambda (prog)
	    (let*
		((output (make-string-output-stream))
		 (process (make-process output)))
	      (when (zerop (call-process process nil prog "--version"))
		(setq output (get-output-stream-string output))
		(when (string-looking-at
		       "(tar )?[(]?GNU tar[)]?\\s*(.*?)\\s*\n" output)
		  (setq tarfh-gnu-tar-program prog)
		  (setq tarfh-gnu-tar-version (expand-last-match "\\2"))
		  (throw 'out t)))))
	  (cons tarfh-gnu-tar-program tarfh-alternative-gnu-tar-programs))
    (error "Can't find/execute GNU tar")))

(defun tarfh-call-tar (input-file output op tar-file &rest args)
  (unless tarfh-gnu-tar-version
    (tarfh-check-tar-program))
  ;; XXX handle non-local files by copying
  ;; XXX but then again, that's a bad idea in gaolled code..
  (setq tar-file (local-file-name tar-file))
  (let*
      ((process (make-process output))
       (mode (cdr (assoc-regexp tar-file tarfh-compression-modes)))
       (all-args `(,op ,@(and mode (list mode)) "--file" ,tar-file ,@args)))
    (zerop (apply call-process process input-file
		  tarfh-gnu-tar-program all-args))))

(defun tarfh-copy-out (tarfile file-name dest-file)
  (let
      ((file (open-file dest-file 'write)))
    (when file
      (unwind-protect
	  (tarfh-call-tar nil file "--extract" tarfile "--to-stdout" file-name)
	(close-file file)))))


;; directory caching

(defconst tarfh-file-full-name 0)
(defconst tarfh-file-name 1)
(defconst tarfh-file-size 2)
(defconst tarfh-file-modtime 3)		;may be unparsed string
(defconst tarfh-file-type 4)
(defconst tarfh-file-modes 5)		;nil if modes-string needs parsing
(defconst tarfh-file-modes-string 6)
(defconst tarfh-file-user 7)
(defconst tarfh-file-group 8)
(defconst tarfh-file-symlink 9)
(defconst tarfh-file-struct-size 10)

(defconst tarfh-cache-tarfile 0)	;canonical name
(defconst tarfh-cache-modtime 1)
(defconst tarfh-cache-entries 2)	;of tarfile
(defconst tarfh-cache-struct-size 3)

(defvar tarfh-pending-output nil)

(defun tarfh-output-function (string cache)
  (when tarfh-pending-output
    (setq string (concat tarfh-pending-output string))
    (setq tarfh-pending-output nil))
  (let
      ((point 0)
       entry next)
    (while (string-match "\n" string point)
      (setq next (match-end))
      (setq entry (tarfh-parse-list string point))
      (setq point next)
      (when entry
	(aset cache tarfh-cache-entries
	      (cons entry (aref cache tarfh-cache-entries)))))
    (when (< point (length string))
      (setq tarfh-pending-output (substring string point)))))

(defun tarfh-parse-list (string point)
  (when (string-looking-at tarfh-list-regexp string point)
    (let
	((mode-string (substring string (match-start 1) (match-end 1)))
	 (user (substring string (match-start 2) (match-end 2)))
	 (group (substring string (match-start 3) (match-end 3)))
	 (size (read-from-string (substring string
					    (match-start 4) (match-end 4))))
	 (modtime (substring string (match-start 5) (match-end 5)))
	 (name (substring string (match-start 6) (match-end 6)))
	 symlink file-name)
      (when (string-match " -> " name)
	(setq symlink (substring name (match-end)))
	(setq name (substring name 0 (match-start))))
      (setq file-name (expand-file-name name ""))
      (vector name file-name size modtime
	      (cdr (assq (aref mode-string 0) tarfh-list-type-alist))
	      nil mode-string user group symlink))))

(defun tarfh-file-get-modtime (file-struct)
  (when (stringp (aref file-struct tarfh-file-modtime))
    (require 'date)
    (let
	((date (parse-date (aref file-struct tarfh-file-modtime))))
      (when date
	(aset file-struct tarfh-file-modtime
	      (aref date date-vec-epoch-time)))))
  (aref file-struct tarfh-file-modtime))

(defun tarfh-file-get-modes (file-struct)
  (unless (aref file-struct tarfh-file-modes)
    (let*
	((string (aref file-struct tarfh-file-modes-string))
	 (tuple-function
	  (lambda (point tuple)
	    (+ (ash (+ (if (/= (aref string point) ?-) 4 0)
		       (if (/= (aref string (1+ point)) ?-) 2 0)
		       (if (lower-case-p (aref string (+ point 2))) 1 0))
		    (* tuple 3))
	       (if (memq (aref string (+ point 2)) '(?s ?S ?t ?T))
		   (ash 01000 tuple)
		 0)))))
      (aset file-struct tarfh-file-modes
	    (+ (tuple-function 1 2)
	       (tuple-function 4 1)
	       (tuple-function 7 0)))))
  (aref file-struct tarfh-file-modes))

(defun tarfh-directory-files (tarfile dir)
  (let
      ((entry (tarfh-lookup-file tarfile dir))
       re files tem)
    (when entry
      (setq dir (aref entry tarfh-file-name)))
    (setq dir (file-name-as-directory dir))
    (setq re (concat (quote-regexp dir) "([^/]+)"))
    (mapc (lambda (e)
	    (when (string-looking-at re (aref e tarfh-file-name))
	      (setq tem (expand-last-match "\\1"))
	      (unless (member tem files)
		(setq files (cons tem files)))))
	  (aref (car tarfh-dir-cache) tarfh-cache-entries))
    files))

(defun tarfh-directory-exists-p (tarfile name)
  (catch 'out
    (let
	((cache (tarfh-tarfile-cached-p tarfile)))
      (setq name (expand-file-name (file-name-as-directory name) ""))
      (when cache
	(mapc (lambda (entry)
		(when (string-head-eq (aref entry tarfh-file-name) name)
		  (throw 'out t)))
	      (aref cache tarfh-cache-entries))
	nil))))

(defun tarfh-file-owner-p (file)
  ;; XXX maybe just return t always?
  (string= (user-login-name) (aref file tarfh-file-user)))

(defun tarfh-tarfile-cached-p (tarfile)
  (setq tarfile (canonical-file-name tarfile))
  (catch 'exit
    (mapc (lambda (dir-entry)
	    (when (string= tarfile (aref dir-entry tarfh-cache-tarfile))
	      (throw 'exit dir-entry)))
	  tarfh-dir-cache)))

(defun tarfh-get-file (tarfile filename)
  (let
      (entry)
    (setq tarfile (canonical-file-name tarfile))
    (setq filename (expand-file-name filename ""))
    (setq entry (tarfh-tarfile-cached-p tarfile))
    (if (not (and entry (equal (aref entry tarfh-cache-modtime)
			       (file-modtime tarfile))))
	(progn
	  ;; Cache TARFILE
	  (when entry
	    (setq tarfh-dir-cache (delq entry tarfh-dir-cache))
	    (setq entry nil))
	  (when (>= (length tarfh-dir-cache) tarfh-max-cached-dirs)
	    ;; delete the least-recently-used entry
	    (setcdr (nthcdr (1- (length tarfh-dir-cache))
			    tarfh-dir-cache) nil))
	  ;; add the new (empty) entry for the directory to be read.
	  (setq entry (vector tarfile (file-modtime tarfile) nil))
	  (setq tarfh-dir-cache (cons entry tarfh-dir-cache))
	  (tarfh-call-tar nil (lambda (o)
				(tarfh-output-function o entry))
			  "--list" tarfile "--verbose")
	  (aset entry tarfh-cache-entries
		(nreverse (aref entry tarfh-cache-entries))))
      ;; entry is still valid, move it to the front of the list
      (setq tarfh-dir-cache (cons entry (delq entry tarfh-dir-cache))))
    ;; ENTRY now has the valid dircache directory structure
    (catch 'return
      (mapc (lambda (f)
	      (when (string= (aref f tarfh-file-name) filename)
		(throw 'return f)))
	    (aref entry tarfh-cache-entries)))))

;; similar to remote-ftp-get-file, but symbolic links are followed
(defun tarfh-lookup-file (tarfile file)
  (let
      ((file-struct (tarfh-get-file tarfile file)))
    (while (and file-struct
		(eq (aref file-struct tarfh-file-type) 'symlink))
      (let
	  ((link (aref file-struct tarfh-file-symlink)))
	(setq file (expand-file-name link (file-name-directory file)))
	(setq file-struct (tarfh-get-file tarfile file))))
    file-struct))

(defun tarfh-invalidate-tarfile (tarfile)
  (setq tarfile (canonical-file-name tarfile))
  (let
      ((entry (tarfh-tarfile-cached-p tarfile)))
    (when entry
      (setq tarfh-dir-cache (delq entry tarfh-dir-cache)))))

(defun tarfh-empty-cache ()
  "Discard all cached TAR directory entries."
  (interactive)
  (setq tarfh-dir-cache nil))

(defun tarfh-after-gc ()
  (let
      (fh)
    (while (setq fh (tarfh-fh-guardian))
      (when (file-binding fh)
	(close-file fh)))))

(add-hook 'after-gc-hook tarfh-after-gc)


;; file handler

;;;###autoload (setq file-handler-alist (cons '("#tar\\b" . tar-file-handler) file-handler-alist))

(defun tarfh-split-filename (name)
  (unless (string-match "#tar/?" name)
    (error "Can't find #tar in %s" name))
  (cons (substring name 0 (match-start)) (substring name (match-end))))

;;;###autoload
(defun tar-file-handler (op &rest args)
  (cond ((filep (car args))
	 ;; an open file handle
	 (let
	     ((split (tarfh-split-filename (file-binding (car args)))))
	   (tarfh-handler (car split) (cdr split) op args)))
	((eq op 'file-name-absolute-p)
	 (file-name-absolute-p (car args)))
	((eq op 'local-file-name)
	 nil)
	((memq op '(expand-file-name file-name-nondirectory file-name-directory
		    file-name-as-directory directory-file-name))
	 (apply (symbol-value op) args))
	((memq op '(write-buffer-contents delete-file delete-directory
		    make-directory set-file-modes make-symlink
		    copy-file-from-local-fs copy-file))
	 (signal 'file-error (list "TAR files are read-only" op args)))
	((memq op '(canonical-file-name open-file read-file-contents
		    copy-file-to-local-fs insert-file-contents
		    file-exists-p file-regular-p file-readable-p
		    file-writable-p file-directory-p file-symlink-p
		    file-owner-p file-nlinks file-size file-modes
		    file-modes-as-string file-modtime directory-files
		    read-symlink))
	 (let
	     ((split (tarfh-split-filename (car args))))
	   (tarfh-handler (car split) (cdr split) op args)))
	(t
	 (error "Unknown file op in TAR handler: %s %S" op args))))
	
(defun tarfh-handler (tarfile rel-file op args)
  (cond
   ((eq op 'canonical-file-name)
    ;; XXX implement this by resolving symlinks
    (car args))
   ((filep (car args))
    ;; Operations on file handles
    (cond
     ((memq op '(seek-file flush-file write-buffer-contents
		 read-file-contents insert-file-contents))
      ;; Just pass these through to the underlying file
      (apply (symbol-value op) (file-bound-stream (car args)) (cdr args)))
     ((eq op 'close-file)
      (let*
	  ((file (car args))
	   (local-file (file-handler-data file)))
	(close-file (file-bound-stream file))
	(delete-file local-file)))
     (t
      (error "Unsupported TAR op on file-handler: %s %s" op args))))
   ((memq op '(copy-file write-buffer-contents copy-file-from-local-fs
	      rename-file delete-file delete-directory make-directory
	      set-file-modes))
    (signal 'file-error (list "TAR fh is read-only" op args)))
   ((eq op 'directory-files)
    (tarfh-directory-files tarfile rel-file))
   (t
    ;; All functions taking a single argument
    (let
	((file (if (eq op 'file-symlink-p)
		   (tarfh-get-file tarfile rel-file)
		 (tarfh-lookup-file tarfile rel-file))))
      (cond
       ((memq op '(read-file-contents insert-file-contents
		   copy-file-to-local-fs))
	;; Need to get the file to the local fs
	(let
	    ((local-name (if (eq op 'copy-file-to-local-fs)
			     (nth 1 args)
			   (make-temp-name))))
	  (or file (signal 'file-error (list "Unknown file:" (car args))))
	  (tarfh-copy-out tarfile (aref file tarfh-file-full-name) local-name)
	  (unless (eq op 'copy-file-to-local-fs)
	    (unwind-protect
		(funcall (symbol-value op) local-name)
	      (delete-file local-name)))
	  t))
       ((eq op 'open-file)
	(let
	    ((type (nth 1 args))
	     (local-file (make-temp-name))
	     local-fh)
	  (when (memq type '(read append))
	    ;; Need to transfer the file initially
	    (tarfh-copy-out
	     tarfile (aref file tarfh-file-full-name) local-file))
	  ;; Open the local file
	  (setq local-fh (make-file-from-stream (car args)
						(open-file local-file type)
						'tar-file-handler))
	  (set-file-handler-data local-fh local-file)
	  (tarfh-fh-guardian local-fh)
	  local-fh))
       ((eq op 'file-exists-p)
	(or file (tarfh-directory-exists-p tarfile rel-file)))
       ((eq op 'file-regular-p)
	(and file (eq (aref file tarfh-file-type) 'file)))
       ((eq op 'file-directory-p)
	(if file
	    (eq (aref file tarfh-file-type) 'directory)
	  (tarfh-directory-exists-p tarfile rel-file)))
       ((eq op 'file-symlink-p)
	(and file (eq (aref file tarfh-file-type) 'symlink)))
       ((eq op 'file-size)
	(and file (aref file tarfh-file-size)))
       ((eq op 'file-modes)
	(and file (tarfh-file-get-modes file)))
       ((eq op 'file-modes-as-string)
	(and file (aref file tarfh-file-modes-string)))
       ((eq op 'file-nlinks)
	1)
       ((eq op 'file-modtime)
	(if file (tarfh-file-get-modtime file) (cons 0 0)))
       ((eq op 'file-owner-p)
	(and file (tarfh-file-owner-p file)))
       ((eq op 'file-readable-p)
	(and file (/= (logand (tarfh-file-get-modes file)
			      (if (tarfh-file-owner-p file)
				  0400 0004)) 0)))
       ((eq op 'file-writable-p)
	nil)
       ((eq op 'read-symlink)
	(and file (or (aref file tarfh-file-symlink)
		      (signal 'file-error
			      (list "File isn't a symlink:" (car args))))))
       (t
	(error "Unsupported TAR op: %s %s" op args)))))))
