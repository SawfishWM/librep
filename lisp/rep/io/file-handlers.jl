#| rep.io.file-handlers bootstrap

   $Id$

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(declare (in-module rep.io.file-handlers))

(open-structures '(rep.lang.symbols
		   rep.io.files
		   rep.module-system
		   rep.data))

;;; file-handler definition

;; load this from the `rep' structure
(defun autoload-file-handler (symbol file)
  (define-file-handler symbol (make-autoload symbol file)))

(defun define-file-handler (name proc)
  (structure-define (current-structure) name proc))

;; replicated in files.c
(defun file-handler-ref (name)
  (%structure-ref (current-structure) name))

(export-bindings '(autoload-file-handler define-file-handler file-handler-ref))

;;; autoloads

;;; ::autoload-start::
(setq file-handler-alist (cons '("^/(([a-zA-Z0-9._-]+)@)?([a-zA-Z0-9._-]+):" . remote-file-handler) file-handler-alist))
(autoload-file-handler 'remote-file-handler "rep/io/file-handlers/remote")
(put 'ftp 'remote-backend 'remote-ftp-handler)
(autoload-file-handler 'remote-ftp-handler "rep/io/file-handlers/remote/ftp")
(put 'rcp 'remote-backend 'remote-rcp-handler)
(autoload-file-handler 'remote-rcp-handler "rep/io/file-handlers/remote/rcp")
(put 'rep 'remote-backend 'remote-rep-handler)
(autoload-file-handler 'remote-rep-handler "rep/io/file-handlers/remote/rep")
(setq file-handler-alist (cons '("#tar\\b" . tar-file-handler) file-handler-alist))
(autoload-file-handler 'tar-file-handler "rep/io/file-handlers/tar")
;;; ::autoload-end::
