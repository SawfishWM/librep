;;;; autoload.jl -- Initialise auto-load functions
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

;;; ::autoload-start::
(autoload 'compile-file "compiler" t)
(autoload 'compile-directory "compiler" t)
(autoload 'compile-lisp-lib "compiler" t)
(autoload 'compile-function "compiler" t)
(autoload 'compile-form "compiler")
(autoload 'debug-entry "debug")
(autoload 'debug-error-entry "debug")
(autoload 'disassemble "disassembler" t)
(autoload 'documentation "lisp-doc")
(autoload 'document-var "lisp-doc")
(autoload 'apropos-function "lisp-doc" t)
(autoload 'apropos-variable "lisp-doc" t)
(autoload 'describe-function "lisp-doc" t)
(autoload 'describe-variable "lisp-doc" t)
(autoload 'add-documentation "lisp-doc")
(autoload 'apropos-function "lisp-doc")
(autoload 'apropos-variable "lisp-doc")
(autoload 'describe-variable "lisp-doc")
(autoload 'describe-function "lisp-doc")
(autoload 'sort "sort")
(autoload 'getenv "environ")
(autoload 'setenv "environ")
(autoload 'unsetenv "environ")
(setq file-handler-alist (cons '("^/(([a-zA-Z0-9._-]+)@)?([a-zA-Z0-9._-]+):" . remote-file-handler) file-handler-alist))
(autoload 'remote-file-handler "remote")
(autoload 'remote-ftp-handler "remote-ftp")
(put 'ftp 'remote-backend remote-ftp-handler)
(autoload 'remote-ftp-add-passwd "remote-ftp" t)
(autoload 'remote-rcp-handler "remote-rcp")
(put 'rcp 'remote-backend remote-rcp-handler)
(autoload 'pwd-prompt "pwd-prompt")
(autoload 'remote-rep-add-passwd "remote-rep" t)
(autoload 'remote-rep-handler "remote-rep")
(put 'rep 'remote-backend remote-rep-handler)
(setq file-handler-alist (cons '("#tar\\b" . tar-file-handler) file-handler-alist))
(autoload 'tar-file-handler "tar-file-handler")
(autoload 'gaol-add-function "gaol")
(autoload 'gaol-replace-function "gaol")
(autoload 'gaol-add-feature "gaol")
(autoload 'gaol-add-special "gaol")
(autoload 'gaol-add-file-handler "gaol")
(autoload 'gaol-replace-file-handler "gaol")
(autoload-macro 'define "define")
(autoload-macro 'with-internal-definitions "define")
(autoload-macro 'with-threads-blocked "threads")
;;; ::autoload-end::
