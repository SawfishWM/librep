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
(autoload 'add-documentation "lisp-doc")
(autoload 'apropos-function "lisp-doc")
(autoload 'apropos-variable "lisp-doc")
(autoload 'describe-variable "lisp-doc")
(autoload 'describe-function "lisp-doc")
(autoload 'sort "sort")
(autoload 'getenv "environ")
(autoload 'setenv "environ")
(setq file-handler-alist (cons '("^/(([a-zA-Z0-9._-]+)@)?([a-zA-Z0-9._-]+):" . remote-file-handler) file-handler-alist))
(autoload 'remote-file-handler "remote")
(put 'ftp 'remote-backend 'remote-ftp-handler)
(autoload 'remote-ftp-handler "remote-ftp")
(autoload 'remote-ftp-add-passwd "remote-ftp" t)
(put 'rcp 'remote-backend 'remote-rcp-handler)
(autoload 'remote-rcp-handler "remote-rcp")
(autoload 'pwd-prompt "pwd-prompt")
(autoload 'remote-rep-add-passwd "remote-rep" t)
(put 'rep 'remote-backend 'remote-rep-handler)
(autoload 'remote-rep-handler "remote-rep")
;;; ::autoload-end::
