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
(setq file-handler-alist (cons '("^/(([a-zA-Z0-9._-]+)@)?([a-zA-Z0-9._-]+):" . remote-file-handler) file-handler-alist))
(autoload-file-handler 'remote-file-handler 'remote)
(put 'ftp 'remote-backend 'remote-ftp-handler)
(autoload-file-handler 'remote-ftp-handler 'remote-ftp)
(put 'rcp 'remote-backend 'remote-rcp-handler)
(autoload-file-handler 'remote-rcp-handler 'remote-rcp)
(put 'rep 'remote-backend 'remote-rep-handler)
(autoload-file-handler 'remote-rep-handler 'remote-rep)
(setq file-handler-alist (cons '("#tar\\b" . tar-file-handler) file-handler-alist))
(autoload-file-handler 'tar-file-handler 'tar-file-handler)
(autoload 'pwd-prompt "pwd-prompt")
;;; ::autoload-end::
