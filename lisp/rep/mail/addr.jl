;;;; mailaddr.jl -- Minor mail configuration
;;;  Copyright (C) 1997 John Harper <john@dcs.warwick.ac.uk>
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

(define-structure rep.mail.addr ()

    (open rep
	  rep.regexp
	  rep.system)

  (define-structure-alias mailaddr rep.mail.addr)

  (defvar mail-domain-name (if (string-match "^([^.]+\\.)[^.]+" (system-name))
			       (substring (system-name) (match-end 1))
			     (system-name))
    "Mail domainname of the local site.")

  (defvar user-mail-address (concat (user-login-name) ?\@ mail-domain-name)
    "Address to put in From: headers of outgoing mail."))
