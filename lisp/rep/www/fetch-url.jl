#| fetch-url.jl -- functions for downloading files

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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure rep.www.fetch-url

    (export fetch-url
	    fetch-url-async)

    (open rep
	  rep.io.processes)

  (defvar *wget-program* "wget"
    "Location of `wget' program.")

  (put 'wget 'error-message "Wget Error")

  (define (fetch-url url dest-stream)
    (let ((process (make-process dest-stream)))
      (set-process-error-stream process standard-error)
      (unless (zerop (call-process process nil *wget-program*
				   "-nv" "-O" "-" url))
	(signal 'wget (list url)))))

  (define (fetch-url-async url dest-stream callback #!optional error-stream)
    (let ((process (make-process dest-stream)))
      (set-process-error-stream process (or error-stream standard-error))
      (set-process-function process callback)
      (start-process process nil *wget-program* "-nv" "-O" "-" url))))
