#| autoload.jl -- auto-loaded definitions for test suite
   $Id$

   Copyright (C) 2001 Harper <john@dcs.warwick.ac.uk>

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

#|

To rebuild this file, load a tags file containing all lisp files into
Jade, select this file, then evaluate:

(let ((output-file (buffer-file-name)))
  (tags-map-buffers (lambda (buffer)
		      (add-autoloads output-file buffer))))

|#

;;; ::autoload-start::
(autoload-self-test 'rep.data.queues 'rep.data.queues)
(autoload-self-test 'rep.data 'rep.test.data)
(autoload-self-test 'rep.www.quote-url 'rep.www.quote-url)
(autoload-self-test 'rep.www.cgi-get 'rep.www.cgi-get)
;;; ::autoload-end::
