#| rep.xml.printer -- companion XML printer to rep.xml.reader

   $Id$

   Copyright (C) 2002 John Harper <jsh@unfactored.org>

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

(define-structure rep.xml.printer

    (export make-xml-output
	    print-xml-item)

    (open rep
	  rep.regexp)

  (define make-xml-output identity)

  (define (substitute-entities string)
    (string-replace "[<&'\"]"
		    (lambda ()
		      (car (rassoc (expand-last-match "\\0")
				   '(("lt" . "<")
				     ("amp" . "&")
				     ("apos" . "'")
				     ("quot" . "\"")))))
		    string))

  (define (print-params stream params)
    (mapc (lambda (cell)
	    (format stream " %s=\"%s\""
		    (car cell) (substitute-entities (cdr cell))))
	  params))

  (define (print-xml-item stream item)
    (cond ((stringp item)
	   (write stream (substitute-entities item)))

	  ((eq (car item) '!)
	   (format stream "<!%s>" (nth 1 stream)))

	  ((symbolp (car item))
	   (format stream "<%s" (car item))
	   (print-params stream (nth 1 item))
	   (cond ((string-match "^\\?" (symbol-name (car item)))
		  (write stream "?>"))
		 ((null (nthcdr 2 item))
		  (write stream "/>"))
		 (t
		  (write stream #\>)
		  (mapc (lambda (x)
			  (print-xml-item stream x)) (nthcdr 2 item))
		  (format stream "</%s>" (car item)))))

	  (t (error "Unknown item type: %s" item)))))
