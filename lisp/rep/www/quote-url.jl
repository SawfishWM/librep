#| quote-url.jl -- url-escape a given string

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

;; Background:

;; Sen Nagata posted code to do the escaping part of this to the rep
;; mailing list (<20000424174557J.1000@eccosys.com>). I've rewritten it
;; to use regexps, and added the decoder.

(define-structure rep.www.quote-url

    (export quote-url
	    unquote-url)

    (open rep
	  rep.regexp
	  rep.test.framework)

  (defconst url-meta-re "[^a-zA-Z0-9$_.!~*'(),-]"
    "A regexp matching a single character that is reserved in the URL spec.
This is taken from draft-fielding-url-syntax-02.txt -- check your local
internet drafts directory for a copy.")
       
  (define (quote-url string)
    "Escape URL meta-characters in STRING."
    (string-replace url-meta-re
		    (lambda (s)
		      (string-upcase
		       (format nil "%%%02x" (aref s (match-start)))))
		    string))

  (define (unquote-url string)
    "Unescape URL meta-characters in STRING."
    (string-replace "%([0-9A-Fa-f][0-9A-Fa-f])"
		    (lambda ()
		      (string->number (expand-last-match "\\1") 16))
		    string))


;; Tests

  (define (self-test)
    (test (string= (quote-url "http://www.foo.com/bar.html")
		   "http%3A%2F%2Fwww.foo.com%2Fbar.html"))
    (test (string= (quote-url "http://www.foo.com/~jsh/")
		   "http%3A%2F%2Fwww.foo.com%2F~jsh%2F"))
    (test (string= (unquote-url "http%3A%2F%2Fwww.foo.com%2Fbar.html")
		   "http://www.foo.com/bar.html"))
    (test (string= (unquote-url "http%3A%2F%2Fwww.foo.com%2F~jsh%2F")
		   "http://www.foo.com/~jsh/")))

  ;;###autoload
  (define-self-test 'rep.www.quote-url self-test))
