;; cgi-get.jl -- return the parameters from a CGI GET request
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

(declare (unsafe-for-call/cc))

(define-structure rep.www.cgi-get

    (export cgi-get-params)

    (open rep
	  rep.system
	  rep.regexp
	  rep.test.framework)

  (define-structure-alias cgi-get rep.www.cgi-get)

  (define unquote-plus-map (let ((map (make-string (1+ ?+)))
				 (i 0))
			     (while (< i ?+)
			       (aset map i i)
			       (setq i (1+ i)))
			     (aset map ?+ ? )
			     map))

  (defun cgi-get-params (#!optional query-string)
    (unless query-string
      (setq query-string (getenv "QUERY_STRING")))
    (let
	((point 0)
	 (params nil)
	 name value)
      (while (string-looking-at "([^=]+)=([^&]*)(&|$)" query-string point)
	(setq point (match-end))
	(setq name (intern
		    (unquote
		     (substring query-string (match-start 1) (match-end 1)))))
	(setq value (unquote
		     (substring query-string (match-start 2) (match-end 2))))
	(when (string= value "")
	  (setq value nil))
	(setq params (cons (cons name value) params)))
      (nreverse params)))

  (defsubst hexdigit (char)
    (if (and (>= char ?0) (<= char ?9))
	(- char ?0)
      (+ (- (char-upcase char) ?A) 10)))

  (defun unquote (string)
    (let
	((frags nil)
	 (point 0))
      (setq string (translate-string string unquote-plus-map))
      (while (string-match "%.." string point)
	(setq frags (cons (substring string point (match-start)) frags))
	(setq point (match-end))
	(setq frags (cons (+ (* (hexdigit (aref string (- point 2))) 16)
			     (hexdigit (aref string (1- point)))) frags)))
      (if (zerop point)
	  string
	(setq frags (cons (substring string point) frags))
	(apply concat (nreverse frags)))))


;; Tests

  (define (self-test)
    (test (equal (cgi-get-params "")
		 '()))
    (test (equal (cgi-get-params "foo=bar")
		 '((foo . "bar"))))
    (test (equal (cgi-get-params "foo=bar&baz=quux")
		 '((foo . "bar") (baz . "quux"))))
    (test (equal (cgi-get-params "foo=&baz=quux")
		 '((foo . ()) (baz . "quux"))))
    (test (equal (cgi-get-params "foo=%3A%2F%3D")
		 '((foo . ":/="))))
    (test (equal (cgi-get-params "foo=+bar+")
		 '((foo . " bar ")))))

  ;;###autoload
  (define-self-test 'rep.www.cgi-get self-test))
