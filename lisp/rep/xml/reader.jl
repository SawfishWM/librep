#| rep.xml.reader -- very basic XML parser

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

;; This is an incredibly basic XML parser. I wrote it to be able to
;; parse the example data in http://www.xmlrpc.com/spec. I haven't read
;; the real XML spec at all, so this definitely doesn't follow it

;; It spits out items that look like this:

;; (TAG PARAMS BODY-ITEMS...)

;; where TAG is a symbol, PARAMS is an alist mapping symbols to strings
;; and BODY-ITEMS... is a list of items

;; So something like <foo>bar</foo> would be (foo () "bar")

;; Also, any item whose begins with an exclamation mark is read as (!
;; STRING), so e.g. <!-- a comment --> would be (! "-- a comment --")

;; Things like <?xml version="1.0"?> uses the first form: (?xml
;; (version . "1.0"))

(define-structure rep.xml.reader

    (export make-xml-input
	    read-xml-item)

    (open rep
	  rep.regexp)

  (defconst token-endings (#\space #\newline #\tab #\> #\= #\/))
  (defconst whitespace-chars (#\space #\newline #\tab #\return))

  (define (make-xml-input input)
    (cons input (read-char input)))

  (define (next stream)
    (let ((c (read-char (car stream))))
      (rplacd stream c)
      c))

  (define-macro (current stream) `(cdr ,stream))

  (define (eat-whitespace stream)
    (when (memq (current stream) whitespace-chars)
      (while (memq (next stream) whitespace-chars))))

  (define (read-string-item stream endings)
    (let loop ((this (current stream))
	       (chars '()))
      (if (or (null this) (memq this endings))
	  (apply concat (nreverse chars))
	(loop (next stream) (cons this chars)))))

  (define (substitute-entities string)
    ;; XXX other entities?
    (string-replace "&(lt|amp|apos|quot);"
		    (lambda ()
		      (cdr (assoc (expand-last-match "\\1")
				  '(("lt" . "<")
				    ("amp" . "&")
				    ("apos" . "'")
				    ("quot" . "\"")))))
		    string))

  (define (read-token stream)
    (eat-whitespace stream)
    (intern (read-string-item stream token-endings)))

  (define (read-body-data stream)
    (substitute-entities (read-string-item stream '(#\<))))

  (define (read-quoted-token stream)
    (cond ((space-char-p (current stream)) "")
	  ((not (memq (current stream) '(#\" #\')))
	   (read-string-item stream token-endings))
	  (t (let ((delim (list (current stream))))
	       (next stream)
	       (prog1
		   (substitute-entities (read-string-item stream delim))
		 (next stream))))))

  (define (read-param-list stream)
      (let loop ((params '()))
	(eat-whitespace stream)
	(if (memq (current stream) '(#\? #\/ #\>))
	    (nreverse params)
	  (let ((name (read-token stream)))
	    (eat-whitespace stream)
	    (or (= (current stream) #\=)
		(error "Expected '=' character: %s" stream))
	    (next stream)
	    (eat-whitespace stream)
	    (let ((data (read-quoted-token stream)))
	      (loop (cons (cons name data) params)))))))

  (define (read-question-body stream)
    (let ((name (read-token stream))
	  (params (read-param-list stream)))
      (or (= (next stream) #\>)
	  (error "Expected '>' character: %s" stream))
      (next stream)
      (list (intern (concat #\? (symbol-name name))) params)))

  (define (read-exclam-body stream)
    (let ((data (substitute-entities (read-string-item stream '(#\>)))))
      (or (= (current stream) #\>)
	  (error "Expected '>' character: %s" stream))
      (next stream)
      (list '! data)))

  (define (read-tag-body stream)
    (let ((name (read-token stream))
	  (params (read-param-list stream)))
      (cond ((= (current stream) #\/)
	     (or (= (next stream) #\>)
		 (error "Expected '>' character: %s" stream))
	     (next stream)
	     (list name params))
	    ((/= (current stream) #\>)
	     (error "Expected '>' character: %s" stream))
	    (t (next stream)
	       (let ((items '()))
		 (let ((ended
			(catch 'list-ended
			  (while (current stream)
			    (setq items (cons (read-xml-item
					       stream 'list-ended) items))))))
		   (or (string= ended name)
		       (error "Unmatched items: %s, %s" name ended)))
		 (list* name params (nreverse items)))))))

  (define (read-xml-item stream #!optional catcher)
    (cond
     ((null (current stream)) nil)

     ((= (current stream) #\<)
      (case (next stream)
	((#\/)
	 (next stream)
	 (eat-whitespace stream)
	 (let ((name (read-token stream)))
	   (eat-whitespace stream)
	   (or (= (current stream) #\>)
	       (error "Expected '>' character: %s" stream))
	   (next stream)
	   (throw catcher name)))

	((#\?)
	 (next stream)
	 (read-question-body stream))

	((#\!)
	 (next stream)
	 (read-exclam-body stream))

	(t (read-tag-body stream))))

     (t (read-body-data stream)))))
