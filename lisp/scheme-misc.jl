#| scheme-misc.jl -- miscellaneous scheme support

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

(define-structure scheme-misc

    (export procedure? apply map for-each force
	    %make-promise call-with-current-continuation
	    call/cc dynamic-wind eval
	    scheme-report-environment null-environment
	    interaction-environment

	    call-with-input-file call-with-output-file
	    input-port? output-port? current-input-port
	    current-output-port with-input-from-file
	    with-output-to-file open-input-file
	    open-output-file close-input-port
	    close-output-port

	    read read-char peek-char eof-object?
	    write display newline write-char load)

  ((open rep scheme-utils)
   (access rep))

;;; control features

  (define procedure? (make-predicate functionp))

  (define (map proc . lists)
    (if (null (cdr lists))
	(mapcar proc (car lists))
      (let loop ((out nil)
		 (in lists))
	(if (car in)
	    (loop (cons (apply proc (mapcar car in)) out)
		  (mapcar cdr in))
	  (nreverse out)))))

  (define (for-each proc . lists)
    (if (null (cdr lists))
	(mapc proc (car lists))
      (let loop ((in lists))
	(when (car in)
	  (apply proc (mapcar car in))
	  (loop (mapcar cdr in))))))

  (define (force promise) (promise))

  (define (%make-promise thunk)
    (let ((result-ready nil)
	  result)
      (lambda ()
	(unless result-ready
	  (let ((x (thunk)))
	    (unless result-ready
	      (setq result-ready t)
	      (setq result x))))
	result)))

  (define (eval form . env) (rep#eval form))

  ;; XXX support these (and in the above)
  (define (scheme-report-environment version) #f)
  (define (null-environment version) #f)
  (define (interaction-environment) #f)

;;; input and output

  (define (call-with-input-file name proc)
    (let ((file (open-input-file name)))
      (prog1
	  (proc file)
	(close-file file))))

  (define (call-with-output-file name proc)
    (let ((file (open-output-file name)))
      (prog1
	  (proc file)
	(close-file file))))

  (define input-port? (make-predicate input-stream-p))
  (define output-port? (make-predicate output-stream-p))

  (define (current-input-port) standard-input)
  (define (current-output-port) standard-output)

  (define (with-input-from-file name thunk)
    (let ((standard-input (open-input-file name)))
      (prog1
	  (thunk)
	(close-file standard-input))))

  (define (with-output-to-file name thunk)
    (let ((standard-output (open-output-file name)))
      (prog1
	  (thunk)
	(close-file standard-output))))

  (define (open-input-file name) (open-file name 'read))
  (define (open-output-file name) (open-file name 'write))

  (define close-input-port close-file)
  (define close-output-port close-file)

;;; input

  (define eof-object (make-datum nil 'eof-object
				 (lambda (x s) (rep#write s "#<scheme-eof>"))))

  (define (read &optional port)
    (condition-case nil
	(rep#read port)
      (end-of-stream eof-object)))

  (define (read-char &optional port)
    (or (rep#read-char (or port standard-input)) eof-object))

  (define (peek-char &optional port)
    (or (rep#peek-char (or port standard-input)) eof-object))

  (define (eof-object? obj) (eq obj eof-object))

;;; output

  (define (write obj &optional port)
    (format (or port standard-output) "%S" obj))

  (define (display obj &optional port)
    (format (or port standard-output) "%s" obj))

  (define (newline &optional port)
    (rep#write (or port standard-output) #\newline))

  (define (write-char char &optional port)
    (write (rep#or port standard-output) char))

;;; system interface

  (define (load filename)
    ;; `t' arg means not to search load-path
    (rep#load filename nil t))

  (setq %load-suffixes '(".scm" ".scmc")))
