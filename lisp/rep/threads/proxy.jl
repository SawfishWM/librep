#| proxy.jl -- move a function to a separate thread

   $Id$

   Copyright (C) 2001 John Harper <jsh@pixelslut.com>

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

;; I don't think I ever tested this code, but I'm tired of having it
;; sitting in working copy of the sources..

(define-structure rep.threads.proxy

    (export make-thread-proxy
	    thread-proxy-async-call
	    thread-proxy-delete)

    (open rep
	  rep.threads
	  rep.threads.message-port)

  (define special-token (cons))

  (define (make-thread-proxy function)
    (let ((in-port (make-message-port))
	  proxy-thread)

      (define (thread-thunk)
	(while t
	  (let ((data (message-fetch in-port)))
	    (case (car data)
	      ((sync-call)
	       (let ((return-port (cadr data))
		     (args (cddr data)))
		 (call-with-exception-handler
		  (lambda ()
		    (let ((result (apply function args)))
		      (message-send return-port (cons t result))))
		  (lambda (exception)
		    (message-send return-port (cons nil exception))))))
	      ((async-call)
	       (apply function (cdr data)))
	      (t (error "Unknown proxy operation: %s\n" (car data)))))))

      (define (proxy . args)
	(if (eq (car args) special-token)
	    (case (cadr args)
	      ((async) (message-send in-port (cons 'async-call (cddr args))))
	      ((get-thread) proxy-thread)
	      (t (error "Unknown special call: %s" (cadr args))))
	  ;; synchronous call
	  (let ((return-port (make-message-port)))
	    (message-send in-port (list* 'sync-call return-port args))
	    (let ((result (message-fetch return-port)))
	      (if (car result)
		  (cdr result)
		(raise-exception (cdr result)))))))

      (setq proxy-thread (make-thread thread-thunk "object-proxy"))
      proxy))

  (define (thread-proxy-async-call proxy . args)
    (apply proxy special-token 'async args))

  (define (thread-proxy-delete proxy)
    (thread-delete (proxy special-token 'get-thread))))
