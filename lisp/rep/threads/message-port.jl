#| message-port.jl -- inter-thread communication channels

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

(define-structure rep.threads.message-port

    (export make-message-port
	    message-port-p
	    message-fetch
	    message-send
	    message-waiting-p)

    (open rep
	  rep.threads
	  rep.threads.mutex
	  rep.threads.condition-variable
	  rep.data.records
	  rep.data.queues)

  (define-record-type :message-port
    (create-port queue mutex condition)
    message-port-p
    (queue port-queue)
    (mutex port-mutex)
    (condition port-condition))

  (define (make-message-port)
    "Create and return a new message port."
    (create-port (make-queue) (make-mutex) (make-condition-variable)))

  (define (message-waiting-p port)
    "Return true if there are messages waiting on message port PORT."
    (obtain-mutex (port-mutex port))
    (unwind-protect
	(not (queue-empty-p (port-queue port)))
      (release-mutex (port-mutex port))))

  (define (message-fetch port #!optional timeout)
    "Fetch the earliest unread message sent to message port PORT. Blocks the
current thread for TIMEOUT milliseconds, or indefinitely if TIMEOUT isn't
defined. Returns the message, or false if no message could be read."
    (obtain-mutex (port-mutex port))
    (unwind-protect
	(let again ((can-wait t))
	  (if (queue-empty-p (port-queue port))
	      (if can-wait
		  (again (condition-variable-wait (port-condition port)
						  (port-mutex port) timeout))
		nil)
	    ;; we have a waiting message
	    (dequeue (port-queue port))))
      (release-mutex (port-mutex port))))

  (define (message-send port message)
    "Send the message MESSAGE (an arbitrary value) to message port PORT."
    (obtain-mutex (port-mutex port))
    (unwind-protect
	(progn
	  (enqueue (port-queue port) message)
	  (condition-variable-signal (port-condition port)))
      (release-mutex (port-mutex port)))))


#| Test function:

  (define (test)

    (let ((port (make-message-port)))

      (define (master)
	(do ((i 0 (1+ i)))
	    ((= i 10))
	  (thread-suspend (current-thread) (random 1000))
	  (let ((data (make-string i (+ (random 10) #\0))))
	    (message-send port data)
	    (format standard-output "master: sent %S\n" data))))

      (define (slave)
	(do ((i 0 (1+ i)))
	    ((= i 10))
	  (thread-suspend (current-thread) (random 1000))
	  (let ((data (message-fetch port)))
	    (format standard-output "slave: received %S\n" data))))

    (call-with-dynamic-root
     (lambda ()
       (random t)
       (make-thread slave "slave")
       (make-thread master "master")))))
|#
