#| queues.jl -- fifo queues

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

(define-structure queues (export make-queue enqueue dequeue
				 queue-empty-p queuep
				 queue->list queue-length
				 delete-from-queue)
    (open rep)

  (define type-id (cons))

  (define-datum-printer type-id (lambda (q stream)
				  (write stream "#<queue>")))

  (define (make-queue)
    (make-datum '() type-id))

  (define (enqueue q x)
    (datum-set q type-id (nconc (datum-ref q type-id) (list x))))

  (define (dequeue q)
    (let ((data (datum-ref q type-id)))
      (if (null data)
	  (error "Can't dequeue from empty queue")
	(datum-set q type-id (cdr data))
	(car data))))

  (define (queue-empty-p q)
    (null (datum-ref q type-id)))

  (define (queuep q)
    (has-type-p q type-id))

  (define (queue->list q)
    (datum-ref q type-id))

  (define (queue-length q)
    (length (queue->list q)))

  (define (delete-from-queue q x)
    (datum-set q type-id (delq x (datum-ref q type-id)))))
	