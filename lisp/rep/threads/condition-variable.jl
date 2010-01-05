#| condition-variable.jl -- condition variables

   $Id$

   Copyright (C) 2000 John Harper <jsh@users.sourceforge.net>

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

(define-structure rep.threads.condition-variable

    (export make-condition-variable
	    condition-variable-p
	    condition-variable-wait
	    condition-variable-signal
	    condition-variable-broadcast)

    (open rep
	  rep.data.datums
	  rep.threads
	  rep.threads.utils
	  rep.threads.mutex)

  (define key (cons))

  (define (make-condition-variable) (make-datum '() key))
  (define (condition-variable-p arg) (has-type-p arg key))

  (define-datum-printer key (lambda (arg stream)
			      (declare (unused arg))
			      (write stream "#<condition-variable>")))

  (define (cv-ref cv) (datum-ref cv key))
  (define (cv-set cv x) (datum-set cv key x))

  (define (condition-variable-wait cv mutex #!optional timeout)
    (let ((thread (current-thread))
	  (acquired nil))
      (unless (memq thread (cv-ref cv))
	(cv-set cv (cons thread (cv-ref cv))))
      (without-interrupts
       ;; these two operations are atomic to prevent people
       ;; signalling the condition before we actually suspend
       (release-mutex mutex)
       (setq acquired (not (thread-suspend thread timeout))))
      (obtain-mutex mutex)
      acquired))

  (define (condition-variable-signal cv)
    (when (cv-ref cv)
      (let ((thread (last (cv-ref cv))))
	(cv-set cv (delq thread (cv-ref cv)))
	(thread-wake thread))))

  (define (condition-variable-broadcast cv)
    (let ((threads (cv-ref cv)))
      (cv-set cv '())
      ;; wake in fifo order
      (mapc thread-wake (nreverse threads)))))


#| Test program:

(structure ()

  (open	rep
	rep.system
	rep.threads
	rep.threads.mutex
	rep.threads.condition-variable)

  (define mutex (make-mutex))
  (define access (make-condition-variable))
  (define count 0)
  (define data 0)

  (define (producer n)
    (do ((i 1 (+ i 1)))
	((> i n))
      (obtain-mutex mutex)
      (while (= count 1)
	(condition-variable-wait access mutex))
      (setq data i)
      (setq count (1+ count))
      (condition-variable-signal access)
      (release-mutex mutex)))

  (define (consumer n)
    (do ((i 1 (+ i 1)))
	((> i n))
      (obtain-mutex mutex)
      (while (= count 0)
	(condition-variable-wait access mutex))
      (format standard-error "consumed: %d\n" data)
      (setq count (1- count))
      (condition-variable-signal access)
      (release-mutex mutex)))

  (let* ((arg (get-command-line-option "--num" t))
	 (n (if arg (string->number arg) 5))
	 (c (make-thread (lambda () (consumer n)))))

    ;; run the producer thread..
    (producer n)

    ;; ..then wait for the consumer to terminate
    (thread-join c)))
|#
