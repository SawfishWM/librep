;; mutex.jl -- thread mutex devices
;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure rep.threads.mutex

    (export make-mutex
	    mutexp
	    obtain-mutex
	    maybe-obtain-mutex
	    release-mutex)

    (open rep
	  rep.threads
	  rep.threads.utils)

  (define-structure-alias mutex rep.threads.mutex)

  ;; Each mutex is (mutex [OWNING-THREAD [BLOCKED-THREADS...]])

  (defun make-mutex ()
    "Create and return a mutex object. No thread will own the new mutex."
    (list 'mutex))

  (defun mutexp (arg)
    "Returns true if ARG is a mutex object."
    (eq (car arg) 'mutex))

  (defun obtain-mutex (mtx #!optional timeout)
    "Obtain the mutex MTX for the current thread. Will suspend the current
thread until the mutex is available. Returns false if the timeout expired."
    (without-interrupts
     (if (null (cdr mtx))
	 (rplacd mtx (list (current-thread)))
       (rplacd mtx (nconc (cdr mtx) (list (current-thread))))
       (not (thread-suspend (current-thread) timeout)))))

  (defun maybe-obtain-mutex (mtx)
    "Attempt to obtain mutex MTX for the current thread without blocking.
Returns true if able to obtain the mutex, false otherwise."
    (without-interrupts
     (if (cdr mtx)
	 nil
       (obtain-mutex mtx)
       t)))

  (defun release-mutex (mtx)
    "Release the mutex object MTX (which should have previously been obtained
by the current thread). Returns true if the mutex has no new owner."
    (or (eq (cadr mtx) (current-thread))
	(error "Not owner of mutex: %S" mtx))
    (without-interrupts
     (rplacd mtx (cddr mtx))
     (if (cdr mtx)
	 (progn
	   (thread-wake (cadr mtx))
	   nil)
       t))))
