;; threads.jl -- first attempt at cooperative multi-threading
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

(provide 'threads)

;; Commentary:

;; This module uses rep's continuations to provide a simple threading
;; mechanism. It really is _very_ simple, probably needs more work to
;; allow useful work to be done

;; Here's an example using them:

;; (require 'threads)
;;
;; (defun thread-fun (id)
;;   (let
;;       ((index 0))
;;     (while t
;;       (format standard-output "thread-%s: %8d\n" id index)
;;       (setq index (1+ index))
;;       (thread-yield))))
;;
;; (setq thread-1 (make-thread (lambda () (thread-fun 1)) 'thread-1))
;; (setq thread-2 (make-thread (lambda () (thread-fun 2)) 'thread-2))
;;
;; (thread-invoke)


;; mutexes

;; Each mutex is (mutex [OWNING-THREAD [BLOCKED-THREADS...]])

(defun make-mutex ()
  "Create and return a mutex object. No thread will own the new mutex."
  (list 'mutex))

(defun mutexp (arg)
  "Return `t' if ARG is a mutex object."
  (eq (car arg) 'mutex))

(defun obtain-mutex (mtx)
  "Obtain the mutex MTX for the current thread. Will suspend the current
thread until the mutex is available."
  (if (null (cdr mtx))
      (rplacd mtx (list current-thread))
    (rplacd mtx (nconc (rplacd mtx) (list current-thread)))
    (thread-suspend (current-thread))))

(defun maybe-obtain-mutex (mtx)
  "Attempt to obtain mutex MTX for the current thread without blocking.
Returns `t' if able to obtain the mutex, `nil' otherwise."
  (if (cdr mtx)
      nil
    (obtain-mutex mtx)
    t))

(defun release-mutex (mtx)
  "Release the mutex object MTX (which should have previously been obtained
by the current thread). Returns `t' if the mutex has no new owner."
  (or (eq (cdr mtx) (current-thread))
      (error "Not owner of mutex: %S" mtx))
  (rplacd mtx (cddr mtx))
  (if (cdr mtx)
      (progn
	(thread-wake (cadr mtx))
	nil)
    t))
