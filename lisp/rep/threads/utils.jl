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


;; variables

;; Each thread is [thread NAME CONTINUATION]

(defvar all-threads nil
  "List of all runnable threads. The currently running thread is at the head
of the list, followed by the next-to-run thread, and so on.")

(defvar current-thread nil
  "The currently running thread, or nil if threaded execution hasn't
started yet")

(defun make-thread (thunk &optional name)
  "Create and return an object representing a new thread of execution. The
new thread will begin by calling THUNK, a function with zero parameters.
If NAME is specified it is taken as the name of the new thread."
  (or (call/cc (lambda (cont)
		 (let
		     ((thread (vector 'thread name cont)))
		   (setq all-threads (nconc all-threads (list thread)))
		   thread)))
      ;; always invoke thread continuations with nil arg
      (thunk)))

(defun thread-invoke ()
  "Pass control to the thread at the head of the list of all threads. This
function should usually only be called once, to enter threaded execution
after creating the initial thread(s)."
  (or all-threads (error "No thread to invoke"))
  (if current-thread
      ;; need to save state of current thread
      (call/cc (lambda (cont)
		 (aset current-thread 2 cont)
		 (when (setq current-thread (car all-threads))
		   ((aref current-thread 2) nil))))
    (when (setq current-thread (car all-threads))
      ((aref current-thread 2) nil))))

(defun threadp (arg)
  "Return `t' if ARG is an object representing a thread of execution."
  (and (vectorp arg) (>= (length arg) 3) (eq (aref arg 0) 'thread)))

(defun delete-thread (&optional thread)
  "Remove the thread of execution represented by THREAD. If THREAD is the
currently active thread, then control will pass to the next thread in the
queue. If THREAD is undefined, the currently executing thread is deleted."
  (unless thread
    (setq thread current-thread))
  (when thread
    (setq all-threads (delq thread all-threads))
    (when (eq current-thread thread)
      (thread-invoke))))

(defun thread-name (thread)
  "Return the name of the thread of execution THREAD, or `nil' if THREAD has
no known name."
  (aref thread 1))

(defun thread-yield ()
  "Pass control away from the current thread if other threads are waiting to
run."
  (when current-thread
    (setq all-threads (nconc (delq current-thread all-threads)
			     (list current-thread))))
  (when (and all-threads (not (eq (car all-threads) current-thread)))
    (thread-invoke)))


;; semaphores

;; Each semaphore is (semaphore [OWNING-THREAD [BLOCKED-THREADS...]])

(defun make-semaphore ()
  "Create and return a semaphore object. No thread will own the new semaphore."
  (list 'semaphore))

(defun semaphorep (arg)
  "Return `t' if ARG is a semaphore object."
  (eq (car arg) 'semaphore))

(defun obtain-semaphore (sem)
  "Obtain the semaphore SEM for the current thread. Will suspend the current
thread until the semaphore is available."
  (if (null (cdr sem))
      (rplacd sem (list current-thread))
    (rplacd sem (nconc (rplacd sem) (list current-thread)))
    (setq all-threads (delq current-thread all-threads))
    (thread-yield)))

(defun maybe-obtain-semaphore (sem)
  "Attempt to obtain semaphore SEM for the current thread without blocking.
Returns `t' if able to obtain the semaphore, `nil' otherwise."
  (if (cdr sem)
      nil
    (obtain-semaphore sem)
    t))

(defun release-semaphore (sem)
  "Release the semaphore object SEM (which should have previously been obtained
by the current thread). Returns `t' if the semaphore has no new owner."
  (or (eq (cdr sem) current-thread)
      (error "Not owner of semaphore: %S, %S" sem current-thread))
  (let
      ((cell (cdr sem)))
    (rplacd sem (cdr cell))
    (if (cdr sem)
	(progn
	  (rplaca cell (car sem))
	  (rplacd cell nil)
	  (setq all-threads (nconc all-threads cell))
	  nil)
      t)))
