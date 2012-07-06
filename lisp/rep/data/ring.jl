#| ring.jl -- ring buffer support

   $Id$

   Copyright (C) 1993, 1994, 2000 John Harper <john@dcs.warwick.ac.uk>

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
   the Free Software Foundation, 51 Franklin Street, Fifth Floor, 
   Boston, MA 02110-1301 USA
|#

(define-structure rep.data.ring

    (export ring-capacity
	    ring-size
	    make-ring
	    ring-append
	    ring-ref
	    ring-replace
	    ring->list
	    add-to-ring
	    get-from-ring
	    set-ring-head)

    (open rep
	  rep.data.datums)

  (define-structure-alias ring rep.data.ring)

  ;; default size of a ring buffer
  (defconst default-size 16)

  ;; key for datum access
  (define key (cons))

  (define-datum-printer key (lambda (d s)
			      (declare (unused d))
			      (write s "#<ring>")))

  ;; A ring buffer of size N is defined by a vector with N+2 slots; the
  ;; first slot is used to store the size of the buffer, the second stores
  ;; the position of the next slot to be filled.

  (define (ring-capacity ring)
    "Returns the number of slots in the ring buffer RING."
    (- (length (datum-ref ring key)) 2))

  (define (ring-size ring)
    "Returns the number of filled slots in the ring buffer RING."
    (aref (datum-ref ring key) 0))

  (define (set-size ring size)
    (aset (datum-ref ring key) 0 size))

  (define (get-pos ring)
    (aref (datum-ref ring key) 1))
  (define (set-pos ring pos)
    (aset (datum-ref ring key) 1 pos))

  (define (get-item ring pos)
    (aref (datum-ref ring key) (+ pos 2)))
  (define (set-item ring pos val)
    (aset (datum-ref ring key) (+ pos 2) val))

;;; higher level public api

  (define (make-ring #!optional size)
    "Create a ring buffer that can contain SIZE values. If SIZE is not
specified the default capacity `ring-default-size' is used."
    (unless size (setq size default-size))
    (let ((ring (make-datum (make-vector (+ size 2)) key)))
      (set-size ring 0)
      (set-pos ring 0)
      ring))

  (define (ring-append ring object)
    "Append OBJECT to the ring buffer RING. This may overwrite a previously
added object."
    (set-item ring (get-pos ring) object)
    (let ((new-pos (mod (1+ (get-pos ring)) (ring-capacity ring))))
      (unless (= (ring-size ring) (ring-capacity ring))
	(set-size ring (1+ (ring-size ring))))
      (set-pos ring new-pos)))

  (define (ring-ref ring #!optional depth)
    "Read an object from the ring buffer RING. If DEPTH is true it
defines the object to access, the most recently added item is at
depth zero, the next at depth one, and so on. If there is no item at
DEPTH nil is returned."
    (unless depth (setq depth 0))
    (if (>= depth (ring-capacity ring))
	nil
      (get-item ring (mod (- (get-pos ring) (1+ depth))
			  (ring-capacity ring)))))

  (define (ring-replace ring object)
    "Replaces the most recently added object in ring buffer RING with OBJECT.
If RING contains no items, add OBJECT as the first."
    (if (zerop (ring-size ring))
	(add-to-ring ring object)
      (set-item ring (mod (1- (get-pos ring)) (ring-capacity ring)) object)))

  (define (ring->list ring)
    "Return the elements in ring buffer RING as a list, newest to oldest."
    (let ((size (ring-size ring))
	  (contents '()))
      (do ((i 0 (1+ i)))
	  ((= i size) (nreverse contents))
	(setq contents (cons (ring-ref ring i) contents)))))

;;; compatibility api

  (define (get-from-ring ring #!optional depth)
    (ring-ref ring (if depth (1- depth) 0)))
  (define add-to-ring ring-append)
  (define set-ring-head ring-replace))
