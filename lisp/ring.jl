;;;; ring.jl -- Ring buffer support
;;;  Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
;;;  $Id$

;;; This file is part of Jade.

;;; Jade is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; Jade is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with Jade; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'ring)

(defvar ring-default-size 16
  "The default size of a ring buffer.")

;; A ring buffer of size N is defined by a vector with N+2 slots; the
;; first slot is used to store the size of the buffer, the second stores
;; the position of the next slot to be filled.0

(defmacro ring-set-size (ring size)
  (list 'aset ring 0 size))

(defmacro ring-pos (ring)
  (list 'aref ring 1))

(defmacro ring-set-pos (ring pos)
  (list 'aset ring 1 pos))

(defmacro ring-item (ring pos)
  (list 'aref ring (list '+ pos 2)))

(defmacro ring-set-item (ring pos val)
  (list 'aset ring (list '+ pos 2) val))


;; Public functions

(defmacro ring-capacity (ring)
  "Returns the number of slots in the ring buffer RING."
  (list '- (list 'length ring) 2))

(defmacro ring-size (ring)
  "Returns the number of filled slots in the ring buffer RING."
  (list 'aref ring 0))

(defun make-ring (&optional size)
  "Create a ring buffer that can contain SIZE values. If SIZE is not
specified the default capacity `ring-default-size' is used."
  (unless (numberp size)
    (setq size ring-default-size))
  (let
      ((buf (make-vector (+ size 2))))
    (ring-set-size buf 0)
    (ring-set-pos buf 0)
    buf))

(defun add-to-ring (ring object)
  "Append OBJECT to the ring buffer RING. This may overwrite a previously
added object."
  (ring-set-item ring (ring-pos ring) object)
  (let
      ((new-pos (1+ (ring-pos ring))))
    (when (= new-pos (ring-capacity ring))
      (setq new-pos 0))
    (unless (= (ring-size ring) (ring-capacity ring))
      (ring-set-size ring (1+ (ring-size ring))))
    (ring-set-pos ring new-pos)))

(defun get-from-ring (ring &optional depth)
  "Read an object from the ring buffer RING. If DEPTH is non-nil it
defines the object to access, the most recently added item is at
depth one, the next at depth two, and so on. If there is no item at
DEPTH nil is returned."
  (unless (numberp depth)
    (setq depth 1))
  (if (> depth (ring-capacity ring))
      nil
    (let
	((pos (- (ring-pos ring) depth)))
      (when (< pos 0)
	(setq pos (+ (ring-capacity ring) pos)))
      (ring-item ring pos))))

(defun set-ring-head (ring object)
  "Replaces the most recently added object in ring buffer RING with OBJECT.
If RING contains no items, add OBJECT as the first."
  (if (zerop (ring-size ring))
      (add-to-ring ring object)
    (let
	((pos (1- (ring-pos ring))))
      (when (< pos 0)
	(setq pos (+ (ring-capacity ring) pos)))
      (ring-set-item ring pos object))))
