#| records.jl -- record types

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

;; Commentary:

;; This was inspired by the Scheme48 record interface (surprise,
;; surprise!). You do something like:

;; (define-record-type :pare
;;   (kons x y)				; constructor
;;   pare?				; predicate
;;   (x kar set-kar!)			; fields w/ optional accessors
;;   (y kdr))				;and modifiers

;; the variable `:pare' is bound to the record type. This can be used
;; to redefine the printed representation of the record type (e.g. by
;; default `#<:pare>') using define-record-discloser:

;; (define-record-discloser :pare
;;   (lambda (x) `(pare ,(kar x) ,(kdr x))))

;; General syntax of define-record-type is:

;; (define-record-type <type-name>
;;   (<constructor-name> <field-tags>*)
;;   [<predicate-name>]
;;   (<field-tag> [<accessor-name> [<modifier-name>]])*)

(define-structure records (export make-record-type
				  record-constructor
				  record-accessor
				  record-modifier
				  record-predicate
				  record-printer
				  define-record-type
				  define-record-discloser)
    (open rep)

;;; record type structures

  (define (make-record-type name fields)
    (let ((rt (vector name fields nil)))
      (define-datum-printer rt (record-printer rt))
      rt))

  (define (record-type-name rt) (aref rt 0))
  (define (record-type-fields rt) (aref rt 1))
  (define (record-type-discloser rt) (aref rt 2))
  (define (define-record-discloser rt x) (aset rt 2 x))

;;; record mechanics

  (define (make-record rt)
    (make-datum (make-vector (length (record-type-fields rt))) rt))

  (define (field-index rt field)
    (do ((i 0 (1+ i))
	 (fields (record-type-fields rt) (cdr fields)))
	((eq (car fields) field) i)
      (and (null fields) (error "No such field: %s, %s"
				(record-type-name rt) field))))
  
  (define (field-ref rt record index)
    (aref (datum-ref record rt) index))

  (define (field-set rt record index value)
    (aset (datum-ref record rt) index value))

;;; interface implementations

  (define (record-constructor rt fields)
    (let ((indices (mapcar (lambda (field)
			     (field-index rt field)) fields)))
      (lambda args
	(let ((record (make-record rt)))
	  (let loop ((rest args)
		     (ids indices))
	    (if (and rest ids)
		(progn
		  (field-set rt record (car ids) (car rest))
		  (loop (cdr rest) (cdr ids)))
	      record))))))

  (define (record-accessor rt field)
    (let ((index (field-index rt field)))
      (lambda (record)
	(field-ref rt record index))))

  (define (record-modifier rt field)
    (let ((index (field-index rt field)))
      (lambda (record value)
	(field-set rt record index value))))

  (define (record-predicate rt)
    (lambda (arg)
      (has-type-p arg rt)))

  (define (record-printer rt)
    (lambda (record stream)
      (if (record-type-discloser rt)
	  (prin1 ((record-type-discloser rt) record) stream)
	(format stream "#<%s>" (record-type-name rt)))))

;;; syntax

  (defmacro define-record-type (rt constructor . fields)
    (let (names predicate-defs accessor-defs modifier-defs)
      (when (and fields (symbolp (car fields)))
	(setq predicate-defs `((define ,(car fields) (record-predicate ,rt))))
	(setq fields (cdr fields)))
      (setq names (mapcar car fields))
      (mapc (lambda (field)
	      (when (cadr field)
		(setq accessor-defs
		      (cons `(define ,(cadr field)
			       (record-accessor ,rt ',(car field)))
			    accessor-defs)))
	      (when (caddr field)
		(setq modifier-defs
		      (cons `(define ,(caddr field)
			       (record-modifier ,rt ',(car field)))
			    modifier-defs))))
	    fields)
      `(progn
	 (define ,rt (make-record-type ',rt ',names))
	 (define ,(car constructor)
	   (record-constructor ,rt ',(cdr constructor)))
	 ,@predicate-defs
	 ,@accessor-defs
	 ,@modifier-defs))))
