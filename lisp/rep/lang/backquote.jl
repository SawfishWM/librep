;;;; backquote.jl --- implement the ` Lisp construct
;;; $Id$

;;; This file originated in GNU Emacs 19.34;
;;; Changes to it were:
;;;	1. Delete backquote-list* definition and replace all
;;;	   calls to this function with calls to primitive list*
;;;	3. Replace car-safe with car (car always safe in Jade)
;;;	4. Remove autoload cookies since Jade doesn't allow
;;;	   autoloaded macros
;;;	5. Remove the use of (` X) for `X, (, X) for ,X and (,@ X)
;;;	   for ,@X since Jade will parse the normal syntax correctly

;;; Copyright (C) 1990, 1992, 1994 Free Software Foundation, Inc.

;; Author: Rick Sladkey <jrs@world.std.com>
;; Maintainer: FSF
;; Keywords: extensions, internal

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(declare (unsafe-for-call/cc))

(define-structure rep.lang.backquote

    (export backquote)

    (open rep)

(defmacro backquote (arg)
  "Argument STRUCTURE describes a template to build.

The whole structure acts as if it were quoted except for certain
places where expressions are evaluated and inserted or spliced in.

For example:

b              => (ba bb bc)		; assume b has this value
`(a b c)       => (a b c)		; backquote acts like quote
`(a ,b c)      => (a (ba bb bc) c)	; insert the value of b
`(a ,@b c)     => (a ba bb bc c)	; splice in the value of b

Vectors work just like lists.  Nested backquotes are permitted."
  (cdr (backquote-process arg)))

;; backquote-process returns a dotted-pair of a tag (0, 1, or 2) and
;; the backquote-processed structure.  0 => the structure is
;; constant, 1 => to be unquoted, 2 => to be spliced in.
;; The top-level backquote macro just discards the tag.

(defun backquote-process (s)
  (cond
   ((vectorp s)
    (let ((n (backquote-process (append s ()))))
      (if (= (car n) 0)
	  (cons 0 s)
	(cons 1 (cond
		 ((eq (nth 1 n) 'list)
		  (cons 'vector (nthcdr 2 n)))
		 ((eq (nth 1 n) 'append)
		  (cons 'vconcat (nthcdr 2 n)))
		 (t
		  (list 'apply '(function vector) (cdr n))))))))
   ((atom s)
    (cons 0 (if (not (symbolp s))
		s
	      (list 'quote s))))
   ((eq (car s) 'backquote-unquote)
    (cons 1 (nth 1 s)))
   ((eq (car s) 'backquote-splice)
    (cons 2 (nth 1 s)))
   ((eq (car s) 'backquote)
    (backquote-process (cdr (backquote-process (nth 1 s)))))
   (t
    (let ((rest s)
	  item firstlist lst lists expression)
      ;; Scan this list-level, setting LISTS to a list of forms,
      ;; each of which produces a list of elements
      ;; that should go in this level.
      ;; The order of LISTS is backwards. 
      ;; If there are non-splicing elements (constant or variable)
      ;; at the beginning, put them in FIRSTLIST,
      ;; as a list of tagged values (TAG . FORM).
      ;; If there are any at the end, they go in LIST, likewise.
      (while (consp rest)
	;; Turn . (, foo) into (,@ foo).
	(if (eq (car rest) 'backquote-unquote)
	    (setq rest (list (list 'backquote-splice (nth 1 rest)))))
	(setq item (backquote-process (car rest)))
	(cond
	 ((= (car item) 2)
	  ;; Put the nonspliced items before the first spliced item
	  ;; into FIRSTLIST.
	  (if (null lists)
	      (setq firstlist lst
		    lst nil))
	  ;; Otherwise, put any preceding nonspliced items into LISTS.
	  (if lst
	      (setq lists (cons (backquote-listify lst '(0 . nil)) lists)))
	  (setq lists (cons (cdr item) lists))
	  (setq lst nil))
	 (t
	  (setq lst (cons item lst))))
	(setq rest (cdr rest)))
      ;; Handle nonsplicing final elements, and the tail of the list
      ;; (which remains in REST).
      (if (or rest lst)
	  (setq lists (cons (backquote-listify lst (backquote-process rest))
			    lists)))
      ;; Turn LISTS into a form that produces the combined list. 
      (setq expression
	    (if (or (cdr lists)
		    (eq (car (car lists)) 'backquote-splice))
		(cons 'append (nreverse lists))
	      (car lists)))
      ;; Tack on any initial elements.
      (if firstlist
	  (setq expression (backquote-listify firstlist (cons 1 expression))))
      (if (eq (car expression) 'quote)
	  (cons 0 (list 'quote s))
	(cons 1 expression))))))

;; backquote-listify takes (tag . structure) pairs from backquote-process
;; and decides between append, list, list*, and cons depending
;; on which tags are in the list.

;; this is just used to unwrap possibly quoted constants
(defun backquote-eval (form)
  (if (eq (car form) 'quote)
      (cadr form)
    form))

(defun backquote-listify (lst old-tail)
  (let ((heads nil) (tail (cdr old-tail)) (list-tail lst) (item nil))
    (if (= (car old-tail) 0)
	(setq tail (backquote-eval tail)
	      old-tail nil))
    (while (consp list-tail)
      (setq item (car list-tail))
      (setq list-tail (cdr list-tail))
      (if (or heads old-tail (/= (car item) 0))
	  (setq heads (cons (cdr item) heads))
	(setq tail (cons (backquote-eval (cdr item)) tail))))
    (cond
     (tail
      (if (null old-tail)
	  (setq tail (list 'quote tail)))
      (if heads
	  (let ((use-list* (or (cdr heads)
			       (and (consp (car heads))
				    (eq (car (car heads))
					'backquote-splice)))))
	    (cons (if use-list* 'list* 'cons)
		  (append heads (list tail))))
	tail))
     (t (cons 'list heads)))))

)

;; backquote.el ends here
