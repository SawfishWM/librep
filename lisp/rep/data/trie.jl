#| trie.jl -- data structure for encoding character lists as a tree

   $Id$

   Copyright (C) 2002 John Harper <jsh@unfactored.org>

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

(define-structure rep.data.trie

    (export make-trie
	    trie-insert-string!
	    trie-string-ref
	    trie-contains-string?
	    trie-foreach
	    make-trie-from-file)

    (open rep
	  rep.io.files
	  rep.regexp)


;; tree nodes

  ;; each node is a list (node (CHAR . VALUE) ...) VALUE is typically
  ;; another tree

  (define (make-node) (list 'node))

  (define (node-ref node key)
    (cdr (assq key (cdr node))))

  (define (node-set! node key v)
    (let ((cell (assq key (cdr node))))
      (if cell
	  (rplacd cell v)
	(rplacd node (cons (cons key v) (cdr node))))))


;; trees of tokens

  (define make-trie make-node)

  ;; returns the sub-tree of the last key, or false
  (define (trie-ref tree keys)
    (if (null keys)
	tree
      (let ((sub-tree (node-ref tree (car keys))))
	(and sub-tree (trie-ref sub-tree (cdr keys))))))

  (define (trie-insert-1! tree key)
    (let ((sub (node-ref tree key)))
      (if (not sub)
	  (let ((new (make-node)))
	    (node-set! tree key new)
	    new)
	sub)))

  ;; returns the sub-tree of the last inserted token
  (define (trie-insert! tree keys)
    (if (not keys)
	tree
      (trie-insert! (trie-insert-1! tree (car keys)) (cdr keys))))


;; string handling

  (defconst word-terminator eow)

  (define (trie-insert-string! tree string)
    (trie-insert-1! (trie-insert! tree (vector->list string)) word-terminator))

  (define (trie-string-ref tree string)
    (trie-ref tree (vector->list string)))

  (define (trie-contains-string? tree string)
    (let ((end (trie-string-ref tree string)))
      (and (node-ref end word-terminator) t)))

  (define (trie-foreach tree callback)
    (define (iter tree tokens)
      (mapc (lambda (x)
	      (if (eq (car x) word-terminator)
		  (callback (apply concat (reverse tokens)))
		(iter (cdr x) (cons (car x) tokens))))
	    (cdr tree)))
    (iter tree '()))

  (define (make-trie-from-file filename #!key callback)
    (let ((file (open-file filename 'read))
	  (tree (make-trie)))
      (unwind-protect
	  (let loop ()
	    (let ((string (read-line file)))
	      (when string
		(when (string-match "\\s+$" string)
		  (setq string (substring string 0 (match-start))))
		(when callback
		  (setq string (callback string)))
		(when string
		  (trie-insert-string! tree string))
		(loop))))
	(close-file file))
      tree)))
