#| rep.test.data -- checks for rep.data module

   $Id$

   Copyright (C) 2001 John Harper <jsh@users.sourceforge.net>

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

(define-structure rep.data.self-tests ()

    (open rep
	  rep.data.records
	  rep.test.framework)

;;; equality function tests

  ;; adapted from guile's test.scm
  (define (equality-self-test)
    (define (gen-counter)
      (let ((n 0))
	(lambda () (setq n (1+ n)) n)))

    (test (eql 'a 'a))
    (test (not (eql 'a 'b)))
    (test (eql 2 2))
    (test (eql '() '()))
    (test (eql '10000 '10000))
    (test (not (eql (cons 1 2) (cons 1 2))))
    (test (not (eql (lambda () 1) (lambda () 2))))

    (let ((p (lambda (x) x)))
      (test (eql p p)))
    (let ((g (gen-counter)))
      (test (eql g g)))
    (test (not (eql (gen-counter) (gen-counter))))
    (letrec ((f (lambda () (if (eql f g) 'f 'both)))
	     (g (lambda () (if (eql f g) 'g 'both))))
      (test (not (eql f g))))

    (test (eq 'a 'a))
    (test (not (eq (list 'a) (list 'a))))
    (test (eq '() '()))
    (test (eq car car))
    (let ((x '(a)))
      (test (eq x x)))
    (let ((x '()))
      (test (eq x x)))
    (let ((x (lambda (x) x)))
      (test (eq x x)))

    (test (equal 'a 'a))
    (test (equal '(a) '(a)))
    (test (equal '(a (b) c) '(a (b) c)))
    (test (equal "abc" "abc"))
    (test (equal 2 2))
    (test (equal (make-vector 5 'a) (make-vector 5 'a))))

;;; cons and list tests

  ;; adapted from guile's test.scm
  (define (cons-self-test)
    (test (consp '(a . b)))
    (test (consp '(a . 1)))
    (test (consp '(a b c)))
    (test (not (consp '())))
    (test (not (consp '#(a b))))

    (test (equal '(a) (cons 'a '())))
    (test (equal '((a) b c d) (cons '(a) '(b c d))))
    (test (equal '("a" b c) (cons "a" '(b c))))
    (test (equal '(a . 3) (cons 'a 3)))
    (test (equal '((a b) . c) (cons '(a b) 'c)))

    (test (equal 'a (car '(a b c))))
    (test (equal '(a) (car '((a) b c d))))
    (test (equal 1 (car '(1 . 2))))

    (test (equal '(b c d) (cdr '((a) b c d))))
    (test (equal 2 (cdr '(1 . 2))))

    (test (equal '(a 7 c) (list 'a (+ 3 4) 'c)))
    (test (equal '() (list)))

    (test (= 3 (length '(a b c))))
    (test (= 3 (length '(a (b) (c d e)))))
    (test (= 0 (length '())))

    (test (equal '(x y) (append '(x) '(y))))
    (test (equal '(a b c d) (append '(a) '(b c d))))
    (test (equal '(a (b) (c)) (append '(a (b)) '((c)))))
    (test (equal '() (append)))
    (test (equal '(a b c . d) (append '(a b) '(c . d))))
    (test (equal 'a (append '() 'a)))

    (test (equal '(c b a) (reverse '(a b c))))
    (test (equal '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f))))))

    (test (equal 'c (nth 2 '(a b c d))))

    (test (equal '(a b c) (memq 'a '(a b c))))
    (test (equal '(b c) (memq 'b '(a b c))))
    (test (null (memq 'a '(b c d))))
    (test (null (memq (list 'a) '(b (a) c))))
    (test (equal '((a) c) (member (list 'a) '(b (a) c))))
    (test (equal '(101 102) (memql 101 '(100 101 102))))

    (let ((e '((a 1) (b 2) (c 3))))
      (test (equal '(a 1) (assq 'a e)))
      (test (equal '(b 2) (assq 'b e)))
      (test (null (assq 'd e))))
    (test (null (assq (list 'a) '(((a)) ((b)) ((c))))))
    (test (equal '((a)) (assoc (list 'a) '(((a)) ((b)) ((c))))))
    (test (equal '(5 7) (assq 5 '((2 3) (5 7) (11 13))))))

;;; tests for rep.data.records

  (define-record-type :pare
    (kons x y)
    pare?
    (x kar set-kar!)
    (y kdr))

  (define-record-discloser :pare
    (lambda (x) (format nil "#<pare %s %s>" (kar x) (kdr x))))

  (define (record-self-test)
    (define pare (kons 1 2))
    (test pare)
    (test (pare? pare))
    (test (eql (kar pare) 1))
    (test (eql (kdr pare) 2))

    (set-kar! pare 3)
    (test (eql (kar pare) 3)))

;;; string-util tests

  (define (string-util-self-test)
    (test (string-upper-case-p "FOO"))
    (test (not (string-upper-case-p "Foo")))
    (test (not (string-upper-case-p "foo")))
    (test (not (string-upper-case-p "123")))

    (test (string-lower-case-p "foo"))
    (test (not (string-lower-case-p "Foo")))
    (test (not (string-lower-case-p "FOO")))
    (test (not (string-lower-case-p "123")))

    (test (string-capitalized-p "Foo"))
    (test (string-capitalized-p "FOO"))
    (test (not (string-capitalized-p "foo")))

    (test (string= (string-upcase "foo") "FOO"))
    (test (string= (string-upcase "FOO") "FOO"))
    (test (string= (string-upcase "Foo") "FOO"))
    (test (string= (string-upcase "123") "123"))

    (test (string= (string-downcase "FOO") "foo"))
    (test (string= (string-downcase "foo") "foo"))
    (test (string= (string-downcase "Foo") "foo"))
    (test (string= (string-downcase "123") "123"))

    (test (string= (capitalize-string "FOO") "FOO"))
    (test (string= (capitalize-string "foo") "Foo"))
    (test (string= (capitalize-string "Foo") "Foo"))
    (test (string= (capitalize-string "123") "123"))

    (test (string= (mapconcat identity '("foo" "bar" "baz") " ")
		   "foo bar baz"))
    (test (string= (mapconcat identity '("foo" "bar" "baz") #\space)
		   "foo bar baz"))
    (test (string= (mapconcat identity '() #\space) ""))
    (test (string= (mapconcat string-upcase '("foo" "bar" "baz") " ")
		   "FOO BAR BAZ")))

  (define (self-test)
    (equality-self-test)
    (cons-self-test)
    (record-self-test)
    (string-util-self-test))

  ;;###autoload
  (define-self-test 'rep.data self-test))
