#| rep.test.framework -- module to allow other modules to test themselves

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

(define-structure rep.test.framework

    (export assert
	    check
	    test

	    set-self-test-action
	    disable-self-tests
	    define-self-test
	    autoload-self-test
	    run-all-self-tests
	    run-module-self-tests
	    run-self-tests-and-exit

	    ;; private functions used in macros
	    self-test/failed
	    self-test/disabledp)

    (open rep
	  rep.util.autoloader
	  rep.data.symbol-table)

  (define action-alist '((test . ())
			 (check . ())
			 (assertion . signal)))

  (define disabled '())

  (define self-tests (make-symbol-table))

  (define failed-tests (make-fluid))


;;; random internal functions

  (define (abort-if-fatal type message)
    (let ((action (cdr (assq type action-alist))))
      (case action
	((exit)
	 (throw 'exit 1))
	((signal)
	 (signal (intern (concat (symbol-name type) "-failed"))
		 (and message (list message)))))))

  (define (self-test/failed type message)
    (format standard-error "\n ** %s failed: %s\n\n"
	    (capitalize-string (symbol-name type)) message)
    (when (and (eq type 'test) (fluid failed-tests))
      (fluid-set failed-tests (1+ (fluid failed-tests))))
    (abort-if-fatal type message))

  (define (self-test/disabledp type) (memq type disabled))


;;; configuration

  (define (set-self-test-action type action)
    (let ((cell (assq type action-alist)))
      (if cell
	  (rplacd cell action)
	(setq action-alist (cons (cons type action) action-alist)))))

  (define (disable-self-tests type)
    (unless (memq type disabled)
      (setq disabled (cons type disabled))))


;;; test management

  (define (ref-1 x) (symbol-table-ref self-tests x))
  (define (set-1 x y) (symbol-table-set self-tests x y))
  (define (walk f) (symbol-table-walk f self-tests))

  ;; initialize autoloading
  (define define-self-test set-1)
  (define autoload-self-test (make-autoloader ref-1 set-1))
  (define self-test-ref (autoloader-ref ref-1))

  (define (run-all-self-tests)
    (let ((failures 0))
      (walk (lambda (x)
	      (setq failures (+ failures (run-module-self-tests x)))))
      failures))

  (define (run-module-self-tests module)
    (let ((test-case (self-test-ref module)))
      (if (not test-case)
	  0
	(format standard-error "%s\n" module)
	(let-fluids ((failed-tests 0))
	  (test-case)
	  (fluid failed-tests)))))

  (define (run-self-tests-and-exit)
    (let ((failures (run-all-self-tests)))
      (throw 'quit (if (zerop failures) 0 1))))


;;; test macros

  (defmacro assert (form)
    `(or (self-test/disabledp 'assertion)
	 ,form (self-test/failed 'assertion ,(prin1-to-string form))))

  (defmacro check (form)
    `(or (self-test/disabledp 'check)
	 ,form (self-test/failed 'check ,(prin1-to-string form))))

  (defmacro test (form)
    `(or (self-test/disabledp 'test)
	 ,form (self-test/failed 'test ,(prin1-to-string form))))


;;; load autoloads

  (load "rep/test/autoload"))
