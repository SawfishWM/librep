#| rep-packages.jl -- built-in structures

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
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(declare (in-module rep))

;; Despite the fact that these things are separated, they're currently
;; implemented in one bundle, the all-encompassing rep structure. I
;; should change this, define individual structures then import them
;; into rep


;; built-in interfaces

(define-interface rep-interpreter
  (export eval apply lambda max-lisp-depth subr-name special-form-p
	  macrop functionp bytecodep subrp signal condition-case
	  macroexpand make-closure closure-function closurep
	  set-closure-function closure-structure closurep
	  set-special-environment load error declare))

(define-interface rep-interpreter-debug
  (export break step backtrace debug-frame-environment
	  debug-outer-frame debug-inner-frame
	  trace untrace debug-entry debug-error-entry
	  default-error-handler))

(define-interface rep-bytecode
  (export run-byte-code validate-byte-code make-byte-code-subr))

(define-interface rep-lisp
  (export t nil funcall progn prog1 while cond case quote catch throw
	  unwind-protect with-object featurep provide require setq
	  %load-suffixes

	  ;; init.jl
	  defmacro defun defconst defsubst function let let* letrec
	  if when unless or and setq-default define-value do prog2
	  make-autoload autoload autoload-macro load-all
	  eval-when-compile dynamic-wind make-guardian

	  ;; backquote.jl
	  backquote

	  ;; define.jl
	  define with-internal-definitions))

(define-interface rep-symbols
  (export make-symbol make-obarray find-symbol intern-symbol intern
	  unintern defvar symbol-value set setplist symbol-name
	  default-value default-boundp set-default boundp symbol-plist
	  gensym symbolp makunbound get put make-binding-immutable
	  binding-immutable-p make-variable-special special-variable-p
	  obarray apropos))

(define-interface rep-structures
  (export define-interface %make-interface %parse-interface
	  define-structure structure %make-structure
	  structure-ref %external-structure-ref))

(define-interface rep-structure-internals
  (export %make-interface %parse-interface %make-structure
	  %structure-ref %structure-bound-p %structure-set
	  %external-structure-ref
	  %structure-name %structure-interface %structure-exports-p
	  %structure-imports %structure-accessible %set-interface
	  %get-structure %intern-structure %open-structures
	  %access-structures %current-structure %structurep
	  %make-closure-in-structure %structure-walk %load-autoload))

(define-interface rep-data
  (export cons car cdr list list* make-list append nconc rplaca rplacd
	  reverse nreverse assoc assq rassoc rassq nth nthcdr last
	  mapcar mapc filter member memq memql delete delq delete-if
	  delete-if-not vector make-vector arrayp aset aref make-string
	  substring concat length copy-sequence elt not equal eq
	  string-head-eq string-equal string-lessp = /= > >= < <= max min
	  null atom consp listp stringp vectorp sequencep
	  garbage-collect garbage-threshold idle-garbage-threshold
	  make-datum define-datum-printer datum-ref datum-set has-type-p

	  random translate-string alpha-char-p upper-case-p lower-case-p
	  digit-char-p alphanumericp space-char-p char-upcase char-downcase
	  complete-string upcase-table downcase-table flatten-table

	  make-fluid fluid fluid-set with-fluids fluid-let

	  ;; init.jl
	  setcar setcdr string= string< nop identity interactive _
	  caar cdar cadr cddr
	  caaar cdaar cadar cddar caadr cdadr caddr cdddr
	  caaaar cadaar caadar caddar caaadr cadadr caaddr cadddr
	  cdaaar cddaar cdadar cdddar cdaadr cddadr cdaddr cddddr

	  ;; string-util.jl
	  string-upper-case-p string-lower-case-p string-capitalized-p
	  string-upcase string-downcase capitalize-string mapconcat

	  ;; sort.jl
	  sort))

(define-interface rep-numeric
  (export + - * / remainder mod quotient lognot eql logior logxor
	  logand zerop 1+ 1- ash floor ceiling truncate round exp
	  log sin cos tan asin acos atan sqrt expt gcd numberp integerp
	  fixnump rationalp realp exactp inexactp exact->inexact
	  inexact->exact numerator denominator positivep negativep
	  oddp evenp abs lcm % modulo lsh string->number number->string))

(define-interface rep-streams
  (export write read-char peek-char read-chars read-line copy-stream
	  read print prin1 princ format make-string-input-stream
	  make-string-output-stream get-output-stream-string streamp
	  input-stream-p output-stream-p prin1-to-string read-from-string))

(define-interface rep-continuations
  (export call/cc call-with-current-continuation continuation-callable-p
	  call-with-object call-with-dynamic-root call-with-barrier
	  make-thread thread-yield thread-delete thread-suspend
	  thread-wake threadp thread-suspended-p thread-exited-p
	  current-thread all-threads thread-forbid thread-permit
	  thread-name with-threads-blocked))

(define-interface rep-files
  (export filep file-binding file-bound-stream file-handler-data
	  set-file-handler-data file-name-absolute-p expand-file-name
	  local-file-name canonical-file-name file-name-nondirectory
	  file-name-directory file-name-as-directory directory-file-name
	  set-input-handler open-file make-file-from-stream close-file
	  flush-file seek-file delete-file rename-file copy-file
	  make-directory delete-directory file-readable-p file-writable-p
	  file-exists-p file-regular-p file-directory-p file-symlink-p
	  file-owner-p file-nlinks file-size file-modes set-file-modes
	  file-modes-as-string file-modtime directory-files read-symlink
	  make-symlink stdin-file stdout-file stderr-file make-temp-name
	  set-file-handler-environment file-newer-than-file-p file-name=
	  autoload-file-handler define-file-handler file-handler-ref
	  write-buffer-contents read-file-contents insert-file-contents))

(define-interface rep-regexp
  (export string-match string-looking-at expand-last-match
	  match-start match-end quote-regexp regexp-cache-control
	  assoc-regexp))

(define-interface rep-system
  (export recursive-edit recursion-depth repl get-command-line-option
	  beep current-time current-utime fix-time current-time-string
	  time-later-p sleep-for sit-for user-login-name user-full-name
	  user-home-directory system-name message system pwd-prompt
	  call-hook add-hook remove-hook
	  in-hook-p eval-after-load getenv setenv unsetenv
	  operating-system rep-version rep-interface-id rep-build-id))

(define-interface rep-process
  (export make-process start-process call-process interrupt-process
	  kill-process stop-process continue-process signal-process
	  process-exit-status process-exit-value process-id
	  process-running-p process-stopped-p process-in-use-p
	  processp process-prog set-process-prog process-args
	  set-process-args process-output-stream set-process-output-stream
	  process-error-stream set-process-error-stream process-function
	  set-process-function process-dir set-process-dir
	  process-connection-type set-process-connection-type
	  active-processes accept-process-output))


;; combine all built-in interfaces into a single structure `rep'

(define-interface rep (compound-interface rep-interpreter
					  rep-interpreter-debug
					  rep-bytecode
					  rep-lisp
					  rep-symbols
					  rep-structures
					  rep-data
					  rep-continuations
					  rep-files
					  rep-regexp
					  rep-system
					  rep-numeric
					  rep-streams
					  rep-process))

(%set-interface (%get-structure 'rep) (%parse-interface 'rep))


;; some more useful modules

(define-interface module-system
  (compound-interface rep-structures (export lambda
					     validate-byte-code
					     run-byte-code
					     load %load-suffixes)))

;; this must be before the first use of `structure' or `define-structure'
(%make-structure (%parse-interface 'module-system)
		 (lambda () (%open-structures '(rep)))
		 nil 'module-system)

(define-structure structure-refs (export structure-ref) (open rep))

(define-structure structure-internals rep-structure-internals)

(let ((struct (%get-structure 'structure-internals)))
  (mapc (lambda (x)
	  (%structure-set struct x (%structure-ref (%current-structure) x)))
	(%structure-interface struct)))
