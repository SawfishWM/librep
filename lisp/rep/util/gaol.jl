;; gaol.jl -- iron-boxes for untrusted code
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

(define-structure rep.util.gaol

    (export gaol-define
	    gaol-define-special
	    gaol-define-file-handler
	    gaol-define-vm
	    make-gaol
	    define-gaol-structure
	    gaol-eval
	    gaol-load
	    gaol-open

	    ;; obsolete
	    gaol-rebuild-environment
	    gaol-replace-function
	    gaol-add-special)

    (open rep
	  rep.io.files
	  rep.io.file-handlers
	  rep.regexp
	  rep.system
	  rep.data.datums
	  rep.structures)

  (define-structure-alias gaol rep.util.gaol)

;;; configuration/variables

  ;; list of all safe functions (only those imported into this
  ;; module may be placed in this list)
  (define gaol-safe-functions
    '(nil t % * + - / /= 1+ 1- < <= = > >= add-hook alpha-char-p
      alphanumericp and append apply aref arrayp aset ash assoc
      assoc-regexp assq atom backquote beep boundp bytecodep call-hook
      car caar cadr caaar cdaar cadar cddar caadr cdadr caddr cdddr
      caaaar cadaar caadar caddar caaadr cadadr caaddr cadddr cdaaar
      cddaar cdadar cdddar cdaadr cddadr cdaddr cddddr case catch
      call-with-catch cdr cdar cddr char-downcase char-upcase closurep
      complete-string concat cond condition-case
      call-with-error-handlers cons consp copy-sequence copy-stream
      current-time current-time-string default-boundp default-value
      defconst %define define defmacro defsubst defun defvar
      delete delete-if delete-if-not delq digit-char-p do
      elt eq eql equal error eval eval-when-compile
      expand-last-match featurep filter fix-time
      format funcall function functionp garbage-collect gensym get
      get-output-stream-string getenv identity if integerp interactive
      intern lambda last length let let* letrec list list* listp logand logior
      lognot logxor lower-case-p lsh macroexpand macrop
      make-closure make-list make-string make-string-input-stream
      make-string-output-stream make-symbol make-vector
      makunbound mapc mapcar match-end match-start max member memq memql
      message min mod nconc nop not nreverse nth nthcdr null numberp or
      prin1 prin1-to-string princ print prog1 prog2 progn put quote
      quote-regexp random rassoc rassq read read-char read-chars
      read-from-string read-line reverse rplaca rplacd sequencep set
      set-default setcar setcdr setplist setq setq-default
      signal sit-for sleep-for sort space-char-p special-form-p
      special-variable-p streamp string-equal string-head-eq
      string-lessp string-looking-at string-match string-split
      string-replace string< string=
      stringp subr-name subrp substring symbol-name symbol-plist
      symbol-value symbolp system-name throw time-later-p
      translate-string unless unwind-protect call-with-unwind-protect
      upper-case-p user-full-name user-login-name vector vectorp when
      while with-internal-definitions with-object write

      zerop remainder quotient modulo floor ceiling truncate round exp
      log sin cos tan asin acos atan sqrt expt gcd fixnump rationalp
      realp exactp inexactp exact->inexact inexact->exact numerator
      denominator positivep negativep oddp evenp abs lcm

      make-datum define-datum-printer datum-ref datum-set has-type-p

      make-fluid fluid fluid-set with-fluids let-fluids

      string->number number->string mapconcat string-upper-case-p
      string-lower-case-p string-capitalized-p string-upcase string-downcase
      capitalize-string mapconcat

      ;; make-timer delete-timer set-timer
      ;; make-table make-weak-table string-hash symbol-hash eq-hash
      ;; equal-hash tablep table-ref table-set table-unset table-walk

      downcase-table flatten-table upcase-table operating-system
      rep-version))

  ;; table containing all variables accessible by gaolled code
  (define gaol-structure nil)

  ;; list of accessible special variables
  (define gaol-safe-specials
    (list 'file-handler-alist 'load-filename 'macro-environment))

  ;; list of file handlers that may be called. These functions shouldn't
  ;; be added to the function environment, since that would allow _any_
  ;; file operation to be performed
  (define gaol-safe-file-handlers '(tilde-file-handler tar-file-handler))

  ;; alist of file handlers
  (define file-handler-env nil)

  ;; function providing the virtual machine, or nil
  (define byte-code-interpreter nil)

;;; building the actual environments

  ;; initialization
  (define (build-structure)
    (unless gaol-structure
      (setq gaol-structure (make-structure))
      (name-structure gaol-structure '%gaol)
      (structure-exports-all gaol-structure t)
      (mapc (lambda (var)
	      (structure-define gaol-structure var
				(%structure-ref (current-structure) var)))
	    gaol-safe-functions)
      (setq file-handler-env (mapcar (lambda (sym)
				       (cons sym t))
				     gaol-safe-file-handlers))))

  (defun make-gaol ()
    (build-structure)
    (declare (bound %open-structures))
    (let ((gaol (make-structure '() (lambda () (%open-structures '(%gaol))))))
      (set-file-handler-environment file-handler-env gaol)
      (set-special-environment gaol-safe-specials gaol)
      (structure-install-vm gaol byte-code-interpreter)
      (call-hook '*make-gaol-hook* (list gaol))
      gaol))

  (define (define-gaol-structure name gaol) (name-structure gaol name))

  (define default-gaol (let (gaol)
			 (lambda ()
			   (unless gaol
			     (setq gaol (make-gaol)))
			   gaol)))

;;; public environment mutators

  (define (gaol-define var value)
    (build-structure)
    (structure-define gaol-structure var value))

  (define (gaol-define-special var)
    (build-structure)
    (unless (memq var gaol-safe-specials)
      ;; use nconc to affect existing environments
      (setq gaol-safe-specials (nconc gaol-safe-specials (list var)))))

  (define (gaol-define-file-handler name fun)
    (build-structure)
    (let ((cell (assq name file-handler-env)))
      (if cell
	  (rplacd cell fun)
	(setq file-handler-env (nconc file-handler-env
				      (list (cons name fun)))))))

  ;; only works properly for gaols created after calling this function
  (define (gaol-define-vm run validate)
    (build-structure)
    (gaol-define 'run-byte-code run)
    (gaol-define 'validate-byte-code validate)
    (setq byte-code-interpreter run))

  (define (gaol-open struct)
    (build-structure)
    (eval `(,open-structures '(,struct)) gaol-structure))

;;; evaluating in the restricted environment

  (define (load-in filename struct)
    (let ((file (open-file filename 'read)))
      (unwind-protect
	  (condition-case nil
	      (let ((load-filename (canonical-file-name filename)))
		(while t
		  (eval (read file) struct)))
	    (end-of-stream))
	(close-file file))))

  (define (gaol-eval form #!optional gaol)
    (eval form (or gaol (default-gaol))))

  (define (gaol-load file #!optional gaol)
    (load-in file (or gaol (default-gaol))))

;;; compatibility

  (define (gaol-rebuild-environment))
  (define gaol-replace-function gaol-define)
  (define gaol-add-special gaol-define-special))
