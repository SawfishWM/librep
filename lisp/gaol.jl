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

(define-structure gaol (export gaol-rebuild-environment
			       gaol-replace-function
			       gaol-add-special
			       gaol-add-file-handler
			       gaol-replace-file-handler
			       gaol-eval
			       gaol-load)

  (open rep structure-internals)


;;; configuration/variables

  ;; list of all safe functions (only those imported into this
  ;; module may be placed in this list)
  (define gaol-safe-functions
    '(nil t % * + - / /= 1+ 1- < <= = > >= add-hook alpha-char-p
      alphanumericp and append apply aref arrayp aset ash assoc
      assoc-regexp assq atom backquote beep boundp bytecodep call-hook
      car caar cadr case catch cdr cdar cddr char-downcase char-upcase
      closurep complete-string concat cond condition-case cons consp
      copy-sequence copy-stream current-time current-time-string
      default-boundp default-value defconst define defmacro defsubst
      defun defvar delete delete-if delete-if-not delete-timer delq
      digit-char-p documentation elt eq eql equal error eval
      eval-when-compile expand-last-match featurep filter fix-time flet
      fmakunbound format funcall function functionp garbage-collect
      gensym get get-output-stream-string getenv identity if integerp
      interactive lambda last length let let* letrec list list* logand
      logior lognot logxor lower-case-p lsh macroexpand macrolet macrop
      make-closure make-list make-string make-string-input-stream
      make-string-output-stream make-symbol make-timer make-vector
      makunbound mapc mapcar match-end match-start max member memq
      message min mod nconc nop not nreverse nth nthcdr null numberp or
      prin1 prin1-to-string princ print prog1 prog2 progn put quote
      quote-regexp random rassoc rassq read read-char read-chars
      read-from-string read-line reverse rplaca rplacd sequencep set
      set-default set-timer setcar setcdr setplist setq setq-default
      signal sit-for sleep-for sort space-char-p special-form-p
      special-variable-p streamp string-equal string-head-eq
      string-lessp string-looking-at string-match string< string=
      stringp subr-name subrp substring symbol-name symbol-plist
      symbol-value symbolp system-name throw time-later-p
      translate-string unless unwind-protect upper-case-p
      user-full-name user-login-name vector vectorp when while
      with-internal-definitions with-object write zerop remainder
      quotient modulo floor ceiling truncate round exp log sin cos tan
      asin acos atan sqrt expt gcd fixnump rationalp realp exactp
      inexactp exact->inexact inexact->exact numerator denominator
      positivep negativep oddp evenp abs lcm make-table make-weak-table
      string-hash symbol-hash eq-hash equal-hash tablep table-ref
      table-set table-unset table-walk))

  ;; alist mapping functions to their safe versions
  (define gaol-redefined-functions)

  ;; list of accessible special variables
  (define gaol-safe-specials
    '(batch-mode downcase-table file-handler-alist
      flatten-table operating-system rep-version upcase-table
      load-filename macro-environment))

  ;; list of file handlers that may be called. These functions shouldn't
  ;; be added to the function environment, since that would allow _any_
  ;; file operation to be performed
  (define gaol-safe-file-handlers '(tilde-file-handler tar-file-handler))

  ;; alist of (HANDLER . SYMBOL)
  (define gaol-redefined-file-handlers nil)


;;; building the actual environments

  (define gaol-structure nil)
  (define gaol-needs-rebuilding t)

  ;; XXX should create a named gaol environment, then let all
  ;; gaolled code open it in their own structure..
  (defun gaol-rebuild-environment ()
    (when gaol-needs-rebuilding
      (let
	  ((fh-env (nconc (mapcar (lambda (sym)
				    (cons sym t)) gaol-safe-file-handlers)
			  (mapcar (lambda (cell)
				    (cons (car cell)
					  (symbol-value (cdr cell))))
				  gaol-redefined-file-handlers))))
	(unless gaol-structure
	  (setq gaol-structure (%make-structure))
	  (set-special-environment gaol-safe-specials gaol-structure))
	(mapc (lambda (sym)
		(%structure-set
		 gaol-structure sym (%structure-ref (%current-structure) sym)))
	      gaol-safe-functions)
	(mapc (lambda (cell)
		(%structure-set gaol-structure (car cell) (cdr cell)))
	      gaol-redefined-functions)
	(set-file-handler-environment fh-env gaol-structure)
;	(%set-interface gaol-structure
;			(nconc (mapcar car gaol-redefined-functions)
;			       gaol-safe-functions))
	(setq gaol-needs-rebuilding nil))))


;;; public environment mutators

  (defun gaol-replace-function (fun def)
    (setq gaol-needs-rebuilding t)
    (setq gaol-redefined-functions (nconc gaol-redefined-functions
					  (list (cons fun def)))))

  (defun gaol-add-special (var)
    ;; use nconc to affect existing environments
    (setq gaol-safe-specials (nconc gaol-safe-specials (list var))))

  (defun gaol-add-file-handler (fun)
    (setq gaol-needs-rebuilding t)
    (setq gaol-safe-file-handlers (nconc gaol-safe-file-handlers (list fun))))

  (defun gaol-replace-file-handler (fun def)
    (setq gaol-needs-rebuilding t)
    (setq gaol-redefined-file-handlers (nconc gaol-redefined-file-handlers
					      (list (cons fun def)))))


;;; evaluating in the restricted environment

  ;; create a piece of code that when evaluate will evaluate FORM in
  ;; a secure environment
  (defun gaol-trampoline (form)
    (gaol-rebuild-environment)
    `(save-environment
      (set-environment t)
      (%eval-in-structure ',form ',gaol-structure)))

  (defun gaol-eval (form)
    (eval (gaol-trampoline form)))

  (defun gaol-load (file &optional no-error no-path-is-ignored no-suffix)
    (gaol-eval `(,(symbol-value 'load) ',file ',no-error t ',no-suffix t))))
