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

(provide 'gaol)


;; configuration/variables

;; list of all safe functions
(defvar gaol-safe-functions
  '(% * + - / /= 1+ 1- < <= = > >= add-hook alpha-char-p alphanumericp
    and append apply aref arrayp aset ash assoc assoc-regexp assq atom
    backquote beep boundp bytecodep call-hook car catch cdr
    char-downcase char-upcase closurep complete-string concat cond
    condition-case cons consp const-variable-p copy-sequence
    copy-stream current-time current-time-string default-boundp
    default-value defconst defmacro defsubst defun defvar delete
    delete-if delete-if-not delete-timer delq digit-char-p
    documentation elt eq eql equal error eval eval-when-compile
    expand-last-match featurep filter fix-time flet fmakunbound
    format funcall function functionp garbage-collect gensym get
    get-output-stream-string getenv identity if integerp interactive
    lambda last length let let* list list* logand logior lognot logxor
    lower-case-p lsh macroexpand macrolet macrop make-closure make-list
    make-string make-string-input-stream make-string-output-stream
    make-symbol make-vector makunbound mapc mapcar match-end
    match-start max member memq message min mod nconc nop not nreverse
    nth nthcdr null numberp or prin1 prin1-to-string princ print prog1
    prog2 progn put quote quote-regexp random rassoc rassq read
    read-char read-chars read-from-string read-line reverse rplaca
    rplacd sequencep set set-default set-timer setcar setcdr setplist
    setq setq-default signal sit-for sleep-for sort space-char-p
    special-form-p special-variable-p streamp string-equal
    string-head-eq string-lessp string-looking-at string-match string<
    string= stringp subr-name subrp substring
    symbol-name symbol-plist symbol-value symbolp system-name throw
    time-later-p translate-string unless unwind-protect upper-case-p
    user-full-name user-login-name vector vectorp when while
    with-object write zerop))

;; alist mapping functions to their safe versions
(defvar gaol-redefined-functions
  '((require . gaol:require)
    (make-timer . gaol:make-timer)))

;; list of accessible special variables
(defvar gaol-safe-specials
  '(nil t batch-mode downcase-table features file-handler-alist flatten-table
    operating-system rep-version upcase-table load-filename))

;; features that the restricted code may ask for
(defvar gaol-safe-features '(timers))

;; list of file handlers that may be called. These functions shouldn't
;; be added to the function environment, since that would allow _any_
;; file operation to be performed
(defvar gaol-safe-file-handlers '(tilde-file-handler tar-file-handler))

;; alist of (HANDLER . SYMBOL)
(defvar gaol-redefined-file-handlers nil)


;; building the actual environments

(defvar gaol-env nil)
(defvar gaol-fh-env nil)
(defvar gaol-env-built nil)

(defun gaol-rebuild-environment ()
  (let
      ((env (nconc (mapcar (lambda (sym)
			     (cons sym (and (boundp sym)
					    (symbol-value sym))))
			   gaol-safe-functions)
		   (mapcar (lambda (cell)
			     (cons (car cell) (symbol-value (cdr cell))))
			   gaol-redefined-functions)))
       (fh-env (nconc (mapcar (lambda (sym)
				(cons sym t)) gaol-safe-file-handlers)
		      (mapcar (lambda (cell)
				(cons (car cell) (symbol-value (cdr cell))))
			      gaol-redefined-file-handlers))))
    (if gaol-env
	(progn
	  ;; affect existing environments
	  (rplaca gaol-env (car env))
	  (rplacd gaol-env (cdr env))
	  (rplaca gaol-fh-env (car fh-env))
	  (rplacd gaol-fh-env (cdr fh-env)))
      (setq gaol-env env)
      (setq gaol-fh-env fh-env))
    (setq gaol-env-built t)))


;; public environment mutators

;;;###autoload
(defun gaol-add-function (fun)
  (setq gaol-env-built nil)
  (setq gaol-safe-functions (nconc gaol-safe-functions (list fun))))

;;;###autoload
(defun gaol-replace-function (fun def)
  (setq gaol-env-built nil)
  (setq gaol-redefined-functions (nconc gaol-redefined-functions
					(list (cons fun def)))))

;;;###autoload
(defun gaol-add-special (var)
  ;; use nconc to affect existing environments
  (setq gaol-safe-specials (nconc gaol-safe-specials (list var))))

;;;###autoload
(defun gaol-add-feature (feature)
  (setq gaol-safe-features (cons feature (delq feature gaol-safe-features))))

;;;###autoload
(defun gaol-add-file-handler (fun)
  (setq gaol-env-built nil)
  (setq gaol-safe-file-handlers (nconc gaol-safe-file-handlers (list fun))))

;;;###autoload
(defun gaol-replace-file-handler (fun def)
  (setq gaol-env-built nil)
  (setq gaol-redefined-file-handlers (nconc gaol-redefined-file-handlers
					    (list (cons fun def)))))


;; evaluating in the restricted environment

;; create a piece of code that when evaluate will evaluate FORM in
;; a secure environment
(defun gaol-trampoline (form)
  (unless gaol-env-built
    (gaol-rebuild-environment))
  `(save-environment
    (set-special-environment ',gaol-safe-specials)
    (set-file-handler-environment ',gaol-fh-env)
    (set-environment ',gaol-env)
    ,form))

(defun gaol-eval (form)
  (eval (gaol-trampoline form)))

(defun gaol-load (file &optional no-error no-path-is-ignored no-suffix)
  (gaol-eval `(,(symbol-value 'load) ',file ',no-error t ',no-suffix t)))


;; safe replacement functions

(defun gaol:require (feature)
  (unless (memq feature gaol-safe-features)
    (error "Gaolled code trying to require %s" feature))
  (require feature)
  (gaol-rebuild-environment))

(defun gaol:make-timer (fun &optional secs msecs)
  (unless (closurep fun)
    (error "Restricted code can only pass closures to make-timer"))
  (require 'timers)
  (make-timer fun secs msecs))
