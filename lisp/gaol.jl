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
    expand-last-match fboundp featurep filter fix-time flet fmakunbound
    format fset funcall function functionp garbage-collect gensym get
    get-output-stream-string getenv identity if integerp interactive
    last length let let* list list* logand logior lognot logxor
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
    string= stringp subr-name subrp substring symbol-function
    symbol-name symbol-plist symbol-value symbolp system-name throw
    time-later-p translate-string unless unwind-protect upper-case-p
    user-full-name user-login-name vector vectorp when while
    with-object write zerop))

(defvar gaol-redefined-functions
  '((require . gaol:require)
    (make-timer . gaol:make-timer)))

(defvar gaol-safe-specials
  '(nil t batch-mode downcase-table features file-handler-alist flatten-table
    operating-system rep-version upcase-table load-filename))

(defvar gaol-safe-features '(timers))


;; building the actual environments

(defvar gaol-function-env nil)
(defvar gaol-fenv-built nil)

(defun gaol-rebuild-environment ()
  (let
      ((env (nconc (mapcar #'(lambda (sym)
			       (cons sym (and (fboundp sym)
					      (symbol-function sym))))
			   gaol-safe-functions)
		   (mapcar #'(lambda (cell)
			       (cons (car cell)
				     (symbol-function
				      (cdr cell))))
			   gaol-redefined-functions))))
    (if gaol-function-env
	(progn
	  ;; affect existing environments
	  (rplaca gaol-function-env (car env))
	  (rplacd gaol-function-env (cdr env)))
      (setq gaol-function-env env))
    (setq gaol-fenv-built t)))


;; public environment mutators

;;;###autoload
(defun gaol-add-function (fun)
  (setq gaol-fenv-built nil)
  (setq gaol-safe-functions (nconc gaol-safe-functions (list fun))))

;;;###autoload
(defun gaol-replace-function (fun def)
  (setq gaol-fenv-built nil)
  (setq gaol-redefined-functions (nconc gaol-redefined-functions
					(list (cons fun def)))))

;;;###autoload
(defun gaol-add-special (var)
  ;; use nconc to affect existing environments
  (setq gaol-safe-specials (nconc gaol-safe-specials (list var))))

;;;###autoload
(defun gaol-add-feature (feature)
  (setq gaol-safe-features (cons feature (delq feature gaol-safe-features))))


;; evaluating in the restricted environment

;; create a piece of code that when evaluate will evaluate FORM in
;; a secure environment
(defun gaol-trampoline (form)
  (unless gaol-fenv-built
    (gaol-rebuild-environment))
  `(save-environment
    (set-variable-environment nil)
    (set-special-environment ',gaol-safe-specials)
    (set-function-environment ',gaol-function-env)
    ,form))

(defun gaol-eval (form)
  (eval (gaol-trampoline form)))

(defun gaol-load (file &optional no-error no-path-is-ignored no-suffix)
  (gaol-eval `(,(symbol-function 'load) ',file ',no-error t ',no-suffix t)))


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
