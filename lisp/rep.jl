;;;; rep.jl -- read-eval-print loop
;;;  Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
;;;  $Id$

;;; This file is part of Jade.

;;; Jade is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; Jade is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with Jade; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(defun rep ()
  (catch 'quit
    (while t
      (condition-case error-data
	  (progn
	    (write standard-output "? ")
	    (flush-file standard-output)
	    (format standard-output " => %S\n" (eval (read standard-input))))
	(end-of-stream
	 (throw 'quit 0))
	(error
	 (format standard-output "error--> %S\n" error-data))))))

(fset 'recursive-edit (symbol-function 'rep))

;; Install all autoload hooks. This is done last so that it works
;; when dumped. We load autoload.jl to ensure that we don't get a
;; compiled (and possibly out of date) version
(load-all "autoload.jl" t)

;; Do operating-system initialisation
(load-all (concat "os-" (symbol-name operating-system)) t)

(unless batch-mode
  (format standard-output ";; rep %s, Copyright (C) 1999 John Harper\n;; rep comes with ABSOLUTELY NO WARRANTY; for details see the file COPYING\n;; built %s\n" rep-version rep-build-id))

;; Load site specific initialisation. Errors here are trapped since
;; they're probably not going to result in an unusable state
(unless (get-command-line-option "--no-rc")
  (condition-case error-data
      (progn
	;; First the site-wide stuff
	(load-all "site-init")
	;; Now try to interpret the user's startup file, or failing that
	;; the default.jl file providing site-wide user options
	(or
	 (load (concat (user-home-directory) ".reprc") t t)
	 (load "rep-default" t)))
    (error
     (format (stderr-file) "error in local config--> %S\n" error-data))))

;; Use all arguments which are left.
(let
    (arg)
  (while (setq arg (car command-line-args))
    (setq command-line-args (cdr command-line-args))
    (cond
      ((equal "-f" arg)
       (setq arg (car command-line-args)
	     command-line-args (cdr command-line-args))
       (funcall (read-from-string arg)))
      ((equal "-l" arg)
       (setq arg (car command-line-args)
	     command-line-args (cdr command-line-args))
       (load arg))
      ((equal "-q" arg)
       (throw 'quit 0))
      (t
       (load arg)))))

(unless batch-mode
  (rep))
