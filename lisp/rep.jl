#| rep.jl -- read-eval-print loop

   $Id$

   Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure user (export rep)
  (open rep)

  (setq *user-structure* 'user)
  (setq *root-structure* 'user)

  (defun rep ()
    (repl 'user))

  ;; Install all autoload hooks.
  (load-all "autoload" (lambda (f) (load f nil t)))

  ;; Do operating-system initialisation
  (load-all (concat "os-" (symbol-name operating-system)) t)
  
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
       (default-error-handler (car error-data) (cdr error-data)))))
  
  ;; Use all arguments which are left.
  (let
      ((do-load (lambda (name)
		  (cond ((file-exists-p name)
			 (load name nil t t))
			((string-match "\\.jlc?$" name)
			 (load name))
			(t (require (intern name))))))
       arg)
    (while (setq arg (car command-line-args))
      (setq command-line-args (cdr command-line-args))
      (cond
       ((member arg '("--call" "-f"))
	(setq arg (car command-line-args))
	(setq command-line-args (cdr command-line-args))
	((symbol-value (read-from-string arg))))
       ((member arg '("--load" "-l"))
	(setq arg (car command-line-args))
	(setq command-line-args (cdr command-line-args))
	(do-load arg))
       ((member arg '("-s" "--scheme"))
	(setq arg (car command-line-args))
	(setq command-line-args (cdr command-line-args))
	(setq batch-mode t)
	(eval `(structure () (open scheme) (load ,arg))))
       ((member arg '("--quit" "-q"))
	(throw 'quit 0))
       (t
	(setq batch-mode t)
	(do-load arg)))))

  (unless batch-mode
    (format standard-output "rep %s, Copyright (C) 1999-2000 John Harper
rep comes with ABSOLUTELY NO WARRANTY; for details see the file COPYING
Built %s\n" rep-version rep-build-id)
    (rep)))
