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

(define-structure user ()

    ((open rep
	   rep.regexp
	   rep.system
	   rep.io.files
	   rep.io.processes)
     (set-binds))

  (defun do-load (name)
    (cond ((file-exists-p name)
	   (load name nil t t))
	  ((string-match "\\.jlc?$" name)
	   (load name))
	  (t (require (intern name)))))

  (defun parse-options ()
    (let (arg)
      (condition-case error-data
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
	     ((string= arg "--check")
	      (require 'rep.test.framework)
	      (run-self-tests-and-exit))
	     ((string= arg "--help")
	      (format standard-error "\
usage: %s [OPTIONS...]

where OPTIONS are any of:

    FILE		load the Lisp file FILE (from the cwd if possible,
			 implies --batch mode)

    --batch		batch mode: process options and exit
    --interp		interpreted mode: don't load compiled Lisp files
    --debug		start in the debugger (implies --interp)

    --call FUNCTION	call the Lisp function FUNCTION
    --f FUNCTION

    --load FILE		load the file of Lisp forms called FILE
    -l FILE

    -s FILE		 (implies --batch mode)

    --check		run self tests and exit

    --version		print version details
    --no-rc		don't load rc or site-init files
    --quit, -q		terminate the interpreter process\n" program-name)
	      (throw 'quit 0))
	     ((string= arg "--version")
	      (format standard-output "rep version %s\n" rep-version)
	      (throw 'quit 0))
	     ((member arg '("--quit" "-q"))
	      (throw 'quit 0))
	     (t
	      (setq batch-mode t)
	      (do-load arg))))
	(error
	 (error-handler-function (car error-data) (cdr error-data))
	 (throw 'quit 1)))))

  (setq *user-structure* 'user)

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
	   (load (concat (user-home-directory) ".reprc") t t t)
	   (load "rep-default" t)))
      (error
       (default-error-handler (car error-data) (cdr error-data)))))
  
  ;; Use all arguments which are left.
  (if (get-command-line-option "--debug")
      (progn
	(require 'rep.lang.debugger)
	(call-with-lexical-origins
	 (lambda ()
	   (setq interpreted-mode t)
	   (setq debug-on-error '(bad-arg missing-arg invalid-function
				  void-value invalid-read-syntax
				  premature-end-of-stream invalid-lambda-list
				  invalid-macro invalid-autoload no-catcher
				  file-error invalid-stream setting-constant
				  process-error arith-error
				  assertion-failed check-failed test-failed))
	   (break)
	   (parse-options))))
    (parse-options))

  (unless batch-mode
    (format standard-output "rep %s, Copyright (C) 1999-2000 John Harper
rep comes with ABSOLUTELY NO WARRANTY; for details see the file COPYING
Built %s\n" rep-version rep-build-id)

    (require 'rep.util.repl)
    (repl)))

;; prevent this being opened as a module
nil
