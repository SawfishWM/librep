#| rep-repl.jl -- rep input loop

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

(require 'readline)

(define repl-in-struct (make-fluid))

(defun repl-eval (form)
  (eval form (%intern-structure (fluid repl-in-struct))))

;;;###autoload
(defun repl (&optional initial-structure)
  (let ((print-escape t)
	input)
    (let-fluids ((repl-in-struct (or initial-structure *user-structure*)))
      (write standard-output "\nEnter `,help' to list commands.\n")
      (catch 'out
	(while t
	  (setq input (concat
		       input (readline
			      (format nil (if input "" "%s> ")
				      (fluid repl-in-struct)))))
	  (catch 'next
	    (condition-case data
		(progn
		  (if (string-looking-at "\\s*,\\s*" input)
		      ;; a `,' introduces a meta command
		      (let ((stream (make-string-input-stream
				     input (match-end)))
			    sexps)
			(condition-case nil
			    (while t
			      (setq sexps (cons (read stream) sexps)))
			  (end-of-stream
			   (setq sexps (nreverse sexps))))
			(if (get (car sexps) 'repl-command)
			    (apply (get (car sexps) 'repl-command) (cdr sexps))
			  (format standard-output
				  "unrecognized command name: %s\n"
				  (car sexps))))
		    (let ((form (condition-case nil
				    (read-from-string input)
				  (end-of-stream
				   (if (and input (not (string= "" input)))
				       (throw 'next)
				     (throw 'out))))))
		      (format standard-output "%S\n" (repl-eval form))))
		  (setq input nil))
	      (error
	       (error-handler-function (car data) (cdr data))
	       (setq input nil)))))))))

(defun rl-completion-generator (w)
  (apropos (concat #\^ (quote-regexp w))
	   (lambda (x)
	     (condition-case nil
		 (progn
		   (repl-eval x)
		   t)
	       (void-value nil)))))

(put 'in 'repl-command
     (lambda (struct &optional form)
       (if form
	   (format standard-output "%S\n"
		   (eval form (%get-structure struct)))
	 (fluid-set repl-in-struct struct))))
(put 'in 'repl-help "STRUCT [FORM ...]")

(put 'load 'repl-command
     (lambda structs
       (mapc (lambda (struct)
	       (%intern-structure struct)) structs)))
(put 'load 'repl-help "STRUCT ...")

(put 'reload 'repl-command
     (lambda structs
       (mapc (lambda (struct)
	       (%name-structure (%get-structure struct) nil)
	       (%intern-structure struct)) structs)))
(put 'reload 'repl-help "STRUCT ...")

(put 'unload 'repl-command
     (lambda structs
       (mapc (lambda (struct)
	       (%name-structure (%get-structure struct) nil)) structs)))
(put 'unload 'repl-help "STRUCT ...")

(put 'load-file 'repl-command
     (lambda files
       (mapc (lambda (f)
	       (repl-eval `(,load ,f))) files)))
(put 'load-file 'repl-help "\"FILENAME\" ...")

(put 'open 'repl-command
     (lambda structs
       (repl-eval `(,%open-structures (,quote ,structs)))))
(put 'open 'repl-help "STRUCT ...")

(put 'access 'repl-command
     (lambda structs
       (repl-eval `(,%access-structures (,quote ,structs)))))
(put 'access 'repl-help "STRUCT ...")

(put 'structures 'repl-command
     (lambda ()
       (let (structures)
	 (%structure-walk (lambda (var value)
			    (setq structures (cons var structures)))
			  (%get-structure '%structures))
	 (format standard-output "%s\n" (sort structures)))))

(put 'interfaces 'repl-command
     (lambda ()
       (let (interfaces)
	 (%structure-walk (lambda (var value)
			    (setq interfaces (cons var interfaces)))
			  (%get-structure '%interfaces))
	 (format standard-output "%s\n" (sort interfaces)))))

(put 'bindings 'repl-command
     (lambda ()
       (%structure-walk (lambda (var value)
			  (format standard-output "  (%s %S)\n" var value))
			(%intern-structure (fluid repl-in-struct)))))

(put 'exports 'repl-command
     (lambda ()
       (format standard-output "%s\n"
	       (%structure-interface
		(%intern-structure (fluid repl-in-struct))))))

(put 'imports 'repl-command
     (lambda ()
       (format standard-output "%s\n"
	       (%structure-imports
		(%intern-structure (fluid repl-in-struct))))))

(put 'accessible 'repl-command
     (lambda ()
       (format standard-output "%s\n"
	       (%structure-accessible
		(%intern-structure (fluid repl-in-struct))))))

(put 'collect 'repl-command garbage-collect)

(put 'dis 'repl-command
     (lambda (arg)
       (require 'disassembler)
       (disassemble (repl-eval arg))))
(put 'dis 'repl-help "FORM")

(put 'compile-proc 'repl-command
     (lambda args
       (require 'compiler)
       (mapc (lambda (arg)
	       (compile-function (repl-eval arg))) args)))
(put 'compile-proc 'repl-help "PROCEDURE ...")

(put 'compile 'repl-command
     (lambda args
       (require 'compiler)
       (if (null args)
	   (compile-module (fluid repl-in-struct))
	 (mapc compile-module args))))
(put 'compile 'repl-help "[STRUCT ...]")

(put 'new 'repl-command
     (lambda (name)
       (%make-structure nil (lambda ()
			      (%open-structures '(module-system)))
			nil name)
       (fluid-set repl-in-struct name)))
(put 'new 'repl-help "STRUCT")

(put 'expand 'repl-command
     (lambda (form)
       (format standard-output "%s\n" (repl-eval `(,macroexpand ',form)))))
(put 'expand 'repl-help "FORM")

(put 'step 'repl-command
     (lambda (form)
       (format standard-output "%s\n" (repl-eval `(,step ',form)))))
(put 'step 'repl-help "FORM")

(put 'help 'repl-command
     (lambda ()
       (let* ((commands (sort (apropos "" (lambda (x)
					    (get x 'repl-command)))))
	      (count (length commands))
	      (mid (ceiling (/ count 2))))
	 (write standard-output "
Either enter lisp forms to be evaluated, and their result printed, or
enter a meta-command prefixed by a `,' character.\n")
	 (let loop ((i 0)
		    (left commands)
		    (right (nthcdr mid commands)))
	   (when (< i mid)
	     (format standard-output "\n  ,%-30s"
		     (format nil "%s %s" (car left)
			     (or (get (car left) 'repl-help) "")))
	     (when right
	       (format standard-output " ,%s %s"
		       (car right) (or (get (car right) 'repl-help) ""))
	       (loop (1+ i) (cdr left) (cdr right)))))
	 (write standard-output #\newline))))

(put 'quit 'repl-command (lambda () (throw 'quit 0)))

(put 'describe 'repl-command
     (lambda (name)
       (require 'lisp-doc)
       (let* ((value (repl-eval name))
	      (doc (documentation name value)))
	 (write standard-output #\newline)
	 (describe-value value name)
	 (write standard-output #\newline)
	 (when doc
	   (format standard-output "%s\n\n" doc)))))
(put 'describe 'repl-help "SYMBOL")

(put 'apropos 'repl-command
     (lambda (re)
       (require 'lisp-doc)
       (let ((funs (apropos re (lambda (x)
				 (condition-case nil
				     (progn
				       (repl-eval x)
				       t)
				   (void-value nil))))))
	 (mapc (lambda (x)
		 (describe-value (repl-eval x) x)) funs))))
(put 'apropos 'repl-help "\"REGEXP\"")

(put 'time 'repl-command
     (lambda (form)
       (let (t1 t2 ret)
	 (setq t1 (current-utime))
	 (setq ret (repl-eval form))
	 (setq t2 (current-utime))
	 (format standard-output
		 "%S\nElapsed: %d seconds\n" ret (/ (- t2 t1) 1e6)))))
(put 'time 'repl-help "FORM")

(put 'profile 'repl-command
     (lambda (form)
       (require 'profiler)
       (format standard-output "%S\n\n" (call-in-profiler
					 (lambda () (repl-eval form))))
       (print-profile)))

(put 'profile 'repl-help "FORM")
