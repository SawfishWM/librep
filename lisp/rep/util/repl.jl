#| repl.jl -- rep input loop

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

(define-structure rep.util.repl

    (export repl
	    make-repl
	    repl-struct
	    repl-pending
	    repl-iterate
	    repl-completions)

    (open rep
	  rep.structures
	  rep.system
	  rep.regexp
	  rep.io.readline)

  (define current-repl (make-fluid))

  (define (make-repl #!optional initial-struct)
    (cons (or initial-struct *user-structure*) nil))

  (define repl-struct car)
  (define repl-pending cdr)
  (define repl-set-struct rplaca)
  (define repl-set-pending rplacd)

  (define (repl-eval form)
    (eval form (intern-structure (repl-struct (fluid current-repl)))))

  (define (repl-iterate repl input)
    (setq input (concat (repl-pending repl) input))
    (repl-set-pending repl nil)
    (let-fluids ((current-repl repl))
      (let ((print-escape t))
	(catch 'return
	  (condition-case data
	      (progn
		(if (string-looking-at "\\s*,\\s*" input)
		    ;; a `,' introduces a meta command
		    (let ((stream (make-string-input-stream input (match-end)))
			  (sexps '()))
		      (condition-case nil
			  (while t
			    (setq sexps (cons (read stream) sexps)))
			(end-of-stream (setq sexps (nreverse sexps))))
		      (if (get (car sexps) 'repl-command)
			  (apply (get (car sexps) 'repl-command) (cdr sexps))
			(format standard-output
				"unrecognized command name: %s\n"
				(car sexps))))
		  (let ((form (condition-case nil
				  (read-from-string input)
				(end-of-stream
				 (repl-set-pending repl input)
				 (throw 'return
					(and input
					     (not (string= "" input))))))))
		    (format standard-output "%S\n" (repl-eval form))))
		t)
	    (error
	     (error-handler (car data) (cdr data))))))))

  (define (repl #!optional initial-structure)
    ;; returns t if repl should run again
    (define (run-repl)
      (let ((input (readline
		    (format nil (if (repl-pending (fluid current-repl))
				    "" "%s> ")
			    (repl-struct (fluid current-repl))))))
	(and input (repl-iterate (fluid current-repl) input))))
    (define (interrupt-handler data)
      (if (eq (car data) 'user-interrupt)
	  (progn
	    (format standard-output "User interrupt!\n")
	    t)
	(raise-exception data)))
    (let-fluids ((current-repl (make-repl initial-structure)))
      (write standard-output "\nEnter `,help' to list commands.\n")
      (let loop ()
	(when (call-with-exception-handler run-repl interrupt-handler)
	  (loop)))))

  (define (print-list data #!optional map)
    (unless map (setq map identity))
    (let* ((count (length data))
	   (mid (inexact->exact (ceiling (/ count 2)))))
      (do ((i 0 (1+ i))
	   (left data (cdr left))
	   (right (nthcdr mid data) (cdr right)))
	  ((null left))
	(when (< i mid)
	  (format standard-output "  %-30s"
		  (format nil "%s" (map (car left))))
	  (when right
	    (format standard-output " %s" (map (car right))))
	  (write standard-output #\newline)))))

  (define (rl-completion-generator w)
    (apropos (concat #\^ (quote-regexp w))
	     (lambda (x)
	       (condition-case nil
		   (progn
		     (repl-eval x)
		     t)
		 (void-value nil)))))

  (define (repl-completions repl word)
    (let-fluids ((current-repl repl))
      (rl-completion-generator word)))

  (define (error-handler err data)
    (write standard-error
	   (format nil "\^G*** %s: %s\n"
		   (or (get err 'error-message) err)
		   (mapconcat (lambda (x)
				(format nil "%s" x)) data ", "))))


;;; module utils

  (define (module-exports-p name var)
    (structure-exports-p (get-structure name) var))

  (define (module-imports name)
     (structure-imports (get-structure name)))

  (define (locate-binding* name)
    (or (locate-binding name (append (list (repl-struct (fluid current-repl)))
				     (module-imports
				      (repl-struct (fluid current-repl)))))
	(and (structure-bound-p
	      (get-structure (repl-struct (fluid current-repl))) name)
	     (repl-struct (fluid current-repl)))))


;;; commands

  (put 'in 'repl-command
       (lambda (struct #!optional form)
	 (if form
	     (format standard-output "%S\n"
		     (eval form (get-structure struct)))
	   (repl-set-struct (fluid current-repl) struct))))
  (put 'in 'repl-help "STRUCT [FORM ...]")

  (put 'load 'repl-command
       (lambda structs
	 (mapc (lambda (struct)
		 (intern-structure struct)) structs)))
  (put 'load 'repl-help "STRUCT ...")

  (put 'reload 'repl-command
       (lambda structs
	 (mapc (lambda (struct)
		 (name-structure (get-structure struct) nil)
		 (intern-structure struct)) structs)))
  (put 'reload 'repl-help "STRUCT ...")

  (put 'unload 'repl-command
       (lambda structs
	 (mapc (lambda (struct)
		 (name-structure (get-structure struct) nil)) structs)))
  (put 'unload 'repl-help "STRUCT ...")

  (put 'load-file 'repl-command
       (lambda files
	 (mapc (lambda (f)
		 (repl-eval `(,load ,f))) files)))
  (put 'load-file 'repl-help "\"FILENAME\" ...")

  (put 'open 'repl-command
       (lambda structs
	 (repl-eval `(,open-structures (,quote ,structs)))))
  (put 'open 'repl-help "STRUCT ...")

  (put 'access 'repl-command
       (lambda structs
	 (repl-eval `(,access-structures (,quote ,structs)))))
  (put 'access 'repl-help "STRUCT ...")

  (put 'structures 'repl-command
       (lambda ()
	 (let (structures)
	   (structure-walk (lambda (var value)
			     (setq structures (cons var structures)))
			   (get-structure '%structures))
	   (print-list (sort structures)))))

  (put 'interfaces 'repl-command
       (lambda ()
	 (let (interfaces)
	   (structure-walk (lambda (var value)
			     (setq interfaces (cons var interfaces)))
			   (get-structure '%interfaces))
	   (print-list (sort interfaces)))))

  (put 'bindings 'repl-command
       (lambda ()
	 (structure-walk (lambda (var value)
			   (format standard-output "  (%s %S)\n" var value))
			 (intern-structure
			  (repl-struct (fluid current-repl))))))

  (put 'exports 'repl-command
       (lambda ()
	 (print-list (structure-interface
		      (intern-structure
		       (repl-struct (fluid current-repl)))))))

  (put 'imports 'repl-command
       (lambda ()
	 (print-list (module-imports (repl-struct (fluid current-repl))))))

  (put 'accessible 'repl-command
       (lambda ()
	 (print-list (structure-accessible
		      (intern-structure
		       (repl-struct (fluid current-repl)))))))

  (put 'collect 'repl-command garbage-collect)

  (put 'dis 'repl-command
       (lambda (arg)
	 (require 'rep.vm.disassembler)
	 (disassemble (repl-eval arg))))
  (put 'dis 'repl-help "FORM")

  (put 'compile-proc 'repl-command
       (lambda args
	 (require 'rep.vm.compiler)
	 (mapc (lambda (arg)
		 (compile-function (repl-eval arg) arg)) args)))
  (put 'compile-proc 'repl-help "PROCEDURE ...")

  (put 'compile 'repl-command
       (lambda args
	 (require 'rep.vm.compiler)
	 (if (null args)
	     (compile-module (repl-struct (fluid current-repl)))
	   (mapc compile-module args))))
  (put 'compile 'repl-help "[STRUCT ...]")

  (put 'new 'repl-command
       (lambda (name)
	 (declare (bound %open-structures))
	 (make-structure nil (lambda ()
			       (%open-structures '(rep.module-system)))
			 nil name)
	 (repl-set-struct (fluid current-repl) name)))
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
	 (write standard-output "
Either enter lisp forms to be evaluated, and their result printed, or
enter a meta-command prefixed by a `,' character.\n\n")
	 (print-list (sort (apropos "" (lambda (x)
					 (get x 'repl-command))))
		     (lambda (x)
		       (format nil ",%s %s" x (or (get x 'repl-help) ""))))))

  (put 'quit 'repl-command (lambda () (throw 'quit 0)))

  (put 'describe 'repl-command
       (lambda (name)
	 (require 'rep.lang.doc)
	 (let* ((value (repl-eval name))
		(struct (locate-binding* name))
		(doc (documentation name struct value)))
	   (write standard-output #\newline)
	   (describe-value value name struct)
	   (write standard-output #\newline)
	   (when doc
	     (format standard-output "%s\n\n" doc)))))
  (put 'describe 'repl-help "SYMBOL")

  (put 'apropos 'repl-command
       (lambda (re)
	 (require 'rep.lang.doc)
	 (let ((funs (apropos re (lambda (x)
				   (condition-case nil
				       (progn
					 (repl-eval x)
					 t)
				     (void-value nil))))))
	   (mapc (lambda (x)
		   (describe-value (repl-eval x) x)) funs))))
  (put 'apropos 'repl-help "\"REGEXP\"")

  (put 'locate 'repl-command
       (lambda (var)
	 (let ((struct (locate-binding* var)))
	   (if struct
	       (format standard-output "%s is bound in: %s.\n" var struct)
	     (format standard-output "%s is unbound.\n" var)))))
  (put 'locate 'repl-help "SYMBOL")

  (put 'whereis 'repl-command
       (lambda (var)
	 (let ((out '()))
	   (structure-walk (lambda (k v)
			     (when (and (structure-name v)
					(structure-exports-p v var))
			       (setq out (cons (structure-name v) out))))
			   (get-structure '%structures))
	   (if out
	       (format standard-output "%s is exported by: %s.\n"
		       var (mapconcat symbol-name (sort out) ", "))
	     (format standard-output "No module exports %s.\n" var)))))
  (put 'whereis 'repl-help "SYMBOL")

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
	 (require 'rep.lang.profiler)
	 (format standard-output "%S\n\n" (call-in-profiler
					   (lambda () (repl-eval form))))
	 (print-profile)))
  (put 'profile 'repl-help "FORM"))
