#| repl.jl -- rep input loop

   $Id: repl.jl,v 1.50 2004/10/07 05:03:54 jsh Exp $

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
	    repl-eval
	    repl-iterate
	    repl-completions
	    define-repl-command)

    (open rep
	  rep.structures
	  rep.system
	  rep.regexp
	  rep.io.files)

  (define current-repl (make-fluid))

  (define (make-repl #!optional initial-struct)
    (cons (or initial-struct *user-structure*) nil))

  (define repl-struct car)
  (define repl-pending cdr)
  (define repl-set-struct rplaca)
  (define repl-set-pending rplacd)

  (define (repl-eval form)
    (eval form (intern-structure (repl-struct (fluid current-repl)))))

  (define (repl-boundp x)
    (condition-case nil
	(progn
	  (repl-eval x)
	  t)
      (void-value nil)))

  ;; returns t if repl should run again
  (define (repl-iterate repl input)
    (setq input (concat (repl-pending repl) input))
    (repl-set-pending repl nil)
    (let-fluids ((current-repl repl))
      (let ((print-escape t))
	(catch 'return
	  (condition-case data
	      (progn
		(cond
		 ((string-looking-at "\\s*,\\s*" input)
		  ;; a `,' introduces a meta command
		  (let ((stream (make-string-input-stream input (match-end)))
			(sexps '()))
		    (condition-case nil
			(while t
			  (setq sexps (cons (read stream) sexps)))
		      (end-of-stream (setq sexps (nreverse sexps))))
		    (let ((command (repl-command (car sexps))))
		      (and command (apply command (cdr sexps))))))

		 ;; ignore empty input lines, or lines with comments only
		 ((string-looking-at "\\s*(;.*)?$" input))

		 (t (let ((form (condition-case nil
				    (read-from-string input)
				  (premature-end-of-stream
				   (repl-set-pending repl input)
				   (throw 'return
					  (and input
					       (not (string= "" input))))))))
		      (let ((result (repl-eval form)))
			(unless (eq result #undefined)
			  (format standard-output "%S\n" result))))))
		t)
	    (error
	     (default-error-handler (car data) (cdr data))
	     t))))))

  (define (do-readline prompt completer)
    (if (file-ttyp standard-input)
	(progn
	  (require 'rep.io.readline)
	  (readline prompt completer))
      (write standard-output prompt)
      (read-line standard-input)))

  (define (repl #!optional initial-structure)
    ;; returns t if repl should run again
    (define (run-repl)
      (let ((input (do-readline
		    (format nil (if (repl-pending (fluid current-repl))
				    "" "%s> ")
			    (repl-struct (fluid current-repl)))
		    completion-generator)))
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

  (define (completion-generator w)
    ;; Either a special command or unquote.
    (if (string-head-eq w ",")
	(mapcar (lambda (x) (concat "," (symbol-name x)))
		(apropos (concat #\^ (quote-regexp (substring w 1)))
			 (lambda (x) (assq x repl-commands))))
      (apropos (concat #\^ (quote-regexp w)) repl-boundp)))

  (define (repl-completions repl word)
    (let-fluids ((current-repl repl))
      (completion-generator word)))


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

  (define repl-commands '())

  (define (define-repl-command name function #!optional doc)
    (let ((cell (assq name repl-commands)))
      (if cell
	  (rplacd cell (list function doc))
	(setq repl-commands (cons (list name function doc) repl-commands)))))

  (define (find-command name)
    (let ((cell (assq name repl-commands)))
      (if cell
	  cell
	;; look for an unambiguous match
	(let ((re (concat "^" (quote-regexp (symbol-name name)))))
	  (let loop ((rest repl-commands)
		     (matched nil))
	    (cond ((null rest)
		   (if matched
		       matched
		     (format standard-output "unknown command: ,%s\n" name)
		     nil))
		  ((string-match re (symbol-name (caar rest)))
		   (if matched
		       ;; already saw something, exit
		       (progn
			 (format standard-output
				 "non-unique abbreviation: ,%s\n" name)
			 nil)
		     (loop (cdr rest) (car rest))))
		  (t (loop (cdr rest) matched))))))))

  (define (repl-command name)
    (let ((cell (find-command name)))
      (and cell (cadr cell))))

  (define (repl-documentation name)
    (let ((cell (find-command name)))
      (and cell (caddr cell))))

  (define-repl-command
   'in
   (lambda (struct #!optional form)
     (if form
	 (format standard-output "%S\n"
		 (eval form (get-structure struct)))
       (repl-set-struct (fluid current-repl) struct)))
   "STRUCT [FORM ...]")

  (define-repl-command
   'load
   (lambda structs
     (mapc (lambda (struct)
	     (intern-structure struct)) structs))
   "STRUCT ...")

  (define-repl-command
   'reload
   (lambda structs
     (mapc (lambda (x)
	     (let ((struct (get-structure x)))
	       (when struct
		 (name-structure struct nil))
	       (intern-structure x))) structs))
   "STRUCT ...")

  (define-repl-command
   'unload
   (lambda structs
     (mapc (lambda (x)
	     (let ((struct (get-structure x)))
	       (when struct
		 (name-structure struct nil)))) structs))
   "STRUCT ...")

  (define-repl-command
   'load-file
   (lambda files
     (mapc (lambda (f)
	     (repl-eval `(,load ,f))) files))
   "\"FILENAME\" ...")

  (define-repl-command
   'open
   (lambda structs
     (repl-eval `(,open-structures (,quote ,structs))))
   "STRUCT ...")

  (define-repl-command
   'access
   (lambda structs
     (repl-eval `(,access-structures (,quote ,structs))))
   "STRUCT ...")

  (define-repl-command
   'structures
   (lambda ()
     (let (structures)
       (structure-walk (lambda (var value)
			 (declare (unused value))
			 (when value
			   (setq structures (cons var structures))))
		       (get-structure '%structures))
       (print-list (sort structures)))))

  (define-repl-command
   'interfaces
   (lambda ()
     (let (interfaces)
       (structure-walk (lambda (var value)
			 (declare (unused value))
			 (setq interfaces (cons var interfaces)))
		       (get-structure '%interfaces))
       (print-list (sort interfaces)))))

  (define-repl-command
   'bindings
   (lambda ()
     (structure-walk (lambda (var value)
		       (format standard-output "  (%s %S)\n" var value))
		     (intern-structure
		      (repl-struct (fluid current-repl))))))

  (define-repl-command
   'exports
   (lambda ()
     (print-list (structure-interface
		  (intern-structure
		   (repl-struct (fluid current-repl)))))))

  (define-repl-command
   'imports
   (lambda ()
     (print-list (module-imports (repl-struct (fluid current-repl))))))

  (define-repl-command
   'accessible
   (lambda ()
     (print-list (structure-accessible
		  (intern-structure
		   (repl-struct (fluid current-repl)))))))

  (define-repl-command
   'collect
   (lambda ()
     (let ((stats (garbage-collect t)))
       (format standard-output "Used %d/%d cons, %d/%d tuples, %d strings, %d vector slots, %d/%d closures\n"
	       (car (nth 0 stats)) (+ (car (nth 0 stats)) (cdr (nth 0 stats)))
	       (car (nth 1 stats)) (+ (car (nth 1 stats)) (cdr (nth 1 stats)))
	       (car (nth 2 stats))
	       (nth 3 stats)
	       (car (nth 4 stats)) (+ (car (nth 4 stats))
				      (cdr (nth 4 stats)))))))

  (define-repl-command
   'disassemble
   (lambda (arg)
     (require 'rep.vm.disassembler)
     (disassemble (repl-eval arg)))
   "FORM")

  (define-repl-command
   'compile-proc
   (lambda args
     (require 'rep.vm.compiler)
     (mapc (lambda (arg)
	     (compile-function (repl-eval arg) arg)) args))
   "PROCEDURE ...")

  (define-repl-command
   'compile
   (lambda args
     (require 'rep.vm.compiler)
     (if (null args)
	 (compile-module (repl-struct (fluid current-repl)))
       (mapc compile-module args)))
   "[STRUCT ...]")

  (define-repl-command
   'compile-file
   (lambda args
     (require 'rep.vm.compiler)
     (mapc compile-file args))
   "\"FILENAME\" ...")

  (define-repl-command
   'new
   (lambda (name)
     (declare (bound %open-structures))
     (make-structure nil (lambda ()
			   (%open-structures '(rep.module-system)))
		     nil name)
     (repl-set-struct (fluid current-repl) name))
   "STRUCT")

  (define-repl-command
   'expand
   (lambda (form)
     (format standard-output "%s\n" (repl-eval `(,macroexpand ',form))))
   "FORM")

  (define-repl-command
   'step
   (lambda (form)
     (format standard-output "%s\n" (repl-eval `(,step ',form))))
   "FORM")

  (define-repl-command
   'help
   (lambda ()
     (write standard-output "
Either enter lisp forms to be evaluated, and their result printed, or
enter a meta-command prefixed by a `,' character. Names of meta-
commands may be abbreviated to their unique leading characters.\n\n")
     (print-list (sort (mapcar car repl-commands))
		 (lambda (x)
		   (format nil ",%s %s" x (or (repl-documentation x) ""))))))

  (define-repl-command 'quit (lambda () (throw 'quit 0)))

  (define-repl-command
   'describe
   (lambda (name)
     (require 'rep.lang.doc)
     (let* ((value (repl-eval name))
	    (struct (locate-binding* name))
	    (doc (documentation name struct value)))
       (write standard-output #\newline)
       (describe-value value name struct)
       (write standard-output #\newline)
       (when doc
	 (format standard-output "%s\n\n" doc))))
   "SYMBOL")

  (define-repl-command
   'apropos
   (lambda (re)
     (require 'rep.lang.doc)
     (let ((funs (apropos re repl-boundp)))
       (mapc (lambda (x)
	       (describe-value (repl-eval x) x)) funs)))
   "\"REGEXP\"")

  (define-repl-command
   'locate
   (lambda (var)
     (let ((struct (locate-binding* var)))
       (if struct
	   (format standard-output "%s is bound in: %s.\n" var struct)
	 (format standard-output "%s is unbound.\n" var))))
   "SYMBOL")

  (define-repl-command
   'whereis
   (lambda (var)
     (let ((out '()))
       (structure-walk (lambda (k v)
			 (declare (unused k))
			 (when (and v (structure-name v)
				    (structure-exports-p v var))
			   (setq out (cons (structure-name v) out))))
		       (get-structure '%structures))
       (if out
	   (format standard-output "%s is exported by: %s.\n"
		   var (mapconcat symbol-name (sort out) ", "))
	 (format standard-output "No module exports %s.\n" var))))
   "SYMBOL")

  (define-repl-command
   'time
   (lambda (form)
     (let (t1 t2 ret)
       (setq t1 (current-utime))
       (setq ret (repl-eval form))
       (setq t2 (current-utime))
       (format standard-output
	       "%S\nElapsed: %d seconds\n" ret (/ (- t2 t1) 1e6))))
   "FORM")

  (define-repl-command
   'profile
   (lambda (form)
     (require 'rep.lang.profiler)
     (format standard-output "%S\n\n" (call-in-profiler
				       (lambda () (repl-eval form))))
     (print-profile))
   "FORM")

  (define-repl-command
   'check
   (lambda (#!optional module)
     (require 'rep.test.framework)
     (if (null module)
	 (run-all-self-tests)
       (run-module-self-tests module)))
   "[STRUCT]"))
