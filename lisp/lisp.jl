;;;; lisp.jl -- Some fundamental Lisp functions
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

;; This file is loaded right at the beginning of the initialisation
;; procedure.

(provide 'lisp)

(defvar standard-output (stdout-file)
  "Stream that `prin?' writes its output to by default.")

(defvar standard-input (stdin-file)
  "Stream that `read' takes its input from by default.")

(defvar standard-error (stderr-file)
  "Standard stream for error output.")


;; Function decls

(defmacro defsubst (&rest decl)
  "Defines a function that will be compiled inline to any functions that
call it. Otherwise exactly the same as defun."
  ;; These actions are also hard-coded into dump.jl
  (list 'prog1
	(cons 'defun decl)
	(list 'put (list 'quote (car decl)) ''compile-inline t)))

(defmacro function (arg)
  "#'ARG

Return the closure from ARG, either a lambda-expression, or a symbol.
When applied to a symbol, the symbol's value is returned."
  (if (symbolp arg)
      arg
    (list 'make-closure (list 'quote arg))))


;; Binding syntax

(defmacro let args
  "let [VAR] (BINDINGS...) BODY...

Binds temporary values to symbols while BODY is being evaluated.

Each of the BINDINGS is either a list `(SYMBOL FORMS...)' in which case
the variable SYMBOL is bound to the result of evaluating `(progn FORMS...)',
or a single symbol, in which case it is bound to the value `nil'.

If VAR is given, then the symbol VAR is bound to a function whose
formal parameters are the same as the variables bound by the `let'
form. Thus the execution of BODY... may be repeated by invoking VAR."

  ((lambda (fun vars values)
     (cond ((symbolp (car args))
	    ;; named let
	    (setq fun (car args))
	    (setq args (cdr args))))
     (setq vars (mapcar (lambda (x)
			  (if (consp x)
			      (car x)
			    x)) (car args)))
     (setq values (mapcar (lambda (x)
			    (if (consp x)
				(cons 'progn (cdr x))
			      nil)) (car args)))
     (cond (fun
	    ;; use the progn so the compiler notices the inner letrec
	    ;; (else it will get macroexpanded away too soon)
	    (list 'progn
		  (list 'letrec
			(list (list fun (list* 'lambda vars (cdr args))))
			(cons fun values))))
	   (t (cons (list* 'lambda vars (cdr args)) values))))
   nil nil nil))

(defmacro let* args
  "let (BINDINGS...) BODY...

Similar to `let' except that the BINDINGS are installed as their values
are computed, in the order they are written."

  (let loop ((rest (reverse (car args)))
	     (body (cons 'progn (cdr args))))
    (cond ((null rest) body)
	  (t (loop (cdr rest) (list 'let (list (car rest)) body))))))

(defmacro letrec (bindings . body)
  "Similar to `let' and `let*' except that the values of the BINDINGS
are evaluated such that all of the bound variables are in the scope.
This means that `letrec' may be used to define mutually recursive
functions."

  ((lambda (vars setters)
     (list* 'let vars (nconc setters body)))
   (mapcar (lambda (x)
	     (cond ((consp x) (car x))
		   (t x))) bindings)
   (mapcar (lambda (x)
	     (cond ((consp x) (cons 'setq x))
		   (t (list 'setq x nil)))) bindings)))


;; Conditional syntax

(defmacro if (condition then &rest else)
  (cond (else (list 'cond (list condition then) (cons t else)))
	(t (list 'cond (list condition then)))))

(defmacro when (condition &rest forms)
  "Evaluates CONDITION, if it is non-nil an implicit progn is performed
with FORMS."
  (list 'if condition (cons 'progn forms)))

(defmacro unless (condition &rest forms)
  "Evaluates CONDITION, if it is nil an implicit progn is performed with
FORMS."
  (list 'if (list 'not condition) (cons 'progn forms)))

(defmacro or args
  (cons 'cond (mapcar list args)))

(defmacro and args
  (let loop ((rest (nreverse args))
	     (body nil))
    (cond ((null rest) body)
	  (t (loop (cdr rest) (if body 
				  (list 'cond (list (car rest) body))
				(car rest)))))))


;; set syntax

(defmacro setq-default args
  "setq-default { VARIABLE FORM } ...

Sets the default value of each VARIABLE to the value of its
corresponding FORM evaluated, returns the value of the last evaluation.
See also `setq'. Returns the value of the last FORM."

  (let loop ((rest args)
	     (body nil))
    (if (null rest)
	(cons 'progn (nreverse body))
      (loop (cddr rest)
	    (cons (list 'set-default
			(list 'quote (car rest)) (cadr rest)) body)))))

;; XXX it would be nice to do the same for setq.. might stress the
;; XXX interpreter somewhat..? :-(


;; Function to allow easy creation of autoload stubs

(defun autoload (symbol file &rest extra)
  "Tell the evaluator that the value of SYMBOL will be initialised
by loading FILE."
  (unless (boundp symbol)
    (define-value symbol (make-closure (list* 'autoload symbol file extra)))))

(defun autoload-macro (symbol file &rest extra)
  "Tell the evaluator that the value of the macro SYMBOL will be initialised
by loading FILE."
  (unless (boundp symbol)
    (define-value symbol (cons 'macro (make-closure
				       (list* 'autoload symbol file extra))))))


;; Hook manipulation

(defun add-hook (hook-symbol new-func &optional at-end)
  "Arrange it so that FUNCTION-NAME is added to the hook-list stored in
symbol, HOOK-SYMBOL. It will added at the head of the list unless AT-END
is non-nil in which case it is added at the end."
  (unless (boundp hook-symbol)
    (make-variable-special hook-symbol)
    (set hook-symbol nil))
  (if at-end
      (set hook-symbol (nconc (symbol-value hook-symbol) (cons new-func nil)))
    (set hook-symbol (cons new-func (symbol-value hook-symbol)))))

(defun remove-hook (hook-symbol old-func)
  "Remove FUNCTION-NAME from the hook HOOK-SYMBOL."
  (set hook-symbol (delete old-func (symbol-value hook-symbol))))

(defun in-hook-p (hook-symbol fun)
  "Returns t if the function FUN is stored in the hook called HOOK-SYMBOL."
  (and (boundp hook-symbol) (memq fun (symbol-value hook-symbol))))

(defun eval-after-load (library form &aux tem)
  "Arrange for FORM to be evaluated immediately after the library of Lisp code
LIBRARY has been read by the `load' function. Note that LIBRARY must exactly
match the FILE argument to `load'."
  (if (setq tem (assoc library after-load-alist))
      (rplacd tem (cons form (cdr tem)))
    (setq after-load-alist (cons (cons library (list form)) after-load-alist))))


;; Miscellanea

(defun load-all (file)
  "Try to load files called FILE (or FILE.jl, etc) from all directories in the
LISP load path (except the current directory)."
  (letrec
      ((loop (lambda (dirs)
	       ;; Normally the last entry in load-path is `.' We don't
	       ;; want to use that. But can't just check if each item
	       ;; is the current directory since sometimes rep is run
	       ;; with REPLISPDIR=.
	       (when dirs
		 (when (or (cdr dirs) (not (member (car dirs) '("." ""))))
		   (let
		       ((full-name (expand-file-name file (car dirs))))
		     (when (or (file-exists-p full-name)
			       (file-exists-p (concat full-name ".jl"))
			       (file-exists-p (concat full-name ".jlc")))
		       (load full-name nil t))))
		 (loop (cdr dirs))))))
    (loop load-path)))

(defmacro eval-when-compile (form)
  "FORM is evaluated at compile-time *only*. The evaluated value is inserted
into the compiled program. When interpreted, nil is returned."
  nil)

(defun prin1-to-string (arg)
  "Return a string representing ARG."
  (format nil "%S" arg))

(defun read-from-string (string &optional start)
  "Reads an object from STRING, starting at character number START (default
is 0)."
  (read (make-string-input-stream string start)))

(defun assoc-regexp (input alist &optional fold-case)
  "Scan ALIST for an element whose car is a regular expression matching the
string INPUT."
  (catch 'return
    (mapc (lambda (cell)
	    (when (string-match (car cell) input nil fold-case)
	      (throw 'return cell))) alist)))

(defun file-newer-than-file-p (file1 file2)
  "Returns t when FILE1 was modified more recently than FILE2."
  (time-later-p (file-modtime file1) (file-modtime file2)))

;; Some function pseudonyms
(define-value 'setcar rplaca)
(define-value 'setcdr rplacd)
(define-value 'string= equal)
(define-value 'string< <)

(defun error (&rest args)
  (signal 'error (list (apply format nil args))))

(defun eval-and-print (form)
  "Eval FORM then print its value in the status line."
  (interactive "xEval: ")
  (prin1 (eval form) t))

(defun nop ()
  "A do-nothing command."
  (interactive))

(defun file-name= (name1 name2)
  "Returns t when NAME1 and NAME2 both name the same file."
  (string= (canonical-file-name name1) (canonical-file-name name2)))

(defun identity (arg)
  "Return ARG."
  arg)

;; Hide interactive decls
(defmacro interactive ())


;; cons accessors

(defun caar (x)
  (car (car x)))

(defun cdar (x)
  (cdr (car x)))

(defun cadr (x)
  (car (cdr x)))

(defun cddr (x)
  (cdr (cdr x)))

(defun caaar (x)
  (car (car (car x))))

(defun cdaar (x)
  (cdr (car (car x))))

(defun cadar (x)
  (car (cdr (car x))))

(defun cddar (x)
  (cdr (cdr (car x))))

(defun caadr (x)
  (car (car (cdr x))))

(defun cdadr (x)
  (cdr (car (cdr x))))

(defun caddr (x)
  (car (cdr (cdr x))))

(defun cdddr (x)
  (cdr (cdr (cdr x))))


;; some scheme compatibility functions

(defun call-with-current-continuation (f)
  "See `call/cc'."
  (call/cc f))

(defun dynamic-wind (before thunk after)
  "Call THUNK without arguments, returning the result of this call.
BEFORE and AFTER are also called (without arguments), whenever
execution respectively enters or leaves the dynamic extent of the call
to THUNK.

In the simplest case (when call/cc isn't used to pass control in or out
of THUNK) each function will be called exactly once."
  (before)
  (unwind-protect
      (call-with-barrier thunk nil before after)
    (after)))


;; numeric functions

(defun positivep (x)
  "Return t if X is greater than zero."
  (> x 0))

(defun negativep (x)
  "Return t if X is less than zero."
  (< x 0))

(defun oddp (x)
  "Return t if X is odd, i.e. (/= (mod X 2) 0)."
  (not (zerop (mod x 2))))

(defun evenp (x)
  "Return t if X is odd, i.e. (= (mod X 2) 0)."
  (zerop (mod x 2)))

(defun abs (x)
  "Return the absolute value of X, i.e. (max X (- X))."
  (max x (- x)))

(defun lcm (a b)
  "Return the least common multiple of integers A and B."
  (quotient (* a b) (gcd a b)))

(define-value '% remainder)
(define-value 'modulo mod)
(define-value 'lsh ash)


;; guardian wrapper

(defun make-guardian ()
  "Create a new guardian. Guardians provide a means of protecting data
objects from deallocation when they have no extant references.

`make-guardian' returns a function representing a single guardian.
Calling this function with a single argument adds that value to the
list of objects protected by the guardian. Calling the function with no
arguments returns one of the objects that would otherwise have been
deallocated by the garbage collector, or `nil' if no such objects
exist that have not already been returned."
  (let
      ((g (make-primitive-guardian)))
    (lambda (&optional arg)
      (if arg
	  (primitive-guardian-push g arg)
	(primitive-guardian-pop g)))))


;; entry point

;; null i18n function until gettext is loaded
(unless (boundp '_)
  (defun _ (arg) arg))

;; Setup format-hooks-alist to a few default'ish things
(setq format-hooks-alist '((?D . file-name-directory)
			   (?F . file-name-nondirectory)))
