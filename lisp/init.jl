#| init.jl -- Standard initialisation script

   $Id$

   Copyright (C) 1993, 1994, 2000 John Harper <john@dcs.warwick.ac.uk>

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

(defvar standard-output (stdout-file)
  "Stream that `prin?' writes its output to by default.")

(defvar standard-input (stdin-file)
  "Stream that `read' takes its input from by default.")

(defvar standard-error (stderr-file)
  "Standard stream for error output.")


;; Function decls

(progn					;progn forces compilation
  (setq defmacro
	(cons 'macro
	      (lambda (symbol . body)
"defmacro NAME LAMBDA-LIST [DOC-STRING] BODY...
defmacro NAME BYTECODE-OBJECT

Defines a macro called NAME with argument spec. LAMBDA-LIST,
documentation DOC-STRING (optional) and body BODY.

Macros are called with their arguments un-evaluated, they are expected
to return a form which will be executed to provide the result of the
expression. Note that macros are expanded at compile-time, and may be
expanded an arbitrary number of times."

	        (cond ((bytecodep (car body))
		       (setq body (car body)))
		      (t
		       (setq body (list 'quote (cons 'lambda body)))))
	        (list 'setq symbol
		      (list 'cons
			    (list 'quote 'macro)
			    (list 'make-closure body
				  (symbol-name symbol))))))))

(defmacro defun (symbol . body)
  "defun NAME LAMBDA-LIST [DOC-STRING] BODY...
defun NAME BYTECODE-OBJECT

Defines a function called NAME with argument specification LAMBDA-LIST,
documentation DOC-STRING (optional) and body BODY."

  (cond ((bytecodep (car body))
	 (setq body (car body)))
	(t
	 (setq body (list 'quote (cons 'lambda body)))))
  (list 'setq symbol (list 'make-closure body (symbol-name symbol))))

(defmacro defconst (symbol . args)
  "defconst NAME VALUE [DOC-STRING]

Define a constant NAME whose (default) value is VALUE. If NAME is
already bound an error is signalled.

Constants are treated specially by the Lisp compiler, basically they
are hard-coded into the byte-code."

  (list 'progn
	(list* 'define-value (list 'quote symbol) args)
	(list 'make-binding-immutable (list 'quote symbol))))

(defmacro defsubst (symbol . body)
  "Defines a function that will be compiled inline to any functions that
call it. Otherwise exactly the same as defun."
  ;; These actions are also hard-coded into dump.jl
  `(prog1 (defun ,symbol ,@body)
     (put ',symbol 'compile-inline ',(cons 'lambda body))))

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
     (cond ((cond ((car args) (symbolp (car args))))	;and expanded
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

(defmacro fluid-let (bindings . body)
  (let ((fluids nil)
	(values nil))
    (mapc (lambda (x)
	    (setq fluids (cons (car x) fluids))
	    (setq values (cons `(progn ,@(cdr x)) values))) bindings)
    `(with-fluids (list ,@fluids) (list ,@values) (lambda () ,@body))))


;; Conditional syntax

(defmacro if (condition then &rest else)
  "First the CONDITION form is evaluated, if it returns `t' (not `nil') the
TRUE-FORM is evaluated and its result returned. Otherwise the result of an
implicit progn on the ELSE forms is returned. If there are no ELSE forms
`nil' is returned."
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
  "The first of the ARGS is evaluated, if it is non-`nil' its value is the
value of the `or' form and no more arguments are evaluated. Otherwise this
step is repeated for the next member of ARGS.

If all of the ARGS have been evaluated and none have a non-`nil' value
`nil' is the value of the `or' form.

If there are no ARGS `nil' is returned."
  (if (null args)
      'nil
    (cons 'cond (mapcar list args))))

(defmacro and args
  "The first of the ARGS is evaluated. If it is `nil' no more of the
ARGS are evaluated and `nil' is the value of the `and' statement.
Otherwise the next member of ARGS is evaluated and its value tested. If
none of the ARGS are `nil' the computed value of the last member of ARGS
is returned from the `and' form."
  (if (null args)
      't
    (let loop ((rest (nreverse args))
	       (body nil))
      (cond ((null rest) body)
	    (t (loop (cdr rest) (if body 
				    (list 'cond (list (car rest) body))
				  (list 'cond (list (car rest))))))))))


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
			(list 'quote (car rest)) (nth 1 rest)) body)))))

(defmacro define-value (var-form value)
  (if (eq (car var-form) 'quote)
      ;; constant symbol
      (list 'setq (nth 1 var-form) value)
    ;; non-constant symbol
    ;; XXX highly dubious, and may need changing (only allow specials?)
    (list 'set var-form value)))

;; XXX it would be nice to do the same for setq.. might stress the
;; XXX interpreter somewhat..? :-(


;; Misc syntax

;; I could have written this using named let, but since rep currently
;; doesn't have a tail-recursive interpreter (only a compiler), using
;; `while' may be more useful..
(defmacro do (vars test . body)
  "do VARS (TEST EXPR...) BODY...

`do' is an iteration construct; VARS specifies a set of variable
bindings to be created, how they are initialized and how they are
updated on each iteration. TEST specifies the termination condition of
the loop, any EXPR... forms are evaluated immediately prior to exiting
the `do' construct. The BODY... forms specify the side effecting body
of the loop.

VARS is a list of variable clauses, each of which has the structure
`(VARIABLE INIT STEP)' where VARIABLE is the name of a variable, INIT
defines the initial value of its binding, and STEP defines how the next
value of the binding is computed. An alternative form is `(VARIABLE
INIT)', in this case the value of the binding does not change across
loop iterations.

Each iteration begins by evaluating TEST, if the result is `nil', then
the BODY... expressions are evaluated, and the variables bound to new
locations initialized to the results of evaluating the associated STEP
forms.

If the result of evaluating TEST is non-`nil' then the EXPR... forms
are evaluated, and the `do' construct returns the value of the last
EXPR form evaluated.

(do ((vec (make-vector 5))
     (i 0 (1+ i)))
    ((= i 5) vec)
  (aset vec i i)) => [0 1 2 3 4]"

  (let ((tem (gensym)))
    `(let ,tem ,(mapcar (lambda (var)
			  (list (car var) (cadr var))) vars)
       (if ,(car test)
	   (progn ,@(cdr test))
	 ,@body
	 (,tem ,@(mapcar (lambda (var)
			   (if (cddr var)
			       (caddr var)
			     (car var))) vars))))))

(defmacro while (condition . body)
  "while CONDITION BODY...

`while' is an imperative looping construct. CONDITION is evaluated, if
it produces a non-nil value, then the sequence of BODY... forms are
evaluated using an implicit `progn' statement, and control passes back
to the beginning of the while form.

When the VALUE of CONDITION is the symbol `nil', the while statement is
exited, returning an undefined value."

  ((lambda (tem)
     (list 'let tem '()
	   (list 'cond (list condition (cons 'progn body) (list tem)))))
   (gensym)))

(defmacro prog2 args
  "prog2 FORM1 FORM2 [FORMS...]

Evaluate FORM1 discarding its result, then evaluate FORM2 followed by
`(progn FORMS...)'. Returns the result of evaluating FORM2."

  (list 'progn (car args) (cons 'prog1 (cdr args))))

;; hide compiler declarations
(defmacro declare ())


;; structure (modules) syntax

(declare (in-module rep))

(%make-structure nil nil nil '%interfaces)

(defun %make-interface (name sig)
  (%structure-set (%get-structure '%interfaces) name sig))

(defun %parse-interface (sig)
  (cond ((null sig) '())
	((eq (car sig) 'export)
	 (cdr sig))
	((eq (car sig) 'compound-interface)
	 (apply append (mapcar %parse-interface (cdr sig))))
	((symbolp sig) (%structure-ref (%get-structure '%interfaces) sig))))

(defmacro define-interface (name sig)
  (list '%make-interface (list 'quote name)
	(list '%parse-interface (list 'quote sig))))

(defmacro structure (&optional sig config . body)
  (unless (listp (car config))
    (setq config (list config)))
  (list '%make-structure (list '%parse-interface (list 'quote sig))
	(list* 'lambda nil (cons '(open module-system) config))
	(list* 'lambda nil body)))

(defmacro define-structure (name &optional sig config . body)
  (unless (listp (car config))
    (setq config (list config)))
  (list '%make-structure (list '%parse-interface (list 'quote sig))
	(list* 'lambda nil (cons '(open module-system) config))
	(list* 'lambda nil body)
	(list 'quote name)))

(defmacro structure-open names
  (list '%open-structures (list 'quote names)))

(defmacro structure-access names
  (list '%access-structures (list 'quote names)))

(defmacro structure-ref (struct-name var-name)
  (list '%external-structure-ref
	(list 'quote struct-name) (list 'quote var-name)))

(define-interface %meta (export define-interface %make-interface
				open %open-structures
				access %access-structures quote))

(let
    ((meta-struct (%make-structure (%parse-interface '%meta) nil nil '%meta)))
  (%structure-set meta-struct 'quote quote)
  (%structure-set meta-struct 'open structure-open)
  (%structure-set meta-struct '%open-structures %open-structures)
  (%structure-set meta-struct 'access structure-access)
  (%structure-set meta-struct '%access-structures %access-structures))

(setq *root-structure* 'rep)


;; Function to allow easy creation of autoload stubs

(defmacro make-autoload (symbol-form file . rest)
  (list 'make-closure
	(list 'list* ''autoload symbol-form file (list 'quote rest))))

(defmacro autoload (symbol-form file &rest extra)
  "Tell the evaluator that the value of SYMBOL will be initialised
by loading FILE."
  (list 'setq (cadr symbol-form)
	(list* 'make-autoload symbol-form file extra)))

(defmacro autoload-macro (symbol-form file &rest extra)
  "Tell the evaluator that the value of the macro SYMBOL will be initialised
by loading FILE."
  (list 'setq (cadr symbol-form)
	(list 'cons ''macro
	      (list* 'make-autoload symbol-form file extra))))


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

(defun eval-after-load (library form)
  "Arrange for FORM to be evaluated immediately after the library of Lisp code
LIBRARY has been read by the `load' function. Note that LIBRARY must exactly
match the FILE argument to `load'."
  (let ((tem (assoc library after-load-alist)))
    (if tem
	(rplacd tem (cons form (cdr tem)))
      (setq after-load-alist (cons (cons library (list form))
				   after-load-alist)))))


;; loading / features

(defun load-all (file &optional callback)
  "Try to load files called FILE (or FILE.jl, etc) from all directories in the
LISP load path (except the current directory)."
  (let loop ((dirs load-path))
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
	    (if callback
		(callback full-name)
	      (load full-name nil t)))))
      (loop (cdr dirs)))))


;; Miscellanea

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

(defun streamp (arg)
  "Returns `t' if ARG is some sort of I/O stream."
  (or (input-stream-p arg) (output-stream-p arg)))

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

(defun caar (x) (car (car x)))
(defun cdar (x) (cdr (car x)))
(defun cadr (x) (car (cdr x)))
(defun cddr (x) (cdr (cdr x)))

(defun caaar (x) (car (caar x)))
(defun cdaar (x) (cdr (caar x)))
(defun cadar (x) (car (cdar x)))
(defun cddar (x) (cdr (cdar x)))
(defun caadr (x) (car (cadr x)))
(defun cdadr (x) (cdr (cadr x)))
(defun caddr (x) (car (cddr x)))
(defun cdddr (x) (cdr (cddr x)))

(defun caaaar (x) (caar (caar x)))
(defun cadaar (x) (cadr (caar x)))
(defun caadar (x) (caar (cdar x)))
(defun caddar (x) (cadr (cdar x)))
(defun caaadr (x) (caar (cadr x)))
(defun cadadr (x) (cadr (cadr x)))
(defun caaddr (x) (caar (cddr x)))
(defun cadddr (x) (cadr (cddr x)))
(defun cdaaar (x) (cdar (caar x)))
(defun cddaar (x) (cddr (caar x)))
(defun cdadar (x) (cdar (cdar x)))
(defun cdddar (x) (cddr (cdar x)))
(defun cdaadr (x) (cdar (cadr x)))
(defun cddadr (x) (cddr (cadr x)))
(defun cdaddr (x) (cdar (cddr x)))
(defun cddddr (x) (cddr (cddr x)))


;; some scheme compatibility functions

(define-value 'call-with-current-continuation call/cc)

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

(defun realp (x)
  "Return t if X is a real number."
  (numberp x))

(defun rationalp (x)
  "Return t if X is a (possibly inexact) rational number."
  (numberp x))

(defun inexactp (x)
  "Return t if X is an inexact number."
  (and (numberp x) (not (exactp x))))

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

(defun lcm args
  "Return the least common multiple of integers A and B."
  (if (null args)
      1
    (quotient (apply * (mapcar abs args)) (apply gcd args))))

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


;; file-handler definition

;; load this from the `rep' structure
(defun autoload-file-handler (symbol file)
  (define-file-handler symbol (make-autoload symbol file)))

(defun define-file-handler (name proc)
  (%structure-set (%current-structure) name proc))

(defun file-handler-ref (name)
  (%structure-ref (%current-structure) name))


;; default error handler

(defun default-error-handler (err data)
  (beep)
  (write t (format nil "*** %s: %s"
		   (or (get err 'error-message) err)
		   (mapconcat (lambda (x)
				(format nil "%s" x)) data ", "))))

(defvar error-handler-function default-error-handler)


;; entry point

;; null i18n function until gettext is loaded
(unless (boundp '_)
  (defun _ (arg) arg))

;; Setup format-hooks-alist to a few default'ish things
(setq format-hooks-alist '((?D . file-name-directory)
			   (?F . file-name-nondirectory)))



;; Load standard libraries

(load "rep-packages")

(require 'backquote)
(require 'tilde)

(load-all "rep-autoload")

;; It's up to the specific system's initialisatio scripts to do the
;; rest. Typically this will involve doing a (load-all "autoload.jl")
;; and loading .reprc and any other user configuration files. See
;; rep.jl for an example
