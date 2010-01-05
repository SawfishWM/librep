#| bootstrap for rep.lang.interpreter

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

(open-structures '(rep.lang.symbols
		   rep.data
		   rep.system
		   rep.io.streams
		   rep.io.files))

(%define nil '()
  "The value of the boolean-false and end-of-list object.")
(%define t 't
  "The symbol often used as the canonical boolean-true value.")
(make-binding-immutable 'nil)
(make-binding-immutable 't)

(%define #F nil)
(%define #T t)
(make-binding-immutable '#F)
(make-binding-immutable '#T)

(%define #undefined '#undefined)

(export-bindings '(nil t #F #T #undefined))


;; function syntax

(%define defmacro
      (cons 'macro
	    (lambda (symbol . body)
	      (cond ((bytecodep (car body))
		     (setq body (car body)))
		    (t (setq body (list 'quote (cons 'lambda body)))))
	      (list '%define symbol
		    (list 'cons
			  (list 'quote 'macro)
			  (list 'make-closure body
				(symbol-name symbol))))))
  "defmacro NAME LAMBDA-LIST [DOC-STRING] BODY...
defmacro NAME BYTECODE-OBJECT

Defines a macro called NAME with argument spec. LAMBDA-LIST,
documentation DOC-STRING (optional) and body BODY.

Macros are called with their arguments un-evaluated, they are expected
to return a form which will be executed to provide the result of the
expression. Note that macros are expanded at compile-time, and may be
expanded an arbitrary number of times.")

(defmacro defun (symbol . body)
  "defun NAME LAMBDA-LIST [DOC-STRING] BODY...
defun NAME BYTECODE-OBJECT

Defines a function called NAME with argument specification LAMBDA-LIST,
documentation DOC-STRING (optional) and body BODY."

  (cond ((bytecodep (car body))
	 (setq body (car body)))
	(t (setq body (list 'quote (cons 'lambda body)))))
  (list '%define symbol (list 'make-closure body (symbol-name symbol))))

(defmacro defconst (symbol value . rest)
  "defconst NAME VALUE [DOC-STRING]

Define a constant NAME whose (default) value is VALUE. If NAME is
already bound an error is signalled.

Constants are treated specially by the Lisp compiler, basically they
are hard-coded into the byte-code."

  (list 'progn
	(list* '%define symbol (list 'quote value) rest)
	(list '%make-binding-immutable (list 'quote symbol))))

(defmacro defsubst (symbol . body)
  "Defines a function that will be compiled inline to any functions that
call it. Otherwise exactly the same as defun."
  ;; These actions are also hard-coded into dump.jl
  (list* 'defun symbol body))

(defmacro function (arg)
  "#'ARG

Return the closure from ARG, either a lambda-expression, or a symbol.
When applied to a symbol, the symbol's value is returned."
  (if (symbolp arg)
      arg
    (list 'make-closure (list 'quote arg))))

(%define %make-binding-immutable make-binding-immutable)

(export-bindings '(defmacro defun defconst defsubst function
		   %make-binding-immutable))


;; Binding syntax

(defmacro let args
  "let [VAR] (BINDINGS...) BODY...

Binds temporary values to symbols while BODY is being evaluated.

Each of the BINDINGS is either a list `(SYMBOL FORMS...)' in which case
the variable SYMBOL is bound to the result of evaluating `(progn FORMS...)',
or a single symbol, in which case it is bound to the false value.

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
     (cond (fun (list 'letrec
		      (list (list fun (list* 'lambda vars (cdr args))))
		      (cons fun values)))
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
	     (cond ((consp x) (list 'setq (car x) (cons 'progn (cdr x))))
		   (t (list 'setq x nil)))) bindings)))

(defmacro let-fluids (bindings . body)
  "Similar to `let' except that the BINDINGS must refer to variables
containing fluid objects. The fluids will be bound to new locations,
not the variables containing the fluids."

  (let ((fluids nil)
	(values nil))
    (mapc (lambda (x)
	    (setq fluids (cons (car x) fluids))
	    (setq values (cons (cons 'progn (cdr x)) values))) bindings)
    (list 'with-fluids (cons 'list fluids)
	  (cons 'list values) (list* 'lambda '() body))))

(export-bindings '(let let* letrec let-fluids))


;; Conditional syntax

(defmacro if (condition then #!rest else)
  "First the CONDITION form is evaluated, if it returns true the
TRUE-FORM is evaluated and its result returned. Otherwise the result of
an implicit progn on the ELSE forms is returned. If there are no ELSE
forms the false value is returned."
  (cond (else (list 'cond (list condition then) (cons t else)))
	(t (list 'cond (list condition then)))))

(defmacro case (key . clauses)
  "Each CLAUSE is `((ITEMS... ) FORMS...)'. Find the first CLAUSE with an
ITEM matching (using `eql') the result of evaluating KEY (only
evaluated once), then evaluate the associated FORMS in a `progn'. The
final clause may have the form `(t FORMS...)', which always matches KEY
if no other CLAUSE has already. Returns false if no clause matches.

If any of the ITEMS appear more than once, then the behaviour is
undefined."
  (let ((tem (gensym)))
    (let loop ((body nil)
	       (rest clauses))
      (if rest
	  (let ((this (car rest)))
	    (loop (cons (cond
			 ((eq (car this) t) (cons 't (cdr this)))
			 ((cdar this)
			  (cons (list 'memql tem (list 'quote (car this)))
				(cdr this)))
			 (t (cons (list 'eql tem (list 'quote (caar this)))
				  (cdr this))))
			body)
		  (cdr rest)))
	(list 'let (list (list tem key))
	      (cons 'cond (nreverse body)))))))

(defmacro when (condition #!rest forms)
  "Evaluates CONDITION, if it is true an implicit progn is performed
with FORMS."
  (list 'if condition (cons 'progn forms)))

(defmacro unless (condition #!rest forms)
  "Evaluates CONDITION, if it is nil an implicit progn is performed with
FORMS."
  (list 'if (list 'not condition) (cons 'progn forms)))

(defmacro or args
  "The first of the ARGS is evaluated, if it is true its value is the value
of the `or' form and no more arguments are evaluated. Otherwise this step
is repeated for the next member of ARGS.

If all of the ARGS have been evaluated and none have a true value
`()' is the value of the `or' form.

If there are no ARGS the false value is returned."
  (if (null args)
      'nil
    (cons 'cond (mapcar list args))))

(defmacro and args
  "The first of the ARGS is evaluated. If it is false no more of the
ARGS are evaluated and the `and' statement evaluates to false.

Otherwise the next member of ARGS is evaluated and its value tested. If
none of the ARGS are false the computed value of the last member of ARGS
is returned from the `and' form."
  (if (null args)
      't
    (let loop ((rest (nreverse args))
	       (body nil))
      (cond ((null rest) body)
	    (t (loop (cdr rest) (if body 
				    (list 'cond (list (car rest) body))
				  (list 'cond (list (car rest))))))))))

(export-bindings '(if case when unless or and))


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

;; XXX it would be nice to do the same for setq.. might stress the
;; XXX interpreter somewhat..? :-(

(defmacro define-special-variable (var #!optional value doc)
  "define-special-variable VARIABLE [VALUE [DOC]]

Declares the symbol VARIABLE as a special variable, then
unconditionally sets its value to VALUE (or false if VALUE isn't
defined). If DOC is given it will be installed as the documentation
string associated with VARIABLE."

  (list 'progn
	(list 'defvar var nil doc)
	(list 'setq var value)))

(export-bindings '(setq-default define-special-variable))


;; Misc syntax

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

Each iteration begins by evaluating TEST, if the result is false, then
the BODY... expressions are evaluated, and the variables bound to new
locations initialized to the results of evaluating the associated STEP
forms.

If the result of evaluating TEST is true then the EXPR... forms are
evaluated, and the `do' construct returns the value of the last EXPR
form evaluated.

(do ((vec (make-vector 5))
     (i 0 (1+ i)))
    ((= i 5) vec)
  (aset vec i i)) => [0 1 2 3 4]"

  (let ((tem (gensym)))
    (list 'let tem (mapcar (lambda (var)
			     (list (car var) (nth 1 var))) vars)
	  (list* 'if (car test)
		 (cons 'progn (cdr test))
		 (append body (list (cons tem (mapcar (lambda (var)
							(if (cddr var)
							    (caddr var)
							  (car var)))
						      vars))))))))

(defmacro while (condition . body)
  "while CONDITION BODY...

`while' is an imperative looping construct. CONDITION is evaluated, if
it produces a true value, then the sequence of BODY... forms are
evaluated using an implicit `progn' statement, and control passes back
to the beginning of the while form.

When the VALUE of CONDITION is false, the while statement is exited,
returning an undefined value."

  ((lambda (tem)
     (list 'let tem '()
	   (list 'cond (list condition (cons 'progn body) (list tem)))))
   (gensym)))

(defmacro prog1 (form1 . forms)
  "First evals FORM1 then FORMS, returns the value that FORM1 gave."
  (let ((tem (gensym)))
    (list (list* 'lambda (list tem) (append forms (list tem))) form1)))

(defmacro prog2 args
  "prog2 FORM1 FORM2 [FORMS...]

Evaluate FORM1 discarding its result, then evaluate FORM2 followed by
`(progn FORMS...)'. Returns the result of evaluating FORM2."

  (list 'progn (car args) (cons 'prog1 (cdr args))))

(defmacro with-object (obj . body)
  "Evaluate OBJ and make its value ``current'' in some way meaningful
for the data type, evaluate all BODY forms, then return to the old
current value of whatever was changed. Return the value of the last
BODY form evaluated."
  (list 'call-with-object obj (list* 'lambda '() body)))

;; hide compiler declarations
(defmacro declare ()
  "declare CLAUSES...

Provide the compiler with extra information while compiling the forms
that appear in the same lexical scope as the declaration.

Each CLAUSE is a list, the first element of which is a symbol defining
the type of declaration, the other elements relate to the declaration.
See the `Compiler Declarations' node of the librep manual for details
of the possible declaration types.")

(export-bindings '(do while prog1 prog2 with-object declare))


;; exception handling and syntax

;; Call and return value of THUNK with a catch for TAG
(defun call-with-catch (tag thunk)
  (call-with-exception-handler
   thunk
   (lambda (data)
     (if (eq (car data) tag)
	 (cdr data)
       (raise-exception data)))))

;; Call and return value of THUNK. PROT-THUNK will always be called
;; after THUNK terminates, exception or no exception
(defun call-with-unwind-protect (thunk prot-thunk)
  (let (saved-data)
    (let ((ret (call-with-exception-handler
		thunk
		(lambda (data) (setq saved-data data)))))
      (prot-thunk)
      (if saved-data
	  (raise-exception saved-data)
	ret))))

;; HANDLERS is list of (ERROR-SPEC . HANDLER-FUN) HANDLER-FUN will be
;; called with a single arg, the list of error data
(defun call-with-error-handlers (thunk . handlers)
  (call-with-exception-handler
   thunk
   (lambda (data)
     (if (not (eq (car data) 'error))
	 (raise-exception data)
       (let ((type (nth 1 data)))
	 (let loop ((rest handlers))
	   (if (null rest)
	       (raise-exception data)
	     (let ((h-type (caar rest)))
	       (if (or (and (listp h-type) (memq type h-type))
		       (eq h-type 'error) (eq h-type type))
		   ((cdar rest) (cdr data))
		 (loop (cdr rest)))))))))))

(defmacro catch (tag . body)
  "Evaluate BODY in an implicit progn; non-local exits are allowed with
`(throw TAG)'. The value of the `catch' form is either the value of the
progn or the value given to any matching `throw' form."
  (list 'call-with-catch tag (list* 'lambda '() body)))

(defun throw (tag #!optional value)
  "Performs a non-local exit to the `catch' form waiting for TAG and return
VALUE from it."
  (raise-exception (cons tag value)))

(defmacro unwind-protect (form . body)
  "Return the result of evaluating FORM. When execution leaves the
dynamic extent of FORM evaluate `(progn BODY)' (even if exiting due to
an exception within FORM).

Note that when FORM is exited by calling a continuation, it is
undefined whether or not BODY will be evaluated."
  (list 'call-with-unwind-protect
	(list 'lambda '() form)
	(list* 'lambda () body)))

(defmacro condition-case (var form . handlers)
  "Evaluates FORM with error-handlers in place, if no errors occur
return the value returned by FORM, else the value of whichever
handler's body was evaluated.

Each HANDLER is a list of `(ERROR BODY...)'. ERROR defines which types
of errors the handler catches, either a symbol or a list of symbols.
The special symbol `error' matches all types of errors.

If VAR is true it's a symbol whose values is bound to `(ERROR-SYMBOL .
DATA)' while the handler is evaluated (these are the arguments given to
`signal' when the error was raised)."
  (list* 'call-with-error-handlers
	 (list 'lambda '() form)
	 (mapcar (lambda (h)
		   (list 'cons (list 'quote (car h))
			 (list* 'lambda (and (symbolp var)
					     (not (eq var 'nil))
					     (list var)) (cdr h))))
		 handlers)))

;; default error handler
(defun default-error-handler (err data)
  (call-with-exception-handler
   (lambda ()
     (beep)
     (write t (format nil "*** %s: %s"
		      (or (get err 'error-message) err)
		      (mapconcat (lambda (x)
				   (format nil "%s" x)) data ", ")))
     ;; XXX ugh.. so kludgey..
     (open-structures '(rep.lang.error-helper))
     (declare (bound error-helper))
     (error-helper err data))
   (lambda (ex)
     ;; really don't want to have errors happening in here..
     (unless (eq (car ex) 'error)
       (raise-exception ex)))))

(defvar error-handler-function default-error-handler)

(export-bindings '(call-with-catch call-with-unwind-protect
		   call-with-error-handlers catch throw
		   unwind-protect condition-case default-error-handler))


;; Function to allow easy creation of autoload stubs

(defmacro make-autoload (symbol-form file . rest)
  (list 'make-closure
	(list 'list* ''autoload symbol-form file (list 'quote rest))))

(defmacro autoload (symbol-form file #!rest extra)
  "Tell the evaluator that the value of SYMBOL will be initialised
by loading FILE."
  (list '%define (nth 1 symbol-form)
	(list* 'make-autoload symbol-form file extra)))

(defmacro autoload-macro (symbol-form file #!rest extra)
  "Tell the evaluator that the value of the macro SYMBOL will be initialised
by loading FILE."
  (list '%define (nth 1 symbol-form)
	(list 'cons ''macro
	      (list* 'make-autoload symbol-form file extra))))

(export-bindings '(make-autoload autoload autoload-macro))


;; some scheme compatibility functions

(%define call-with-current-continuation call/cc)

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

(export-bindings '(call-with-current-continuation dynamic-wind))


;; misc

(defun error (#!rest args)
  (signal 'error (list (apply format nil args))))

(defun identity (arg)
  "Return ARG."
  arg)

(defmacro eval-when-compile (form)
  "FORM is evaluated at compile-time *only*. The evaluated value is inserted
into the compiled program. When interpreted, nil is returned."
  (declare (unused form))
  nil)

;; Hide interactive decls
(defmacro interactive ())

(defun nop ()
  "A do-nothing command."
  (interactive))

(autoload-macro 'define "rep/lang/define")
(autoload-macro 'define-macro "rep/lang/define")
(autoload-macro 'with-internal-definitions "rep/lang/define")

(export-bindings '(error identity eval-when-compile nop interactive eval
		   define define-macro with-internal-definitions))

;; do this last since declare is defined in this file
(declare (in-module rep.lang.interpreter))
