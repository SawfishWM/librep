;;;; compiler.jl -- Simple compiler for Lisp files/forms
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

(require 'bytecodes)
(provide 'compiler)


;;; Notes:
;;;
;;; Instruction Encoding
;;; ====================
;;; Instructions which get an argument (with opcodes of zero up to
;;; `op-last-with-args') encode the type of argument in the low 3 bits
;;; of their opcode (this is why these instructions take up 8 opcodes).
;;; A value of 0 to 5 (inclusive) is the literal argument, value of
;;; 6 means the next byte holds the argument, or a value of 7 says
;;; that the next two bytes are used to encode the argument (in big-
;;; endian form, i.e. first extra byte has the high 8 bits)
;;;
;;; All instructions greater than the `op-last-before-jmps' are branches,
;;; currently only absolute destinations are supported, all branch
;;; instructions encode their destination in the following two bytes (also
;;; in big-endian form).
;;;
;;; Any opcode between `op-last-with-args' and `op-last-before-jmps' is
;;; a straightforward single-byte instruction.
;;;
;;; The machine simulated by lispmach.c is a simple stack-machine, each
;;; call to the byte-code interpreter gets its own stack; the size of
;;; stack needed is calculated by the compiler.
;;;
;;; If you hadn't already noticed I based this on the Emacs version 18
;;; byte-compiler.
;;;
;;; Constants
;;; =========
;;; `defconst' forms have to be used with some care. The compiler assumes
;;; that the value of the constant is always the same, whenever it is
;;; evaluated. It may even be evaluated more than once.
;;;
;;; In general, any symbols declared as constants (by defconst) have their
;;; values set in stone. These values are hard-coded into the compiled
;;; byte-code.
;;;
;;; Also, the value of a constant-symbol is *not* likely to be eq to itself!
;;;
;;; Use constants as you would use macros in C, i.e. to define values which
;;; have to be the same throughout a module. For example, this compiler uses
;;; defconst forms to declare the instruction opcodes.
;;;
;;; If you have doubts about whether or not to use constants -- don't; it may
;;; lead to subtle bugs.
;;;
;;; Inline Functions
;;; ================
;;; The defsubst macro allows functions to be defined which will be open-
;;; coded into any callers at compile-time. Of course, this can have a
;;; similar effect to using a macro, with some differences:
;;;
;;;	* Macros can be more efficient, since the formal parameters
;;;	  are only bound at compile time. But, this means that the
;;;	  arguments may be evaluated more than once, unlike a defsubst
;;;	  where applied forms will only ever be evaluated once, when
;;;	  they are bound to a formal parameter
;;;
;;;	* Macros are more complex to write; though the backquote mechanism
;;;	  can help a lot with this
;;;
;;;	* defsubst's are more efficient in uncompiled code, but this
;;;	  shouldn't really be a consideration, unless code is being
;;;	  generated on the fly
;;;
;;; Warnings
;;; ========
;;; Currently warnings are generated for the following situations:
;;;
;;;	* Functions or special variables are multiply defined
;;;	* Undefined variables are referenced or set ("undefined" means
;;;	  not defined by defvar, not currently (lexically) bound, and
;;;	  not boundp at compile-time)
;;;	* Undefined functions are referenced, that is, not defun'd and
;;;	  not fboundp at compile-time
;;;	* Functions are called with an incorrect number of arguments,
;;;	  either too few required parameters, or too many supplied
;;;	  to a function without a &rest keyword
;;;	* Unreachable code in conditional statements
;;;	* Possibly some other things...
;;;
;;; TODO
;;; ====

;;; Obviously, more optimisation of output code. This isdone in two
;;; stages, (1) source code transformations, (2) optimisation of
;;; intermediate form (basically bytecode, but as a list of operations
;;; and symbolic labels, i.e. the basic blocks)
;;;
;;; Both (1) and (2) are already being done, but there's probably
;;; scope for being more aggressive, especially at the source code
;;; (parse tree) level.
;;;
;;; Optimisation would be a lot more profitable if variables were
;;; lexically scoped, perhaps I should switch to lexical scoping. It
;;; shouldn't break anything much, since the compiler will give
;;; warnings if any funky dynamic-scope tricks are used without the
;;; symbols being defvar'd (and therefore declared special/dynamic)
;;;
;;; The constant folding code is a bit simplistic. For example the form
;;; (+ 1 2 3) would be folded to 6, but (+ 1 2 x) *isn't* folded to
;;; (+ 3 x) as we would like. This is due to the view of folded functions
;;; as ``side-effect-free constant black boxes''.

;; 8/11/99: lexical scoping has arrived.. and it works.. and the
;; performance hit is minimal :-)

;; so I need to do all those funky lexical scope optimisation now..


;; Options

(defvar comp-write-docs nil
  "When t all doc-strings are appended to the doc file and replaced with
their position in that file.")

(defvar comp-max-inline-depth 8
  "The maximum nesting of open-coded function applications.")

(defvar comp-no-low-level-optimisations nil)
(defvar comp-debug nil)

(defvar comp-constant-functions
  '(+ - * / % mod max min 1+ 1- car cdr assoc assq rassoc rassq nth nthcdr
    last member memq arrayp aref substring concat length elt lognot not
    logior logxor logand equal = /= > < >= <= ash zerop null atom consp
    listp numberp integerp stringp vectorp bytecodep functionp macrop
    special-form-p subrp sequencep quotient floor ceiling truncate round
    exp log sin cos tan asin acos atan sqrt expt)
  "List of side-effect-free functions. They should always return the same
value when given the same inputs. Used when constant folding.")

(defvar comp-nth-insns (list (cons 0 op-car)
			     (cons 1 op-cadr)
			     (cons 2 op-caddr)
			     (cons 3 op-cadddr)
			     (cons 4 op-caddddr)
			     (cons 5 op-cadddddr)
			     (cons 6 op-caddddddr)
			     (cons 7 op-cadddddddr)))

(defvar comp-nthcdr-insns (list (cons 0 nil)
				(cons 1 op-cdr)
				(cons 2 op-cddr)))


;; Environment of this byte code sequence being compiled

;; Output state
(defvar comp-constant-alist '())	;list of (VALUE . INDEX)
(defvar comp-constant-index 0)		;next free constant index number
(defvar comp-current-stack 0)		;current stack requirement
(defvar comp-max-stack 0)		;highest possible stack
(defvar comp-output nil)		;list of (BYTE . INDEX)
(defvar comp-output-pc 0)		;INDEX of next byte
(defvar comp-intermediate-code nil)	;list of (INSN . [ARG]), (TAG . REFS)

;; Compilation "environment"
(defvar comp-macro-env			;alist of (NAME . MACRO-DEF)
  (list (cons 'eval-when-compile (lambda (x)
				   `(quote ,(eval x))))))
(defvar comp-const-env '())		;alist of (NAME . CONST-DEF)
(defvar comp-inline-env '())		;alist of (NAME . FUNCTION-VALUE)
(defvar comp-defuns nil)		;alist of (NAME REQ OPT RESTP)
					; for all functions/macros in the file
(defvar comp-defvars nil)		;all variables declared at top-level
(defvar comp-defines nil)		;all lex. vars. declared at top-level
(defvar comp-spec-bindings '())		;list of currently bound variables
(defvar comp-lex-bindings '())		;list of currently bound variables
(defvar comp-current-file nil)		;the file being compiled
(defvar comp-current-fun nil)		;the function being compiled
(defvar comp-inline-depth 0)		;depth of lambda-inlining
(defvar comp-lexically-pure t)		;any dynamic state?
(defvar comp-lambda-name nil)		;name of current lambda exp
(defvar comp-lambda-args nil)		;arg spec of current lambda

(defvar comp-output-stream nil)		;stream for compiler output


;; Message output

(defun comp-message (fmt &rest args)
  (when (null comp-output-stream)
    (if (or batch-mode (not (featurep 'jade)))
	(setq comp-output-stream (stdout-file))
      (setq comp-output-stream (open-buffer "*compilation-output*"))))
  (when (and (featurep 'jade)
	     (bufferp comp-output-stream)
	     (not (eq (current-buffer) comp-output-stream)))
    (goto-buffer comp-output-stream)
    (goto (end-of-buffer)))
; (when comp-current-file
;   (format comp-output-stream "%s:" comp-current-file))
  (when comp-current-fun
    (format comp-output-stream "%s:" comp-current-fun))
  (apply format comp-output-stream fmt args))

(put 'compile-error 'error-message "Compilation mishap")
(defun comp-error (&rest data)
  (signal 'compile-error data))

(defun comp-warning (fmt &rest args)
  (apply comp-message fmt args)
  (write comp-output-stream "\n"))


;; Code to handle warning tests

;; Note that there's a function or macro NAME with lambda-list ARGS
;; in the current file
(defun comp-remember-fun (name args)
  (if (assq name comp-defuns)
      (comp-warning "Multiply defined function or macro: %s" name)
    (let
	((required 0)
	 (optional nil)
	 (rest nil)
	 (state 'required))
      ;; Scan the lambda-list for the number of required and optional
      ;; arguments, and whether there's a &rest clause
      (while args
	(if (symbolp args)
	    ;; (foo . bar)
	    (setq rest t)
	  (if (memq (car args) '(&optional &rest &aux))
	      (cond
	       ((eq (car args) '&optional)
		(setq state 'optional)
		(setq optional 0))
	       ((eq (car args) '&rest)
		(setq args nil)
		(setq rest t))
	       ((eq (car args) '&aux)
		(setq args nil)))
	    (set state (1+ (symbol-value state)))))
	  (setq args (cdr args)))
      (setq comp-defuns (cons (list name required optional rest)
			      comp-defuns)))))

;; Similar for variables
(defun comp-remember-var (name)
  (cond ((memq name comp-defines)
	 (comp-error "Variable %s was previously declared lexically" name))
	((memq name comp-defvars)
	 (comp-warning "Multiply defined variable: %s" name))
	(t
	 (setq comp-defvars (cons name comp-defvars)))))

(defun comp-remember-lexical-var (name)
  (cond ((memq name comp-defvars)
	 (comp-error "Variable %s was previously declared special" name))
	((memq name comp-defines)
	 (comp-warning "Multiply defined lexical variable: %s" name))
	(t
	 (setq comp-defines (cons name comp-defines)))))

;; Test that a reference to variable NAME appears valid
(defun comp-test-varref (name)
  (when (and (symbolp name)
	     (null (memq name comp-defvars))
	     (null (memq name comp-defines))
	     (null (memq name comp-spec-bindings))
	     (null (memq name comp-lex-bindings))
	     (null (assq name comp-defuns))
	     (not (special-variable-p name))
	     (not (const-variable-p name))
	     (not (boundp name)))
    (comp-warning "Reference to undeclared free variable: %s" name)))

;; Test that binding to variable NAME appears valid
(defun comp-test-varbind (name)
  (cond ((assq name comp-defuns)
	 (comp-warning "Binding to %s shadows function" name))
	;((memq name comp-defvars)
	; (comp-warning "Binding to %s shadows special variable" name))
	((or (memq name comp-spec-bindings)
	     (memq name comp-lex-bindings))
	 (comp-warning "Binding to %s shadows earlier binding" name))
	((and (boundp name) (functionp (symbol-value name)))
	 (comp-warning "Binding to %s shadows pre-defined value" name))))

;; Test a call to NAME with NARGS arguments
;; XXX functions in comp-fun-bindings aren't type-checked
(defun comp-test-funcall (name nargs)
  (when (symbolp name)
    (catch 'return
      (let
	  ((decl (assq name comp-defuns)))
	(when (and (null decl) (or (boundp name) (assq name comp-inline-env)))
	  (setq decl (or (cdr (assq name comp-inline-env))
			 (symbol-value name)))
	  (when (or (subrp decl)
		    (and (closurep decl)
			 (eq (car (closure-function decl)) 'autoload)))
	    (throw 'return))
	  (when (eq (car decl) 'macro)
	    (setq decl (cdr decl)))
	  (when (closurep decl)
	    (setq decl (closure-function decl)))
	  (if (bytecodep decl)
	      (comp-remember-fun name (aref decl 0))
	    (comp-remember-fun name (nth 1 decl)))
	  (setq decl (assq name comp-defuns)))
	(if (null decl)
	    (unless (or (memq name comp-spec-bindings)
			(memq name comp-lex-bindings)
			(memq name comp-defvars)
			(memq name comp-defines))
	      (comp-warning "Call to undeclared function: %s" name))
	  (let
	      ((required (nth 1 decl))
	       (optional (nth 2 decl))
	       (rest (nth 3 decl)))
	    (if (< nargs required)
		(comp-warning "%d arguments required by %s; %d supplied"
			      required name nargs)
	      (when (and (null rest) (> nargs (+ required (or optional 0))))
		(comp-warning "Too many arguments to %s (%d given, %d used)"
			      name nargs (+ required (or optional 0)))))))))))


;; Top level entrypoints

(defvar comp-top-level-compiled
  '(if cond when unless let let* catch unwind-protect condition-case
    progn prog1 prog2 while and or case)
  "List of symbols, when the name of the function called by a top-level form
is one of these that form is compiled.")

(defvar comp-top-level-unexpanded
  '(defun defmacro defvar defconst defsubst require define-value)
  "List of symbols, when the car of a top-level form is a member of this list,
don't macroexpand the form before compiling.")

;;;###autoload
(defun compile-file (file-name)
  "Compiles the file of jade-lisp code FILE-NAME into a new file called
`(concat FILE-NAME ?c)' (ie, `foo.jl' => `foo.jlc')."
  (interactive "fLisp file to compile:")
  (let
      ((comp-current-file file-name)
       (comp-macro-env comp-macro-env)
       (comp-const-env '())
       (comp-inline-env '())
       (comp-defuns '())
       (comp-defvars '())
       (comp-defines '())
       (comp-spec-bindings '())
       (comp-lex-bindings '())
       (comp-lexically-pure t)
       (comp-output-stream nil)
       (temp-file (make-temp-name))
       src-file dst-file input-forms header form)
    (unwind-protect
	(progn
	  (message (concat "Compiling " file-name "...") t)
	  (when (setq src-file (open-file file-name 'read))
	    (unwind-protect
		(progn
		  ;; Pass 1. [read the file, remembering definitions]

		  ;; First check for `#! .. !#' at start of file
		  (if (and (= (read-char src-file) ?#)
			   (= (read-char src-file) ?!))
		      (let
			  ((out (make-string-output-stream))
			   tem)
			(write out "#!")
			(catch 'done
			  (while (setq tem (read-char src-file))
			    (write out tem)
			    (when (and (= tem ?!)
				       (setq tem (read-char src-file)))
			      (write out tem)
			      (when (= tem ?#)
				(throw 'done t)))))
			(setq header (get-output-stream-string out)))
		    (seek-file src-file 0 'start))

		  ;; Scan for top-level definitions in the file.
		  ;; Also eval require forms (for macro defs)
		  (condition-case nil
		      (while t
			(setq form (read src-file))
			(unless (memq (car form) comp-top-level-unexpanded)
			  (setq form (macroexpand form comp-macro-env)))
			(cond
			 ((eq (car form) 'defun)
			  (comp-remember-fun (nth 1 form) (nth 2 form)))
			 ((eq (car form) 'defmacro)
			  (comp-remember-fun (nth 1 form) (nth 2 form))
			  (setq comp-macro-env
				(cons (cons (nth 1 form)
					    (make-closure
					     (cons 'lambda (nthcdr 2 form))))
				      comp-macro-env)))
			 ((eq (car form) 'defsubst)
			  (setq comp-inline-env
				(cons (cons (nth 1 form)
					    (cons 'lambda (nthcdr 2 form)))
				      comp-inline-env)))
			 ((eq (car form) 'defvar)
			  (comp-remember-var (nth 1 form)))
			 ((eq (car form) 'defconst)
			  (comp-remember-var (nth 1 form))
			  (setq comp-const-env (cons (cons (nth 1 form)
							   (nth 2 form))
						     comp-const-env)))
			 ((eq (car form) 'define-value)
			  (let
			      ((sym (nth 1 form)))
			    (when (comp-constant-p sym)
			      (comp-remember-lexical-var
			       (comp-constant-value sym)))))
			 ((eq (car form) 'require)
			  (eval form)))
			(setq input-forms (cons form input-forms)))
		    (end-of-stream)))
	      (close-file src-file))
	    (setq input-forms (nreverse input-forms))
	    (when (setq dst-file (open-file temp-file 'write))
	      (condition-case error-info
		  (unwind-protect
		      (progn
			;; Pass 2. The actual compile
			(when header
			  (write dst-file header))
			(format dst-file
				";; Source file: %s\n(validate-byte-code %d %d)\n"
				file-name bytecode-major bytecode-minor)
			(while (setq form (car input-forms))
			  (if (memq (car form) comp-top-level-unexpanded)
			      (setq form (comp-compile-top-form form))
			    ;; just in case?
			    (setq form (macroexpand form comp-macro-env))
			    (when (memq (car form) comp-top-level-compiled)
			      ;; Compile this form
			      (setq form (compile-form form))))
			  (when form
			    (print form dst-file))
			  (setq input-forms (cdr input-forms)))
			(write dst-file ?\n))
		    (close-file dst-file))
		(error
		 ;; Be sure to remove any partially written dst-file.
		 ;; Also, signal the error again so that the user sees it.
		 (delete-file temp-file)
		 ;; Hack to signal error without entering the debugger (again)
		 (throw 'error error-info)))
	      ;; Copy the file to its correct location, and copy
	      ;; permissions from source file
	      (let
		  ((real-name (concat file-name (if (string-match
						     "\\.jl$" file-name)
						    ?c ".jlc"))))
		(copy-file temp-file real-name)
		(set-file-modes real-name (file-modes file-name)))
	      t)))
      (when (file-exists-p temp-file)
	(delete-file temp-file)))))

;;;###autoload
(defun compile-directory (dir-name &optional force-p exclude-list)
  "Compiles all jade-lisp files in the directory DIRECTORY-NAME whose object
files are either older than their source file or don't exist. If FORCE-P
is non-nil every lisp file is recompiled.
EXCLUDE-LIST is a list of files which shouldn't be compiled."
  (interactive "DDirectory of Lisp files to compile:\nP")
  (let
      ((dir (directory-files dir-name)))
    (while (consp dir)
      (when (and (string-match "\\.jl$" (car dir))
		 (null (member (car dir) exclude-list)))
	(let*
	    ((file (expand-file-name (car dir) dir-name))
	     (cfile (concat file ?c)))
	  (when (or (not (file-exists-p cfile))
		    (file-newer-than-file-p file cfile))
	    (compile-file file))))
      (setq dir (cdr dir)))
    t))

(defvar compile-lib-exclude-list
  '("autoload.jl"))

;;;###autoload
(defun compile-lisp-lib (&optional directory force-p)
  "Recompile all out of date files in the lisp library directory. If FORCE-P
is non-nil it's as though all files were out of date.
This makes sure that all doc strings are written to their special file and
that files which shouldn't be compiled aren't."
  (interactive "\nP")
  (let
      ((comp-write-docs t))
    (compile-directory (or directory lisp-lib-directory)
		       force-p compile-lib-exclude-list)))

;; Call like `rep --batch -l compiler -f compile-lib-batch [--force] DIR'
(defun compile-lib-batch ()
  (let
      ((force (when (equal (car command-line-args) "--force")
		(setq command-line-args (cdr command-line-args))
		t))
       (dir (car command-line-args)))
    (setq command-line-args (cdr command-line-args))
    (compile-lisp-lib dir force)))

;; Call like `rep --batch -l compiler -f compile-batch [--write-docs] FILES...'
(defun compile-batch ()
  (when (get-command-line-option "--write-docs")
    (setq comp-write-docs t))
  (while command-line-args
    (compile-file (car command-line-args))
    (setq command-line-args (cdr command-line-args))))

;; Used when bootstrapping from the Makefile, recompiles compiler.jl if
;; it's out of date
(defun compile-compiler ()
  (let
      ((comp-write-docs t))
    (mapc (lambda (file)
	    (setq file (expand-file-name file lisp-lib-directory))
	    (when (or (not (file-exists-p (concat file ?c)))
		      (file-newer-than-file-p file (concat file ?c)))
	      (compile-file file)))
	  '("compiler.jl" "compiler-opt.jl" "sort.jl"))))

;;;###autoload
(defun compile-function (function)
  "Compiles the body of the function FUNCTION."
  (interactive "aFunction to compile:")
  (let*
      ((fbody (if (symbolp function)
		  (symbol-value function)
		function))
       (body (if (closurep fbody) (closure-function fbody) fbody))
       (comp-current-fun function)
       (comp-defuns nil)
       (comp-defvars nil)
       (comp-defines nil)
       (comp-output-stream nil))
    (when (assq 'jade-byte-code body)
      (comp-error "Function already compiled" function))
    (setq body (comp-compile-lambda body function))
    (if (closurep fbody)
	(set-closure-function fbody body)
      (set function body))
    function))
    

;; Low level compilation

;; Compile a form which occurred at the `top-level' into a byte code form.
;; defuns, defmacros, defvars, etc... are treated specially.
;; require forms are evaluated before being output uncompiled; this is so
;; any macros are brought in before they're used.
(defun comp-compile-top-form (form)
  (let
      ((fun (car form)))
    (cond
     ((eq fun 'defun)
      (let
	  ((tmp (assq (nth 1 form) comp-macro-env))
	   (comp-current-fun (nth 1 form)))
	(when tmp
	  (rplaca tmp nil)
	  (rplacd tmp nil))
	(list 'defun (nth 1 form)
	      (comp-compile-lambda (cons 'lambda (nthcdr 2 form))
				   (nth 1 form)))))
     ((eq fun 'defmacro)
      (let
	  ((code (comp-compile-lambda (cons 'lambda (nthcdr 2 form))
				      (nth 1 form)))
	   (tmp (assq (nth 1 form) comp-macro-env))
	   (comp-current-fun (nth 1 form)))
	(if tmp
	    (rplacd tmp (make-closure code))
	  (comp-error "Compiled macro wasn't in environment" (nth 1 form)))
	(list 'defmacro (nth 1 form) code)))
     ((eq fun 'defsubst)
      (when comp-write-docs
	(cond
	 ((stringp (nth 3 form))
	  (add-documentation (nth 1 form) (nth 3 form))
	  (setq form (delq (nth 3 form) form)))
	 ((stringp (nth 4 form))
	  (add-documentation (nth 1 form) (nth 4 form))
	  (setq form (delq (nth 4 form) form)))))
      (unless (assq (nth 1 form) comp-inline-env)
	(comp-error "Inline function wasn't in environment" (nth 1 form)))
      form)
     ((eq fun 'defconst)
      (let
	  ((value (eval (nth 2 form)))
	   (doc (nth 3 form)))
	(when (and comp-write-docs (stringp doc))
	  (add-documentation (nth 1 form) doc)
	  (setq form (delq (nth 3 form) form)))
	(unless (memq (nth 1 form) comp-defvars)
	  (comp-remember-var (nth 1 form)))
	(unless (assq (nth 1 form) comp-const-env)
	  (comp-warning "Constant wasn't in environment" (nth 1 form))))
      form)
     ((eq fun 'defvar)
      (let
	  ((value (nth 2 form))
	   (doc (nth 3 form)))
	(when (and (listp value)
		   (not (comp-constant-p value)))
	  ;; Compile the definition. A good idea?
	  (rplaca (nthcdr 2 form) (compile-form (nth 2 form))))
	(when (and comp-write-docs (stringp doc))
	  (add-documentation (nth 1 form) doc)
	  (setq form (delq (nth 3 form) form)))
	(unless (memq (nth 1 form) comp-defvars)
	  (comp-remember-var (nth 1 form))))
      form)
     ((eq fun 'define-value)
      (let
	  ((sym (nth 1 form))
	   (value (nth 2 form)))
	(when (comp-constant-p sym)
	  (setq sym (comp-constant-value sym))
	  (unless (memq sym comp-defines)
	    (comp-remember-lexical-var (comp-constant-value sym))))
	(when (and (listp value) (not (comp-constant-p value)))
	  ;; Compile the definition. A good idea?
	  (rplaca (nthcdr 2 form) (compile-form (nth 2 form))))
	form))
     ((eq fun 'require)
      (eval form)
      form)
     (t
      (comp-error "Shouldn't have got here!")))))

;;;###autoload
(defun compile-form (form)
  "Compile the Lisp form FORM into a byte code form."
  (let
      (comp-constant-alist
       (comp-constant-index 0)
       (comp-current-stack 0)
       (comp-max-stack 0)
       comp-output
       (comp-output-pc 0)
       (comp-intermediate-code nil))

    ;; Do the high-level compilation
    (comp-compile-form form t)
    (comp-write-op op-return)

    ;; Now we have a [reversed] list of intermediate code
    (setq comp-intermediate-code (nreverse comp-intermediate-code))

    ;; Unless disabled, run the peephole optimiser
    (unless comp-no-low-level-optimisations
      (require 'compiler-opt)
      (when comp-debug
	(format standard-error "lap-0 code: %S\n\n" comp-intermediate-code))
      (setq comp-intermediate-code (comp-peephole-opt comp-intermediate-code)))
    (when comp-debug
      (format standard-error "lap-1 code: %S\n\n" comp-intermediate-code))

    ;; Then optimise the constant layout
    (unless comp-no-low-level-optimisations
      (require 'compiler-opt)
      (when comp-debug
	(format standard-error
		"original-constants: %S\n\n" comp-constant-alist))
      (setq comp-intermediate-code
	    (comp-optimise-constants comp-intermediate-code))
      (when comp-debug
	(format standard-error
		"final-constants: %S\n\n" comp-constant-alist)))

    ;; Now transform the intermediate code to byte codes
    (comp-assemble-bytecodes)
    (when comp-debug
      (format standard-error "lap-2 code: %S\n\n" comp-intermediate-code))
    (when comp-output
      (list 'jade-byte-code (comp-make-code-string) (comp-make-const-vec)
	    comp-max-stack))))

;; Turn the alist of byte codes into a string
(defun comp-make-code-string ()
  (let
      ((code-string (make-string comp-output-pc ?*))
       (data comp-output))
    (while (consp data)
      (aset code-string (cdr (car data)) (car (car data)))
      (setq data (cdr data)))
    code-string))

;; Turn the alist of constants into a vector
(defun comp-make-const-vec ()
  (let
      ((vec (make-vector comp-constant-index))
       (consts comp-constant-alist))
    (while (consp consts)
      (aset vec (cdr (car consts)) (car (car consts)))
      (setq consts (cdr consts)))
    vec))

;; Increment the current stack size, setting the maximum stack size if
;; necessary
(defmacro comp-inc-stack (&optional n)
  (list 'when (list '> (list 'setq 'comp-current-stack
			     (if n
				 (list '+ 'comp-current-stack n)
			       (list '1+ 'comp-current-stack)))
		       'comp-max-stack)
	'(setq comp-max-stack comp-current-stack)))

;; Decrement the current stack usage
(defmacro comp-dec-stack (&optional n)
  (list 'setq 'comp-current-stack 
	(if n
	    (list '- 'comp-current-stack n)
	  (list '1- 'comp-current-stack))))

;; Compile one form so that its value ends up on the stack when interpreted
(defun comp-compile-form (form &optional return-follows)
  (cond
    ((eq form nil)
      (comp-write-op op-nil)
      (comp-inc-stack))
    ((eq form t)
      (comp-write-op op-t)
      (comp-inc-stack))
    ((symbolp form)
     ;; A variable reference
     (let
	 (val)
       (comp-test-varref form)
       (cond
	((const-variable-p form)
	 ;; A constant already interned
	 (comp-compile-constant (symbol-value form)))
	((setq val (assq form comp-const-env))
	 ;; A constant from this file
	 (comp-compile-constant (cdr val)))
	(t
	 ;; Not a constant
	 (if (comp-spec-bound-p form)
	     ;; Specially bound
	     (comp-write-op op-refq (comp-add-constant form))
	   (let
	       ((lex-addr (comp-binding-lexical-addr form)))
	     (if lex-addr
		 ;; We know the lexical address, so use it
		 (comp-write-op op-refn lex-addr)
	       ;; It's not bound, so just update the global value
	       (comp-write-op op-refg (comp-add-constant form)))))
	 (comp-inc-stack)))))
    ((consp form)
     (when (and (memq (car form) comp-constant-functions)
		(not (or (memq (car form) comp-spec-bindings)
			 (memq (car form) comp-lex-bindings))))
       ;; See if this form can be folded
       (setq form (comp-fold-constants form))
       ;; If the form is still a function application, avoid the
       ;; extra recursion
       (unless (consp form)
	 (comp-compile-form form return-follows)
	 (setq form nil)))
     (unless (null form)
       ;; A subroutine application of some sort
       (comp-test-funcall (car form) (length (cdr form)))
       (let
	   (fun)
	 (cond
	  ;; Check if there's a source code transformation
	  ((and (symbolp (car form))
		(setq fun (get (car form) 'compile-transform)))
	   ;; Yes, there is, so call it.
	   (comp-compile-form (funcall fun form) return-follows))

	  ;; Check if there's a special handler for this function
	  ((and (symbolp (car form))
		(setq fun (get (car form) 'compile-fun))
		(not (or (memq (car form) comp-spec-bindings)
			 (memq (car form) comp-lex-bindings))))
	   (fun form return-follows))

	  ;; Is it a function to be inlined?
	  ((and (symbolp (car form)) (get (car form) 'compile-inline))
	   (comp-compile-inline-function form))

	  (t
	   ;; Expand macros
	   (if (not (eq (setq fun (macroexpand form comp-macro-env)) form))
	       ;; The macro did something, so start again
	       (comp-compile-form fun return-follows)
	     ;; No special handler, so do it ourselves
	     (setq fun (car form))
	     (cond
	      ((and (consp fun) (eq (car fun) 'lambda))
	       ;; An inline lambda expression
	       (comp-compile-lambda-inline (car form) (cdr form)))
	      ((and (symbolp fun) (assq fun comp-inline-env))
	       ;; A call to a function that should be open-coded
	       (comp-compile-lambda-inline (cdr (assq fun comp-inline-env))
					   (cdr form)))
	      (t
	       ;; Assume a normal function call
	       (if (and return-follows
			comp-lexically-pure
			(eq fun comp-lambda-name))
		   (progn
		     (comp-do-tail-call comp-lambda-args (cdr form))
		     ;; fake it, the next caller will pop the (non-existant)
		     ;; return value
		     (comp-inc-stack))
		 (comp-compile-form fun)
		 (setq form (cdr form))
		 (let
		     ((i 0))
		   (while (consp form)
		     (comp-compile-form (car form))
		     (setq i (1+ i)
			   form (cdr form)))
		   (comp-write-op op-call i)
		   (comp-dec-stack i)))))))))))
    (t
     ;; Not a variable reference or a function call; so what is it?
     (comp-compile-constant form))))

;; Push a constant onto the stack
(defun comp-compile-constant (form)
  (cond
   ((eq form nil)
    (comp-write-op op-nil))
   ((eq form t)
    (comp-write-op op-t))
   ((and (integerp form) (<= form 65535) (>= form -65535))
    ;; use one of the pushi instructions
    (cond ((zerop form)
	   (comp-write-op op-pushi-0))
	  ((= form 1)
	   (comp-write-op op-pushi-1))
	  ((= form 2)
	   (comp-write-op op-pushi-2))
	  ((= form -1)
	   (comp-write-op op-pushi-minus-1))
	  ((= form -2)
	   (comp-write-op op-pushi-minus-2))
	  ((and (<= form 127) (>= form -128))
	   (comp-write-op op-pushi (logand form 255)))
	  ((and (< form 0) (>= form -65535))
	   (comp-write-op op-pushi-pair-neg (- form)))
	  (t
	   (comp-write-op op-pushi-pair-pos form))))
   (t
    (comp-write-op op-push (comp-add-constant form))))
  (comp-inc-stack))

;; Put a constant into the alist of constants, returning its index number.
;; It won't be added twice if it's already there.
(defun comp-add-constant (const)
  (or (cdr (assoc const comp-constant-alist))
      (progn
	(setq comp-constant-alist (cons (cons const comp-constant-index)
					comp-constant-alist)
	      comp-constant-index (1+ comp-constant-index))
	(1- comp-constant-index))))

;; Compile a list of forms, the last form's evaluated value is left on
;; the stack. If the list is empty nil is pushed.
(defun comp-compile-body (body &optional return-follows name)
  (if (null body)
      (progn
	(comp-write-op op-nil)
	(comp-inc-stack))
    (while (consp body)
      (if (and (null (cdr body)) (comp-constant-function-p (car body)) name)
	  ;; handle named lambdas specially so we track name of current fun
	  (progn
	    (comp-compile-constant
	     (comp-compile-lambda
	      (comp-constant-function-value (car body)) name))
	    (comp-write-op op-enclose))
	(comp-compile-form (car body) (if (cdr body) nil return-follows)))
      (when (cdr body)
	(comp-write-op op-pop)
	(comp-dec-stack))
      (setq body (cdr body)))))

;; Remove all keywords from a lambda list ARGS, returning the list of
;; variables that would be bound (in the order they would be bound)
(defun comp-get-lambda-vars (args)
  (let
      (vars)
    (while args
      (if (symbolp args)
	  (setq vars (cons args vars))
	(unless (memq (car args) '(&optional &rest &aux))
	  (setq vars (cons (car args) vars))))
      (setq args (cdr args)))
    (nreverse vars)))

;; From LST, `(lambda (ARGS) [DOC-STRING] BODY ...)' returns a byte-code
;; vector
(defun comp-compile-lambda (lst &optional name)
  (let
      ((args (nth 1 lst))
       (body (nthcdr 2 lst))
       doc interactive form)
    (when (stringp (car body))
      (setq doc (car body))
      (setq body (cdr body)))
    (when (eq (car (car body)) 'interactive)
      ;; If we have (interactive), set the interactive spec to t
      ;; so that it's not ignored
      (setq interactive (or (car (cdr (car body))) t)
	    body (cdr body))
      ;; See if it might be a good idea to compile the interactive decl
      (when (and (consp interactive)
		 (memq (car interactive) comp-top-level-compiled))
	(setq interactive (compile-form interactive))))
    (when (and comp-write-docs doc name)
      (add-documentation name doc))
    (let
	((vars (comp-get-lambda-vars args)))
      (mapc comp-test-varbind vars)
      (when (setq form (let
			   ((comp-spec-bindings comp-spec-bindings)
			    (comp-lex-bindings comp-lex-bindings)
			    (comp-lexically-pure t)
			    (comp-lambda-name name)
			    (comp-lambda-args args))
			 (comp-note-bindings vars)
			 (compile-form (cons 'progn body))))
	(make-byte-code-subr args (nth 1 form) (nth 2 form) (nth 3 form)
			     (and (not comp-write-docs) doc) interactive)))))

;; Return t if FORM is a constant
(defun comp-constant-p (form)
  (cond
   ((or (integerp form) (stringp form)
	(vectorp form) (bytecodep form)
	(eq form t) (eq form nil)))
   ((consp form)
    (eq (car form) 'quote))
   ((symbolp form)
    (or (const-variable-p form)
	(assq form comp-const-env)))
   ;; What other constant forms have I missed..?
   (t
    nil)))

;; If FORM is a constant, return its value
(defun comp-constant-value (form)
  (cond
   ((or (integerp form) (stringp form)
	(vectorp form) (bytecodep form)
	(eq form t) (eq form nil))
    ;; Self-evaluating types
    form)
   ((consp form)
    ;; only quote
    (nth 1 form))
   ((symbolp form)
    (if (const-variable-p form)
	(symbol-value form)
      (cdr (assq form comp-const-env))))))

(defun comp-constant-function-p (form)
  (setq form (macroexpand form comp-macro-env))
  (or (memq (car form) '(lambda function))))

(defun comp-constant-function-value (form)
  (setq form (macroexpand form comp-macro-env))
  (cond ((eq (car form) 'lambda)
	 form)
	((eq (car form) 'function)
	 (nth 1 form))))

(defmacro comp-spec-bound-p (var)
  (list 'or (list 'memq var 'comp-defvars) (list 'special-variable-p var)))

(defun comp-note-binding (var)
  (if (comp-spec-bound-p var)
      (progn
	;; specially bound (dynamic scope)
	(setq comp-spec-bindings (cons var comp-spec-bindings))
	(setq comp-lexically-pure nil))
    ;; assume it's lexically bound otherwise
    (setq comp-lex-bindings (cons var comp-lex-bindings)))
  (when (eq var comp-lambda-name)
    (setq comp-lambda-name nil)))

(defmacro comp-note-bindings (vars)
  (list 'mapc 'comp-note-binding vars))

(defun comp-binding-lexical-addr (var)
  (if (comp-spec-bound-p var)
      nil
    (catch 'out
      (let
	  ((i 0))
	(mapc (lambda (x)
		(when (eq x var)
		  (throw 'out i))
		(setq i (1+ i))) comp-lex-bindings)
	nil))))

(defun comp-emit-binding (var)
  (comp-write-op (if (comp-spec-bound-p var) op-bindspec op-bind)
		 (comp-add-constant var))
  (comp-note-binding var))

(defun comp-emit-varset (sym)
  (comp-test-varref sym)
  (if (comp-spec-bound-p sym)
      (comp-write-op op-setq (comp-add-constant sym))
    (let
	((lex-addr (comp-binding-lexical-addr sym)))
      (if lex-addr
	  ;; The lexical address is known. Use it to avoid scanning
	  (comp-write-op op-setn lex-addr)
	;; No lexical binding, but not special either. Just
	;; update the global value
	(comp-write-op op-setg (comp-add-constant sym))))))


;; Managing the output code

;; Output one byte
(defsubst comp-byte-out (byte)
  (setq comp-output (cons (cons byte comp-output-pc) comp-output))
  (setq comp-output-pc (1+ comp-output-pc)))

(defun comp-insn-out (insn)
  (let
      ((opcode (car insn))
       (arg (cdr insn)))
    (cond
     ((eq opcode 'label)
      ;; backpatch already output instructions referrring to this label
      (mapc (lambda (addr)
	      (setq comp-output (cons (cons (ash comp-output-pc -8) addr)
				      comp-output))
	      (setq comp-output (cons (cons (logand comp-output-pc 255)
					    (1+ addr)) comp-output)))
	    arg)
      ;; set the address of the label
      (rplacd insn comp-output-pc))
     ((and (>= opcode op-last-with-args)
	   (not (and (>= opcode op-first-with-args-2)
		     (< opcode op-last-with-args-2))))
      ;; ``normal'' one-byte insn encoding
      (comp-byte-out opcode)
      (when arg
	(when (and (eq (car arg) 'label) (numberp (cdr arg)))
	  ;; label whose address is already known
	  (setq arg (cdr arg)))
	(cond ((eq (car arg) 'label)
	       ;; label whose address isn't yet known
	       ;; add the address for backpatching
	       (rplacd arg (cons comp-output-pc (cdr arg)))
	       ;; step over waiting slot
	       (setq comp-output-pc (+ comp-output-pc 2)))
	      ((memq opcode comp-two-byte-insns)
	       (if (< arg 256)
		   (comp-byte-out arg)
		 (error "Argument overflow in two-byte insn: %d" opcode)))
	      ((memq opcode comp-three-byte-insns)
	       (if (< arg 65536)
		   (progn
		     (comp-byte-out (ash arg -8))
		     (comp-byte-out (logand arg 255)))
		 (error "Argument overflow in three-byte insn: %d" opcode)))
	      (t
	       (error "Spurious argument given to insn: %d" opcode)))))
     (t
      ;; insn with encoded argument
      (cond ((<= arg comp-max-1-byte-arg)
	     (comp-byte-out (+ opcode arg)))
	    ((<= arg comp-max-2-byte-arg)
	     (comp-byte-out (+ opcode 6))
	     (comp-byte-out arg))
	    ((<= arg comp-max-3-byte-arg)
	     (comp-byte-out (+ opcode 7))
	     (comp-byte-out (ash arg -8))
	     (comp-byte-out (logand arg 255)))
	    (t
	     (error "Argument overflow in insn: %d" opcode)))))))

(defun comp-assemble-bytecodes ()
  (mapc comp-insn-out comp-intermediate-code))
    

;; Managing the intermediate codes
  
;; Output one opcode and its optional argument
(defmacro comp-write-op (opcode &optional arg)
  `(setq comp-intermediate-code (cons (cons ,opcode ,arg)
				      comp-intermediate-code)))

;; Create a new label
(defmacro comp-make-label ()
  ;; a label is either (label . nil) or (label . (CODE-REFS...))
  ;; or (label BYTE-ADDRESS)
  `(cons 'label nil))

;; Arrange for the address of LABEL to be pushed onto the stack
(defmacro comp-push-label-addr (label)
  `(progn
     (comp-write-op op-pushi-pair-pos ,label)
     (comp-inc-stack)))

(defun comp-compile-jmp (opcode label)
  (comp-write-op opcode label))

;; Set the address of the label LABEL to the current pc
(defmacro comp-set-label (label)
  `(setq comp-intermediate-code (cons ,label comp-intermediate-code)))

;; return the label marking the start of the bytecode sequence
(defun comp-start-label ()
  (let
      ((label (last comp-intermediate-code)))
    (unless (eq (car label) 'label)
      (setq label (comp-make-label))
      (setq comp-intermediate-code (nconc comp-intermediate-code
					(list label))))
    label))


;; Constant folding

;; This assumes that FORM is a list, and its car is one of the functions
;; in the comp-constant-functions list
(defun comp-fold-constants (form)
  (catch 'exit
    (let
	((args (mapcar (lambda (arg)
			 (when (consp arg)
			   (setq arg (macroexpand arg comp-macro-env)))
			 (when (and (consp arg)
				    (memq (car arg) comp-constant-functions)
				    (not (or (memq (car arg)
						   comp-spec-bindings)
					     (memq (car arg)
						   comp-lex-bindings))))
			   (setq arg (comp-fold-constants arg)))
			 (if (comp-constant-p arg)
			     (comp-constant-value arg)
			   ;; Not a constant, abort, abort
			   (throw 'exit form)))
		       (cdr form))))
      ;; Now we have ARGS, the constant [folded] arguments from FORM
      (setq form (apply (eval (car form)) args))
      ;; If the folded version is a symbol or a list, quote it to preserve
      ;; its constant-ness
      (if (or (symbolp form) (consp form))
	  (setq form (list 'quote form))
	form))))

;; Source code transformations. These are basically macros that are only
;; used at compile-time.

(defun comp-trans-if (form)
  (let
      ((condition (nth 1 form))
       (then-form (nth 2 form))
       (else-forms (nthcdr 3 form)))
    (if (null else-forms)
	(list 'cond (list condition then-form))
      (list 'cond (list condition then-form) (cons 't else-forms)))))
(put 'if 'compile-transform comp-trans-if)

(defun comp-trans-and (form)
  (setq form (cdr form))
  (let
      (lst slot)
    (while form
      (if slot
	  (progn
	    (setcdr slot (cons (list 'cond (list (car form))) nil))
	    (setq slot (car (cdr (car (cdr slot))))))
	(setq lst (list 'cond (list (car form))))
	(setq slot (car (cdr lst))))
      (setq form (cdr form)))
    lst))
(put 'and 'compile-transform comp-trans-and)

(defun comp-trans-or (form)
  (cons 'cond (mapcar list (cdr form))))
(put 'or 'compile-transform comp-trans-or)

(defun comp-trans-setq-default (form)
  (let
      (lst)
    (setq form (cdr form))
    (while form
      (unless (consp (cdr form))
	(comp-error "Odd number of args to setq-default"))
      (setq lst (cons `(set-default ',(car form) ,(nth 1 form)) lst))
      (setq form (nthcdr 2 form)))
    (cons 'progn (nreverse lst))))
(put 'setq-default 'compile-transform comp-trans-setq-default)

(defun comp-trans-setq (form)
  (let
      (lst)
    (setq form (cdr form))
    (while form
      (unless (consp (cdr form))
	(comp-error "Odd number of args to setq"))
      (setq lst (cons `(set ',(car form) ,(nth 1 form)) lst))
      (setq form (nthcdr 2 form)))
    (cons 'progn (nreverse lst))))
(put 'setq 'compile-transform comp-trans-setq)

(defun comp-trans-defvar (form)
  (let
      ((name (nth 1 form))
       (value (nth 2 form))
       (doc (nth 3 form)))
    (comp-remember-var name)
    `(progn
       (when ,doc
	 (put ',name 'variable-documentation ,doc))
       (unless (boundp ',name)
	 (setq ,name ,value)))))
(put 'defvar 'compile-transform comp-trans-defvar)

(defun comp-trans-require (form)
  (let
      ((feature (nth 1 form)))
    (when (comp-constant-p feature)
      (require (comp-constant-value feature)))
    ;; Must transform to something other than (require FEATURE) to
    ;; prevent infinite regress
    `(funcall require ,feature)))
(put 'require 'compile-transform comp-trans-require)

(defun comp-trans-/= (form)
  `(not (= ,@(cdr form))))
(put '/= 'compile-transform comp-trans-/=)


;; Functions which compile non-standard functions (ie special-forms)

(defun comp-compile-quote (form)
  (comp-compile-constant (car (cdr form))))
(put 'quote 'compile-fun comp-compile-quote)

(defun comp-compile-function (form)
  (comp-compile-form (cadr form)))
(put 'function 'compile-fun comp-compile-function)

(defun comp-compile-lambda-form (form)
  (comp-compile-constant (comp-compile-lambda form))
  (comp-write-op op-enclose))
(put 'lambda 'compile-fun comp-compile-lambda-form)

(defun comp-compile-while (form)
  (let
      ((top-label (comp-make-label))
       (test-label (comp-make-label)))
    (comp-compile-jmp op-jmp test-label)
    (comp-set-label top-label)
    (comp-compile-body (nthcdr 2 form))
    (comp-write-op op-pop)
    (comp-dec-stack)
    (comp-set-label test-label)
    (comp-compile-form (nth 1 form))
    (comp-compile-jmp op-jpt top-label)))
(put 'while 'compile-fun comp-compile-while)

;; Compile mapc specially if we can open code the function call
(defun comp-compile-mapc (form)
  (let
      ((fun (nth 1 form))
       (lst (nth 2 form)))
    (if (comp-constant-function-p fun)
	;; We can open code the function
	(let
	    ((top-label (comp-make-label))
	     (test-label (comp-make-label)))
	  (setq fun (comp-constant-function-value fun))
	  (comp-compile-form lst)
	  (comp-compile-jmp op-jmp test-label)
	  (comp-set-label top-label)
	  (comp-write-op op-dup)
	  (comp-inc-stack)
	  (comp-write-op op-car)
	  (comp-compile-lambda-inline fun nil 1)
	  (comp-write-op op-pop)
	  (comp-dec-stack)
	  (comp-write-op op-cdr)
	  (comp-set-label test-label)
	  ;; I don't have a jump-if-t-but-never-pop instruction, so
	  ;; make one out of "jpt TOP; nil". If I ever get a peep hole
	  ;; optimiser working, the nil should be fodder for it..
	  (comp-compile-jmp op-jtp top-label)
	  (comp-write-op op-nil))
      ;; The function must be called, so just use the mapc opcode
      (comp-compile-form fun)
      (comp-compile-form lst)
      (comp-write-op op-mapc)
      (comp-dec-stack))))
(put 'mapc 'compile-fun comp-compile-mapc)
      
(defun comp-compile-progn (form &optional return-follows)
  (comp-compile-body (cdr form) return-follows))
(put 'progn 'compile-fun comp-compile-progn)

(defun comp-compile-prog1 (form)
  (comp-compile-form (nth 1 form))
  (comp-compile-body (nthcdr 2 form))
  (comp-write-op op-pop)
  (comp-dec-stack))
(put 'prog1 'compile-fun comp-compile-prog1)

(defun comp-compile-prog2 (form)
  (comp-compile-form (nth 1 form))
  (comp-write-op op-pop)
  (comp-dec-stack)
  (comp-compile-form (nth 2 form))
  (comp-compile-body (nthcdr 3 form))
  (comp-write-op op-pop)
  (comp-dec-stack))
(put 'prog2 'compile-fun comp-compile-prog2)

(defun comp-compile-set (form)
  (let
      ((fun (car form))
       (sym (nth 1 form))
       (val (nth 2 form)))
    (if (comp-constant-p sym)
	;; use setq
	(progn
	  (setq sym (comp-constant-value sym))
	  (comp-compile-form val)
	  (comp-write-op op-dup)
	  (comp-inc-stack)
	  (comp-emit-varset sym)
	  (comp-dec-stack))
      (comp-compile-form sym)
      (comp-compile-form val)
      (comp-write-op op-set)
      (comp-dec-stack))))
(put 'set 'compile-fun comp-compile-set)

(defun comp-push-args (lambda-list args &optional pushed-args-already tester)
  (let
      ((arg-count 0))
    (if (not pushed-args-already)
	;; First of all, evaluate each argument onto the stack
	(while (consp args)
	  (comp-compile-form (car args))
	  (setq args (cdr args)
		arg-count (1+ arg-count)))
      ;; Args already on stack
      (setq args nil
	    arg-count pushed-args-already))
    ;; Now the interesting bit. The args are on the stack, in
    ;; reverse order. So now we have to scan the lambda-list to
    ;; see what they should be bound to.
    (let
	((state 'required)
	 (args-left arg-count)
	 (bind-stack '()))
      (mapc tester (comp-get-lambda-vars lambda-list))
      (while lambda-list
	(cond
	 ((symbolp lambda-list)
	  (setq bind-stack (cons (cons lambda-list args-left) bind-stack))
	  (setq args-left 0))
	 ((consp lambda-list)
	  (if (memq (car lambda-list) '(&optional &rest &aux))
	      (setq state (car lambda-list))
	    (cond
	     ((eq state 'required)
	      (if (zerop args-left)
		  (comp-error "Required arg missing" (car lambda-list))
		(setq bind-stack (cons (car lambda-list) bind-stack)
		      args-left (1- args-left))))
	     ((eq state '&optional)
	      (if (zerop args-left)
		  (progn
		    (comp-write-op op-nil)
		    (comp-inc-stack))
		(setq args-left (1- args-left)))
	      (setq bind-stack (cons (car lambda-list) bind-stack)))
	     ((eq state '&rest)
	      (setq bind-stack (cons (cons (car lambda-list) args-left)
				     bind-stack)
		    args-left 0
		    state '&aux))
	     ((eq state '&aux)
	      (setq bind-stack (cons (cons (car lambda-list) nil)
				     bind-stack)))))))
	(setq lambda-list (cdr lambda-list)))
      (when (> args-left 0)
	(comp-warning "%d unused parameters to lambda expression" args-left))
      (cons args-left bind-stack))))

(defun comp-pop-args (bind-stack args-left setter)
  ;; Bind all variables
  (while bind-stack
    (if (consp (car bind-stack))
	(progn
	  (if (null (cdr (car bind-stack)))
	      (progn
		(comp-write-op op-nil)
		(comp-inc-stack))
	    (comp-write-op op-list (cdr (car bind-stack)))
	    (comp-dec-stack (cdr (car bind-stack)))
	    (comp-inc-stack))
	  (setter (car (car bind-stack))))
      (setter (car bind-stack)))
    (comp-dec-stack)
    (setq bind-stack (cdr bind-stack)))
  ;; Then pop any args that weren't used.
  (while (> args-left 0)
    (comp-write-op op-pop)
    (comp-dec-stack)
    (setq args-left (1- args-left))))

;; This compiles an inline lambda, i.e. FUN is something like
;; (lambda (LAMBDA-LIST...) BODY...)
;; If PUSHED-ARGS-ALREADY is non-nil it should be a count of the number
;; of arguments pushed onto the stack (in reverse order). In this case,
;; ARGS is ignored
(defun comp-compile-lambda-inline (fun args &optional pushed-args-already)
  (when (>= (setq comp-inline-depth (1+ comp-inline-depth))
	    comp-max-inline-depth)
    (setq comp-inline-depth 0)
    (comp-error "Won't inline more than %d nested functions"
		comp-max-inline-depth))
  (unless (eq (car fun) 'lambda)
    (comp-error "Invalid function to inline: %s, %s" fun args))
  (let*
      ((lambda-list (nth 1 fun))
       (body (nthcdr 2 fun))
       (out (comp-push-args
	     lambda-list args pushed-args-already comp-test-varbind))
       (args-left (car out))
       (bind-stack (cdr out))
       (comp-spec-bindings comp-spec-bindings)
       (comp-lex-bindings comp-lex-bindings)
       (comp-lexically-pure comp-lexically-pure)
       (comp-lambda-name comp-lambda-name))

    ;; Set up the body for compiling, skip any interactive form or
    ;; doc string
    (while (and (consp body) (or (stringp (car body))
				 (and (consp (car body))
				      (eq (car (car body)) 'interactive))))
      (setq body (cdr body)))
    
    ;; Now we have a list of things to bind to, in the same order
    ;; as the stack of evaluated arguments. The list has items
    ;; SYMBOL, (SYMBOL . ARGS-TO-BIND), or (SYMBOL . nil)
    (if bind-stack
	(progn
	  (comp-write-op op-init-bind)
	  (comp-pop-args bind-stack args-left comp-emit-binding)
	  (comp-compile-body body)
	  (comp-write-op op-unbind))
      ;; Nothing to bind to. Just pop the evaluated args and
      ;; evaluate the body
      (while (> args-left 0)
	(comp-write-op op-pop)
	(comp-dec-stack)
	(setq args-left (1- args-left)))
      (comp-compile-body body)))
  (setq comp-inline-depth (1- comp-inline-depth)))

;; The defsubst form stuffs this into the compile-fun property of
;; all defsubst declared functions
(defun comp-compile-inline-function (form)
  (comp-compile-lambda-inline
   (closure-function (symbol-value (car form))) (cdr form)))

(defun comp-do-tail-call (arg-spec args)
  (let*
      ((out (comp-push-args arg-spec args nil comp-test-varref))
       (args-left (car out))
       (bind-stack (cdr out))
       (comp-spec-bindings comp-spec-bindings)
       (comp-lex-bindings comp-lex-bindings)
       (comp-lexically-pure comp-lexically-pure)
       (comp-lambda-name comp-lambda-name))
    (comp-pop-args bind-stack args-left comp-emit-varset)
    (comp-write-op op-unbindall)
    (comp-write-op op-jmp (comp-start-label))))

(defun comp-compile-let* (form &optional return-follows)
  (let
      ((lst (car (cdr form)))
       (comp-spec-bindings comp-spec-bindings)
       (comp-lex-bindings comp-lex-bindings)
       (comp-lexically-pure comp-lexically-pure)
       (comp-lambda-name comp-lambda-name))
    (comp-write-op op-init-bind)
    (while (consp lst)
      (cond
	((consp (car lst))
	  (let
	      ((tmp (car lst)))
	    (comp-compile-body (cdr tmp))
	    (comp-test-varbind (car tmp))
	    (comp-emit-binding (car tmp))))
	(t
	  (comp-write-op op-nil)
	  (comp-inc-stack)
	  (comp-test-varbind (car lst))
	  (comp-emit-binding (car lst))))
      (comp-dec-stack)
      (setq lst (cdr lst)))
    (comp-compile-body (nthcdr 2 form) return-follows)
    (comp-write-op op-unbind)))
(put 'let* 'compile-fun comp-compile-let*)

(defun comp-compile-let (form &optional return-follows)
  (let
      ((lst (car (cdr form)))
       (sym-stk nil)
       bindings)
    (comp-write-op op-init-bind)
    (while (consp lst)
      (cond
	((consp (car lst))
	  (setq sym-stk (cons (caar lst) sym-stk))
	  (comp-compile-body (cdar lst)))
	(t
	  (setq sym-stk (cons (car lst) sym-stk))
	  (comp-write-op op-nil)
	  (comp-inc-stack)))
      (setq lst (cdr lst)))
    (mapc comp-test-varbind sym-stk)
    (let
	((comp-spec-bindings comp-spec-bindings)
	 (comp-lex-bindings comp-lex-bindings)
	 (comp-lexically-pure comp-lexically-pure)
	 (comp-lambda-name comp-lambda-name))
      (while (consp sym-stk)
	(comp-emit-binding (car sym-stk))
	(comp-dec-stack)
	(setq sym-stk (cdr sym-stk)))
      (comp-compile-body (nthcdr 2 form) return-follows))
    (comp-write-op op-unbind)))
(put 'let 'compile-fun comp-compile-let)

(defun comp-compile-letrec (form &optional return-follows)
  (let
      ((bindings (car (cdr form)))
       (comp-spec-bindings comp-spec-bindings)
       (comp-lex-bindings comp-lex-bindings)
       (comp-lexically-pure comp-lexically-pure)
       (comp-lambda-name comp-lambda-name))
    (comp-write-op op-init-bind)
    ;; create the bindings, should really be to void values, but use nil..
    (mapc (lambda (cell)
	    (let
		((var (or (car cell) cell)))
	      (comp-test-varbind var)
	      (comp-compile-constant nil)
	      (comp-emit-binding var)
	      (comp-dec-stack))) bindings)
    ;; then set them to their values
    (mapc (lambda (cell)
	    (let
		((var (or (car cell) cell)))
	      (comp-compile-body (cdr cell) nil var)
	      (comp-emit-varset var)
	      (comp-dec-stack))) bindings)
    (comp-compile-body (nthcdr 2 form) return-follows)
    (comp-write-op op-unbind)))
(put 'letrec 'compile-fun comp-compile-letrec)

(defun comp-compile-save-environment (form)
  (comp-write-op op-bindenv)
  (comp-compile-body (cdr form))
  (comp-write-op op-unbind))
(put 'save-environment 'compile-fun comp-compile-save-environment)

(defun comp-compile-defun (form)
  (comp-remember-fun (nth 1 form) (nth 2 form))
  (comp-compile-constant (nth 1 form))
  (comp-write-op op-dup)
  (comp-inc-stack)
  (comp-compile-constant (comp-compile-lambda (cons 'lambda (nthcdr 2 form))
					      (nth 1 form)))
  (comp-write-op op-enclose)
  (comp-write-op op-dset)
  (comp-write-op op-pop)
  (comp-dec-stack 2))
(put 'defun 'compile-fun comp-compile-defun)

(defun comp-compile-defmacro (form)
  (comp-remember-fun (nth 1 form) (nth 2 form))
  (comp-compile-constant (nth 1 form))
  (comp-write-op op-dup)
  (comp-inc-stack)
  (comp-compile-constant (cons 'macro (comp-compile-lambda
				       (cons 'lambda (nthcdr 2 form))
				       (nth 1 form))))
  (comp-write-op op-enclose)
  (comp-write-op op-dset)
  (comp-write-op op-pop)
  (comp-dec-stack 2))
(put 'defmacro 'compile-fun comp-compile-defmacro)

(defun comp-compile-cond (form &optional return-follows)
  (let
      ((end-label (comp-make-label))
       (need-trailing-nil t))
    (setq form (cdr form))
    (while (consp form)
      (let*
	  ((subl (car form))
	   (condition (car subl))
	   (next-label (comp-make-label)))
	;; See if we can squash a constant condition to t or nil
	(when (comp-constant-p condition)
	  (setq condition (not (not (comp-constant-value condition)))))
	(cond
	 ((eq condition t)
	  ;; condition t -- always taken
	  (if (consp (cdr subl))
	      ;; There's something besides the condition
	      (progn
		(comp-compile-body (cdr subl) return-follows)
		(comp-dec-stack))
	    (if (eq condition (car subl))
		(comp-write-op op-t)
	      (comp-compile-form (car subl) return-follows)
	      (comp-dec-stack)))
	  (when (consp (cdr form))
	    (comp-warning "Unreachable conditions after t in cond statement")
	    ;; Ignore the rest of the statement
	    (setq form nil))
	  (setq need-trailing-nil nil))
	 ((eq condition nil)
	  ;; condition nil -- never taken
	  (when (cdr subl)
	    (comp-warning "Unreachable forms after nil in cond statement")))
	 (t
	  ;; non t-or-nil condition
	  (comp-compile-form (car subl))
	  (comp-dec-stack)
	  (if (consp (cdr subl))
	      ;; Something besides the condition
	      (if (cdr form)
		  ;; This isn't the last condition list
		  (progn
		    (comp-compile-jmp op-jn next-label)
		    (comp-compile-body (cdr subl) return-follows)
		    (comp-dec-stack)
		    (comp-compile-jmp op-jmp end-label)
		    (comp-set-label next-label))
		;; It is the last condition list, use the result
		;; of this condition for the return value when it's
		;; nil
		(comp-compile-jmp op-jnp end-label)
		(comp-compile-body (cdr subl) return-follows)
		(comp-dec-stack)
		(setq need-trailing-nil nil))
	    ;; No action to take
	    (if (cdr form)
		;; This isn't the last condition list
		(comp-compile-jmp op-jtp end-label)
	      ;; This is the last condition list, since there's no
	      ;; action to take, just fall out the bottom, with the
	      ;; condition as value.
	      (setq need-trailing-nil nil))))))
      (setq form (cdr form)))
    (when need-trailing-nil
      (comp-write-op op-nil))
    (comp-inc-stack)
    (comp-set-label end-label)))
(put 'cond 'compile-fun comp-compile-cond)

(defun comp-compile-case (form &optional return-follows)
  (let
      ((end-label (comp-make-label)))
    (setq form (cdr form))
    (unless form
      (comp-error "No key value in case statement"))
    ;; XXX if key is constant optimise case away..
    (comp-compile-form (car form))
    (setq form (cdr form))
    (while (consp form)
      (unless (consp form)
	(comp-error "Badly formed clause in case statement"))
      (let
	  ((cases (caar form))
	   (forms (cdar form))
	   (next-label (comp-make-label)))
	(cond ((consp cases)
	       (comp-write-op op-dup)
	       (comp-inc-stack)
	       (if (consp (cdr cases))
		   ;; >1 possible case
		   (progn
		     (comp-compile-constant cases)
		     (comp-write-op op-memq))
		 ;; only one case, use eq
		 (comp-compile-constant (car cases))
		 (comp-write-op op-eq))
	       (comp-dec-stack)
	       (comp-compile-jmp op-jn next-label)
	       (comp-dec-stack))
	      ((not (eq cases t))
	       (comp-error "Badly formed clause in case statement")))
	(comp-compile-body forms return-follows)
	(comp-dec-stack)
	(comp-compile-jmp op-jmp end-label)
	(comp-set-label next-label)
	(setq form (cdr form))))
    (comp-inc-stack)
    (comp-set-label end-label)
    (comp-write-op op-swap)
    (comp-write-op op-pop)))
(put 'case 'compile-fun comp-compile-case)

(defun comp-compile-catch (form)
  (let
      ((catch-label (comp-make-label))
       (start-label (comp-make-label))
       (end-label (comp-make-label))
       (comp-lexically-pure nil))
    ;;		jmp start
    (comp-compile-jmp op-jmp start-label)

    ;; catch:
    ;;		catch TAG
    ;;		ejmp end
    (comp-inc-stack)			;enter with one arg on stack
    (comp-set-label catch-label)
    (comp-compile-form (nth 1 form))
    (comp-write-op op-catch)
    (comp-dec-stack)
    (comp-compile-jmp op-ejmp end-label)
    (comp-dec-stack)

    ;; start:
    ;;		push #catch
    ;;		binderr
    ;;		FORMS...
    ;;		unbind
    ;; end:
    (comp-set-label start-label)
    (comp-push-label-addr catch-label)
    (comp-write-op op-binderr)
    (comp-dec-stack)
    (comp-compile-body (nthcdr 2 form))
    (comp-write-op op-unbind)
    (comp-set-label end-label)))
(put 'catch 'compile-fun comp-compile-catch)

(defun comp-compile-unwind-pro (form)
  (let
      ((cleanup-label (comp-make-label))
       (start-label (comp-make-label))
       (end-label (comp-make-label))
       (comp-lexically-pure nil))

    ;;		jmp start
    (comp-compile-jmp op-jmp start-label)

    ;; cleanup:
    ;;		CLEANUP-FORMS
    ;;		pop
    ;;		ejmp end
    ;; [overall, stack +1]
    (comp-inc-stack 2)
    (comp-set-label cleanup-label)
    (comp-compile-body (nthcdr 2 form))
    (comp-write-op op-pop)
    (comp-compile-jmp op-ejmp end-label)
    (comp-dec-stack 2)

    ;; start:
    ;;		push #cleanup
    ;;		binderr
    ;;		FORM
    ;;		unbind
    ;;		nil
    ;;		jmp cleanup
    ;; [overall, stack +2]
    (comp-set-label start-label)
    (comp-push-label-addr cleanup-label)
    (comp-write-op op-binderr)
    (comp-dec-stack)
    (comp-compile-form (nth 1 form))
    (comp-write-op op-unbind)
    (comp-write-op op-nil)
    (comp-dec-stack)
    (comp-compile-jmp op-jmp cleanup-label)

    ;; end:
    (comp-set-label end-label)))
(put 'unwind-protect 'compile-fun comp-compile-unwind-pro)

(defun comp-compile-condition-case (form)
  (let
      ((cleanup-label (comp-make-label))
       (start-label (comp-make-label))
       (end-label (comp-make-label))
       (handlers (nthcdr 3 form))
       (comp-lexically-pure nil))

    ;;		jmp start
    ;; cleanup:
    (comp-compile-jmp op-jmp start-label)
    (comp-set-label cleanup-label)

    (comp-inc-stack 2)			;reach here with two items on stack
    (if (consp handlers)
	(let
	    ((comp-spec-bindings comp-spec-bindings)
	     (comp-lex-bindings comp-lex-bindings)
	     (comp-lambda-name comp-lambda-name))
	  (when (nth 1 form)
	    (comp-test-varbind (nth 1 form))
	    (comp-note-binding (nth 1 form)))
	  ;; Loop over all but the last handler
	  (while (consp (cdr handlers))
	    (if (consp (car handlers))
		(let
		    ((next-label (comp-make-label)))
		  ;;		push CONDITIONS
		  ;;		errorpro
		  ;;		jtp next
		  ;;		HANDLER
		  ;;		jmp end
		  ;; next:
		  (comp-compile-constant (car (car handlers)))
		  (comp-write-op op-errorpro)
		  (comp-dec-stack)
		  (comp-compile-jmp op-jtp next-label)
		  (comp-dec-stack)
		  (comp-compile-body (cdr (car handlers)))
		  (comp-compile-jmp op-jmp end-label)
		  (comp-set-label next-label))
	      (comp-error "Badly formed condition-case handler"))
	    (setq handlers (cdr handlers)))
	  ;; The last handler
	  (if (consp (car handlers))
	      (let
		  ((pc-label (comp-make-label)))
		;;		push CONDITIONS
		;;		errorpro
		;;		ejmp pc
		;; pc:		HANDLER
		;;		jmp end
		(comp-compile-constant (car (car handlers)))
		(comp-write-op op-errorpro)
		(comp-dec-stack)
		(comp-compile-jmp op-ejmp pc-label)
		(comp-set-label pc-label)
		(comp-dec-stack)
		(comp-compile-body (cdr (car handlers)))
		(comp-compile-jmp op-jmp end-label))
	    (comp-error "Badly formed condition-case handler")))
      (comp-error "No handlers in condition-case"))
    (comp-dec-stack)
    (comp-dec-stack)

    ;; start:
    ;;		push VAR
    ;;		push cleanup
    ;;		binderr
    ;;		FORM
    (comp-set-label start-label)
    (comp-compile-constant (nth 1 form))
    (comp-push-label-addr cleanup-label)
    (comp-write-op op-binderr)
    (comp-dec-stack)
    (comp-compile-form (nth 2 form))

    ;; end:
    ;;		unbind			;unbind error handler or VAR
    ;;		swap			;result<->VAR
    ;;		pop			;pop VAR
    (comp-set-label end-label)
    (comp-write-op op-unbind)
    (comp-write-op op-swap)
    (comp-write-op op-pop)
    (comp-dec-stack)))
(put 'condition-case 'compile-fun comp-compile-condition-case)

(defun comp-compile-with-object (form)
  (let
      ((comp-lexically-pure nil))
    (comp-compile-form (nth 1 form))
    (comp-write-op op-bindobj)
    (comp-dec-stack)
    (comp-compile-body (nthcdr 2 form))
    (comp-write-op op-unbind)))
(put 'with-object 'compile-fun comp-compile-with-object)

(defun comp-compile-list (form)
  (let
      ((count 0))
    (setq form (cdr form))
    (while (consp form)
      (comp-compile-form (car form))
      (setq
       count (1+ count)
       form (cdr form)))
    (comp-write-op op-list count)
    (comp-dec-stack (1- count))))
(put 'list 'compile-fun comp-compile-list)

;; Funcall normally translates to a single call instruction. However,
;; if the function being called is a constant lambda expression, open
;; code it.
(defun comp-compile-funcall (form)
  (let*
      ((fun (nth 1 form))
       (args (nthcdr 2 form))
       (arg-count 0)
       (open-code (comp-constant-function-p fun)))
    (unless open-code
      (comp-compile-form fun))
    (while args
      (comp-compile-form (car args))
      (setq args (cdr args)
	    arg-count (1+ arg-count)))
    (if open-code
	(progn
	  (comp-compile-lambda-inline
	   (comp-constant-function-value fun) nil arg-count)
	  ;; We push one less value than when using op-call
	  (if (zerop arg-count)
	      (comp-inc-stack)
	    (comp-dec-stack (1- arg-count))))
      (comp-write-op op-call arg-count)
      (comp-dec-stack arg-count))))
(put 'funcall 'compile-fun comp-compile-funcall)

(defun comp-compile-nth (form)
  (let
      ((insn (cdr (assq (nth 1 form) comp-nth-insns))))
    (if insn
	(progn
	  (comp-compile-form (nth 2 form))
	  (comp-write-op insn))
      (comp-compile-2-args form))))
(put 'nth 'compile-fun comp-compile-nth)
(put 'nth 'compile-opcode op-nth)

(defun comp-compile-nthcdr (form)
  (let
      ((insn (assq (nth 1 form) comp-nthcdr-insns)))
    (if insn
	(progn
	  (comp-compile-form (nth 2 form))
	  (when (cdr insn)
	    (comp-write-op (cdr insn))))
      (comp-compile-2-args form))))
(put 'nthcdr 'compile-fun comp-compile-nthcdr)
(put 'nthcdr 'compile-opcode op-nthcdr)

(defun comp-compile-minus (form)
  (if (/= (length form) 2)
      (comp-compile-binary-op form)
    (comp-compile-form (car (cdr form)))
    (comp-write-op op-neg)))
(put '- 'compile-fun comp-compile-minus)
(put '- 'compile-opcode op-sub)

;; Instruction with no arguments
(defun comp-compile-0-args (form)
  (when (cdr form)
    (comp-warning "All parameters to %s ignored" (car form)))
  (comp-write-op (get (car form) 'compile-opcode))
  (comp-inc-stack))

;; Instruction taking 1 arg on the stack
(defun comp-compile-1-args (form)
  (when (nthcdr 2 form)
    (comp-warning "More than one parameter to %s; rest ignored" (car form)))
  (comp-compile-form (nth 1 form))
  (comp-write-op (get (car form) 'compile-opcode)))

;; Instruction taking 2 args on the stack
(defun comp-compile-2-args (form)
  (when (nthcdr 3 form)
    (comp-warning "More than two parameters to %s; rest ignored" (car form)))
  (comp-compile-form (nth 1 form))
  (comp-compile-form (nth 2 form))
  (comp-write-op (get (car form) 'compile-opcode))
  (comp-dec-stack))

;; Instruction taking 3 args on the stack
(defun comp-compile-3-args (form)
  (when (nthcdr 4 form)
    (comp-warning "More than three parameters to %s; rest ignored" (car form)))
  (comp-compile-form (nth 1 form))
  (comp-compile-form (nth 2 form))
  (comp-compile-form (nth 3 form))
  (comp-write-op (get (car form) 'compile-opcode))
  (comp-dec-stack 2))

;; Compile a form `(OP ARG1 ARG2 ARG3 ...)' into as many two argument
;; instructions as needed (PUSH ARG1; PUSH ARG2; OP; PUSH ARG3; OP; ...)
(defun comp-compile-binary-op (form)
  (let
      ((opcode (get (car form) 'compile-opcode)))
    (setq form (cdr form))
    (unless (>= (length form) 2)
      (comp-error "Too few args to binary operator" form))
    (comp-compile-form (car form))
    (setq form (cdr form))
    (while (consp form)
      (comp-compile-form (car form))
      (comp-write-op opcode)
      (comp-dec-stack)
      (setq form (cdr form)))))

;; Used for >, >=, < and <=
(defun comp-compile-transitive-relation (form)
  (cond
   ((<= (length form) 2)
    (comp-error "Too few args to relation" form))
   ((= (length form) 3)
    (let
	((opcode (get (car form) 'compile-opcode)))
      ;; Simple case, only two arguments, i.e. `(OP ARG1 ARG2)' into:
      ;;  PUSH ARG1; PUSH ARG2; OP;
      (comp-compile-form (nth 1 form))
      (comp-compile-form (nth 2 form))
      (comp-write-op opcode)
      (comp-dec-stack)))
     (t
      ;; Tricky case, >2 args,

      ;; Originally I did `(OP ARG1 ARG2 ARG3... ARGN)' as:

      ;;  PUSH ARG1; PUSH ARG2; DUP; SWAP2; OP; JNP Fail;
      ;;  PUSH ARG3; DUP; SWAP2; OP; JNP Fail;
      ;;  ...
      ;;  PUSH ARGN; OP; JMP End;
      ;; Fail:
      ;;  SWAP; POP;
      ;; End:

      ;; But that doesn't always evaluate all arguments..
      (comp-compile-funcall (cons 'funcall form)))))


;; Opcode properties for the generic instructions, in a progn for compiled
;; speed

(progn
  (put 'cons 'compile-fun comp-compile-2-args)
  (put 'cons 'compile-opcode op-cons)
  (put 'car 'compile-fun comp-compile-1-args)
  (put 'car 'compile-opcode op-car)
  (put 'cdr 'compile-fun comp-compile-1-args)
  (put 'cdr 'compile-opcode op-cdr)
  (put 'rplaca 'compile-fun comp-compile-2-args)
  (put 'rplaca 'compile-opcode op-rplaca)
  (put 'rplacd 'compile-fun comp-compile-2-args)
  (put 'rplacd 'compile-opcode op-rplacd)
  (put 'aset 'compile-fun comp-compile-3-args)
  (put 'aset 'compile-opcode op-aset)
  (put 'aref 'compile-fun comp-compile-2-args)
  (put 'aref 'compile-opcode op-aref)
  (put 'length 'compile-fun comp-compile-1-args)
  (put 'length 'compile-opcode op-length)
  (put 'eval 'compile-fun comp-compile-1-args)
  (put 'eval 'compile-opcode op-eval)
  (put '+ 'compile-fun comp-compile-binary-op)
  (put '+ 'compile-opcode op-add)
  (put '* 'compile-fun comp-compile-binary-op)
  (put '* 'compile-opcode op-mul)
  (put '/ 'compile-fun comp-compile-binary-op)
  (put '/ 'compile-opcode op-div)
  (put 'remainder 'compile-fun comp-compile-2-args)
  (put 'remainder 'compile-opcode op-rem)
  (put 'mod 'compile-fun comp-compile-2-args)
  (put 'mod 'compile-opcode op-mod)
  (put 'lognot 'compile-fun comp-compile-1-args)
  (put 'lognot 'compile-opcode op-lnot)
  (put 'not 'compile-fun comp-compile-1-args)
  (put 'not 'compile-opcode op-not)
  (put 'logior 'compile-fun comp-compile-binary-op)
  (put 'logior 'compile-opcode op-lor)
  (put 'logxor 'compile-fun comp-compile-binary-op)
  (put 'logxor 'compile-opcode op-lxor)
  (put 'logand 'compile-fun comp-compile-binary-op)
  (put 'logand 'compile-opcode op-land)
  (put 'ash 'compile-fun comp-compile-2-args)
  (put 'ash 'compile-opcode op-ash)
  (put 'equal 'compile-fun comp-compile-2-args)
  (put 'equal 'compile-opcode op-equal)
  (put 'eq 'compile-fun comp-compile-2-args)
  (put 'eq 'compile-opcode op-eq)
  (put '= 'compile-fun comp-compile-transitive-relation)
  (put '= 'compile-opcode op-equal)
  (put '> 'compile-fun comp-compile-transitive-relation)
  (put '> 'compile-opcode op-gt)
  (put '< 'compile-fun comp-compile-transitive-relation)
  (put '< 'compile-opcode op-lt)
  (put '>= 'compile-fun comp-compile-transitive-relation)
  (put '>= 'compile-opcode op-ge)
  (put '<= 'compile-fun comp-compile-transitive-relation)
  (put '<= 'compile-opcode op-le)
  (put '1+ 'compile-fun comp-compile-1-args)
  (put '1+ 'compile-opcode op-inc)
  (put '1- 'compile-fun comp-compile-1-args)
  (put '1- 'compile-opcode op-dec)
  (put 'zerop 'compile-fun comp-compile-1-args)
  (put 'zerop 'compile-opcode op-zerop)
  (put 'null 'compile-fun comp-compile-1-args)
  (put 'null 'compile-opcode op-not)
  (put 'atom 'compile-fun comp-compile-1-args)
  (put 'atom 'compile-opcode op-atom)
  (put 'consp 'compile-fun comp-compile-1-args)
  (put 'consp 'compile-opcode op-consp)
  (put 'listp 'compile-fun comp-compile-1-args)
  (put 'listp 'compile-opcode op-listp)
  (put 'numberp 'compile-fun comp-compile-1-args)
  (put 'numberp 'compile-opcode op-numberp)
  (put 'stringp 'compile-fun comp-compile-1-args)
  (put 'stringp 'compile-opcode op-stringp)
  (put 'vectorp 'compile-fun comp-compile-1-args)
  (put 'vectorp 'compile-opcode op-vectorp)
  (put 'throw 'compile-fun comp-compile-2-args)
  (put 'throw 'compile-opcode op-throw)
  (put 'boundp 'compile-fun comp-compile-1-args)
  (put 'boundp 'compile-opcode op-boundp)
  (put 'symbolp 'compile-fun comp-compile-1-args)
  (put 'symbolp 'compile-opcode op-symbolp)
  (put 'get 'compile-fun comp-compile-2-args)
  (put 'get 'compile-opcode op-get)
  (put 'put 'compile-fun comp-compile-3-args)
  (put 'put 'compile-opcode op-put)
  (put 'signal 'compile-fun comp-compile-2-args)
  (put 'signal 'compile-opcode op-signal)
  (put 'quotient 'compile-fun comp-compile-2-args)
  (put 'quotient 'compile-opcode op-quotient)
  (put 'reverse 'compile-fun comp-compile-1-args) ; new 12/7/94
  (put 'reverse 'compile-opcode op-reverse)
  (put 'nreverse 'compile-fun comp-compile-1-args)
  (put 'nreverse 'compile-opcode op-nreverse)
  (put 'assoc 'compile-fun comp-compile-2-args)
  (put 'assoc 'compile-opcode op-assoc)
  (put 'assq 'compile-fun comp-compile-2-args)
  (put 'assq 'compile-opcode op-assq)
  (put 'rassoc 'compile-fun comp-compile-2-args)
  (put 'rassoc 'compile-opcode op-rassoc)
  (put 'rassq 'compile-fun comp-compile-2-args)
  (put 'rassq 'compile-opcode op-rassq)
  (put 'last 'compile-fun comp-compile-1-args)
  (put 'last 'compile-opcode op-last)
  (put 'mapcar 'compile-fun comp-compile-2-args)
  (put 'mapcar 'compile-opcode op-mapcar)
  (put 'member 'compile-fun comp-compile-2-args)
  (put 'member 'compile-opcode op-member)
  (put 'memq 'compile-fun comp-compile-2-args)
  (put 'memq 'compile-opcode op-memq)
  (put 'delete 'compile-fun comp-compile-2-args)
  (put 'delete 'compile-opcode op-delete)
  (put 'delq 'compile-fun comp-compile-2-args)
  (put 'delq 'compile-opcode op-delq)
  (put 'delete-if 'compile-fun comp-compile-2-args)
  (put 'delete-if 'compile-opcode op-delete-if)
  (put 'delete-if-not 'compile-fun comp-compile-2-args)
  (put 'delete-if-not 'compile-opcode op-delete-if-not)
  (put 'copy-sequence 'compile-fun comp-compile-1-args)
  (put 'copy-sequence 'compile-opcode op-copy-sequence)
  (put 'sequencep 'compile-fun comp-compile-1-args)
  (put 'sequencep 'compile-opcode op-sequencep)
  (put 'functionp 'compile-fun comp-compile-1-args)
  (put 'functionp 'compile-opcode op-functionp)
  (put 'special-form-p 'compile-fun comp-compile-1-args)
  (put 'special-form-p 'compile-opcode op-special-form-p)
  (put 'subrp 'compile-fun comp-compile-1-args)
  (put 'subrp 'compile-opcode op-subrp)
  (put 'eql 'compile-fun comp-compile-2-args)
  (put 'eql 'compile-opcode op-eql)
  (put 'max 'compile-fun comp-compile-binary-op)
  (put 'max 'compile-opcode op-max)
  (put 'min 'compile-fun comp-compile-binary-op)
  (put 'min 'compile-opcode op-min)
  (put 'filter 'compile-fun comp-compile-2-args)
  (put 'filter 'compile-opcode op-filter)
  (put 'macrop 'compile-fun comp-compile-1-args)
  (put 'macrop 'compile-opcode op-macrop)
  (put 'bytecodep 'compile-fun comp-compile-1-args)
  (put 'bytecodep 'compile-opcode op-bytecodep)
  (put 'make-closure 'compile-fun comp-compile-2-args)
  (put 'make-closure 'compile-opcode op-make-closure)
  (put 'closurep 'compile-fun comp-compile-1-args)
  (put 'closurep 'compile-opcode op-closurep)
  (put 'thread-forbid 'compile-fun comp-compile-0-args)
  (put 'thread-forbid 'compile-opcode op-forbid)
  (put 'thread-permit 'compile-fun comp-compile-0-args)
  (put 'thread-permit 'compile-opcode op-permit)

  (put 'caar 'compile-fun comp-compile-1-args)
  (put 'caar 'compile-opcode op-caar)
  (put 'cadr 'compile-fun comp-compile-1-args)
  (put 'cadr 'compile-opcode op-cadr)
  (put 'cdar 'compile-fun comp-compile-1-args)
  (put 'cdar 'compile-opcode op-cdar)
  (put 'cddr 'compile-fun comp-compile-1-args)
  (put 'cddr 'compile-opcode op-cddr)
  (put 'caddr 'compile-fun comp-compile-1-args)
  (put 'caddr 'compile-opcode op-caddr)

  (put 'floor 'compile-fun comp-compile-1-args)
  (put 'floor 'compile-opcode op-floor)
  (put 'ceiling 'compile-fun comp-compile-1-args)
  (put 'ceiling 'compile-opcode op-ceiling)
  (put 'truncate 'compile-fun comp-compile-1-args)
  (put 'truncate 'compile-opcode op-truncate)
  (put 'round 'compile-fun comp-compile-1-args)
  (put 'round 'compile-opcode op-round)
  (put 'exp 'compile-fun comp-compile-1-args)
  (put 'exp 'compile-opcode op-exp)
  (put 'log 'compile-fun comp-compile-1-args)
  (put 'log 'compile-opcode op-log)
  (put 'sin 'compile-fun comp-compile-1-args)
  (put 'sin 'compile-opcode op-sin)
  (put 'cos 'compile-fun comp-compile-1-args)
  (put 'cos 'compile-opcode op-cos)
  (put 'tan 'compile-fun comp-compile-1-args)
  (put 'tan 'compile-opcode op-tan)
  (put 'sqrt 'compile-fun comp-compile-1-args)
  (put 'sqrt 'compile-opcode op-sqrt)
  (put 'expt 'compile-fun comp-compile-2-args)
  (put 'expt 'compile-opcode op-expt)

  ;; some pseudonyms
  (put 'setcar 'compile-fun comp-compile-2-args)
  (put 'setcar 'compile-opcode op-rplaca)
  (put 'setcdr 'compile-fun comp-compile-2-args)
  (put 'setcdr 'compile-opcode op-rplacd)
  (put 'string= 'compile-fun comp-compile-2-args)
  (put 'string= 'compile-opcode op-equal)
  (put 'string< 'compile-fun comp-compile-transitive-relation)
  (put 'string< 'compile-opcode op-lt)
  (put '% 'compile-fun comp-compile-2-args)
  (put '% 'compile-opcode op-rem)
  (put 'modulo 'compile-fun comp-compile-2-args)
  (put 'modulo 'compile-opcode op-mod)
  (put 'lsh 'compile-fun comp-compile-2-args)
  (put 'lsh 'compile-opcode op-ash))
