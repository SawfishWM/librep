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


;; Options
(defvar comp-write-docs nil
  "When t all doc-strings are appended to the doc file and replaced with
their position in that file.")


;; Environment of this byte code sequence being compiled

(defvar comp-constant-alist '())	;list of (VALUE . INDEX)
(defvar comp-constant-index 0)		;next free constant index number
(defvar comp-current-stack 0)		;current stack requirement
(defvar comp-max-stack 0)		;highest possible stack
(defvar comp-output nil)		;list of (BYTE . INDEX)
(defvar comp-output-pc 0)		;INDEX of next byte
(defvar comp-macro-env '())		;alist of (NAME . MACRO-DEF)
(defvar comp-const-env '())		;alist of (NAME . CONST-DEF)
(defvar comp-current-file nil)		;the file being compiled
(defvar comp-current-fun nil)		;the function being compiled

(defvar comp-buffer (make-buffer "*compilation-output*"))
(set-buffer-special comp-buffer t)


(defvar comp-top-level-compiled
  '(if cond when unless let let* catch unwind-protect condition-case
    with-buffer with-window with-view progn prog1 prog2 while and or)
  "List of symbols, when the name of the function called by a top-level form
is one of these that form is compiled.")

;;;###autoload
(defun compile-file (file-name)
  "Compiles the file of jade-lisp code FILE-NAME into a new file called
`(concat FILE-NAME ?c)' (ie, `foo.jl' => `foo.jlc')."
  (interactive "fLisp file to compile:")
  (let
      ((comp-current-file file-name)
       src-file dst-file form
       comp-macro-env
       comp-const-env
       form)
    (message (concat "Compiling " file-name "...") t)
    (when (setq src-file (open file-name "r"))
      (unwind-protect
	  ;; Pass 1. Scan for macro definitions in FILE-NAME
	  (while (not (file-eof-p src-file))
	    (setq form (read src-file))
	    (when (eq (car form) 'defmacro)
	      (setq comp-macro-env (cons (cons (nth 1 form)
					       (cons 'lambda (nthcdr 2 form)))
					 comp-macro-env))))
	(close src-file))
      (when (and (setq src-file (open file-name "r"))
		 (setq dst-file (open (concat file-name ?c) "w")))
	(condition-case error-info
	    (unwind-protect
		(progn
		  ;; Pass 2. The actual compile
		  (format dst-file
			  ";;; Source file: %s
;;; Compiled by %s@%s on %s
;;; Jade %d.%d

(validate-byte-code %d %d %d %d)\n\n"
			  file-name (user-login-name) (system-name)
			  (current-time-string) (major-version-number)
			  (minor-version-number)
			  bytecode-major bytecode-minor
			  (major-version-number) (minor-version-number))
		  (while (not (file-eof-p src-file))
		    (when (setq form (read src-file))
		      (cond
		       ((memq (car form) '(defun defmacro defvar
					    defconst require))
			(setq form (comp-compile-top-form form)))
		       ((memq (car form) comp-top-level-compiled)
			;; Compile this form
			(setq form (compile-form form))))
		      (when form
			(print form dst-file)))))
	      (close dst-file)
	      (close src-file))
	  (error
	   ;; Be sure to remove any partially written dst-file. Also, signal
	   ;; the error again so that the user sees it.
	   (let
	       ((fname (concat file-name ?c)))
	     (when (file-exists-p fname)
	       (delete-file fname)))
	   ;; Hack to signal error without entering the debugger (again)
	   (throw 'error error-info)))
	t))))

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
	    ((file (file-name-concat dir-name (car dir)))
	     (cfile (concat file ?c)))
	  (when (file-newer-than-file-p file cfile)
	    (compile-file file))))
      (setq dir (cdr dir)))
    t))

(defvar compile-lib-exclude-list
  '("autoload.jl"))

;;;###autoload
(defun compile-lisp-lib (&optional force-p)
  "Recompile all out of date files in the lisp library directory. If FORCE-P
is non-nil it's as though all files were out of date.
This makes sure that all doc strings are written to their special file and
that files which shouldn't be compiled aren't."
  (interactive "P")
  (let
      ((comp-write-docs t))
    (compile-directory lisp-lib-dir force-p compile-lib-exclude-list)))

;; Used when bootstrapping from the Makefile
(defun compile-compiler ()
  (let
      ((comp-write-docs t))
    (compile-file (file-name-concat lisp-lib-dir "compiler.jl"))))

;;;###autoload
(defun compile-function (function)
  "Compiles the body of the function FUNCTION."
  (interactive "aFunction to compile:")
  (let
      ((fbody (symbol-function function))
       (comp-current-fun function))
    (when (assq 'jade-byte-code fbody)
      (comp-error "Function already compiled" function))
    (fset function (comp-compile-lambda fbody)))
  function)
    

(put 'compile-error 'error-message "Compilation mishap")
(defun comp-error (&rest data)
  (signal 'compile-error data))

(defun comp-warning (string)
  (unless (memq comp-buffer buffer-list)
    (add-buffer comp-buffer))
  (goto-buffer comp-buffer)
  (insert "Warning: ")
  (when comp-current-file
    (format comp-buffer "%s:" comp-current-file))
  (when comp-current-fun
    (format comp-buffer "%s:" comp-current-fun))
  (format comp-buffer " %s\n" string))

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
	(list 'fset (list 'quote (nth 1 form))
	      (comp-compile-lambda (cons 'lambda (nthcdr 2 form))))))
     ((eq fun 'defmacro)
      (let
	  ((code (comp-compile-lambda (cons 'lambda (nthcdr 2 form)) t))
	   (tmp (assq (nth 1 form) comp-macro-env))
	   (comp-current-fun (nth 1 form)))
	(if tmp
	    (rplacd tmp code)
	  (comp-error "Compiled macro wasn't in environment" (nth 1 form)))
	(list 'fset (list 'quote (nth 1 form)) code)))
     ((eq fun 'defconst)
      (let
	  ((value (eval (nth 2 form)))
	   (doc (nth 3 form)))
	(when (and comp-write-docs (stringp doc))
	  (rplaca (nthcdr 3 form) (add-doc-string doc)))
	(setq comp-const-env (cons (cons (nth 1 form) value) comp-const-env)))
      form)
     ((eq fun 'defvar)
      (let
	  ((doc (nth 3 form)))
	(when (and comp-write-docs (stringp doc))
	  (rplaca (nthcdr 3 form) (add-doc-string doc))))
      form)
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
       (comp-output-pc 0))
    (comp-compile-form form)
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
(defun comp-compile-form (form)
  (cond
    ((eq form nil)
      (comp-write-op op-nil)
      (comp-inc-stack))
    ((eq form t)
      (comp-write-op op-t)
      (comp-inc-stack))
    ((symbolp form)
     (let
	 (val)
       (cond
	((const-variable-p form)
	 ;; A constant already interned
	 (comp-write-op op-push (comp-add-constant (symbol-value form)))
	 (comp-inc-stack))
	((setq val (assq form comp-const-env))
	 ;; A constant from this file
	 (comp-compile-form (cdr val)))
	(t
	 ;; Not a constant
	 (comp-write-op op-refq (comp-add-constant form))
	 (comp-inc-stack)))))
    ((consp form)
      (let
	  (fun)
	(if (and (symbolp (car form)) (setq fun (get (car form) 'compile-fun)))
	    (funcall fun form)
	  (setq form (macroexpand form comp-macro-env))
	  (if (and (symbolp (car form))
		   (setq fun (get (car form) 'compile-fun)))
	      (funcall fun form)
	    (setq fun (car form))
	    (cond
	     ((symbolp fun)
	      (comp-compile-constant fun))
	     ((and (consp fun) (eq (car fun) 'lambda))
	      (comp-compile-constant (comp-compile-lambda fun)))
	     (t
	      (comp-error "Bad function name" fun)))
	    (setq form (cdr form))
	    (let
		((i 0))
	      (while (consp form)
		(comp-compile-form (car form))
		(setq i (1+ i)
		      form (cdr form)))
	      (comp-write-op op-call i)
	      (comp-dec-stack i))))))
    (t
      (comp-compile-constant form))))

;; Push a constant onto the stack
(defun comp-compile-constant (form)
  (cond
   ((eq form nil)
    (comp-write-op op-nil))
   ((eq form t)
    (comp-write-op op-t))
   (t
    (comp-write-op op-push (comp-add-constant form))))
  (comp-inc-stack))

;; Put a constant into the alist of constants, returning its index number.
;; It won't be added twice if it's already there.
(defun comp-add-constant (const)
  (unless (cdr (assoc const comp-constant-alist))
    (setq comp-constant-alist (cons (cons const comp-constant-index)
				    comp-constant-alist)
	  comp-constant-index (1+ comp-constant-index))
    (1- comp-constant-index)))

;; Compile a list of forms, the last form's evaluated value is left on
;; the stack. If the list is empty nil is pushed.
(defun comp-compile-body (body)
  (if (null body)
      (progn
	(comp-write-op op-nil)
	(comp-inc-stack))
    (while (consp body)
      (comp-compile-form (car body))
      (when (cdr body)
	(comp-write-op op-pop)
	(comp-dec-stack))
      (setq body (cdr body)))))

;; From LIST, `(lambda (ARGS) [DOC-STRING] BODY ...)' returns a byte-code
;; vector
(defun comp-compile-lambda (list &optional macrop)
  (let
      ((args (nth 1 list))
       (body (nthcdr 2 list))
       doc interactive form)
    (when (stringp (car body))
      (setq doc (if comp-write-docs
		    (add-doc-string (nth 2 list))
		  (nth 2 list))
	    body (cdr body)))
    (when (eq (car (car body)) 'interactive)
      ;; If we have (interactive), set the interactive spec to t
      ;; so that it's not ignored
      (setq interactive (or (car (cdr (car body))) t)
	    body (cdr body)))
    (when (setq form (compile-form (cons 'progn body)))
      (make-byte-code-subr args (nth 1 form) (nth 2 form) (nth 3 form)
			   doc interactive macrop))))


;; Managing the output code

;; Return a new label
(defmacro comp-make-label ()
  ;; a label is, (PC-OF-LABEL . (LIST-OF-REFERENCES))
  '(cons nil nil))

;; Output a branch instruction to the label LABEL, if LABEL has not been
;; located yet this branch is recorded for later backpatching.
(defun comp-compile-jmp (opcode label)
  (comp-byte-out opcode)
  (cond
    ((numberp (car label))
      ;; we know the final offset of this label so use it
      (comp-byte-out (lsh (car label) -8))
      (comp-byte-out (logand (car label) 0xff)))
    (t
      ;; offset unknown, show we need it patched in later
      (rplacd label (cons comp-output-pc (cdr label)))
      (setq comp-output-pc (+ comp-output-pc 2)))))

;; Set the address of the label LABEL, any references to it are patched
;; with its address.
(defun comp-set-label (label)
  (when (> comp-output-pc comp-max-3-byte-arg)
    (comp-error "Jump destination overflow!"))
  (rplaca label comp-output-pc)
  (setq label (cdr label))
  (while (consp label)
    (setq comp-output (cons (cons (lsh comp-output-pc -8) (car label))
			    (cons (cons (logand comp-output-pc 0xff)
					(1+ (car label)))
				  comp-output))
	  label (cdr label))))

(defmacro comp-get-label-addr (label)
  (list 'car label))

;; Output one opcode and its optional argument
(defun comp-write-op (opcode &optional arg)
  (cond
   ((null arg)
    (comp-byte-out opcode))
   ((<= arg comp-max-1-byte-arg)
    (comp-byte-out (+ opcode arg)))
   ((<= arg comp-max-2-byte-arg)
    ;; 2-byte instruction
    (comp-byte-out (+ opcode 6))
    (comp-byte-out arg))
   ((<= arg comp-max-3-byte-arg)
    ;; 3-byte instruction
    (comp-byte-out (+ opcode 7))
    (comp-byte-out (lsh arg -8))
    (comp-byte-out (logand arg 0xff)))
   (t
    (comp-error "Opcode overflow!"))))

;; Output one byte
(defun comp-byte-out (byte)
  (setq comp-output (cons (cons byte comp-output-pc) comp-output)
	comp-output-pc (1+ comp-output-pc)))


;; Functions which compile non-standard functions (ie special-forms)

(put 'quote 'compile-fun 'comp-compile-quote)
(defun comp-compile-quote (form)
  (comp-compile-constant (car (cdr form))))

(put 'function 'compile-fun 'comp-compile-function)
(defun comp-compile-function (form)
  (setq form (car (cdr form)))
  (if (symbolp form)
      (comp-compile-constant form)
    (comp-compile-constant (comp-compile-lambda form))))

(put 'while 'compile-fun 'comp-compile-while)
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

(put 'progn 'compile-fun 'comp-compile-progn)
(defun comp-compile-progn (form)
  (comp-compile-body (cdr form)))

(put 'prog1 'compile-fun 'comp-compile-prog1)
(defun comp-compile-prog1 (form)
  (comp-compile-form (nth 1 form))
  (comp-compile-body (nthcdr 2 form))
  (comp-write-op op-pop)
  (comp-dec-stack))

(put 'prog2 'compile-fun 'comp-compile-prog2)
(defun comp-compile-prog2 (form)
  (comp-compile-form (nth 1 form))
  (comp-write-op op-pop)
  (comp-dec-stack)
  (comp-compile-form (nth 2 form))
  (comp-compile-body (nthcdr 3 form))
  (comp-write-op op-pop)
  (comp-dec-stack))

(put 'setq 'compile-fun 'comp-compile-setq)
(defun comp-compile-setq (form)
  (setq form (cdr form))
  (while (and (consp form) (consp (cdr form)))
    (comp-compile-form (car (cdr form)))
    (comp-write-op op-setq (comp-add-constant (car form)))
    (when (consp (nthcdr 2 form))
      (comp-write-op op-pop)
      (comp-dec-stack))
    (setq form (nthcdr 2 form))))

(put 'set 'compile-fun 'comp-compile-set)
(defun comp-compile-set (form)
  (comp-compile-form (nth 2 form))
  (comp-compile-form (nth 1 form))
  (comp-write-op op-set)
  (comp-dec-stack))

(put 'fset 'compile-fun 'comp-compile-fset)
(defun comp-compile-fset (form)
  (comp-compile-form (nth 2 form))
  (comp-write-op op-dup)
  (comp-inc-stack)
  (comp-compile-form (nth 1 form))
  (comp-write-op op-fset)
  (comp-dec-stack 2))

(put 'let* 'compile-fun 'comp-compile-let*)
(defun comp-compile-let* (form)
  (let
      ((list (car (cdr form))))
    (comp-write-op op-init-bind)
    (while (consp list)
      (cond
	((consp (car list))
	  (let
	      ((tmp (car list)))
	    (comp-compile-body (cdr tmp))
	    (comp-write-op op-bind (comp-add-constant (car tmp)))))
	(t
	  (comp-write-op op-nil)
	  (comp-inc-stack)
	  (comp-write-op op-bind (comp-add-constant (car list)))))
      (comp-dec-stack)
      (setq list (cdr list)))
    (comp-compile-body (nthcdr 2 form))
    (comp-write-op op-unbind)))

(put 'let 'compile-fun 'comp-compile-let)
(defun comp-compile-let (form)
  (let
      ((list (car (cdr form)))
       (sym-stk nil))
    (comp-write-op op-init-bind)
    (while (consp list)
      (cond
	((consp (car list))
	  (setq sym-stk (cons (car (car list)) sym-stk))
	  (comp-compile-body (cdr (car list))))
	(t
	  (setq sym-stk (cons (car list) sym-stk))
	  (comp-write-op op-nil)
	  (comp-inc-stack)))
      (setq list (cdr list)))
    (while (consp sym-stk)
      (comp-write-op op-bind (comp-add-constant (car sym-stk)))
      (comp-dec-stack)
      (setq sym-stk (cdr sym-stk)))
    (comp-compile-body (nthcdr 2 form))
    (comp-write-op op-unbind)))

(put 'defun 'compile-fun 'comp-compile-defun)
(defun comp-compile-defun (form)
  (comp-compile-constant (nth 1 form))
  (comp-write-op op-dup)
  (comp-inc-stack)
  (comp-compile-constant (comp-compile-lambda (cons 'lambda (nthcdr 2 form))))
  (comp-write-op op-swap)
  (comp-write-op op-fset)
  (comp-dec-stack 2))

(put 'defmacro 'compile-fun 'comp-compile-defmacro)
(defun comp-compile-defmacro (form)
  (comp-compile-constant (nth 1 form))
  (comp-write-op op-dup)
  (comp-inc-stack)
  (comp-compile-constant (comp-compile-lambda
			  (cons 'lambda (nthcdr 2 form)) t))
  (comp-write-op op-swap)
  (comp-write-op op-fset)
  (comp-dec-stack 2))

(put 'cond 'compile-fun 'comp-compile-cond)
(defun comp-compile-cond (form)
  (let
      ((end-label (comp-make-label))
       (need-trailing-nil t))
    (setq form (cdr form))
    (while (consp form)
      (let
	  ((subl (car form))
	   (next-label (comp-make-label)))
	(if (eq (car subl) t)
	    ;; condition t -- always taken
	    (progn
	      ;; There's something besides the condition
	      (if (consp (cdr subl))
		  (progn
		    (comp-compile-body (cdr subl))
		    (comp-dec-stack))
		(comp-write-op op-t))
	      (when (consp (cdr form))
		(comp-warning "Unreachable code after t in cond statement"))
	      (setq need-trailing-nil nil))
	  ;; non-t condition
	  (comp-compile-form (car subl))
	  (comp-dec-stack)
	  (if (consp (cdr subl))
	      ;; Something besides the condition
	      (if (cdr form)
		  ;; This isn't the last condition list
		  (progn
		    (comp-compile-jmp op-jn next-label)
		    (comp-compile-body (cdr subl))
		    (comp-dec-stack)
		    (comp-compile-jmp op-jmp end-label)
		    (comp-set-label next-label))
		;; It is the last condition list, use the result
		;; of this condition for the return value when it's
		;; nil
		(comp-compile-jmp op-jnp end-label)
		(comp-compile-body (cdr subl))
		(comp-dec-stack)
		(setq need-trailing-nil nil))
	    ;; No action to take
	    (if (cdr form)
		;; This isn't the last condition list
		(comp-compile-jmp op-jtp end-label)
	      ;; This is the last condition list, since there's no
	      ;; action to take, just fall out the bottom, with the
	      ;; condition as value.
	      (setq need-trailing-nil nil)))))
      (setq form (cdr form)))
    (when need-trailing-nil
      (comp-write-op op-nil))
    (comp-inc-stack)
    (comp-set-label end-label)))

(put 'catch 'compile-fun 'comp-compile-catch)
(defun comp-compile-catch (form)
  (let
      ((catch-label (comp-make-label))
       (start-label (comp-make-label))
       (end-label (comp-make-label)))
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
    (comp-compile-constant (comp-get-label-addr catch-label))
    (comp-write-op op-binderr)
    (comp-dec-stack)
    (comp-compile-body (nthcdr 2 form))
    (comp-write-op op-unbind)
    (comp-set-label end-label)))

(put 'unwind-protect 'compile-fun 'comp-compile-unwind-pro)
(defun comp-compile-unwind-pro (form)
  (let
      ((cleanup-label (comp-make-label))
       (start-label (comp-make-label))
       (end-label (comp-make-label)))

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
    (comp-compile-constant (comp-get-label-addr cleanup-label))
    (comp-write-op op-binderr)
    (comp-dec-stack)
    (comp-compile-form (nth 1 form))
    (comp-write-op op-unbind)
    (comp-write-op op-nil)
    (comp-dec-stack)
    (comp-compile-jmp op-jmp cleanup-label)

    ;; end:
    (comp-set-label end-label)))

(put 'condition-case 'compile-fun 'comp-compile-condition-case)
(defun comp-compile-condition-case (form)
  (let
      ((cleanup-label (comp-make-label))
       (start-label (comp-make-label))
       (end-label (comp-make-label))
       (handlers (nthcdr 3 form)))

    ;;		jmp start
    ;; cleanup:
    (comp-compile-jmp op-jmp start-label)
    (comp-set-label cleanup-label)

    (comp-inc-stack 2)			;reach here with two items on stack
    (if (consp handlers)
	(progn
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
    (comp-compile-constant (comp-get-label-addr cleanup-label))
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

(put 'list 'compile-fun 'comp-compile-list)
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

;; Handles (with-X X FORMS...)
(defun comp-compile-with-form (form)
  (let
      ((swap-opcode (get (car form) 'compile-opcode)))
    (comp-compile-form (nth 1 form))
    (comp-write-op swap-opcode)
    (comp-dec-stack)
    (comp-compile-body (nthcdr 2 form))
    (comp-write-op op-unbind)))

(put 'with-buffer 'compile-fun 'comp-compile-with-form)
(put 'with-buffer 'compile-opcode op-bind-buffer)

(put 'with-window 'compile-fun 'comp-compile-with-form)
(put 'with-window 'compile-opcode op-bind-window)

(put 'with-view 'compile-fun 'comp-compile-with-form)
(put 'with-view 'compile-opcode op-bind-view)

(put '- 'compile-fun 'comp-compile-minus)
(put '- 'compile-opcode op-sub)
(defun comp-compile-minus (form)
  (if (/= (length form) 2)
      (comp-compile-binary-op form)
    (comp-compile-form (car (cdr form)))
    (comp-write-op op-neg)))

;; Instruction with no arguments
(defun comp-compile-0-args (form)
  (when (cdr form)
    (comp-warning (format nil "Parameters to %s discarded" (car form))))
  (comp-write-op (get (car form) 'compile-opcode) 0)
  (comp-inc-stack))

;; Instruction taking 1 arg on the stack
(defun comp-compile-1-args (form)
  (when (nthcdr 2 form)
    (comp-warning (format nil "More than one parameter to %s; rest ignored"
			  (car form))))
  (comp-compile-form (nth 1 form))
  (comp-write-op (get (car form) 'compile-opcode) 0))

;; Instruction taking 2 args on the stack
(defun comp-compile-2-args (form)
  (when (nthcdr 3 form)
    (comp-warning (format nil "More than two parameters to %s; rest ignored"
			  (car form))))
  (comp-compile-form (nth 1 form))
  (comp-compile-form (nth 2 form))
  (comp-write-op (get (car form) 'compile-opcode) 0)
  (comp-dec-stack))

;; Instruction taking 3 args on the stack
(defun comp-compile-3-args (form)
  (when (nthcdr 4 form)
    (comp-warning (format nil "More than three parameters to %s; rest ignored"
			  (car form))))
  (comp-compile-form (nth 1 form))
  (comp-compile-form (nth 2 form))
  (comp-compile-form (nth 3 form))
  (comp-write-op (get (car form) 'compile-opcode) 0)
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
  (let
      ((opcode (get (car form) 'compile-opcode)))
    (setq form (cdr form))
    (cond
     ((<= (length form) 1)
      (comp-error "Too few args to relation" form))
     ((= (length form) 2)
      ;; Simple case, only two arguments, i.e. `(OP ARG1 ARG2)' into:
      ;;  PUSH ARG1; PUSH ARG2; OP;
      (comp-compile-form (car form))
      (comp-compile-form (nth 1 form))
      (comp-write-op opcode)
      (comp-dec-stack))
     (t
      ;; Tricky case, >2 args,
      ;; Eg. `(OP ARG1 ARG2 ARG3... ARGN)' into something like,
      ;;  PUSH ARG1; PUSH ARG2; DUP; SWAP2; OP; JNP Fail;
      ;;  PUSH ARG3; DUP; SWAP2; OP; JNP Fail;
      ;;  ...
      ;;  PUSH ARGN; OP; JMP End;
      ;; Fail:
      ;;  SWAP; POP;
      ;; End:
      (let
	  ((fail-label (comp-make-label))
	   (end-label (comp-make-label)))
	(comp-compile-form (car form))
	(setq form (cdr form))
	(while (>= (length form) 2)
	  (comp-compile-form (car form))
	  (comp-write-op op-dup)
	  (comp-inc-stack)
	  (comp-write-op op-swap2)
	  (comp-write-op opcode)
	  (comp-dec-stack)
	  (comp-compile-jmp op-jnp fail-label)
	  (comp-dec-stack)
	  (setq form (cdr form)))
	;; Last arg coming up.
	(comp-compile-form (car form))
	(comp-write-op opcode)
	(comp-dec-stack)
	(comp-compile-jmp op-jmp end-label)
	(comp-set-label fail-label)
	(comp-write-op op-swap)
	(comp-write-op op-pop)
	(comp-dec-stack)
	(comp-set-label end-label))))))


;; Opcode properties for the generic instructions, in a progn for compiled
;; speed

(progn
  (put 'cons 'compile-fun 'comp-compile-2-args)
  (put 'cons 'compile-opcode op-cons)
  (put 'car 'compile-fun 'comp-compile-1-args)
  (put 'car 'compile-opcode op-car)
  (put 'cdr 'compile-fun 'comp-compile-1-args)
  (put 'cdr 'compile-opcode op-cdr)
  (put 'rplaca 'compile-fun 'comp-compile-2-args)
  (put 'rplaca 'compile-opcode op-rplaca)
  (put 'rplacd 'compile-fun 'comp-compile-2-args)
  (put 'rplacd 'compile-opcode op-rplacd)
  (put 'nth 'compile-fun 'comp-compile-2-args)
  (put 'nth 'compile-opcode op-nth)
  (put 'nthcdr 'compile-fun 'comp-compile-2-args)
  (put 'nthcdr 'compile-opcode op-nthcdr)
  (put 'aset 'compile-fun 'comp-compile-3-args)
  (put 'aset 'compile-opcode op-aset)
  (put 'aref 'compile-fun 'comp-compile-2-args)
  (put 'aref 'compile-opcode op-aref)
  (put 'length 'compile-fun 'comp-compile-1-args)
  (put 'length 'compile-opcode op-length)
  (put 'eval 'compile-fun 'comp-compile-1-args)
  (put 'eval 'compile-opcode op-eval)
  (put '+ 'compile-fun 'comp-compile-binary-op)
  (put '+ 'compile-opcode op-add)
  (put '* 'compile-fun 'comp-compile-binary-op)
  (put '* 'compile-opcode op-mul)
  (put '/ 'compile-fun 'comp-compile-binary-op)
  (put '/ 'compile-opcode op-div)
  (put '% 'compile-fun 'comp-compile-2-args)
  (put '% 'compile-opcode op-rem)
  (put 'mod 'compile-fun 'comp-compile-2-args)
  (put 'mod 'compile-opcode op-mod)
  (put 'lognot 'compile-fun 'comp-compile-1-args)
  (put 'lognot 'compile-opcode op-lnot)
  (put 'not 'compile-fun 'comp-compile-1-args)
  (put 'not 'compile-opcode op-not)
  (put 'logior 'compile-fun 'comp-compile-binary-op)
  (put 'logior 'compile-opcode op-lor)
  (put 'logxor 'compile-fun 'comp-compile-binary-op)
  (put 'logxor 'compile-opcode op-lxor)
  (put 'logand 'compile-fun 'comp-compile-binary-op)
  (put 'logand 'compile-opcode op-land)
  (put 'equal 'compile-fun 'comp-compile-2-args)
  (put 'equal 'compile-opcode op-equal)
  (put 'eq 'compile-fun 'comp-compile-2-args)
  (put 'eq 'compile-opcode op-eq)
  (put '= 'compile-fun 'comp-compile-2-args)
  (put '= 'compile-opcode op-num-eq)
  (put '/= 'compile-fun 'comp-compile-2-args)
  (put '/= 'compile-opcode op-num-noteq)
  (put '> 'compile-fun 'comp-compile-transitive-relation)
  (put '> 'compile-opcode op-gt)
  (put '< 'compile-fun 'comp-compile-transitive-relation)
  (put '< 'compile-opcode op-lt)
  (put '>= 'compile-fun 'comp-compile-transitive-relation)
  (put '>= 'compile-opcode op-ge)
  (put '<= 'compile-fun 'comp-compile-transitive-relation)
  (put '<= 'compile-opcode op-le)
  (put '1+ 'compile-fun 'comp-compile-1-args)
  (put '1+ 'compile-opcode op-inc)
  (put '1- 'compile-fun 'comp-compile-1-args)
  (put '1- 'compile-opcode op-dec)
  (put 'lsh 'compile-fun 'comp-compile-2-args)
  (put 'lsh 'compile-opcode op-lsh)
  (put 'zerop 'compile-fun 'comp-compile-1-args)
  (put 'zerop 'compile-opcode op-zerop)
  (put 'null 'compile-fun 'comp-compile-1-args)
  (put 'null 'compile-opcode op-null)
  (put 'atom 'compile-fun 'comp-compile-1-args)
  (put 'atom 'compile-opcode op-atom)
  (put 'consp 'compile-fun 'comp-compile-1-args)
  (put 'consp 'compile-opcode op-consp)
  (put 'listp 'compile-fun 'comp-compile-1-args)
  (put 'listp 'compile-opcode op-listp)
  (put 'numberp 'compile-fun 'comp-compile-1-args)
  (put 'numberp 'compile-opcode op-numberp)
  (put 'stringp 'compile-fun 'comp-compile-1-args)
  (put 'stringp 'compile-opcode op-stringp)
  (put 'vectorp 'compile-fun 'comp-compile-1-args)
  (put 'vectorp 'compile-opcode op-vectorp)
  (put 'throw 'compile-fun 'comp-compile-2-args)
  (put 'throw 'compile-opcode op-throw)
  (put 'fboundp 'compile-fun 'comp-compile-1-args)
  (put 'fboundp 'compile-opcode op-fboundp)
  (put 'boundp 'compile-fun 'comp-compile-1-args)
  (put 'boundp 'compile-opcode op-boundp)
  (put 'symbolp 'compile-fun 'comp-compile-1-args)
  (put 'symbolp 'compile-opcode op-symbolp)
  (put 'get 'compile-fun 'comp-compile-2-args)
  (put 'get 'compile-opcode op-get)
  (put 'put 'compile-fun 'comp-compile-3-args)
  (put 'put 'compile-opcode op-put)
  (put 'signal 'compile-fun 'comp-compile-2-args)
  (put 'signal 'compile-opcode op-signal)
  (put 'return 'compile-fun 'comp-compile-1-args)
  (put 'return 'compile-opcode op-return)
  (put 'reverse 'compile-fun 'comp-compile-1-args) ; new 12/7/94
  (put 'reverse 'compile-opcode op-reverse)
  (put 'nreverse 'compile-fun 'comp-compile-1-args)
  (put 'nreverse 'compile-opcode op-nreverse)
  (put 'assoc 'compile-fun 'comp-compile-2-args)
  (put 'assoc 'compile-opcode op-assoc)
  (put 'assq 'compile-fun 'comp-compile-2-args)
  (put 'assq 'compile-opcode op-assq)
  (put 'rassoc 'compile-fun 'comp-compile-2-args)
  (put 'rassoc 'compile-opcode op-rassoc)
  (put 'rassq 'compile-fun 'comp-compile-2-args)
  (put 'rassq 'compile-opcode op-rassq)
  (put 'last 'compile-fun 'comp-compile-2-args)
  (put 'last 'compile-opcode op-last)
  (put 'mapcar 'compile-fun 'comp-compile-2-args)
  (put 'mapcar 'compile-opcode op-mapcar)
  (put 'mapc 'compile-fun 'comp-compile-2-args)
  (put 'mapc 'compile-opcode op-mapc)
  (put 'member 'compile-fun 'comp-compile-2-args)
  (put 'member 'compile-opcode op-member)
  (put 'memq 'compile-fun 'comp-compile-2-args)
  (put 'memq 'compile-opcode op-memq)
  (put 'delete 'compile-fun 'comp-compile-2-args)
  (put 'delete 'compile-opcode op-delete)
  (put 'delq 'compile-fun 'comp-compile-2-args)
  (put 'delq 'compile-opcode op-delq)
  (put 'delete-if 'compile-fun 'comp-compile-2-args)
  (put 'delete-if 'compile-opcode op-delete-if)
  (put 'delete-if-not 'compile-fun 'comp-compile-2-args)
  (put 'delete-if-not 'compile-opcode op-delete-if-not)
  (put 'copy-sequence 'compile-fun 'comp-compile-1-args)
  (put 'copy-sequence 'compile-opcode op-copy-sequence)
  (put 'sequencep 'compile-fun 'comp-compile-1-args)
  (put 'sequencep 'compile-opcode op-sequencep)
  (put 'functionp 'compile-fun 'comp-compile-1-args)
  (put 'functionp 'compile-opcode op-functionp)
  (put 'special-form-p 'compile-fun 'comp-compile-1-args)
  (put 'special-form-p 'compile-opcode op-special-form-p)
  (put 'subrp 'compile-fun 'comp-compile-1-args)
  (put 'subrp 'compile-opcode op-subrp)
  (put 'eql 'compile-fun 'comp-compile-2-args)
  (put 'eql 'compile-opcode op-eql)
  (put 'max 'compile-fun 'comp-compile-binary-op)
  (put 'max 'compile-opcode op-max)
  (put 'min 'compile-fun 'comp-compile-binary-op)
  (put 'min 'compile-opcode op-min)

  (put 'set-current-buffer 'compile-fun 'comp-compile-2-args)
  (put 'set-current-buffer 'compile-opcode op-set-current-buffer)
  (put 'current-buffer 'compile-fun 'comp-compile-1-args)
  (put 'current-buffer 'compile-opcode op-current-buffer)
  (put 'bufferp 'compile-fun 'comp-compile-1-args)
  (put 'bufferp 'compile-opcode op-bufferp)
  (put 'markp 'compile-fun 'comp-compile-1-args)
  (put 'markp 'compile-opcode op-markp)
  (put 'windowp 'compile-fun 'comp-compile-1-args)
  (put 'windowp 'compile-opcode op-windowp)
  (put 'viewp 'compile-fun 'comp-compile-1-args)
  (put 'viewp 'compile-opcode op-viewp)
  (put 'current-view 'compile-fun 'comp-compile-1-args)
  (put 'current-view 'compile-opcode op-current-view)
  (put 'pos 'compile-fun 'comp-compile-2-args)
  (put 'pos 'compile-opcode op-pos)
  (put 'posp 'compile-fun 'comp-compile-1-args)
  (put 'posp 'compile-opcode op-posp))
