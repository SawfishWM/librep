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


;; Mutually recursive binding

(defmacro letrec (bindings &rest body)
  (let
      ((vars (mapcar (lambda (x)
		       (if (consp x) (car x) x)) bindings))
       (setters (mapcar (lambda (x)
			  (if (consp x)
			      (cons 'setq x)
			    (list 'setq x nil))) bindings)))
    `(let ,vars ,@setters ,@body)))
    

;; Convenient conditional macros, defined using cond

(defmacro when (condition &rest forms)
  "Evaluates CONDITION, if it is non-nil an implicit progn is performed
with FORMS."
  (list 'cond (cons condition forms)))

(defmacro unless (condition &rest forms)
  "Evaluates CONDITION, if it is nil an implicit progn is performed with
FORMS."
  (list 'if (list 'not condition) (cons 'progn forms)))


;; Features

(defvar features nil
  "List of features currently loaded by the interpreter.")

(defun featurep (feature)
  "Return non-nil if FEATURE (a symbol) has been loaded."
  (memq feature features))

(defun provide (feature)
  "Mark that FEATURE (a symbol) has been loaded."
  (setq features (cons feature features)))

(defun require (feature)
  "If FEATURE (a symbol) hasn't been loaded yet, load it."
  (unless (featurep feature)
    (load (symbol-name feature)))
  feature)


;; Function to allow easy creation of autoload stubs

(defmacro autoload (symbol file &rest extra)
  "Tell the evaluator that the value of SYMBOL will be initialised
by loading FILE."
  `(if (not (boundp ,symbol))
     (set ,symbol (make-closure (list 'autoload ,symbol ,file ,@extra)))))


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
  (mapc (lambda (dir)
	  (unless (or (string= dir "") (string= "."))
	    (let
		((full-name (expand-file-name file dir)))
	      (when (or (file-exists-p full-name)
			(file-exists-p (concat full-name ".jl"))
			(file-exists-p (concat full-name ".jlc")))
		(load full-name nil t)))))
	load-path))

(defmacro eval-when-compile (form)
  "FORM is evaluated at compile-time *only*. The evaluated value is inserted
into the compiled program. When interpreted, nil is returned."
  nil)

(defmacro prin1-to-string (arg)
  "Return a string representing ARG."
  (list 'format nil "%S" arg))

(defmacro read-from-string (string &optional start)
  "Reads an object from STRING, starting at character number START (default
is 0)."
  (list 'read (list 'make-string-input-stream string start)))

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
(defmacro setcar (&rest args)
  (cons 'rplaca args))

(defmacro setcdr (&rest args)
  (cons 'rplacd args))

(defmacro string= (&rest args)
  (cons 'equal args))

(defmacro string< (&rest args)
  (cons '< args))

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


;; null i18n function until gettext is loaded

(unless (boundp '_)
  (defun _ (arg) arg))


;; Setup format-hooks-alist to a few default'ish things

(setq format-hooks-alist '((?D . file-name-directory)
			   (?F . file-name-nondirectory)))
