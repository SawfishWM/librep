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

(defvar dumped-lisp-libraries nil
  "When a dumped binary is being executed, a list of the names of all Lisp
libraries that were dumped.")

;; A list of the dumped libraries whose .jld files have been autoloaded
(defvar dumped-loaded-libraries nil)


;; Function decls

(defmacro defsubst (&rest decl)
  "Defines a function that will be compiled inline to any functions that
call it. Otherwise exactly the same as defun."
  ;; These actions are also hard-coded into dump.jl
  (list 'prog1
	(cons 'defun decl)
	(list 'put (list 'quote (car decl))
	      ''compile-fun ''comp-compile-inline-function)))


;; Convenient conditional macros, all defined using cond

(defmacro if (condition then-form &rest else-forms)
  "Eval CONDITION, if it is non-nil then eval THEN-FORM and return it's 
result, else do an implicit progn with the ELSE-FORMS returning its value."
  (cond
   ((null else-forms)
    (list 'cond (list condition then-form)))
   (t
    (list 'cond (list condition then-form) (cons 't else-forms)))))

(defmacro when (condition &rest forms)
  "Evaluates CONDITION, if it is non-nil an implicit progn is performed
with FORMS."
  (list 'cond (cons condition forms)))

(defmacro unless (condition &rest forms)
  "Evaluates CONDITION, if it is nil an implicit progn is performed with
FORMS."
  (list 'cond (cons condition nil) (cons 't forms)))

(defmacro or (&rest forms)
  "Evaluates each member of FORMS in turn until one returns t, the result of
which is returned. If none are t then return nil."
  (cons 'cond (mapcar #'list forms)))

(defmacro and (&rest forms &aux slot list)
  "Evaluates each member of FORMS in turn, until one returns nil, and returns
nil. If none return nil then the value of the last form evaluated is
returned."
  (while forms
    (if slot
	(progn
	  (setcdr slot (cons (list 'cond (list (car forms))) nil))
	  (setq slot (car (cdr (car (cdr slot))))))
      (setq list (list 'cond (list (car forms)))
	    slot (car (cdr list))))
    (setq forms (cdr forms)))
  list)


;; Feature definition

(defvar features ()
  "A list of symbols defining which ``features'' Jade currently has loaded.
This is used by the `featurep', `provide' and `require' functions.")

(defun require (feature &optional file)
  "If FEATURE (a symbol) has not already been loaded, load it. The file
loaded is either FILE (if given), or the print name of FEATURE."
  (interactive "SFeature to load:")
  (if (member feature features)
      t
    (load (unless file (symbol-name feature)))))

(defun provide (feature)
  "Show that the feature FEATURE (a symbol) has been loaded."
  (unless (member feature features)
      (setq features (cons feature features))))

(defun featurep (feature)
  "Return non-nil if feature FEATURE has already been loaded."
  (member feature features))


;; Function to allow easy creation of autoload stubs

(when (boundp 'dumped-lisp-libraries)
  (setq dumped-loaded-libraries nil))

(defun autoload (symbol file &rest extra)
  "Tell the evaluator that the function value of SYMBOL will be initialised
from a named file. The AUTOLOAD-DEFN is the contents of the SYMBOL's
autoload definition. Currently two items are used, the first is the name
of the file to load the value of SYMBOL from. The second says whether or
not the function SYMBOL may be called interactively (as a command)."
  (if (and (boundp 'dumped-lisp-libraries)
	   (member file dumped-lisp-libraries))
      ;; If FILE has been dumped, but not yet loaded, load it
      (unless (member file dumped-loaded-libraries)
	(load file)
	(setq dumped-loaded-libraries (cons file dumped-loaded-libraries)))
    ;; Else just add the autoload defn as normal
    (fset symbol (list* 'autoload file extra))))

(defun autoload-variable (symbol file)
  "Tell the evaluator that the value of SYMBOL can be initialised by loading
the lisp library called FILE.

Note that at present, autoloading of variables is only performed by the
`lookup-event-binding' function, to allow autoloaded keymaps."
  (if (and (boundp 'dumped-lisp-libraries)
	   (member file dumped-lisp-libraries))
      ;; If FILE has been dumped, but not yet loaded, load it
      (unless (member file dumped-loaded-libraries)
	(load file)
	(setq dumped-loaded-libraries (cons file dumped-loaded-libraries)))
    ;; Else just add the autoload defn as normal
    (set symbol (list 'autoload file))))


;; Hook manipulation

(defun add-hook (hook-symbol new-func &optional at-end)
  "Arrange it so that FUNCTION-NAME is added to the hook-list stored in
symbol, HOOK-SYMBOL. It will added at the head of the list unless AT-END
is non-nil in which case it is added at the end."
  (unless (boundp hook-symbol)
    (set hook-symbol nil))
  (if at-end
      (set hook-symbol (nconc (symbol-value hook-symbol) (cons new-func nil)))
    (set hook-symbol (cons new-func (symbol-value hook-symbol)))))

(defun remove-hook (hook-symbol old-func)
  "Remove FUNCTION-NAME from the hook HOOK-SYMBOL."
  (set hook-symbol (delete old-func (symbol-value hook-symbol))))

(defun eval-after-load (library form &aux tem)
  "Arrange for FORM to be evaluated immediately after the library of Lisp code
LIBRARY has been read by the `load' function. Note that LIBRARY must exactly
match the FILE argument to `load'."
  (if (setq tem (assoc library after-load-alist))
      (rplacd tem (cons form (cdr tem)))
    (setq after-load-alist (cons (cons library (list form)) after-load-alist))))


;; Miscellanea

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

;; Some function pseudonyms
(defmacro setcar (&rest args)
  (cons 'rplaca args))

(defmacro setcdr (&rest args)
  (cons 'rplacd args))

(defmacro string= (&rest args)
  (cons 'equal args))
(fset 'string-equal-p (symbol-function 'string=))

(defmacro string< (&rest args)
  (cons '< args))
(fset 'string-less-p (symbol-function 'string<))

(defun error (&rest args)
  (signal 'error (list (apply 'format nil args))))

(defun eval-and-print (form)
  "Eval FORM then print its value in the status line."
  (interactive "xEval: ")
  (prin1 (eval form) t))

(defun nop ()
  "A do-nothing command."
  (interactive))

(defmacro return (&optional arg)
  "Return ARG from the outermost function."
  (list 'throw ''defun arg))

(defmacro function (arg)
  "Normally the same as `quote'. When being compiled, if ARG is not a symbol
it causes ARG to be compiled as a lambda expression. This macro is also
available as the reader shortcut #', i.e. #'foo == (function foo)."
  (list 'quote arg))

(defun file-name= (name1 name2)
  "Returns t when NAME1 and NAME2 both name the same file."
  (string= (canonical-file-name name1) (canonical-file-name name2)))


;; Macros for handling positions

(defmacro pos-col (p)
  "Return the column pointed to by position P."
  (list 'cdr p))

(defmacro pos-line (p)
  "Return the row pointed to by position P."
  (list 'car p))
