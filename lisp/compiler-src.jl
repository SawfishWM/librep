#| compiler-src.jl -- source code program transforms

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

(define-structure compiler-src (export source-code-transform)

  (open rep
	compiler-utils
	compiler-modules
	compiler-lap
	compiler-vars
	bytecodes)

;;; Constant folding

  (defun foldablep (name)
    (unless (or (memq name comp-spec-bindings)
		(assq name comp-lex-bindings))
      (let
	  ((fun (get-procedure-handler name 'compiler-foldablep)))
	(and fun (fun name)))))
	
  ;; This assumes that FORM is a list, and its car is one of the functions
  ;; in the comp-constant-functions list
  (defun fold-constants (form)
    (catch 'exit
      (let
	  ((args (mapcar (lambda (arg)
			   (when (consp arg)
			     (setq arg (compiler-macroexpand arg)))
			   (when (and (consp arg) (foldablep (car arg)))
			     (setq arg (fold-constants arg)))
			   (if (compiler-constant-p arg)
			       (compiler-constant-value arg)
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

;;; Entry point

  (defun source-code-transform (form)
    (let (tem)
      ;; first try constant folding
      (when (and (consp form) (foldablep (car form)))
	(setq form (fold-constants form)))

      ;; then look for a specific tranformer
      (when (and (symbolp (car form))
		 (setq tem (get-procedure-handler
			    (car form) 'compiler-transform-property)))
	(setq form (tem form)))

      form)))
