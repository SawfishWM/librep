#| compiler-bindings.jl -- handling variable bindings

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

(define-structure compiler-bindings (export spec-bindings lex-bindings
					    lexically-pure
					    spec-bound-p
					    note-binding
					    note-bindings
					    binding-lexical-addr
					    emit-binding
					    emit-varset
					    note-binding-modified
					    binding-modified-p
					    note-binding-captured
					    binding-captured-p
					    note-closure-made)
  (open rep
	compiler-utils
	compiler-lap
	compiler-basic
	compiler-const
	bytecodes)

  (define spec-bindings (make-fluid '()))	;list of bound variables
  (define lex-bindings (make-fluid '()))	;alist of bound variables
  (define lexically-pure (make-fluid t))	;any dynamic state?

  (defun spec-bound-p (var)
    (or (memq var (fluid defvars)) (special-variable-p var)))

  ;; note that the outermost binding of symbol VAR has state TAG
  (defun tag-binding (var tag)
    (let ((cell (assq var (fluid lex-bindings))))
      (when cell
	(unless (memq tag (cdr cell))
	  (rplacd cell (cons tag (cdr cell)))))))

  ;; return t if outermost binding of symbol VAR has state TAG
  (defun binding-tagged-p (var tag)
    (let ((cell (assq var (fluid lex-bindings))))
      (and cell (memq tag (cdr cell)))))

  ;; note that symbol VAR has been bound
  (defun note-binding (var)
    (if (spec-bound-p var)
	(progn
	  ;; specially bound (dynamic scope)
	  (fluid-set spec-bindings (cons var (fluid spec-bindings)))
	  (fluid-set lexically-pure nil))
      ;; assume it's lexically bound otherwise
      (fluid-set lex-bindings (cons (list var) (fluid lex-bindings))))
    (when (eq var (fluid lambda-name))
      (fluid-set lambda-name nil)))

  (defmacro note-bindings (vars)
    (list 'mapc 'note-binding vars))

  ;; note that the outermost binding of VAR has been modified
  (defun note-binding-modified (var)
    (tag-binding var 'modified))

  (defun binding-modified-p (var)
    (binding-tagged-p var 'modified))

  ;; note that the outermost binding of VAR has been captured by a closure
  (defun note-binding-captured (var)
    (tag-binding var 'captured))

  (defun binding-captured-p (var)
    (binding-tagged-p var 'captured))

  ;; note that all current lexical bindings have been captured
  (defun note-closure-made ()
    (mapc (lambda (cell)
	    (note-binding-captured (car cell))) (fluid lex-bindings)))

  (defun binding-lexical-addr (var)
    (if (spec-bound-p var)
	nil
      (catch 'out
	(let
	    ((i 0))
	  (mapc (lambda (x)
		  (when (eq (car x) var)
		    (throw 'out i))
		  (setq i (1+ i))) (fluid lex-bindings))
	  nil))))

  (defun emit-binding (var)
    (emit-insn (if (spec-bound-p var)
		   (bytecode bindspec)
		 (bytecode bind))
	       (add-constant var))
    (note-binding var))

  (defun emit-varset (sym)
    (test-variable-ref sym)
    (if (spec-bound-p sym)
	(emit-insn (bytecode setq) (add-constant sym))
      (let
	  ((lex-addr (binding-lexical-addr sym)))
	(if lex-addr
	    ;; The lexical address is known. Use it to avoid scanning
	    (progn
	      (emit-insn (bytecode setn) lex-addr)
	      (note-binding-modified sym))
	  ;; No lexical binding, but not special either. Just
	  ;; update the global value
	  (emit-insn (bytecode setg) (add-constant sym)))))))
