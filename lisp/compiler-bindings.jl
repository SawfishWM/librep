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

(define-structure compiler-bindings (export comp-spec-bound-p
					    comp-note-binding
					    comp-note-bindings
					    comp-binding-lexical-addr
					    comp-emit-binding
					    comp-emit-varset)
  (open rep
	compiler-utils
	compiler-lap
	compiler-vars
	compiler-const
	bytecodes)

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
    (comp-write-op (if (comp-spec-bound-p var)
		       (bytecode bindspec)
		     (bytecode bind))
		   (comp-add-constant var))
    (comp-note-binding var))

  (defun comp-emit-varset (sym)
    (comp-test-varref sym)
    (if (comp-spec-bound-p sym)
	(comp-write-op (bytecode setq) (comp-add-constant sym))
      (let
	  ((lex-addr (comp-binding-lexical-addr sym)))
	(if lex-addr
	    ;; The lexical address is known. Use it to avoid scanning
	    (comp-write-op (bytecode setn) lex-addr)
	  ;; No lexical binding, but not special either. Just
	  ;; update the global value
	  (comp-write-op (bytecode setg) (comp-add-constant sym)))))))
