#| compiler-vars.jl -- special variables for compiler

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

(define-structure compiler-vars (export)

  (open rep)


;;; Options

  (defvar comp-write-docs nil
    "When t all doc-strings are appended to the doc file and replaced with
their position in that file.")

  (defvar comp-max-inline-depth 8
    "The maximum nesting of open-coded function applications.")

  (defvar comp-no-low-level-optimisations nil)
  (defvar comp-debug nil)


;;; Environment of this byte code sequence being compiled

  ;; Output state

  (defvar comp-constant-alist '())	;list of (VALUE . INDEX)
  (defvar comp-constant-index 0)	;next free constant index number
  (defvar comp-current-stack 0)		;current stack requirement
  (defvar comp-max-stack 0)		;highest possible stack
  (defvar comp-intermediate-code nil)	;list of (INSN . [ARG]), (TAG . REFS)

  ;; Compilation "environment"

  (defvar comp-open-modules '(rep))
  (defvar comp-accessed-modules nil)

  (defvar comp-macro-env nil)		;alist of (NAME . MACRO-DEF)
  (defvar comp-default-macro-env
    (list (cons 'eval-when-compile (lambda (x)
				     `(quote ,(eval x))))))

  (defvar comp-const-env '())		;alist of (NAME . CONST-DEF)
  (defvar comp-inline-env '())		;alist of (NAME . FUNCTION-VALUE)
  (defvar comp-defuns nil)		;alist of (NAME REQ OPT RESTP)
					; for all functions/macros in the file
  (defvar comp-defvars nil)		;all variables declared at top-level
  (defvar comp-defines nil)		;all lex. vars. declared at top-level
  (defvar comp-spec-bindings '())	;list of currently bound variables
  (defvar comp-lex-bindings '())	;list of currently bound variables
  (defvar comp-current-file nil)	;the file being compiled
  (defvar comp-current-fun nil)		;the function being compiled
  (defvar comp-inline-depth 0)		;depth of lambda-inlining
  (defvar comp-lexically-pure t)	;any dynamic state?
  (defvar comp-lambda-name nil)		;name of current lambda exp
  (defvar comp-lambda-args nil)		;arg spec of current lambda

  (defvar comp-output-stream nil)	;stream for compiler output

)
