#| unscheme.jl -- inliners for compiling unScheme code

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

(define-structure rep.vm.compiler.unscheme ()

    (open rep
	  rep.lang.doc
	  rep.vm.compiler.modules
	  rep.vm.compiler.utils
	  rep.vm.compiler.basic
	  rep.vm.compiler.const
	  rep.vm.compiler.inline
	  rep.vm.compiler.lap
	  rep.vm.compiler.bindings
	  rep.vm.compiler.rep
	  rep.vm.compiler.scheme
	  rep.vm.bytecodes)

  ;; setup properties to tell the compiler where to look for symbols
  ;; in the `unscheme'  package
  (put 'unscheme 'compiler-handler-property 'unscheme-compile-fun)
  (put 'unscheme 'compiler-transform-property 'unscheme-compile-transform)
  (put 'unscheme 'compiler-sequencer 'begin)


;;; parasite on scheme compiler

  (put 'unscheme 'compiler-pass-1 (get 'scheme 'compiler-pass-1))
  (put 'unscheme 'compiler-pass-2 (get 'scheme 'compiler-pass-2))
  (put 'unscheme 'compiler-foldablep (get 'scheme 'compiler-foldablep))


;;; special compilers

  ;; module compilers from compiler-modules
  (put 'structure 'unscheme-compile-fun compile-structure)
  (put 'define-structure 'unscheme-compile-fun compile-define-structure)
  (put 'structure-ref 'unscheme-compile-fun compile-structure-ref)

  (put 'quote 'unscheme-compile-fun (get 'quote 'rep-compile-fun))
  (put '\#lambda 'unscheme-compile-fun (get 'lambda 'rep-compile-fun))
  (put '\#progn 'unscheme-compile-fun (get 'progn 'rep-compile-fun))

  (put 'set! 'unscheme-compile-fun (get 'set! 'scheme-compile-fun))
  (put '\#define 'unscheme-compile-fun (get '%define 'rep-compile-fun))

  ;; compile let* specially to coalesce all bindings into a single frame
  (put 'let* 'unscheme-compile-fun (get 'let* 'rep-compile-fun))

  ;; let can be compiled straight from its macro definition

  ;; compile letrec specially to handle tail recursion elimination
  (put 'letrec 'unscheme-compile-fun (get 'letrec 'rep-compile-fun))

  (put '\#cond 'unscheme-compile-fun (get 'cond 'rep-compile-fun))
  (put 'case 'unscheme-compile-fun (get 'case 'rep-compile-fun))

  ;; set properties of scheme functions that are pseudonyms of rep fns
  (mapc (lambda (cell)
	  (if (symbolp cell)
	      (put cell 'unscheme-compile-fun (get cell 'rep-compile-fun))
	    (put (car cell) 'unscheme-compile-fun
		 (get (cdr cell) 'rep-compile-fun))))
	'(list list* cons car cdr apply
	  caar cadr cdar cddr caddr
	  (set-car! . rplaca)
	  (set-cdr! . rplacd)
	  (string-set! . aset)
	  (vector-set! . aset)
	  (string-ref . aref)
	  (vector-ref . aref)
	  length
	  (string-length . length)
	  (vector-length . length)
	  - + * / remainder modulo quotient max min floor ceiling
	  truncate round exp log sin cos tan sqrt expt
	  (string-copy . copy-sequence)
	  (vector-copy . copy-sequence)
	  (eqv? . eql)
	  (eq? . eq)
	  (equal? . equal)
	  (pair? . consp)
	  (null? . null)
	  (symbol? . symbolp)
	  (number? . numberp)
	  = < > <= >=
	  (zero? . zerop)
	  (char=? . =)
	  (char<? . <)
	  (char>? . >)
	  (char<=? . <=)
	  (char>=? . >=)
	  (string? . stringp)
	  (string=? . =)
	  (string<? . <)
	  (string>? . >)
	  (string<=? . <=)
	  (string>=? . >=)
	  (vector? . vectorp)
	  (procedure? . functionp)
	  memq memv member assq assoc)))
