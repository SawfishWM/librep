#| no-lang.jl -- minimal environment for when modules don't import a lang

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

(define-structure rep.vm.compiler.no-lang ()

    (open rep
	  rep.vm.compiler.modules
	  rep.vm.compiler.rep)

  ;; setup properties to tell the compiler where to look for symbols
  ;; in the `no-lang'  package
  (put 'no-lang 'compiler-handler-property 'no-lang-compile-fun)

;;; no pass-1 or pass-2 handlers means no compilation!

;;; module compilers

  ;; module compilers from compiler-modules
  (put 'structure 'no-lang-compile-fun compile-structure)
  (put 'define-structure 'no-lang-compile-fun compile-define-structure)
  (put 'structure-ref 'no-lang-compile-fun compile-structure-ref))
