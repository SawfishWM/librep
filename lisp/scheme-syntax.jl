#| scheme-syntax.jl -- syntax macros

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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;; ugh! rep's macros really suck when used across module boundaries..

(define-structure scheme-syntax (export quote lambda if set! cond case
					and or let let* letrec begin do
					delay define)
    (open rep scheme-syntax-funs)

;;; syntax

  (defmacro lambda args (apply expand-lambda args))

  (defmacro if args (apply expand-if args))

  (defmacro set! args (apply expand-set! args))

  (defmacro cond args (apply expand-cond args))

  (defmacro case args (apply expand-case args))

  (defmacro or args (apply expand-or args))

  (defmacro and args (apply expand-and args))

  (defmacro let args (apply expand-let args))

  (defmacro let* args (apply expand-let* args))

  (defmacro letrec args (apply expand-letrec args))

  (defmacro begin forms (cons '\#progn forms))

  (defmacro do args (apply expand-do args))

  (defmacro delay args (apply expand-delay args))

  (defmacro define args (apply expand-define args)))
