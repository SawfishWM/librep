#| syntax-funs.jl -- syntax expansion functions

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

(define-structure unscheme.syntax-funs

    (export expand-lambda
	    expand-if
	    expand-set!
	    expand-cond
	    expand-case
	    expand-and
	    expand-or
	    expand-let
	    expand-let*
	    expand-letrec
	    expand-do
	    expand-delay
	    expand-define)

    ((open rep
	   scheme.syntax-funs)
     (access unscheme.data))

;;; syntax

  (define (expand-if test consequent . alternative)
    (cond ((cdr alternative)
	   (error "Scheme `if' only takes one else form"))
	  (alternative
	   `(\#cond (,test ,consequent)
		    ('t ,(car alternative))))
	  (t `(\#cond (,test ,consequent))))))
