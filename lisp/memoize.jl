;; memoize.jl -- create caching-enabled functions
;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

;; $Id$

;; This file is part of librep.

;; librep is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; librep is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with librep; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'memoize)

(defun memoize-function (fun)
  "Create a caching version of the function FUN, either a symbol or a
function. If a symbol, its value is replaced by a caching version of
the function stored in the symbol.

FUN may not be, or be defined as, an autoload definition."
  (let
      ((memoize
	(lambda (f)
	  (let
	      ;; XXX need a hash table here..
	      (cache)
	    (lambda (&rest args)
	      (let
		  ((cell (assoc args cache)))
		(if cell
		    (cdr cell)
		  (setq cell (cons args (apply f args)))
		  (setq cache (cons cell cache))
		  (cdr cell))))))))
    (cond ((functionp fun)
	   (memoize fun))
	  ((symbolp fun)
	   (set fun (memoize (symbol-value fun))))
	  (t
	   (error "Not a function or symbol: %s" fun)))))
