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

(define-structure rep.util.memoize

    (export memoize memoize-function)

    (open rep
	  rep.data.tables)

  (define-structure-alias memoize rep.util.memoize)

  (define (memoize f)
    "Create and return a caching version of the function F. F may not be
an autoload definition."

    (unless (functionp f)
      (error "can only memoize functions: %s" f))

    (let ((cache (make-table equal-hash equal)))

      (lambda args
	(or (table-ref cache args)
	    (table-set cache args (apply f args))))))

  ;; backwards compatibility
  (define memoize-function memoize))
