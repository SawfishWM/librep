#| profiler.jl -- interface to low-level lisp profiler

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

(define-structure rep.lang.profiler

    (export call-in-profiler
	    print-profile
	    profile-interval)

    (open rep
	  rep.lang.record-profile
	  rep.data.symbol-table)

  (define (call-in-profiler thunk)
    (start-profiler)
    (unwind-protect
	(thunk)
      (stop-profiler)))

  (define (print-profile #!optional stream)
    ;; each element is (SYMBOL . (LOCAL . TOTAL))
    (let ((profile '())
	  (total-samples 0))
      (symbol-table-walk (lambda (key data)
			   (setq profile (cons (cons key data) profile))
			   (setq total-samples (+ total-samples (car data))))
			 (fetch-profile))
      (setq profile (sort profile (lambda (x y)
				    (> (cadr x) (cadr y)))))
      (format (or stream standard-output)
	      "%-32s       %10s       %10s\n\n"
	      "Function Name" "Self" "Total")
      (mapc (lambda (cell)
	      (let ((name (car cell))
		    (local (cadr cell))
		    (total (cddr cell)))
		(when (> local 0)
		  (format (or stream standard-output)
			  "%-32s %10d (%02.2d%%) %10d (%02.2d%%)\n"
			  (symbol-name name) local
			  (round (* (/ local total-samples) 100)) total
			  (round (* (/ total total-samples) 100))))))
	    profile))))
