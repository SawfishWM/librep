#| rep.util.time -- time conversion functions

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

(define-structure rep.util.time

    (export seconds-per-day
	    time->seconds
	    seconds->time
	    time-)

    (open rep)

  (defconst seconds-per-day 86400)

  (define (time->seconds time)
    "Convert the timestamp TIME to the number of seconds since the epoch."
    (+ (* (car time) seconds-per-day) (cdr time)))

  (define (seconds->time secs)
    "Convert the number of secs past the epoch, SECS, to a timestamp."
    (cons (quotient secs seconds-per-day) (mod secs seconds-per-day)))

  (define (time- t1 t2)
    "Return the number of seconds difference between timestamps T1 and T2."
    (- (time->seconds t1) (time->seconds t2))))
