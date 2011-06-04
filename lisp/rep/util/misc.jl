#| rep.util.misc -- misc functions

   $Id$

   Copyright (C) 2011 Christopher Roy Bratusek et all

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

(define-structure rep.util.misc

    (export position
            string->symbol)

    (open rep)

  (define string->symbol intern)

  (define (position item l)
    (let loop ((rest l)
               (i 0))
         (if (equal item (car rest))
             i
           (if rest
               (loop (cdr rest) (1+ i)))))))
