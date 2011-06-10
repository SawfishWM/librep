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
            string->symbol
            beautify-symbol-name
            remove-newlines
            option-index)

    (open rep
	  rep.regexp)

  (define string->symbol intern)

  (define (position item l)
    (let loop ((slow l) (l l) (i 0))
      (cond ((not (consp l)) #f)
            ((equal item (car l)) i)
            (#t (let ((l (cdr l)) (i (1+ i)))
                  (cond ((not (consp l)) #f)
                        ((equal item (car l)) i)
                        ((eq l slow) #f)
                        (#t (loop (cdr slow) (cdr l) (1+ i)))))))))

  (define (beautify-symbol-name symbol)
    (cond ((stringp symbol) symbol)
	  ((not (symbolp symbol)) (format "%s" symbol))
	  (t
	   (let ((name (copy-sequence (symbol-name symbol))))
	     (while (string-match "[-:]" name)
	       (setq name (concat (substring name 0 (match-start))
				  ?  (substring name (match-end)))))
	     (aset name 0 (char-upcase (aref name 0)))
	     (_ name)))))

  (define (remove-newlines string)
    (let loop
	((point 0)
	 (out '()))
      (if (string-match "\n" string point)
	  (loop (match-end)
		(list* #\space (substring string point (match-start)) out))
	(apply concat (nreverse (cons (substring string point) out))))))

  (define (option-index lst x)
    (let loop ((i 0) (rest lst))
      (cond ((null rest) nil)
	    ((eq (or (caar rest) (car rest)) x) i)
	    (t (loop (1+ i) (cdr rest)))))))