#| rep.net.domain-name -- domain name utility functions

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

(define-structure rep.net.domain-name

    (export domain-parts
	    domain-above
	    domain-above-p
	    domain-below-p
	    domain-equal-p
	    map-domains
	    map-domains-downwards)

    (open rep
	  rep.regexp)

  (define (domain-parts domain)
    "Return the list of domain components that the string DOMAIN consists of."
    (string-split "\\." domain))

  (define (domain-above domain)
    "Return the name of parent domain of the string DOMAIN."
    (and (string-match "\\." domain)
	 (substring domain (match-end))))

  ;; Returns +ve if D1 is a superset of D2, -ve if D1 is a subset of D2,
  ;; zero if (= D1 D2), or false if D1 and D2 aren't similar
  (define (compare-domains d1 d2)
    (let loop ((p1 (domain-parts d1))
	       (p2 (domain-parts d2))
	       (ret 0))
      (cond ((> (length p1) (length p2))
	     (loop (cdr p1) p2 (1+ ret)))
	    ((< (length p1) (length p2))
	     (loop p1 (cdr p2) (1- ret)))
	    ((and p1 p2)
	     (if (string= (car p1) (car p2))
		 (loop (cdr p1) (cdr p2) ret)
	       nil))
	    (t ret))))

  (define (domain-above-p d1 d2)
    "Return true if domain name D1 is `above' domain name D2."
    (let ((value (compare-domains d1 d2)))
      (and value (< value 0))))

  (define (domain-below-p d1 d2)
    "Return true if domain name D1 is `below' domain name D2."
    (let ((value (compare-domains d1 d2)))
      (and value (> value 0))))

  (define (domain-equal-p d1 d2)
    "Return true if the domain names D1 and D2 are the same"
    (string= d1 d2))

  (define (map-domains fun domain)
    "Call (FUN NAME) for each sub-domain of DOMAIN (starting with DOMAIN)."
    (when domain
      (fun domain)
      (map-domains fun (domain-above domain))))

  (define (map-domains-downwards fun domain)
    "Call (FUN NAME) for each sub-domain of DOMAIN (ending with DOMAIN)."
    (let ((parts (nreverse (domain-parts domain))))
      (let loop ((current (car parts))
		 (todo (cdr parts)))
	(fun current)
	(when todo
	  (loop (concat (car todo) ?. current) (cdr todo)))))))
