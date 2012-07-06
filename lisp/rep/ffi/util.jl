#| util.jl -- FFI helpers

  Author: Sergey Bolshakov <sbolshakov@altlinux.ru>
  Version: $Id: util.jl,v 1.5 2006/07/16 19:40:32 me Exp $

  This file is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This file is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this file.  If not, write to the Free Software
  Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

|#

(define-structure rep.ffi.util

    (export ffi-bind
	    ffi-type-enum)

    (open rep
	  rep.ffi
	  rep.data.tables)

  (define (ffi-bind soname tab)
    (let ((handle (ffi-load-library soname))
	  (symtab (make-table symbol-hash eq-hash))
	  (ifacetab (make-table symbol-hash eq-hash)))
      (mapc
       (lambda (elt)
	 (apply
	  (lambda (key dlsym ret . args)
	    (table-set symtab key (ffi-lookup-symbol handle dlsym))
	    (table-set ifacetab key (ffi-interface ret args)))
	  elt))
       tab)
      (lambda (key . args)
	(and
	 (table-bound-p symtab key)
	 (ffi-apply (table-ref ifacetab key)
		    (table-ref symtab key) args)))))

  (define (enump sym enum-alist)
    (and (symbolp sym)
	 (assq sym enum-alist)))

  (define (enum->sym num enum-alist)
    (let ((p (rassq num enum-alist)))
      (if p (car p)
	(signal 'bad-arg `(,num)))))

  (define (sym->enum sym enum-alist)
    (let ((p (assq sym enum-alist)))
      (if p (cdr p)
	(signal 'bad-arg `(,sym)))))
  
  (define (ffi-type-enum enum-alist)
    (ffi-type
     ffi-type-sint32
     (lambda (x) (enump x enum-alist))
     (lambda (x) (sym->enum x enum-alist))
     (lambda (x) (enum->sym x enum-alist))))

  )
