#| rep.io.files bootstrap

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

(declare (in-module rep.io.files))

(open-structures '(rep.lang.symbols
		   rep.data
		   rep.system))

(defun file-name= (name1 name2)
  "Returns t when NAME1 and NAME2 both name the same file."
  (string= (canonical-file-name name1) (canonical-file-name name2)))

(defun file-newer-than-file-p (file1 file2)
  "Returns t when FILE1 was modified more recently than FILE2."
  (time-later-p (file-modtime file1) (file-modtime file2)))

(defun load-all (file #!optional callback)
  "Try to load files called FILE (or FILE.jl, etc) from all directories in the
LISP load path (except the current directory)."
  (let loop ((dirs load-path))
    ;; Normally the last entry in load-path is `.' We don't
    ;; want to use that. But can't just check if each item
    ;; is the current directory since sometimes rep is run
    ;; with REPLISPDIR=.
    (when dirs
      (when (or (cdr dirs) (not (member (car dirs) '("." ""))))
	(let
	    ((full-name (expand-file-name file (car dirs))))
	  (when (or (file-exists-p full-name)
		    (file-exists-p (concat full-name ".jl"))
		    (file-exists-p (concat full-name ".jlc")))
	    (if callback
		(callback full-name)
	      (load full-name nil t)))))
      (loop (cdr dirs)))))

(defun call-after-load (library thunk)
  "Arrange for THUNK to be called immediately after the library of Lisp code
LIBRARY has been read by the `load' function. Note that LIBRARY must exactly
match the FILE argument to `load'."
  (let ((tem (assoc library after-load-alist)))
    (if tem
	(rplacd tem (cons thunk (cdr tem)))
      (setq after-load-alist (cons (cons library (list thunk))
				   after-load-alist)))))

(defun eval-after-load (library form)
  "Arrange for FORM to be evaluated immediately after the library of Lisp code
LIBRARY has been read by the `load' function. Note that LIBRARY must exactly
match the FILE argument to `load'."
  (call-after-load library (lambda ()
			     (eval form (get-structure *user-structure*)))))

(export-bindings '(file-name= file-newer-than-file-p
		   load-all call-after-load eval-after-load))
