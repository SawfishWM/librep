#| environ.jl -- Functions to manipulate the process-environment

   $Id$

   Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>

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

(declare (in-module rep.system))

(open-structures '(rep.regexp
		   rep.data))

;;;###autoload
(defun getenv (name)
  "Return the value of the environment variable NAME, a string. The variable
`process-environment' is used to find the value."
  (let ((cell process-environment)
	(regexp (concat (quote-regexp name) ?=)))
    (catch 'return
      (while (consp cell)
	(when (string-looking-at regexp (car cell))
	  (throw 'return (substring (car cell) (match-end))))
	(setq cell (cdr cell))))))

;;;###autoload
(defun setenv (name value)
  "Set the current value of the environment variable NAME to the string VALUE.
The `process-environment' variable is destructively modified."
  (let ((cell process-environment)
	(regexp (concat (quote-regexp name) ?=)))
    (catch 'return
      (while (consp cell)
	(when (string-looking-at regexp (car cell))
	  (rplaca cell (concat name ?= value))
	  (throw 'return value))
	(setq cell (cdr cell)))
      (setq process-environment (cons (concat name ?= value)
				      process-environment)))))

;;;###autoload
(defun unsetenv (name)
  "Delete the environment variable called NAME."
  (let ((re (concat (quote-regexp name) ?=)))
    (setq process-environment
	  (delete-if (lambda (x)
		       (string-looking-at re x)) process-environment))))
