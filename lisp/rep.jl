#| bootstrap for rep module

   $Id$

   Copyright (C) 1993, 1994, 2000 John Harper <john@dcs.warwick.ac.uk>

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

(declare (in-module rep))

(open-structures '(rep.module-system
		   rep.lang.interpreter
		   rep.lang.symbols
		   rep.lang.math
		   rep.lang.debug
		   rep.vm.interpreter
		   rep.io.streams
		   rep.io.files
		   rep.io.processes
		   rep.io.file-handlers
		   rep.data
		   rep.regexp
		   rep.system))

;;(setq backtrace-on-error '(void-value invalid-function bad-arg missing-arg))

(defvar standard-output (stdout-file)
  "Stream that `prin?' writes its output to by default.")

(defvar standard-input (stdin-file)
  "Stream that `read' takes its input from by default.")

(defvar standard-error (stderr-file)
  "Standard stream for error output.")

;; null i18n function until gettext is loaded
(defun _ (arg) arg)
(export-bindings '(_))

(export-bindings (parse-interface '(compound-interface
				    (structure-interface rep.lang.interpreter)
				    (structure-interface rep.lang.debug)
				    (structure-interface rep.lang.symbols)
				    (structure-interface rep.lang.math)
				    (structure-interface rep.lang.debug)
				    (structure-interface rep.data)
				    (structure-interface rep.io.streams)
				    (structure-interface rep.vm.interpreter)
				    (structure-interface rep.module-system)
				    (export backquote))))

;; later changed to 'user
(setq *user-structure* 'rep)

(require 'rep.lang.backquote)
(require 'rep.io.file-handlers.tilde)

(defvar debug-entry (make-autoload 'debug-entry "rep/lang/debugger"))
(defvar debug-exit)
(defvar debug-error-entry (make-autoload 'debug-error-entry "rep/lang/debugger"))
