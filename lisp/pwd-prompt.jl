;;;; pwd-prompt.jl -- Prompt for a confidential answer (i.e. a password)
;;;  Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
;;;  $Id$

;;; This file is part of librep.

;;; librep is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; librep is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with librep; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;###autoload
(defun pwd-prompt (prompt)
  "Prompt for a confidential string, with PROMPT as the title string. The
contents of the prompt will be masked out whilst being entered."
  (when (zerop (system "stty -echo"))
    (unwind-protect
	(progn
	  (write standard-error prompt)
	  (unless (string-match "\s$" prompt)
	    (write standard-error ? ))
	  (flush-file standard-error)
	  (let
	      ((string (read-line standard-input)))
	    (when (string-match "\n$" string)
	      (setq string (substring string 0 (match-start))))
	    (write standard-error ?\n)
	    string))
      (system "stty echo"))))
