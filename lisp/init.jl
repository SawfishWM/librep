;;;; init.jl -- Standard initialisation script
;;;  Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
;;;  $Id$

;;; This file is part of Jade.

;;; Jade is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; Jade is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with Jade; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(message "Initialising; wait..." t)

(load "lisp")
(load "backquote")
(load "tilde")
(load "loadkeys")
(load "windows")
(load "buffers")
(load "modes")
(load "edit")
(load "rcs-hooks")
(load "dired-hooks")

;; Install all autoload hooks. This is done last so that it works
;; when dumped. We load autoload.jl to ensure that we don't get a
;; compiled (and possibly out of date) version
(load "autoload.jl")

;; Load site specific initialisation. Errors here are trapped since
;; they're probably not going to leave the editor in an unusable state
(condition-case error-data
    (progn
      ;; First the site-wide stuff, the t means don't complain
      ;; if it doesn't exist
      (load "site-init" t)
      ;; Now try to interpret the user's startup file, or failing that
      ;; the default.jl file providing site-wide user options
      (or
       (load (concat (user-home-directory) ".jaderc") t t)
       (load "default" t)))
  (error
   (format (stderr-file) "error in local config--> %S\n" error-data)))

;; Set up the first window as command shell type thing
(with-buffer default-buffer
  (lisp-mode))

;; Print a message in the first buffer
(format default-buffer
	";; %s, Copyright (C) 1993, 1994 John Harper
;; Jade comes with ABSOLUTELY NO WARRANTY; for details see the file COPYING\n\n"
	(version-string))
;; Don't want it in the undo list
(setq buffer-undo-list nil)
(set-buffer-modified default-buffer nil)

;; Use all arguments which are left.
(let
    (arg)
  (while (setq arg (car command-line-args))
    (setq command-line-args (cdr command-line-args))
    (cond
      ((equal "-f" arg)
       (setq arg (car command-line-args)
	     command-line-args (cdr command-line-args))
       (funcall (read-from-string arg)))
      ((equal "-l" arg)
       (setq arg (car command-line-args)
	     command-line-args (cdr command-line-args))
       (load arg))
      ((equal "-q" arg)
       (throw 'quit 0))
      (t
       (find-file arg)))))

(message (version-and-build-string))
