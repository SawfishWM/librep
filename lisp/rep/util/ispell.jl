#| ispell.jl -- ispell wrapper

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

(define-structure rep.util.ispell

    (export ispell-start
	    ispell-stop
	    ispell-word
	    ispell-test-word
	    ispell-set-dictionary
	    ispell-add-word-to-dictionary
	    ispell-add-word-for-session
	    ispell-save-dictionary)

    (open rep
	  rep.regexp
	  rep.io.processes
	  rep.system)

  (defvar *ispell-program* "ispell"
    "Filename of program used to start ispell(1).")

  (defvar *ispell-options* nil
    "List of options to pass to Ispell")

  (defvar *ispell-dictionary* nil
    "Name of dictionary to pass to Ispell, or nil for the default.")

  (defvar *ispell-timeout* 5
    "Seconds to wait for ispell output before giving up.")

  (defvar *ispell-echo-output* nil
    "Use for debugging only.")

  (define process nil
    "Subprocess that ispell is running in, or nil if ispell isn't running.")

  (define process-busy nil
    "When t, the process is being used to check a word, but not all
results have been received.")

  (define id-string nil
    "String sent by ispell identifying itself when it started executing.")

  (define pending-output nil
    "String of output received from ispell but not processed.")

  (define line-callback (make-fluid nil)
    "Function to call asynchronously with a single line of output from ispell.")

;;; Process management

  ;; Function to buffer output from Ispell
  (define (output-filter output)
    (when (integerp output)
      (setq output (make-string 1 output)))
    (and *ispell-echo-output*
	 (stringp output)
	 (let ((print-escape t))
	   (format standard-error "Ispell: %S\n" output)))
    (setq pending-output (concat pending-output output))
    (while (and (fluid line-callback)
		pending-output
		(string-match "\n" pending-output))
      (let ((line (substring pending-output 0 (match-end))))
	(setq pending-output (substring pending-output (match-end)))
	((fluid line-callback) line))))

  ;; Start the process if it isn't already
  (define (ispell-start)
    (unless process
      (setq process (make-process output-filter))
      (set-process-function process (lambda ()
				      (setq process nil)
				      (setq id-string nil)))
      ;; Use a pty if possible. This allow EOF to be sent via ^D
      (set-process-connection-type process 'pty)
      (apply start-process process *ispell-program* "-a"
	     (nconc (and *ispell-dictionary*
			 (list "-d" *ispell-dictionary*))
		    *ispell-options*))
      (setq pending-output nil)
      (fluid-set line-callback nil)
      (setq id-string (ispell-read-line))
      (unless (string-match "ispell version" id-string 0 t)
	(ispell-stop)
	(error "Ispell: %s" id-string))))

  (define (ispell-stop)
    "Kill any subprocesses being used internally to run Ispell."
    (accept-process-output-1 process 0)	;in case the process already died
    (when process
      (ispell-save-dictionary)
      (if (eq (process-connection-type process) 'pty)
	  (write process ?\^D)
	;; Not so successful..
	(interrupt-process process))
      (let ((counter 0))
	(while (and (accept-process-output-1 process *ispell-timeout*) process)
	  (if (< counter 2)
	      (interrupt-process process)
	    (kill-process process))
	  (setq counter (1+ counter))))))

  ;; Read one whole line from the process (including newline)
  (define (ispell-read-line)
    (let ((out nil))
      (let-fluids ((line-callback (lambda (l)
				    (setq out l)
				    ;; Only want the first line
				    (fluid-set line-callback nil))))
	;; Flush any pending output
	(output-filter nil)
	(while (and (not out) process
		    (not (accept-process-output-1 process *ispell-timeout*))))
	(or out (error "Ispell timed out waiting for output")))))

  ;; put in the before-exit-hook
  (define (before-exit)
    (when process
      (ispell-stop)))

  (add-hook 'before-exit-hook before-exit)

  ;; Arbitrate access to the Ispell process, the mutex must be obtained
  ;; before sending a command that generates output. An error is signalled
  ;; if the process is busy
  (define (mutex grab)
    (if grab
	(if process-busy
	    (error "Ispell process is busy!")
	  (ispell-start)
	  (setq process-busy t))
      (setq process-busy nil)))

  ;; Check a word with Ispell. Returns the raw (single-line) output
  ;; see ispell(1) for details (under the -a option)
  (define (ispell-word word)
    (let (response tem)
      (mutex t)
      (unwind-protect
	  (progn
	    (format process "%s\n" word)
	    (setq response (ispell-read-line))
	    (if (eq (aref response 0) ?\n)
		;; This shouldn't happen
		(error "Null output from Ispell")
	      ;; Gobble following blank line
	      (setq tem (ispell-read-line))
	      (unless (eq (aref tem 0) ?\n)
		(error "Non-null trailing line from Ispell"))))
	(mutex nil))
      response))

  ;; return true if WORD is spelt correctly
  (define (ispell-test-word word)
    (let ((response (ispell-word word)))
      (string-looking-at "^[*+-]" response)))

;;; Dictionary management

  (define (ispell-set-dictionary dict-name)
    "Set the name of the dictionary used by Ispell to DICT-NAME."
    (setq *ispell-dictionary* dict-name)
    (when process
      (ispell-stop)
      (ispell-start))
    (call-hook '*ispell-dictionary-changed*))

  (define (ispell-add-word-to-dictionary word)
    "Add the string WORD to your personal Ispell dictionary."
    (ispell-start)
    (format process "*%s\n" word)
    (call-hook '*ispell-dictionary-changed*))

  (define (ispell-add-word-for-session word)
    "Add the string WORD to Ispell's per-session dictionary."
    (ispell-start)
    (format process "@%s\n" word)
    (call-hook '*ispell-dictionary-changed*))

  (define (ispell-save-dictionary)
    "Make Ispell save the current personal dictionary to its file."
    (when process
      (write process "#\n"))))
