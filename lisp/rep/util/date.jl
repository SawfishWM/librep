;;;; date.jl -- Date manipulation
;;;  Copyright (C) 1997 John Harper <john@dcs.warwick.ac.uk>
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

(declare (unsafe-for-call/cc))

(define-structure rep.util.date

    (export parse-date
	    date-vec-day-abbrev
	    date-vec-day
	    date-vec-month-abbrev
	    date-vec-month
	    date-vec-year
	    date-vec-hour
	    date-vec-minute
	    date-vec-second
	    date-vec-timezone
	    date-vec-epoch-time)

    (open rep
	  rep.system
	  rep.regexp)

  (define-structure-alias date rep.util.date)

  (define date-month-alist '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4)
			     ("May" . 5) ("Jun" . 6) ("Jul" . 7) ("Aug" . 8)
			     ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)
			     ("January" . 1) ("February" . 2) ("March" . 3)
			     ("April" . 4) ("June" . 6) ("July" . 7)
			     ("August" . 8) ("September" . 9) ("October" . 10)
			     ("November" . 11) ("December" . 12))
    "Alist of (MONTH-NAME . MONTH-NUM).")

  (define date-timezone-alist
    '(("UT" . 0) ("GMT" . 0)
      ("EST" . -300) ("EDT" . -240)
      ("CST" . -360) ("CDT" . -300)
      ("MST" . -420) ("MDT" . -360)
      ("PST" . -480) ("PDT" . -420))
    "Alist of (TIMEZONE . MINUTES-DIFFERENCE).")

  (define date-two-digit-year-prefix (substring (current-time-string) 20 22)
    "A two-digit string that will be prepended to year specifications that
only have two, lower order, digits. This is picked up automatically from
the current year, i.e. 1997 -> \"19\", 2001 -> \"20\".")


;; Date parsing

  ;; Constants defining date structure fields
  (defconst date-vec-day-abbrev 0)
  (defconst date-vec-day 1)
  (defconst date-vec-month-abbrev 2)
  (defconst date-vec-month 3)
  (defconst date-vec-year 4)
  (defconst date-vec-hour 5)
  (defconst date-vec-minute 6)
  (defconst date-vec-second 7)
  (defconst date-vec-timezone 8)
  (defconst date-vec-epoch-time 9)

  ;; Parse the date header at position POINT in STRING, returns vector
  ;; [DAY-ABBREV DAY MONTH-ABBREV MONTH YEAR HOUR MINUTE SECOND
  ;;  TZ-STRING TIME_T]

  (defun parse-date (string #!optional point)
    "Parse the date specifiction in STRING, optionally starting at the POINT'th
character in the string. This will parse dates in RFC-822 mail messages."
    (unless point (setq point 0))
    (let
	((day-abbrev "")
	 (day -1)
	 (month-abbrev "")
	 (month -1)
	 (year -1)
	 (hour 0)
	 (minute 0)
	 (second 0)
	 (timezone 0)
	 time_t tem)
      (while (< point (length string))
	(cond
	 ((string-looking-at "[\t ]*([0-9]+)([\t ]+|$)" string point)
	  (let*
	      ((start (match-start 1))
	       (end (match-end 1))
	       (value (string->number (substring string start end))))
	    ;; Could be year or day of month
	    (if (or (>= day 0) (> (- end start) 2))
		;; Assume year
		(if (= (- end start) 2)
		    ;; two-digit year
		    (setq year (+ (* 100 (string->number
					  date-two-digit-year-prefix)) value))
		  (setq year value))
	      (setq day value))
	    (setq point end)))
	 
	 ((string-looking-at
	   "[\t ]*([0-9]+):([0-9]+)(:[0-9]+)?[\t ]*([A-Z]+|[+-][0-9]+)?[\t ]*"
	   string point)
	  ;; Time spec.
	  (setq point (match-end))
	  (setq hour (string->number (substring string (match-start 1)
						(match-end 1)))
		minute (string->number (substring string (match-start 2)
						  (match-end 2)))
		second (if (equal (match-start 3) (match-end 3))
			   0
			 (string->number (substring string (1+ (match-start 3))
						    (match-end 3))))
		timezone (if (equal (match-start 4) (match-end 4))
			     "UT"
			   (substring string (match-start 4) (match-end 4))))
	  (if (setq tem (assoc timezone date-timezone-alist))
	      (setq timezone (cdr tem))
	    ;; Try +-HHMM
	    (if (string-looking-at "[+-]([0-9][0-9])([0-9][0-9])" timezone)
		(setq timezone (* (if (= (aref timezone 0) ?+) 1 -1)
				  (+ (* 60 (string->number
					    (substring timezone
						       (match-start 1)
						       (match-end 1))))
				     (string->number
				      (substring timezone
						 (match-start 2)
						 (match-end 2))))))
	      ;; whatever..
	      (setq timezone 0))))
	 
	 ((string-looking-at
	   "[\t ]*(Mon|Tue|Wed|Thu|Fri|Sat|Sun)[a-z]*[\t ]*,?[\t ]*"
	   string point t)
	  ;; Found day spec
	  (setq day-abbrev (substring string (match-start 1) (match-end 1)))
	  (setq point (match-end)))
	 
	 ((string-looking-at
	   "[\t ]*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*[\t ]*"
	   string point t)
	  ;; Month name
	  (setq month-abbrev (substring string (match-start 1) (match-end 1)))
	  (setq month (cdr (assoc month-abbrev date-month-alist)))
	  (setq point (match-end)))
	 
	 ((string-looking-at
	   "[\t ]*([0-9][0-9][0-9][0-9])-([0-9][0-9])-([0-9][0-9])[\t ]*"
	   string point)
	  ;; ISO 8601 calendar date YYYY-MM-DD
	  (setq year (string->number (expand-last-match "\\1")))
	  (setq month (string->number (expand-last-match "\\2")))
	  (setq month-abbrev (car (rassq month date-month-alist)))
	  (setq day (string->number (expand-last-match "\\3")))
	  ;; XXX day of week calculation
	  (setq point (match-end)))
	 
	 (t
	  ;; Garbage in -- garbage out
	  (setq point (length string)))))
      
      (when (< year 0)
	(setq year (string->number (current-time-string nil "%Y"))))
      
      (when (< month 0)
	(setq month (string->number (current-time-string nil "%m"))))
      
      (when (< day 0)
	(setq day (string->number (current-time-string nil "%d"))))
      
      ;; Use Gauss' algorithm (?) to find seconds since 1970
      ;; This subroutine is copied from my VMM operating system,
      ;; which was in turn copied from Linux
      (let
	  ((g-month (- month 2))
	   (g-year year)
	   total-seconds total-days)
	(when (>= 0 g-month)
	  ;; Put feb last since it has leap day
	  (setq g-month (+ g-month 12)
		g-year (1- g-year)))
	;; (DAYS . SECONDS)
	(setq total-days (+ (- (quotient g-year 4)
			       (quotient g-year 100))
			    (quotient g-year 400)
			    (quotient (* 367 g-month) 12)
			    day
			    (* g-year 365)
			    -719499)
	      total-seconds (+ second (* 60 (+ minute
					       (- timezone)
					       (* 60 hour)))))
	(setq time_t (fix-time (cons total-days total-seconds))))
      
      (when (and (string= day-abbrev "") time_t)
	;; January 1, 1970 was a Thursday
	(let
	    ((dow (% (+ (car time_t) 4) 7)))
	  (when (< dow 0)
	    (setq dow (+ dow 7)))
	  (setq day-abbrev
		(aref ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"] dow))))
      
      (vector day-abbrev day month-abbrev month
	      year hour minute second timezone time_t))))
