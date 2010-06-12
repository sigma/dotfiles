;;; schedule.el --- simple schedule maintainer

;; Copyright (C) 1999 John Wiegley.

;; Author: John Wiegley <johnw@oneworld.new-era.com>
;; Created: 20 Jan 1999
;; Version: 1.41
;; Keywords: calendar
;; X-URL: http://oneworld.new-era.com/johnw/emacs.html

;; The program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This module works with outline-mode, calendar and diary to provide a
;; simple way of keeping track of schedules.  It's recommended that
;; you autoload the `schedule-mode' command in your .emacs file:
;;
;;    (autoload 'schedule-mode "schedule" nil t)

;; To integrate with timeclock.el, set the variable
;; `schedule-integrate-timeclock' to t before loading your schedule
;; file.

;; A typical schedule file might look like the following:
;;
;;    -*-mode: outline; mode: schedule-*-
;;    
;;    * +(Apr09) 1.1d  73%  Small project [2.1d]
;;    ** (Apr07)   1h   0%  Begin the work
;;    ** [Apr07]   4h 100%  Think about the work
;;    ** (Apr07)   1h  80%  Modify the work
;;    ** (Apr09)   3h 110%  Final procrastination [2d]
;;    *  (Apr12)   1d  20%  Some small task
;;    *  (Apr13)   1d   0%  Another small task
;;    *  (Apr14)   1d   0%  Make sure that I did the other tasks!
;;
;;    * +(Apr14)   4h   7%  Very small project [3h]
;;    ** (Apr14)   1h  13%  Sub-task 1
;;    ** (Apr14)   3h   0%  Sub-task 2
;;    *  (Apr15)   4h   0%  Maybe I'll go check my e-mail now...
;;    *  (Apr16)   1d   0%  Finish writing schedule.el
;;
;; Entries can be grouped as project and sub-projects, to any level of
;; nesting (although it stops looking so aesthetic once you go very
;; deep).  Completed tasks have their date bracketed by square
;; brackets.  This records the date that the task was actually
;; finished.  A date in parentheses is a projected date, and will
;; always be on or after the present day.
;;
;; As you work on a task, you must update the percentage complete
;; figure by hand.  Alternately, you can use my timeclock.el mode (see
;; the URL mentioned at the top of this file), which will update it
;; automatically whenever you "checkout" of, or "change" your current
;; working task (provided that point is in the schedule buffer, and
;; located on the same line as the task you want to checkout of).
;;
;; If you want to see how much time a task is expected to take, add
;; the string " []" anywhere within the task (typically at the end).
;; The scheduler will update this figure depending on how much time is
;; remaining to complete the task (based on current projections).
;;
;; If a task runs over-schedule, you can use the " []" string to
;; indicate how much more time is left (an additional estimate).
;; Unless you use timeclock mode to update your task strings for you,
;; you will have to modify both the percentage figure, and the time
;; remaining string, whenever you do work on the task.

;; There are many pieces of data being displayed on each line of a
;; schedule file.  They are:
;;
;;   1) The date when it's expected that this task will be completed,
;;      in parentheses.
;;
;;   2) The date that the task was actually completed, in square
;;      brackets.
;;
;;   3) The original time estimated for the task.
;;
;;   4) The percentage complete.  If this is different from 100%
;;      (either above or below) at the time that the task is finished,
;;      it reflects the amount of time above or below your original
;;      estimate that it took you to complete the task.  Beware.  This
;;      can be very sobering.
;;
;;   5) A description of the task, or project.
;;
;;   6) In square brackets within the task description, the amount of
;;      remaining for "on schedule" tasks, or the amount of extra time
;;      needed for overdue tasks.  "Extra time" is figured into the
;;      amount of time needed for the project as a whole, and the date
;;      projection for when its believed that the task will be
;;      finished.
;;
;; The usefulness of preserving the original time estimate, having the
;; percentage complete figure reflect the amount of time you actually
;; spent, and using the time remaining string to indicate further time
;; estimates, allows you both to keep your manager happy by projecting
;; a schedule which remains valid, and also lets you (and your
;; manager) know if you're learning how to estimate your time better
;; or not.  Whether you want this feature is up to you.  Some say that
;; ignorance is bliss, others that the unexamined life...

;; Use the command `schedule-create-item' (bound to C-c s c in
;; schedule-mode) to create a new scheduling item.  It will use the
;; same nesting depth as the entry closest after point.

;; Whenever the file is saved, the time estimates will be recomputed
;; based on whatever changes have been made.  Alternatively, you can
;; call `schedule-refresh' to do that at any time.
;;
;; Note that this mode is very sensitive to the format of the file.
;; Sub-projects can only go one level deep right now.  I also don't have
;; integration with diary-mode in yet.  When that's complete, the time
;; estimator will also skip over meetings -- where you obviously don't
;; get any work done!  :)

;;; History:

;; Changes from 1.4:

;; * `schedule-week' was coming up as nil whenever the file was byte
;;   compiled.

;;; To do:

;; * If `schedule-integrate-timeclock' is non-nil, and the user checks
;;   out while not in the .schedule buffer, use a completing-read
;;   which selects from among the task names defined in
;;   `schedule-file'.

;;; Code:

(require 'calendar)
(require 'holidays)
(require 'outline)

(defconst schedule-version "1.41"
  "This version of schedule.")

(defgroup schedule nil
  "A mode to help keep track of project schedules."
  :group 'data)

;;; User Variables:

(defcustom schedule-file "~/.schedule"
  "*The filename of the primary schedule file."
  :type 'file
  :group 'schedule)

(defcustom schedule-integrate-timeclock nil
  "*If non-nil, integrate \"timeclock\" mode, if available."
  :set (lambda (symbol value)
         (if value
             (require 'timeclock))
         (setq schedule-integrate-timeclock value))
  :type 'boolean
  :group 'schedule
  :link '(url-link "http://oneworld.new-era.com/johnw/timeclock.el"))

(defcustom schedule-use-group-for-project t
  "*If non-nil, for timeclock, use the group name, not the task name."
  :type 'boolean
  :group 'schedule)

(defcustom schedule-load-hook nil
  "*Hook that gets run after schedule has been loaded."
  :type 'hook
  :group 'schedule)

(defcustom schedule-mode-hook nil
  "*A series of function to be run upon entering schedule mode."
  :type 'hook
  :group 'schedule)

(defvar schedule-workday-length nil
  "The length of a typical workday in seconds.
This variable is computed whenever `schedule-typical-workday' is set.")

(defun schedule-duration-to-seconds (code)
  "Convert the given CODE into a integer quantity of seconds."
  (if (string-match "\\([0-9.]+\\)\\([smhdw]\\)" code)
      (let ((amount (string-to-number (match-string 1 code)))
            (kind (match-string 2 code)))
        (cond ((equal kind "s")
               amount)
              ((equal kind "m")
               (* amount 60))
              ((equal kind "h")
               (* amount 3600))
              ((equal kind "d")
               (* amount (schedule-workday-length)))
              ((equal kind "w")
               (* amount (schedule-week-length)))
              (t
               (error "Invalid duration code"))))
    (error "Invalid duration code")))

(defcustom schedule-typical-workday "8h"
  "*A duration string representing the length of a typical workday.
Although each day of the way can have its own working length (see
`schedule-week'), the value of this variable is used as the multiplier
whenever a duration string uses the \"d\" code, such as \"4d\".  Thus,
the value of this variable should represent the length of a typical
workday, as it will relate to time estimates.

Note: If timeclock integration is being used, `timeclock-workday' will
override the value of this variable."
  :set (lambda (symbol value)
         (setq schedule-workday-length
               (schedule-duration-to-seconds value)
               schedule-typical-workday value))
  :type 'string
  :group 'schedule)

(defun schedule-workday-length ()
  "Use `timeclock-workday', if present."
  (or (and schedule-integrate-timeclock
           (boundp 'timeclock-workday)
           timeclock-workday)
      schedule-workday-length
      (setq schedule-workday-length
            (schedule-duration-to-seconds schedule-typical-workday))))

(defvar schedule-week-length nil
  "The length of the week in seconds.
This variable is computed whenever `schedule-week' is set.")

(defun schedule-compute-week-length ()
  "Compute the length of a based, based on `schedule-week'."
  (apply
   '+ (mapcar
       (function
        (lambda (entry)
          (or (and entry
                   (schedule-duration-to-seconds
                    (cadr entry)))
              0)))
       schedule-week)))

(defun schedule-week-length ()
  "Determine the length of a week.
This function only exists in the case the user doesn't have proper
defcustom support, and the week-length hasn't been computed yet."
  (or schedule-week-length
      (setq schedule-week-length
            (schedule-compute-week-length))))

(defcustom schedule-week
  '(nil
    ("9:00a" "8h")
    ("9:00a" "8h")
    ("9:00a" "8h")
    ("9:00a" "8h")
    ("9:00a" "8h")
    nil)
  "*A description of what your regular week looks like.
The list should be seven elements long, the first entry being for
Sunday.  Each entry is either nil, or a list of the form (TIME-IN
DURATION).  TIME-IN should be a string of the form \"11:30a\",
and DURATION a duration string, such as \"8h\"."
  :set (lambda (symbol value)
         (setq schedule-week value
               schedule-week-length (schedule-compute-week-length))
         schedule-week)
  :type '(repeat (choice (const :tag "No work" nil)
                         (list :tag "Workday"
                               (string :tag "Time in")
                               (string :tag "Duration"))))
  :group 'schedule)

(defcustom schedule-time-left-regexp
  '("\\s-+\\[\\(.*\\)\\]" 1)
  "*A regexp which gets updated with the time left for each task.
If this regexp is found anywhere in the description text for a task,
it will be updated with the time left for that task each time the
schedule is refreshed.  Marking the task as done via `timeclock-out'
(if timeclock is available) will cause the entire text string
specified by this regexp to be deleted (since it's no longer needed).

Similar to `schedule-regexp', this variable should be a list comprised
of a regular expression string, and a parenthesis group index to
indicate the text to be updated."
  :type '(list regexp (set integer))
  :group 'schedule)

(defcustom schedule-regexp
  (list (concat "\\s-*\\(\\+\\)?\\(\\(\\[\\|(\\)"
                "[A-Za-z0-9]*\\(\\]\\|)\\)\\)"
                "\\(\\s-*[0-9.]+[hdw]\\)"
                "\\(\\s-*[0-9]+\\)%"
                "\\s-*\\(.*\\)")
        '(1 2 5 6 7))
  "*A regexp for matching lines within a schedule file.
The first element of this list should be the regexp to match with.
The second identifies the meaningful parenthesis groups within the
regular expression, in this order:

  (HEADING-P DATE DURATION PERCENT-COMPLETE DESCRIPTION)

Where HEADING-P is a non-empty string if the line identifies a project
heading (rather than a regular schedule entry).  DATE is the
modifiable section of the line where date calculations should be
updated.  DURATION is the time code representing the length of time it
is estimated that this task will take.  And PERCENT-COMPLETE states
how much of the task has been completed so far.

Note that DATE should include the delimiter text.  Parentheses are
used for tasks that have not yet been completed, while square brackets
are used for completed entries.  This allows PERCENT-COMPLETE to be
above or below 100%, and yet the task can still be marked as complete
\(this becomes significant when tracking the variance from projected
durations in the schedule).

DESCRIPTION is the group which identifies the textual description of
the task/project name.

Finally, if `outline-mode' is the major mode, then the
`outline-regexp' string will be expected at the beginning of each
line.  If `outline-mode' is not being used as the major mode, project
groups will be ignored, even if HEADING-P identifies a non-empty
string."
  :type '(list regexp
               (list (integer :tag "Heading-p group")
                     (integer :tag "Date group")
                     (integer :tag "Duration group")
                     (integer :tag "Percent complete group")
                     (integer :tag "Description group")))
  :group 'schedule)

(defcustom schedule-diary-period nil
  "*How many days at a time we should look through the diary.
If you have lots of repeating appointments, things may go faster if
you decrease this number.  If this variable is set to nil, the diary
won't be consulted."
  :set (lambda (symbol value)
         (and value
              (require 'diary-lib))
         (setq schedule-diary-period value))
  :type '(choice (integer :tag "Quantum of days to examine")
                 (const :tag "Don't consult diary" nil))
  :group 'schedule)

;;; Internal Variables:

(defvar schedule-day-remainder nil
  "The number of seconds remaining today.
Used in calculations only.")

(defvar schedule-diary-entries nil
  "A list of diary entries in a period.")

(defvar schedule-diary-entries-begin nil
  "The time of the beginning of `schedule-diary-entries'.")

;;; User Functions:

;;;###autoload
(defun schedule-refresh (&optional no-message)
  "Refresh the current schedule, computing projected completion dates.
Projections are done based on the amount of time available each day,
the number of days a week you plan to work, and factoring out any
holidays that have been recorded in the Emacs calendar.  If NO-MESSAGE
is non-nil, don't print anything to the minibuffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq schedule-diary-entries nil
          schedule-diary-entries-begin nil)
    (let ((now (current-time)))
      (if (and schedule-integrate-timeclock
               (fboundp 'timeclock-workday-remaining)
               (> (timeclock-workday-remaining) 0))
          (setq schedule-day-remainder (timeclock-workday-remaining))
        ;; this initialization of `schedule-day-remainder' is
        ;; inaccurate, since we can't really compute how appointments
        ;; will effect things.  Better to use timeclock.
        (setq now (schedule-align-now now)
              schedule-day-remainder
              (cdr (schedule-nearest-workday now))))
      (let* ((regexp (schedule-determine-regexp))
             (sum (schedule-update-group
                   now (car regexp) (cadr regexp) 0)))
        (unless no-message
          (message "Schedule ends on %s, requiring %s, with %d%% done so far."
                   (nth 3 sum) (schedule-seconds-to-duration (nth 1 sum))
                   (nth 2 sum))))))
  nil)

;;;###autoload
(defun schedule-create-item (description duration &optional project)
  "Create a new schedule item before point.
DESCRIPTION is a string describing the task, and DURATION is a
schedule duration string (i.e., \"4h\", \"2d\", etc) specifying how
much time the task is expected to require.

If PROJECT is non-nil, then this task will be entered as a project
header entry, rather than an individual task.  The duration quantity
in that case will be overwritten the next time the schedule is
refreshed."
  (interactive "sDescription of task: \nsTime required: ")
  (let ((regexp (schedule-determine-regexp)) depth)
    (save-excursion
      (beginning-of-line)
      (when (eq major-mode 'outline-mode)
        (save-excursion
          (if (re-search-forward (car regexp) nil t)
              (setq depth (match-string 1)))))
      (if depth
          (insert depth " "))
      (if project
          (insert "+"))
      (insert (format "() %4s   0%%  %s\n" duration description)))
    (schedule-refresh)))

;;; Internal Functions:

(defun schedule-determine-regexp ()
  "Determine the regexp used to find schedule entries."
  (let ((regexp (car schedule-regexp))
        (paren-groups (cadr schedule-regexp)))
    ;; if we're in outline major mode, use the `outline-regexp' to
    ;; differentiate task items at different levels; also, since the
    ;; outline regexp is being recorded in a parenthesis group, we
    ;; need to increase all the `paren-groups' by 1
    (if (eq major-mode 'outline-mode)
        (setq regexp
              (concat "^\\(" outline-regexp "\\)" regexp)
              paren-groups (mapcar '1+ paren-groups)))
    (list regexp paren-groups)))
      
(defun schedule-time-to-seconds (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defun schedule-seconds-to-time (seconds)
  "Convert SECONDS (a floating point number) to an Emacs time structure."
  (list (floor seconds 65536)
	(floor (mod seconds 65536))
	(floor (* (- seconds (ffloor seconds)) 1000000))))

(defun schedule-seconds-to-duration (seconds)
  "Convert SECONDS to a compact time string."
  (let ((daylen (schedule-workday-length)))
    (cond ((< seconds 60)
           (format "%ds" seconds))
          ((< seconds 3600)
           (format "%.1fm" (/ (float seconds) 60.0)))
          ((< seconds daylen)
           (format "%.1fh" (/ (float seconds) 3600.0)))
          ((< seconds (schedule-week-length))
           (format "%.1fd" (/ (float seconds) daylen)))
          (t
           (format "%.1fw"
                   (/ (float seconds) (schedule-week-length)))))))

(defun schedule-time-date (then)
  "Return the DATE part of THEN, in calendar friendly format."
  (let* ((elems (decode-time then)))
    (list (nth 4 elems)
          (nth 3 elems)
          (nth 5 elems))))

(defun schedule-day-begin (then)
  "Given a time THEN, return the beginning time and length for that date."
  (let* ((elems (decode-time then))
         (dow (nth 6 elems))
         (today (nth dow schedule-week)))
    (if (not today)
        (cons then 0)
      (if (string-match "^\\([0-9]+\\):\\([0-9]+\\)\\([ap]\\)m?$" (car today))
          (let ((hour (string-to-number (match-string 1 (car today))))
                (min (string-to-number (match-string 2 (car today))))
                (ampm (match-string 3 (car today))))
            (if (and (= hour 12) (string= ampm "a"))
                (setq hour 0))
            (if (and (< hour 12) (string= ampm "p"))
                (setq hour (+ hour 12)))
            (cons (encode-time 0 min hour
                               (nth 3 elems) (nth 4 elems)
                               (nth 5 elems) (nth 8 elems))
                  (schedule-duration-to-seconds (cadr today))))
        (cons then 0)))))

(defsubst schedule-advance-day (then &optional count)
  "Given a time THEN, advance it by COUNT days."
  (schedule-seconds-to-time
   (+ (schedule-time-to-seconds then)
      (* 86400 (or count 1)))))

;; This is from "cal-desk-calendar.el".  It should be part of Emacs.
(defun schedule-diary-entry-times (s)
  "List of times at beginning of string S in military-style integers.
For example, returns 1325 for 1:25pm.  Returns -9999 if no time is
recognized.  The recognized forms are XXXX or X:XX or XX:XX (military
time), XXam or XXpm, and XX:XXam or XX:XXpm.  If a range is given, the
list contains two elements which will be the start and end of the
range.  If only one time is given, both elements of the list will
contain the time given."
  (cond
   ;; Hour and minute range XX:XX-XX:XX[ap]m
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)-\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>"
     s)
    (list
     (+ (* 100 (% (string-to-int
		   (substring s (match-beginning 1) (match-end 1)))
		  12))
	(string-to-int (substring s (match-beginning 2) (match-end 2)))
	(if (string-equal "a"
			  (substring s (match-beginning 5) (match-end 5)))
	    0 1200))
     (+ (* 100 (% (string-to-int
		   (substring s (match-beginning 3) (match-end 3)))
		  12))
	(string-to-int (substring s (match-beginning 4) (match-end 4)))
	(if (string-equal "a"
			  (substring s (match-beginning 5) (match-end 5)))
	    0 1200))
     (substring s (+ 2 (match-end 5)))))

   ;; Military time range
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)-\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)\\(\\|[^ap]\\)"
     s)
    (list
     (+ (* 100 (string-to-int
                (substring s (match-beginning 1) (match-end 1))))
        (string-to-int (substring s (match-beginning 2) (match-end 2))))
     (+ (* 100 (string-to-int
                (substring s (match-beginning 3) (match-end 3))))
        (string-to-int (substring s (match-beginning 4) (match-end 4))))
     (substring s (1+ (match-end 4)))))

   ;; Hour range HH[ap]m-HH[ap]m
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\)\\([ap]\\)m-\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" s)
    (list
     (+ (* 100 (% (string-to-int
                   (substring s (match-beginning 1) (match-end 1)))
                  12))
        (if (string-equal "a"
                          (substring s (match-beginning 2) (match-end 2)))
            0 1200))
     (+ (* 100 (% (string-to-int
                   (substring s (match-beginning 3) (match-end 3)))
                  12))
        (if (string-equal "a"
                          (substring s (match-beginning 4) (match-end 4)))
            0 1200))
     (substring s (+ 2 (match-end 4)))))

   ;; Hour range HH-HH[ap]m
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\)-\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" s)
    (list
     (+ (* 100 (% (string-to-int
                   (substring s (match-beginning 1) (match-end 1)))
                  12))
        (if (string-equal "a"
                          (substring s (match-beginning 3) (match-end 3)))
            0 1200))
     (+ (* 100 (% (string-to-int
                   (substring s (match-beginning 2) (match-end 2)))
                  12))
        (if (string-equal "a"
                          (substring s (match-beginning 3) (match-end 3)))
            0 1200))
     (substring s (+ 2 (match-end 3)))))

   ;; Hour and minute range XX:XX[ap]m-XX:XX[ap]m
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m-\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>"
     s)
    (list
     (+ (* 100 (% (string-to-int
		   (substring s (match-beginning 1) (match-end 1)))
		  12))
	(string-to-int (substring s (match-beginning 2) (match-end 2)))
	(if (string-equal "a"
			  (substring s (match-beginning 3) (match-end 3)))
	    0 1200))
     (+ (* 100 (% (string-to-int
		   (substring s (match-beginning 4) (match-end 4)))
		  12))
	(string-to-int (substring s (match-beginning 5) (match-end 5)))
	(if (string-equal "a"
			  (substring s (match-beginning 6) (match-end 6)))
	    0 1200))
     (substring s (+ 2 (match-end 6)))))

   ;; Military time
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)\\(\\>\\|[^ap]\\)" s)
    (let ((time (+ (* 100 (string-to-int
			   (substring s (match-beginning 1)
                                      (match-end 1))))
		   (string-to-int (substring s (match-beginning 2)
                                             (match-end 2))))))
      (list time time (substring s (1+ (match-end 2))))))

   ;; Hour only XXam or XXpm
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" s)
    (let ((time (+ (* 100 (% (string-to-int
			      (substring s (match-beginning 1) (match-end 1)))
			     12))
		   (if (string-equal
                        "a" (substring s (match-beginning 2) (match-end 2)))
		       0 1200))))
      (list time time (substring s (+ 2 (match-end 2))))))

   ;; Hour and minute XX:XXam or XX:XXpm
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>" s)
    (let ((time (+ (* 100 (% (string-to-int
			      (substring s (match-beginning 1)
                                         (match-end 1)))
			     12))
		   (string-to-int (substring s (match-beginning 2)
                                             (match-end 2)))
		   (if (string-equal
                        "a" (substring s (match-beginning 3) (match-end 3)))
		       0 1200))))
      (list time time (substring s (+ 2 (match-end 3))))))

   ;; Sunrise/sunset produced by %%(diary-sunrise-sunset)
   ((string-match
     "^[ 	]*Sunrise \\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\> ([A-Za-z0-9+-]*), sunset \\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\> ([A-Za-z0-9+-]*)\\(.*\\)" s)
    (let ((sunrise-time
           (+ (* 100 (% (string-to-int
                         (substring s (match-beginning 1) (match-end 1)))
                        12))
              (string-to-int (substring s (match-beginning 2) (match-end 2)))
              (if (string-equal "a"
                                (substring s (match-beginning 3) (match-end 3)))
                  0 1200)))
	  (sunset-time
           (+ (* 100 (% (string-to-int
                         (substring s (match-beginning 4) (match-end 4)))
                        12))
              (string-to-int (substring s (match-beginning 5) (match-end 5)))
              (if (string-equal "a"
                                (substring s (match-beginning 6) (match-end 6)))
                  0 1200))))
      (list sunrise-time sunrise-time
            (concat "Sunrise "
                    (substring s (match-beginning 1) (match-end 2)) "am"
                    (substring s (1+ (match-end 6)))
                    (substring s (match-beginning 7) (match-end 7)))
	    sunset-time sunset-time
            (concat "Sunset "
                    (substring s (match-beginning 4) (match-end 5)) "pm"
                    (substring s (1+ (match-end 6)))
                    (substring s (match-beginning 7) (match-end 7))))))

   ;; Lunar phase produced by %%(diary-phases-of-moon)
   ((string-match
     "^[ 	]*\\(New\\|First Quarter\\|Full\\|Last Quarter\\) Moon \\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\> ([A-Z0-9+-]*)" s)
    (let ((time
           (+ (* 100 (% (string-to-int
                         (substring s (match-beginning 2) (match-end 2)))
                        12))
              (string-to-int (substring s (match-beginning 3) (match-end 3)))
              (if (string-equal
                   "a" (substring s (match-beginning 4) (match-end 4)))
                  0 1200))))
      (list time time s)))

   ;; Equinox/Solstice produced by %%(diary-equinoxes-solstices)
   ((string-match
     "^[ 	]*\\(Vernal Equinox\\|Summer Solstice\\|Autumnal Equinox\\|Winter Solstice\\) \\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\> ([A-Z0-9+-]*)" s)
    (let ((time
           (+ (* 100 (% (string-to-int
                         (substring s (match-beginning 2) (match-end 2)))
                        12))
              (string-to-int (substring s (match-beginning 3) (match-end 3)))
              (if (string-equal
                   "a" (substring s (match-beginning 4) (match-end 4)))
                  0 1200))))
      (list time time s)))

   ;; Unrecognizable
   (t (list -9999 -9999 s))))

(defun schedule-get-diary-entries (then)
  "Find if there are any diary entries occurring THEN (a time value).
Return the amount of time they are scheduled to consume."
  (let ((then-date (schedule-time-date then))
        (diff (and schedule-diary-entries-begin
                   (truncate
                    (/ (- (schedule-time-to-seconds then)
                          (schedule-time-to-seconds
                           schedule-diary-entries-begin)) 86400)))))
    (if (or (not schedule-diary-entries)
            (> diff schedule-diary-period))
        (let ((display-hook diary-display-hook))
          (unwind-protect
              (save-window-excursion
                (setq diary-display-hook nil
                      schedule-diary-entries
                      (list-diary-entries then-date
                                          schedule-diary-period)
                      schedule-diary-entries-begin then))
            (setq diary-display-hook display-hook))))
    (let ((entry schedule-diary-entries) (length 0))
      (while entry
        (let ((date (caar entry)))
          (if (and (= (nth 0 date) (nth 0 then-date))
                   (= (nth 1 date) (nth 1 then-date))
                   (= (nth 2 date) (nth 2 then-date)))
              (let* ((times (schedule-diary-entry-times
                             (cadr (car entry))))
                     (first (car times))
                     (last (cadr times)))
                (setq first (* (+ (* (/ first 100) 60)
                                  (% first 100)) 60)
                      last  (* (+ (* (/ last 100) 60)
                                  (% last 100)) 60))
                (setq length (+ length (- last first))))))
        (setq entry (cdr entry)))
      length)))

(defun schedule-nearest-workday (then)
  "Given a time THEN, find the nearest workday."
  (let ((max 8) entry)
    (while (and (> max 0)
                (setq entry (schedule-day-begin then))
                (or (not entry) (= (cdr entry) 0)))
      (setq then (schedule-advance-day then)
            max (1- max)))
    (if (= max 0)
        (error "There are is no work time defined during the week"))
    (and schedule-diary-period
         (setcdr entry (- (cdr entry)
                          (schedule-get-diary-entries then))))
    entry))

(defun schedule-nearest-true-workday (then)
  "Given a time THEN, find the nearest real workday (not a holiday)."
  (let ((max 365) entry)
    (while (and (> max 0)
                (setq entry (schedule-nearest-workday then))
                (check-calendar-holidays
                 (schedule-time-date (car entry))))
      (setq then (car entry)
            then (schedule-advance-day then)
            max (1- max)))
    (if (= max 0)
        (error "There is no time available for at least a year"))
    entry))
  
(defun schedule-time-less-p (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defun schedule-align-now (then)
  "Given a time THEN, move it ahead to the next valid moment."
  (let ((day (schedule-nearest-true-workday then)))
    (if (schedule-time-less-p then (car day))
        (car day)
      (if (> (- (schedule-time-to-seconds then)
                (schedule-time-to-seconds (car day)))
             (cdr day))
          (car (schedule-nearest-true-workday
                (schedule-advance-day then)))
        then))))

(defun schedule-advance-seconds (then count)
  "Advance THEN by COUNT seconds, skipping the weekends and holidays.
THEN must not already be in a holiday or non-worktime.  Make sure that
`schedule-align-now' is called at least once before this function ever
gets called."
  (while (> count 0)
    (if (< count schedule-day-remainder)
        (setq then (schedule-seconds-to-time
                    (+ (schedule-time-to-seconds then)
                       count))
              schedule-day-remainder
              (- schedule-day-remainder
                 count)
              count 0)
      (setq count (- count schedule-day-remainder)
            then (schedule-align-now (schedule-advance-day then))
            schedule-day-remainder
            (cdr (schedule-nearest-workday then)))))
  then)

(defun schedule-update-group (begin-time regexp groups depth)
  "Refresh the current schedule project after point.
BEGIN-TIME specifies the seconds-past-the-epoch moment after which the
estimates encountered will be projected.  REGEXP is the regular
expression used to find tasks, and GROUPS is a list of integers
identifying parenthesis groups within REGEXP that are significant.
See `schedule-regexp' for more information.

DEPTH is used to track recursion, and should always initially be zero.

This function returns the computed totals for that project in a list
of the form:

  (COUNT SECONDS COMPLETE FINAL-DATE UNDONE-COUNT NEW-TIME TIME-LEFT)

COUNT is the number of tasks in the project.  SECONDS is the total
number of seconds predicted that the project will take (it needn't
actually use this degree of resolution, but it's provided
nonetheless).  COMPLETE is the percentage complete, based on the
seconds worked so far, and the predicted duration.  FINAL-DATE is the
estimated date on which work will be completed.  UNDONE-COUNT is the
number of tasks that have yet to be completed for the project.
NEW-TIME is the next available working time, which is essentially
BEGIN-TIME plus the amount of time necessary for all the tasks in this
project.  TIME-LEFT is the computed (and adjusted) amount of time left
on this project.  If the users embeds strings watched for by
`schedule-time-left-regexp', and the task has exceeded 100% complete,
then this is only way to record the amount of time that is now
believed to be left, and yet still track how far over estimates the
project ended up being."
  (let ((now begin-time)
        (seconds 0)
        (complete 0)
        (count 0)
        (undone 0)
        (time-left 0)
        final-date level begin)
    (while
        (and (setq begin (point))
             (re-search-forward regexp nil t)
             (not (and (eq major-mode 'outline-mode)
                       (<= (setq level (length (match-string 1)))
                           depth)
                       (goto-char begin))))
      (let* ((heading-p (match-string (nth 0 groups)))
             (date-beg
              (copy-marker (match-beginning (nth 1 groups))))
             (date-end
              (copy-marker (match-end (nth 1 groups)))))

        (if (and heading-p level)
            (let* ((d-beg (match-beginning (nth 2 groups)))
                   (d-end (match-end (nth 2 groups)))
                   (c-beg (match-beginning (nth 3 groups)))
                   (c-end (match-end (nth 3 groups)))
                   (project (schedule-update-group
                             now regexp groups level)))
              (if (> (nth 0 project) 0)
                  (save-excursion
                    (goto-char c-beg)
                    (delete-region c-beg c-end)
                    (insert " " (format "%3.f" (nth 2 project)))

                    (goto-char d-beg)
                    (delete-region d-beg d-end)
                    (insert " " (format "%4s"
                                        (schedule-seconds-to-duration
                                         (nth 1 project))))

                    (goto-char date-beg)
                    (delete-region date-beg date-end)
                    (if (= (nth 4 project) 0)
                        (progn
                          (insert "[Ended]")
                          (schedule-update-time-left nil nil))
                      (insert "(" (nth 3 project) ")")
                      (schedule-update-time-left
                       (schedule-seconds-to-duration (nth 6 project)) nil))

                    (setq now        (nth 5 project)
                          seconds    (+ seconds (nth 1 project))
                          complete   (+ complete (nth 2 project))
                          count      (+ count (nth 0 project))
                          undone     (+ undone (nth 4 project))
                          time-left  (+ time-left (nth 6 project))
                          final-date (nth 3 project)))))
          (let* ((duration
                  (match-string (nth 2 groups)))
                 (task-complete
                  (float (string-to-number
                          (match-string (nth 3 groups)))))
                 (task-seconds (schedule-duration-to-seconds duration))
                 (prev-seconds task-seconds))
                
            (setq seconds  (+ seconds task-seconds)
                  complete (+ complete task-complete)
                  count    (1+ count))
            
            ;; if the date of the task is enclosed in square
            ;; brackets, it means that the task has been completed,
            ;; independent of the percentage complete figure
            (goto-char date-beg)
            (unless (looking-at "\\[")
              (setq undone (1+ undone))
              
              (if (> task-complete 0.0)
                  (setq task-seconds
                        (- task-seconds
                           (* task-seconds
                              (/ task-complete 100.0)))))

              (setq task-seconds (schedule-update-time-left
                                task-seconds task-complete)
                    time-left  (+ time-left task-seconds))

              (setq now (schedule-advance-seconds now task-seconds))
          
              (let ((then (format-time-string "%b%d" now)))
                (delete-region date-beg date-end)
                (insert "(" then ")")
                (setq final-date then)))

            (end-of-line)))))

    (list count seconds
          (if (> count 0)
              (/ complete count)
            complete)
          final-date undone now time-left)))

(defun schedule-update-time-left (seconds complete)
  "Update the time left string for the task under point.
SECONDS is the amount of time that has be applied toward the task, and
COMPLETE is the percentage complete metric for the task."
  (when schedule-time-left-regexp
    (save-excursion
      (let ((groups (schedule-task-under-point-p)))
        (when groups
          (goto-char (match-end (nth 3 groups)))
           (let ((eol (save-excursion (end-of-line) (point))))
            (if (re-search-forward
                 (car schedule-time-left-regexp) eol t)
                (if (not seconds)
                    (delete-region (match-beginning 0)
                                   (match-end 0))
                  (let ((b (match-beginning
                            (cadr schedule-time-left-regexp)))
                        (e (match-end
                            (cadr schedule-time-left-regexp))))
                    (if (and complete (>= complete 100.0))
                        (let ((str (buffer-substring b e)))
                          (if (string-match "^[0-9.]+[smhdw]$" str)
                              (setq seconds
                                    (schedule-duration-to-seconds str))))
                      (goto-char b)
                      (delete-region b e)
                      (if complete
                          (insert (schedule-seconds-to-duration seconds))
                        (insert seconds)))))))))))
  (and (numberp seconds) (max seconds 0)))

(defvar schedule-mode nil
  "Non-nil if using schedule mode as a minor mode of some other mode.")

(make-variable-buffer-local 'schedule-mode)
(or (assq 'schedule-mode minor-mode-alist)
    (setq minor-mode-alist
          (append minor-mode-alist
                  (list '(schedule-mode " Schd")))))

(defun schedule-task-under-point-p ()
  "Return non-nil if there is a task under point.
The return value is the set of parenthesis groups that identify the
contents of the task."
  (let ((regexp (schedule-determine-regexp)))
    (save-excursion
      (beginning-of-line)
      (and (looking-at (car regexp))
           (cadr regexp)))))
  
(defun schedule-project-name-at-point ()
  "Find the project name to use for clocking out from the schedule."
  (when (assq 'schedule-mode minor-mode-alist)
    (let ((groups (schedule-task-under-point-p))
          task-name)
      (if (not groups)
          ""
        (setq task-name (match-string-no-properties (nth 4 groups)))
        (if (and schedule-use-group-for-project
                 (not (string= (match-string (nth 0 groups)) "+")))
          (let (stop)
            (save-excursion
              (while (and (not stop)
                          (not (string= (match-string (nth 0 groups)) "+")))
                (if (bobp)
                    (setq stop t)
                  (forward-line -1)
                  (schedule-task-under-point-p))))
            (unless stop
              (setq task-name
                    (match-string-no-properties(nth 4 groups))))))
        (if (string-match (car schedule-time-left-regexp) task-name)
            (setq task-name (replace-match "" t t task-name)))
        task-name))))

(defun schedule-update-task (&optional seconds)
  "Update the current task to reflect that it has been worked on.
If SECONDS is non-nil, it represents the number of seconds that have
been applied toward the task.  If it is nil, the function
`timeclock-last-period' will be called to determine how much time was
worked."
  (interactive
   (list (schedule-duration-to-seconds
             (read-string "Amount of time applied to task (ex. 4h, 1d): "))))
  (let ((groups (schedule-task-under-point-p)))
    (if groups
        (let* ((duration
                (match-string (nth 2 groups)))
               (task-complete
                (float (string-to-number
                        (match-string (nth 3 groups)))))
               (prev-complete task-complete)
               (c-beg (match-beginning (nth 3 groups)))
               (c-end (match-end (nth 3 groups)))
               (task-seconds (float (schedule-duration-to-seconds duration)))
               (seconds-so-far
                (if (> task-complete 0.0)
                    (* task-seconds
                       (/ task-complete 100.0))
                  0.0))
               (period (or seconds (timeclock-last-period)))
               (seconds-spent period))
          (setq seconds-so-far (+ seconds-so-far seconds-spent)
                task-complete (* (/ seconds-so-far task-seconds) 100.0))
          (save-excursion
            (goto-char c-beg)
            (delete-region c-beg c-end)
            (insert " " (format "%3.f" task-complete)))
          (if (and (< prev-complete 100.0)
                   (>= task-complete 100.0))
              (schedule-update-time-left "???" nil)
            (if (> task-complete 0.0)
                (setq task-seconds
                      (- task-seconds
                         (* task-seconds
                            (/ task-complete 100.0)))))
            (setq task-seconds
                  (schedule-update-time-left task-seconds
                                             task-complete))
            (when (>= task-complete 100.0)
              (setq task-seconds (- task-seconds seconds-spent))
              (schedule-update-time-left
               (if (<= task-seconds 0)
                   "???"
                 (schedule-seconds-to-duration task-seconds)) nil)))
          (schedule-refresh)))))

(defun schedule-complete-task ()
  "Mark the current task as completed today.
If it's already been completed, just update the date string."
  (let ((groups (schedule-task-under-point-p)))
    (if groups
        (let ((date-beg (match-beginning (nth 1 groups)))
              (date-end (match-end (nth 1 groups)))
              (then (format-time-string "%b%d" (current-time))))
          (save-excursion
            (goto-char date-beg)
            (delete-region date-beg date-end)
            (insert "[" then "]")
            (schedule-update-time-left nil nil))))))

(defvar schedule-mode-map ())
(if schedule-mode-map
    ()
  (setq schedule-mode-map (make-sparse-keymap))
  (define-key schedule-mode-map "\C-csc" 'schedule-create-item)
  (define-key schedule-mode-map "\C-csr" 'schedule-refresh))

(or (assq 'schedule-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'schedule-mode schedule-mode-map)
		minor-mode-map-alist)))

;;;###autoload
(defun schedule-mode (&optional arg)
  "Toggle schedule mode.
With ARG, turn schedule mode on if arg is positive, off otherwise.
\\<schedule-mode-map>A schedule is an outline with certain special
entries, whose contents represent progress toward a scheduled goal.

\\{schedule-mode-map}"
  (interactive "P")
  (setq schedule-mode
	(if (null arg) (not schedule-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if schedule-mode
      (progn
	;; turn off this mode if we change major modes.
	(make-local-hook 'change-major-mode-hook)
	(add-hook 'change-major-mode-hook
		  (function (lambda ()
                              (schedule-mode -1))
                            nil t))
        
        ;; make sure to always refresh schedule before writing to disk
        (add-hook 'local-write-file-hooks 'schedule-refresh)
        
        ;; tie in with timeclock.el, if desired and available
        (when schedule-integrate-timeclock
          (make-local-variable 'timeclock-get-project-function)
          (setq timeclock-get-project-function
                'schedule-project-name-at-point)

          ;; when clocking in for the first time each day, make sure
          ;; to entry any irregular daily time periods into the
          ;; timelog file
          (make-local-variable 'timeclock-get-workday-function)
          (setq timeclock-get-workday-function
                (function
                 (lambda ()
                   (let* ((now (current-time))
                          (day (schedule-day-begin now)))
                     (if (or (= (cdr day) 0)
                             (check-calendar-holidays
                              (schedule-time-date now)))
                         0
                       (cdr day))))))
        
          (make-local-hook 'timeclock-out-hook)
          (add-hook 'timeclock-out-hook 'schedule-update-task)
        
          (make-local-hook 'timeclock-done-hook)
          (add-hook 'timeclock-done-hook 'schedule-complete-task))
         
	(run-hooks 'schedule-mode-hook))
    (remove-hook 'local-write-file-hooks 'schedule-refresh))
  (force-mode-line-update))

(provide 'schedule)

(run-hooks 'schedule-load-hook)

;;; schedule.el ends here
