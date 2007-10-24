;;; calendar-config.el --- Configuration for calendar

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Yann Hodique <Yann.Hodique@lifl.fr>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

;;; Calendar

(when (request 'calendar)
  (setq european-calendar-style t)
  (setq calendar-holidays nil))

(add-hook 'diary-display-hook 'fancy-diary-display)

(defun diary-cyclic-bounded (limit n month day year &optional mark)
  (let* ((d (if european-calendar-style month day))
         (m (if european-calendar-style
                day
              month))
         (diff (- (calendar-absolute-from-gregorian date)
                  (calendar-absolute-from-gregorian
                   (list m d year))))
         (cycle (/ diff n)))
    (if (and (>= diff 0)
             (zerop (% diff n))
             (< cycle limit))
        (cons mark (format entry cycle (diary-ordinal-suffix cycle))))))

;; (defun calendar-iso-week-string (&optional date)
;;   "String of ISO week number of Gregorian DATE."
;;   (format "W%02d "
;; 	  (extract-calendar-month
;; 	   (calendar-iso-from-absolute
;; 	    (calendar-absolute-from-gregorian date)))))

;; (defun generate-calendar-month (month year indent)
;;   "Produce a calendar for MONTH, YEAR on the Gregorian calendar.
;; The calendar is inserted at the top of the buffer in which point is currently
;; located, but indented INDENT spaces.  The indentation is done from the first
;; character on the line and does not disturb the first INDENT characters on the
;; line."
;;   (let* ((blank-days ;; at start of month
;;           (mod
;;            (- (calendar-day-of-week (list month 1 year))
;;               calendar-week-start-day)
;;            7))
;; 	 (last (calendar-last-day-of-month month year)))
;;     (goto-char (point-min))
;;     (calendar-insert-indented
;;      (calendar-string-spread
;;       (list (format "%s %d" (calendar-month-name month) year)) ?  20)
;;      indent t)
;;     (calendar-insert-indented "" indent) ;; Go to proper spot
;;     ;; Use the first two characters of each day to head the columns.
;;     (dotimes (i 7)
;;       (insert
;;        (let ((string
;;               (calendar-day-name (mod (+ calendar-week-start-day i) 7) nil t)))
;;          (if enable-multibyte-characters
;;              (truncate-string-to-width string 2)
;;            (substring string 0 2)))
;;        " "))

;;     ;; HACK
;;     (setq indent (- indent 4))

;;     (calendar-insert-indented "" 0 t) ;; Force onto following line

;;     ;; HACK
;;     (calendar-insert-indented  ;; Go to proper spot
;;      (calendar-iso-week-string ;; Insert ISO week number.
;;       (list
;;        month
;;        (if (and (= 0 blank-days) (= 0 calendar-week-start-day))
;;            2 ;; Use 2nd day if 1st day of calendar month is Sunday.
;;          1) year)) indent)

;;     ;; Add blank days before the first of the month
;;     (dotimes (idummy blank-days) (insert "   "))
;;     ;; Put in the days of the month
;;     (calendar-for-loop i from 1 to last do
;;                        (insert (format "%2d " i))
;;                        (add-text-properties
;;                         (- (point) 3) (1- (point))
;;                         '(mouse-face highlight
;;                                      help-echo "mouse-2: menu of operations for this date"))
;;                        (and (zerop (mod (+ i blank-days) 7))
;;                             (/= i last)
;;                             (calendar-insert-indented "" 0 t) ;; Force onto following line
;;                             (calendar-insert-indented ;; Go to proper spot
;;                              (calendar-iso-week-string ;; Insert ISO week number.
;;                               (list month (+ 2 i) year))
;;                              indent))))) ;; Go to proper spot

(provide 'calendar-config)
;;; calendar-config.el ends here
