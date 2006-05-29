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

(provide 'calendar-config)
;;; calendar-config.el ends here
