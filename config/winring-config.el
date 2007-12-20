;;; winring-config.el ---

;; Copyright (C) 2006  Free Software Foundation, Inc.

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

(eval-when-compile
  '(require 'winring)
  '(require 'patches))

(defun yh/winring-fix-switch ()
  (defmadvice (winring-next-configuration winring-prev-configuration)
    (around winring act)
    "Creation of new configuration if only one existing"
    (condition-case nil
        ad-do-it
      (error (winring-new-configuration t))))
  ;; for some reason, this is improper in my configuration...
  (defadvice winring-initialize (after winring-fix act)
    (remove-hook 'after-make-frame-functions 'winring-create-frame-hook)))

(eval-after-load 'winring
  '(yh/winring-fix-switch))

(defun winring-by-name (name)
  (let* ((ring (winring-get-ring))
	 (n (1- (ring-length ring)))
	 (current (winring-name-of-current))
	 (table (list (cons current -1))))
    ;; populate the completion table
    (while (<= 0 n)
      (setq table (cons (cons (winring-name-of (ring-ref ring n)) n) table)
	    n (1- n)))
    (cdr (assoc name table))))

(defun winring-select (name)
  (interactive "sWinring: ")
  (flet ((winring-complete-name () (winring-by-name name)))
    (winring-jump-to-configuration)))

(defmacro with-selected-winring (name &rest body)
  "Execute the forms in BODY with FRAME as the selected frame.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
  (declare (indent 1) (debug t))
  (let ((old-ring (make-symbol "old-ring")))
    `(let ((,old-ring (winring-name-of-current)))
       (unless (string= ,old-ring ,name)
         (unwind-protect
             (progn (winring-select ,name)
                    ,@body)
           (winring-select ,old-ring))))))

(provide 'winring-config)
;;; winring-config.el ends here
