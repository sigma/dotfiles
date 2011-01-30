;;; hl-line-config.el --- configuration for hl-line

;; Copyright (C) 2011  Free Software Foundation, Inc.

;; Author: Yann Hodique <yann.hodique@gmail.com>
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

(require 'hl-line+)

(defun hl-line-toggle-when-idle (&optional arg)
  "Turn on or off using `global-hl-line-mode' when Emacs is idle.
When on, use `global-hl-line-mode' whenever Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off."
  (interactive "P")
  (setq hl-line-when-idle-p
        (if arg (> (prefix-numeric-value arg) 0) (not hl-line-when-idle-p)))
  (cond (hl-line-when-idle-p
         (timer-activate-when-idle hl-line-idle-timer)
         (message "Turned ON using `global-hl-line-mode' when Emacs is idle."))
        (t
         (cancel-timer hl-line-idle-timer)
         (message "Turned OFF using `global-hl-line-mode' when
         Emacs is idle."))))

(defun hl-line-highlight-now ()
  "Turn on `global-hl-line-mode' and highlight current line now."
  (unless global-hl-line-mode
    (global-hl-line-mode 1)
    (global-hl-line-highlight)
    (add-hook 'pre-command-hook 'hl-line-unhighlight-now)
    ))

(defun hl-line-unhighlight-now ()
  "Turn off `global-hl-line-mode' and unhighlight current line now."
  (global-hl-line-mode -1)
  (global-hl-line-unhighlight)
  (remove-hook 'pre-command-hook 'hl-line-unhighlight-now))

(toggle-hl-line-when-idle 1)
(hl-line-when-idle-interval 1)

(provide 'hl-line-config)
;;; hl-line-config.el ends here
