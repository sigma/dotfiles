;;; fracc-config.el ---

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

;;; Fracc : french accent mode

(require 'fracc)
(defun install-french-accent-mode-if-needed ()
  "Install French accent mode if the buffer seems to contain French text.
The guess is made by computing the proportion of letters with accents. If
there are more than 1% of such letters then turn French accent mode on."
  (save-excursion
    (goto-char (point-min))
    (let ((n 0)(size (- (point-max) (point-min))))
      (while (re-search-forward "\\\\['^`][eauo]" (point-max) t)
        (setq n (+ n 1)) )
      (while (re-search-forward "[éèàùçêë]" (point-max) t)
        (setq n (+ n 1)) )
      (message "diacritic/normal ratio = %d/%d" n size)
      (cond ((> (* n 100) size)
             (fracc-mode fracc-8bits-tex-encoding))))))

(add-hook 'tex-mode-hook 'install-french-accent-mode-if-needed)
(add-hook 'LaTeX-mode-hook 'install-french-accent-mode-if-needed)
(add-hook 'text-mode-hook 'install-french-accent-mode-if-needed)

(provide 'fracc-config)
;;; fracc-config.el ends here
