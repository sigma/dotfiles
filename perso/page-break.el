;;; page-break.el ---

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

(defvar page-break-face 'bold)
(defvar page-break-string-char ?-)

(defun page-break-display-table ()
  "Create a display-table that displays page-breaks prettily."
  (let ((table (or (copy-sequence standard-display-table)
                   (make-display-table))))
    (aset table ?\^L
          (let ((face-offset (lsh (face-id page-break-face) 19)))
            (vconcat (mapcar (lambda (c) (+ face-offset c)) (make-string (1- (window-width))
                                                                         page-break-string-char)))))
    table))

(define-minor-mode page-break-mode
    "Toggle Page Break mode.

In Page Break mode, page breaks (^L characters) are displayed as a
horizontal line of `page-break-string-char' characters."
  nil " Pgbrk" nil
  (setq buffer-display-table (if page-break-mode
                                 (page-break-display-table)
                                 nil)))

(defun turn-on-page-break-mode ()
  (page-break-mode 1))

(defun turn-off-page-break-mode ()
  (page-break-mode -1))

(provide 'page-break)
;;; page-break.el ends here
