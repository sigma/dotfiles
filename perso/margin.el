;;; margin.el --- Add a vertical margin line where text overlaps
;;                margin-column

;; Copyright (C) 2004 Walter Higgins

;; Author: Walter Higgins <walterh@rocketmail.com>
;; Maintainer: Walter Higgins <walterh@rocketmail.com>
;; Keywords: tools

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file defines a minor mode for adding a vertical bar which marks
;; a right-hand margin. This is similar to the feint 1 pixel wide right
;; margin that appears in many IDEs such as codewright. The margin is a
;; 1 character wide overlay (black text on red background) so (as with
;; glasses-mode - which is how this mode came about) the text is never
;; modified directly.
;;
;; This file defines the `margin-mode' minor mode, which displays a vertical
;; bar (not really a bar but an inverse-video overlay) at the margin
;; specified by `margin-column'. See also the variable `margin-face'.
;; You can also use the command `M-x customize-group RET margin RET'.
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup margin nil
  "Add a right-hand margin for rows exceeding length
specified by margin-column"
  :version "21.2"
  :group 'tools)

(defcustom margin-column 80
  "*Location of right-hand margin at which text will be marked"
  :group 'margin
  :type 'integer
  :set 'margin-custom-set
  :initialize 'custom-initialize-default)

(defface margin-face
  '()
  "Face for specifying the righthand margin"
  :group 'margin)

(set-face-attribute 'margin-face nil
                    :inverse-video t)

(defun make-margin-overlay-at-line (line)
  "Put a feint margin on line N"
  (interactive "NPut margin on line: ")
  (save-excursion
    (goto-line line)
    (make-margin-overlay (line-beginning-position)
                         (line-end-position))
    ))

(defun margin-overlay-on ()
  "Turn on the margin throughout entire buffer"
  (interactive)
  (let ((line-count (count-lines 1 (point-max))))
    (while (> line-count 0)
      (progn
        (make-margin-overlay-at-line line-count)
        (setq line-count (- line-count 1)))
      ) ) )
;;
;; delete all margin overlays
;;
(defun margin-overlay-off ()
  "Turn off the margin."
  (interactive)
  (dolist (o (overlays-in (point-min) (point-max)))
    (when (margin-overlay-p o)
      (delete-overlay o)))
)

(defun margin-custom-set (symbol value)
  "Set value of the variable SYMBOL to VALUE and update
overlay categories. Used in :set parameter of some customized
glasses variables."
  (set-default symbol value)
  (if margin-mode
      (progn
        (margin-overlay-off)
        (margin-overlay-on))))

(defun make-margin-overlay (start-of-line end-of-line)
  "Display a margin on a single line"
  (let ((line-length (- end-of-line start-of-line )))
    (if (and (> line-length margin-column)
             (not (= (char-after (+ start-of-line margin-column 1)) 10)))
        (let ((overlay (make-overlay (+ start-of-line margin-column 1)
                                     (+ start-of-line margin-column 2))))
          (overlay-put overlay 'category 'margin-overlay)
          (overlay-put overlay 'face 'margin-face)
          )
      ) ) )

(defun margin-overlay-p (o)
  "Is overlay of type 'margin-overlay ?"
  (memq (overlay-get o 'category) '(margin-overlay)))

(defun margin-change (beg end &optional old-len)
  "Turn on margins in visible region"
  (save-excursion
    (let ((w-start (window-start))
          (w-end (window-end)))

      (dolist (o (overlays-in w-start w-end))
        (when (margin-overlay-p o)
          (delete-overlay o)))

      (goto-char w-start)
      (while (> w-end (point))
        (let ((start-of-line (line-beginning-position))
              (end-of-line (line-end-position)))
          (make-margin-overlay start-of-line end-of-line)
          (goto-char (+ end-of-line 1))
          )
        ))))

;;;###autoload
(define-minor-mode margin-mode
  "Minor mode that displays a margin"
  nil " | " nil
  (if margin-mode
      (progn
        (margin-overlay-on)
        (jit-lock-register 'margin-change)
        (setq margin-mode t))
    ;; else
    (progn
      (margin-overlay-off)
      (jit-lock-unregister 'margin-change)
      (setq margin-mode nil))))

(provide 'margin)
