;;; hideshow-config.el --- 

;; Copyright (C) 2009  Free Software Foundation, Inc.

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

(require 'hideshow)
(require 'patches)

(define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])

(defcustom hs-fringe-face 'hs-fringe-face
  "*Specify face used to highlight the fringe on hidden regions."
  :type 'face
  :group 'hideshow)

(defface hs-fringe-face
  '((t (:foreground "#888" :box (:line-width 2 :color "grey75" :style released-button))))
  "Face used to highlight the fringe on folded regions"
  :group 'hideshow)

(defcustom hs-face 'hs-face
  "*Specify the face to to use for the hidden region indicator."
  :type 'face
  :group 'hideshow)

(defface hs-face
  '((t (:background "#ff8" :box t)))
  "Face to hightlight the ... area of hidden regions"
  :group 'hideshow)

(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((marker-string "*fringe-dummy*")
           (marker-length (length marker-string))
           (display-string (format "(%d)..." (count-lines (overlay-start ov) (overlay-end ov)))))
      (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'hs-fringe-face) marker-string)
      (overlay-put ov 'before-string marker-string)
      (put-text-property 0 (length display-string) 'face 'hs-face display-string)
      (overlay-put ov 'display display-string)
      )))

(setq hs-set-up-overlay 'display-code-line-counts)

(defvar yh/hs-hidden nil "Current state of hideshow for toggling all.")

(defun yh/toggle-hideshow-all () 
  "Toggle hideshow all."
  (interactive)
  (if (setq yh/hs-hidden (not yh/hs-hidden))
      (hs-hide-all)
    (hs-show-all)))

(defun yh/hs-toggle-hiding-or-level (arg)
  "Toggle hiding for current block. With numeric argument, ensure
only level `arg' blocks are hidden."
  (interactive "P")
  (if arg
      (progn
        (hs-show-block)
        (hs-hide-level arg))
    (hs-toggle-hiding)))

;;; do not require being at the end of line to hideshow C-like blocks
(mapc (lambda (mode) 
        (setf (cadr (assoc mode hs-special-modes-alist)) '(".*\\({\\)" 1)))
      '(c-mode c++-mode java-mode))

;;; make sure several buffers do not fight for yh/hs-hidden
(add-hook 'hs-minor-mode-hook (lambda () (make-local-variable 'yh/hs-hidden)))

;;; fix bad (IMHO) behavior. Point should not move if possible
(defmadvice (hs-hide-block hs-show-block hs-hide-all hs-show-all) 
  (around hs-hide/show-block-at-point-around act)
  "Try not to move when hiding/showing"
  (let ((initpoint (point)))
    ad-do-it
    (unless (ad-get-arg 0)
      (goto-char initpoint))))

;;; associate decent keys for hideshow (I'm mapping the "Alt" modifier to right
;;; control, remap according to your keyboard)
(define-key hs-minor-mode-map (kbd "<A-tab>") 'yh/hs-toggle-hiding-or-level)
(define-key hs-minor-mode-map (kbd "<A-S-iso-lefttab>") 'yh/toggle-hideshow-all)

(provide 'hideshow-config)
;;; hideshow-config.el ends here
