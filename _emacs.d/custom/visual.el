;;; visual.el --- visual settings

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

;;;;;;;;;;;;
;; Visuals
;;

;; New faces
(defvar font-lock-escape-char-face 'font-lock-escape-char-face)
(defvar font-lock-number-face 'font-lock-number-face)
(defvar font-lock-hexnumber-face 'font-lock-hexnumber-face)
(defvar font-lock-floatnumber-face 'font-lock-floatnumber-face)
(defvar font-lock-keys-face 'font-lock-keys-face)
(defvar font-lock-qt-face 'font-lock-qt-face)

(defface font-lock-escape-char-face
  '((((class color)) (:foreground "seagreen2")))
  "highlight c escapes char like vim"
  :group 'font-lock-faces)

(defface font-lock-number-face
  '(
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:background "hotpink"))
    (((class color) (background dark)) (:foreground "yellow3" :background "hotpink"))
    (t (:italic t)))
  "blah"
  :group 'font-lock-highlighting-faces)

(defface font-lock-hexnumber-face
  '(
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:background "darkblue"))
    (((class color) (background dark)) (:foreground "black" :background "darkblue"))
    (t (:italic t)))
  "blah"
  :group 'font-lock-highlighting-faces)

(defface font-lock-floatnumber-face
  '(
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:background "darkgreen"))
    (((class color) (background dark)) (:foreground "black" :background "darkgreen"))
    (t (:italic t)))
  "blah"
  :group 'font-lock-highlighting-faces)

(defface font-lock-keys-face
  '(
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:background "yellow"))
    (((class color) (background dark)) (:foreground "yellow"))
    (t (:italic t)))
  "blah"
  :group 'font-lock-highlighting-faces)

(defface font-lock-qt-face
  '(
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:background "brown"))
    (((class color) (background dark)) (:foreground "green" :background "brown"))
    (t (:italic t)))
  "blah"
  :group 'font-lock-highlighting-faces)

(unless (facep 'minibuffer-prompt)
  (defvar minibuffer-face 'minibuffer-face)
  (defface minibuffer-face '((t (:bold t :foreground "LightBlue")))
    "Face used to color the minibuffer."
    :group 'font-lock-highlighting-faces))

(require 'jit-lock)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 1)

;; Frame appearence
(setq default-frame-alist '((left-fringe)
                            (right-fringe)
                            (menu-bar-lines . 0)
                            (vertical-scroll-bars)
                            (tool-bar-lines . 0)))

(setq window-system-default-frame-alist
      '((x (width . 100)
           (height . 50)
           (foreground-color . "wheat")
           (background-color . "black")
           (cursor-color . "yellow"))
        (ns (width . 100)
            (height . 50)
            (foreground-color . "wheat")
            (background-color . "black")
            (cursor-color . "yellow"))))

(if (> emacs-major-version 20) (custom-set-variables '(tool-bar-mode nil nil (tool-bar))))
(if (> emacs-major-version 20) (set-scroll-bar-mode nil))
(menu-bar-mode -1)

(setq-default even-window-heights nil
              resize-mini-windows nil)

;; Window Fringes
(require 'fringe)
(setq-default indicate-buffer-boundaries 'left) ; Indicate the
(setq-default indicate-empty-lines t)           ; Display an in
(fringe-mode 'default)

   ;;; ----------------------------------------------------------------
   ;;; list-fonts-display, Miles Bader <miles@gnu.org>

(defun list-fonts-display (&optional matching)
  "Display a list of font-families available via font-config, in a new buffer.

   If the optional argument MATCHING is non-nil, only font families
   matching that regexp are displayed; interactively, a prefix
   argument will prompt for the regexp.

   The name of each font family is displayed using that family, as
   well as in the default font (to handle the case where a font
   cannot be used to display its own name)."
  (interactive
   (list
    (and current-prefix-arg
         (read-string "Display font families matching regexp: "))))
  (let (families)
    (with-temp-buffer
      (shell-command "fc-list : family" t)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((fam (buffer-substring (line-beginning-position)
                                     (line-end-position))))
          (when (or (null matching) (string-match matching fam))
            (push fam families)))
        (forward-line)))
    (setq families
          (sort families
                (lambda (x y) (string-lessp (downcase x) (downcase y)))))
    (let ((buf (get-buffer-create "*Font Families*")))
      (with-current-buffer buf
        (erase-buffer)
        (dolist (family families)
          ;; We need to pick one of the comma-separated names to
          ;; actually use the font; choose the longest one because some
          ;; fonts have ambiguous general names as well as specific
          ;; ones.
          (let ((family-name
                 (car (sort (split-string family ",")
                            (lambda (x y) (> (length x) (length y))))))
                (nice-family (replace-regexp-in-string "," ", " family)))
            (insert (concat (propertize nice-family
                                        'face (list :family family-name))
                            " (" nice-family ")"))
            (newline)))
        (goto-char (point-min)))
      (display-buffer buf))))

(provide 'visual)
;;; visual.el ends here
