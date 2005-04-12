;;; ediff-config.el ---

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

;;; Ediff

(eval-after-load "ediff-wind"
  '(progn
     (setq ediff-window-setup-function 'ediff-setup-windows-plain)
     (setq ediff-split-window-function 'split-window-horizontally)))

;; Some custom configuration to ediff
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")

(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")

(defcustom my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my-ediff-bwin-config (current-window-configuration))
  (set-register my-ediff-bwin-reg
                (list my-ediff-bwin-config (point-marker))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my-ediff-awin-config (current-window-configuration))
  (set-register my-ediff-awin-reg
                (list my-ediff-awin-config (point-marker))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))

(defun ediff-add-changelog (&optional key)
  (interactive)
  (with-current-buffer
      (ediff-get-buffer
       (ediff-char-to-buftype (or key last-command-char)))
    (add-change-log-entry-other-window)))

(defun yh/install-ediff-changelog-keys ()
  (define-key ediff-mode-map ".a" 'ediff-add-changelog)
  (define-key ediff-mode-map ".b" 'ediff-add-changelog)
  (define-key ediff-mode-map ".c" 'ediff-add-changelog))

(eval-after-load "ediff-init"
  '(progn
     (add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
     (add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash)
     (add-hook 'ediff-quit-hook 'my-ediff-qh)
     (add-hook 'ediff-keymap-setup-hook 'yh/install-ediff-changelog-keys)))

(provide 'ediff-config)
;;; ediff-config.el ends here
