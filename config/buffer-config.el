;;; buffer-config.el ---

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

;;; Buffers

(eval-after-load "buff-menu" '(request 'buff-menu+))
(global-set-key (kbd "C-x C-b") 'buffer-menu)

(add-hook 'dired-load-hook
          (lambda ()
            (require 'dired-aux)
            (require 'dired-x)

            ;; use ediff for diffing
            (defadvice dired-diff (around ad-dired-diff-ediff act)
              (flet ((diff (old new switches) (ediff old new)))
                ad-do-it))
            (defadvice dired-backup-diff (around ad-dired-backup-diff-ediff act)
              (flet ((diff-backup (old switches) (ediff-backup old)))
                ad-do-it))

            ;; Set dired-x global variables here.  For example:
            (setq dired-x-hands-off-my-keys nil
                  dired-find-subdir nil)

            (define-key dired-mode-map (kbd "C-c C-c") 'wdired-change-to-wdired-mode)
            ))

(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (dired-omit-mode 1)
            ))

(eval-after-load "icomplete" '(progn (request 'icomplete+)))
(when (request 'icomplete) (icomplete-mode 1))
(when (request 'iswitchb) (iswitchb-mode 1))

(provide 'buffer-config)
;;; buffer-config.el ends here
