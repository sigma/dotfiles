;;; bm-config.el ---

;; Copyright (C) 2007  Free Software Foundation, Inc.

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

(setq bm-restore-repository-on-load t)

(when (request 'bm)

  (global-set-key (kbd "C-c b C-m") 'bm-toggle)
  (global-set-key (kbd "C-c b n")   'bm-next)
  (global-set-key (kbd "C-c b p") 'bm-previous)
  (global-set-key (kbd "C-c b s") 'bm-show)
  (global-set-key (kbd "C-c b a") 'bm-bookmark-annotate)

;; make bookmarks persistent as default
  (setq-default bm-buffer-persistence t)

;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when emacs is killed, so we
;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))

;; Update bookmark repository when saving the file.
  (add-hook 'after-save-hook 'bm-buffer-save)

;; Restore bookmarks when buffer is reverted.
  (add-hook 'after-revert-hook 'bm-buffer-restore)

;; make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save))

(provide 'bm-config)
;;; bm-config.el ends here
