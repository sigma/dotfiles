;;; org-config.el --- config for org-mode

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
(require 'org)
(require 'org-publish)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(define-key org-mode-map (kbd "<C-tab>") nil)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-agenda-include-diary t
      org-log-done t
      org-agenda-include-all-todo nil
      org-archive-stamp-time nil
      org-highest-priority ?A
      org-default-priority ?C
      org-lowest-priority ?E
      org-tags-column -79
      org-todo-keywords '((type "NEXTACTION" "WAITING" "MAYBE" "|" "DONE")))

(when (request 'remember)
  (setq remember-annotation-functions '(org-remember-annotation))
  (setq remember-handler-functions '(org-remember-handler))
  (add-hook 'remember-mode-hook 'org-remember-apply-template)

  (setq org-remember-templates
        `((?n "* NEXTACTION %?\n  %i\n  %a" ,org-default-notes-file "Misc")
          (?w "* WAITING %?\n  %i\n  %a" ,org-default-notes-file "Misc")
          (?m "* MAYBE %?\n  %i\n  %a" ,org-default-notes-file "Misc"))))

(global-set-key (kbd "C-c t") 'fc-toggle-notes)
(defun fc-toggle-notes ()
  "Switch to the notes file, or switch to the previous buffer."
  (interactive)
  (let ((notes-file org-default-notes-file))
    (when (file-exists-p notes-file)
      (if (and (buffer-file-name)
               (string= (expand-file-name notes-file)
                        (expand-file-name (buffer-file-name))))
          (bury-buffer)
        (find-file notes-file)))))

(defun yh/looking-at-tags ()
  (looking-at "[ \t]*:\\([a-zA-Z_@0-9:]+\\):[ \t]*\\([\n\r]\\|\\'\\)"))

(defadvice org-insert-heading (before org-insert-heading-before act)
  "If the cursor is between heading and tags list, then open
a new heading WITHOUT moving the tags"
  (when (yh/looking-at-tags)
    (goto-char (point-at-eol))))

(defadvice org-priority (after org-priority-after act)
  "Restore the tags alignment after changing priorities"
  (and org-auto-align-tags (org-set-tags nil t)))

(defmadvice (org-agenda)
  (around ecb-org act)
  "Inhibit minimization of compile window when ecb is active"
  (flet ((ecb-toggle-compile-window (&rest args) nil))
    ad-do-it))

(provide 'org-config)
;;; org-config.el ends here
