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
(require 'org-install)
(require 'org-archive)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "<C-tab>") nil)
     (define-key org-mode-map (kbd "C-c p") 'org-insert-property-drawer)

     (define-key global-map "\C-cl" 'org-store-link)
     (define-key global-map "\C-ca" 'org-agenda)

     (setq org-agenda-include-diary t
           org-log-done '(state)
           org-agenda-include-all-todo nil
           org-agenda-skip-deadline-if-done t
           org-agenda-skip-scheduled-if-done t
           org-reverse-note-order t
           org-archive-stamp-time nil
           org-highest-priority ?A
           org-default-priority ?C
           org-lowest-priority ?E
           org-tags-column -79
           org-agenda-start-on-weekday nil
           org-todo-keywords '((type "TODO(t)" "WAITING(w@)" "MAYBE(m)" "|" "DONE(d)" "CANCELED(c@)"))
           org-agenda-custom-commands '(("t" . "Open tasks")
                                        ("tn" tags-todo "URGENT|NORMAL/TODO|WAITING")
                                        ("tu" tags-todo "URGENT/TODO|WAITING")
                                        ("d" todo "DELEGATED" nil)
                                        ("c" todo "DONE|DEFERRED|CANCELLED" nil)
                                        ("w" todo "WAITING" nil)
                                        ("W" agenda "" ((org-agenda-ndays 21)))
                                        ("A" "Today's Priority #A tasks: " agenda ""
                                         ((org-agenda-skip-function
                                           (lambda nil
                                             (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
                                          (org-agenda-ndays 1)))
                                        ("u" "Unscheduled TODO entries: " alltodo ""
                                         ((org-agenda-skip-function
                                           (lambda nil
                                             (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                                                       (quote regexp) "<[^>\n]+>")))))))

     (when (request 'remember)
       (setq remember-annotation-functions '(org-remember-annotation))
       (setq remember-handler-functions '(org-remember-handler))
       (add-hook 'remember-mode-hook 'org-remember-apply-template)

       (setq org-remember-templates
             `((?t "* TODO %?\n  %i\n  %a" ,org-default-notes-file "Misc")
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

     (require 'appt)
     (setq appt-time-msg-list nil)
     (org-agenda-to-appt)

     (defadvice  org-agenda-redo (after org-agenda-redo-add-appts act)
       "Pressing `r' on the agenda will also add appointments."
       (progn
         (setq appt-time-msg-list nil)
         (org-agenda-to-appt)))

     (progn
       (appt-activate 1)
       (setq appt-display-format 'window)
       (setq appt-disp-window-function (function my-appt-disp-window))
       (defun my-appt-disp-window (min-to-app new-time msg)
         (call-process "remind.sh" nil 0 nil min-to-app msg new-time)))

;;; remind.sh contains something like :
     ;; #!/bin/sh

     ;; minutes=$1
     ;; shift
     ;; text="$*"

     ;; notify-send -i /usr/share/icons/crystalsvg/32x32/apps/bell.png "Appt in $minutes mins" "$text"

     (defun org-get-repeat ()
       "Check if tere is a deadline/schedule with repeater in this entry."
       (save-match-data
         (save-excursion
           (org-back-to-heading t)
           (or (org-entry-get (point) "Recurring" t)
               (if (re-search-forward
                    org-repeat-re (save-excursion (outline-next-heading) (point)) t)
                   (match-string 1))))))

     (defun org-property-visibility ()
       "Switch subtree visibility according to :visibility: property."
       (interactive)
       (let (state)
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward
                   "^[ \t]*:visibility:[ \t]+\\([a-z]+\\)"
                   nil t)
             (setq state (match-string 1))
             (save-excursion
               (org-back-to-heading t)
               (hide-subtree)
               (org-reveal)
               (cond
                ((equal state "children")
                 (org-show-hidden-entry)
                 (show-children))
                ((equal state "all")
                 (show-subtree)))))
           (org-cycle-hide-drawers 'all))))

     (add-hook 'org-mode-hook 'org-property-visibility)))

(provide 'org-config)
;;; org-config.el ends here
