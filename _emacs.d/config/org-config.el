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

(eval-after-load 'org-latex
  '(add-to-list 'org-export-latex-classes
                '("beamer"
                  "\\documentclass{beamer}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}"
                  ("\\part{%s}" . "\\part*{%s}")
                  ("\\frame{\\frametitle{%s}\\begin{itemize}" "\\end{itemize}}"
                   "\\frame{\\frametitle{%s}\\begin{itemize}" "\\end{itemize}}")
                  ("\\item{%s}"  "\\item*{%s}")
                  ("" ""))))

(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "<C-tab>") nil)
     (define-key global-map "\C-cl" 'org-store-link)
     (define-key global-map "\C-ca" 'org-agenda)
     (global-set-key "\C-cb" 'org-ido-switchb)

     (setq org-agenda-include-diary t
           org-log-done '(time note)
           org-agenda-skip-comment-trees nil
           org-agenda-include-all-todo nil
           org-agenda-skip-deadline-if-done t
           org-agenda-skip-scheduled-if-done t
           org-reverse-note-order t
           org-highest-priority ?A
           org-default-priority ?C
           org-lowest-priority ?E
           org-tags-column -79
           org-agenda-start-on-weekday nil
           ;; Use IDO for target completion
           org-completion-use-ido t
           ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
           org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)))
           ;; Targets start with the file name - allows creating level 1 tasks
           org-refile-use-outline-path (quote file))

     (setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
                                     (sequence "WAITING(w@/!)" "MAYBE(m!)" "|" "CANCELLED(c@/!)"))))

     (setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                          ("STARTED" :foreground "blue" :weight bold)
                                          ("DONE" :foreground "forest green" :weight bold)
                                          ("WAITING" :foreground "orange" :weight bold)
                                          ("MAYBE" :foreground "magenta" :weight bold)
                                          ("CANCELLED" :foreground "forest green" :weight bold))))

     (setq org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
                                                ("WAITING" ("WAITING" . t) ("NEXT"))
                                                ("MAYBE" ("WAITING" . t))
                                                (done ("NEXT") ("WAITING"))
                                                ("TODO" ("WAITING") ("CANCELLED"))
                                                ("STARTED" ("WAITING")))))

     (setq org-agenda-custom-commands
           (quote (("s" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
                   ("w" "Tasks waiting on something" tags "WAITING" ((org-use-tag-inheritance nil)))
                   ("r" "Refile New Notes and Tasks" tags "REFILE" ((org-agenda-todo-ignore-with-date nil)))
                   ("n" "Notes" tags "NOTE" nil))))

     ;; Resume clocking tasks when emacs is restarted
     (setq org-clock-persistence-insinuate)
     ;; Yes it's long... but more is better ;)
     (setq org-clock-history-length 35)
     ;; Resume clocking task on clock-in if the clock is open
     (setq org-clock-in-resume t)
     ;; Change task state to STARTED when clocking in
     (setq org-clock-in-switch-to-state "STARTED")
     ;; Save clock data and notes in the LOGBOOK drawer
     (setq org-clock-into-drawer t)
     ;; Sometimes I change tasks I'm clocking quickly - this just removes clocked tasks with 0:00 duration
     (setq org-clock-out-remove-zero-time-clocks t)
     ;; Don't clock out when moving task to a done state
     (setq org-clock-out-when-done nil)
     ;; Save the running clock and all clock history when exiting Emacs, load it on startup
     (setq org-clock-persist t)
     ;; Agenda clock reports parameters (no links, 2 levels deep)
     (setq org-agenda-clockreport-parameter-plist (quote (:link nil :maxlevel 2)))
     ;; Agenda log mode items to display (clock time only by default)
     (setq org-agenda-log-mode-items (quote (clock)))

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

     (when (request 'org-crypt)
       (define-key org-mode-map (kbd "C-c C-/") 'org-decrypt-entry))

;;; remind.sh contains something like :
     ;; #!/bin/sh

     ;; minutes=$1
     ;; shift
     ;; text="$*"

     ;; notify-send -i /usr/share/icons/crystalsvg/32x32/apps/bell.png "Appt in $minutes mins" "$text"
     ))

(provide 'org-config)
;;; org-config.el ends here
