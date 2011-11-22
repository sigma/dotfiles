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
;;(require 'org-man)

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

;;
;; Phone capture template handling with BBDB lookup
;; modified from the original code by Gregory J. Grubbs
;;
(defvar gjg/capture-phone-record nil
  "Either BBDB record vector, or person's name as a string, or nil")

(defun bh/phone-call ()
  (interactive)
  (let* ((myname (completing-read "Who is calling? " (bbdb-hashtable) 'bbdb-completion-predicate 'confirm))
         (my-bbdb-name (if (> (length myname) 0) myname nil)))
    (setq gjg/capture-phone-record
          (if my-bbdb-name
              (first (or (bbdb-search (bbdb-records) my-bbdb-name nil nil)
                         (bbdb-search (bbdb-records) nil my-bbdb-name nil)))
            myname)))
  (gjg/bbdb-name))

(defun gjg/bbdb-name ()
  "Return full name of saved bbdb record, or empty string - for use in Capture templates"
  (if (and gjg/capture-phone-record (vectorp gjg/capture-phone-record))
      (concat "[[bbdb:"
              (bbdb-record-name gjg/capture-phone-record) "]["
              (bbdb-record-name gjg/capture-phone-record) "]]")
    "NAME"))

(defun gjg/bbdb-company ()
  "Return company of saved bbdb record, or empty string - for use in Capture templates"
  (if (and gjg/capture-phone-record (vectorp gjg/capture-phone-record))
      (or (bbdb-record-company gjg/capture-phone-record) "")
    "COMPANY"))

(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "<C-tab>") nil)
     (global-set-key (kbd "C-c l") 'org-store-link)
     (global-set-key (kbd "C-c a") 'org-agenda)
     (global-set-key (kbd "C-c b") 'org-ido-switchb)
     (global-set-key (kbd "C-c n") 'org-capture)

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

     (setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
                               (sequence "WAITING(w@/!)" "MAYBE(m!)" "|" "CANCELLED(c@/!)")))

     (setq org-todo-keyword-faces '(("TODO" :foreground "red" :weight bold)
                                    ("STARTED" :foreground "blue" :weight bold)
                                    ("DONE" :foreground "forest green" :weight bold)
                                    ("WAITING" :foreground "orange" :weight bold)
                                    ("MAYBE" :foreground "magenta" :weight bold)
                                    ("CANCELLED" :foreground "forest green" :weight bold)))

     (setq org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                          ("WAITING" ("WAITING" . t) ("NEXT"))
                                          ("MAYBE" ("WAITING" . t))
                                          (done ("NEXT") ("WAITING"))
                                          ("TODO" ("WAITING") ("CANCELLED"))
                                          ("STARTED" ("WAITING"))))

     (setq org-agenda-custom-commands
           '(("s" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
             ("w" "Tasks waiting on something" tags "WAITING" ((org-use-tag-inheritance nil)))
             ("r" "Refile New Notes and Tasks" tags "REFILE" ((org-agenda-todo-ignore-with-date nil)))
             ("n" "Notes" tags "NOTE" nil)))

     (setq org-capture-templates
           '(("t" "Todo" entry (file "~/org/refile.org") "* TODO %?\n  %i\n  %a" :prepend t)
             ("m" "Mail task" entry (file "~/org/refile.org")
              "* TODO Treat mail from %:from\n  SCHEDULED: %t\n  group: %:group\n  subject: %:subject\n  %a"
              :prepend t :immediate-finish t)
             ("p" "Phone call" entry (file "~/org/refile.org")
              "* Phone %(bh/phone-call) - %(gjg/bbdb-company) :PHONE:\n%U\n\n%?"
              :clock-in t :clock-resume t)
             ("j" "Journal" entry (file+datetree "~/org/journal.org") "* %?\n  Entered on %U\n  %i\n  %a")
             ("n" "Note" entry (file+headline "~/org/inbox.org" "Notes") "* %? :NOTE:\n  %u\n  %a" :prepend t)))

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

(defun yh/make-capture-frame ()
  "Create a new frame and run org-capture."
   (interactive)
   (select-frame
    (make-frame '((name . "_Remember_"))))
   (flet ((org-switch-to-buffer-other-window (&rest args) (apply 'switch-to-buffer args)))
     (condition-case nil
         (org-capture)
       (error (delete-frame)))))

(defun yh/delete-frame ()
  (when (equal "_Remember_" (frame-parameter nil 'name))
    (delete-frame)))

(add-hook 'org-capture-after-finalize-hook 'yh/delete-frame)

(org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (C . t)))

(provide 'org-config)
;;; org-config.el ends here
