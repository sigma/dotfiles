;;; Sacha's configuration for planner.el
;; Sacha Chua <sacha@free.net.ph>

;;;_+ Loading

;; This directory contains all the latest emacs-wiki and planner files.
(require 'planner)
(require 'planner-experimental)
(require 'planner-bbdb)
(require 'planner-notes-index)
(require 'planner-diary)
(require 'planner-gnus)
(planner-gnus-insinuate)
;(require 'planner-erc)
(require 'planner-id)
;; (require 'planner-notes)
(require 'planner-rss)
;; (require 'planner-schedule)
;; (require 'planner-timeclock)
(require 'planner-w3m)
;(planner-use-muse-backend)
(planner-install-extra-task-keybindings)
;; (planner-insinuate-calendar)
;(load "remember-config")

;;;_+ Font lock

;; (unless (listp font-lock-support-mode)
;;   (setq font-lock-support-mode (cons t font-lock-support-mode)))
;; (add-to-list 'font-lock-support-mode '(planner-mode . nil))
;; (add-to-list 'font-lock-support-mode '(planner-muse-mode . nil))
;; (add-to-list 'font-lock-support-mode '(planner-emacs-wiki-mode . nil))

;;;_+ Basic setup

;(setq planner-directory "/home/sacha/notebook/plans")
(setq planner-carry-tasks-forward t)
(setq planner-expand-name-favor-future-p t)
;(setq planner-publishing-directory "/home/sacha/public_html/notebook/wiki")

(setq planner-emacs-wiki-custom-variables
      '((emacs-wiki-publishing-header . "<lisp>(my-publishing-header)</lisp>")
	(emacs-wiki-publishing-footer . "<lisp>(my-publishing-footer)</lisp>")
	(emacs-wiki-publishing-file-suffix . ".php")))
(setq planner-day-page-template
      "<contents>\n\n* Tasks\n\n\n* ~/.diary schedule\n\n* Notes\n\n")
(setq planner-diary-string "* ~/.diary schedule")

;;; Compatibility, purely for old pages I'm too lazy to change.
;;; planner-diary is so much cooler.

(defun sacha/planner-get-diary-entries (date)
  "For DATE (yyyy.mm.dd), return a list of diary entries as a string."
  (require 'diary-lib)
  (when (string-match planner-date-regexp date)
    (let* ((diary-display-hook 'ignore)
           (entries (list-diary-entries
                     (list (string-to-number (match-string 2 date)) ; month
                           (string-to-number (match-string 3 date)) ; day
                           (string-to-number (match-string 1 date))) ; year
                     1))) ; Get entries for one day
      (if entries
          (mapconcat (lambda (item) (nth 1 item)) entries "\n")
        nil))))

(fset 'planner-get-diary-entries 'sacha/planner-get-diary-entries)

;;; Here we use planner-diary.
(planner-diary-insinuate)

;;;_+ Header and footer

;;(defvar planner-day-header-file "/home/sacha/notebook/wiki/.day.header"
;;  "The header file to include for planner day pages (ex: 2003.03.17)")
;;(defvar planner-day-footer-file "/home/sacha/notebook/wiki/.day.footer"
;;  "The footer file to include for planner day pages (ex: 2003.03.17)")
(defvar planner-header-file "/home/sacha/notebook/wiki/.header"
  "The header file to include for normal planner pages (ex: WelcomePage)")
(defvar planner-footer-file "/home/sacha/notebook/wiki/.footer"
  "The footer file to include for normal planner pages (ex: WelcomePage)")


(defvar sacha/planner-no-header-or-footer
  '("SideBar"))

(defun my-publishing-header ()
  "Insert the header only if this file should have it."
  (cond
   ((member (emacs-wiki-page-name) sacha/planner-no-header-or-footer) "")
   ((file-readable-p planner-header-file) (ignore (insert-file-contents planner-header-file)))
   (t "this is the default header text, if the file can't be found\n")))

(defun my-publishing-footer ()
  "Insert the footer only if this file should have it."
  (cond
   ((member (emacs-wiki-page-name) sacha/planner-no-header-or-footer) "")
   ((file-readable-p planner-footer-file)
    (ignore (insert-file-contents planner-footer-file)))
   (t "this is the default footer text, if the file can't be found\n")))

;;;_+ Allout

;;(when (load "allout" t)
;;  (add-to-list 'planner-mode-hook (lambda ()
;;				    (condition-case err
;;					(progn
;;					  (allout-mode 1)
;;					  (allout-show-all)
;;					  (auto-fill-mode -1)
;;                                          (setq allout-old-style-prefixes nil))
;;				      (error nil)) t)))

;;;_+ Emacspeak

(defadvice emacs-wiki-next-reference (after emacspeak pre act comp)
  "Provide additional feedback"
  (message "%s" (match-string 0)))
(defadvice emacs-wiki-previous-reference (after emacspeak pre act comp)
  "Provide additional feedback"
  (message "%s" (match-string 0)))

;;;_+ RSS blogging
;(add-to-list 'remember-append-to-planner-hook 'planner-rss-add-note t)

;(planner-update-project)
;;;_* Local emacs vars.
;;;Local variables:
;;;allout-layout: (* 0 : )
;;;End:

(provide 'planner-config)

;;; planner-config.el ends here
