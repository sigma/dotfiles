;;; Sacha's configuration for planner.el
;; Sacha Chua <sacha@free.net.ph>

;;;_+ Loading

;; (setq planner-diary-use-schedule nil)

(require 'planner)
(planner-install-extra-task-keybindings)

(define-key planner-mode-map "\C-c\C-t\C-e" 'planner-edit-task-description)

;;(require 'planner-accomplishments)
;; (require 'planner-cyclic)
;; (require 'planner-experimental)
(eval-after-load "bbdb"
  '(require 'planner-bbdb))
;; (require 'planner-notes-index)
(eval-after-load "gnus"
  '(progn
     (require 'planner-gnus)
     (planner-gnus-insinuate)))
;; (require 'planner-erc)
;; (require 'planner-lisp)
(require 'planner-id)
;; (require 'planner-notes)
;; (require 'planner-rss)
;; (require 'planner-schedule)
;; (require 'planner-timeclock)
;; (require 'planner-w3m)

(planner-insinuate-calendar)
;; (require 'remember-config)

(emacs-wiki-configure-highlighting 'emacs-wiki-highlight-markup
                                   (delete '("=[^\t =]" ?= emacs-wiki-highlight-verbatim)
					   emacs-wiki-highlight-markup))
(defadvice emacs-wiki-highlight-verbatim-tag (around sacha activate)
  "Do not do verbatim at all.")
;; I hate marking up words!
(setq emacs-wiki-publishing-markup
      (delq    ;; emphasized or literal text
       '["\\(^\\|[-[ \t\n<('`\"]\\)\\(=[^= \t\n]\\|_[^_ \t\n]\\|\\*+[^* \t\n]\\)"
         2 emacs-wiki-markup-word]
       emacs-wiki-publishing-markup))

;;;_+ Font lock

;(unless (listp font-lock-support-mode)
;  (setq font-lock-support-mode (cons t font-lock-support-mode)))
;(add-to-list 'font-lock-support-mode '(planner-mode . nil))
;(add-to-list 'font-lock-support-mode '(planner-muse-mode . nil))
;(add-to-list 'font-lock-support-mode '(planner-emacs-wiki-mode . nil))

;;;_+ Basic setup

(setq planner-directory "~/Plans")
(setq planner-carry-tasks-forward t)
(setq planner-expand-name-favor-future-p t)
(setq planner-publishing-directory "~/Published/")

;; Do not automatically add task IDs. I used to set this to non-nil,
;; but realized that I didn't edit my task descriptions that often. If
;; I want to edit a task, I can just add the task ID _before_ editing.
(setq planner-id-add-task-id-flag nil)

(setq planner-custom-variables
      '((emacs-wiki-publishing-header . "<lisp>(my-publishing-header)</lisp>")
        (emacs-wiki-publishing-footer . "<lisp>(my-publishing-footer)</lisp>")
        (emacs-wiki-publishing-file-suffix . ".php")))
(setq planner-day-page-template
      "* ~/.diary schedule

* Tasks


* Notes

")

;;; Compatibility, purely for old pages I'm too lazy to change.
;;; planner-diary is so much cooler.

;; (defun sacha/planner-get-diary-entries (date)
;;   "For DATE (yyyy.mm.dd), return a list of diary entries as a string."
;;   (require 'diary-lib)
;;   (when (string-match planner-date-regexp date)
;;     (let* ((diary-display-hook 'ignore)
;;            (entries (list-diary-entries
;;                      (list (string-to-number (match-string 2 date)) ; month
;;                            (string-to-number (match-string 3 date)) ; day
;;                            (string-to-number (match-string 1 date))) ; year
;;                      1))) ; Get entries for one day
;;       (if entries
;;           (mapconcat (lambda (item) (nth 1 item)) entries "\n")
;;         nil))))

;; (fset 'planner-get-diary-entries 'sacha/planner-get-diary-entries)

;;; Here we use planner-diary.
;; (require 'planner-diary)
;; (setq planner-diary-string "* ~/.diary schedule")
;; (setq planner-diary-use-diary t)
;; ;;(planner-diary-insinuate)
;; (defadvice plan (after sacha activate)
;;   "Call `planner-diary-insert-diary'."
;;   (planner-diary-insert-diary))

;; (defun sacha/planner-diary-schedule-task (start end)
;;   "Add a diary entry for the current task from START to END."
;;   (interactive "MStart: \nMEnd: ")
;;   (save-window-excursion
;;     (save-excursion
;;       (save-restriction
;;         (let* ((info (planner-current-task-info))
;;                (original (planner-task-description info))
;;                description)
;;           ;; TODO: Mark the task as scheduled for a particular time
;;           (setq description
;;                 (cond
;;                  ((string-match "\\s-+{{Schedule:\\([^}]+\\)}}" original)
;;                   (replace-match (concat start "-" end) t t original 1))
;;                  ((string-match "\\(\\s-*\\)$" original)
;;                   (replace-match (concat " {{Schedule:" start "-" end "}}")
;;                                  t t original 1))))
;;           (save-window-excursion
;;             (when (planner-task-link info)
;;               (planner-jump-to-linked-task)
;;               (let ((info (planner-current-task-info)))
;;                 (setcar (cdr (cdr (cdr (cdr info))))
;;                         description)
;;                 (delete-region (line-beginning-position)
;;                                (min (point-max)
;;                                     (1+ (line-end-position))))
;;                 (insert (planner-format-task info) "\n"))))
;;           (setcar (cdr (cdr (cdr (cdr info))))
;;                   description)
;;           (delete-region (line-beginning-position)
;;                          (min (point-max)
;;                               (1+ (line-end-position))))
;;           (insert (planner-format-task info) "\n")
;;           ;; Add the diary entry
;;           (sacha/planner-diary-add-entry
;;            (planner-task-date info)
;;            (concat start " | " end " | " original)))))))

;; (defun sacha/planner-diary-add-entry (date text &optional annotation)
;;   "Prompt for a diary entry to add to `diary-file'."
;;   (interactive
;;    (list
;;     (if (or current-prefix-arg
;;             (not (string-match planner-date-regexp (planner-page-name))))
;;         (planner-read-date)
;;       (planner-page-name))
;;     (read-string
;;      "Diary entry: ")))
;;   (save-excursion
;;     (save-window-excursion
;;       (make-diary-entry
;;        (concat
;;         (let ((cal-date (planner-filename-to-calendar-date date)))
;;           (calendar-date-string cal-date t t))
;;         " " text
;;         (or annotation
;;             (let ((annotation (run-hook-with-args-until-success
;;                                'planner-annotation-functions)))
;;               (if annotation
;;                   (concat " " annotation)
;;                 "")))))
;;       (planner-goto date)
;;       (planner-diary-insert-diary-maybe))))

;; (defun sacha/planner-diary-unschedule-entry ()
;;   "Unschedule the current entry."
;;   (interactive)
;;   (goto-char (line-beginning-position))
;;   (let ((id
;;          (if (re-search-forward "{{Tasks:\\([^}]+\\)}}" (line-end-position) t)
;;              (match-string 0)
;;            nil)))
;;     (sacha/planner-diary-delete-entry)
;;     (when id
;;       (planner-seek-to-first "Tasks")
;;       (re-search-forward id nil t))))

;; (defun sacha/planner-diary-delete-entry ()
;;   "Delete the current entry from `diary-file'."
;;   (interactive)
;;   (let ((cal-date (planner-filename-to-calendar-date (planner-page-name)))
;;         (text (buffer-substring (line-beginning-position)
;;                                 (line-end-position)))
;;         (case-fold-search nil))
;;     (save-excursion
;;       (save-window-excursion
;;         (find-file diary-file)
;;         (save-excursion
;;           (save-restriction
;;             (widen)
;;             (goto-char (point-max))
;;             (when (re-search-backward
;;                    (concat "^"
;;                            (regexp-quote
;;                             (concat (calendar-date-string cal-date t t)
;;                                     " " text))))
;;               (delete-region (line-beginning-position)
;;                              (min (1+ (line-end-position)) (point-max))))
;;             (save-buffer))))
;;       (planner-diary-insert-diary-maybe t))))

;;;_+ Header and footer

;;(defvar planner-day-header-file "/home/sacha/notebook/wiki/.day.header"
;;  "The header file to include for planner day pages (ex: 2003.03.17)")
;;(defvar planner-day-footer-file "/home/sacha/notebook/wiki/.day.footer"
;;  "The footer file to include for planner day pages (ex: 2003.03.17)")
(defvar planner-header-file "~/.header"
  "The header file to include for normal planner pages (ex: WelcomePage)")
(defvar planner-footer-file "~/.footer"
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

(define-key planner-mode-map (kbd "C-c C-s") 'sacha/planner-diary-schedule-task)
(define-key planner-mode-map (kbd "C-c C-S-s") 'sacha/planner-diary-unschedule-entry)
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
;; (add-to-list 'remember-planner-append-hook 'planner-rss-add-note t)

;; (defadvice planner-rss-add-note (around sacha/absolute-urls activate)
;;   "Publish absolute URLs."
;;   (let ((sacha/emacs-wiki-use-absolute-url-flag t))
;;     (setq ad-return-value ad-do-it)))

;; (defun sacha/rss-delete-item ()
;;   (interactive)
;;   (delete-region
;;    (if (looking-at "<item>")
;;        (point)
;;      (when (re-search-backward "<item>" nil t)
;;        (match-beginning 0)))
;;    (when (re-search-forward "</item>" nil t)
;;      (match-end 0))))

;;;_+ Misc

(defun sacha/planner-replan-region (beg end &optional page)
  "Replan all tasks from BEG to END to PAGE."
  (interactive (list (point) (mark)
                     (planner-read-name (planner-file-alist) "Replan to: ")))
  (let ((start (if (< beg end) beg end))
        (finish (if (< beg end) end beg)))
    ;; Invoke planner-copy-or-move-task on each line in reverse
    (save-excursion
      (save-restriction
        (narrow-to-region
         (and (goto-char start) (line-beginning-position))
         (and (goto-char finish) (min (point-max)
                                      (1+ (line-end-position)))))
        (goto-char (point-min))
        (while (not (eobp))
          (planner-replan-task page)
          (forward-line -1))))))

;; 20040504: Relative annotations
(setq planner-annotation-use-relative-file
      (lambda (filename)
        "Use relative filename if FILENAME is under my home directory."
        (save-match-data
          (string-match "^/home/yann" filename))))

;; I don't need my tasks renumbered.
;; (add-hook 'planner-mode-hook
;;           (lambda ()
;;             (remove-hook 'write-file-functions 'planner-renumber-tasks t)
;;             (remove-hook 'write-file-functions 'planner-align-tasks t)
;;             (remove-hook 'write-file-functions 'planner-renumber-notes t)))

;; I want notes preceded by a number so I know how to link to them.

(defun sacha/planner-markup-note ()
  "Replace note with marked-up span."
  (let ((id (concat
             emacs-wiki-bare-digits-anchor-prefix
             (match-string 1)))
        (val (match-string 1)))
    (replace-match
     (save-match-data
       (format "#%s\n** %s " id
               (planner-make-link
                (concat (emacs-wiki-page-name)
                        "#" val)
                (concat val ".")))))))

(defalias 'planner-markup-note 'sacha/planner-markup-note)

;;;_+ End

(planner-update-wiki-project)
;; (planner-use-emacs-wiki-backend)

(eval-when-compile (require 'cl))
(defun planner-find-file-reuse-window (file)
  (let ((list (remove-if-not (lambda (w)
                               (with-current-buffer (window-buffer w)
                                 (eq major-mode 'planner-mode)))
                             (window-list))))
    (save-excursion
      (if (consp list)
          (progn
            (select-window (car list))
            (find-file file))
        (find-file-other-window file)))))

(setq planner-reuse-window t)

(defun planner-goto (date &optional just-show)
  "Jump to the planning page for DATE.
If no page for DATE exists and JUST-SHOW is non-nil, don't create
a new page - simply return nil."
  (interactive (list (or
                      (planner-read-date)
                      (planner-read-non-date-page (planner-file-alist)))))
  (if (or (not just-show) (planner-page-exists-p date))
      (progn
        (planner-find-file date
                           (cond (planner-reuse-window
                                  'planner-find-file-reuse-window)
                                 (planner-use-other-window
                                  'find-file-other-window)
                                 (t
                                  'find-file)))
        (widen)
        (goto-char (point-min))
        (run-hooks 'planner-goto-hook)
        t)
    (when (interactive-p)
      (message "No planner file for %s." date))
    nil))

(provide 'planner-config)


;;;_* Local emacs vars.
;;;Local variables:
;;;allout-layout: (* 0 : )
;;;End:

;;; planner-config.el ends here
