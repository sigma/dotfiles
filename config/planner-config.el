;;; Sacha's configuration for planner.el
;; Sacha Chua <sacha@free.net.ph>

;;;_+ Loading

(request 'planner)
(planner-install-extra-task-keybindings)

(define-key planner-mode-map "\C-c\C-t\C-e" 'planner-edit-task-description)

(eval-after-load "bbdb"
  '(require 'planner-bbdb))

(eval-after-load "gnus"
  '(progn
     (require 'planner-gnus)
     (planner-gnus-insinuate)))

(request 'planner-id)

(planner-insinuate-calendar)

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
        (emacs-wiki-publishing-file-suffix . ".html")))
(setq planner-day-page-template
      "* ~/.diary schedule

* Tasks


* Notes

")

;;;_+ Header and footer

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

(defadvice emacs-wiki-next-reference (after emacspeak pre act comp)
  "Provide additional feedback"
  (message "%s" (match-string 0)))
(defadvice emacs-wiki-previous-reference (after emacspeak pre act comp)
  "Provide additional feedback"
  (message "%s" (match-string 0)))

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

(planner-update-wiki-project)

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

(require 'planner-log-edit)

(provide 'planner-config)


;;;_* Local emacs vars.
;;;Local variables:
;;;allout-layout: (* 0 : )
;;;End:

;;; planner-config.el ends here
