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
(add-to-list 'planner-log-edit-flush-lines-list "^## .*")

(provide 'planner-config)


;;;_* Local emacs vars.
;;;Local variables:
;;;allout-layout: (* 0 : )
;;;End:

;;; planner-config.el ends here
