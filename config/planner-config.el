;;; Sacha's configuration for planner.el
;; Sacha Chua <sacha@free.net.ph>

;;;_+ Loading

(require 'patches)

(when (request 'planner)

  (planner-install-extra-task-keybindings)

  (planner-install-extra-note-keybindings)
  (define-key planner-mode-map "\C-c\C-o\C-h" (lambda () (interactive) (planner-create-note (muse-page-name))))

  (setq planner-annotation-format-local-file-name 'yh/collapse-home-directory)

  (define-key planner-mode-map "\C-c\C-t\C-e" 'planner-edit-task-description)

  (define-key planner-mode-map (kbd "C-c C-t C-S-c") 'planner-task-cancelled)
  (define-key planner-mode-map (kbd "C-c C-S-c") 'planner-task-cancelled)

  (eval-after-load "bbdb"
    '(require 'planner-bbdb))

  (eval-after-load "gnus"
    '(progn
       (require 'planner-gnus)
       (planner-gnus-insinuate)))

  (request 'planner-bibtex)
  (request 'planner-id)

  (planner-insinuate-calendar)

  (request 'planner-log-edit)

  (add-to-list 'planner-log-edit-flush-regexp-list "^##.*$")

  (request 'planner-w3m)

                                        ; (request 'planner-xtla)

  (request 'planner-bookmark)

  (when (require 'planner-multi)
    (setq planner-multi-copy-tasks-to-page "TaskPool"))

  ;; (require 'action-lock)

  ;; (add-to-list 'action-lock-default-rules
  ;;             (list (with-planner muse-explicit-link-regexp)
  ;;                   'planner-action-lock-follow-name-at-point))

  ;; (setq action-lock-rules action-lock-default-rules)

  ;; (defvar planner-action-lock-default-directory (planner-directory) "Expand files relative to this directory.")

  ;; (defsubst planner-url-p (name)
  ;;   "Return non-nil if NAME is a URL."
  ;;   (save-match-data
  ;;     (string-match muse-url-regexp name)))

  ;; (defun planner-wiki-tag (wiki-name)
  ;;   (save-match-data
  ;;     (if (string-match "#" wiki-name)
  ;;         (substring wiki-name (match-end 0)))))

  ;; (defun planner-action-lock-follow-name-at-point (&optional other-window)
  ;;  (let ((link-name (match-string 0))
  ;;        (base-buffer (current-buffer))
  ;;        ;; the name of the buffer that contains the link.  check
  ;;        ;; whether buffer-name is a WikiName, else make it one
  ;;        (parent-name (or (planner-page-name)
  ;;                         (planner-make-link (buffer-name)))))
  ;;    (with-planner
  ;;      (let ((link (planner-link-target link-name)))
  ;;        (if (planner-url-p link)
  ;;            (planner-browse-url link other-window)
  ;;          ;; The name list is current since the last time the buffer was
  ;;          ;; highlighted
  ;;          (let* ((base (planner-link-base link-name))
  ;;                 (file (planner-page-file base))
  ;;                 (tag  (and (not (planner-url-p link))
  ;;                            (planner-wiki-tag link)))
  ;;                 (find-file-function (if other-window
  ;;                                         'find-file-other-window
  ;;                                       'find-file))
  ;;                 (newbuf
  ;;                  (funcall find-file-function
  ;;                           (or file
  ;;                               (expand-file-name
  ;;                                       base
  ;;                                       (or planner-action-lock-default-directory
  ;;                                           (and (buffer-file-name)
  ;;                                                (file-name-directory
  ;;                                                 (buffer-file-name)))
  ;;                                           default-directory))))))
  ;;            (when tag
  ;;              (goto-char (point-min))
  ;;              (re-search-forward (concat "^\\.?#" tag) nil t))))))))

  )

(provide 'planner-config)

;;;_* Local emacs vars.
;;;Local variables:
;;;allout-layout: (* 0 : )
;;;End:

;;; planner-config.el ends here
