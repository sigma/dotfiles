;;; Sacha's configuration for planner.el
;; Sacha Chua <sacha@free.net.ph>

;;;_+ Loading

(request 'planner)

(planner-install-extra-task-keybindings)

(planner-install-extra-note-keybindings)
(define-key planner-mode-map "\C-c\C-o\C-h" (lambda () (interactive) (planner-create-note (muse-page-name))))

(setq planner-annotation-format-local-file-name 'yh/collapse-home-directory)

(define-key planner-mode-map "\C-c\C-t\C-e" 'planner-edit-task-description)

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

(request 'planner-xtla)

(request 'planner-bookmark)

(provide 'planner-config)

(defvar planner-config-current-annotation nil)

(defun planner-config-copy-annotation ()
  (interactive)
  (setq planner-config-current-annotation
        (run-hook-with-args-until-success
         'planner-annotation-functions)))

(defun planner-config-paste-annotation ()
  (interactive)
  (if planner-config-current-annotation
      (insert planner-config-current-annotation)))

(define-key planner-mode-map "\C-c\M-w" 'planner-config-copy-annotation)
(define-key planner-mode-map "\C-c\C-y" 'planner-config-paste-annotation)

;;;_* Local emacs vars.
;;;Local variables:
;;;allout-layout: (* 0 : )
;;;End:

;;; planner-config.el ends here
