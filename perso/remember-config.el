;;; Sacha Chua's remember.el configuration

;; You can get remember.el from
;; http://sacha.free.net.ph/notebook/emacs/dev/remember

(require 'remember)
(require 'planner)
;; (require 'planner-rss)
(require 'remember-planner)
;; (require 'remember-bibl)

(setq remember-save-after-remembering t)
(setq remember-handler-functions '(remember-planner-append))

(defvaralias 'remember-annotation-functions 'planner-annotation-functions)

(setq remember-append-to-planner-hook
      '(remember-planner-add-timestamp remember-planner-add-xref planner-rss-add-note))

(provide 'remember-config)