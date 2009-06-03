;;; Sacha Chua's remember.el configuration

;; You can get remember.el from
;; http://sacha.free.net.ph/notebook/emacs/dev/remember

(request 'remember)

(defvar remember-original-winring nil)

;; (add-hook 'remember-before-remember-hook
;;           (lambda ()
;;             (setq remember-original-winring (winring-name-of-current))
;;             (winring-select ecb-winman-winring-name)))

;; (defadvice remember-destroy (after remember-destroy-after-ecb () act)
;;   (winring-select remember-original-winring))

;; (defadvice remember-buffer (after remember-buffer-after-ecb () act)
;;   (winring-select remember-original-winring))

(provide 'remember-config)