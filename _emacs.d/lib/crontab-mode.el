;;; crontab-mode.el --- Mode for editing crontab files
;;
;; ~/lib/emacs/jhg-lisp/crontab-mode.el ---
;;
;; $Id: crontab-mode.el,v 1.10 1999/05/25 17:56:18 harley Exp harley $
;;

;; Author:   James H Gorrell <harley@bcm.tmc.edu>
;; Keywords: cron, crontab
;; URL:      http://www.hgsc.bcm.tmc.edu/~harley/elisp/crontab-mode.el

;;; Commentary:

;; I want to keep my crontabs under rcs to keep a history of
;; the file.  Editing them with 'crontab -e' is rather
;; cumbersome.  My method is to keep the crontab as a file,
;; under rcs, and check in the changes with 'C-c C-c' after
;; editing.

;;; Code:

(defvar crontab-suffix ".crontab"
  "*Suffix for crontab buffers.")

(defvar crontab-apply-after-save nil
  "*Non-nil to apply the crontab after a save.")
(make-variable-buffer-local 'crontab-apply-after-save)

(defvar crontab-host nil
  "*Default hostname to use for the crontab.")
(make-variable-buffer-local 'crontab-host)

;; Would be better to have "\\([0-9]\\([-,][0-9]+\\)+\\|...
(defvar crontab-unit-regexp "\\([-,0-9]+\\|\\*\\)"
  "A regexp which matches a cron time unit.")

(defvar crontab-sep-regexp "[ \t]+"
  "A regexp to match whitespace seperating cron time units.")

(defvar crontab-ruler "
# min   hour    day     month   day-of-week command
#(0-59) (0-23)  (1-31)  (1-12)  (0-6)
#
#------------------------------------------------------------
"
  "*The ruler `crontab-insert-ruler' inserts.")

;;
(defvar crontab-mode-hook nil
  "*Hook for customising crontab mode.")

(defvar crontab-load-hook nil
  "*Hook run when the cronmode is loaded.")

;;
(defvar crontab-font-lock-keywords
  (list
   ;; Comments
   '("^#.*$" . font-lock-comment-face)
   ;; Blank lines are bad!
   '("^[ \t]+$" . highlight)
   ;; Cron lines
   ;; 50 * * * * /usr/gnu/bin/bash
   (cons
    (concat "^"
	    crontab-unit-regexp crontab-sep-regexp
	    crontab-unit-regexp crontab-sep-regexp
	    crontab-unit-regexp crontab-sep-regexp
	    crontab-unit-regexp crontab-sep-regexp
	    crontab-unit-regexp crontab-sep-regexp
	    "\\(.*\\)$")
    '(
      (1 font-lock-keyword-face)
      (2 font-lock-keyword-face)
      (3 font-lock-keyword-face)
      (4 font-lock-keyword-face)
      (5 font-lock-keyword-face)
      (6 font-lock-string-face)))
   )
  "Info for function `font-lock-mode'."
  )


(defvar crontab-mode-map nil
  "Keymap used in crontab mode.")
(if crontab-mode-map
    ()
  (setq crontab-mode-map (make-sparse-keymap))
  (define-key crontab-mode-map "\C-c\C-c" 'crontab-save-and-apply)
  (define-key crontab-mode-map "\C-cc" 'crontab-save-and-apply)
  (define-key crontab-mode-map "\C-ca" 'crontab-save-and-apply-to)
  (define-key crontab-mode-map "\C-ci" 'crontab-insert-local-var)
  (define-key crontab-mode-map "\C-cr" 'crontab-insert-ruler)
  )


;;;###autoload
(defun crontab-mode ()
  "Major mode for editing crontabs.
Defines commands for getting and applying crontabs for hosts.
Sets up command `font-lock-mode'.

\\{crontab-mode-map}"
  (interactive)
  ;;
  (kill-all-local-variables)
  (setq mode-name "crontab")
  (setq major-mode 'crontab-mode)
  (use-local-map crontab-mode-map)
  ;;
  (setq comment-start "#")
  (setq comment-start-skip "#+ *")
  ;;
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(crontab-font-lock-keywords))
  ;; Add to the end of the buffers save hooks.
  (add-hook 'after-save-hook 'crontab-after-save t t)
  ;;
  (run-hooks 'crontab-mode-hook)
  )


;;;###autoload
(defun crontab-get (host)
  "Get the crontab for the HOST into a buffer."
  (interactive "sCrontab for host:")
  (let ((cbn (generate-new-buffer-name (concat host crontab-suffix))))
    (switch-to-buffer-other-window cbn)
    (erase-buffer)
    (crontab-mode)
    (crontab-insert host)
    (not-modified)
    (setq crontab-host host)
    ))

(defun crontab-insert (host)
  "Insert the crontab for the HOST into the current buffer."
  (shell-command (concat "rsh " host " crontab -l") t))

(defun crontab-apply (host)
  "Apply the crontab to a HOST."
  (if (and host (buffer-file-name))
      (shell-command (concat "rsh " host " crontab " (buffer-file-name)))
    (error "No filename to write to")))

(defun crontab-save-and-apply ()
  "Save and apply the buffer to the HOST."
  (interactive)
  (save-buffer)
  (if (not crontab-apply-after-save) ;; Dont apply it twice.
      (crontab-apply (crontab-host))))

(defun crontab-save-and-apply-to (host)
  "Prompt for the HOST and apply the file."
  (interactive "sApply to host:")
  (setq crontab-host host)
  (crontab-save-and-apply))

(defun crontab-insert-ruler ()
  "Insert a ruler with comments into the crontab."
  (interactive)
  (end-of-line)
  (insert crontab-ruler))

(defun crontab-insert-local-var ()
  "Insert the current values of buffer local variables."
  (interactive)
  (end-of-buffer)
  (insert "
" comment-start " Local " "Variables:
" comment-start " mode: " (format "%s" (or mode-name "crontab")) "
" comment-start " crontab-host: " (crontab-host) "
" comment-start " crontab-apply-after-save: "
(format "%s" crontab-apply-after-save) "
" comment-start " End:
"))

(defun crontab-host ()
  "Return the hostname as a string, defaulting to the local host.
The variable `crontab-host' could be a symbol or a string."
  (format "%s" (or crontab-host system-name)))

;;
(defun crontab-after-save ()
  "If `crontab-apply-after-save' is set, apply the crontab after a save."
  (if crontab-apply-after-save (crontab-apply (crontab-host))))

(provide 'crontab-mode)
(run-hooks 'crontab-load-hook)

;;; crontab-mode.el ends here
