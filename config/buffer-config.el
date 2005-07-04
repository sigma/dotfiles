;;; buffer-config.el ---

;; Copyright (C) 2004  Free Software Foundation, Inc.

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

;;; Buffers

(eval-after-load "buff-menu" '(request 'buff-menu+))
(global-set-key (kbd "C-x C-b") 'buffer-menu)

(add-hook 'dired-load-hook
          (lambda ()
            (require 'dired-aux)
            (require 'dired-x)

            ;; use ediff for diffing
            (defadvice dired-diff (around ad-dired-diff-ediff act)
              (flet ((diff (old new switches) (ediff old new)))
                ad-do-it))
            (defadvice dired-backup-diff (around ad-dired-backup-diff-ediff act)
              (flet ((diff-backup (old switches) (ediff-backup old)))
                ad-do-it))

            ;; Set dired-x global variables here.  For example:
            (setq dired-x-hands-off-my-keys nil
                  dired-find-subdir nil)

            (define-key dired-mode-map (kbd "C-c C-c") 'wdired-change-to-wdired-mode)
            ))

(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (dired-omit-mode 1)
            ))

(when (request 'icomplete) (icomplete-mode 1))
;(when (request 'iswitchb) (iswitchb-mode 1))

(defun ido-file-internal (method &optional fallback default prompt item initial switch-cmd)
  ;; Internal function for ido-find-file and friends
  (unless item
    (setq item 'file))
  (let* ((ido-current-directory (ido-expand-directory default))
	 (ido-directory-nonreadable (ido-nonreadable-directory-p ido-current-directory))
	 (ido-directory-too-big (and (not ido-directory-nonreadable)
				     (ido-directory-too-big-p ido-current-directory)))
	 (ido-context-switch-command switch-cmd)
	 filename)

    (cond
     ((or (not ido-mode) (ido-is-slow-ftp-host))
      (setq filename t
	    ido-exit 'fallback))

     ((and (eq item 'file)
	   (or ido-use-url-at-point ido-use-filename-at-point))
      (let (fn d)
	(require 'ffap)
	;; Duplicate code from ffap-guesser as we want different behaviour for files and URLs.
	(cond
	 ((and ido-use-url-at-point
	       ffap-url-regexp
	       (ffap-fixup-url (or (ffap-url-at-point)
				   (ffap-gopher-at-point))))
	  (setq ido-exit 'ffap
		filename t))

	 ((and ido-use-filename-at-point
	       (setq fn (ffap-guesser))
	       (not (string-match "^http:/" fn))
	       (setq d (file-name-directory fn))
	       (file-directory-p d))
	  (setq ido-current-directory d)
	  (setq initial (file-name-nondirectory fn)))))))

    (let (ido-saved-vc-hb
	  (vc-handled-backends (and (boundp 'vc-handled-backends) vc-handled-backends))
	  (ido-work-directory-index -1)
	  (ido-work-file-index -1)
       	  (ido-find-literal nil))

      (unless filename
	(setq ido-saved-vc-hb vc-handled-backends)
	(setq filename (ido-read-internal item
					  (or prompt "Find file: ")
					  'ido-file-history nil nil initial)))

      ;; Choose the file name: either the text typed in, or the head
      ;; of the list of matches

      (cond
       ((eq ido-exit 'fallback)
	;; Need to guard setting of default-directory here, since
	;; we don't want to change directory of current buffer.
	(let ((default-directory ido-current-directory)
	      (read-file-name-function nil))
	  (call-interactively (or fallback 'find-file))))

       ((eq ido-exit 'switch-to-buffer)
	(ido-buffer-internal ido-default-buffer-method nil nil nil ido-text))

       ((eq ido-exit 'insert-buffer)
	(ido-buffer-internal 'insert 'insert-buffer "Insert buffer: " nil ido-text 'ido-enter-insert-file))

       ((eq ido-exit 'dired)
	(dired (concat ido-current-directory (or ido-text ""))))

       ((eq ido-exit 'ffap)
	(find-file-at-point))

       ((eq method 'alt-file)
	(ido-record-work-file filename)
	(setq default-directory ido-current-directory)
	(ido-record-work-directory)
	(find-alternate-file filename))

       ((memq method '(dired list-directory))
	(if (equal filename ".")
	    (setq filename ""))
	(let* ((dirname (ido-final-slash (concat ido-current-directory filename) t))
	       (file (substring dirname 0 -1)))
	  (cond
	   ((file-directory-p dirname)
	    (ido-record-command method dirname)
	    (ido-record-work-directory dirname)
	    (funcall method dirname))
	   ((file-directory-p ido-current-directory)
	    (cond
	     ((file-exists-p file)
	      (ido-record-command method ido-current-directory)
	      (ido-record-work-directory)
	      (funcall method ido-current-directory)
	      (if (eq method 'dired)
		  (dired-goto-file (expand-file-name file))))
	     ((string-match "[[*?]" filename)
	      (setq dirname (concat ido-current-directory filename))
	      (ido-record-command method dirname)
	      (ido-record-work-directory)
	      (funcall method dirname))
	     ((y-or-n-p (format "Directory %s does not exist. Create it " filename))
	      (ido-record-command method dirname)
	      (ido-record-work-directory dirname)
	      (make-directory-internal dirname)
	      (funcall method dirname))
	     (t
	      ;; put make-directory command on history
	      (ido-record-command 'make-directory dirname))))
	   (t (error "No such directory")))))

       ((eq method 'write)
	(ido-record-work-file filename)
	(setq default-directory ido-current-directory)
	(ido-record-command 'write-file (concat ido-current-directory filename))
	(ido-record-work-directory)
	(write-file filename))

       ((eq method 'read-only)
	(ido-record-work-file filename)
	(setq filename (concat ido-current-directory filename))
	(ido-record-command fallback filename)
	(ido-record-work-directory)
	(funcall fallback filename))

       ((eq method 'insert)
	(ido-record-work-file filename)
	(setq filename (concat ido-current-directory filename))
	(ido-record-command
	 (if ido-find-literal 'insert-file-literally 'insert-file)
	 filename)
	(ido-record-work-directory)
	(if ido-find-literal
	    (insert-file-contents-literally filename)
	  (insert-file-contents filename)))

       (filename
	(ido-record-work-file filename)
	(setq filename (concat ido-current-directory filename))
	(ido-record-command 'find-file filename)
	(ido-record-work-directory)
	(ido-visit-buffer (find-file-noselect filename nil ido-find-literal) method))))))

(provide 'buffer-config)
;;; buffer-config.el ends here
