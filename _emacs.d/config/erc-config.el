;;; erc-config.el --- Configuration for erc

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

(provide 'erc-config)
;;; erc-config.el ends here

(request 'erc)
(request 'erc-auto)

(request 'erc-nicklist)

(when (request 'erc-autojoin)
  (erc-autojoin-mode 1)

  (defvar my-erc-autojoin-channels-alist nil)

  ;; This function overrides the default one to allow autojoining password
  ;; protected chans
  (defun erc-autojoin-channels (server nick)
    (dolist (l my-erc-autojoin-channels-alist)
      (when (string-match (car l) server)
        (dolist (chan (cdr l))
          (if (consp chan)
              (erc-send-command (concat "join " (car chan) " " (cdr chan)))
            (erc-send-command (concat "join " chan))))))))

(when (request 'erc-match)
  (erc-match-mode))

(when (request 'erc-track)
  (erc-track-mode t))

(add-hook 'erc-mode-hook
          '(lambda ()
             (require 'erc-pcomplete)
             (pcomplete-erc-setup)
             (erc-completion-mode 1)))

(when (request 'erc-fill)
  (erc-fill-mode t)
  (setq erc-fill-column 150))

(when (request 'erc-ring)
  (erc-ring-mode t))

(when (request 'erc-netsplit)
  (erc-netsplit-mode t))

(when (request 'erc-timestamp)
  (erc-timestamp-mode t)
  (setq erc-timestamp-format "[%R-%m/%d]"))

(when (request 'erc-button)
  (erc-button-mode nil))

;; logging:
(setq erc-log-insert-log-on-open nil)
(setq erc-log-channels t)
(setq erc-save-buffer-on-part t)
(setq erc-hide-timestamps nil)

(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(add-hook 'erc-mode-hook '(lambda () (when (not (featurep 'xemacs))
                                       (set (make-variable-buffer-local
                                             'coding-system-for-write)
                                            'emacs-mule))))
;; end logging

;; Truncate buffers so they don't hog core.
(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)

;; Clears out annoying erc-track-mode stuff for when we don't care.
;; Useful for when ChanServ restarts :P
(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(global-set-key (kbd "C-c r") 'reset-erc-track-mode)

(defvar my-erc-networks nil)

(defun irc ()
  (interactive)
  (dolist (net my-erc-networks)
    (apply 'erc net)))

;;; This section is shamelessly stolen from bojohan's config
;;; ERC...err...prompt -----------------------------------------
(defun my-erc-prompt-magic-time-string ()
  (let ((string "00:00"))
    (propertize
     "$$:$$"
     'display `(when (store-substring ,string 0 (format-time-string "%H:%M"))
		 . ,string))))

(defun my-erc-prompt-color-erc ()
  (let ((string "ERC"))
    (dolist (props `((0 1 (face ((:foreground "red"))))
		     (1 2 (face ((:foreground "blue"))))
		     (2 3 (face ((:foreground "yellow"))))))
      (apply 'add-text-properties (append props (list string))))
    string))

(defun my-erc-prompt-dashes-1 (&optional col)
  (make-string (- (window-width)
		  (or col (save-excursion
			    (goto-char buffer-position)
			    (current-column)))
		  2)
	       ?-))

(defun display-spec-hack (form)
  (let ((spec (list '(margin) "")))
    `(when (progn (setcdr ',spec ,form) t) . ,spec)))

(defun my-erc-prompt-dashes (len)
  (let ((form `(my-erc-prompt-dashes-1 ,len)))
    (propertize "$" 'display (display-spec-hack form))))

(defun my-erc-prompt ()
  (if (boundp 'cb)
      (erc-command-indicator)	; ?
    (let* ((str1
	    ;;(erc-prepare-mode-line-format '("[ ERC : " target " ] " "\n"))
	    (format "  %s          (%s)--------------------------------------[%s]"
		    (propertize (erc-format-target)
				'face '((:weight bold :foreground "white")))
		    (my-erc-prompt-magic-time-string)
		    (my-erc-prompt-color-erc)))
	   (string (concat str1 (my-erc-prompt-dashes (length str1)) "\n")))

      (dolist (props '((face ((:background "magenta"
			       :box (:line-width 2 :style released-button))))
		       (field erc-prompt)))
	(apply 'font-lock-append-text-property
	       0 (length string) (append props (list string))))
      string)))

(setq erc-prompt 'my-erc-prompt
      erc-command-indicator "ERC>")

(define-key erc-mode-map [remap font-lock-fontify-block] 'undefined)
