;;; circe-config.el ---

;; Copyright (C) 2005  Free Software Foundation, Inc.

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

(require 'patches)

(autoload 'circe "circe" "Connect to an IRC server" t)

(setq circe-default-realname user-full-name
      circe-ignore-list nil
      circe-server-coding-system '(utf-8 . undecided))

(setq lui-max-buffer-size 30000
      lui-flyspell-p t)

(eval-after-load 'circe
  '(progn
     ;; enable logging
     (require 'lui-logging)
     (add-hook 'circe-chat-mode-hook 'enable-lui-logging)
     (add-hook 'circe-query-mode-hook 'enable-lui-logging)
     ;; make circe-query-face of highest priority
     (setq circe-track-faces-priorities
           (cons 'circe-query-face circe-track-faces-priorities))
     (require 'lui-irc-colors)
     (add-to-list 'lui-pre-output-hook 'lui-irc-colors)
          ;; give time for cloak installation
     (add-hook 'circe-server-connected-hook (lambda () (sit-for 1)))))

;; I want channel tracking by priority
(defun yh/lui-property (str)
  "Extract the relevant property from
`tracking-faces-priorities' or nil"
  (car (delete-if-not
        (lambda (pr)
          (member pr tracking-faces-priorities))
        (text-properties-at 0 str))))

(defun yh/sort-lui-tracks (a b)
  "Predicate to sort tracks by decreasing priorities, then by
lexicographic order"
  (let ((pra (yh/lui-property a))
        (prb (yh/lui-property b)))
    (cond ((eq pra prb)
           (string-lessp a b))
          ((null prb)
           t)
          ((null pra)
           nil)
          (t
           (and (member prb (memq pra tracking-faces-priorities)) t)))))

(defadvice tracking-add-buffer (after tracking-add-buffer-after act)
  "Sort `tracking-buffers' according to their priority"
  (let ((l (length tracking-buffers)))
    (setq tracking-buffers (sort tracking-buffers 'yh/sort-lui-tracks))
    (assert (equal l (length tracking-buffers))))
  (setq tracking-mode-line-buffers (tracking-status))
  (sit-for 0) ;; Update mode line
  )

;; Additional face for query buffer tracking
(defvar circe-query-face 'circe-query-face
  "The face used to highlight query messages.")
(defface circe-query-face
  '((((type tty)) :foreground "red" :weight bold)
    (t (:foreground "red")))
  "The face used to highlight query messages."
  :group 'circe)

;; I want query buffer to be tracked with high priority even if no face is
;; present
(defadvice tracking-add-buffer
  (before tracking-add-buffer-before
          (buffer &optional faces) act)
  "Add `circe-query-face' to the list of faces if BUFFER is in
`circe-query-mode'"
  (with-current-buffer (get-buffer buffer)
    (when (eq major-mode 'circe-query-mode)
      (setq faces (cons 'circe-query-face faces)))))

(defun circe-channels-for-current-server ()
  (interactive)
  (when (or (eq major-mode 'circe-channel-mode)
            (eq major-mode 'circe-query-mode))
    (let ((server-buffer circe-server-buffer))
      (delete nil (mapcar #'(lambda (buf)
                              (with-current-buffer buf
                                (and (eq major-mode 'circe-channel-mode)
                                     (eq server-buffer circe-server-buffer)
                                     (buffer-name buf))))
                          (buffer-list))))))

;; /invite is highly useful for bitlbee
(defun circe-command-INVITE (who &optional channel)
  "Invite WHO in CHANNEL."
  (interactive
   (let ((w (read-from-minibuffer "Who: "))
         (c (let ((channels (circe-channels-for-current-server)))
              (completing-read "Channel: " channels nil nil
                               (and (member circe-chat-target channels)
                                    circe-chat-target)))))
     (list w c)))

  (if (not (or channel circe-chat-target))
      (circe-server-message "No target for invitation")
    (circe-server-send (format "INVITE %s %s"
                               who
                               (if (and channel
                                        (string-match "[^ ]" channel))
                                   channel
                                 circe-chat-target)))))

(defun circe-command-RECOVER (nick)
  (interactive "sNick: ")
  (circe-command-MSG "NickServ"
                     (format "recover %s %s" nick public-passwd))
  (circe-command-MSG "NickServ"
                     (format "release %s %s" nick public-passwd))
  (circe-command-NICK nick))

(defun circe-clear ()
  (interactive)
  (save-excursion
    (let ((lui-max-buffer-size 10000))
      (lui-truncate))))

(add-hook 'lui-mode-hook
          (lambda ()
            (define-key lui-mode-map (kbd "C-c C-o") 'circe-clear)))

(provide 'circe-config)
;;; circe-config.el ends here
