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
     ;; make circe-query-face of highest priority
     (setq circe-track-faces-priorities
           (cons 'circe-query-face circe-track-faces-priorities))
     (require 'lui-irc-colors)
     (add-to-list 'lui-pre-output-hook 'lui-irc-colors)
     (require 'circe-highlight-all-nicks)
     (enable-circe-highlight-all-nicks)
     ;; give time for cloak installation
     (add-hook 'circe-server-connected-hook (lambda () (sit-for 1)))))

;; I want channel tracking by priority
(defun yh/lui-property (str)
  "Extract the relevant property from
`lui-track-faces-priorities' or nil"
  (car (delete-if-not
        (lambda (pr)
          (member pr lui-track-faces-priorities))
        (text-properties-at 0 str))))

(defun yh/sort-lui-tracks (a b)
  "Predicate to sort tracks by decreasing priorities"
  (let ((pra (yh/lui-property a))
        (prb (yh/lui-property b)))
    (cond ((null prb)
           t)
          ((null pra)
           nil)
          (t
           (and (member prb (memq pra lui-track-faces-priorities)) t)))))

(defadvice lui-track-set-modified-status (after lui-track-set-modified-status-after act)
  "Sort `lui-track-buffers' according to their priority"
  (sort lui-track-buffers 'yh/sort-lui-tracks)
  (setq lui-track-mode-line-buffers (lui-track-status))
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
(defadvice lui-track-set-modified-status
  (before lui-track-set-modified-status-before
          (buffer status faces) act)
  "Add `circe-query-face' to the list of faces if BUFFER is in
`circe-query-mode'"
  (with-current-buffer (get-buffer buffer)
    (when (eq major-mode 'circe-query-mode)
      (setq faces (cons 'circe-query-face faces)))))

;; /invite is highly useful for bitlbee
(defun circe-command-INVITE (who &optional channel)
  "Invite WHO in CHANNEL."
  (interactive)
  (if (not (or channel circe-chat-target))
      (circe-server-message "No target for invitation")
    (circe-server-send (format "INVITE %s %s"
                               who
                               (if (and channel
                                        (string-match "[^ ]" channel))
                                   channel
                                 circe-chat-target)))))

(provide 'circe-config)
;;; circe-config.el ends here
