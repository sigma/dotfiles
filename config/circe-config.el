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

(when (request 'circe)

  (setq circe-default-realname user-full-name
        circe-ignore-list nil
        circe-server-coding-system '(latin-1 . undecided)
        circe-server-auto-join-channels '(("^freenode$" "#emacs" "#fsbot" "#muse"))
        circe-nickserv-passwords `(("freenode" ,public-passwd)))

  (setq lui-max-buffer-size 30000
        lui-flyspell-p t
        lui-flyspell-alist '(("+linux.de" "german8")
                             ("." "american")))

  (eval-after-load "circe"
    '(progn
       (require 'lui-irc-colors)
       (add-to-list 'lui-pre-output-hook 'lui-irc-colors)
       (add-to-list 'circe-receive-message-functions
                    'fc-bitlbee-auth)))

  (defun fc-bitlbee-auth (nick user host command args)
    "Authenticate to a bitlbee server."
    (when (and (string= command "JOIN")
               (circe-server-my-nick-p nick))
      (with-circe-server-buffer
       (when (string= circe-server-network "bitlbee")
         (circe-server-send
          (format "PRIVMSG #bitlbee :identify %s"
                  bitlbee-passwd))))))

  (defun irc ()
    "Connect to IRC."
    (interactive)
    (circe "irc.freenode.net" "6667" "freenode" public-passwd "Sigma[Mtp]"))
  )

(provide 'circe-config)
;;; circe-config.el ends here
