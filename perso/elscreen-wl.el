;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen-wl.el 
;;
(defconst elscreen-wl-version "0.3.1 (November 27, 2002)")
;;
;; Author:   Naoto Morishima <naoto@morishima.net>
;;              Nara Institute of Science and Technology, Japan
;; Created:  March 24, 2001

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'elscreen-wl)
(require 'elscreen)
(require 'wl)

;
; code
;
(setq wl-draft-use-frame nil)

(defun elscreen-wl-draft-buffer-style (buf-name)
  (elscreen-create)
  (switch-to-buffer buf-name))
(setq wl-draft-buffer-style 'elscreen-wl-draft-buffer-style)
(setq wl-draft-reply-buffer-style 'elscreen-wl-draft-buffer-style)

(defun elscreen-wl-mail-setup ()
  (if (not wl-draft-use-frame)
    (progn
      (make-local-variable 'kill-buffer-hook)
      (add-hook 'kill-buffer-hook'(lambda () (elscreen-kill))))))
(add-hook 'wl-mail-setup-hook 'elscreen-wl-mail-setup)

(defun wl-jump-to-draft-buffer (&optional arg) 
  "Jump to the draft if exists."
  (interactive "P")
  (if arg
      (wl-jump-to-draft-folder)
    (let ((draft-bufs (wl-collect-draft))
          buf)
      (cond
       ((null draft-bufs)
        (message "No draft buffer exist."))
       (t
        (setq draft-bufs
              (sort (mapcar 'buffer-name draft-bufs)
                    (function (lambda (a b)
                                (not (string< a b))))))
        (if (setq buf (cdr (member (buffer-name)
                                   draft-bufs)))
            (setq buf (car buf))
          (setq buf (car draft-bufs)))
        (elscreen-get-screen-create (get-buffer buf)))))))
