;;; scratch-config.el --- scratch configuration

;; Copyright (C) 2011  Free Software Foundation, Inc.

;; Author: Yann Hodique <yann.hodique@gmail.com>
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

(defun yh/mode-scratch-list-modes ()
  (let ((modes nil))
    (mapatoms (lambda (x)
                (let ((name (format "%s" x)))
                  (and (string-match "\\(.*\\)-mode" name)
                       (fboundp x)
                       (commandp (symbol-function x))
                       (add-to-list 'modes (match-string 1 name))))))
    modes))

(defvar yh/mode-scratch-modes
  (yh/mode-scratch-list-modes))

(defun yh/mode-scratch-get-mode (arg)
  (or (and (null arg)
           major-mode)
      (completing-read "Mode: " yh/mode-scratch-modes)))

(defun yh/mode-scratch-create (mode)
  (interactive (list (yh/mode-scratch-get-mode current-prefix-arg)))
  (let* ((raw-name (format "%s" mode))
         (name (if (string-match "\\(.*\\)-mode" raw-name)
                   (match-string 1 raw-name)
                 raw-name))
         (buf (get-buffer-create (format "*scratch-%s*" name))))
    (with-current-buffer buf
      (let ((m (intern (format "%s-mode" name))))
        (funcall m)))
    (pop-to-buffer buf)))

(global-set-key (kbd "M-`") 'yh/mode-scratch-create)


;; If the *scratch* buffer is killed, recreate it automatically
;; FROM: Morten Welind
;;http://www.geocrawler.com/archives/3/338/1994/6/0/1877802/
(defun prepare-scratch-for-kill ()
  (save-excursion
    (set-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode)
    (make-local-variable 'kill-buffer-query-functions)
    (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)))

(defun kill-scratch-buffer ()
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (prepare-scratch-for-kill)
  ;; Since we killed it, don't let caller do that.
  nil)

(prepare-scratch-for-kill)

;; now setup *scratch* correctly
(kill-scratch-buffer)

(provide 'scratch-config)
;;; scratch-config.el ends here
