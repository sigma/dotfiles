;;; save-history.el --- Save mini-buffer histories between sessions

;; Copyright (C) 2000 by Lars R. Clausen <lrclause@cs.uiuc.edu>>

;; Author: Lars R. Clausen <lrclause@cs.uiuc.edu>
;; Keywords: convenience

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

;; Hook-functions to load and save minibuffer histories between sessions.

;;; Code:

(defvar save-history-varlist
  '(;name-history
coding-system-history
command-history
extended-command-history
file-name-history
frame-name-history
input-method-history
;list-command-history
minibuffer-history
query-replace-history
read-expression-history
regexp-history
set-variable-value-history
shell-command-history
yes-or-no-p-history
)
  "A list of variables that should be saved by `save-history-save'.")

(defvar save-history-max-length 20
  "The maximum number of items that is saved for each history.
If this is nil, there is no limit.")

(defvar save-history-file
  "~/.emacs-histories"
  "The file in which minibuffer histories are saved.")

;; This function taken from desktop.el
;; We can be destructive because we're exiting when called anyway.
(defun save-history-truncate-list (l n)
  "Truncate LIST to at most N elements destructively and return LIST."
  (let ((here (nthcdr (1- n) l)))
    (if (consp here)
        (setcdr here nil))
    l))

(defun save-history-save () 
  "Save all histories in `save-history-varlist' to `save-history-file'"
  (let ((old-buffer (current-buffer))
        (histbuffer (find-file-noselect save-history-file t t)))
    (switch-to-buffer histbuffer)
    (if (> (buffer-size) 0)
        (delete-region 1 (buffer-size)))
    (insert "(setq\n")
    (mapcar (lambda (x)
              (if (and x (eval x))
                  (progn
                    (insert "  ")
                    (prin1 x histbuffer)
                    (insert "\n    '")
                    (if save-history-max-length
                        (let ((truncated (save-history-truncate-list
                                          (eval x) 
                                          save-history-max-length)))
                          (prin1 truncated histbuffer))
                      (prin1 (eval x) histbuffer))
                    (insert "\n"))))
            save-history-varlist)
    (insert ")\n")
    (basic-save-buffer)
    (switch-to-buffer old-buffer)))

(defun save-history-load ()
  "Load histories from `save-history-file'"
  (if (file-exists-p save-history-file)
      (load-file save-history-file)))

(add-hook 'after-init-hook 'save-history-load)
(add-hook 'kill-emacs-hook 'save-history-save)

(provide 'save-history)
;;; save-history.el ends here
