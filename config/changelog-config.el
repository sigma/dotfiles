;;; changelog-config.el ---

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

;;; Changelog

(eval-when-compile (require 'cl))

(when (request 'project)
  (defun yh/project-changelog-file ()
    (let ((rep (file-name-directory (buffer-file-name)))
          (projects (delete-if 'not (mapcar (lambda (p) (yh/project-get p 'root)) (yh/project-list)))))
      (mapcond (lambda (s) (string-match (expand-file-name s) rep))
               (lambda (s) (expand-file-name (concat s "/ChangeLog")))
               projects)))

  (defadvice add-change-log-entry (around ad-add-change-log-entry act)
    "Override default ChangeLog file according to project directory"
    (let ((change-log-default-name (yh/project-changelog-file)))
      ad-do-it))
  )

(add-hook 'change-log-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c")
                                    (lambda () (interactive) (save-buffer) (kill-this-buffer)))))

(provide 'changelog-config)
;;; changelog-config.el ends here
