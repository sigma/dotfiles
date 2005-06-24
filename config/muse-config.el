;;; muse-config.el ---

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

;;; Muse

(when (request 'muse-mode)
  (progn
    (request 'muse-html)
    (request 'muse-latex)
    (request 'muse-texinfo)
    (request 'muse-docbook)
    (request 'muse-wiki)
    (setq muse-mode-highlight-p t)))

(add-to-list 'auto-mode-alist '("\\.muse$"  . muse-mode))

(defvar xtla-transform-alist
  '(("hodique@lifl.fr--2005" . "http://www.lifl.fr/~hodique/archives")))

(defun xtla-url-transform (target)
  (tla--archive-tree-build-archives t)
  (let ((reg (concat "^xtla://\\("
                     (mapconcat 'car tla--archive-tree "\\|")
                     "\\)/\\(.*\\)$")))
    (save-match-data
      (if (string-match reg target)
          (let ((subst (or (cdr (assoc (match-string 1 target) xtla-transform-alist))
                           (cadr (assoc (match-string 1 target) tla--archive-tree)))))
            (concat subst "/"
                    (replace-regexp-in-string "--" "/" (match-string 2 target))))
        target))))

(add-to-list 'muse-publish-url-transforms 'xtla-url-transform)

(defun local-file-url-transform (target)
  (if (and (string-match muse-file-regexp target)
           (not (or (string-match muse-url-regexp target)
                    (string-match muse-image-regexp target))))
      (expand-file-name target)
    target))

(add-to-list 'muse-publish-url-transforms 'local-file-url-transform)

(defun gnus-url-transform (target)
  (save-match-data
    (if (string-match "^gnus://" target)
        ""
      target)))

;(add-to-list 'muse-publish-url-transforms 'gnus-url-transform)

(provide 'muse-config)
;;; muse-config.el ends here
