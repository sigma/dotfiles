;;; python-config.el ---

;; Copyright (C) 2007  Free Software Foundation, Inc.

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
(if (request 'python-mode)
    (add-to-list 'hs-special-modes-alist
                 `(python-mode "^\\s-*\\(?:def\\|class\\)\\>" nil "#"
                               ,(lambda (arg)
                                  (py-end-of-def-or-class 'either)
                                  (skip-chars-backward " \t\n"))
                               nil))
  (require 'python))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;; SCons files are just python code
(add-to-list 'auto-mode-alist '("SCons\\(cript\\|truct\\)\\'" . python-mode))

(add-to-list 'interpreter-mode-alist '("ipython" . python-mode))

(request 'ipython)

(setq python-font-lock-keywords
      ;; same additional font-locking as in cc-mode
      (append python-font-lock-keywords
              (list
               '("[{}()<>=;,:+\\*\\/\\[]\\|\\]\\|\\-" (0 font-lock-keys-face))
               '("\\<[0-9]+\\>" (0 font-lock-number-face))
               '("\\<0x[0-9a-fA-F]+\\>" (0 font-lock-hexnumber-face))
               ;; PyQt specific
               '("\\<\\(S\\(IGNAL\\|LOT\\)\\|connect\\|disconnect\\|emit\\)\\>"
                 (0 font-lock-qt-face)))))

(add-hook 'python-mode-hook
          (lambda ()
            (make-variable-buffer-local 'beginning-of-defun-function)
            (setq beginning-of-defun-function 'py-beginning-of-def-or-class)
            (setq outline-regexp "def\\|class ")
            (hs-minor-mode 1)
            (glasses-mode 1)
            (c-subword-mode 1)))

(remove-hook 'python-mode-hook 'wisent-python-default-setup)

(provide 'python-config)
;;; python-config.el ends here
