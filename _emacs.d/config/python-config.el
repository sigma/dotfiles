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
(request 'python)

(add-to-list 'hs-special-modes-alist
             `(python-mode "^\\s-*\\(?:def\\|class\\)\\>" nil "#"
                           ,(lambda (arg)
                              (python-end-of-defun)
                              (skip-chars-backward " \t\n"))
                           nil))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;; SCons files are just python code
(add-to-list 'auto-mode-alist '("SCons\\(cript\\|truct\\)\\'" . python-mode))

(add-to-list 'interpreter-mode-alist '("ipython" . python-mode))

(setq python-font-lock-keywords
      ;; same additional font-locking as in cc-mode
      (append python-font-lock-keywords
              (list
               '("[{}()<>=;,:+\\*\\/\\[]\\|\\]\\|\\-" (0 font-lock-keys-face))
               '("\\<[0-9]+\\>" (0 font-lock-number-face))
               '("\\<0x[0-9a-fA-F]+\\>" (0 font-lock-hexnumber-face))
               ;; PyQt specific
               '("\\<\\(S\\(IGNAL\\|LOT\\)\\|connect\\|disconnect\\|emit\\)\\>"
                 (0 font-lock-qt-face))))
      py-pdbtrack-stack-entry-regexp
      "^> \\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()")

(add-hook 'python-mode-hook
          (lambda ()
            (make-variable-buffer-local 'beginning-of-defun-function)
            (setq beginning-of-defun-function 'python-beginning-of-defun)
            (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
            (setq outline-regexp "def\\|class ")
            (hs-minor-mode 1)
            (glasses-mode 1)
            (subword-mode 1)))

;; just in case, semantic is a bit too intrusive and buggy here
(remove-hook 'python-mode-hook 'wisent-python-default-setup)

(request 'cython-mode)

(when (request 'elpy)
  (elpy-enable)
  (elpy-use-ipython)
  (elpy-clean-modeline))

(provide 'python-config)
;;; python-config.el ends here
