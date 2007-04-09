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

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;; SCons files are just python code
(add-to-list 'auto-mode-alist '("SCons\\(cript\\|truct\\)\\'" . python-mode))

(add-to-list 'interpreter-mode-alist '("ipython" . python-mode))

(eval-after-load 'python
  '(progn
     (setq python-font-lock-keywords
           ;; same additional font-locking as in cc-mode
           (append python-font-lock-keywords
                   (list
                    '("[{}()<>=;,:+\\*\\/\\[]\\|\\]\\|\\-" (0 font-lock-keys-face))
                    '("\\<[0-9]+\\>" (0 font-lock-number-face))
                    '("\\<0x[0-9a-fA-F]+\\>" (0 font-lock-hexnumber-face))
                    ;; PyQt specific
                    '("\\<\\(S\\(IGNAL\\|LOT\\)\\|connect\\|disconnect\\|emit\\)\\>"
                      (0 font-lock-qt-face)))))))

(add-hook 'python-mode-hook
          (lambda ()
            (glasses-mode 1)
            (c-subword-mode 1)))

(provide 'python-config)
;;; python-config.el ends here
