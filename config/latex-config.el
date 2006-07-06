;;; latex-config.el --- Configuration for latex

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

;;; LaTeX

;; (request 'typopunct)
;; (typopunct-change-language 'francais t)

(defun turn-on-reftex-if-available ()
  (when (request 'reftex)
    (turn-on-reftex)))

(defun LaTeX-preview-setup-if-available ()
  (when (request 'preview)
    (LaTeX-preview-setup)))

(when (request 'tex-site)
  (progn
    ;; oddly auctex is contained twice
    (setq TeX-data-directory (file-name-directory TeX-data-directory))

    (setq-default TeX-master t)
    ;; reftex helps managing references, toc, ...
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex-if-available)
    ;; show/hide parts of your document
    (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
    ;; preview-latex is great
    (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup-if-available)
    ;; point my typos
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    ;; use abbrev
    (add-hook 'LaTeX-mode-hook 'abbrev-mode)
    ;;       ;; DWIM with quotes
                                        ;       (add-hook 'LaTeX-mode-hook 'typopunct-mode)
    ))

(eval-after-load 'tex
  '(progn
     (setq TeX-auto-save t
           TeX-newline-function 'newline-and-indent
           TeX-parse-self t)))

(eval-after-load 'latex
  '(progn
     (add-to-list 'LaTeX-command-style '("omega" "lambda"))))

(eval-after-load 'reftex
  '(setq reftex-plug-into-AUCTeX t
         reftex-enable-partial-scans t
         reftex-save-parse-info t
         reftex-use-multiple-selection-buffers t
         reftex-extra-bindings nil
         reftex-index-follow-mode t
         reftex-toc-follow-mode t))

(eval-after-load 'font-latex
  '(setq font-latex-fontify-script nil))

(provide 'latex-config)
;;; latex-config.el ends here
