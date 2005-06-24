;;; latex-config.el ---

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

(defun LaTeX-env-slide (environment)
  (LaTeX-insert-environment environment
			    (concat TeX-grop
				    (read-string "Title: ")
				    TeX-grcl))
  (end-of-line 0)
  (delete-char 1)
  (delete-horizontal-space)
  (newline-and-indent))

(defun LaTeX-env-overlays (environment)
  (LaTeX-insert-environment environment
			    (concat TeX-grop
				    (read-string "Number: " "2")
				    TeX-grcl))
  (end-of-line 0)
  (delete-char 1)
  (delete-horizontal-space)
  (newline-and-indent))

(defun LaTeX-env-sigmalog (environment)
  (LaTeX-insert-environment environment)
  (end-of-line 0)
  (delete-char 1)
  (delete-horizontal-space)
  (newline-and-indent))

(setq reftex-plug-into-AUCTeX t)
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)

(defun my-LaTeX-hook ()
      ;; I like to have my own verbatim contructions well indented
      (setq font-lock-defaults
            '((font-latex-keywords font-latex-keywords-1 font-latex-keywords-2)
              nil nil
              ((40 . ".")
               (41 . ".")
               (36 . "\""))
              nil
              (font-lock-comment-start-regexp . "%")
              (font-lock-mark-block-function . mark-paragraph)
              (font-lock-syntactic-keywords
               ("^\\\\begin *{verbatim\\*?}\\(.?\\).*\\(\n\\)"
                (1 "<")
                (2 "|"))
               ("\\(\n\\)\\\\end *{verbatim\\*?}\\(.?\\)"
                (1 "|")
                (2 "<"))
               ("^\\\\begin *{sigmalog\\*?}\\(.?\\).*\\(\n\\)"
                (1 "<")
                (2 "|"))
               ("\\(\n\\)\\\\end *{sigmalog\\*?}\\(.?\\)"
                (1 "|")
                (2 "<"))
               ("\\\\verb\\*?\\([^a-z@]\\).*?\\(\\1\\)"
                (1 "\"")
                (2 "\"")))))
      (add-to-list 'LaTeX-style-list '("prosper"))

      (LaTeX-add-environments
       '("slide" LaTeX-env-slide)
       '("overlays" LaTeX-env-overlays)
       '("sigmalog" LaTeX-env-sigmalog))
      )

(when (request 'tex-site)
  (progn
    (setq-default TeX-master t)
    ;; reftex helps managing references, toc, ...
    (add-hook 'LaTeX-mode-hook 'reftex-mode)
    ;; show/hide parts of your document
    (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
    ;; preview-latex is great
    (when (request 'preview)
      (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup))
    ;; point my typos
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    ;; use abbrev
    (add-hook 'LaTeX-mode-hook 'abbrev-mode)
    ;; Most people don't want that... I do
    ;; highlight *any* change, color rotation
    (add-hook 'LaTeX-mode-hook 'highlight-changes-mode)
    ;;       ;; DWIM with quotes
                                        ;       (add-hook 'LaTeX-mode-hook 'typopunct-mode)
    (add-hook 'LaTeX-mode-hook 'my-LaTeX-hook)
    ))

(provide 'latex-config)
;;; latex-config.el ends here
