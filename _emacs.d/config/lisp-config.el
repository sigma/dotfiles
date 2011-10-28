;;; lisp-config.el --- Configuration for lisp

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

;;; Lisp

(eval-when (load)
  (require 'patches))

(request 'edebug)

(defun elint-current-buffer ()
  (interactive)
  (when (request 'elint)
    (elint-initialize)
    (elint-current-buffer)))

(eval-after-load 'elint
  '(progn
     (add-to-list 'elint-standard-variables 'current-prefix-arg)
     (add-to-list 'elint-standard-variables 'command-line-args-left)
     (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
     (add-to-list 'elint-standard-variables 'emacs-major-version)
     (add-to-list 'elint-standard-variables 'window-system)))

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<C-right>") 'forward-word)
     (define-key paredit-mode-map (kbd "<C-left>") 'backward-word)
     (define-key paredit-mode-map (kbd "<C-M-right>") 'forward-sexp)
     (define-key paredit-mode-map (kbd "<C-M-left>") 'backward-sexp)))

(defun yh/lisp-hook()
  (turn-on-eldoc-mode)
  (try (hs-minor-mode 1))
  (paredit-mode 1))

(add-mhook '(emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook lisp-mode-hook slime-repl-mode-hook)
           'yh/lisp-hook t)

(defun yh/insert-elisp-key ()
  (interactive)
  (insert (concat "(kbd \""
                  (help-key-description (read-key-sequence "Key: ") nil)
                  "\")")))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'emacs-lisp-byte-compile)
(define-key emacs-lisp-mode-map (kbd "C-c k") 'yh/insert-elisp-key)

(font-lock-add-keywords 'emacs-lisp-mode
                        `((,(concat "\\<" (regexp-opt
                                           '("add-hook" "add-mhook"
                                             "autoload" "defmadvice"
                                             "aset" "set" "fset"
                                             "remove-hook" "clear-hook"
                                             "request" "make-double-command"
                                             "ert-deftest") t)
                                    "\\>[ 	']*\\(\\sw+\\)?")
                           (1 font-lock-keyword-face)
                           (2 font-lock-constant-face nil t))
                          ))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (request 'guile-c)
              (define-key c-mode-map "\C-c\C-g\C-p" 'guile-c-insert-define)
              (define-key c-mode-map "\C-c\C-g\C-e" 'guile-c-edit-docstring)
              (define-key c-mode-map "\C-c\C-g\C-d" 'guile-c-deprecate-region)
              )))

(font-lock-add-keywords 'guile-scheme-mode
                        `((,(concat "\\<" (regexp-opt '("defun" "defvar" "defmacro" "defmacro*" "defface") t)
                                    "\\>[ 	']*\\(\\sw+\\)?")
                           (1 font-lock-keyword-face)
                           (2 font-lock-constant-face nil t))
                          (,(concat "\\<" (regexp-opt '("export") t) "\\>")
                           (1 font-lock-keyword-face))
                          ))

(add-to-list 'auto-mode-alist '("\\.scm\\'" . guile-scheme-mode))

(defun define-lisp-indent-function (sym val)
  (put sym 'lisp-indent-function val))

(put 'define-lisp-indent-function 'safe-local-eval-function t)
(put 'font-lock-add-keywords 'safe-local-eval-function t)

(provide 'lisp-config)
;;; lisp-config.el ends here
