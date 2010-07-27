;;; tmake-mode.el --- major mode for editing tmake project files

;; Copyright (C) 2000 Jan Borsodi

;; Author: Jan Borsodi <jb@ez.no>
;; Maintainer: jb@ez.no
;; Keywords: tmake
;; Created: 2000-07-01
;; Modified: 2000-07-18

(defconst tmake-version "0.1"
  "Tmake Mode version number.")

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Usage

;; Place this file in your Emacs lisp path (eg. site-lisp)
;; and add to your .emacs file:
;; (require 'tmake-mode)

;;; Commentary

;; Tmake mode is a major mode for editing tmake project files.

;; This mode officially supports only GNU Emacs. If you have have any
;; problems with it in XEmacs and/or has a tips on how to make it
;; work please report to me.

;;; Changelog

;; 0.1
;; 
;; Initial version, very basic

;; Requirements

(require 'font-lock)
(require 'custom)

;; The code

(defgroup tmake nil
  "Highlight or hide text according to tmake conditionals."
  :group 'text
  :prefix "tmake-")

(defcustom tmake-mode-hook nil
  "*Hook called by `tmake-mode'."
  :type 'hook
  :group 'tmake)

(defvar tmake-mode-map ()
  "Keymap used in tmake mode.")
(when (not tmake-mode-map)
  (setq tmake-mode-map (make-sparse-keymap))
  (define-key tmake-mode-map (read-kbd-macro "M-TAB") 'tmake-complete-property))

; (defun tmake-indent-line()
;   (interactive))

(defvar tmake-types
  (regexp-opt
   '("qt" "debug" "warn_on" "warn_off" "dll" "staticlib")))

(defvar tmake-font-lock-keywords
  (list
   ;; Comments
   (cons "^#.*$" '(0 font-lock-comment-face))
   ;; Variable assignment
   '("^\\([A-Za-z0-9_]+:\\)?[A-Za-z0-9_]+[ \t]*\\([\\+-\\*/]?=\\)[ \t]*\\(\\([ \t]*[^ \t\n\\]+\\)*\\)[ \t]*\\(\\\\[\n]\\)?"
     (0 font-lock-variable-name-face t t)
     (1 font-lock-type-face t t)
     (2 font-lock-builtin-face t t)
     (3 font-lock-string-face t t)
     (5 font-lock-constant-face t t))
   ;;
   (cons (concat "\\<\\(" tmake-types "\\)\\>") '(0 font-lock-type-face t t)))
  "Rules for highlighting tmake project files.")

;;;###autoload
(defun tmake-mode()
  "Major mode for editing tmake files.\n\n\\{tmake-mode-map}"
  (interactive)

  (kill-all-local-variables)

  ;; Indentation
;   (make-local-variable 'indent-line-function)
;   (setq indent-line-function 'tmake-indent-line)

  ; font-locking
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(tmake-font-lock-keywords nil t nil nil))

  ; Setting up typing shortcuts
  (make-local-variable 'skeleton-end-hook)
  (setq skeleton-end-hook nil)

  (setq case-fold-search t)

  (turn-on-font-lock)

  (use-local-map tmake-mode-map)

  (setq font-lock-maximum-decoration t)

  ;; Set up comments
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)

  ;; Make a menu keymap (with a prompt string)
  ;; and make it the menu bar item's definition.
  (define-key tmake-mode-map [menu-bar] (make-sparse-keymap))
  (define-key tmake-mode-map [menu-bar tmake]
    (cons "tmake" (make-sparse-keymap "tmake")))

  (setq comment-start "#"
 	comment-end   ""
 	comment-start-skip "#[ \t\n]*")

  ; Setting up syntax table
  (modify-syntax-entry ?\# "< b")

  (setq mode-name "tmake"
	major-mode 'tmake-mode)
  (run-hooks 'tmake-mode-hook))

;; Make tmake-mode the default mode for tmake source code buffers.
;;;###autoload
(setq auto-mode-alist
      (append
       '( ("\\.pro\\'" . tmake-mode))
       auto-mode-alist))

;; Set up font locking
(defconst tmake-font-lock-keywords-1 nil
  "Subdued level highlighting for tmake mode.")

(defconst tmake-font-lock-keywords-2 nil
  "Medium level highlighting for tmake mode.")

(defconst tmake-font-lock-keywords-3 nil
  "Gauchy level highlighting for tmake mode.")

(defvar tmake-font-lock-keywords tmake-font-lock-keywords-3
  "Default expressions to highlight in tmake mode.")


;; font-lock-add-keywords is only defined in GNU Emacs
(if (not (string-match "XEmacs" emacs-version))
    (font-lock-add-keywords 'tmake-mode tmake-font-lock-keywords))

(provide 'tmake-mode)
