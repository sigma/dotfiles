;;; tempo-lisp.el --- abbrevs for lisp programming

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Yann Hodique <Yann.Hodique@lifl.fr>
;; Keywords: lisp, languages

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

(require 'tempo)

(defvar elisp-tempo-tags nil)
(defvar elisp-tempo-keys-alist nil)

(yh/tempo-define-template "elisp-defun"
                       "f"
                       'elisp-tempo-keys-alist
		       '("defun " p " (" p ")" n> "\"" p "\"" n> r> ")")
		       "defun"
		       "Insert a defun expression"
		       'elisp-tempo-tags)

(yh/tempo-define-template "elisp-defvar"
                       "v"
                       'elisp-tempo-keys-alist
		       '("defvar " p  n> "\"" p "\")")
		       "defvar"
		       "Insert a defvar expression"
		       'elisp-tempo-tags)

(yh/tempo-define-template "elisp-if"
                       "i"
                       'elisp-tempo-keys-alist
		       '("if " p n> r> ")")
		       "if"
		       "Insert an if expression"
		       'elisp-tempo-tags)

(yh/tempo-define-template "elisp-cond"
                       "c"
                       'elisp-tempo-keys-alist
		       '("cond ((" p ") " r> "))")
		       "cond"
		       "Insert a cond expression"
		       'elisp-tempo-tags)

(defun elisp-tempo ()
   "Set up emacs-lisp mode to use tempo.el"
   (local-set-key [M-S-tab] 'tempo-complete-tag)
   (local-set-key "\C-c\C-f" 'tempo-forward-mark)
   (local-set-key "\C-c\C-b" 'tempo-backward-mark)
;   (local-set-key " " 'tempo-space)
   (setq tempo-match-finder "(\\([^\\b]+\\)\\=")
   (tempo-use-tag-list 'elisp-tempo-tags)
   (yh/tempo-build-local-map elisp-tempo-keys-alist))

(add-hook 'emacs-lisp-mode-hook 'elisp-tempo)

(provide 'tempo-lisp)
;;; tempo-lisp.el ends here
