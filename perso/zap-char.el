;;; zap-char.el --- Vi style zap to/upto/from/following next typed char

;; Author: Sandip V. Chitale
;; Created: Apr 26 2004
;; Keywords: kill to/upto/from/following char

;; This file is not part of GNU Emacs yet.

;;; Commentary:
;;
;; This package enables kill chars to/upto/from/following next typed
;; char. This is simialr to the Vi commands
;;
;; d t char
;; d T char
;; d f char
;; d F char
;;
;; By default the char search is case sensitive. You can toggle the
;; case sensitivity using the `toggle-zap-char-case-fold-search'
;; command. You can customize the value of
;; `zap-char-case-fold-search'.
;;
;; Installation:
;;
;; To install and use, put this file on your Emacs-Lisp load path and add the
;; following into your ~/.emacs startup file:
;;
;;  (require 'zap-char)
;;
;; You may set the following key binding:
;;
;; (global-set-key [(control z)]               'zap-upto-char)
;; (global-set-key [(control meta z)]          'zap-to-char)
;; (global-set-key [(shift control z)]         'zap-following-char)
;; (global-set-key [(shift control meta z)]    'zap-from-char)
;;
;;
;;; Code:
(defgroup zap-char nil
  "Vi style zap-char functionality."
  :prefix "zap-"
  :group 'editing)

(defcustom zap-char-case-fold-search nil
  "*Non-nil means zap-*-char functions should ignore case."
  :type 'boolean
  :group 'zap-char)

(defun toggle-zap-char-case-fold-search ()
  "Toggle the case sensitivity of the zap-*-char functions."
  (interactive)
  (set-variable 'zap-char-case-fold-search (not zap-char-case-fold-search)))

(defun zap-from-char (arg char)
  "Kill from ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "*p\ncZap from char: ")
  (kill-region (point) (let ((case-fold-search zap-char-case-fold-search))
                         (search-backward (char-to-string char) nil nil arg)
                         (point))))

(defun zap-following-char (arg char)
  "Kill following ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "*p\ncZap following char: ")
  (kill-region (point) (let ((case-fold-search zap-char-case-fold-search))
                         (search-backward (char-to-string char) nil nil arg)
                         (forward-char)
                         (point))))

(defun zap-upto-char (arg char)
  "Kill up to ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "*p\ncZap upto char: ")
      (kill-region (point) (let ((case-fold-search zap-char-case-fold-search))
			     (while (looking-at (char-to-string char)) (forward-char))
			     (search-forward (char-to-string char) nil nil arg)
			     (backward-char)
			     (point))))

(defun zap-to-char (arg char)
  "Kill to ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "*p\ncZap to char: ")
  (kill-region (point) (let ((case-fold-search zap-char-case-fold-search))
			 (while (looking-at (char-to-string char)) (forward-char))
			 (search-forward (char-to-string char) nil nil arg)
			 (point))))

(provide 'zap-char)
