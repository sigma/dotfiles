;;; cheat-font-lock.el --- modify font-lock-keywords
;;; Copyright (c) 2002, 2003, 2004
;;;   by HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: cheat-font-lock.el,v 1.7 2004/01/21 13:03:51 hira Exp $
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------

;; depends on internal implementation of font-lock.el

;; renamed from howm-font-lock.el [2003-12-12]

(require 'font-lock)

;; see (find-file "/usr/share/emacs/21.2/lisp/font-lock.el")
(defun cheat-font-lock-compiled-p (keywords)
  (eq (car-safe keywords) t))
(defun cheat-font-lock-compiled-body (keywords)
  (cdr keywords))

(defun cheat-font-lock-keywords (keywords)
  (if (cheat-font-lock-compiled-p keywords)
      (cheat-font-lock-compiled-body keywords)
    keywords))

(defun cheat-font-lock-merge-keywords (&rest keywords-list)
  (let ((bodies-list (mapcar #'cheat-font-lock-keywords keywords-list)))
    (setq font-lock-keywords
          (apply #'append bodies-list))))
(defun cheat-font-lock-append-keywords (entries)
  (cheat-font-lock-merge-keywords font-lock-keywords entries))
(defun cheat-font-lock-prepend-keywords (entries)
  (cheat-font-lock-merge-keywords entries font-lock-keywords))

(defun cheat-font-lock-mode (&optional silent)
  ;; For xemacs. But this seems to have no effect. ;_; [2004-01-14]
  (when silent
    (set (make-local-variable 'font-lock-verbose) nil))
  ;; Keywords are not highlighted on the fly in emacs-21.3.50.1
  ;; when font-lock-defaults is nil. I don't understand this. [2003-11-28]
  (when (null font-lock-defaults)
    (set (make-local-variable 'font-lock-defaults) '(nil)))
  ;; Without the next line, global value is changed to t. [2003-12-30]
  ;; (emacs-20.7.2 on Vine Linux 2.6)
  (make-local-variable 'font-lock-fontified)
  (let ((font-lock-fontified t)) ;; adjourn fontify-buffer
    (font-lock-mode 1))
  (font-lock-set-defaults))

(provide 'cheat-font-lock)

;;; cheat-font-lock.el ends here
