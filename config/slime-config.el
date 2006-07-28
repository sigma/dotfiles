;;; slime-config.el ---

;; Copyright (C) 2006  Free Software Foundation, Inc.

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

(autoload 'slime "slime" "Superior Lisp Interaction Mode for Emacs" t)

(autoload 'slime-mode "slime" "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode)." t)

(eval-after-load 'slime
  '(progn
     ;; default
     (setq slime-edit-definition-fallback-function 'find-tag)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     (slime-setup :autodoc t)
     (global-set-key (kbd "C-c SPC") 'slime-selector)
     (setq slime-lisp-implementations
           '((sbcl ("sbcl" "--noinform" "--no-linedit"))
             (s48 ("scheme48") :init slime48-init-command)))
     (setq slime-default-lisp 'sbcl)))

(autoload 'slime48-init-command "slime48"
  "Return a string to initialize Scheme48 running under SLIME.")

;; This snippet lets you specify a scheme48-package local variable,
;; in a file's -*- line or local variables section, and have SLIME48
;; automatically evaluate code in the right package.  For instance,
;; all of my Scheme48 source files start with:
;;   ;;; -*- Mode: Scheme; scheme48-package: ... -*-
(eval-after-load "slime48"
  '(add-hook 'slime-mode-hook
             (lambda ()
               (if (and (boundp 'scheme48-package)
                        scheme48-package)
                   (setq slime-buffer-package scheme48-package)))))

(provide 'slime-config)
;;; slime-config.el ends here
