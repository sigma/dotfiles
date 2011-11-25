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

(request 'slime-autoloads)

(eval-after-load 'slime
  '(progn
     ;; default
     (setq slime-edit-definition-fallback-function 'find-tag)
     (slime-setup '(slime-repl))

     (setq slime-net-coding-system 'utf-8-unix)

     (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

     (global-set-key (kbd "C-c SPC") 'slime-selector)
     (setq slime-lisp-implementations
           '((clisp ("clisp" "-K" "full"))
             (cmucl ("cmucl"))
             (sbcl ("sbcl" "--noinform"))))
     (setq slime-default-lisp 'sbcl)))

(provide 'slime-config)
;;; slime-config.el ends here
