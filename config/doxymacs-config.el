;;; doxymacs-config.el ---

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

;;; Doxymacs : great documentation system

(add-hook 'c-mode-common-hook 'doxymacs-mode)
(eval-after-load "doxymacs"
  '(progn
     (setq doxymacs-relative-path "Doc/html"
           doxymacs-use-external-xml-parser t)
     (defun my-doxymacs-font-lock-hook ()
       (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
           (font-lock-add-keywords nil doxymacs-doxygen-keywords)
         ))
     (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)))

(provide 'doxymacs-config)
;;; doxymacs-config.el ends here
