;;; sawfish-config.el ---

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

;;; Sawfish : the ultimate window manager

;; Open sawfish-realted files with the proper mode
(setq auto-mode-alist (append '(("\\.sawfishrc$"  . sawfish-mode)
                                ("\\.jl$"         . sawfish-mode)
                                ("\\.sawfish/rc$" . sawfish-mode)) auto-mode-alist))

;; TODO: investigate why this code is not loaded from sawfish.el
(eval-after-load "sawfish"
  '(font-lock-add-keywords 'sawfish-mode
                           (list
                            ;; highlight define-*
                            (list
                             sawfish-defines-regexp
                             '(1 font-lock-keyword-face)
                             `(,(regexp-opt-depth sawfish-defines-regexp)
                               font-lock-variable-name-face nil t))
                            ;; extra keywords
                            (if sawfish-extra-keyword-list
                                (list (concat "\\<"
                                              `,(regexp-opt sawfish-extra-keyword-list)
                                              "\\>")
                                      '(0 font-lock-keyword-face)))
                            ;; highlight warnings
                            (if sawfish-warning-keyword-list
                                (list (concat "\\<"
                                              `,(regexp-opt sawfish-warning-keyword-list)
                                              "\\>")
                                      '(0 font-lock-warning-face prepend))))))

(provide 'sawfish-config)
;;; sawfish-config.el ends here
