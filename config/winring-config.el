;;; winring-config.el ---

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

(eval-when-compile
  '(require 'winring)
  '(require 'patches))

(defun yh/winring-fix-switch ()
  (defmadvice (winring-next-configuration winring-prev-configuration)
    (around winring act)
    "Creation of new configuration if only one existing"
    (condition-case nil
        ad-do-it
      (error (winring-new-configuration t)))))

(eval-after-load 'winring
  '(yh/winring-fix-switch))

(provide 'winring-config)
;;; winring-config.el ends here