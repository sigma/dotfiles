;;; ecb-config.el ---

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
  '(progn
     (require 'winring)
     (require 'ecb)))

(defun yh/compilation-predicate (buffer)
  (and (comint-check-proc buffer)
       (with-current-buffer buffer
         (not (or (eq major-mode 'circe-server-mode)
                  (eq major-mode 'message-mode))))))

(defun yh/init-ecb ()
  (require 'ecb-winman-support)
  (ecb-winman-winring-enable-support)
  (winring-initialize)
  (winring-rename-configuration ecb-winman-winring-name)
  (setq ecb-compilation-predicates '(yh/compilation-predicate)))

(eval-after-load 'ecb
  '(yh/init-ecb))

(provide 'ecb-config)
;;; ecb-config.el ends here
