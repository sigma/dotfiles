;;; project.el --- set some properties according to current project

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Yann Hodique <Yann.Hodique@lifl.fr>
;; Keywords: convenience

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

;; Map minimal project concep on symbol in dedicated obarray. Fun and useless

;;; Code:

(require 'cl)

(defvar yh/project-obarray (make-vector 100 0) "List of active projects")

(defun yh/project-new (name)
  (intern name yh/project-obarray))

(defun yh/project-set (name &rest pairs)
  (let ((s (intern name yh/project-obarray)))
    (dolist (p pairs)
      (put s (car p) (cdr p)))))

(defun yh/project-get (name prop)
  (let ((s (intern name yh/project-obarray)))
    (get s prop)))

(defun yh/project-list ()
  (let ((l nil))
    (mapatoms (lambda (s)
                (add-to-list 'l (symbol-name s)))
              yh/project-obarray) l))

(provide 'project)
;;; project.el ends here
