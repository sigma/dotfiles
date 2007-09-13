;;; completion-config.el --- Configuration for completion

;; Copyright (C) 2005  Free Software Foundation, Inc.

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

(request 'tempo-c++)
(request 'tempo-java)
(request 'tempo-lisp)
(request 'tempo-latex)

  (defvar tempo-snippets-source-map (make-sparse-keymap))
  (define-key tempo-snippets-source-map (kbd "<tab>") 'tempo-snippets-next-field)
  (define-key tempo-snippets-source-map (kbd "<backtab>") 'tempo-snippets-previous-field)
  (define-key tempo-snippets-source-map (kbd "C-m") 'tempo-snippets-clear-latest)

  (defadvice tempo-snippets-finish-source (before clear-post-overlay (o) act)
    (delete-overlay (overlay-get o 'tempo-snippets-post)))

  (defadvice tempo-snippets-insert-source (after install-custom-map act)
    (let ((overlay (car tempo-snippets-sources)))
      (overlay-put overlay 'keymap tempo-snippets-source-map)
      (overlay-put overlay 'tempo-snippets-post (point))))

  (defadvice tempo-snippets-insert-template (after install-post-map act)
    (dolist (s tempo-snippets-sources)
      (let ((pos (overlay-get s 'tempo-snippets-post)))
        (when (integerp pos)
          (let ((o (make-overlay pos (1+ pos))))
            (overlay-put o 'keymap tempo-snippets-source-map)
            (overlay-put s 'tempo-snippets-post o)))))
    ad-return-value)

(provide 'completion-config)
;;; completion-config.el ends here
