;;; highlight-changes--config.el --- Configuration for highlight

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

;;; Highlight-changes
(add-hook 'highlight-changes-enable-hook
          (lambda ()
            (local-set-key "\C-c+" 'highlight-changes-next-change)
            (local-set-key "\C-c-" 'highlight-changes-previous-change)
            (local-set-key (kbd "C-c DEL")
                           (lambda ()
                             (interactive)
                             (let ((mod (buffer-modified-p)))
                               (highlight-changes-remove-highlight (point-min) (point-max))
                               (restore-buffer-modified-p mod))))
            (local-set-key "\C-c_" 'highlight-changes-rotate-faces)))

(provide 'highlight-changes-config)
;;; highlight-changes--config.el ends here
