;;; bbdb-config.el ---

;; Copyright (C) 2008  Free Software Foundation, Inc.

;; Author: Yann Hodique <yann.hodique@gmail.com>
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

;;; BBDB

(require 'bbdb)
(bbdb-initialize 'gnus 'message 'sc 'w3)

(eval-after-load 'gnus
  '(progn
     (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
     (add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)

     (setq gnus-score-find-score-files-function
           '(gnus-score-find-bnews bbdb/gnus-score))
     (setq bbdb/gnus-score-default 15)))

(eval-after-load 'message
  '(progn
     (bbdb-insinuate-message)
     (add-hook 'message-setup-hook 'bbdb-define-all-aliases)
     (add-hook 'message-mode-hook
               (lambda () (local-set-key [(meta tab)] 'bbdb-complete-name)))))

(eval-after-load 'sc
  '(progn
     (bbdb-insinuate-sc)))

(setq
 bbdb-offer-save 'yes
 bbdb-electric-p t
 bbdb-pop-up-target-lines 5
 bbdb-use-pop-up nil
 bbdb-north-american-phone-numbers-p nil)

(add-hook 'bbdb-list-hook 'my-bbdb-display-xface)

(defun my-bbdb-display-xface ()
  "Search for face properties and display the faces."
  (when (or (gnus-image-type-available-p 'xface)
            (gnus-image-type-available-p 'pbm))
    (save-excursion
      (goto-char (point-min))
      (let ((inhibit-read-only t); edit the BBDB buffer
            (default-enable-multibyte-characters nil); prevents corruption
            pbm faces)
        (while (re-search-forward "^           face: \\(.*\\)" nil t)
          (setq faces (match-string 1))
          (replace-match "" t t nil 1)
          (dolist (data (split-string faces ", "))
            (setq pbm (uncompface data))
            (if (gnus-image-type-available-p 'xface)
                (insert-image
                 (gnus-create-image
                  (concat "X-Face: " data)
                  'xface t :ascent 'center :face 'gnus-x-face))

              (let ((png (gnus-convert-face-to-png data)))
                (insert-image (gnus-create-image png 'png t)))
              ;; (when pbm
              ;;   (insert-image
              ;;    (gnus-create-image
              ;;     pbm 'pbm t :ascent 'center :face 'gnus-x-face)))
              )
            (insert " ")))))))

(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)

(setq bbdb-auto-notes-alist '(("X-Face" (".+" face 0 'replace))
                              ("Face" (".+" face 0 'replace))))

(provide 'bbdb-config)
;;; bbdb-config.el ends here
