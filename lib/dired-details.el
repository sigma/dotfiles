;;; dired-details.el -- make file details hide-able in dired

;; Copyright (C) 2003 Rob Giardina

;; Version: 1.2
;; Keywords: dired, hide
;; Author: Rob Giardina <rgiardin.ohmmanepadmespam@oracle.com>
;; Maintainer: Rob Giardina
;; Last updated: Aug 24, 2003
;; Contributors: Harold Maier

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; `dired-details-hide' makes dired buffers that look like this:
;;
;;  /private/rgiardin/lispHome:
;;  used 1264 available files
;;
;;  drwxr-xr-x   5 rgiardin g632         512 Jan 19  2003 ..
;;  -rw-r--r--   1 rgiardin svrtech     4141 Aug 23 17:07 dired-details.el
;;  -rw-r--r--   1 rgiardin svrtech     4141 Aug 23 17:07 my-really-really-long-I-mean-really-long-filename.el
;;  -rw-r--r--   1 rgiardin svrtech       56 Aug 23 17:07 linked-file.el -> /var/tmp/checkouts/linked-file.el
;;
;; look like this:
;;
;;  /private/rgiardin/lispHome/emacs.config:
;;  used 1264 available files
;;
;;  [...] ..
;;  [...] dired-details.el
;;  [...] my-really-really-long-I-mean-really-long-filename.el
;;  [...] linked-file.el -> [...]
;;
;; The functions `dired-details-show' and `dired-details-hide' will
;; toggle details on and off.
;;
;;
;; INSTALLATION:
;;
;; To make `dired-details-hide' active in all new dired buffers, add
;; the following to your .emacs:
;;
;; (require 'dired-details)
;; (dired-details-install)
;;
;; This also binds the following keys in dired buffers:
;;
;;   ) - dired-details-show
;;   ( - dired-details-hide
;;
;; CHANGES:
;;
;; * 1.1: Setup hide and show keybindings earlier than the first hide.
;; * 1.1: add dired-details-initially-hide customization as suggested by Harold Maier
;; * 1.2: extensive change to support subdirs in dired buffers

;;; customizable vars

(defgroup dired-details nil
  "Settings for the dired-details package."
  :group 'dired
  :prefix "dired-details-")

(defcustom dired-details-hidden-string "[...]"
  "*This string will be shown in place of the file details."
  :group 'dired-details
  :type 'string)

(defcustom dired-details-hide-link-targets t
  "*Hide link targets '-> lnTarget' regions of dired listings."
  :group 'dired-details
  :type 'boolean)
  

(defcustom dired-details-initially-hide t
  "*Hide dired details on entry to dired buffers."
  :group 'dired-details
  :type 'boolean)


;;; implementation

(defvar dired-details-internal-overlay-list nil)
(make-variable-buffer-local 'dired-details-internal-overlay-list)

(defvar dired-details-hidden nil)
(make-variable-buffer-local 'dired-details-hidden)

(defun dired-details-install()
  (eval-after-load "dired"
    '(progn
       (add-hook 'dired-after-readin-hook 'dired-details-activate)
      
       (defadvice dired-revert (before remember-the-details activate)
         (dired-details-delete-overlays)))))
  
(defun dired-details-activate()
  "Set up dired-details in a dired buffer. Called by
dired-after-readin-hook."
  (when dired-details-initially-hide
    (dired-details-hide))

  (local-set-key "(" 'dired-details-hide)
  (local-set-key ")" 'dired-details-show))

(defun dired-details-delete-overlays()
  (mapc '(lambda(list) (mapc 'delete-overlay
                             (cdr list)))
        dired-details-internal-overlay-list)
  (setq dired-details-internal-overlay-list nil))

(defun dired-details-toggle (&optional arg)
  "Toggle visibility of dired details.
With positiv prefix argument ARG hide the details, with negative show them."
  (interactive "P")
  (let ((new-hidden-state (if (null arg)
                              (not dired-details-hidden)
                            (> (prefix-numeric-value arg) 0))))
    (if new-hidden-state
        (dired-details-hide)
      (dired-details-show))))

(defun dired-details-hide()
  "Make an invisible, evaporable overlay for each file-line's details
in this dired buffer."
  (interactive)
  (unless (eq 'dired-mode major-mode)
    (error "dired-details-hide can only be called in dired mode"))

  (unless dired-details-hidden
    (save-excursion
      (save-restriction
        (widen)
        ;;hide each displayed subdirectory
        (mapc
         '(lambda( dir-and-pos )
            (let ((cached-overlays (assoc (car dir-and-pos)
                                          dired-details-internal-overlay-list)))
              (if cached-overlays
                  ;;reuse the existing overlays
                  (dired-details-frob-overlays t)
                ;;no existing overlays for this subdir, make 'em
                (let ((cache (list (car dir-and-pos))))
                  (goto-char (cdr dir-and-pos))
                  (dired-goto-next-file)
                  (while (not (eobp))
                    (dired-details-make-current-line-overlay cache)
                    (dired-next-line 1))
                  (setq dired-details-internal-overlay-list
                        (cons cache dired-details-internal-overlay-list))))))
         dired-subdir-alist)))
    (setq dired-details-hidden t)))

(defun dired-details-show()
  "Show whatever details a call to `dired-details-hide' may have
hidden in this buffer."
  (interactive)
  (unless (not dired-details-hidden)
    (dired-details-frob-overlays nil)
    (setq dired-details-hidden nil)))

(defun dired-details-make-current-line-overlay( cache )
  (let ((details                      ;hide the flags, size, owner, date, etc.
         (make-overlay
          (+ 2 (progn (beginning-of-line) (point)))
          (- (progn (dired-move-to-filename)(point)) 1)))
         
        (ln-target                    ;hide the destination of a symbolic link
         (when dired-details-hide-link-targets
           (if (progn (beginning-of-line)
                      (search-forward-regexp
                       "-> \\(.*\\)"
                       (save-excursion (end-of-line)(point)) t))
               (make-overlay (match-beginning 1) (match-end 1))))))

    ;;delete the overlay when the dired line goes away
    (overlay-put details 'evaporate t)
    (dired-details-hide-overlay details)

    (when ln-target
      (overlay-put ln-target 'evaporate t)
      (dired-details-hide-overlay ln-target))

    (let ((new (list details)))
      (if ln-target
          (setq details (cons ln-target details)))
      (setcdr cache (append new (cdr cache))))))

(defun dired-details-hide-overlay( o )
  (overlay-put o 'invisible t)
  (overlay-put o 'before-string dired-details-hidden-string))

(defun dired-details-show-overlay( o )
  (overlay-put o 'invisible nil)
  (overlay-put o 'before-string nil))

(defun dired-details-frob-overlays( hide )
  (if dired-details-internal-overlay-list
      (mapc '(lambda(list)
               (mapc (if hide 'dired-details-hide-overlay 'dired-details-show-overlay)
                     (cdr list)))
            dired-details-internal-overlay-list)))

(provide 'dired-details)
