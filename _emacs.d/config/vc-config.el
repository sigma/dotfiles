;;; vc-config.el --- Configuration for vc

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

(require 'vc)
(require 'ediff)

(defun ecb-vc-dir-managed-by-SVN (directory)
 "Return 'SVN if DIRECTORY is managed by SVN. nil if not."
 (and (file-exists-p (concat directory "/.svn/"))
      'SVN))

;;; vc-checkin-partially.el --- Check in some changes of a file into version control

;; Copyright (C) 2005 Matthias Koeppe
;; Based on code from vc.el, Copyright 1992,93,94,95,96,97,98,2000,2001  Free Software Foundation, Inc.

;; Author: Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;; Keywords: local

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Often I want to check in just a few changes of a version-controlled
;; file.  This can be done by checking out a clean copy, using ediff
;; to put the desired changes into it and then checking in that
;; version.  The command `vc-checkin-partially' automates this.

;;; Code:

(defun vc-checkin-partially ()
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (error "Not a file buffer."))
    (unless (vc-registered file)
      (error "Not under version control."))
    (vc-recompute-state file)
    (vc-mode-line file)
    (let ((state (vc-state file))
	  (version (vc-workfile-version file)))
      (unless (eq state 'edited)
	(error "Partial check-in only makes sense for locally edited files."))
      (let ((merge-file (concat (vc-version-backup-file-name file version 'manual) ".partial-merge"))
	    (config (current-window-configuration)))
	(vc-call checkout file nil version merge-file)
	(set-buffer (ediff-files file merge-file))
        (set (make-local-variable 'vc-ediff-windows) config)
	(set (make-local-variable 'vc-checkin-partially-file) file)
	(set (make-local-variable 'vc-checkin-partially-merge-file) merge-file)
	(set (make-local-variable  'ediff-quit-hook)
	     (lambda ()
	       (let ((buffer-A ediff-buffer-A)
		     (buffer-B ediff-buffer-B)
		     (windows vc-ediff-windows)
		     (file vc-checkin-partially-file)
		     (merge-file vc-checkin-partially-merge-file))
		 (ediff-cleanup-mess)
		 (set-buffer buffer-A)
		 (set (make-local-variable 'vc-checkin-partially-file) file)
		 (set (make-local-variable 'vc-checkin-partially-merge-file) merge-file)
		 (set (make-local-variable 'vc-checkin-partially-windows) windows)
		 (set-buffer buffer-B)
		 (unless (y-or-n-p "Commit this? ")
		   (error "Commit aborted."))
		 (save-buffer)
		 (vc-start-entry
		  file nil nil nil
		  "Enter a change comment."
		  (lambda (file rev comment)
		    (let ((windows (with-current-buffer
				       (or (get-file-buffer file) (current-buffer))
				     vc-checkin-partially-windows)))
		      (message "Checking in the selected changes to %s..." file)
		      ;; "This log message intentionally left almost blank".
		      ;; RCS 5.7 gripes about white-space-only comments too.
		      (or (and comment (string-match "[^\t\n ]" comment))
			  (setq comment "*** empty log message ***"))
		      (with-vc-properties
		       file
		       ;; Change buffers to get local value of vc-checkin-switches.
		       (with-current-buffer (or (get-file-buffer file) (current-buffer))
			 (progn
			   (let ((backup-file (concat file ".partial-merge-backup"))
				 (merge-file vc-checkin-partially-merge-file))
			     (rename-file file backup-file t)
			     (rename-file merge-file file t)
			     (set-buffer (get-file-buffer file))
			     (revert-buffer nil t t)
			     (vc-call checkin file rev comment)
			     (rename-file backup-file file t)
			     (set-buffer (get-file-buffer file))
			     (revert-buffer nil t nil) ; reverts again
			     )))
		       `((vc-state . nil)
			 (vc-checkout-time . ,(nth 5 (file-attributes file)))
			 (vc-workfile-version . nil)))
		      (message "Checking in %s...done" file)
		      (set-window-configuration windows)
		      (message "Magic has been done.")))
		  'vc-checkin-hook))))))))

(provide 'vc-config)
;;; vc-config.el ends here
