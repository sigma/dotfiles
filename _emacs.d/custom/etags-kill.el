;;; etags-kill.el --- kill useless buffers when browsing tags

;; Copyright (C) 2009  Free Software Foundation, Inc.

;; Author: Yann Hodique <yhodique@vmware.com>
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

;; This code will kill buffers that:
;; - have been opened by `find-tag' (was not open before)
;; - are not living anymore in the tag stack (you just M-* from it)

;; An exception is if user uses C-u M-* instead. In this case they will be
;; considered as already open for any future `pop-tag-mark'

;; Limitation: this will work only if `pop-tag-mark' is run from the
;; buffer. This is how I browse, so I don't see it as a major limitation.

;;; Code:

(defvar yh/destroy-buffer-when-pop nil)
(make-variable-buffer-local 'yh/destroy-buffer-when-pop)

(defvar yh/orig-find-file-noselect (symbol-function 'find-file-noselect))

(defun yh/find-file-noselect-and-record (file &optional nowarn rawfile wildcards)
  (let ((buffer (funcall yh/orig-find-file-noselect file nowarn rawfile wildcards)))
    (with-current-buffer buffer
      (setq yh/destroy-buffer-when-pop t))
    buffer))

(defadvice tag-find-file-of-tag-noselect (around yh/tag-ff-around (file) act)
  (flet ((find-file-noselect (file &optional nowarn rawfile wildcards)
                             (yh/find-file-noselect-and-record file nowarn rawfile wildcards)))
    ad-do-it))

(defadvice pop-tag-mark (around yh/tag-pop-around () act)
  (let* ((yh/buffer (current-buffer))
         (yh/pop-destroy-buffer (buffer-local-value 'yh/destroy-buffer-when-pop yh/buffer)))
    (when current-prefix-arg
      (setq yh/pop-destroy-buffer nil)
      (with-current-buffer yh/buffer
        (setq yh/destroy-buffer-when-pop nil)))
    ad-do-it
    (if (and yh/pop-destroy-buffer (not (eq yh/buffer (current-buffer))))
        (kill-buffer yh/buffer))))

;; need for an advice here since it can be called from `tag-find-file-of-tag-noselect'
(defadvice visit-tags-table-buffer (around yh/visit-tags-around (&optional cont) act)
  (flet ((find-file-noselect (file &optional nowarn rawfile wildcards)
                             (funcall yh/orig-find-file-noselect file nowarn rawfile wildcards)))
    ad-do-it))

(provide 'etags-kill)
;;; etags-kill.el ends here
