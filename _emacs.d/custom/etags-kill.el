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
