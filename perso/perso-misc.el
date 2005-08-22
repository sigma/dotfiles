;;; perso-misc.el --- misc functions

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


;; TODO: needs some work
;;;###autoload
(defun yh/c-rearrange-electrics ()
  "Rearrange electric chars according to current c-style"
  (interactive)
  (save-excursion
    (mapcar (lambda (symb)
              (goto-char (point-min))
              (while (search-forward (car symb) (point-max) t)
                (let ((p (- (point) 1)))
                  (back-to-indentation)
                  (if (equal p (point))
                      (progn
                        (delete-indentation)
                        (if (eq (cdr symb) '+)
                            (forward-char)))
                    (goto-char p))
                  (delete-char 1)
                  (execute-kbd-macro (car symb)))))
            '(("{" +) ("}" -)))))

;;;###autoload
(defun simplify-blank-lines ()
  "Delete extra blank lines"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (string ?^ 10 10) nil t)
      (backward-char)
      (delete-blank-lines))))

;;;###autoload
(defun find-file-guessing (arg)
  "Call find-file with file at point if valid. With a universal argument, force call to find-file"
  (interactive "P")
  (let ((target (and (not arg) (request 'ffap) (ffap-guesser))))
    (if target
        (ffap)
      (call-interactively 'find-file))))

;; convert a buffer from dos ^M end of lines to unix end of lines
;;;###autoload
(defun dos2unix ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t) (replace-match ""))))

;; vice versa
;;;###autoload
(defun unix2dos ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "\r\n"))))

;; ASCII table function
;;;###autoload
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)  (switch-to-buffer "*ASCII*")  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)      (setq i (1+ i))
           (insert (format "%4d %c\n" i i))))  (beginning-of-buffer))

;;;###autoload
(defun yank-and-forward-line ()
  (interactive)
  (let ((old-col (current-column)))
    (yank)
    (forward-line)
    (while (and (not (eolp)) (> old-col 0))
      (forward-char)
      (setq old-col (1- old-col)))))

;;;###autoload
(defun totd ()
  (interactive)
  (with-output-to-temp-buffer "*Tip of the day*"
    (let* ((commands (loop for s being the symbols
                           when (commandp s) collect s))
           (command (nth (random (length commands)) commands)))
      (princ
       (concat "Your tip for the day is:\n========================\n\n"
               (describe-function command)
               "\n\nInvoke with:\n\n"
               (with-temp-buffer
                 (where-is command t)
                 (buffer-string)))))))

;;;###autoload
(defun kill-syntax-forward ()
  "Kill characters with syntax at point."
  (interactive)
  (let ((beg (point)))
    (skip-syntax-forward (string (char-syntax (char-after))))
    (kill-region beg (point))))

;;;###autoload
(defun kill-syntax-backward ()
  "Kill characters with syntax at point."
  (interactive)
  (let ((beg (point)))
    (skip-syntax-backward (string (char-syntax (char-before))))
    (kill-region beg (point))))

;;;###autoload
(defun my-occur ()
  "Switch to *Occur* buffer, or run `moccur'."
  (interactive)
  (if (get-buffer "*Moccur*")
      (switch-to-buffer "*Moccur*")
    (call-interactively 'moccur)))

;;;###autoload
(defun diff-buffer-with-associated-file ()
  "View the differences between BUFFER and its associated file.
This requires the external program \"diff\" to be in your `exec-path'.
Returns nil if no differences found, 't otherwise."
  (interactive)
  (let ((buf-filename buffer-file-name)
        (buffer (current-buffer)))
    (unless buf-filename
      (error "Buffer %s has no associated file" buffer))
    (let ((diff-buf (get-buffer-create
                     (concat "*Assoc file diff: "
                             (buffer-name)
                             "*"))))
      (with-current-buffer diff-buf
        (setq buffer-read-only nil)
        (erase-buffer))
      (let ((tempfile (make-temp-file "buffer-to-file-diff-")))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (write-region (point-min) (point-max) tempfile nil 'nomessage))
              (if (zerop
                   (apply #'call-process "diff" nil diff-buf nil
                          (append
                           (when (and (boundp 'ediff-custom-diff-options)
                                      (stringp ediff-custom-diff-options))
                             (list ediff-custom-diff-options))
                           (list buf-filename tempfile))))
                  (progn
                    (message "No differences found")
                    nil)
                (progn
                  (with-current-buffer diff-buf
                    (goto-char (point-min))
                    (if (fboundp 'diff-mode)
                        (diff-mode)
                      (fundamental-mode)))
                  (display-buffer diff-buf)
                  t)))
          (when (file-exists-p tempfile)
            (delete-file tempfile)))))))

;;;###autoload
(defun de-context-kill (arg)
  "Kill buffer, taking gnuclient into account."
  (interactive "p")
  (catch 'return
    (when (and (buffer-modified-p)
               buffer-file-name
               (not (string-match "\\*.*\\*" (buffer-name)))
               ;; erc buffers will be automatically saved
               (not (eq major-mode 'erc-mode))
               (= 1 arg))
      (let ((differences 't))
        (when (file-exists-p buffer-file-name)
          (setq differences (diff-buffer-with-associated-file)))
        (message (if differences
                   "Buffer has unsaved changes"
                 "Buffer has unsaved changes, but no differences wrt. the file"))
        (throw 'return nil)
        ))
    (if (and (boundp 'gnuserv-minor-mode)
             gnuserv-minor-mode)
        (gnuserv-edit)
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

;;;###autoload
(defun increment-number-at-point (arg)
  (interactive "p")
  (let ((inc (or arg 1)))
    (skip-chars-backward "0123456789xABCDEFabcdef")
    (cond ((looking-at "0x\\([0123456789ABCDEFabcdef]+\\)")
           (replace-match (format "0x%x" (+ inc (string-to-number (match-string 1) 16)))))
          ((looking-at "[0123456789]+")
           (replace-match (number-to-string (+ inc (string-to-number (match-string 0))))))
          (error "No number at point"))))

;;;###autoload
(defun fc-eval-and-replace (arg)
  "Replace the preceding sexp with its value."
  (interactive "P")
  (interactive)
  (backward-kill-sexp)
  (let ((res (eval (read (current-kill 0)))))
    (unless arg
      (prin1 res
             (current-buffer)))))

(provide 'perso-misc)
;;; misc.el ends here
