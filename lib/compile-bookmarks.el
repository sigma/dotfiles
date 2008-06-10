;;; compile-bookmarks.el --- bookmarks for compilation commands
;;
;; Copyright (C) 2008 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 0.1
;; Keywords: tools, processes
;; URL: http://nschum.de/src/emacs/compile-bookmarks/
;; Compatibility: GNU Emacs 22.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; compile-bookmarks.el allows you to bookmark your compile commands and retain
;; them across sessions.
;;
;; When you enable the global `compile-bookmarks-mode', your bookmarks will be
;; loaded from `compile-bm-save-file'.  If you quit emacs with the mode enabled,
;; the bookmarks will be stored automatically.
;;
;; You can manage your bookmarks with `compile-bm-add', `compile-bm-remove' and
;; `compile-bm-recompile', or use the "Compile" menu.
;;
;;; Change Log:
;;
;; 2008-06-09 (0.1)
;;    Initial release.
;;
;;; Code:

(require 'compile)

(defgroup compile-bookmarks nil
  "Bookmarks for compilation commands"
  :group 'tools
  :group 'processes)

(defcustom compile-bm-save-file "~/.compile-bm"
  "*File name for storing the compilation bookmarks"
  :group 'compile-bookmarks
  :type 'file)

(defvar compile-bookmarks-mode-map (make-sparse-keymap)
  "*Keymap used by `compile-bm-mode'.")
(defvaralias 'compile-bm-mode-map 'compile-bookmarks-mode-map)

(defvar compile-bm-list nil
  "The bookmarks for `compile-bookmarks-mode'.")

(require 'recentf)
(defalias 'compile-bm-dump-variable 'recentf-dump-variable)

(defun compile-bm-save-list ()
  "Store the saved bookmarks to `compile-bm-save-file'."
  ;; based on recentf-save-list
  (with-temp-buffer
    (erase-buffer)
    (set-buffer-file-coding-system 'emacs-mule)
    (insert (format ";;; Generated by `compile-bm' on %s"
                    (current-time-string)))
    (compile-bm-dump-variable 'compile-bm-list)
    (insert "\n\n;;; Local Variables:\n"
            (format ";;; coding: %s\n" 'emacs-mule)
            ";;; End:\n")
    (write-file (expand-file-name compile-bm-save-file))))

(defun compile-bm-load-list (&optional force)
  "Load the previously saved bookmarks from `recentf-save-file'.
Unless optional argument FORCE is given, the command will fail if
`compile-bm-list' already contains any values."
  (when compile-bm-list
    (error "Refusing to overwrite existing bookmarks"))
  (let ((file (expand-file-name compile-bm-save-file)))
    (when (file-readable-p file)
      (load-file file))))

(defun compile-bm-add (&optional name)
  "Add the current `compile-command' to the saved command list."
  (interactive)
  (let* ((pair (cons compilation-directory compile-command))
         (entry (assoc pair compile-bm-list)))
    (unless name
      (setq name (or (cdr entry)
                     (compile-bm-make-name compilation-directory
                                           compile-command))))
  (when (interactive-p)
    (setq name (read-from-minibuffer "name: " name)))
  (if (assoc pair compile-bm-list)
      (when name
        (setcdr entry name))
    (push (cons pair (or name
                         (compile-bm-make-name compilation-directory
                                               compile-command)))
          compile-bm-list))
  (setq compile-bm-list
        (sort compile-bm-list (lambda (a b) (string< (cdr a) (cdr b)))))
  (compile-bm-update-menu)))

(defun compile-bm-make-menu-entry (entry)
  (vector
   (cdr entry)
   `(compile-bm-restore-and-compile (quote ,entry))
   :style 'toggle
   :selected `(and (equal ,(caar entry) compilation-directory)
                   (equal ,(cdar entry) compile-command))))

(defun compile-bm-update-menu ()
  (easy-menu-define compile-bm-menu compile-bm-mode-map
    "Compile Bookmarks"
    `("Compile"
      ,@(mapcar 'compile-bm-make-menu-entry compile-bm-list)
      "-"
      ["Rename" compile-bm-add
       :visible (assoc (cons compilation-directory compile-command)
                       compile-bm-list)]
      ["Remove" compile-bm-remove
       :visible (assoc (cons compilation-directory compile-command)
                       compile-bm-list)]
      ["Add" compile-bm-add
       :visible (and compilation-directory
                     (not (assoc (cons compilation-directory compile-command)
                                 compile-bm-list)))]
      ))
  (easy-menu-add compile-bm-menu))

;;;###autoload
(define-minor-mode compile-bookmarks-mode
  "Minor mode for keeping track of multiple `compile-command's.
This mode enables a bookmark menu for the commands used by `recompile'.
Once you have stored the last compilation with `compile-bm-add' (or the
menu), you will be able to execute that compilation from the menu."
  nil nil compile-bookmarks-mode-map :global t
  (if compile-bookmarks-mode
      (progn (compile-bm-load-list)
             (add-hook 'kill-emacs-hook 'compile-bm-save-list)
             (compile-bm-update-menu))
    (compile-bm-save-list)
    ;; delete list, so not to trigger overwrite warning when enabling again
    (setq compile-bm-list nil)
    (remove-hook 'kill-emacs-hook 'compile-bm-save-list)))

(defalias 'compile-bm-mode 'compile-bookmarks-mode)

(defun compile-bm-remove ()
  "Remove the current `compile-command' from the saved command list."
  (interactive)
  (setq compile-bm-list
        (delete (assoc (cons compilation-directory compile-command)
                       compile-bm-list)
                compile-bm-list))
  (compile-bm-update-menu))

(defun compile-bm-make-name (directory command)
  (concat
   (mapconcat 'identity (last (split-string directory "/" t) 2) "/")
   " | "
   (if (> (length command) 40)
       (concat "..." (substring command -37))
     command)))

(defun compile-bm-restore (entry)
  "Restore ENTRY from `compile-bm-list'."
  (setq compilation-directory (caar entry))
  (setq compile-command (cdar entry))
  (compile-bm-update-menu))

(defun compile-bm-restore-and-compile (entry)
  "Restore ENTRY from `compile-bm-list' and compile."
  (compile-bm-restore entry)
  (recompile))

(defsubst compile-bm-swap (c)
  (cons (cdr c) (car c)))

(defun compile-bm-recompile ()
  "Pick a compile bookmark and compile."
  (interactive)
  (let* ((swapped (mapcar 'compile-bm-swap compile-bm-list))
         (history (mapcar 'cdr compile-bm-list)))
    (compile-bm-restore-and-compile
     (compile-bm-swap
      (assoc (completing-read "Compile: " swapped nil t nil 'history)
             swapped)))))

(provide 'compile-bookmarks)
;;; compile-bookmarks.el ends here
