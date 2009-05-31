;;; sure-tags.el --- tags enhancements

;; Copyright 1997 Bob Glickstein.      <http://www.zanshin.com/~bobg/>

;; Author: Bob Glickstein <b...@zanshin.com>
;; Maintainer: Bob Glickstein <b...@zanshin.com>
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, send e-mail to
;; this program's maintainer or write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Plug:

;; Check out my book, "Writing GNU Emacs Extensions," from O'Reilly
;; and Associates.  <http://www.ora.com/catalog/gnuext/>

;;; Commentary:

;; This file surrounds various tag-searching functions with code that
;;  - First makes sure you've selected a tags file;
;;  - Makes sure that tags file exists (and builds it if it doesn't);
;;  - Performs the requested search;
;;  - Retries the search if it fails, allowing you to first rebuild
;;    the tags file or specify a new one

;; To use this, simply
;;  (require 'sure-tags)
;; in your .emacs.

;;; Code:

(require 'electric)

(defvar find-prog "find"
  "*Name of the standard `find' program.")
(defvar xargs-prog "xargs"
  "*Name of the standard `xargs' program.")
(defvar etags-prog "etags"
  "*Name of the `etags' program.")

(defvar etags-suffixes '("c" "cxx" "h" "cc" "hh" "pl" "pm" "el")
  "*What file suffixes to look for when auto-running etags.")

(defun build-tags-file (file &optional remove-p)
  "Build tags file FILE.
This is done by running the command

  find DIR \\( -name \\*.c -o -name \\*.h \\) -print | xargs etags -a -o FILE

where DIR is the `file-name-directory' of FILE.  The actual list of
file name suffixes to use in the find command is given by
`etags-suffixes' (q.v.).

If optional second argument REMOVE-P is non-nil, FILE is silently
deleted first."
  (condition-case nil
      (if remove-p
          (delete-file file))
    (error))
  (message "Building %s..." file)
  (let* ((dir (file-name-directory file))
         (find-args (mapconcat
                     'identity
                     (mapcar (function
                              (lambda (x)
                                (format "-name \\*.%s" x)))
                             etags-suffixes)
                     " -o "))
         (result (call-process
                  "/bin/sh" nil nil nil
                  "-c"
                  (format
                   "%s %s \\( %s \\) -print | %s %s -a -o %s"
                   find-prog
                   dir find-args
                   xargs-prog
                   etags-prog
                   file))))
    (if (numberp result)
        (if (zerop result)
            t
          (error "etags exited with status %d" result))
      (error "etags died because: \"%s\"" result)))
  (message "Building %s... done" file))

(defun choose-tags-file ()
  "Prompt for a tags file, then build it if necessary with `build-tags-file'."
  (let ((file (expand-file-name
               (read-file-name "Visit tags file: (default TAGS) "
                               nil "TAGS"))))
    (if (file-directory-p file)
        (setq file (expand-file-name "TAGS" file)))
    (if (or (file-exists-p file)
            (and (y-or-n-p (format "%s does not exist.  Build it? " file))
                 (build-tags-file file)))
        (visit-tags-table file))))

(defun tags-retry ()
  "Prompt for an action following a failed tag search.

This function should be installed in a `condition-case' error clause
in a `defadvice' surrounding a tag-finding function such as
`find-tag'.  See `advise-tag-function'.

This function prompts the user to rebuild the current tags file,
switch to a new tags file, or quit the current tag search.  Return
value is nil if the tag search should be aborted, non-nil if it should
be retried with the same arguments."
  (save-window-excursion
    (select-window (minibuffer-window))
    (let ((new (make-sparse-keymap))
          (saved (current-local-map)))
      (suppress-keymap new t)
      (define-key new "r" 'tags-retry-rebuild)
      (define-key new "s" 'tags-retry-switch)
      (define-key new "q" 'tags-retry-quit)
      (define-key new "?" 'tags-retry-help)
      (define-key new "h" 'tags-retry-help)
      (unwind-protect
          (progn
            (use-local-map new)
            (catch 'tags-retry-prompt
              (Electric-command-loop
               'tags-retry-prompt
               "Tag not found: (r)ebuild tags, (s)witch tags, (q)uit:")))
        (use-local-map saved)))))

(defun tags-retry-rebuild ()
  "Call `build-tags-file' on current tags file(s) for retrying tag search.
For use only within `tags-retry' (q.v.)."
  (interactive)
  (let ((files tags-table-list))
    (cond (files
           (while files
             (build-tags-file (car files) t)
             (setq files (cdr files))))
          (tags-file-name
           (build-tags-file tags-file-name t))
          (t
           (choose-tags-file))))
  (throw 'tags-retry-prompt t))

(defun tags-retry-switch ()
  "Call `choose-tags-file' for retrying tag search.
For use only within `tags-retry' (q.v.)."
  (interactive)
  (choose-tags-file)
  (throw 'tags-retry-prompt t))

(defun tags-retry-quit ()
  "Exit a pending `tags-retry'."
  (interactive)
  (throw 'tags-retry-prompt nil))

(defun tags-retry-help ()
  "Show the user's options during a `tags-retry'."
  (interactive)
  (let ((buffer (get-buffer-create "*Tags Retry Help*")))
    (set-buffer buffer)
    (erase-buffer)
    (insert
     (substitute-command-keys
      (concat "A tag-searching command has failed.  Your choices are now:\n"
              " - Rebuild the tags file(s) with `\\[tags-retry-rebuild]';\n"
              " - Switch to a new tags file with `\\[tags-retry-switch]';\n"
              " - Give up with `\\[tags-retry-quit]'.")))
    (let ((pop-up-windows t))
      (display-buffer buffer))))

(defmacro advise-tag-function (function)
  "Advise FUNCTION, a tag-finding function, with sure-tags code.
This means that before FUNCTION is called, an appropriate tags file
will be selected and, if necessary, built (with `build-tags-file')."
  `(defadvice ,function (around sure-tags activate compile)
     "Make sure a tags file is selected.
Then make sure it exists.  (Build it if necessary.)
Then search.

Then, if the search fails, allow the user to automatically search
again after switching tags files or updating the current one."
     (or tags-table-list
         tags-file-name
         (choose-tags-file))
     (condition-case nil
         ad-do-it
       (error (if (tags-retry)
                  (apply (function ,function) (ad-get-args 0)))))))

(advise-tag-function find-tag)
(advise-tag-function find-tag-other-window)
(advise-tag-function find-tag-other-frame)

(provide 'sure-tags)

;;; sure-tags.el ends here