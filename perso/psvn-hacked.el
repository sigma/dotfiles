;;; psvn.el --- Subversion interface for emacs
;; Copyright (C) 2002-2004 by Stefan Reichoer

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; $Id: psvn.el 106 2004-11-21 18:19:20Z yann $

;; psvn.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; psvn.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; psvn.el is tested with GNU Emacs 21.3 on windows, debian linux,
;; freebsd5 with svn 1.05

;; psvn.el is an interface for the revision control tool subversion
;; (see http://subversion.tigris.org)
;; psvn.el provides a similar interface for subversion as pcl-cvs for cvs.
;; At the moment the following commands are implemented:
;; M-x svn-status: run 'svn -status -v'
;; and show the result in the *svn-status* buffer.  This buffer uses
;; svn-status mode in which the following keys are defined:
;; g     - svn-status-update:               run 'svn status -v'
;; C-u g - svn-status-update:               run 'svn status -vu'
;; =     - svn-status-show-svn-diff         run 'svn diff'
;; l     - svn-status-show-svn-log          run 'svn log'
;; i     - svn-status-info                  run 'svn info'
;; r     - svn-status-revert                run 'svn revert'
;; V     - svn-status-resolved              run 'svn resolved'
;; U     - svn-status-update-cmd            run 'svn update'
;; c     - svn-status-commit-file           run 'svn commit'
;; a     - svn-status-add-file              run 'svn add --non-recursive'
;; A     - svn-status-add-file-recursively  run 'svn add'
;; +     - svn-status-make-directory        run 'svn mkdir'
;; R     - svn-status-mv                    run 'svn mv'
;; C-d   - svn-status-rm                    run 'svn rm'
;; M-c   - svn-status-cleanup               run 'svn cleanup'
;; b     - svn-status-blame                 run 'svn blame'
;; RET   - svn-status-find-file-or-examine-directory
;; ^     - svn-status-examine-parent
;; ~     - svn-status-get-specific-revision
;; E     - svn-status-ediff-with-revision
;; s     - svn-status-show-process-buffer
;; e     - svn-status-toggle-edit-cmd-flag
;; ?     - svn-status-toggle-hide-unknown
;; _     - svn-status-toggle-hide-unmodified
;; m     - svn-status-set-user-mark
;; u     - svn-status-unset-user-mark
;; $     - svn-status-toggle-elide
;; DEL   - svn-status-unset-user-mark-backwards
;; * !   - svn-status-unset-all-usermarks
;; * ?   - svn-status-mark-unknown
;; * A   - svn-status-mark-added
;; * M   - svn-status-mark-modified
;; * D   - svn-status-mark-deleted
;; * *   - svn-status-mark-changed
;; .     - svn-status-goto-root-or-return
;; f     - svn-status-find-file
;; o     - svn-status-find-file-other-window
;; v     - svn-status-view-file-other-window
;; I     - svn-status-parse-info
;; P l   - svn-status-property-list
;; P s   - svn-status-property-set
;; P d   - svn-status-property-delete
;; P e   - svn-status-property-edit-one-entry
;; P i   - svn-status-property-ignore-file
;; P I   - svn-status-property-ignore-file-extension
;; P C-i - svn-status-property-edit-svn-ignore
;; P k   - svn-status-property-set-keyword-list
;; P y   - svn-status-property-set-eol-style
;; h     - svn-status-use-history
;; q     - svn-status-bury-buffer

;; To use psvn.el put the following line in your .emacs:
;; (require 'psvn)
;; Start the svn interface with M-x svn-status

;; The latest version of psvn.el can be found at:
;;   http://www.xsteve.at/prg/emacs/psvn.el
;; Or you can check it out from the subversion repository:
;;   svn co http://svn.collab.net/repos/svn/trunk/contrib/client-side/psvn psvn

;; TODO:
;; * shortcut for svn propset svn:keywords "Date" psvn.el
;; * docstrings for the functions
;; * perhaps shortcuts for ranges, dates
;; * when editing the command line - offer help from the svn client
;; * finish svn-status-property-set
;; * eventually use the customize interface
;; * interactive svn-status should complete existing directories only;
;;   unfortunately `read-directory-name' doesn't exist in Emacs 21.3
;; * Add repository browser
;; * Improve support for svn blame

;; Overview over the implemented/not (yet) implemented svn sub-commands:
;; * add                       implemented
;; * blame                     implemented
;; * cat                       implemented
;; * checkout (co)
;; * cleanup                   implemented
;; * commit (ci)               implemented
;; * copy (cp)
;; * delete (del, remove, rm)  implemented
;; * diff (di)                 implemented
;; * export
;; * help (?, h)
;; * import
;; * info                      implemented
;; * list (ls)
;; * log                       implemented
;; * merge
;; * mkdir                     implemented
;; * move (mv, rename, ren)    implemented
;; * propdel (pdel)            implemented
;; * propedit (pedit, pe)      not needed
;; * propget (pget, pg)        used
;; * proplist (plist, pl)      implemented
;; * propset (pset, ps)        used
;; * resolved                  implemented
;; * revert                    implemented
;; * status (stat, st)         implemented
;; * switch (sw)
;; * update (up)               implemented

;; For the not yet implemented commands you should use the command line
;; svn client. If there are user requests for any missing commands I will
;; probably implement them.

;; Comments / suggestions and bug reports are welcome!

;;; Code:

;;; user setable variables
(defvar svn-log-edit-file-name "++svn-log++" "*Name of a saved log file.")
(defvar svn-log-edit-insert-files-to-commit t "*Insert the filelist to commit in the *svn-log* buffer")
(defvar svn-status-hide-unknown nil "*Hide unknown files in *svn-status* buffer.")
(defvar svn-status-hide-unmodified nil "*Hide unmodified files in *svn-status* buffer.")
(defvar svn-status-directory-history nil "*List of visited svn working directories.")
(defvar svn-status-sort-status-buffer t "Sort the *svn-status* buffer.
Setting this variable to nil speeds up M-x svn-status.
However, it is possible, that the sorting is wrong in this case.")

(defvar svn-status-unmark-files-after-list '(commit revert)
  "*List of operations after which all user marks will be removed.
Possible values are: commit, revert.")

;;; default arguments to pass to svn commands
(defvar svn-status-default-log-arguments ""
  "*Arguments to pass to svn log.
\(used in `svn-status-show-svn-log'; override these by giving prefixes\).")

(defvar svn-status-wash-control-M-in-process-buffers
  (eq system-type 'windows-nt)
  "*Remove any trailing ^M from the *svn-process* buffer.")

;;; Customize group
(defgroup psvn nil
  "Subversion interface for Emacs."
  :group 'tools)

(defgroup psvn-faces nil
  "psvn faces."
  :group 'psvn)


(eval-and-compile
  (require 'cl)
  (require 'ediff)
  (defconst svn-xemacsp (featurep 'xemacs))
  (if svn-xemacsp
      (require 'overlay)
    (require 'overlay nil t)))

;; Use the normally used mode for files ending in .~HEAD~, .~BASE~, ...
(add-to-list 'auto-mode-alist '("\\.~?\\(HEAD\\|BASE\\|PREV\\)~?\\'" ignore t))

;;; internal variables
(defvar svn-process-cmd nil)
(defvar svn-status-info nil)
(defvar svn-status-base-info nil)
(defvar svn-status-initial-window-configuration nil)
(defvar svn-status-default-column 23)
(defvar svn-status-default-revision-width 4)
(defvar svn-status-default-author-width 9)
(defvar svn-status-line-format " %c%c%c %4s %4s %-9s")
(defvar svn-status-short-mod-flag-p t)
(defvar svn-start-of-file-list-line-number 0)
(defvar svn-status-files-to-commit nil)
(defvar svn-status-pre-commit-window-configuration nil)
(defvar svn-status-pre-propedit-window-configuration nil)
(defvar svn-status-head-revision nil)
(defvar svn-status-root-return-info nil)
(defvar svn-status-property-edit-must-match-flag nil)
(defvar svn-status-propedit-property-name nil)
(defvar svn-status-propedit-file-list nil)
(defvar svn-status-mode-line-process "")
(defvar svn-status-mode-line-process-status "")
(defvar svn-status-mode-line-process-edit-flag "")
(defvar svn-status-edit-svn-command nil)
(defvar svn-status-update-previous-process-output nil)
(defvar svn-status-temp-dir
  (or
   (when (boundp 'temporary-file-directory) temporary-file-directory) ;emacs
   (when (boundp 'temp-directory) temp-directory)                     ;xemacs
   "/tmp/"))
(defvar svn-temp-suffix (make-temp-name "."))
(defvar svn-status-temp-file-to-remove nil)
(defvar svn-status-temp-arg-file (concat svn-status-temp-dir "svn.arg" svn-temp-suffix))
(defvar svn-status-options nil)
(defvar svn-status-commit-rev-number nil)
(defvar svn-status-operated-on-dot nil)
(defvar svn-status-elided-list nil)

(defvar svn-ediff-windows nil)
(defvar svn-ediff-result nil)
(defvar svn-status-get-specific-revision-file-info nil)

;;; faces
(defface svn-status-marked-face
  '((((type tty) (class color)) (:foreground "green" :weight light))
    (((class color) (background light)) (:foreground "green3"))
    (((class color) (background dark)) (:foreground "palegreen2"))
    (t (:weight bold)))
  "Face to highlight the mark for user marked files in svn status buffers."
  :group 'psvn-faces)

(defface svn-status-marked-popup-face
  '((((type tty) (class color)) (:foreground "green" :weight light))
    (((class color) (background light)) (:foreground "green3"))
    (((class color) (background dark)) (:foreground "palegreen2"))
    (t (:weight bold)))
  "Face to highlight the actual file, if a popup menu is activated."
  :group 'psvn-faces)

(defface svn-status-modified-external-face
  '((((type tty) (class color)) (:foreground "magenta" :weight light))
    (((class color) (background light)) (:foreground "magenta"))
    (((class color) (background dark)) (:foreground "yellow"))
    (t (:weight bold)))
  "Face to highlight the phrase \"externally modified\" in *svn-status* buffers."
  :group 'psvn-faces)

;based on cvs-filename-face
(defface svn-status-directory-face
  '((((type tty) (class color)) (:foreground "lightblue" :weight light))
    (((class color) (background light)) (:foreground "blue4"))
    (((class color) (background dark)) (:foreground "lightskyblue1"))
    (t (:weight bold)))
  "Face for directories in svn status buffers.
See `svn-status--line-info->directory-p' for what counts as a directory."
  :group 'psvn-faces)

;based on font-lock-comment-face
(defface svn-status-filename-face
  '((((class color) (background light)) (:foreground "chocolate"))
    (((class color) (background dark)) (:foreground "beige")))
  "Face for non-directories in svn status buffers.
See `svn-status--line-info->directory-p' for what counts as a directory."
  :group 'psvn-faces)

;based on font-lock-warning-face
(defface svn-status-locked-face
  '((t
     (:weight bold :foreground "Red")))
  "Face for the phrase \"[ LOCKED ]\" *svn-status* buffers."
  :group 'psvn-faces)

;based on vhdl-font-lock-directive-face
(defface svn-status-switched-face
  '((((class color)
      (background light))
     (:foreground "CadetBlue"))
    (((class color)
      (background dark))
     (:foreground "Aquamarine"))
    (t
     (:bold t :italic t)))
  "Face for the phrase \"(switched)\" non-directories in svn status buffers."
  :group 'psvn-faces)

(defvar svn-highlight t)
;; stolen from PCL-CVS
(defun svn-add-face (str face &optional keymap)
  "Return string STR decorated with the specified FACE.
If `svn-highlight' is nil then just return STR."
  (when svn-highlight
    ;; Do not use `list*'; cl.el might not have been loaded.  We could
    ;; put (require 'cl) at the top but let's try to manage without.
    (add-text-properties 0 (length str)
                         `(face ,face
                                mouse-face highlight)
;; 18.10.2004: the keymap parameter is not used (yet) in psvn.el
;;                           ,@(when keymap
;;                               `(mouse-face highlight
;;                                 local-map ,keymap)))
                         str))
  str)

(defun svn-status-maybe-add-face (condition text face)
  "If CONDITION then add FACE to TEXT.
Else return TEXT unchanged."
  (if condition
      (svn-add-face text face)
    text))

(defun svn-status-choose-face-to-add (condition text face1 face2)
  "If CONDITION then add FACE1 to TEXT, else add FACE2 to TEXT."
  (if condition
      (svn-add-face text face1)
    (svn-add-face text face2)))

(defun svn-status-maybe-add-string (condition string face)
  "If CONDITION then return STRING decorated with FACE.
Otherwise, return \"\"."
  (if condition
      (svn-add-face string face)
    ""))

; compatibility
; emacs 20
(unless (fboundp 'point-at-eol) (defalias 'point-at-eol 'line-end-position))
(unless (fboundp 'point-at-bol) (defalias 'point-at-bol 'line-beginning-position))
(unless (functionp 'read-directory-name) (defalias 'read-directory-name 'read-file-name))

(eval-when-compile
  (if (not (fboundp 'gethash))
      (require 'cl-macs)))
(if (not (fboundp 'puthash))
    (defalias 'puthash 'cl-puthash))

(defvar svn-status-display-new-status-buffer nil)
;;;###autoload
(defun svn-status (dir &optional arg)
  "Examine the status of Subversion working copy in directory DIR.
If ARG then pass the -u argument to `svn status'."
  (interactive (list (read-directory-name "SVN status directory: "
                                          nil default-directory nil)))
  (unless (file-directory-p dir)
    (error "%s is not a directory" dir))
  (if (not (file-exists-p (concat dir "/.svn/")))
      (when (y-or-n-p
             (concat dir
                     " does not seem to be a Subversion working copy (no .svn directory).  "
                     "Run dired instead? "))
        (dired dir))
    (setq dir (file-name-as-directory dir))
    (setq svn-status-directory-history (delete dir svn-status-directory-history))
    (add-to-list 'svn-status-directory-history dir)
    (if (string= (buffer-name) "*svn-status*")
        (setq svn-status-display-new-status-buffer nil)
      (setq svn-status-display-new-status-buffer t)
      ;;(message "psvn: Saving initial window configuration")
      (setq svn-status-initial-window-configuration (current-window-configuration)))
    (let* ((status-buf (get-buffer-create "*svn-status*"))
           (proc-buf (get-buffer-create "*svn-process*")))
      (save-excursion
        (set-buffer status-buf)
        (setq default-directory dir)
        (set-buffer proc-buf)
        (setq default-directory dir)
        (if arg
            (svn-run-svn t t 'status "status" "-vu")
          (svn-run-svn t t 'status "status" "-v"))))))

(defun svn-status-use-history ()
  (interactive)
  (let* ((hist svn-status-directory-history)
         (dir (read-from-minibuffer "svn-status on directory: "
                              (cadr svn-status-directory-history)
                              nil nil 'hist)))
    (if (file-directory-p dir)
        (svn-status dir)
      (error "%s is not a directory" dir))))

(defun svn-run-svn (run-asynchron clear-process-buffer cmdtype &rest arglist)
  "Run svn with arguments ARGLIST.

If RUN-ASYNCHRON is t then run svn asynchronously.

If CLEAR-PROCESS-BUFFER is t then erase the contents of the
*svn-process* buffer before commencing.

CMDTYPE is a symbol such as 'mv, 'revert, or 'add, representing the
command to run.

ARGLIST is a list of arguments \(which must include the command name,
for  example: '(\"revert\" \"file1\"\)"
  (if (eq (process-status "svn") nil)
      (progn
        (when svn-status-edit-svn-command
          (setq arglist (append arglist
                                (split-string
                                 (read-from-minibuffer
                                  (format "svn %s %S " cmdtype arglist)))))
          (when (eq svn-status-edit-svn-command t)
            (svn-status-toggle-edit-cmd-flag t))
          (message "svn-run-svn %s: %S" cmdtype arglist))
        (let* ((proc-buf (get-buffer-create "*svn-process*"))
               (svn-proc))
          (when (listp (car arglist))
            (setq arglist (car arglist)))
          (save-excursion
            (set-buffer proc-buf)
            (setq buffer-read-only nil)
            (fundamental-mode)
            (if clear-process-buffer
                (delete-region (point-min) (point-max))
              (goto-char (point-max)))
            (setq svn-process-cmd cmdtype)
            (setq svn-status-mode-line-process-status (format " running %s" cmdtype))
            (svn-status-update-mode-line)
            (sit-for 0.1)
            (if run-asynchron
                (progn
                  (setq svn-proc (apply 'start-process "svn" proc-buf "svn" arglist))
                  (set-process-sentinel svn-proc 'svn-process-sentinel))
              ;;(message "running synchron: svn %S" arglist)
              (apply 'call-process "svn" nil proc-buf nil arglist)
              (setq svn-status-mode-line-process-status "")
              (svn-status-update-mode-line)))))
    (error "You can only run one svn process at once!")))

(defun svn-process-sentinel (process event)
  ;;(princ (format "Process: %s had the event `%s'" process event)))
  ;;(save-excursion
  (let ((act-buf (current-buffer)))
    (set-buffer (process-buffer process))
    (setq svn-status-mode-line-process-status "")
    (svn-status-update-mode-line)
    (cond ((string= event "finished\n")
           (cond ((eq svn-process-cmd 'status)
                  ;;(message "svn status finished")
                  (if (eq system-type 'windows-nt)
                      ;; convert path separator to UNIX style
                      (save-excursion
                        (goto-char (point-min))
                        (while (search-forward "\\" nil t)
                          (replace-match "/"))))
                  (svn-parse-status-result)
                  (set-buffer act-buf)
                  (svn-status-update-buffer)
                  (when svn-status-update-previous-process-output
                    (set-buffer (process-buffer process))
                    (delete-region (point-min) (point-max))
                    (insert "Output from svn command:\n")
                    (insert svn-status-update-previous-process-output)
                    (goto-char (point-min))
                    (setq svn-status-update-previous-process-output nil))
                  (when svn-status-display-new-status-buffer
                    (set-window-configuration svn-status-initial-window-configuration)
                    (switch-to-buffer "*svn-status*")))
                 ((eq svn-process-cmd 'log)
                  (svn-status-show-process-buffer-internal t)
                  (pop-to-buffer "*svn-process*")
                  (switch-to-buffer (get-buffer-create "*svn-log*"))
                  (let ((buffer-read-only nil))
                    (delete-region (point-min) (point-max))
                    (insert-buffer-substring "*svn-process*"))
                  (svn-log-view-mode)
                  (goto-char (point-min))
                  (forward-line 3)
                  (font-lock-fontify-buffer)
                  (message "svn log finished"))
                 ((eq svn-process-cmd 'info)
                  (svn-status-show-process-buffer-internal t)
                  (message "svn info finished"))
                 ((eq svn-process-cmd 'parse-info)
                  (svn-status-parse-info-result))
                 ((eq svn-process-cmd 'blame)
                  (svn-status-show-process-buffer-internal t)
                  (message "svn blame finished"))
                 ((eq svn-process-cmd 'commit)
                  (svn-status-remove-temp-file-maybe)
                  ;;(svn-status-show-process-buffer-internal t)
                  (when (member 'commit svn-status-unmark-files-after-list)
                    (svn-status-unset-all-usermarks))
                  ;;(svn-status-update)
                  ;;(svn-status-update), svn-status-show-process-buffer-internal should be no longer necessary!
                  (svn-status-update-with-command-list (svn-status-parse-commit-output))
                  (message "svn commit finished"))
                 ((eq svn-process-cmd 'update)
                  (svn-status-show-process-buffer-internal t)
                  (svn-status-update)
                  (message "svn update finished"))
                 ((eq svn-process-cmd 'add)
                  (svn-status-update)
                  (message "svn add finished"))
                 ((eq svn-process-cmd 'mkdir)
                  (svn-status-update)
                  (message "svn mkdir finished"))
                 ((eq svn-process-cmd 'revert)
                  (when (member 'revert svn-status-unmark-files-after-list)
                    (svn-status-unset-all-usermarks))
                  (svn-status-update)
                  (message "svn revert finished"))
                 ((eq svn-process-cmd 'resolved)
                  (svn-status-update)
                  (message "svn resolved finished"))
                 ((eq svn-process-cmd 'mv)
                  (svn-status-update)
                  (message "svn mv finished"))
                 ((eq svn-process-cmd 'rm)
                  (svn-status-update)
                  (message "svn rm finished"))
                 ((eq svn-process-cmd 'cleanup)
                  (message "svn cleanup finished"))
                 ((eq svn-process-cmd 'proplist)
                  (svn-status-show-process-buffer-internal t)
                  (message "svn proplist finished"))
                 ((eq svn-process-cmd 'proplist-parse)
                  (svn-status-property-parse-property-names))
                 ((eq svn-process-cmd 'propset)
                  (svn-status-remove-temp-file-maybe)
                  (svn-status-update))
                 ((eq svn-process-cmd 'propdel)
                  (svn-status-update))))
          ((string= event "killed\n")
           (message "svn process killed"))
          ((string-match "exited abnormally" event)
           (while (accept-process-output process 0 100))
           ;; find last error message and show it.
           (goto-char (point-max))
           (message "svn failed: %s"
                    (if (re-search-backward "^svn: \\(.*\\)" nil t)
                        (match-string 1)
                      event)))
          (t
           (message "svn process had unknown event: %s" event))
          (svn-status-show-process-buffer-internal t))))

(defun svn-parse-rev-num (str)
  (if (and str (stringp str)
           (save-match-data (string-match "^[0-9]+" str)))
      (string-to-number str)
    -1))


(defun svn-parse-status-result ()
  "Parse the *svn-process* buffer.
The results are used to build the `svn-status-info' variable."
  (setq svn-status-head-revision nil)
  (save-excursion
    (let ((old-ui-information (svn-status-ui-information-hash-table))
          (line-string)
          (user-mark)
          (svn-marks)
          (svn-file-mark)
          (svn-property-mark)
          (svn-locked-mark)
          (svn-with-history-mark)
          (svn-switched-mark)
          (svn-update-mark)
          (local-rev)
          (last-change-rev)
          (author)
          (path)
          (user-elide nil)
          (ui-status '(nil nil))     ; contains (user-mark user-elide)
          (revision-width svn-status-default-revision-width)
          (author-width svn-status-default-author-width))
      (set-buffer "*svn-process*")
      (setq svn-status-info nil)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (cond
         ((= (point-at-eol) (point-at-bol)) ;skip blank lines
          nil)
         ((looking-at "Status against revision:[ ]+\\([0-9]+\\)")
          ;; the above message appears for the main listing plus once for each svn:externals entry
          (unless svn-status-head-revision
            (setq svn-status-head-revision (match-string 1))))
         ((looking-at "Performing status on external item at '\(.*\)'")
          ;; The *next* line has info about the directory named in svn:externals
          ;; [ie the directory in (match-string 1)]
          ;; we should parse it, and merge the info with what we have already know
          ;; but for now just ignore the line completely
          (forward-line)
          )
         (t
          (setq svn-marks (buffer-substring (point) (+ (point) 8))
                svn-file-mark (elt svn-marks 0)         ; 1st column - M,A,C,D,G,? etc
                svn-property-mark (elt svn-marks 1)     ; 2nd column - M,C (properties)
                svn-locked-mark (elt svn-marks 2)       ; 3rd column - L or blank
                svn-with-history-mark (elt svn-marks 3) ; 4th column - + or blank
                svn-switched-mark (elt svn-marks 4)     ; 5th column - S or blank
                svn-update-mark (elt svn-marks 7))      ; 8th column - * or blank

          (when (eq svn-property-mark ?\ )     (setq svn-property-mark nil))
          (when (eq svn-locked-mark ?\ )       (setq svn-locked-mark nil))
          (when (eq svn-with-history-mark ?\ ) (setq svn-with-history-mark nil))
          (when (eq svn-switched-mark ?\ )     (setq svn-switched-mark nil))
          (when (eq svn-update-mark ?\ )       (setq svn-update-mark nil))
          (forward-char 8)
          (skip-chars-forward " ")
          (cond
           ((looking-at "\\([-?]\\|[0-9]+\\) +\\([-?]\\|[0-9]+\\) +\\([^ ]+\\) *\\(.+\\)")
            (setq local-rev (svn-parse-rev-num (match-string 1))
                  last-change-rev (svn-parse-rev-num (match-string 2))
                  author (match-string 3)
                  path (match-string 4)))
           ((looking-at "\\(.*\\)")
            (setq path (match-string 1)
                  local-rev -1
                  last-change-rev -1
                  author (if (eq svn-file-mark 88) "" "?"))) ;clear author of svn:externals dirs
           (t
            (error "Unknown status line format")))
          (unless path (setq path "."))
          (setq ui-status (or (gethash path old-ui-information) (list user-mark user-elide)))
          (setq svn-status-info (cons (list ui-status
                                            svn-file-mark
                                            svn-property-mark
                                            path
                                            local-rev
                                            last-change-rev
                                            author
                                            svn-update-mark
                                            svn-locked-mark
                                            svn-with-history-mark
                                            svn-switched-mark)
                                      svn-status-info))
          (setq revision-width (max revision-width
                                    (length (number-to-string local-rev))
                                    (length (number-to-string last-change-rev))))
          (setq author-width (max author-width (length author)))))
        (forward-line 1))
      ;; With subversion 0.29.0 and above, `svn -u st' returns files in
      ;; a random order (especially if we have a mixed revision wc)
      (setq svn-status-default-column
            (+ 6 revision-width revision-width author-width
               (if svn-status-short-mod-flag-p 3 0)))
      (setq svn-status-line-format (format " %%c%%c%%c %%%ds %%%ds %%-%ds"
                                           revision-width
                                           revision-width
                                           author-width))
      (setq svn-status-info (nreverse svn-status-info))
      (when svn-status-sort-status-buffer
        (setq svn-status-info (sort svn-status-info 'svn-status-sort-predicate))))))

;;(string-lessp "." "%") => nil
;(svn-status-sort-predicate '(t t t ".") '(t t t "%")) => t
(defun svn-status-sort-predicate (a b)
  "Return t if A should appear before B in the *svn-status* buffer.
A and B must be line-info's."
  (string-lessp (concat (svn-status-line-info->full-path a) "/")
                (concat (svn-status-line-info->full-path b) "/")))

(defun svn-status-remove-temp-file-maybe ()
  "Remove any (no longer required) temporary files created by psvn.el."
  (when svn-status-temp-file-to-remove
    (when (file-exists-p svn-status-temp-file-to-remove)
      (delete-file svn-status-temp-file-to-remove))
    (when (file-exists-p svn-status-temp-arg-file)
      (delete-file svn-status-temp-arg-file))
    (setq svn-status-temp-file-to-remove nil)))

(defun svn-status-remove-control-M ()
  "Remove ^M at end of line in the whole buffer."
  (interactive)
  (let ((buffer-read-only nil))
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\r$" (point-max) t)
          (replace-match "" nil nil))))))

(condition-case nil
    ;;(easy-menu-add-item nil '("tools") ["SVN Status" svn-status t] "PCL-CVS")
    (easy-menu-add-item nil '("tools") ["SVN Status" svn-status t])
  (error (message "psvn: could not install menu")))

(defvar svn-status-mode-map () "Keymap used in `svn-status-mode' buffers.")
(defvar svn-status-mode-property-map ()
  "Subkeymap used in `svn-status-mode' for property commands.")
(defvar svn-status-mode-options-map ()
  "Subkeymap used in `svn-status-mode' for option commands.")

(when (not svn-status-mode-map)
  (setq svn-status-mode-map (make-sparse-keymap))
  (suppress-keymap svn-status-mode-map)
  ;; Don't use (kbd "<return>"); it's unreachable with GNU Emacs 21.3 on a TTY.
  (define-key svn-status-mode-map (kbd "RET") 'svn-status-find-file-or-examine-directory)
  (define-key svn-status-mode-map (kbd "<mouse-2>") 'svn-status-mouse-find-file-or-examine-directory)
  (define-key svn-status-mode-map (kbd "^") 'svn-status-examine-parent)
  (define-key svn-status-mode-map (kbd "s") 'svn-status-show-process-buffer)
  (define-key svn-status-mode-map (kbd "f") 'svn-status-find-files)
  (define-key svn-status-mode-map (kbd "o") 'svn-status-find-file-other-window)
  (define-key svn-status-mode-map (kbd "v") 'svn-status-view-file-other-window)
  (define-key svn-status-mode-map (kbd "e") 'svn-status-toggle-edit-cmd-flag)
  (define-key svn-status-mode-map (kbd "g") 'svn-status-update)
  (define-key svn-status-mode-map (kbd "q") 'svn-status-bury-buffer)
  (define-key svn-status-mode-map (kbd "h") 'svn-status-use-history)
  (define-key svn-status-mode-map (kbd "m") 'svn-status-set-user-mark)
  (define-key svn-status-mode-map (kbd "u") 'svn-status-unset-user-mark)
  ;; This matches a binding of `dired-unmark-all-files' in `dired-mode-map'
  ;; of both GNU Emacs and XEmacs.  It seems unreachable with XEmacs on
  ;; TTY, but if that's a problem then its Dired needs fixing too.
  ;; Or you could just use "*!".
  (define-key svn-status-mode-map "\M-\C-?" 'svn-status-unset-all-usermarks)
  ;; The key that normally deletes characters backwards should here
  ;; instead unmark files backwards.  In GNU Emacs, that would be (kbd
  ;; "DEL") aka [?\177], but XEmacs treats those as [(delete)] and
  ;; would bind a key that normally deletes forwards.  [(backspace)]
  ;; is unreachable with GNU Emacs on a tty.  Try to recognize the
  ;; dialect and act accordingly.
  ;;
  ;; XEmacs has a `delete-forward-p' function that checks the
  ;; `delete-key-deletes-forward' option.  We don't use those, for two
  ;; reasons: psvn.el may be loaded before user customizations, and
  ;; XEmacs allows simultaneous connections to multiple devices with
  ;; different keyboards.
  (define-key svn-status-mode-map
              (if (member (kbd "DEL") '([(delete)] [delete]))
                  [(backspace)]         ; XEmacs
                (kbd "DEL"))            ; GNU Emacs
              'svn-status-unset-user-mark-backwards)
  (define-key svn-status-mode-map (kbd "$") 'svn-status-toggle-elide)
  (define-key svn-status-mode-map (kbd ".") 'svn-status-goto-root-or-return)
  (define-key svn-status-mode-map (kbd "I") 'svn-status-parse-info)
  (define-key svn-status-mode-map (kbd "?") 'svn-status-toggle-hide-unknown)
  (define-key svn-status-mode-map (kbd "_") 'svn-status-toggle-hide-unmodified)
  (define-key svn-status-mode-map (kbd "a") 'svn-status-add-file)
  (define-key svn-status-mode-map (kbd "A") 'svn-status-add-file-recursively)
  (define-key svn-status-mode-map (kbd "+") 'svn-status-make-directory)
  (define-key svn-status-mode-map (kbd "R") 'svn-status-mv)
  (define-key svn-status-mode-map (kbd "D") 'svn-status-rm)
  (define-key svn-status-mode-map (kbd "c") 'svn-status-commit-file)
  (define-key svn-status-mode-map (kbd "M-c") 'svn-status-cleanup)
  (define-key svn-status-mode-map (kbd "U") 'svn-status-update-cmd)
  (define-key svn-status-mode-map (kbd "r") 'svn-status-revert)
  (define-key svn-status-mode-map (kbd "l") 'svn-status-show-svn-log)
  (define-key svn-status-mode-map (kbd "i") 'svn-status-info)
  (define-key svn-status-mode-map (kbd "b") 'svn-status-blame)
  (define-key svn-status-mode-map (kbd "=") 'svn-status-show-svn-diff)
  ;; [(control ?=)] is unreachable on TTY, but you can use "*u" instead.
  ;; (Is the "u" mnemonic for something?)
  (define-key svn-status-mode-map (kbd "C-=") 'svn-status-show-svn-diff-for-marked-files)
  (define-key svn-status-mode-map (kbd "~") 'svn-status-get-specific-revision)
  (define-key svn-status-mode-map (kbd "E") 'svn-status-ediff-with-revision)
  (define-key svn-status-mode-map (kbd "X") 'svn-status-resolve-conflicts)

  (define-key svn-status-mode-map (kbd "C-n") 'svn-status-next-line)
  (define-key svn-status-mode-map (kbd "C-p") 'svn-status-previous-line)
  (define-key svn-status-mode-map (kbd "<down>") 'svn-status-next-line)
  (define-key svn-status-mode-map (kbd "<up>") 'svn-status-previous-line)
  (define-key svn-status-mode-map [down-mouse-3] 'svn-status-popup-menu)
  (setq svn-status-mode-mark-map (make-sparse-keymap))
  (define-key svn-status-mode-map (kbd "*") svn-status-mode-mark-map)
  (define-key svn-status-mode-mark-map (kbd "!") 'svn-status-unset-all-usermarks)
  (define-key svn-status-mode-mark-map (kbd "?") 'svn-status-mark-unknown)
  (define-key svn-status-mode-mark-map (kbd "A") 'svn-status-mark-added)
  (define-key svn-status-mode-mark-map (kbd "M") 'svn-status-mark-modified)
  (define-key svn-status-mode-mark-map (kbd "D") 'svn-status-mark-deleted)
  (define-key svn-status-mode-mark-map (kbd "*") 'svn-status-mark-changed)
  (define-key svn-status-mode-mark-map (kbd "V") 'svn-status-resolved)
  (define-key svn-status-mode-mark-map (kbd "u") 'svn-status-show-svn-diff-for-marked-files))
(when (not svn-status-mode-property-map)
  (setq svn-status-mode-property-map (make-sparse-keymap))
  (define-key svn-status-mode-property-map (kbd "l") 'svn-status-property-list)
  (define-key svn-status-mode-property-map (kbd "s") 'svn-status-property-set)
  (define-key svn-status-mode-property-map (kbd "d") 'svn-status-property-delete)
  (define-key svn-status-mode-property-map (kbd "e") 'svn-status-property-edit-one-entry)
  (define-key svn-status-mode-property-map (kbd "i") 'svn-status-property-ignore-file)
  (define-key svn-status-mode-property-map (kbd "I") 'svn-status-property-ignore-file-extension)
  ;; XEmacs 21.4.15 on TTY (vt420) converts `C-i' to `TAB',
  ;; which [(control ?i)] won't match.  Handle it separately.
  ;; On GNU Emacs, the following two forms bind the same key,
  ;; reducing clutter in `where-is'.
  (define-key svn-status-mode-property-map [(control ?i)] 'svn-status-property-edit-svn-ignore)
  (define-key svn-status-mode-property-map (kbd "TAB") 'svn-status-property-edit-svn-ignore)
  (define-key svn-status-mode-property-map (kbd "k") 'svn-status-property-set-keyword-list)
  (define-key svn-status-mode-property-map (kbd "y") 'svn-status-property-set-eol-style)
  (define-key svn-status-mode-property-map (kbd "p") 'svn-status-property-parse)
  ;; TODO: Why is `svn-status-select-line' in `svn-status-mode-property-map'?
  (define-key svn-status-mode-property-map (kbd "RET") 'svn-status-select-line)
  (define-key svn-status-mode-map (kbd "P") svn-status-mode-property-map))
(when (not svn-status-mode-options-map)
  (setq svn-status-mode-options-map (make-sparse-keymap))
  (define-key svn-status-mode-options-map (kbd "s") 'svn-status-save-state)
  (define-key svn-status-mode-options-map (kbd "l") 'svn-status-load-state)
  (define-key svn-status-mode-options-map (kbd "x") 'svn-status-toggle-sort-status-buffer)
  (define-key svn-status-mode-map (kbd "O") svn-status-mode-options-map))

(easy-menu-define svn-status-mode-menu svn-status-mode-map
  "'svn-status-mode' menu"
  '("SVN"
    ["svn status" svn-status-update t]
    ["svn update" svn-status-update-cmd t]
    ["svn commit" svn-status-commit-file t]
    ["svn log" svn-status-show-svn-log t]
    ["svn info" svn-status-info t]
    ["svn blame" svn-status-blame t]
    ("Diff"
     ["svn diff current file" svn-status-show-svn-diff t]
     ["svn diff marked files" svn-status-show-svn-diff-for-marked-files t]
     ["svn ediff current file" svn-status-ediff-with-revision t]
     ["svn resolve conflicts" svn-status-resolve-conflicts]
     )
    ["svn cat ..." svn-status-get-specific-revision t]
    ["svn add" svn-status-add-file t]
    ["svn mkdir..." svn-status-make-directory t]
    ["svn mv..." svn-status-mv t]
    ["svn rm..." svn-status-rm t]
    ["Up Directory" svn-status-examine-parent t]
    ["Elide Directory" svn-status-toggle-elide t]
    ["svn revert" svn-status-revert t]
    ["svn resolved" svn-status-resolved t]
    ["svn cleanup" svn-status-cleanup t]
    ["Show Process Buffer" svn-status-show-process-buffer t]
    ("Property"
     ["svn proplist" svn-status-property-list t]
     ["Set Multiple Properties..." svn-status-property-set t]
     ["Edit One Property..." svn-status-property-edit-one-entry t]
     ["svn propdel..." svn-status-property-delete t]
     "---"
     ["svn:ignore File..." svn-status-property-ignore-file t]
     ["svn:ignore File Extension..." svn-status-property-ignore-file-extension t]
     ["Edit svn:ignore Property" svn-status-property-edit-svn-ignore t]
     "---"
     ["Set svn:keywords List" svn-status-property-set-keyword-list t]
     ["Set svn:eol-style" svn-status-property-set-eol-style t]
     )
    ("Options"
     ["Save Options" svn-status-save-state t]
     ["Load Options" svn-status-load-state t]
     ["Toggle sorting of *svn-status* buffer" svn-status-toggle-sort-status-buffer
      :style toggle :selected svn-status-sort-status-buffer]
     )
    "---"
    ["Edit Next SVN Cmd Line" svn-status-toggle-edit-cmd-flag t]
    ["Work Directory History..." svn-status-use-history t]
    ("Mark / Unmark"
     ["Mark" svn-status-set-user-mark t]
     ["Unmark" svn-status-unset-user-mark t]
     ["Unmark all" svn-status-unset-all-usermarks t]
     "---"
     ["Mark/Unmark unknown" svn-status-mark-unknown t]
     ["Mark/Unmark added" svn-status-mark-added t]
     ["Mark/Unmark modified" svn-status-mark-modified t]
     ["Mark/Unmark deleted" svn-status-mark-deleted t]
     ["Mark/Unmark modified/added/deleted" svn-status-mark-changed t]
     )
    ["Hide Unknown" svn-status-toggle-hide-unknown
     :style toggle :selected svn-status-hide-unknown]
    ["Hide Unmodified" svn-status-toggle-hide-unmodified
     :style toggle :selected svn-status-hide-unmodified]
    ))


(defun svn-status-popup-menu (event)
  (interactive "e")
  (mouse-set-point event)
  (let* ((line-info (svn-status-get-line-information))
         (name (svn-status-line-info->filename line-info)))
    (when line-info
      (easy-menu-define svn-status-actual-popup-menu nil nil
        (list name
               ["svn diff" svn-status-show-svn-diff t]
               ["svn commit" svn-status-commit-file t]
               ["svn log" svn-status-show-svn-log t]
               ["svn info" svn-status-info t]
               ["svn blame" svn-status-blame t]))
      (svn-status-face-set-temporary-during-popup
       'svn-status-marked-popup-face (line-beginning-position) (line-end-position)
       svn-status-actual-popup-menu))))

(defun svn-status-face-set-temporary-during-popup (face begin end menu &optional prefix)
  "Put FACE on BEGIN and END in the buffer during Popup MENU.
PREFIX is passed to `popup-menu'."
  (let (o)
    (unwind-protect
        (progn
          (setq o (make-overlay begin end))
          (overlay-put o 'face face)
          (sit-for 0)
          (popup-menu menu prefix))
      (delete-overlay o))))

(defun svn-status-mode ()
  "Major mode used by  psvn.el to process the output of \"svn status\".

psvn.el is an interface for the revision control tool subversion
\(see http://subversion.tigris.org).
psvn.el provides a similar interface for subversion as pcl-cvs does for cvs.
At the moment the following commands are implemented:
  M-x svn-status: run 'svn -status -v'
  and show the result in the *svn-status* buffer, this buffer uses the
  svn-status mode. In this mode the following keys are defined:
\\{svn-status-mode-map}"
  (interactive)
  (kill-all-local-variables)

  (use-local-map svn-status-mode-map)
  (easy-menu-add svn-status-mode-menu)

  (setq major-mode 'svn-status-mode)
  (setq mode-name "svn-status")
  (setq mode-line-process 'svn-status-mode-line-process)
  (let ((view-read-only nil))
    (toggle-read-only 1)))

(defun svn-status-update-mode-line ()
  (setq svn-status-mode-line-process
        (concat svn-status-mode-line-process-edit-flag svn-status-mode-line-process-status))
  (force-mode-line-update))

(defun svn-status-bury-buffer (arg)
  "Bury the buffers used by psvn.el
Currently this is:
  *svn-status*
  *svn-log-edit*
  *svn-property-edit*
  *svn-log*
  *svn-process*
When called with a prefix argument, ARG, switch back to the window configuration that was
in use before `svn-status' was called."
  (interactive "P")
  (cond (arg
         (when svn-status-initial-window-configuration
           (set-window-configuration svn-status-initial-window-configuration)))
        (t
         (let ((bl '("*svn-log-edit*" "*svn-property-edit*" "*svn-log*" "*svn-process*")))
           (while bl
             (when (get-buffer (car bl))
               (bury-buffer (car bl)))
             (setq bl (cdr bl)))
           (when (string= (buffer-name) "*svn-status*")
             (bury-buffer))))))

(defun svn-status-find-files ()
  "Open selected file(s) for editing.
See `svn-status-marked-files' for what counts as selected."
  (interactive)
  (let ((fnames (mapcar 'svn-status-line-info->full-path (svn-status-marked-files))))
    (mapc 'find-file fnames)))


(defun svn-status-find-file-other-window ()
  "Open the file in the other window for editing."
  (interactive)
  (svn-status-ensure-cursor-on-file)
  (find-file-other-window (svn-status-line-info->filename
                           (svn-status-get-line-information))))

(defun svn-status-view-file-other-window ()
  "Open the file in the other window for viewing."
  (interactive)
  (svn-status-ensure-cursor-on-file)
  (view-file-other-window (svn-status-line-info->filename
                           (svn-status-get-line-information))))

(defun svn-status-find-file-or-examine-directory ()
  "If point is on a directory, run `svn-status' on that directory.
Otherwise run `find-file'."
  (interactive)
  (svn-status-ensure-cursor-on-file)
  (let ((line-info (svn-status-get-line-information)))
    (if (svn-status-line-info->directory-p line-info)
        (svn-status (svn-status-line-info->full-path line-info))
      (find-file (svn-status-line-info->filename line-info)))))

(defun svn-status-examine-parent ()
  "Run `svn-status' on the parent of the current directory."
  (interactive)
  (svn-status (expand-file-name "../")))

(defun svn-status-mouse-find-file-or-examine-directory (event)
  "Move point to where EVENT occurred, and do `svn-status-find-file-or-examine-directory'
EVENT could be \"mouse clicked\" or similar."
  (interactive "e")
  (mouse-set-point event)
  (svn-status-find-file-or-examine-directory))

(defun svn-status-line-info->ui-status (line-info) (nth 0 line-info))

(defun svn-status-line-info->has-usermark (line-info) (nth 0 (nth 0 line-info)))
(defun svn-status-line-info->user-elide (line-info) (nth 1 (nth 0 line-info)))

(defun svn-status-line-info->filemark (line-info) (nth 1 line-info))
(defun svn-status-line-info->propmark (line-info) (nth 2 line-info))
(defun svn-status-line-info->filename (line-info) (nth 3 line-info))
(defun svn-status-line-info->filename-nondirectory (line-info)
  (file-name-nondirectory (svn-status-line-info->filename line-info)))
(defun svn-status-line-info->localrev (line-info)
  (if (>= (nth 4 line-info) 0)
      (nth 4 line-info)
    nil))
(defun svn-status-line-info->lastchangerev (line-info)
  "Return the last revision in which LINE-INFO was modified."
  (if (>= (nth 5 line-info) 0)
      (nth 5 line-info)
    nil))
(defun svn-status-line-info->author (line-info) (nth 6 line-info))
(defun svn-status-line-info->modified-external (line-info) (nth 7 line-info))
(defun svn-status-line-info->locked (line-info)
  "Return whether LINE-INFO represents a locked file.
This is column three of the `svn status' output.
The result will be nil or \"L\".
\(A file becomes locked when an operation is interupted; run \[svn-status-cleanup]'
to unlock it.\)"
  (nth 8 line-info))
(defun svn-status-line-info->historymark (line-info)
  "Mark from column four of output from `svn status'.
This will be nil unless the file is scheduled for addition with
history, when it will be \"+\"."
  (nth 9 line-info))
(defun svn-status-line-info->switched (line-info)
  "Return whether LINE-INFO is switched relative to its parent.
This is column five of the output from `svn status'.
The result will be nil or \"S\"."
  (nth 10 line-info))

(defun svn-status-line-info->is-visiblep (line-info)
  (not (or (svn-status-line-info->hide-because-unknown line-info)
           (svn-status-line-info->hide-because-unmodified line-info)
           (svn-status-line-info->hide-because-user-elide line-info))))

(defun svn-status-line-info->hide-because-unknown (line-info)
  (and svn-status-hide-unknown
       (eq (svn-status-line-info->filemark line-info) ??)))

(defun svn-status-line-info->hide-because-unmodified (line-info)
  ;;(message " %S %S %S %S - %s" svn-status-hide-unmodified (svn-status-line-info->propmark line-info) ?_
  ;;         (svn-status-line-info->filemark line-info) (svn-status-line-info->filename line-info))
  (and svn-status-hide-unmodified
       (and (or (eq (svn-status-line-info->filemark line-info) ?_)
                (eq (svn-status-line-info->filemark line-info) ? ))
            (or (eq (svn-status-line-info->propmark line-info) ?_)
                (eq (svn-status-line-info->propmark line-info) ? )
                (eq (svn-status-line-info->propmark line-info) nil)))))

(defun svn-status-line-info->hide-because-user-elide (line-info)
  (eq (svn-status-line-info->user-elide line-info) t))

(defun svn-status-line-info->show-user-elide-continuation (line-info)
  (eq (svn-status-line-info->user-elide line-info) 'directory))

;; modify the line-info
(defun svn-status-line-info->set-filemark (line-info value)
  (setcar (nthcdr 1 line-info) value))

(defun svn-status-line-info->set-propmark (line-info value)
  (setcar (nthcdr 2 line-info) value))

(defun svn-status-line-info->set-localrev (line-info value)
  (setcar (nthcdr 4 line-info) value))

(defun svn-status-line-info->set-lastchangerev (line-info value)
  (setcar (nthcdr 5 line-info) value))

(defun svn-status-toggle-elide ()
  (interactive)
  (let ((st-info svn-status-info)
        (fname)
        (test (svn-status-line-info->filename (svn-status-get-line-information)))
        (len-test)
        (len-fname)
        (new-elide-mark t)
        (elide-mark))
    (if (member test svn-status-elided-list)
        (setq svn-status-elided-list (delete test svn-status-elided-list))
      (add-to-list 'svn-status-elided-list test))
    (when (string= test ".")
      (setq test ""))
    (setq len-test (length test))
    (while st-info
      (setq fname (svn-status-line-info->filename (car st-info)))
      (setq len-fname (length fname))
      (when (and (>= len-fname len-test)
                 (string= (substring fname 0 len-test) test))
        ;;(message "elide: %s %s" fname (svn-status-line-info->user-elide (car st-info)))
        (setq elide-mark new-elide-mark)
        (when (or (string= fname ".")
                  (and (= len-fname len-test) (svn-status-line-info->directory-p (car st-info))))
          (message "Elide directory %s and all its files." fname)
          (setq new-elide-mark (not (svn-status-line-info->user-elide (car st-info))))
          (setq elide-mark (if new-elide-mark 'directory nil)))
        ;;(message "elide-mark: %S member: %S" elide-mark (member fname svn-status-elided-list))
        (when (and (member fname svn-status-elided-list) (not elide-mark))
          (setq svn-status-elided-list (delete fname svn-status-elided-list)))
        (setcar (nthcdr 1 (svn-status-line-info->ui-status (car st-info))) elide-mark))
      (setq st-info (cdr st-info))))
  ;;(message "svn-status-elided-list: %S" svn-status-elided-list)
  (svn-status-update-buffer))

(defun svn-status-apply-elide-list ()
  "Elide files/directories according to `svn-status-elided-list'."
  (interactive)
  (let ((st-info svn-status-info)
        (fname)
        (len-fname)
        (test)
        (len-test)
        (elide-mark))
    (while st-info
      (setq fname (svn-status-line-info->filename (car st-info)))
      (setq len-fname (length fname))
      (setq elide-mark nil)

      (let ((elided-list svn-status-elided-list))
        (while elided-list
          (setq test (car elided-list))
          (when (string= test ".")
            (setq test ""))
          (setq len-test (length test))
          (when (and (>= len-fname len-test)
                     (string= (substring fname 0 len-test) test))
            (setq elide-mark t)
            (when (or (string= fname ".")
                      (and (= len-fname len-test) (svn-status-line-info->directory-p (car st-info))))
              (setq elide-mark 'directory)))
          (setq elided-list (cdr elided-list))))

      ;;(message "fname: %s elide-mark: %S" fname elide-mark)
      (setcar (nthcdr 1 (svn-status-line-info->ui-status (car st-info))) elide-mark)
      (setq st-info (cdr st-info))))
  (svn-status-update-buffer))

(defun svn-status-update-with-command-list (cmd-list)
  (save-excursion
    (set-buffer "*svn-status*")
    (let ((st-info)
          (found)
          (action))
      (setq cmd-list (sort cmd-list '(lambda (item1 item2) (string-lessp (car item1) (car item2)))))
      (while cmd-list
        (unless st-info (setq st-info svn-status-info))
        ;;(message "%S" (caar cmd-list))
        (setq found nil)
        (while (and (not found) st-info)
          (setq found (string= (caar cmd-list) (svn-status-line-info->filename (car st-info))))
          ;;(message "found: %S" found)
          (unless found (setq st-info (cdr st-info))))
        (unless found
          (message "continue to search for %s" (caar cmd-list))
          (setq st-info svn-status-info)
          (while (and (not found) st-info)
            (setq found (string= (caar cmd-list) (svn-status-line-info->filename (car st-info))))
            (unless found (setq st-info (cdr st-info)))))
        (if found
            ;;update the info line
            (progn
              (setq action (cadar cmd-list))
              ;;(message "found %s, action: %S" (caar cmd-list) action)
              (svn-status-annotate-status-buffer-entry action (car st-info)))
          (message "did not find %s" (caar cmd-list)))
        (setq cmd-list (cdr cmd-list))))))

(defun svn-status-annotate-status-buffer-entry (action line-info)
  (let ((tag-string))
    (svn-status-goto-file-name (svn-status-line-info->filename line-info))
    (cond ((equal action 'committed)
           (when svn-status-commit-rev-number
             (svn-status-line-info->set-localrev line-info svn-status-commit-rev-number))
             (svn-status-line-info->set-lastchangerev line-info svn-status-commit-rev-number)
             (setq tag-string " <committed>"))
          ((equal action 'added)
           (setq tag-string " <added>"))
          ((equal action 'deleted)
           (setq tag-string " <deleted>"))
          (t
           (message "Unknown action '%s for %s" action (svn-status-line-info->filename line-info))))
    (when tag-string
      (svn-status-line-info->set-filemark line-info ? )
      (svn-status-line-info->set-propmark line-info ? )
      (let ((buffer-read-only nil))
        (delete-region (point-at-bol) (point-at-eol))
        (svn-insert-line-in-status-buffer line-info)
        (backward-char 1)
        (insert tag-string)
        (delete-char 1)))))



;; (svn-status-update-with-command-list '(("++ideas" committed) ("a.txt" committed) ("alf")))
;; (svn-status-update-with-command-list (svn-status-parse-commit-output))


(defun svn-status-parse-commit-output ()
  "Parse the output of svn commit.
Return a list that is suitable for `svn-status-update-with-command-list'"
  (save-excursion
    (set-buffer "*svn-process*")
    (let ((action)
          (name)
          (done)
          (skip)
          (result))
      (goto-char (point-min))
      (setq svn-status-commit-rev-number nil)
      (setq skip nil) ; set to t whenever we find a line not about a committed file
      (while (< (point) (point-max))
        (cond ((= (point-at-eol) (point-at-bol)) ;skip blank lines
               (setq skip t))
              ((looking-at "Sending")
               (setq action 'committed))
              ((looking-at "Adding")
               (setq action 'added))
              ((looking-at "Deleting")
               (setq action 'deleted))
              ((looking-at "Transmitting file data")
               (setq skip t))
              ((looking-at "Committed revision \([0-9]+\)")
               (setq svn-status-commit-rev-number
                     (string-to-number (match-string-no-properties 1)))
               (setq skip t))
              (t ;; this should never be needed(?)
               (setq action 'unknown)))
        (unless skip                                ;found an interesting line
          (forward-char 15)
          (when svn-status-operated-on-dot
            ;; when the commit used . as argument, delete the trailing directory
            ;; from the svn output
            (search-forward "/" nil t))
          (setq name (buffer-substring-no-properties (point) (point-at-eol)))
          ;;(message "%S %S" action name)
          (setq result (cons (list name action)
                             result))
          (setq skip nil))
        (forward-line 1))
      result)))
;;(svn-status-parse-commit-output)
;;(svn-status-annotate-status-buffer-entry)

(defun svn-status-line-info->directory-p (line-info)
  "Return t if LINE-INFO refers to a directory, nil otherwise.
Symbolic links to directories count as directories (see `file-directory-p')."
  (file-directory-p (svn-status-line-info->filename line-info)))

(defun svn-status-line-info->full-path (line-info)
  "Return the full path of the file represented by LINE-INFO."
  (expand-file-name
   (svn-status-line-info->filename line-info)))

;;Not convinced that this is the fastest way, but...
(defun svn-status-count-/ (string)
  "Return number of \"/\"'s in STRING."
  (let ((n 0)
        (last 0))
    (while (setq last (string-match "/" string (1+ last)))
      (setq n (1+ n)))
    n))

(defun svn-insert-line-in-status-buffer (line-info)
  "Format LINE-INFO and insert the result in the current buffer."
  (let ((usermark (if (svn-status-line-info->has-usermark line-info) "*" " "))
        (external (if (svn-status-line-info->modified-external line-info)
                      (svn-add-face (if svn-status-short-mod-flag-p
                                        "** "
                                      " (modified external)")
                                    'svn-status-modified-external-face)
                    (if svn-status-short-mod-flag-p "   " "")))
        ;; To add indentation based on the
        ;; directory that the file is in, we just insert 2*(number of "/" in
        ;; filename) spaces, which is rather hacky (but works)!
        (filename (svn-status-choose-face-to-add
                   (svn-status-line-info->directory-p line-info)
                   (concat (make-string
                            (* 2 (svn-status-count-/
                                  (svn-status-line-info->filename line-info)))
                            32)
                           (if svn-status-hide-unmodified
                               (svn-status-line-info->filename line-info)
                             (svn-status-line-info->filename-nondirectory line-info)))
                   'svn-status-directory-face
                   'svn-status-filename-face))
        (elide-hint (if (svn-status-line-info->show-user-elide-continuation line-info) " ..." "")))
    (insert (svn-status-maybe-add-face
             (svn-status-line-info->has-usermark line-info)
             (concat usermark
                     (format svn-status-line-format
                             (svn-status-line-info->filemark line-info)
                             (or (svn-status-line-info->propmark line-info) ? )
                             (or (svn-status-line-info->historymark line-info) ? )
                             (or (svn-status-line-info->localrev line-info) "")
                             (or (svn-status-line-info->lastchangerev line-info) "")
                             (svn-status-line-info->author line-info)))
             'svn-status-marked-face)
            (if svn-status-short-mod-flag-p external filename)
            (if svn-status-short-mod-flag-p filename external)
            (svn-status-maybe-add-string (svn-status-line-info->locked line-info)
                                         " [ LOCKED ]" 'svn-status-locked-face)
            (svn-status-maybe-add-string (svn-status-line-info->switched line-info)
                                         " (switched)" 'svn-status-switched-face)
            elide-hint
            "\n")))

(defun svn-status-update-buffer ()
  (interactive)
  ;(message (format "buffer-name: %s" (buffer-name)))
  (unless (string= (buffer-name) "*svn-status*")
    ;;(delete-other-windows)
    ;;(split-window-vertically)
    ;;(switch-to-buffer "*svn-status*")
    (set-buffer "*svn-status*"))
  (svn-status-mode)
  (let ((st-info svn-status-info)
        (buffer-read-only nil)
        (start-pos)
        (overlay)
        (unmodified-count 0)
        (unknown-count 0)
        (marked-count 0)
        (fname (svn-status-line-info->filename (svn-status-get-line-information)))
        (fname-pos (point))
        (column (current-column)))
    (delete-region (point-min) (point-max))
    (insert "\n")
    ;; Insert all files and directories
    (while st-info
      (setq start-pos (point))
      (cond ((svn-status-line-info->has-usermark (car st-info))
             ;; Show a marked file always
             (svn-insert-line-in-status-buffer (car st-info)))
            ((svn-status-line-info->hide-because-user-elide (car st-info))
             );(message "user wanted to hide %s" (svn-status-line-info->filename (car st-info))))
            ((svn-status-line-info->hide-because-unknown (car st-info))
             (setq unknown-count (+ unknown-count 1)))
            ((svn-status-line-info->hide-because-unmodified (car st-info))
             (setq unmodified-count (+ unmodified-count 1)))
            (t
             (svn-insert-line-in-status-buffer (car st-info))))
      (when (svn-status-line-info->has-usermark (car st-info))
        (setq marked-count (+ marked-count 1)))
      (setq overlay (make-overlay start-pos (point)))
      (overlay-put overlay 'svn-info (car st-info))
      (setq st-info (cdr st-info)))
    ;; Insert status information at the buffer beginning
    (goto-char (point-min))
    (insert (format "svn status for directory %s%s\n"
                    default-directory
                    (if svn-status-head-revision (format " (status against revision: %s)"
                                                         svn-status-head-revision)
                      "")))
    (when svn-status-base-info
      (insert (concat "Repository: " (svn-status-base-info->url) "\n")))
    (when svn-status-hide-unknown
      (insert
       (format "%d Unknown files are hidden - press ? to toggle hiding\n"
               unknown-count)))
    (when svn-status-hide-unmodified
      (insert
       (format "%d Unmodified files are hidden - press _ to toggle hiding\n"
               unmodified-count)))
    (insert (format "%d files marked\n" marked-count))
    (setq svn-start-of-file-list-line-number (+ (count-lines (point-min) (point)) 1))
    (if fname
        (progn
          (goto-char fname-pos)
          (svn-status-goto-file-name fname)
          (goto-char (+ column (point-at-bol))))
      (goto-char (+ (next-overlay-change (point-min)) svn-status-default-column)))))

(defun svn-status-parse-info (arg)
  "Parse the svn info output for the base directory.
Show the repository url after this call in the *svn-status* buffer.
When called with the prefix argument 0, reset the information to nil.
This hides the repository information again."
  (interactive "P")
  (if (eq arg 0)
      (setq svn-status-base-info nil)
    (svn-run-svn nil t 'parse-info "info" ".")
    (svn-status-parse-info-result))
  (svn-status-update-buffer))

(defun svn-status-parse-info-result ()
  (let ((url))
    (save-excursion
      (set-buffer "*svn-process*")
      (goto-char (point-min))
      (search-forward "Url: ")
      (setq url (buffer-substring-no-properties (point) (point-at-eol))))
    (setq svn-status-base-info `((url ,url)))))

(defun svn-status-base-info->url ()
  (if svn-status-base-info
      (cadr (assoc 'url svn-status-base-info))
    ""))

(defun svn-status-toggle-edit-cmd-flag (&optional reset)
  (interactive)
  (cond ((or reset (eq svn-status-edit-svn-command 'sticky))
         (setq svn-status-edit-svn-command nil))
        ((eq svn-status-edit-svn-command nil)
         (setq svn-status-edit-svn-command t))
        ((eq svn-status-edit-svn-command t)
         (setq svn-status-edit-svn-command 'sticky)))
  (cond ((eq svn-status-edit-svn-command t)
         (setq svn-status-mode-line-process-edit-flag " EditCmd"))
        ((eq svn-status-edit-svn-command 'sticky)
         (setq svn-status-mode-line-process-edit-flag " EditCmd#"))
        (t
         (setq svn-status-mode-line-process-edit-flag "")))
  (svn-status-update-mode-line))

(defun svn-status-goto-root-or-return ()
  "Bounce point between the root (\".\") and the current line."
  (interactive)
  (if (string= (svn-status-line-info->filename (svn-status-get-line-information)) ".")
      (when svn-status-root-return-info
        (svn-status-goto-file-name
         (svn-status-line-info->filename svn-status-root-return-info)))
    (setq svn-status-root-return-info (svn-status-get-line-information))
    (svn-status-goto-file-name ".")))

(defun svn-status-next-line (nr-of-lines)
  (interactive "p")
  (next-line nr-of-lines)
  (when (svn-status-get-line-information)
    (goto-char (+ (point-at-bol) svn-status-default-column))))

(defun svn-status-previous-line (nr-of-lines)
  (interactive "p")
  (previous-line nr-of-lines)
  (when (svn-status-get-line-information)
    (goto-char (+ (point-at-bol) svn-status-default-column))))

(defun svn-status-update (&optional arg)
  "Run 'svn status -v'.
When called with a prefix argument run 'svn status -vu'."
  (interactive "P")
  (unless (interactive-p)
    (save-excursion
      (set-buffer "*svn-process*")
      (setq svn-status-update-previous-process-output (buffer-substring (point-min) (point-max)))))
  (svn-status default-directory arg))

(defun svn-status-get-line-information ()
  "Find out about the file under point.
The result may be parsed with the various `svn-status-line-info->...' functions."
  (let ((overlay (car (overlays-at (point)))))
    (when overlay
      (overlay-get overlay 'svn-info))))

(defun svn-status-get-file-list (use-marked-files)
  "Get either the marked files or the files, where the cursor is on."
  (if use-marked-files
      (svn-status-marked-files)
    (list (svn-status-get-line-information))))

(defun svn-status-get-file-list-names (use-marked-files)
  (mapcar 'svn-status-line-info->filename (svn-status-get-file-list use-marked-files)))

(defun svn-status-select-line ()
  (interactive)
  (let ((info (svn-status-get-line-information)))
    (if info
        (message "%S %S %S" info (svn-status-line-info->hide-because-unknown info)
                                 (svn-status-line-info->hide-because-unmodified info))
      (message "No file on this line"))))

(defun svn-status-ensure-cursor-on-file ()
  (unless (svn-status-get-line-information)
    (error "No file on the current line")))

(defun svn-status-directory-containing-point (allow-self)
  "Find the (full path of) directory containing the file under point.

If ALLOW-SELF and the file is a directory, return that directory,
otherwise return the directory containing the file under point."
  ;;the first `or' below is because s-s-g-l-i returns `nil' if
  ;;point was outside the file list, but we need
  ;;s-s-l-i->f to return a string to add to `default-directory'.
  (let ((line-info (or (svn-status-get-line-information)
                       '(nil nil nil ""))))
    (file-name-as-directory
     (expand-file-name
      (if (and allow-self (svn-status-line-info->directory-p line-info))
          (svn-status-line-info->filename line-info)
        ;;The next `or' is because (file-name-directory "file") returns nil
        (or (file-name-directory (svn-status-line-info->filename line-info))
            "."))))))

(defun svn-status-set-user-mark (arg)
  "Set a user mark on the current file or directory.
If the cursor is on a file this file is marked and the cursor advances to the next line.
If the cursor is on a directory all files in this directory are marked.

If this function is called with a prefix argument, only the current line is
marked, even if it is a directory."
  (interactive "P")
  (let ((info (svn-status-get-line-information)))
    (if info
        (progn
          (svn-status-apply-usermark t arg)
          (svn-status-next-line 1))
      (message "No file on this line - cannot set a mark"))))

(defun svn-status-unset-user-mark (arg)
  "Remove a user mark on the current file or directory.
If the cursor is on a file, this file is unmarked and the cursor advances to the next line.
If the cursor is on a directory, all files in this directory are unmarked.

If this function is called with a prefix argument, only the current line is
unmarked, even if is a directory."
  (interactive "P")
  (let ((info (svn-status-get-line-information)))
    (if info
        (progn
          (svn-status-apply-usermark nil arg)
          (svn-status-next-line 1))
      (message "No file on this line - cannot unset a mark"))))

(defun svn-status-unset-user-mark-backwards ()
  "Remove a user mark from the previous file.
Then move to that line."
  ;; This is consistent with `dired-unmark-backward' and
  ;; `cvs-mode-unmark-up'.
  (interactive)
  (let ((info (save-excursion
                (svn-status-next-line -1)
                (svn-status-get-line-information))))
    (if info
        (progn
          (svn-status-next-line -1)
          (svn-status-apply-usermark nil t))
      (message "No file on previous line - cannot unset a mark"))))

(defun svn-status-apply-usermark (set-mark only-this-line)
  "Do the work for the various marking/unmarking functions."
  (let* ((st-info svn-status-info)
         (line-info (svn-status-get-line-information))
         (file-name (svn-status-line-info->filename line-info))
     (sub-file-regexp (concat "^" (regexp-quote
                       (file-name-as-directory file-name))))
         (newcursorpos-fname)
         (i-fname)
         (current-line svn-start-of-file-list-line-number))
    (while st-info
      (when (svn-status-line-info->is-visiblep (car st-info))
        (setq current-line (1+ current-line)))
      (setq i-fname (svn-status-line-info->filename (car st-info)))
      (when (or (string= file-name i-fname)
        (string-match sub-file-regexp i-fname))
        (when (svn-status-line-info->is-visiblep (car st-info))
          (when (or (not only-this-line) (string= file-name i-fname))
            (setq newcursorpos-fname i-fname)
            (if set-mark
                (message "marking: %s" i-fname)
              (message "unmarking: %s" i-fname))
            ;;(message "ui-status: %S" (svn-status-line-info->ui-status (car st-info)))
            (setcar (svn-status-line-info->ui-status (car st-info)) set-mark)
            (save-excursion
              (let ((buffer-read-only nil))
                (goto-line current-line)
                (delete-region (point-at-bol) (point-at-eol))
                (svn-insert-line-in-status-buffer (car st-info))
                (delete-char 1))))))
      (setq st-info (cdr st-info)))
    ;;(svn-status-update-buffer)
    (svn-status-goto-file-name newcursorpos-fname)))

(defun svn-status-apply-usermark-checked (check-function set-mark)
  "Mark or unmark files, whether a given function returns t.
The function is called with the line information. Therefore the svnstatus-line-info->* functions can be
used in the check."
  (let ((st-info svn-status-info))
    (while st-info
      (when (apply check-function (list (car st-info)))
        (if set-mark
            (when (not (svn-status-line-info->has-usermark (car st-info)))
              (message "marking: %s" (svn-status-line-info->filename (car st-info))))
          (when (svn-status-line-info->has-usermark (car st-info))
            (message "unmarking: %s" (svn-status-line-info->filename (car st-info)))))
        (setcar (svn-status-line-info->ui-status (car st-info)) set-mark))
      (setq st-info (cdr st-info)))
    (svn-status-update-buffer)))

(defun svn-status-mark-unknown (arg)
  "Mark all unknown files.
These are the files marked with '?' in the *svn-status* buffer.
If the function is called with a prefix arg, unmark all these files."
  (interactive "P")
  (svn-status-apply-usermark-checked
   '(lambda (info) (eq (svn-status-line-info->filemark info) ??)) (not arg)))

(defun svn-status-mark-added (arg)
  "Mark all added files.
These are the files marked with 'A' in the *svn-status* buffer.
If the function is called with a prefix ARG, unmark all these files."
  (interactive "P")
  (svn-status-apply-usermark-checked
   '(lambda (info) (eq (svn-status-line-info->filemark info) ?A)) (not arg)))

(defun svn-status-mark-modified (arg)
  "Mark all modified files.
These are the files marked with 'M' in the *svn-status* buffer.
If the function is called with a prefix ARG, unmark all these files."
  (interactive "P")
  (svn-status-apply-usermark-checked
   '(lambda (info) (or (eq (svn-status-line-info->filemark info) ?M)
                       (eq (svn-status-line-info->filemark info)
                           svn-status-file-modified-after-save-flag)))
   (not arg)))

(defun svn-status-mark-deleted (arg)
  "Mark all files scheduled for deletion.
These are the files marked with 'D' in the *svn-status* buffer.
If the function is called with a prefix ARG, unmark all these files."
  (interactive "P")
  (svn-status-apply-usermark-checked
   '(lambda (info) (eq (svn-status-line-info->filemark info) ?D)) (not arg)))

(defun svn-status-mark-changed (arg)
  "Mark all files that could be committed.
This means we mark
* all modified files
* all files scheduled for addition
* all files scheduled for deletion

The last two categories include all copied and moved files.
If called with a prefix ARG, unmark all such files."
  (interactive "P")
  (svn-status-mark-added arg)
  (svn-status-mark-modified arg)
  (svn-status-mark-deleted arg))

(defun svn-status-unset-all-usermarks ()
  (interactive)
  (svn-status-apply-usermark-checked '(lambda (info) t) nil))

(defun svn-status-toggle-hide-unknown ()
  (interactive)
  (setq svn-status-hide-unknown (not svn-status-hide-unknown))
  (svn-status-update-buffer))

(defun svn-status-toggle-hide-unmodified ()
  (interactive)
  (setq svn-status-hide-unmodified (not svn-status-hide-unmodified))
  (svn-status-update-buffer))

(defun svn-status-goto-file-name (name)
  ;; (message "svn-status-goto-file-name: %s %d" name (point))
  (let ((start-pos (point))
        (found))
    ;; performance optimization: search from point to end of buffer
    (while (and (not found) (< (point) (point-max)))
      (goto-char (next-overlay-change (point)))
      (when (string= name (svn-status-line-info->filename
                           (svn-status-get-line-information)))
        (setq start-pos (+ (point) svn-status-default-column))
        (setq found t)))
    ;; search from buffer start to point
    (goto-char (point-min))
    (while (and (not found) (< (point) start-pos))
      (goto-char (next-overlay-change (point)))
      (when (string= name (svn-status-line-info->filename
                           (svn-status-get-line-information)))
        (setq start-pos (+ (point) svn-status-default-column))
        (setq found t)))
    (unless found
      (message "Warning: svn-status-goto-file-name: %s not found" name))
    (goto-char start-pos)))

(defun svn-status-find-info-for-file-name (name)
  (let* ((st-info svn-status-info)
         (info))
    (while st-info
      (when (string= name (svn-status-line-info->filename (car st-info)))
        (setq info (car st-info))
        (setq st-info nil)) ; terminate loop
      (setq st-info (cdr st-info)))
    info))

(defun svn-status-marked-files ()
  "Return all files marked by `svn-status-set-user-mark',
or (if no files were marked) the file under point."
  (let* ((st-info svn-status-info)
         (file-list))
    (while st-info
      (when (svn-status-line-info->has-usermark (car st-info))
        (setq file-list (append file-list (list (car st-info)))))
      (setq st-info (cdr st-info)))
    (or file-list
        (if (svn-status-get-line-information)
            (list (svn-status-get-line-information))
          nil))))

(defun svn-status-marked-file-names ()
  (mapcar 'svn-status-line-info->filename (svn-status-marked-files)))

(defun svn-status-ui-information-hash-table ()
  (let ((st-info svn-status-info)
        (svn-status-ui-information (make-hash-table :test 'equal)))
    (while st-info
      (puthash (svn-status-line-info->filename (car st-info))
               (svn-status-line-info->ui-status (car st-info))
               svn-status-ui-information)
      (setq st-info (cdr st-info)))
    svn-status-ui-information))


(defun svn-status-create-arg-file (file-name prefix file-info-list postfix)
  (with-temp-file file-name
    (insert prefix)
    (let ((st-info file-info-list))
      (while st-info
        (insert (svn-status-line-info->filename (car st-info)))
        (insert "\n")
        (setq st-info (cdr st-info)))

    (insert postfix))))

(defun svn-status-show-process-buffer-internal (&optional scroll-to-top)
  (when (eq (current-buffer) "*svn-status*")
    (delete-other-windows))
  (pop-to-buffer "*svn-process*")
  (when svn-status-wash-control-M-in-process-buffers
    (svn-status-remove-control-M))
  (when scroll-to-top
    (goto-char (point-min)))
  (other-window 1))

(defun svn-status-show-svn-log (arg)
  "Run `svn log' on selected files.
The output is put into the *svn-log* buffer
The optional prefix argument ARG determines which switches are passed to `svn log':
 no prefix               --- use whatever is in the string `svn-status-default-log-arguments'
 prefix argument of -1   --- use no arguments
 prefix argument of 0:   --- use the -q switch (quiet)
 other prefix arguments: --- use the -v switch (verbose)

See `svn-status-marked-files' for what counts as selected."
  (interactive "P")
  (let ((switch (cond ((eq arg 0) "-q")
                      ((eq arg -1) "")
                      (arg        "-v")
                      (t          svn-status-default-log-arguments))))
    (svn-status-create-arg-file svn-status-temp-arg-file "" (svn-status-marked-files) "")
    (if (> (length switch) 0)
        (svn-run-svn t t 'log "log" "--targets" svn-status-temp-arg-file switch)
      (svn-run-svn t t 'log "log" "--targets" svn-status-temp-arg-file))
    (save-excursion
      (set-buffer "*svn-process*")
      (svn-log-view-mode))))

(defun svn-status-info ()
  "Run `svn info' on all selected files.
See `svn-status-marked-files' for what counts as selected."
  (interactive)
  (svn-status-create-arg-file svn-status-temp-arg-file "" (svn-status-marked-files) "")
  (svn-run-svn t t 'info "info" "--targets" svn-status-temp-arg-file))

;; Todo: add possiblity to specify the revision
(defun svn-status-blame ()
  "Run `svn blame' on the current file."
  (interactive)
  ;;(svn-run-svn t t 'blame "blame" "-r" "BASE" (svn-status-line-info->filename (svn-status-get-line-information))))
  (svn-run-svn t t 'blame "blame" (svn-status-line-info->filename (svn-status-get-line-information))))

(defun svn-status-show-svn-diff (arg)
  "Run `svn diff' on the current file.
If there is a newer revision in the repository, the diff is done against HEAD, otherwise
compare the working copy with BASE.
If ARG then prompt for revision to diff against."
  (interactive "P")
  (svn-status-ensure-cursor-on-file)
  (svn-status-show-svn-diff-internal arg nil))

(defun svn-status-show-svn-diff-for-marked-files (arg)
  "Run `svn diff' on all selected files.
See `svn-status-marked-files' for what counts as selected.
If ARG then prompt for revision to diff against, else compare working copy with BASE."
  (interactive "P")
  (svn-status-show-svn-diff-internal arg t))

(defun svn-status-show-svn-diff-internal (arg &optional use-all-marked-files)
  (let* ((fl (if use-all-marked-files
                 (svn-status-marked-files)
               (list (svn-status-get-line-information))))
         (clear-buf t)
         (revision (if arg
                       (svn-status-read-revision-string "Diff with files for version: " "PREV")
                     (if use-all-marked-files
                         "BASE"
                       (if (svn-status-line-info->modified-external (car fl)) "HEAD" "BASE")))))
    (while fl
      (svn-run-svn nil clear-buf 'diff "diff" "-r" revision (svn-status-line-info->filename (car fl)))
      (setq clear-buf nil)
      (setq fl (cdr fl))))
  (svn-status-show-process-buffer-internal t)
  (save-excursion
    (set-buffer "*svn-process*")
    (diff-mode)
    (font-lock-fontify-buffer)))

(defun svn-status-show-process-buffer ()
  (interactive)
  (svn-status-show-process-buffer-internal))

(defun svn-status-add-file-recursively (arg)
  "Run `svn add' on all selected files.
When a directory is added, add files recursively.
See `svn-status-marked-files' for what counts as selected.
When this function is called with a prefix argument, use the actual file instead."
  (interactive "P")
  (message "adding: %S" (svn-status-get-file-list-names (not arg)))
  (svn-status-create-arg-file svn-status-temp-arg-file "" (svn-status-get-file-list (not arg)) "")
  (svn-run-svn t t 'add "add" "--targets" svn-status-temp-arg-file))

(defun svn-status-add-file (arg)
  "Run `svn add' on all selected files.
When a directory is added, don't add the files of the directory
 (svn add --non-recursive <file-list> is called).
See `svn-status-marked-files' for what counts as selected.
When this function is called with a prefix argument, use the actual file instead."
  (interactive "P")
  (message "adding: %S" (svn-status-get-file-list-names (not arg)))
  (svn-status-create-arg-file svn-status-temp-arg-file "" (svn-status-get-file-list (not arg)) "")
  (svn-run-svn t t 'add "add" "--non-recursive" "--targets" svn-status-temp-arg-file))

(defun svn-status-make-directory (dir)
  "Run `svn mkdir DIR'."
  ;; TODO: Allow entering a URI interactively.
  ;; Currently, `read-file-name' corrupts it.
  (interactive (list (read-file-name "Make directory: "
                                     (svn-status-directory-containing-point t))))
  (unless (string-match "^[^:/]+://" dir) ; Is it a URI?
    (setq dir (file-relative-name dir)))
  (svn-run-svn t t 'mkdir "mkdir" "--" dir))

;;TODO: write a svn-status-cp similar to this---maybe a common
;;function to do both?
(defun svn-status-mv ()
  "Prompt for a destination, and `svn mv' selected files there.
See `svn-status-marked-files' for what counts as `selected'.

If one file was selected then the destination DEST should be a
filename to rename the selected file to, or a directory to move the
file into; if multiple files were selected then DEST should be a
directory to move the selected files into.

The default DEST is the directory containing point.

BUG: If we've marked some directory containging a file as well as the
file itself, then we should just mv the directory, but this implementation
doesn't check for that.
SOLUTION: for each dir, umark all its contents (but not the dir
itself) before running mv."
  (interactive)
  (let* ((marked-files (svn-status-marked-files))
         (num-of-files (length marked-files))
         original
         dest)
    (if (= 1 num-of-files)
        ;; one file to rename, prompt for new name, or directory to move the
        ;; file into.
        (setq dest (read-file-name (format "Rename %s to: "
                                           (svn-status-line-info->filename (car marked-files)))
                                   (svn-status-directory-containing-point t)))
      ;;multiple files selected, so prompt for existing directory to mv them into.
      (setq dest (read-directory-name (format "Move %d files to directory: " num-of-files)
                                      (svn-status-directory-containing-point t) nil t))
      (unless (file-directory-p dest)
        (error "%s is not a directory" dest)))
    (when (string= dest "")
      (error "No destination entered; no files moved"))
    (unless (string-match "^[^:/]+://" dest) ; Is it a URI?
      (setq dest (file-relative-name dest)))
;
    ;;do the move: svn mv only lets us move things once at a time, so
    ;;we need to run svn mv once for each file (hence second arg to
    ;;svn-run-svn is nil.)

    ;;TODO: before doing any moving, For every marked directory,
    ;;ensure none of its contents are also marked, since we dont want
    ;;to move both file *and* its parent...
    ;; what about hidden files?? what if user marks a dir+contents, then presses `_' ??
;;   ;one solution:
;;      (dolist (original marked-files)
;;          (when (svn-status-line-info->directory-p original)
;;              ;; run  svn-status-goto-file-name to move point to line of file
;;              ;; run  svn-status-unset-user-mark to unmark dir+all contents
;;              ;; run  svn-status-set-user-mark   to remark dir
;;              ;; maybe check for local mods here, and unmark if user does't say --force?
;;              ))
        (dolist (original marked-files)
      (let ((original-name (svn-status-line-info->filename original))
                        (original-filemarks (svn-status-line-info->filemark original))
                        (original-propmarks (svn-status-line-info->propmark original)))
        (cond
         ((or (eq original-filemarks 77)  ;;original has local mods: maybe do `svn mv --force'
              (eq original-propmarks 77)) ;;original has local prop mods: maybe do `svn mv --force'
          (if (yes-or-no-p (format "%s has local modifications; use `--force' to really move it? "
                                   original-name))
              (svn-run-svn nil t 'mv "mv" "--force" "--" original-name dest)
            (message "Not moving %s" original-name)))
         ((eq original-filemarks 63) ;;original is unversioned: maybe do plain `mv'
          (if (yes-or-no-p (format "%s is unversioned.  Use plain `mv -i %s %s'? "
                                   original-name original-name dest))
              (call-process "mv" nil (get-buffer-create "*svn-process*") nil "-i" original-name dest)
            (message "Not moving %s" original-name)))

         ((eq original-filemarks 65) ;;original has `A' mark (eg it was `svn add'ed, but not committed)
          (message "Not moving %s (try committing it first)" original-name))

         ((eq original-filemarks 32) ;;original is unmodified: can use `svn mv'
          (svn-run-svn nil t 'mv "mv" "--" original-name dest))

         ;;file is conflicted in some way?
         (t
          (if (yes-or-no-p (format "The status of %s looks scary.  Risk moving it anyway? " original-name))
              (svn-run-svn nil t 'mv "mv" "--" original-name dest)
            (message "Not moving %s" original-name))))))
        (svn-status-update)))

(defun svn-status-revert ()
  "Run `svn revert' on all selected files.
See `svn-status-marked-files' for what counts as selected."
  (interactive)
  (let* ((marked-files (svn-status-marked-files))
         (num-of-files (length marked-files)))
    (when (yes-or-no-p
           (if (= 1 num-of-files)
               (format "Revert %s? " (svn-status-line-info->filename (car marked-files)))
             (format "Revert %d files? " num-of-files)))
      (message "reverting: %S" (svn-status-marked-file-names))
      (svn-status-create-arg-file svn-status-temp-arg-file "" (svn-status-marked-files) "")
      (svn-run-svn t t 'revert "revert" "--targets" svn-status-temp-arg-file))))

(defun svn-status-rm (force)
  "Run `svn rm' on all selected files.
See `svn-status-marked-files' for what counts as selected.
When called with a prefix argument add the command line switch --force."
  (interactive "P")
  (let* ((marked-files (svn-status-marked-files))
         (num-of-files (length marked-files)))
    (when (yes-or-no-p
           (if (= 1 num-of-files)
               (format "%sRemove %s? " (if force "Force " "") (svn-status-line-info->filename (car marked-files)))
             (format "%sRemove %d files? " (if force "Force " "") num-of-files)))
      (message "removing: %S" (svn-status-marked-file-names))
      (svn-status-create-arg-file svn-status-temp-arg-file "" (svn-status-marked-files) "")
      (if force
          (svn-run-svn t t 'rm "rm" "--force" "--targets" svn-status-temp-arg-file)
        (svn-run-svn t t 'rm "rm" "--targets" svn-status-temp-arg-file)))))

(defun svn-status-update-cmd ()
  (interactive)
  ;TODO: use file names also
  (svn-run-svn t t 'update "update"))

(defun svn-status-commit-file ()
  (interactive)
  (let* ((marked-files (svn-status-marked-files)))
    (setq svn-status-files-to-commit marked-files)
    (svn-log-edit-show-files-to-commit)
    (svn-status-pop-to-commit-buffer)
    (when svn-log-edit-insert-files-to-commit
      (svn-log-edit-insert-files-to-commit))))

(defun svn-status-pop-to-commit-buffer ()
  (interactive)
  (setq svn-status-pre-commit-window-configuration (current-window-configuration))
  (let* ((use-existing-buffer (get-buffer "*svn-log-edit*"))
         (commit-buffer (get-buffer-create "*svn-log-edit*"))
         (dir default-directory))
    (pop-to-buffer commit-buffer)
    (setq default-directory dir)
    (unless use-existing-buffer
      (when (and svn-log-edit-file-name (file-readable-p svn-log-edit-file-name))
        (insert-file-contents svn-log-edit-file-name)))
    (svn-log-edit)))

(defun svn-status-cleanup ()
  (interactive)
  (let ((file-names (svn-status-marked-file-names)))
    (if file-names
        (progn
          ;(message "svn-status-cleanup %S" file-names))
          (svn-run-svn t t 'cleanup (append (list "cleanup") file-names)))
      (message "No valid file selected - No status cleanup possible"))))

(defun svn-status-resolved ()
  "Run `svn resolved' on all selected files.
See `svn-status-marked-files' for what counts as selected."
  (interactive)
  (let* ((marked-files (svn-status-marked-files))
         (num-of-files (length marked-files)))
    (when (yes-or-no-p
           (if (= 1 num-of-files)
               (format "Resolve %s? " (svn-status-line-info->filename (car marked-files)))
             (format "Resolve %d files? " num-of-files)))
      (message "resolving: %S" (svn-status-marked-file-names))
      (svn-status-create-arg-file svn-status-temp-arg-file "" (svn-status-marked-files) "")
      (svn-run-svn t t 'resolved "resolved" "--targets" svn-status-temp-arg-file))))

;; --------------------------------------------------------------------------------
;; Update the *svn-status* buffer, when a file is saved
;; --------------------------------------------------------------------------------

(defvar svn-status-file-modified-after-save-flag ?m
  "Flag shown whenever a file is modified and saved in Emacs.
The flag is shown in the *svn-status* buffer.
Recommended values are ?m or ?M.")
(defun svn-status-after-save-hook ()
  "Set a modified indication, when a file is saved from a svn working copy."
  (let* ((svn-dir (car-safe svn-status-directory-history))
         (svn-dir (when svn-dir (expand-file-name svn-dir)))
         (file-dir (file-name-directory (buffer-file-name)))
         (svn-dir-len (length (or svn-dir "")))
         (file-dir-len (length file-dir))
         (file-name))
    (when (and svn-dir
               (>= file-dir-len svn-dir-len)
               (string= (substring file-dir 0 svn-dir-len) svn-dir))
      (setq file-name (substring (buffer-file-name) svn-dir-len))
      ;;(message (format "In svn-status directory %S" file-name))
      (let ((st-info svn-status-info)
            (i-fname))
        (while st-info
          (setq i-fname (svn-status-line-info->filename (car st-info)))
          ;;(message (format "i-fname=%S" i-fname))
          (when (and (string= file-name i-fname)
                     (not (eq (svn-status-line-info->filemark (car st-info)) ??)))
            (svn-status-line-info->set-filemark (car st-info)
                                                svn-status-file-modified-after-save-flag)
            (save-excursion
              (set-buffer "*svn-status*")
              (svn-status-goto-file-name i-fname)
              (let ((buffer-read-only nil))
                (delete-region (point-at-bol) (point-at-eol))
                (svn-insert-line-in-status-buffer (car st-info))
                (delete-char 1))))
          (setq st-info (cdr st-info))))))
  nil)

(add-hook 'after-save-hook 'svn-status-after-save-hook)

;; --------------------------------------------------------------------------------
;; Getting older revisions
;; --------------------------------------------------------------------------------

(defun svn-status-get-specific-revision (arg)
  "Retrieve older revisions.
The older revisions are stored in backup files named F.~REVISION~.

When the function is called without a prefix argument: get all marked files.
Otherwise get only the actual file."
  (interactive "P")
  (svn-status-get-specific-revision-internal (not arg) t))

(defun svn-status-get-specific-revision-internal (&optional only-actual-file arg)
  (let* ((file-names (if only-actual-file
                         (list (svn-status-line-info->filename (svn-status-get-line-information)))
                       (svn-status-marked-file-names)))
         (revision (if arg (svn-status-read-revision-string "Get files for version: " "PREV") "BASE"))
         (file-name)
         (file-name-with-revision))
    (message "Getting revision %s for %S" revision file-names)
    (setq svn-status-get-specific-revision-file-info nil)
    (while file-names
      (setq file-name (car file-names))
      (setq file-name-with-revision (concat file-name ".~" revision "~"))
      (add-to-list 'svn-status-get-specific-revision-file-info
                   (cons file-name file-name-with-revision))
      (save-excursion
        (find-file file-name-with-revision)
        (setq buffer-read-only nil)
        (delete-region (point-min) (point-max))
        (svn-run-svn nil t 'cat (append (list "cat" "-r" revision) (list file-name)))
        ;;todo: error processing
        ;;svn: Filesystem has no item
        ;;svn: file not found: revision `15', path `/trunk/file.txt'
        (insert-buffer-substring "*svn-process*")
        (save-buffer))
      (setq file-names (cdr file-names)))
    (setq svn-status-get-specific-revision-file-info
      (nreverse svn-status-get-specific-revision-file-info))
    (message "svn-status-get-specific-revision-file-info: %S"
             svn-status-get-specific-revision-file-info)))


(defun svn-status-ediff-with-revision (arg)
  "Run ediff on the current file with a previous revision.
If ARG then prompt for revision to diff against."
  (interactive "P")
  (svn-status-get-specific-revision-internal t arg)
  (let* ((ediff-after-quit-destination-buffer (current-buffer))
         (my-buffer (find-file-noselect (caar svn-status-get-specific-revision-file-info)))
         (base-buff (find-file-noselect (cdar svn-status-get-specific-revision-file-info)))
         (svn-transient-buffers (list base-buff ))
         (startup-hook '(svn-ediff-startup-hook)))
    (ediff-buffers my-buffer base-buff  startup-hook)))

(defun svn-ediff-startup-hook ()
  (add-hook 'ediff-after-quit-hook-internal
        `(lambda ()
           (svn-ediff-exit-hook
        ',ediff-after-quit-destination-buffer ',svn-transient-buffers))
        nil 'local))

(defun svn-ediff-exit-hook (svn-buf tmp-bufs)
  ;; kill the temp buffers (and their associated windows)
  (dolist (tb tmp-bufs)
    (when (and tb (buffer-live-p tb) (not (buffer-modified-p tb)))
      (let ((win (get-buffer-window tb t)))
    (when win (delete-window win))
    (kill-buffer tb))))
  ;; switch back to the *svn* buffer
  (when (and svn-buf (buffer-live-p svn-buf)
         (not (get-buffer-window svn-buf t)))
    (ignore-errors (switch-to-buffer svn-buf))))


(defun svn-status-read-revision-string (prompt &optional default-value)
  "Prompt the user for a svn revision number."
  (interactive)
  (read-string prompt default-value))

;; --------------------------------------------------------------------------------
;; SVN process handling
;; --------------------------------------------------------------------------------

(defun svn-process-kill ()
  "Kill the current running svn process."
  (interactive)
  (let ((process (get-process "svn")))
    (if process
        (delete-process process)
      (message "No running svn process"))))

(defun svn-process-send-string (string)
  "Send a string to the running svn process.
This is useful, if the running svn process asks the user a question.
Note: use C-q C-j to send a line termination character."
  (interactive "sSend string to svn process: ")
  (save-excursion
    (set-buffer "*svn-process*")
    (let ((buffer-read-only nil))
      (insert string))
    (set-marker (process-mark (get-process "svn")) (point)))
  (process-send-string "svn" string))

;; --------------------------------------------------------------------------------
;; Property List stuff
;; --------------------------------------------------------------------------------

(defun svn-status-property-list ()
  (interactive)
  (let ((file-names (svn-status-marked-file-names)))
    (if file-names
        (progn
          (svn-run-svn t t 'proplist (append (list "proplist" "-v") file-names)))
      (message "No valid file selected - No property listing possible"))))

(defun svn-status-proplist-start ()
  (svn-run-svn t t 'proplist-parse "proplist" (svn-status-line-info->filename
                                               (svn-status-get-line-information))))

(defun svn-status-property-parse ()
  (interactive)
  (svn-status-proplist-start))

(defun svn-status-property-edit-one-entry (arg)
  "Edit a property.
When called with a prefix argument, it is possible to enter a new property."
  (interactive "P")
  (setq svn-status-property-edit-must-match-flag (not arg))
  (svn-status-proplist-start))

(defun svn-status-property-set ()
  (interactive)
  (setq svn-status-property-edit-must-match-flag nil)
  (svn-status-proplist-start))

(defun svn-status-property-delete ()
  (interactive)
  (setq svn-status-property-edit-must-match-flag t)
  (svn-status-proplist-start))

(defun svn-status-property-parse-property-names ()
  ;(svn-status-show-process-buffer-internal t)
  (message "svn-status-property-parse-property-names")
  (let ((pl)
        (pfl)
        (prop-name)
        (prop-value))
    (save-excursion
      (set-buffer "*svn-process*")
      (goto-char (point-min))
      (forward-line 1)
      (while (looking-at "  \\(.+\\)")
        (setq pl (append pl (list (match-string 1))))
        (forward-line 1)))
    ;(cond last-command: svn-status-property-set, svn-status-property-edit-one-entry
    ;svn-status-property-parse:
    (cond ((eq last-command 'svn-status-property-parse)
           ;(message "%S %S" pl last-command)
           (while pl
             (svn-run-svn nil t 'propget-parse "propget" (car pl)
                          (svn-status-line-info->filename
                           (svn-status-get-line-information)))
             (save-excursion
               (set-buffer "*svn-process*")
               (setq pfl (append pfl (list
                                      (list
                                       (car pl)
                                       (buffer-substring
                                        (point-min) (- (point-max) 1)))))))
             (setq pl (cdr pl))
             (message "%S" pfl)))
          ((eq last-command 'svn-status-property-edit-one-entry)
           ;;(message "svn-status-property-edit-one-entry")
           (setq prop-name
                 (completing-read "Set Property - Name: " (mapcar 'list pl)
                                  nil svn-status-property-edit-must-match-flag))
           (unless (string= prop-name "")
             (save-excursion
               (set-buffer "*svn-status*")
               (svn-status-property-edit (list (svn-status-get-line-information))
                                         prop-name))))
          ((eq last-command 'svn-status-property-set)
           (message "svn-status-property-set")
           (setq prop-name
                 (completing-read "Set Property - Name: " (mapcar 'list pl) nil nil))
           (setq prop-value (read-from-minibuffer "Property value: "))
           (unless (string= prop-name "")
             (save-excursion
               (set-buffer "*svn-status*")
               (message "setting property %s := %s for %S" prop-name prop-value
                        (svn-status-marked-files)))))
          ((eq last-command 'svn-status-property-delete)
           (setq prop-name
                 (completing-read "Delete Property - Name: " (mapcar 'list pl) nil t))
           (unless (string= prop-name "")
             (save-excursion
               (set-buffer "*svn-status*")
               (let ((file-names (svn-status-marked-file-names)))
                 (when file-names
                   (message "Going to delete prop %s for %s" prop-name file-names)
                   (svn-run-svn t t 'propdel
                                (append (list "propdel" prop-name) file-names))))))))))

(defun svn-status-property-edit (file-info-list prop-name &optional new-prop-value)
  (let* ((commit-buffer (get-buffer-create "*svn-property-edit*"))
         (dir default-directory)
         ;; now only one file is implemented ...
         (file-name (svn-status-line-info->filename (car file-info-list)))
         (prop-value))
    (message "Edit property %s for file %s" prop-name file-name)
    (svn-run-svn nil t 'propget-parse "propget" prop-name file-name)
    (save-excursion
      (set-buffer "*svn-process*")
      (setq prop-value (if (> (point-max) 1)
                           (buffer-substring (point-min) (- (point-max) 1))
                         "")))
    (setq svn-status-propedit-property-name prop-name)
    (setq svn-status-propedit-file-list file-info-list)
    (setq svn-status-pre-propedit-window-configuration (current-window-configuration))
    (pop-to-buffer commit-buffer)
    (delete-region (point-min) (point-max))
    (setq default-directory dir)
    (insert prop-value)
    (svn-status-remove-control-M)
    (when new-prop-value
      (when (listp new-prop-value)
        (message "Adding new prop values %S " new-prop-value)
        (while new-prop-value
          (goto-char (point-min))
          (unless (re-search-forward
                   (concat "^" (regexp-quote (car new-prop-value)) "$") nil t)
            (goto-char (point-max))
            (when (> (current-column) 0) (insert "\n"))
            (insert (car new-prop-value)))
          (setq new-prop-value (cdr new-prop-value)))))
    (svn-prop-edit-mode)))

(defun svn-status-property-set-property (file-info-list prop-name prop-value)
  "Set a property on a given file list."
  (save-excursion
    (set-buffer (get-buffer "*svn-property-edit*"))
    (delete-region (point-min) (point-max))
    (insert prop-value))
  (setq svn-status-propedit-file-list (svn-status-marked-files))
  (setq svn-status-propedit-property-name prop-name)
  (svn-prop-edit-do-it nil)
  (svn-status-update))


(defun svn-status-get-directory (line-info)
  (let* ((file-name (svn-status-line-info->filename line-info))
         (file-dir (file-name-directory file-name)))
    ;;(message "file-dir: %S" file-dir)
    (if file-dir
        (substring file-dir 0 (- (length file-dir) 1))
      ".")))

(defun svn-status-get-file-list-per-directory (files)
  ;;(message "%S" files)
  (let ((dir-list nil)
        (i files)
        (j)
        (dir))
    (while i
      (setq dir (svn-status-get-directory (car i)))
      (setq j (assoc dir dir-list))
      (if j
          (progn
            ;;(message "dir already present %S %s" j dir)
            (setcdr j (append (cdr j) (list (car i)))))
        (setq dir-list (append dir-list (list (list dir (car i))))))
      (setq i (cdr i)))
    ;;(message "svn-status-get-file-list-per-directory: %S" dir-list)
    dir-list))

(defun svn-status-property-ignore-file ()
  (interactive)
  (let ((d-list (svn-status-get-file-list-per-directory (svn-status-marked-files)))
        (dir)
        (f-info)
        (ext-list))
    (while d-list
      (setq dir (caar d-list))
      (setq f-info (cdar d-list))
      (setq ext-list (mapcar '(lambda (i)
                                (svn-status-line-info->filename-nondirectory i)) f-info))
      ;;(message "ignore in dir %s: %S" dir f-info)
      (save-window-excursion
        (when (y-or-n-p (format "Ignore %S for %s? " ext-list dir))
          (svn-status-property-edit
           (list (svn-status-find-info-for-file-name dir)) "svn:ignore" ext-list)
          (svn-prop-edit-do-it nil)))   ; synchronous
      (setq d-list (cdr d-list)))
    (svn-status-update)))

(defun svn-status-property-ignore-file-extension ()
  (interactive)
  (let ((d-list (svn-status-get-file-list-per-directory (svn-status-marked-files)))
        (dir)
        (f-info)
        (ext-list))
    (while d-list
      (setq dir (caar d-list))
      (setq f-info (cdar d-list))
      ;;(message "ignore in dir %s: %S" dir f-info)
      (setq ext-list nil)
      (while f-info
        (add-to-list 'ext-list (concat "*."
                                       (file-name-extension
                                        (svn-status-line-info->filename (car f-info)))))
        (setq f-info (cdr f-info)))
      ;;(message "%S" ext-list)
      (save-window-excursion
        (when (y-or-n-p (format "Ignore %S for %s? " ext-list dir))
          (svn-status-property-edit
           (list (svn-status-find-info-for-file-name dir)) "svn:ignore"
           ext-list)
          (svn-prop-edit-do-it nil)))
      (setq d-list (cdr d-list)))
    (svn-status-update)))

(defun svn-status-property-edit-svn-ignore ()
  (interactive)
  (let* ((line-info (svn-status-get-line-information))
         (dir (if (svn-status-line-info->directory-p line-info)
                  (svn-status-line-info->filename line-info)
                (svn-status-get-directory line-info))))
    (svn-status-property-edit
     (list (svn-status-find-info-for-file-name dir)) "svn:ignore")
    (message "Edit svn:ignore on %s" dir)))


(defun svn-status-property-set-keyword-list ()
  "Edit the svn:keywords property on the marked files."
  (interactive)
  ;;(message "Set svn:keywords for %S" (svn-status-marked-file-names))
  (svn-status-property-edit (svn-status-marked-files) "svn:keywords"))

(defun svn-status-property-set-eol-style ()
  "Edit the svn:eol-style property on the marked files."
  (interactive)
  (svn-status-property-set-property
   (svn-status-marked-files) "svn:eol-style"
   (completing-read "Set svn:eol-style for the marked files: "
                    (mapcar 'list '("native" "CRLF" "LF" "CR"))
                    nil t)))

;; --------------------------------------------------------------------------------
;; svn-prop-edit-mode:
;; --------------------------------------------------------------------------------

(defvar svn-prop-edit-mode-map () "Keymap used in `svn-prop-edit-mode' buffers.")

(when (not svn-prop-edit-mode-map)
  (setq svn-prop-edit-mode-map (make-sparse-keymap))
  (define-key svn-prop-edit-mode-map [(control ?c) (control ?c)] 'svn-prop-edit-done)
  (define-key svn-prop-edit-mode-map [(control ?c) (control ?d)] 'svn-prop-edit-svn-diff)
  (define-key svn-prop-edit-mode-map [(control ?c) (control ?s)] 'svn-prop-edit-svn-status)
  (define-key svn-prop-edit-mode-map [(control ?c) (control ?l)] 'svn-prop-edit-svn-log)
  (define-key svn-prop-edit-mode-map [(control ?c) (control ?q)] 'svn-prop-edit-abort))

(easy-menu-define svn-prop-edit-mode-menu svn-prop-edit-mode-map
"'svn-prop-edit-mode' menu"
                  '("SVN-PropEdit"
                    ["Commit" svn-prop-edit-done t]
                    ["Show Diff" svn-prop-edit-svn-diff t]
                    ["Show Status" svn-prop-edit-svn-status t]
                    ["Show Log" svn-prop-edit-svn-log t]
                    ["Abort" svn-prop-edit-abort t]))

(defun svn-prop-edit-mode ()
  "Major Mode to edit file properties of files under svn control.
Commands:
\\{svn-prop-edit-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map svn-prop-edit-mode-map)
  (easy-menu-add svn-prop-edit-mode-menu)
  (setq major-mode 'svn-prop-edit-mode)
  (setq mode-name "svn-prop-edit"))

(defun svn-prop-edit-abort ()
  (interactive)
  (bury-buffer)
  (set-window-configuration svn-status-pre-propedit-window-configuration))

(defun svn-prop-edit-done ()
  (interactive)
  (svn-prop-edit-do-it t))

(defun svn-prop-edit-do-it (async)
  (message "svn propset %s on %s"
           svn-status-propedit-property-name
           (mapcar 'svn-status-line-info->filename svn-status-propedit-file-list))
  (save-excursion
    (set-buffer (get-buffer "*svn-property-edit*"))
    (set-buffer-file-coding-system 'undecided-unix nil)
    (setq svn-status-temp-file-to-remove
          (concat svn-status-temp-dir "svn-prop-edit.txt" svn-temp-suffix))
    (write-region (point-min) (point-max) svn-status-temp-file-to-remove nil 1))
  (when svn-status-propedit-file-list ; there are files to change properties
    (svn-status-create-arg-file svn-status-temp-arg-file ""
                                svn-status-propedit-file-list "")
    (setq svn-status-propedit-file-list nil)
    (svn-run-svn async t 'propset "propset"
         svn-status-propedit-property-name
                 "--targets" svn-status-temp-arg-file
                 "-F" (concat svn-status-temp-dir "svn-prop-edit.txt" svn-temp-suffix))
    (unless async (svn-status-remove-temp-file-maybe)))
  (set-window-configuration svn-status-pre-propedit-window-configuration))

(defun svn-prop-edit-svn-diff (arg)
  (interactive "P")
  (set-buffer "*svn-status*")
  (svn-status-show-svn-diff-for-marked-files arg))

(defun svn-prop-edit-svn-log (arg)
  (interactive "P")
  (set-buffer "*svn-status*")
  (svn-status-show-svn-log arg))

(defun svn-prop-edit-svn-status ()
  (interactive)
  (pop-to-buffer "*svn-status*")
  (other-window 1))

;; --------------------------------------------------------------------------------
;; svn-log-edit-mode:
;; --------------------------------------------------------------------------------

(require 'log-edit)

(defun svn-log-edit ()
  (let ((parent (current-buffer)))
    (svn-log-edit-mode)
    (set (make-local-variable 'log-edit-callback) 'svn-log-edit-done)
    (set (make-local-variable 'log-edit-listfun) 'svn-log-edit-files-to-commit)
    (set (make-local-variable 'log-edit-initial-files) (log-edit-files))
    (goto-char (point-min)) (push-mark (point-max))
    (message (substitute-command-keys
	      "Press \\[log-edit-done] when you are done editing."))))

(define-derived-mode svn-log-edit-mode log-edit-mode "Svn-Log-Edit"
  "Wrapper around `log-edit-mode'"
  (easy-menu-add svn-log-edit-mode-menu)
  (setq svn-log-edit-update-log-entry nil))

(define-key svn-log-edit-mode-map (kbd "C-c C-d") 'svn-log-edit-svn-diff)
(define-key svn-log-edit-mode-map (kbd "C-c C-s") 'svn-log-edit-save-message)
(define-key svn-log-edit-mode-map (kbd "C-c C-i") 'svn-log-edit-svn-status)
(define-key svn-log-edit-mode-map (kbd "C-c C-l") 'svn-log-edit-svn-log)
(define-key svn-log-edit-mode-map (kbd "C-c C-z") 'svn-log-edit-erase-edit-buffer)
(define-key svn-log-edit-mode-map (kbd "C-c C-q") 'svn-log-edit-abort)

(easy-menu-define svn-log-edit-mode-menu svn-log-edit-mode-map
  "'svn-log-edit-mode' menu"
  '("SVN-Log"
    ["Save to disk" svn-log-edit-save-message t]
    ["Show Diff" svn-log-edit-svn-diff t]
    ["Show Status" svn-log-edit-svn-status t]
    ["Show Log" svn-log-edit-svn-log t]
    ["Show files to commit" log-edit-show-files t]
    ["Erase buffer" svn-log-edit-erase-edit-buffer]
    ["Abort" svn-log-edit-abort t]))

(defun svn-log-edit-abort ()
  (interactive)
  (bury-buffer)
  (set-window-configuration svn-status-pre-commit-window-configuration))

(defun svn-log-edit-done ()
  (interactive)
  (message "svn-log editing done")
  (save-excursion
    (set-buffer (get-buffer "*svn-log-edit*"))
    (when svn-log-edit-insert-files-to-commit
      (svn-log-edit-remove-comment-lines))
    (set-buffer-file-coding-system 'undecided-unix nil)
    (when (or svn-log-edit-update-log-entry svn-status-files-to-commit)
      (setq svn-status-temp-file-to-remove
            (concat svn-status-temp-dir "svn-log-edit.txt" svn-temp-suffix))
      (write-region (point-min) (point-max) svn-status-temp-file-to-remove nil 1)))
  (if svn-log-edit-update-log-entry
      (when (y-or-n-p "Update the log entry? ")
        ;;   svn propset svn:log --revprop -r11672 -F file
        (svn-run-svn nil t 'propset "propset" "svn:log" "--revprop"
                     (concat "-r" svn-log-edit-update-log-entry)
                     "-F" svn-status-temp-file-to-remove)
        (save-excursion
          (set-buffer "*svn-process*")
          (message (buffer-substring (point-min) (- (point-max) 1)))))
    (when svn-status-files-to-commit ; there are files to commit
      (setq svn-status-operated-on-dot
            (and (= 1 (length svn-status-files-to-commit))
                 (string= "." (svn-status-line-info->filename (car svn-status-files-to-commit)))))
      (svn-status-create-arg-file svn-status-temp-arg-file ""
                                  svn-status-files-to-commit "")
      (setq svn-status-files-to-commit nil)
      (svn-run-svn t t 'commit "commit" "--targets" svn-status-temp-arg-file
                   "-F" svn-status-temp-file-to-remove)))
    (set-window-configuration svn-status-pre-commit-window-configuration))

(defun svn-log-edit-svn-diff (arg)
  "Show the diff we are about to commit.
If ARG then show diff between some other version of the selected files."
  (interactive "P")
  (set-buffer "*svn-status*")
  (svn-status-show-svn-diff-for-marked-files arg))

(defun svn-log-edit-svn-log (arg)
  (interactive "P")
  (set-buffer "*svn-status*")
  (svn-status-show-svn-log arg))

(defun svn-log-edit-svn-status ()
  (interactive)
  (pop-to-buffer "*svn-status*")
  (other-window 1))

(defun svn-log-edit-files-to-commit ()
  (mapcar 'svn-status-line-info->filename svn-status-files-to-commit))

(defun svn-log-edit-show-files-to-commit ()
  (interactive)
  (message "Files to commit: %S" (svn-log-edit-files-to-commit)))

(defun svn-log-edit-save-message ()
  "Save the current log message to the file `svn-log-edit-file-name'."
  (interactive)
  (write-region (point-min) (point-max) svn-log-edit-file-name))

(defun svn-log-edit-erase-edit-buffer ()
  "Delete everything in the *svn-log-edit* buffer."
  (interactive)
  (set-buffer "*svn-log-edit*")
  (erase-buffer))

(defun svn-log-edit-insert-files-to-commit ()
  (interactive)
  (svn-log-edit-remove-comment-lines)
  (let ((buf-size (- (point-max) (point-min))))
    (save-excursion
      (goto-char (point-min))
      (insert "## Lines starting with '## ' will be removed from the log message.\n")
      (insert "## File(s) to commit:\n")
      (let ((file-list svn-status-files-to-commit))
        (while file-list
          (insert (concat "## " (svn-status-line-info->filename (car file-list)) "\n"))
          (setq file-list (cdr file-list)))))
    (when (= 0 buf-size)
      (goto-char (point-max)))))

(defun svn-log-edit-remove-comment-lines ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^## .*")))


;; --------------------------------------------------------------------------------
;; svn-log-view-mode:
;; --------------------------------------------------------------------------------

(defvar svn-log-view-mode-map () "Keymap used in `svn-log-view-mode' buffers.")

(when (not svn-log-view-mode-map)
  (setq svn-log-view-mode-map (make-sparse-keymap))
  (define-key svn-log-view-mode-map (kbd "p") 'svn-log-view-prev)
  (define-key svn-log-view-mode-map (kbd "n") 'svn-log-view-next)
  (define-key svn-log-view-mode-map (kbd "=") 'svn-log-view-diff)
  (define-key svn-log-view-mode-map (kbd "e") 'svn-log-edit-log-entry)
  (define-key svn-log-view-mode-map (kbd "q") 'bury-buffer))

(easy-menu-define svn-log-view-mode-menu svn-log-view-mode-map
"'svn-log-view-mode' menu"
                  '("SVN-LogView"
                    ["Show Changeset" svn-log-view-diff t]
                    ["Edit log message" svn-log-edit-log-entry t]))

(defvar svn-log-view-font-lock-keywords
  '(("^r.+" . font-lock-keyword-face)
  "Keywords in svn-log-view-mode."))

(define-derived-mode svn-log-view-mode log-view-mode "svn-log-view"
  "Major Mode to show the output from svn log.
Commands:
\\{svn-log-view-mode-map}
"
  (use-local-map svn-log-view-mode-map)
  (easy-menu-add svn-log-view-mode-menu)
  (set (make-local-variable 'font-lock-defaults) '(svn-log-view-font-lock-keywords t)))

(defun svn-log-view-next ()
  (interactive)
  (when (re-search-forward "^r[0-9]+" nil t)
    (beginning-of-line 3)))

(defun svn-log-view-prev ()
  (interactive)
  (when (re-search-backward "^r[0-9]+" nil t 2)
    (beginning-of-line 3)))

(defun svn-log-revision-at-point ()
  (save-excursion
    (re-search-backward "^r\\([0-9]+\\)")
    (match-string-no-properties 1)))

(defun svn-log-view-diff (arg)
  "Show the changeset for a given log entry.
When called with a prefix argument, ask the user for the revision."
  (interactive "P")
  (let* ((upper-rev (svn-log-revision-at-point))
        (lower-rev (number-to-string (- (string-to-number upper-rev) 1)))
        (rev-arg (concat lower-rev ":" upper-rev)))
    (when arg
      (setq rev-arg (read-string "Revision for changeset: " rev-arg)))
    (svn-run-svn nil t 'diff "diff" (concat "-r" rev-arg))
    (svn-status-show-process-buffer-internal t)
    (save-excursion
      (set-buffer "*svn-process*")
      (diff-mode)
      (font-lock-fontify-buffer))))

(defun svn-log-edit-log-entry ()
  "Edit the given log entry."
  (interactive)
  (let ((rev (svn-log-revision-at-point))
        (log-message))
    (svn-run-svn nil t 'propget-parse "propget" "--revprop" (concat "-r" rev) "svn:log")
    (save-excursion
      (set-buffer "*svn-process*")
      (setq log-message (if (> (point-max) 1)
                            (buffer-substring (point-min) (- (point-max) 1))
                          "")))
    (svn-status-pop-to-commit-buffer)
    (delete-region (point-min) (point-max))
    (insert log-message)
    (goto-char (point-min))
    (setq svn-log-edit-update-log-entry rev)))

;; --------------------------------------------------------------------------------
;; svn status persistent options
;; --------------------------------------------------------------------------------

(defun svn-status-base-dir ()
  (let ((base-dir default-directory)
        (dot-svn-dir)
        (dir-below default-directory))
    (setq dot-svn-dir (concat base-dir ".svn"))
    (while (when (file-exists-p dot-svn-dir)
             (setq base-dir (file-name-directory dot-svn-dir))
             (string-match "\\(.+/\\).+/" dir-below)
             (setq dir-below (match-string 1 dir-below))
             (setq dot-svn-dir (concat dir-below ".svn"))))
    base-dir))

(defun svn-status-save-state ()
  (interactive)
  (let ((buf (find-file (concat (svn-status-base-dir) "++psvn.state"))))
    (delete-region (point-min) (point-max))
    (setq svn-status-options
          (list
           (list "sort-status-buffer" svn-status-sort-status-buffer)
           (list "elide-list" svn-status-elided-list)))
    (insert (pp-to-string svn-status-options))
    (save-buffer)
    (kill-buffer buf)))

(defun svn-status-load-state ()
  (interactive)
  (let ((file (concat (svn-status-base-dir) "++psvn.state")))
    (if (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (setq svn-status-options (read (current-buffer)))
          (setq svn-status-sort-status-buffer
                (nth 1 (assoc "sort-status-buffer" svn-status-options)))
          (setq svn-status-elided-list
                (nth 1 (assoc "elide-list" svn-status-options)))
          (when svn-status-elided-list (svn-status-apply-elide-list)))
      (error "%s is not readable." file))
    (message "Loaded %s" file)))

(defun svn-status-toggle-sort-status-buffer ()
  "If you turn off sorting, you can speed up M-x svn-status.
However, the buffer is not correct sorted then.
This function will be removed again, when a faster parsing and
display routine for svn-status is available."
  (interactive)
  (setq svn-status-sort-status-buffer (not svn-status-sort-status-buffer))
  (message (concat "The *svn-status* buffer will be"
                   (if svn-status-sort-status-buffer "" " not")
                   " sorted.")))

;;;------------------------------------------------------------
;;; resolve conflicts using ediff
;;;------------------------------------------------------------
(defun svn-resolve-conflicts-ediff (&optional name-A name-B)
  "Invoke ediff to resolve conflicts in the current buffer.
The conflicts must be marked with rcsmerge conflict markers."
  (interactive)
  (let* ((found nil)
         (file-name (file-name-nondirectory buffer-file-name))
         (your-buffer (generate-new-buffer
                       (concat "*" file-name
                               " " (or name-A "WORKFILE") "*")))
         (other-buffer (generate-new-buffer
                        (concat "*" file-name
                                " " (or name-B "CHECKED-IN") "*")))
         (result-buffer (current-buffer)))
    (save-excursion
      (set-buffer your-buffer)
      (erase-buffer)
      (insert-buffer result-buffer)
      (goto-char (point-min))
      (while (re-search-forward "^<<<<<<< .mine\n" nil t)
        (setq found t)
        (replace-match "")
        (if (not (re-search-forward "^=======\n" nil t))
            (error "Malformed conflict marker"))
        (replace-match "")
        (let ((start (point)))
          (if (not (re-search-forward "^>>>>>>> .r[0-9]+\n" nil t))
              (error "Malformed conflict marker"))
          (delete-region start (point))))
      (if (not found)
          (progn
            (kill-buffer your-buffer)
            (kill-buffer other-buffer)
            (error "No conflict markers found")))
      (set-buffer other-buffer)
      (erase-buffer)
      (insert-buffer result-buffer)
      (goto-char (point-min))
      (while (re-search-forward "^<<<<<<< .mine\n" nil t)
        (let ((start (match-beginning 0)))
          (if (not (re-search-forward "^=======\n" nil t))
              (error "Malformed conflict marker"))
          (delete-region start (point))
          (if (not (re-search-forward "^>>>>>>> .r[0-9]+\n" nil t))
              (error "Malformed conflict marker"))
          (replace-match "")))
      (let ((config (current-window-configuration))
            (ediff-default-variant 'default-B))

        ;; Fire up ediff.

        (set-buffer (ediff-merge-buffers your-buffer other-buffer))

        ;; Ediff is now set up, and we are in the control buffer.
        ;; Do a few further adjustments and take precautions for exit.

        (make-local-variable 'svn-ediff-windows)
        (setq svn-ediff-windows config)
        (make-local-variable 'svn-ediff-result)
        (setq svn-ediff-result result-buffer)
        (make-local-variable 'ediff-quit-hook)
        (setq ediff-quit-hook
              (lambda ()
                (let ((buffer-A ediff-buffer-A)
                      (buffer-B ediff-buffer-B)
                      (buffer-C ediff-buffer-C)
                      (result svn-ediff-result)
                      (windows svn-ediff-windows))
                  (ediff-cleanup-mess)
                  (set-buffer result)
                  (erase-buffer)
                  (insert-buffer buffer-C)
                  (kill-buffer buffer-A)
                  (kill-buffer buffer-B)
                  (kill-buffer buffer-C)
                  (set-window-configuration windows)
                  (message "Conflict resolution finished; you may save the buffer"))))
        (message "Please resolve conflicts now; exit ediff when done")
        nil))))

(defun svn-resolve-conflicts (filename)
  (let ((buff (find-file-noselect filename)))
    (if buff
        (progn (switch-to-buffer buff)
               (svn-resolve-conflicts-ediff))
      (error "can not open file %s" filename))))

(defun svn-status-resolve-conflicts ()
  "Resolve conflict in the selected file"
  (interactive)
  (let ((file-info (svn-status-get-line-information)))
    (or (and file-info
             (= ?C (svn-status-line-info->filemark file-info))
             (svn-resolve-conflicts
              (svn-status-line-info->full-path file-info)))
        (error "can not resolve conflicts at this point"))))

;; --------------------------------------------------------------------------------
;; svn status profiling
;; --------------------------------------------------------------------------------
;;; Note about profiling psvn:
;;  (load-library "elp")
;;  M-x elp-reset-all
;;  (elp-instrument-package "svn-")
;;  M-x svn-status
;;  M-x elp-results

;; (defun svn-status-elp-init ()
;;   (interactive)
;;   (require 'elp)
;;   (elp-reset-all)
;;   (elp-instrument-package "svn-")
;;   (message "Run the desired svn command (e.g. M-x svn-status), then use M-x elp-results."))


(provide 'psvn-hacked)

;;; psvn.el ends here
