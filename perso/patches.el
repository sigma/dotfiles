;;; patches.el --- various patches for emacs

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Yann Hodique <Yann.Hodique@lifl.fr>
;; Keywords: lisp, extensions

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

;; Some patches I cannot live without. Fix bad default behaviors, and provide
;; missing basic features. Some code is by me, some is hacked by me, and some
;; has nothing to do with me :-)

;;; Code:

;; Use this one instead of require to ignore errors
(defun request (feature)
  "Fail to require silently"
  (condition-case nil
    (require feature)
  (error nil)))

;; Map a condition/action on a list
(defun mapcond (test result list &optional default)
  (or (let ((ex t))
        (while (and ex list)
          (if (funcall test (car list))
              (progn
                (setq ex nil)
                (funcall result (car list)))
            (setq list (cdr list)))))
      default))

(defmacro make-double-command (name args doc-string interactive
                                      first-form second-form)
  "define a new command from 2 behaviors"
  (declare (indent 2))
  (let ((int-form (if (not interactive)
                      '(interactive)
                    (list 'interactive interactive))))
    `(progn
       (defun ,name ,args ,doc-string
         ,int-form
         (if (eq last-command this-command)
             ,(if (and (listp second-form) (> (length second-form) 1))
                  (cons 'progn second-form)
                second-form)
           ,first-form)))))

;; Many thanks to utis (Oliver Scholz)
(defmacro defmadvice (flist spec &rest body)
  (let ((defs (mapcar
               (lambda (f) `(defadvice ,f ,(append (list (car spec) (intern (format "ad-%s-%s-%s"
                                                                                    (symbol-name f)
                                                                                    (symbol-name (cadr spec))
                                                                                    (car spec)))) (cddr spec))
                              ,@body)) flist))) `(progn ,@defs)))

;; add the same function to multiple hooks
(defun add-mhook (mlist func) (dolist (m mlist) (add-hook m func)))

;; clear a hook
(defun clear-hook (hook) (set hook nil))

;; suppress annoying messages from speedbar package
(defmadvice (dframe-handle-make-frame-visible dframe-handle-iconify-frame dframe-handle-delete-frame)
  (around dframe act)
  "Inhibit message function"
  (flet ((message (&rest args) nil))
    ad-do-it))

;; fix split-window behavior
(defmadvice (split-window-vertically split-window-horizontally)
  (after split act)
  "Open another buffer in the new window"
  (set-window-buffer (next-window) (other-buffer)))

;; make the y or n suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

;; Goto matching parenthesis
(defun match-paren ()
  "Will bounce between matching parens just like % in vi"
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
	(next-char (char-to-string (following-char))))
	(cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
		  ((string-match "[\]})>]" prev-char) (backward-sexp 1))
		  (t (error "%s" "Not on a paren, brace, or bracket")))))

(global-set-key [(control =)] 'match-paren)

;; ...never switch to overwrite mode, not even accidentally
(global-set-key [insert]
  (function
   (lambda () (interactive)
     (message "Sorry, overwrite mode has been disabled forever."))))

;; disable backups for files in /tmp or in my Mail or News directories.
(defun ecm-backup-enable-predicate (filename)
  (and (not (string= "/tmp/" (substring filename 0 5)))
       (not (string-match "/mail/" filename))
       (not (string-match "/News/" filename))))

(setq backup-enable-predicate 'ecm-backup-enable-predicate)
(setq backup-directory-alist '(("." . "~/.backups")))
(setq backup-by-copying t)

;; Put autosaves files in a single directory too
(when (request 'auto-save)
  (setq auto-save-directory (expand-file-name "~/.autosaves/")))

;; Why the hell should some commands be disabled?
(setq disabled-command-function nil)

;; scroll-margin does not work with hl-line :-(
(defvar top-margin)
(defvar bottom-margin)
(defvar buffer-no-margin-alist)

(defun check-margin ()
  (let ((window (selected-window)))
    (if (not (or (window-minibuffer-p window) (member (buffer-name (window-buffer)) buffer-no-margin-alist)))
        (let* ((position (progn (set-buffer (window-buffer)) (point)))
               (wstart (window-start))
               (wbottl (- (window-height) 2))
               (topshift (- (max 0 (min top-margin (- wbottl 1)))))
               (diff (- (min topshift (- bottom-margin wbottl)) topshift)))
          (vertical-motion topshift)
          (if (cond
               ((> wstart (point)))
               ((= wstart (point)) nil)
               ((or (= diff 0)
                    (save-restriction
                      (narrow-to-region wstart (point-max))
                      (vertical-motion diff)
                      (< wstart (point))))))
              (set-window-start window (point)))
          (goto-char position)))))

(defun set-scroll-margin (up down except)
  "Enable hacked scroll margin"
  (setq top-margin up
        bottom-margin down
        buffer-no-margin-alist except)
  (add-hook 'post-command-hook 'check-margin))

;; Scroll up then down should go back to the start point
(setq scroll-preserve-screen-position t)

;; Color prefix in minibuffer
(unless (facep 'minibuffer-prompt)
  (let ((face (cdr (memq 'face minibuffer-prompt-properties))))
    (if face
        (setcar face minibuffer-face)
      (setq minibuffer-prompt-properties
            (append minibuffer-prompt-properties
                    (list 'face 'minibuffer-face))))))

;; Adapt open-line behavior when arg <= 0
(defadvice open-line( around open-line-around (arg) act )
  "Open new line(s) at end of line if arg is <= 0."
  (if (<= arg 0)
      (let ((var (if (equal arg 0) -1 arg)))
        (save-excursion
          (end-of-line)
          (open-line (- var))))
    ad-do-it))

;; BUFFER SWITCHING FIX
;;
;; This changes the behaviour of the switch-to-buffer completion functions so
;; that the current buffer is NOT in the completion list.
;;
;; i.e. say you're working in "temp.c", and you want to visit "temp.h"; so you
;; type "C-xb", then "t<TAB>" which then presents you with a completion list of
;; temp.c and temp.h, so you then must type "h<RET>".  This is annoying since
;; why would you want to switch back to the buffer you're in?!?
;; Using this fix would remove "temp.c" from the completion lits so that when
;; you had typed "t<TAB>" the name would be completed as "temp.h" as desired.
;;
;; Steve Dodd
;; March 1998
;; Re-hacked and dramatically packed by Yann Hodique (2004)

(defvar yh/remove-first-completion nil)

(defun yh/clean-minibuffer-completion-table ()
  "Suppress current buffer from completion list if needed"
  (when yh/remove-first-completion
    (progn (setq yh/remove-first-completion nil)
           (when (consp minibuffer-completion-table)
             (setq  minibuffer-completion-table
                    (cdr minibuffer-completion-table))))))

(defmadvice (minibuffer-complete minibuffer-complete-word minibuffer-complete-and-exit)
  (before complete act)
  "Suppress current buffer from completion list if needed"
  (yh/clean-minibuffer-completion-table))

(defadvice switch-to-buffer (before ad-switch-to-buffer-before act)
  "Activate first entry removal, in order to avoid completing the current buffer name"
  (setq yh/remove-first-completion t))

(defun yank-rpop (arg)
  (interactive "*p")
  (yank-pop (- arg)))
(global-set-key "\M-Y" 'yank-rpop)

(defun yh/completion-setup-function ()
  (if minibuffer-completing-file-name
      (file-completion-setup-function)
    (completion-setup-function)))

;; The default completion-setup-function conflicts with my dircolors settings
(defun file-completion-setup-function ()
  (let ((mainbuf (current-buffer))
	(mbuf-contents (minibuffer-contents)))
    (with-current-buffer mainbuf
      (setq default-directory (file-name-directory mbuf-contents)))
    (when (and partial-completion-mode (not (eobp)))
      (setq mbuf-contents
	    (substring mbuf-contents 0 (- (point) (point-max)))))
    (with-current-buffer standard-output
      (completion-list-mode)
      (make-local-variable 'completion-reference-buffer)
      (setq completion-reference-buffer mainbuf)
      ;; Insert help string.
      (goto-char (point-min))
      (if (display-mouse-p)
	  (insert (substitute-command-keys
		   "Click \\[mouse-choose-completion] on a completion to select it.\n")))
      (insert (substitute-command-keys
	       "In this buffer, type \\[choose-completion] to \
select the completion near point.\n\n")))))

(remove-hook 'completion-setup-hook 'completion-setup-function)
(add-hook 'completion-setup-hook 'yh/completion-setup-function)

;; Suppress annoying messages. Needs some work
;; (defadvice message (around message-around act)
;;   "Don't let annoying messages popup while using the minibuffer"
;;   (unless (minibuffer-window-active-p (minibuffer-window))
;;     ad-do-it))

;; Author: Patrick Gundlach
;; nice mark - shows mark as a highlighted 'cursor' so user 'always'
;; sees where the mark is. Especially nice for killing a region.

(defvar pg-mark-overlay nil
  "Overlay to show the position where the mark is")
(make-variable-buffer-local 'pg-mark-overlay)

(put 'pg-mark-mark 'face 'secondary-selection)

(defvar pg-mark-old-position nil
  "The position the mark was at. To be able to compare with the
current position")

(defun pg-show-mark ()
  "Display an overlay where the mark is at. Should be hooked into
activate-mark-hook"
  (unless pg-mark-overlay
    (setq pg-mark-overlay (make-overlay 0 0))
    (overlay-put pg-mark-overlay 'category 'pg-mark-mark))
  (let ((here (mark t)))
    (when here
      (move-overlay pg-mark-overlay here (1+ here)))))

(defadvice  exchange-point-and-mark (after pg-mark-exchange-point-and-mark)
  "Show visual marker"
  (pg-show-mark))

(ad-activate 'exchange-point-and-mark)
(add-hook 'activate-mark-hook 'pg-show-mark)

(provide 'patches)
;;; patches.el ends here
