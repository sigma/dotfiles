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
(require 'cl)

(defmacro try (&rest code)
  "Execute `code' failing silently"
  `(condition-case nil
       ,@code
     (error nil)))

;; Use this one instead of require to ignore errors
(defun request (feature)
  "Fail to require silently"
  (try (require feature)))

;; Map a condition/action on a list
(defun mapcond (test result list &optional default)
  "Map a TEST function over LIST and return the application of
the RESULT function over the first positive answer. If DEFAULT
is not nil, then in case of no success, this value is returned"
  (loop for n in list
        if (funcall test n) return (funcall result n)
        finally return default))

(defmacro make-double-command (name args doc-string interactive
                                      first-form second-form)
  "Define a new command from 2 behaviors. First invocation runs
  the first one. An immediate second invocation runs the second
  command. Any further invocation keeps running the second
  command."
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
  "Define the same advice for several functions."
  (let ((defs (mapcar
               (lambda (f) `(defadvice ,f ,(append (list (car spec) (intern (format "ad-%s-%s-%s"
                                                                                    (symbol-name f)
                                                                                    (symbol-name (cadr spec))
                                                                                    (car spec)))) (cddr spec))
                              ,@body)) flist))) `(progn ,@defs)))

(defun add-mhook (mlist func &optional append local)
  "Add the same function to multiple hooks"
  (dolist (m mlist) (add-hook m func append local)))

(defun clear-hook (hook)
  "Clear a hook"
  (set hook nil))

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
(global-set-key [insert] 'undefined)

;; disable backups for files in /tmp or in my Mail or News directories.
(defun ecm-backup-enable-predicate (filename)
  (and (not (string= "/tmp/" (substring filename 0 5)))
       (not (string-match "/mail/" filename))
       (not (string-match "/News/" filename))))

(setq backup-enable-predicate 'ecm-backup-enable-predicate)
(setq backup-directory-alist `(("." . "~/.backups") ; centralize backup files
                               (,tramp-file-name-regexp . nil))) ; disable for tramp

(setq
 ;; don't break links
 backup-by-copying t
 ;; use numbered backups
 version-control t
 ;; keep only 20 latest versions
 kept-old-versions 0
 kept-new-versions 20
 ;; and always delete the others without confirmation
 delete-old-versions t)

;; Put autosaves files in a single directory too
(when (request 'auto-save)
  (setq auto-save-directory (expand-file-name "~/.autosaves/")))

(setq temporary-file-directory (expand-file-name "~/tmp/"))

;; Why the hell should some commands be disabled?
(setq disabled-command-function nil)

;; Why the f*** has it been removed ??
(eval-after-load 'cus-edit
  '(add-hook 'custom-mode-hook
             (lambda ()
               (define-key custom-mode-map "\^m" 'widget-button-press))))

;; scroll-margin does not work with hl-line :-(
(defvar top-margin)
(defvar bottom-margin)
(defvar buffer-no-margin-alist)

(defun check-margin ()
  (let ((window (selected-window)))
    (if (not (or (window-minibuffer-p window) (member (buffer-name (window-buffer)) buffer-no-margin-alist)))
        (let* ((position (progn (set-buffer (window-buffer)) (point)))
               (wstart (window-start))
               (wbottl (- (window-height) 4))
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
  (let ((face (cdr (memq 'face minibuffer-prompt-properties)))
        (minibuff-face (if (boundp 'minibuffer-face) minibuffer-face nil)))
    (if face
        (setcar face minibuff-face)
      (setq minibuffer-prompt-properties
            (append minibuffer-prompt-properties
                    (list 'face 'minibuff-face))))))

;; Adapt open-line behavior when arg <= 0
(defadvice open-line (around open-line-around (arg) act)
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
;; type "C-x b", then "t<TAB>" which then presents you with a completion list
;; of temp.c and temp.h, so you then must type "h<RET>".  This is annoying
;; since why would you want to switch back to the buffer you're in?!?  Using
;; this fix would remove "temp.c" from the completion lits so that when you had
;; typed "t<TAB>" the name would be completed as "temp.h" as desired.
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

;; Suppress annoying messages. Needs some work
;; (defadvice message (around message-around act)
;;   "Don't let annoying messages popup while using the minibuffer"
;;   (unless (minibuffer-window-active-p (minibuffer-window))
;;     ad-do-it))

(defun yh/collapse-home-directory (filename)
  "When possible, transform an absolute path into its ~ prefixed form"
  (if (string-match (concat "^" (regexp-quote home-directory)) filename)
      (concat "~/" (file-relative-name filename home-directory))
    filename))

;; Author: Patrick Gundlach
;; nice mark - shows mark as a highlighted 'cursor' so user 'always'
;; sees where the mark is. Especially nice for killing a region.

;; (defvar pg-mark-overlay nil
;;   "Overlay to show the position where the mark is")
;; (make-variable-buffer-local 'pg-mark-overlay)

;; (put 'pg-mark-mark 'face 'secondary-selection)

;; (defvar pg-mark-old-position nil
;;   "The position the mark was at. To be able to compare with the
;; current position")

;; (defun pg-show-mark ()
;;   "Display an overlay where the mark is at. Should be hooked into
;; activate-mark-hook"
;;   (unless pg-mark-overlay
;;     (setq pg-mark-overlay (make-overlay 0 0))
;;     (overlay-put pg-mark-overlay 'category 'pg-mark-mark))
;;   (let ((here (mark t)))
;;     (when here
;;       (move-overlay pg-mark-overlay here (1+ here)))))

;; (defadvice  exchange-point-and-mark (after pg-mark-exchange-point-and-mark)
;;   "Show visual marker"
;;   (pg-show-mark))

;; (ad-activate 'exchange-point-and-mark)
;; (add-hook 'activate-mark-hook 'pg-show-mark)

;; Use better names than plop<1> and plop<2> for files with same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-strip-common-suffix nil)

;; Provide modes for common config files
(require 'generic-x)

;; (mouse-sel-mode 1)

;;; default function doesn't honour yank-excluded-properties
;; (eval-after-load 'mouse-sel
;;   '(defadvice mouse-insert-selection-internal (around mouse-insert-selection-internal-yank act)
;;      (flet ((insert (str) (insert-for-yank str)))
;;        ad-do-it)))

;; If the *scratch* buffer is killed, recreate it automatically
;; FROM: Morten Welind
;;http://www.geocrawler.com/archives/3/338/1994/6/0/1877802/
(defun prepare-scratch-for-kill ()
  (save-excursion
    (set-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode)
    (make-local-variable 'kill-buffer-query-functions)
    (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)))

(defun kill-scratch-buffer ()
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (prepare-scratch-for-kill)
  ;; Since we killed it, don't let caller do that.
  nil)

(prepare-scratch-for-kill)

(provide 'patches)
;;; patches.el ends here
