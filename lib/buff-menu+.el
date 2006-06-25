;;; buff-menu-plus.el --- Extensions to `buff-menu.el'.  New bindings.
;;; FILE NAME SHOULD BE buff-menu+.el, but EmacsWiki doesn't like that.
;; 
;; Emacs Lisp Archive Entry
;; Filename: buff-menu+.el
;; Description: Extensions to `buff-menu.el'
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2004, Drew Adams, all rights reserved.
;; Created: Mon Sep 11 10:29:56 1995
;; Version: $Id$
;; Last-Updated: Wed Jul 21 15:01:33 2004
;;           By: dradams
;;     Update #: 741
;; Keywords: mouse, local
;; Compatibility: GNU Emacs 20.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;;    Extensions to `buff-menu.el'.  New bindings & fonts & menu.
;;
;;  `Buffer-menu-mouse-3-menu' popup menu added.
;;  New prefix arg options for `buffer-menu'.
;;
;;  Main new functions defined here:
;;
;;    `Buffer-menu-mouse-3-menu', `Buffer-menu-mouse-delete',
;;    `Buffer-menu-mouse-execute', `Buffer-menu-mouse-modified',
;;    `Buffer-menu-mouse-other-window', `Buffer-menu-mouse-save',
;;    `Buffer-menu-mouse-unmark'.
;;
;;
;;  ***** NOTE: The following functions defined in `buff-menu.el'
;;              have been REDEFINED HERE:
;;
;;  `buffer-menu' -
;;     1. Different help message.
;;     2. Prefix ARG =< 0 now means list (all) buffers alphabetically.
;;        (It used to mean the same as ARG > 0.)
;;        Prefix ARG >= 0 means list just file buffers.
;;     3. Call `font-lock-fontify-buffer' at end.
;;  `Buffer-menu-execute' - Deletes windows (frame) when kills buffer.
;;  `Buffer-menu-mode' -
;;     1. Doc string reflects new bindings.
;;     2. mouse-face on whole line, not just buffer name.
;;  `Buffer-menu-select' - When Buffer Menu is `window-dedicated-p',
;;                         uses `pop-to-buffer' to display.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `buff-menu.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "buff-menu" '(require 'buff-menu+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 2004/07/21 dadams
;;     Buffer-menu-mode: Don't set Buffer-menu-buffer-column unless < Emacs 20.
;; 2001/01/02 dadams
;;     Protect undefine-killer-commands via fboundp.
;; 1999/08/26 dadams
;;     1. Added: buffer-menu-*-face's, buffer-menu-font-lock-keywords.
;;     2. Add buffer-menu-font-lock-keywords to buffer-menu-mode-hook.
;; 1997/03/21 dadams
;;     Buffer-menu-execute, Buffer-menu-mouse-execute:
;;       Only use kill-buffer-and-its-windows if fboundp.
;; 1996/07/01 dadams
;;     buffer-menu: Prefix arg =< 0 sorts alphabetically now.
;; 1996/07/01 dadams
;;     Added redefinition of Buffer-menu-select.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/01/25 dadams
;;     1. kill-buffer -> kill-buffer-and-its-windows.
;;     2. Buffer-menu-mode: Put mouse-face on whole buffer line.
;; 1996/01/12 dadams
;;     Redefined buffer-menu.
;; 1996/01/09 dadams
;;     kill-buffer -> kill-buffer-delete-frames
;; 1995/12/28 dadams
;;     Buffer-menu-mouse-3-menu: Corrected by adding temp local var.
;; 1995/12/14 dadams
;;     1. Highlight buffer line when mouse-3 menu displayed.
;;        Added Buffer-menu-overlay.
;;     2. mouse-3 menu is reduced to non-buffer-specifics when not on a buffer line.
;; 1995/12/13 dadams
;;     Added Buffer-menu-mouse-3-menu.  Use it instead of Buffer-menu-mouse-3-map.
;; 1995/12/13 dadams
;;     1) Put back Buffer-menu-select, in place of Buffer-menu-mouse-other-window.
;;     2) Added menu on mouse-3: Added: Buffer-menu-mouse-3-map,
;;        Buffer-menu-mouse-execute, Buffer-menu-mouse-modified,
;;        Buffer-menu-mouse-delete, Buffer-menu-mouse-save,
;;        Buffer-menu-mouse-unmark.
;; 1995/09/11 dadams
;;     Buffer-menu-mode: Added bindings list to doc string.
;; 1995/09/11 dadams
;;     Redefined Buffer-menu-execute: deletes frame w/ kill.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code: 

 ;; Cannot do (require 'buff-menu), because `buff-menu.el' does no `provide'.
 ;; Don't want to do a (load-library "buff-menu") either, because it wouldn't
 ;; allow doing (eval-after-load "buff-menu" '(progn (require 'buff-menu+)))

(require 'cl) ;; push, pop, unless

;; Get macro `define-face-const' when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
(eval-when-compile (require 'def-face-const))

(require 'misc-fns nil t) ;; (no error if not found): undefine-killer-commands
(require 'misc-cmds nil t) ;; (no error if not found): kill-buffer-and-its-windows


;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Undefine some bindings that would try to modify a buffer-menu buffer.
;;; Their key sequences will then appear to the user as available for
;;; local (Buffer Menu) definition.
(when (fboundp 'undefine-killer-commands)
  (undefine-killer-commands Buffer-menu-mode-map (current-global-map)))

;;; Faces used to fontify buffer.
(unless (boundp 'orange-on-darkgreen-face)
  (define-face-const "Orange" "DarkGreen"))
(defvar buffer-menu-headings-face orange-on-darkgreen-face
  "Face used for headings in *Buffer List* buffer.")
(unless (boundp 'red-on-aquamarine-face)
  (define-face-const "Red" "Aquamarine"))
(defvar buffer-menu-current-buffer-face red-on-aquamarine-face
  "Face used for current buffer mark in *Buffer List* buffer.")
(unless (boundp 'red-on-aquamarine-face)
  (define-face-const "Red" "Aquamarine"))
(defvar buffer-menu-view-mark-face red-on-aquamarine-face
  "Face used for buffers to view mark (>) in *Buffer List* buffer.")
(unless (boundp 'aquamarine-on-red-face)
  (define-face-const "Aquamarine" "Red"))
(defvar buffer-menu-delete-mark-face aquamarine-on-red-face
  "Face used for buffers to delete mark (D) in *Buffer List* buffer.")
(unless (boundp 'orange-on-blue-face)
  (define-face-const "Orange" "Blue"))
(defvar buffer-menu-save-mark-face orange-on-blue-face
  "Face used for buffers to save mark (S) in *Buffer List* buffer.")
(unless (boundp 'darkorange-foreground-face)
  (define-face-const "DarkOrange" nil))
(defvar buffer-menu-modified-mark-face darkorange-foreground-face
  "Face used for modified buffers mark (*) in *Buffer List* buffer.")
(unless (boundp 'yellow-foreground-face)
  (define-face-const "Yellow" nil))
(defvar buffer-menu-read-only-mark-face yellow-foreground-face
  "Face used for read-only buffers mark (%) in *Buffer List* buffer.")
(unless (boundp 'blue-foreground-face)
  (define-face-const "Blue" nil))
(defvar buffer-menu-buffer-name-face blue-foreground-face
  "Face used for buffer names in *Buffer List* buffer.")
(unless (boundp 'darkgreen-foreground-face)
  (define-face-const "DarkGreen" nil))
(defvar buffer-menu-mode-face darkgreen-foreground-face
  "Face used for buffer modes in *Buffer List* buffer.")
(unless (boundp 'darkred-foreground-face)
  (define-face-const "DarkRed" nil))
(defvar buffer-menu-size-face darkred-foreground-face
  "Face used for buffer sizes in *Buffer List* buffer.")
(unless (boundp 'darkmagenta-foreground-face)
  (define-face-const "DarkMagenta" nil))
(defvar buffer-menu-file-name-face darkmagenta-foreground-face
  "Face used for file names in *Buffer List* buffer.")


;;;###autoload
(defvar buffer-menu-font-lock-keywords
  '(
    ("^\\( M.*\\)" 1 buffer-menu-headings-face) ; Headings
    ("^\\([.]\\)" 1 buffer-menu-current-buffer-face) ; Current buffer mark (.)
    ("^\\(>\\)" 1 buffer-menu-view-mark-face) ; To view mark (>)
    ("^\\(D\\)" 1 buffer-menu-delete-mark-face) ; Deletion flag (D)
    ("^.\\(S\\)" 1 buffer-menu-save-mark-face) ; Save flag (S)
    ("^.\\([*]\\)" 1 buffer-menu-modified-mark-face) ; Buffer-modified-p (*)
    ("^..\\(%\\)" 1 buffer-menu-read-only-mark-face) ; Read-only-p (%)
    ("^....\\(.+\\)[ \t\n][0-9]" 1 buffer-menu-buffer-name-face) ; Buffer name
    ("^.*[ \t][0-9]+[ \t]+\\([^/\n]+\\)" 1 buffer-menu-mode-face) ; Mode
    ("^.*[ \t]\\([0-9]+\\)[ \t]+[^/\n]+" 1 buffer-menu-size-face) ; Size
    ("\\(/.*\\)$" 1 buffer-menu-file-name-face) ; File name
    ) "Expressions to highlight in Buffer Menu mode.")

;; Fontify by default.
(add-hook 'buffer-menu-mode-hook
          '(lambda ()
             (make-local-variable 'font-lock-defaults)
             (setq font-lock-defaults '(buffer-menu-font-lock-keywords t))))


;; REPLACES ORIGINAL in `buff-menu.el':
;;   1. Different help message.
;;   2. Prefix ARG =< 0 now means list all buffers alphabetically.
;;      (It used to mean the same as ARG > 0.)
;;      Prefix ARG >= 0 means list just file buffers.
;;;###autoload
(defun buffer-menu (&optional arg)
  "Make a menu of buffers so you can save, delete or select them.
By default (no or null prefix arg), the buffers are listed in order of
last access.  With a non-nil prefix ARG:
  ARG >= 0   => Only buffers visiting files are listed.
  ARG =< 0   => The buffers are listed alphabetically.
 (ARG =  0   => Only buffers visiting files, listed alphabetically.)

Type `?' in buffer \"*Buffer List*\" to get help on available commands.
Type `q' there to quit the buffer menu."
  (interactive "P")
  (let ((num-arg (prefix-numeric-value arg)))
    (if (and arg (< num-arg 0))
        (list-buffers)
      (list-buffers arg))
    (let ((newpoint (save-excursion (set-buffer "*Buffer List*") (point))))
      (pop-to-buffer "*Buffer List*")
      (when (and arg (not (> num-arg 0))) ; Sort lines after header.
        (let ((buffer-read-only nil))
          (goto-char (point-min)) (forward-line 2) (forward-char 4) ; Header.
          (sort-columns nil (point)
                        (save-excursion (goto-char (point-max))
                                        (when (bolp) (backward-char 1))
                                        (point)))))
      (goto-char newpoint)))
  (font-lock-fontify-buffer)
  (message "Help: ?;   Menu: mouse-3;   Show: v;   Mark: u,m,s,d;   \
Save/Delete: x;   Misc: g,~,%%,t"))


;; REPLACES ORIGINAL in `buff-menu.el':
;; 1. Doc string reflects new bindings.
;; 2. mouse-face on whole line, not just buffer name.
;;;###autoload
(defun Buffer-menu-mode ()
  "Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
In Buffer menu mode, chars do not insert themselves, but are commands.
\\<Buffer-menu-mode-map>
\(\"Current line\" here is the line of the text cursor or the mouse.)

Also, pressing `mouse-3' on a buffer name in this mode provides a
popup menu that duplicates most of the functions below.


Display buffers:
---------------
\\[Buffer-menu-mouse-select], \\[Buffer-menu-select] -- Select current line's \
buffer.
\\[Buffer-menu-mark]\t-- Mark current line's buffer `>' to be displayed (via \
`\\[Buffer-menu-select]').
\\[Buffer-menu-select]\t-- Show buffers marked `>'.  Select current line's \
buffer.
\\[Buffer-menu-1-window]\t-- Select current line's buffer (only) in a \
full-frame window.
\\[Buffer-menu-2-window]\t-- Select current line's buffer in one window.
\t   Display previous buffer in a second window.
\\[Buffer-menu-switch-other-window]\t-- Display current line's buffer in \
another window. No select.

Mark/Unmark buffers to be Saved/Deleted:
---------------------------------------
\\[Buffer-menu-save]\t-- Mark current line's buffer `S' to be saved.    \
Cursor down.
\\[Buffer-menu-delete]\t-- Mark current line's buffer `D' to be deleted.  \
Cursor down.
\\[Buffer-menu-delete-backwards]\t-- Mark current line's buffer `D' to be \
deleted.  Cursor up.
\\[Buffer-menu-unmark]\t-- Unmark current line. Cursor down. (Prefix arg: \
Cursor up.)
\\[Buffer-menu-backup-unmark]\t-- Cursor up, then unmark line.

Save/Delete buffers:
-------------------
\\[Buffer-menu-execute]\t-- Save / Delete marked buffers (marks `S', `D').

Miscellaneous:
-------------
\\[Buffer-menu-not-modified]\t-- Clear modified-flag on current line's buffer.
\\[Buffer-menu-toggle-read-only]\t-- Toggle read-only status of current \
line's buffer.
\\[Buffer-menu-visit-tags-table]\t-- `visit-tags-table' using current line's \
buffer.


Bindings in Buffer Menu mode:
----------------------------

\\{Buffer-menu-mode-map}"
  (kill-all-local-variables)
  (use-local-map Buffer-menu-mode-map)
  (setq major-mode 'Buffer-menu-mode)
  (setq mode-name "Buffer Menu")
  (save-excursion
    (goto-char (point-min))
    (when (< emacs-major-version 20)    ; Hardcoded to 4, starting in Emacs 20
      (search-forward "Buffer")
      (backward-word 1)
      (setq Buffer-menu-buffer-column (current-column)))
    (forward-line 2)
    (while (not (eobp))
      (put-text-property (point)
                         (save-excursion (end-of-line) (point))
                         'mouse-face 'highlight)
      (forward-line 1)))
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'Buffer-menu-revert-function)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (run-hooks 'buffer-menu-mode-hook))


;; REPLACES ORIGINAL in `buff-menu.el': Deletes frame when kills buffer.
;;;###autoload
(defun Buffer-menu-execute ()
  "Save or delete buffers marked `S' or `D', respectively.
Buffers are so marked using command `\\<Buffer-menu-mode-map>\
\\[Buffer-menu-save]' or `\\<Buffer-menu-mode-map>\\[Buffer-menu-delete]', respectively."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward "^.S" nil t)
      (let ((modp nil))
        (save-excursion
          (set-buffer (Buffer-menu-buffer t))
          (save-buffer)
          (setq modp (buffer-modified-p)))
        (let ((buffer-read-only nil))
          (delete-char -1)
          (insert (if modp ?* ? ))))))
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let ((buff-menu-buffer (current-buffer))
          (buffer-read-only nil))
      (while (search-forward "\nD" nil t)
        (forward-char -1)
        (let ((buf (Buffer-menu-buffer nil)))
          (or (eq buf nil) (eq buf buff-menu-buffer)
              (save-excursion (if (fboundp 'kill-buffer-and-its-windows)
                                  (kill-buffer-and-its-windows buf)
                                (kill-buffer buf)))))
        (if (Buffer-menu-buffer nil)
            (progn (delete-char 1) (insert ? ))
          (delete-region (point) (progn (forward-line 1) (point)))
          (forward-char -1))))))


;; REPLACES ORIGINAL in `buff-menu.el':
;; When Buffer Menu is `window-dedicated-p', uses `pop-to-buffer' to display.
;;;###autoload
(defun Buffer-menu-select ()
  "Select this line's buffer; also display buffers marked with `>'.
You can mark buffers with the \\<Buffer-menu-mode-map>\\[Buffer-menu-mark] \
command."
  (interactive)
  (let ((buff (Buffer-menu-buffer t))
        (menu (current-buffer))       
        (others ())
        tem)
    (goto-char (point-min))
    (while (search-forward "\n>" nil t)
      (setq tem (Buffer-menu-buffer t))
      (let ((buffer-read-only nil)) (delete-char -1) (insert ?\ ))
      (or (eq tem buff) (memq tem others) (push tem others)))
    (setq others (nreverse others))
    (cond ((window-dedicated-p (selected-window)) ; Can't split dedicated win.
           (pop-to-buffer buff)
           (unless (eq menu buff) (bury-buffer menu))
           (while others
             (pop-to-buffer (car others))
             (pop others)))
          (t
           (setq tem (/ (1- (frame-height)) (1+ (length others))))
           (delete-other-windows)
           (switch-to-buffer buff)
           (unless (eq menu buff) (bury-buffer menu))
           (if (equal (length others) 0)
               (progn
;;;              ;; Restore previous window configuration before displaying
;;;              ;; selected buffers.
;;;              (if Buffer-menu-window-config
;;;                  (progn (set-window-configuration
;;;                            Buffer-menu-window-config)
;;;                         (setq Buffer-menu-window-config nil)))
                 (switch-to-buffer buff))
             (while others
               (split-window nil tem)
               (other-window 1)
               (switch-to-buffer (car others))
               (pop others))
             (other-window 1))))))      ; Back to the beginning.


(define-key Buffer-menu-mode-map [down-mouse-3] 'Buffer-menu-mouse-3-menu)
(define-key Buffer-menu-mode-map [mouse-3] 'ignore)

;; Another way, but it shows the menu even if not on a buffer line,
;; and it doesn't show it if on the line but not on the buffer name itself.
;;(defvar Buffer-menu-mouse-3-map (make-sparse-keymap "Buffers"))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-execute]
;;  '("Execute: Save/Delete Marked Buffers" . Buffer-menu-mouse-execute))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-modified]
;;  '("Mark as Modified/Unmodified (*)" . Buffer-menu-mouse-modified))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-delete]
;;  '("Mark to Delete Buffer (D)" . Buffer-menu-mouse-delete))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-save]
;;  '("Mark to Save Buffer (S)" . Buffer-menu-mouse-save))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-unmark]
;;  '("Unmark Buffer" . Buffer-menu-mouse-unmark))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-select]
;;  '("Select Buffer" . Buffer-menu-mouse-select))

;; Used to highlight buffer name's line during popup of Mouse-3 menu.
(defvar Buffer-menu-overlay nil)

;;;###autoload
(defun Buffer-menu-mouse-3-menu (event)
  "Pop up menu for Mouse-3 for buffer listed in buffer menu."
  (interactive "e")
  (let* ((mouse-pos (event-start event))
         bol eol temp
         (buffer-name
          (save-excursion
            (set-buffer (window-buffer (posn-window mouse-pos)))
            (save-excursion
              (goto-char (posn-point mouse-pos))
              (save-excursion
                (setq bol (progn (beginning-of-line) (point)))
                (setq eol (progn (end-of-line) (point))))
              (if Buffer-menu-overlay   ; Don't recreate if exists.
                  (move-overlay Buffer-menu-overlay bol eol (current-buffer))
                (setq Buffer-menu-overlay (make-overlay bol eol))
                (overlay-put Buffer-menu-overlay 'face 'region))
              (setq temp (and (not (eobp)) (Buffer-menu-buffer nil)))
              ;; Nil if mouse is not on a buffer name.
              (and temp (buffer-name temp)))))) ; temp no longer used.
    (sit-for 0)
    (let ((selection
           (x-popup-menu
            event
            (list
             "Menu"
             (if buffer-name
                 (list
                  buffer-name
                  '("Select Buffer" . Buffer-menu-mouse-select)
                  '("Unmark Buffer" . Buffer-menu-mouse-unmark)
                  '("Mark to Save Buffer (S)" . Buffer-menu-mouse-save)
                  '("Mark to Delete Buffer (D)" . Buffer-menu-mouse-delete)
                  '("Mark as Modified/Unmodified (*)" .
                    Buffer-menu-mouse-modified)
                  '("--")               ; Separator: next not buffer-specific.
                  '("Execute: Save/Delete Marked Buffers" .
                    Buffer-menu-mouse-execute))
               (list "" '("Execute: Save/Delete Marked Buffers" .
                          Buffer-menu-mouse-execute)))))))
      (when Buffer-menu-overlay (delete-overlay Buffer-menu-overlay))
      (and selection (call-interactively selection)))))

;; Don't need this if use dedicated frame for buffer menu.
;;;###autoload
(defun Buffer-menu-mouse-other-window (event)
  "Select, in another window, the buffer on whose line you click."
  (interactive "e")
  (let (buffer)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq buffer (Buffer-menu-buffer t))))
    (select-window (posn-window (event-end event)))
    (switch-to-buffer-other-window buffer)))

;;;###autoload
(defun Buffer-menu-mouse-unmark (event)
  "Cancel all requested operations on buffer."
  (interactive "e")
  (let (buffer)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq buffer (Buffer-menu-buffer t))))
    (select-window (posn-window (event-end event)))
    (goto-char (posn-point (event-end event)))
    (beginning-of-line)
    (if (looking-at " [-M]")            ;header lines
        (ding)
      (let* ((mod (buffer-modified-p buffer))
             (readonly (save-excursion (set-buffer buffer) buffer-read-only))
             (buffer-read-only nil))
        (delete-char 3)
        (insert (if readonly (if mod " *%" "  %") (if mod " * " "   ")))))
    (beginning-of-line)))

;;;###autoload
(defun Buffer-menu-mouse-save (event)
  "Mark buffer to be saved.
Actual deletion is done via `\\<Buffer-menu-mode-map>\\[Buffer-menu-execute]' \
or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-execute]'."
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (beginning-of-line)
  (forward-char 1)
  (if (looking-at " [-M]")              ;header lines
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
        (insert ?S)))
  (beginning-of-line))

;;;###autoload
(defun Buffer-menu-mouse-delete (event)
  "Mark buffer to be deleted.
Actual deletion is done via `\\<Buffer-menu-mode-map>\\[Buffer-menu-execute]' \
or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-execute]'."
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (beginning-of-line)
  (if (looking-at " [-M]")              ;header lines
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?D)))
  (beginning-of-line))

;;;###autoload
(defun Buffer-menu-mouse-modified (event)
  "Mark buffer as unmodified (no changes to save) if modified, and vice versa."
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (beginning-of-line)
  (forward-char 1)
  (let ((buffer-read-only nil)
        modified-p)
    (save-excursion
      (set-buffer (Buffer-menu-buffer t))
      (set-buffer-modified-p (not (buffer-modified-p))))
    (cond ((= ?\* (char-after (point)))
           (delete-char 1)
           (insert ?\ ))
          (t
           (delete-char 1)
           (insert ?\*))))
  (beginning-of-line))

;;;###autoload
(defun Buffer-menu-mouse-execute (event)
  "Save and/or delete buffers marked `S' or `D', respectively.
Buffers can be marked via commands `\\<Buffer-menu-mode-map>\
\\[Buffer-menu-save]' and `\\<Buffer-menu-mode-map>\\[Buffer-menu-delete]'
\(or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-save]' and \
`\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-delete]')."
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward "^.S" nil t)
      (let ((modp nil))
        (save-excursion
          (set-buffer (Buffer-menu-buffer t))
          (save-buffer)
          (setq modp (buffer-modified-p)))
        (let ((buffer-read-only nil))
          (delete-char -1)
          (insert (if modp ?* ? ))))))
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let ((buff-menu-buffer (current-buffer))
          (buffer-read-only nil))
      (while (search-forward "\nD" nil t)
        (forward-char -1)
        (let ((buf (Buffer-menu-buffer nil)))
          (or (eq buf nil)
              (eq buf buff-menu-buffer)
              (save-excursion (if (fboundp 'kill-buffer-and-its-windows)
                                  (kill-buffer-and-its-windows buf)
                                (kill-buffer buf)))))
        (if (Buffer-menu-buffer nil)
            (progn (delete-char 1)
                   (insert ? ))
          (delete-region (point) (progn (forward-line 1) (point)))
          (forward-char -1))))))


;;;;;;;;;;;;;;;;;;;;;;;

(provide 'buff-menu+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE NAME SHOULD BE buff-menu+.el, but EmacsWiki doesn't like that.
;;; buff-menu-plus.el ends here
