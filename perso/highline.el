;;; highline.el --- Minor mode to highlight current line in buffer.

;; Copyright (C) 2000 Vinicius Jose Latorre

;; Author: Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Maintainer: Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Keywords: faces, frames, editing
;; Time-stamp: <2000/08/01 10:04:21 vinicius>
;; Version: 1.7
;; X-URL: http://www.cpqd.com.br/~vinicius/emacs/Emacs.html

;; This file is *NOT* (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package is a minor mode to highlight the current line in buffer.
;;
;; highline was inspired on:
;;
;;    linemenu.el
;;  Bill Brodie <wbrodie@panix.com>
;;    hl-line.el
;;  Dave Love <fx@gnu.org>
;;    highlight-current-line.el
;;  Christoph Conrad <christoph.conrad@gmx.de>
;;
;; To use highline, insert in your ~/.emacs:
;;
;;    (require 'highline)
;;
;; Or:
;;
;;    (autoload 'highline-on         "highline"
;;      "Turn on local highlightining current line in buffer."          t)
;;    (autoload 'highline-off        "highline"
;;      "Turn off local highlightining current line in buffer."         t)
;;    (autoload 'highline-local-mode "highline"
;;      "Toggle local minor mode to highlight current line in buffer."  t)
;;    (autoload 'highline-mode-on    "highline"
;;      "Turn on global highlightining current line in buffer."         t)
;;    (autoload 'highline-mode-off   "highline"
;;      "Turn off global highlightining current line in buffer."        t)
;;    (autoload 'highline-mode       "highline"
;;      "Toggle global minor mode to highlight current line in buffer." t)
;;    (autoload 'highline-customize  "highline"
;;      "Customize highlightining current line in buffer."              t)
;;
;; For good performance, be sure to byte-compile highline.el, e.g.
;;
;;    M-x byte-compile-file <give the path to highline.el when prompted>
;;
;; This will generate highline.elc, which will be loaded instead of
;; highline.el.
;;
;; highline was tested with GNU Emacs 20.4.1.
;;
;;
;; Using highline
;; --------------
;;
;; To activate highline locally, type:
;;
;;    M-x highline-on RET
;;
;; Or:
;;
;;    C-u 1 M-x highline-local-mode RET
;;
;; To deactivate highline locally, type:
;;
;;    M-x highline-off RET
;;
;; Or:
;;
;;    C-u 0 M-x highline-local-mode RET
;;
;; To toggle highline locally, type:
;;
;;    M-x highline-local-mode RET
;;
;; To activate highline globally, type:
;;
;;    M-x highline-mode-on RET
;;
;; Or:
;;
;;    C-u 1 M-x highline-mode RET
;;
;; To deactivate highline globally, type:
;;
;;    M-x highline-mode-off RET
;;
;; Or:
;;
;;    C-u 0 M-x highline-mode RET
;;
;; To toggle highline globally, type:
;;
;;    M-x highline-mode RET
;;
;; To customize highline, type:
;;
;;    M-x highline-customize RET
;;
;; You can also bind `highline-local-mode', `highline-mode', `highline-on',
;; `highline-off', `highline-mode-on', `highline-mode-off' and
;; `highline-customize' to some key, like:
;;
;;    (global-set-key "\C-c\C-a" 'highline-on)
;;    (global-set-key "\C-c\C-b" 'highline-off)
;;    (global-set-key "\C-c\C-l" 'highline-local-mode)
;;    (global-set-key "\C-c\C-d" 'highline-mode-on)
;;    (global-set-key "\C-c\C-e" 'highline-mode-off)
;;    (global-set-key "\C-c\C-g" 'highline-mode)
;;    (global-set-key "\C-c\C-c" 'highline-customize)
;;
;; NOTE: There is no problem if you mix local and global minor mode usage.
;;
;;
;; Example
;; -------
;;
;; As an example, try to insert this in your .emacs file:
;;
;;  (require 'highline)
;;  ;; Turn on local highlighting for Dired (C-x d)
;;  (add-hook 'dired-after-readin-hook 'highline-on)
;;  ;; Turn on local highlighting for list-buffers (C-x C-b)
;;  (defadvice list-buffers (after highlight-line activate)
;;    (save-excursion
;;      (set-buffer "*Buffer List*")
;;      (highline-on)))
;;
;;
;; Hooks
;; -----
;;
;; highline has the following hook variables:
;;
;; `highline-hook'
;;    It is evaluated always when highline is turned on globally.
;;
;; `highline-local-hook'
;;    It is evaluated always when highline is turned on locally.
;;
;; `highline-load-hook'
;;    It is evaluated after highline package is loaded.
;;
;;
;; Options
;; -------
;;
;; Below it's shown a brief description of highline options, please, see the
;; options declaration in the code for a long documentation.
;;
;; `highline-face'   Specify face used to highlight the
;;     current line.
;;
;; `highline-line'   Specify which part of line should be
;;     highlighted.
;;
;; `highline-verbose'   Non-nil means generate messages.
;;
;; `highline-ignore-regexp'  Specify regexp for buffers to ignore.
;;
;; `highline-priority'   Specify highline overlay priority.
;;
;; To set the above options you may:
;;
;; a) insert the code in your ~/.emacs, like:
;;
;;  (setq highline-face 'highlight)
;;
;;    This way always keep your default settings when you enter a new Emacs
;;    session.
;;
;; b) or use `set-variable' in your Emacs session, like:
;;
;;  M-x set-variable RET highline-face RET highlight RET
;;
;;    This way keep your settings only during the current Emacs session.
;;
;; c) or use customization, for example:
;;  click on menu-bar *Help* option,
;;  then click on *Customize*,
;;  then click on *Browse Customization Groups*,
;;  expand *Editing* group,
;;  expand *Highline* group
;;  and then customize highline options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; d) or see the option value:
;;
;;  C-h v highline-face RET
;;
;;    and click the *customize* hypertext button.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; e) or invoke:
;;
;;  M-x highline-customize RET
;;
;;    and then customize highline options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;;
;; Acknowledgements
;; ----------------
;;
;; Thanks to Trey Jackson <bigfaceworm@hotmail.com> for `highline-line'
;; enhancements.
;;
;; Thanks to Fredrik Sundstroem <fresun-7@sm.luth.se> for permanent-local
;; overlay property indication.
;;
;; Thanks to:
;;    Bill Brodie <wbrodie@panix.com> linemenu.el
;;    Dave Love <fx@gnu.org>  hl-line.el
;;    Christoph Conrad <christoph.conrad@gmx.de>
;;     highlight-current-line.el
;; And to all people who contributed with them.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


;; XEmacs needs overlay emulation package
(eval-and-compile
  (and (let (case-fold-search)
  (string-match "XEmacs\\|Lucid\\|Epoch" emacs-version))
       (not (require 'overlay nil 'noerr))
       (error "`highline' requires `overlay' package.")))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Variables:


;;; Interface to the command system

(defgroup highline nil
  "Highlight the current line"
  :link '(emacs-library-link :tag "Source Lisp File" "highline.el")
  :group 'faces
  :group 'frames
  :group 'editing)


(defcustom highline-face 'highline-face
  "*Specify face used to highlight the current line."
  :type 'face
  :group 'highline)


(defface highline-face '((t (:background "paleturquoise")))
  "Face used to highlight current line.")


(defcustom highline-line t
  "*Specify which part of line should be highlighted.

Valid values are:

   t   mark up to end of line.

   nil   mark up to window border.

   integer  mark up from column 0 to column indicated by integer.

   (LOWER . UPPER) mark up the region from column LOWER to column UPPER.
   LOWER must be lesser than UPPER.

Any other value is treated as t."
  :type '(choice :menu-tag "Mark Up To"
   :tag "Mark Up To"
   (const :tag "End Of Line" t)
   (const :tag "Window Border" nil)
   (integer :tag "Column")
   (cons :tag "Range"
         (integer :tag "From")
         (integer :tag "To")))
  :group 'highline)


(defcustom highline-verbose t
  "*Non-nil means generate messages."
  :type 'boolean
  :group 'highline)


(defcustom highline-ignore-regexp
  (concat "Faces\\|Colors\\|Minibuf"
   ;; for example:
   ;; "\\|RMAIL.*summary\\|\\*Group\\|\\*Summary"
   )
  "*Specify regexp for buffers to ignore.

Set to nil or \"\", to accept any buffer.

Used by `highline-highlight-current-line'."
  :type 'regexp
  :group 'highline)


(defcustom highline-priority 0
  "*Specify highline overlay priority.

Higher integer means higher priority, so highline overlay will have precedence
over overlays with lower priority.  *Don't* use negative number."
  :type 'integer
  :group 'highline)

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; GNU Emacs
(or (fboundp 'line-beginning-position)
    (defun line-beginning-position (&optional n)
      (save-excursion
 (and n (/= n 1) (forward-line (1- n)))
 (beginning-of-line)
 (point))))


;; GNU Emacs
(or (fboundp 'line-end-position)
    (defun line-end-position (&optional n)
      (save-excursion
 (and n (/= n 1) (forward-line (1- n)))
 (end-of-line)
 (if (and (eobp) (eq highline-line t))
     (1- (point))
   (point)))))


;; Macro
(defmacro highline-message (&rest body)
  `(and highline-verbose (interactive-p)
 (message ,@body)))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization


;;;###autoload
(defun highline-customize ()
  "Customize highline group."
  (interactive)
  (customize-group 'highline))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User commands


(defvar highline-mode nil
  "Non-nil means highline global minor mode is enabled (HL on modeline).")


(defvar highline-local-mode nil
  "Non-nil means highline local minor mode is enabled (hl on modeline).")
(make-variable-buffer-local 'highline-local-mode)


(defvar highline-overlay nil
  "Highlight for current line")
(make-variable-buffer-local 'highline-overlay)

(put 'highline-overlay 'permanent-local t)


;;;###autoload
(defun highline-mode (&optional arg)
  "Toggle global minor mode to highlight line about point (HL on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system."
  (interactive "P")
  (if (if arg
   (> (prefix-numeric-value arg) 0)
 (not highline-mode))
      (highline-mode-on)
    (highline-mode-off))
  (highline-message "Highline global mode is %s"
      (if highline-mode "on" "off")))


;;;###autoload
(defun highline-mode-on ()
  "Turn on global minor mode to highlight line about point (HL on modeline)."
  (interactive)
  (save-excursion
    (let ((buffers (buffer-list))
   (temp (get-buffer-create (make-temp-name " *Temp"))))
      ;; be sure to access global `post-command-hook'
      (set-buffer temp)
      (setq highline-mode t)
      (add-hook 'post-command-hook 'highline-highlight-current-line)
      (while buffers   ; adjust all local mode
 (set-buffer (car buffers))
 (unless highline-local-mode
   (add-hook 'post-command-hook 'highline-highlight-current-line nil t)
   (highline-highlight-current-line))
 (setq buffers (cdr buffers)))
      (highline-highlight-current-line)
      (kill-buffer temp)))
  (run-hooks 'highline-hook)
  (highline-message "Highline global mode is on"))


;;;###autoload
(defun highline-mode-off ()
  "Turn off global minor mode to highlight line about point (HL on modeline)."
  (interactive)
  (save-excursion
    (let ((buffers (buffer-list))
   (temp (get-buffer-create (make-temp-name " *Temp"))))
      ;; be sure to access global `post-command-hook'
      (set-buffer temp)
      (setq highline-mode nil)
      (remove-hook 'post-command-hook 'highline-highlight-current-line)
      (while buffers   ; adjust all local mode
 (set-buffer (car buffers))
 (unless highline-local-mode
   (remove-hook 'post-command-hook 'highline-highlight-current-line t)
   (highline-unhighlight-current-line))
 (setq buffers (cdr buffers)))
      (kill-buffer temp)))
  (highline-message "Highline global mode is off"))


;;;###autoload
(defun highline-local-mode (&optional arg)
  "Toggle local minor mode to highlight the line about point (hl on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system."
  (interactive "P")
  (if (if arg
   (> (prefix-numeric-value arg) 0)
 (not highline-local-mode))
      (highline-on)
    (highline-off))
  (highline-message "Highline local mode is %s"
      (if highline-local-mode "on" "off")))


;;;###autoload
(defun highline-on ()
  "Turn on local highlighting of the current line in buffer (hl on modeline)."
  (interactive)
  (setq highline-local-mode t)
  (make-local-variable 'post-command-hook)
  (add-hook 'post-command-hook 'highline-highlight-current-line nil t)
;;;  (make-local-variable 'mouse-leave-buffer-hook)
;;;  (add-hook 'mouse-leave-buffer-hook 'highline-mouse nil t)
  (highline-highlight-current-line)
  (run-hooks 'highline-local-hook)
  (highline-message "Highline local mode is on"))


;;;###autoload
(defun highline-off ()
  "Turn off local highlighting of the current line in buffer (hl on modeline)."
  (interactive)
  (setq highline-local-mode nil)
  (remove-hook 'post-command-hook 'highline-highlight-current-line t)
;;;  (remove-hook 'mouse-leave-buffer-hook 'highline-mouse t)
  (highline-unhighlight-current-line)
  (highline-message "Highline local mode is off"))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions


(defun highline-highlight-current-line ()
  "Highlight current line."
  (unless (and highline-ignore-regexp
        (not (equal "" highline-ignore-regexp))
        (string-match highline-ignore-regexp (buffer-name)))
    (or highline-overlay
 (setq highline-overlay (make-overlay 1 1))) ; Hide it for now
    (overlay-put highline-overlay 'hilit t)
    (overlay-put highline-overlay 'face highline-face)
    (overlay-put highline-overlay 'priority highline-priority)
    ;; move highlight to the current line
    (let ((start  (line-beginning-position))
   (end    (line-end-position))
   (rangep (and (consp highline-line)
         (integerp (car highline-line))
         (integerp (cdr highline-line))
         (< (car highline-line) (cdr highline-line)))))
      (move-overlay highline-overlay
      (if rangep
   (max start (+ start (car highline-line)))
        start)
      (cond (rangep
      (min end (+ start (cdr highline-line))))
     ((integerp highline-line)
      (min end (+ start highline-line)))
     (highline-line
      end)
     ((1+ end)))))))


(defun highline-unhighlight-current-line ()
  "Unhighlight current line."
  (and highline-overlay
       (move-overlay highline-overlay 1 1)))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advising functions


(defsubst highline-p ()
  (or highline-mode highline-local-mode))


(defadvice mouse-set-region (before highline-mouse-set-region)
  (and (highline-p)
       (highline-highlight-current-line)))


(defadvice mouse-drag-region (before highline-mouse-drag-region)
  (and (highline-p)
       (highline-unhighlight-current-line)))


(ad-activate 'mouse-set-region)
(ad-activate 'mouse-drag-region)

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'minor-mode-alist '(highline-mode " HL"))
(add-to-list 'minor-mode-alist '(highline-local-mode " hl"))


(provide 'highline)


(run-hooks 'highline-load-hook)


;;; highline.el ends here