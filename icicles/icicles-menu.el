;;; icicles-menu.el --- Execute menu items as commands, with completion.
;;
;; Filename: icicles-menu.el
;; Description: Execute menu items as commands, with completion.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005-2006, Drew Adams, all rights reserved.
;; Created: Fri Aug 12 17:18:02 2005
;; Version: 22.0
;; Last-Updated: Mon Feb 27 10:41:47 2006 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 301
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-menu.el
;; Keywords: menu-bar, menu, command, help, abbrev, minibuffer, keys,
;;           completion, matching, local, internal, extensions,
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `subr-21'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Execute menu items as commands, with completion.
;;
;;  Type a menu item.  Completion is available.  The corresponding
;;  command is executed.
;;
;;  Put this in your init file (~/.emacs):
;;
;;    (require 'icicles-menu)
;;
;;  Suggested binding:
;;
;;    (global-set-key [?\e ?\M-x] 'icicle-execute-menu-command)
;;
;;  Consider also replacing `tmm-menu':
;;
;;    (global-set-key [?\M-`] 'icicle-execute-menu-command)
;;
;;  For a powerful and easy-to-use extension to ordinary minibuffer
;;  completion, try library `icicles.el'.  It enhances the
;;  functionality of `icicles-menu.el' in several ways, but it is not
;;  required, to be able to use `icicles-menu.el'.
;;
;;  Note: If you use MS Windows keyboard accelerators, consider using
;;        `icicle-remove-w32-keybd-accelerators' as the value of
;;        `icicle-convert-menu-item-function'.  It removes any
;;        unescaped `&' characters (indicating an accelerator) from
;;        the menu items.  A library that adds keyboard accelerators
;;        to your menu items is `emacsw32-menu.el', by Lennart Borgman
;;        (<lennart.borgman.073@student.lu.se>).
;;
;;
;;  To Do?
;;  ------
;;
;;  1. Provide an option to sort by menu-bar order, instead of
;;     alphabetically.
;;  2. Echo key bindings for each completed menu item.
;;
;;  3. Maybe use tmm-get-bind?
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/02/18 dadams
;;      icicle-execute-menu-command: \s -> \\s.  (Thx to dslcustomer-211-74.vivodi.gr.)
;; 2006/01/07 dadams
;;      Added :link for sending bug reports.
;; 2006/01/06 dadams
;;      Changed defgroup to icicles-menu from icicles.
;;      Added :link.
;; 2005/11/08 dadams
;;     icicle-execute-menu-command: 
;;       Reset icicle-menu-items-alist in unwind-protect.
;;       Fix for dynamic menus Select and Paste, Buffers, and Frames: 
;;         Treat special cases of last-command-event.
;;     icicle-get-overall-menu-item-alist: setq result of sort.
;; 2005/11/05 dadams
;;     Replaced icicle-menu-items with icicle-menu-items-alist (no need for both).
;;     icicle-execute-menu-command: Set, don't bind icicle-menu-items-alist.
;; 2005/08/23 dadams
;;     icicle-execute-menu-command: renamed alist to icicle-menu-items-alist, so can
;;       refer to it unambiguously in icicle-help-on-candidate (in icicles.el).
;; 2005/08/19 dadams
;;     Added: icicle-convert-menu-item-function, icicle-remove-w32-keybd-accelerators,
;;            icicle-escape-w32-accel.
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(when (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when, unless

(unless (fboundp 'replace-regexp-in-string) (require 'subr-21 nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;



;;; User Options -------------------------------------------

(defgroup Icicles-Menu nil
  "Execute menu items as commands, with completion."
  :prefix "icicle-" :group 'icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=
icicles-menu.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download" "http://www.emacswiki.org/cgi-bin/wiki/icicles-menu.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/cgi-bin/wiki/IciclesMenu")
  :link '(emacs-commentary-link :tag "Commentary" "icicles-menu.el")
  )

(defcustom icicle-convert-menu-item-function nil
  "*Function to call to convert a menu item.
Used by `icicle-execute-menu-command'.  A typical use would be to
remove the `&' characters used in MS Windows menus to define keyboard
accelerators.  See `icicle-remove-w32-keybd-accelerators'."
  :type  'function :group 'Icicles-Menu)

;; $$$ NOT YET IMPLEMENTED
;; (defcustom icicle-sort-menu-bar-order-flag nil
;;   "*Non-nil means that `icicle-execute-menu-command' uses menu-bar order.
;; Nil means use alphabetic order.
;; The order is what is used for completion.
;; Note: Using a non-nil value imposes an extra sorting operation, which
;;       slows down the creation of the completion-candidates list."
;;   :type 'boolean :group 'Icicles-Menu)



;;; Internal variables -------------------------------------

;; This is used also in `icicle-help-on-candidate', which is defined in `icicles.el'.
(defvar icicle-menu-items-alist nil
  "Alist of pairs (MENU-ITEM . COMMAND).
The pairs are defined by the current local and global keymaps.
MENU-ITEM is a menu item, with ancestor-menu prefixes.
  Example: `(\"Files > Insert File...\" . insert-file)'.
COMMAND is the command  bound to the menu item.")



;;; Functions -------------------------------

(defun icicle-execute-menu-command ()
  "Execute a menu-bar menu command.
Type a menu item.  Completion is available."
  (interactive)
  (unwind-protect
       (progn
         (setq icicle-menu-items-alist (icicle-get-overall-menu-item-alist))
         (let* ((menu-item (completing-read "Menu command: " icicle-menu-items-alist))
                (cmd (cdr (assoc menu-item icicle-menu-items-alist))))
           (unless cmd (error "No such menu command"))
           ;; Treat special cases of `last-command-event', reconstructing it for
           ;; menu items that get their meaning from the click itself.
           (cond ((eq cmd 'menu-bar-select-buffer)
                  (string-match " >\\s-+\\(.+\\)\\s-+\\*?%?\\s-+\\S-*\\s-*$"
                                menu-item)
                  (setq menu-item (substring menu-item (match-beginning 1) (match-end 1)))
                  (when (string-match "  \\*?%?" menu-item)
                    (setq menu-item (substring menu-item 0 (match-beginning 0))))
                  (setq last-command-event menu-item))
                 ((eq cmd 'menu-bar-select-yank)
                  (string-match "Edit > Select and Paste > \\(.*\\)$" menu-item)
                  (setq last-command-event
                        (substring menu-item (match-beginning 1) (match-end 1))))
                 ((eq cmd 'menu-bar-select-frame)
                  (string-match " >\\s-[^>]+>\\s-+\\(.+\\)$" menu-item)
                  (setq menu-item (substring menu-item (match-beginning 1) (match-end 1)))
                  (setq last-command-event menu-item)))
           (call-interactively cmd)))
    (setq icicle-menu-items-alist nil))) ; Reset it.

(defun icicle-get-overall-menu-item-alist ()
  "Alist formed from menu items in current local and global keymaps.
See `icicle-get-a-menu-item-alist' for the structure."
  (let ((alist
         (nconc (icicle-get-a-menu-item-alist (assq 'menu-bar (current-local-map)))
                (icicle-get-a-menu-item-alist (assq 'menu-bar (current-global-map))))))
    (if nil;; icicle-sort-menu-bar-order-flag ; Not yet implemented.
        (setq alist (sort alist))
      alist)))

(defun icicle-get-a-menu-item-alist (keymap)
  "Alist of pairs (MENU-ITEM . COMMAND) defined by KEYMAP.
KEYMAP is any keymap that has menu items.
MENU-ITEM is a menu item, with ancestor-menu prefixes.
  Example: `(\"Files > Insert File...\" . insert-file)'.
COMMAND is the command  bound to the menu item."
  (setq icicle-menu-items-alist nil)
  (icicle-get-a-menu-item-alist-1 keymap)
  (nreverse icicle-menu-items-alist))

(defun icicle-get-a-menu-item-alist-1 (keymap &optional root)
  "Helper function for `icicle-get-a-menu-item-alist'.
This calls itself recursively, to process submenus."
  (let ((scan keymap))
    (setq root (or root))               ; nil, for top level.
    (while (consp scan)
      (if (atom (car scan))
          (setq scan (cdr scan))
        (let ((defn (cdr (car scan)))
              composite-name)
          ;; Get REAL-BINDING for the menu item.
          (cond
            ;; (menu-item ITEM-STRING): non-selectable item - skip it.
            ((and (eq 'menu-item (car-safe defn))
                  (null (cdr-safe (cdr-safe defn))))
             (setq defn nil))           ; So `keymapp' test, below, fails.

            ;; (ITEM-STRING): non-selectable item - skip it.
            ((and (stringp (car-safe defn)) (null (cdr-safe defn)))
             (setq defn nil))           ; So `keymapp' test, below, fails.

            ;; (menu-item ITEM-STRING REAL-BINDING . PROPERTIES)
            ((eq 'menu-item (car-safe defn))
             (setq composite-name (concat root (and root " > ") (eval (cadr defn))))
             (setq defn (car (cddr defn))))

            ;; (ITEM-STRING . REAL-BINDING) or
            ;; (ITEM-STRING [HELP-STRING] (KEYBD-SHORTCUTS) . REAL-BINDING)
            ((stringp (car-safe defn))
             (setq composite-name (concat root (and root " > ") (eval (car defn))))
             (setq defn (cdr defn))
             ;; Skip HELP-STRING
             (when (stringp (car-safe defn)) (setq defn (cdr defn)))
             ;; Skip (KEYBD-SHORTCUTS): cached key-equivalence data for menu items.
             (when (and (consp defn) (consp (car defn))) (setq defn (cdr defn)))))

          ;; If REAL-BINDING is a keymap, then recurse on it.
          (when (keymapp defn)
            ;; Follow indirections to ultimate symbol naming a command.
            (while (and (symbolp defn) (fboundp defn) (keymapp (symbol-function defn)))
              (setq defn (symbol-function defn)))
            (if (eq 'keymap (car-safe defn))
                (icicle-get-a-menu-item-alist-1 (cdr defn) composite-name)
              (icicle-get-a-menu-item-alist-1 (symbol-function defn) composite-name)))

          ;; Add menu item and command pair to `icicle-menu-items-alist' alist.
          ;; (`name' is bound in `icicle-get-a-menu-item-alist'.)
          (when (and root (not (keymapp defn)))
            (setq icicle-menu-items-alist
                  (cons
                   (cons (if (and (functionp icicle-convert-menu-item-function)
                                  (stringp composite-name)) ; Could be nil
                             (funcall icicle-convert-menu-item-function composite-name)
                           composite-name)
                         defn)
                   icicle-menu-items-alist))))
        (when (consp scan) (setq scan (cdr scan)))))
    icicle-menu-items-alist)) 

(defun icicle-remove-w32-keybd-accelerators (menu-item)
  "Remove `&' characters that define keyboard accelerators in MS Windows.
\"&&\" is an escaped `&' - it is replaced by a single `&'.
This is a candidate value for `icicle-convert-menu-item-function'."
  (replace-regexp-in-string "&&?" 'icicle-escape-w32-accel menu-item))

(defun icicle-escape-w32-accel (match-string)
  "If STRING is \"&&\", then return \"&\".  Else return \"\"."
  (if (> (length match-string) 1) "&" ""))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-menu.el ends here
