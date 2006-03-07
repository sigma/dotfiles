;;; icicles-var.el --- Internal variables for Icicles
;; 
;; Filename: icicles-var.el
;; Description: Internal variables for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:23:26 2006
;; Version: 22.0
;; Last-Updated: Sun Mar 05 16:51:06 2006 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 119
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-var.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;; 
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `bookmark', `cl', `color-theme',
;;   `cus-face', `easymenu', `hexrgb', `icicles-opt', `pp',
;;   `recentf', `thingatpt', `thingatpt+', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  This is a helper library for library `icicles.el'.  It defines
;;  internal variables (not to be modified by users.  See `icicles.el'
;;  for documentation.
;; 
;;  Variables defined here:
;;
;;    `icicle-candidate-nb', `icicle-cmd-calling-for-completion',
;;    `icicle-complete-input-overlay', `icicle-completion-candidates'
;;    `icicle-completion-help-string',
;;    `icicle-current-completion-candidate-overlay',
;;    `icicle-current-input', `icicle-default-directory',
;;    `icicle-default-thing-insertion-flipped-p',
;;    `icicle-extra-candidates', `icicle-icicle-completing-p',
;;    `icicle-icompleting-p', `icicle-ignored-extensions',
;;    `icicle-ignored-extensions-regexp',
;;    `icicle-incremental-completion-p', `icicle-initial-value',
;;    `icicle-insert-string-at-pt-end',
;;    `icicle-insert-string-at-pt-start',
;;    `icicle-last-completion-candidate',
;;    `icicle-last-completion-command', `icicle-last-input',
;;    `icicle-last-sort-function', `icicle-menu-items-alist',
;;    `icicle-must-match-regexp', `icicle-must-not-match-regexp',
;;    `icicle-must-pass-predicate',
;;    `icicle-nb-of-other-cycle-candidates',
;;    `icicle-post-command-hook', `icicle-pre-command-hook',
;;    `icicle-prompt', `icicle-prompt-suffix',
;;    `icicle-saved-completion-candidates',
;;    `icicle-saved-ignored-extensions',
;;    `icicle-saved-regexp-search-ring-max',
;;    `icicle-saved-region-background',
;;    `icicle-saved-search-ring-max', `icicle-successive-grab-count',
;;    `icicle-thing-at-pt-fns-pointer'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 2006/03/05 dadams
;;     Moved to icicles-mode.el: icicle-mode-map.
;; 2006/03/04 dadams
;;     Moved options stuff to Options menu, when available.
;;     Moved apropos stuff to Apropos menu, when available.
;;     Moved describe stuff to Describe menu, when available.
;; 2006/03/03 dadams 
;;     Added to Icicles menu: icicle-complete-thesaurus-entry, icicle-apropos*,
;;       option-setting cmds, buffer-config cmds icicle-(var|fun)doc.
;;     Require apropos-fn+var.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(when (< emacs-major-version 21)     ;; for Emacs < 21: push
  (eval-when-compile (require 'cl))) ;; for Emacs < 20: when, unless

(require 'apropos-fn+var nil t) ;; (no error if not found): apropos-command, apropos-function,
                                ;; apropos-option, apropos-variable
(require 'icicles-opt) ;; icicle-sort-function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Internal variables (alphabetical) ----------------------

;; These two are defined here so they won't raise an error in `font-lock-add-keywords'.
(defvar font-lock-function-name-face 'font-lock-function-name-face ; Defined in `font-lock.el'.
  "Face name to use for function names.")

(defvar font-lock-keyword-face 'font-lock-keyword-face ; Defined in `font-lock.el'.
  "Face name to use for keywords.")

(defvar icicle-icompleting-p nil
  "Internal flag: non-nil when editing text in minibuffer.
This is really non-nil when inside simple character-editing commands
such as `icicle-self-insert' and `icicle-delete-backward-char'.")

(defvar icicle-candidate-action-fn nil
  "Function to be applied to current completion candidate.
For `icicle-all-candidates-action' to be able to report successes,
this should return non-nil for \"success\" and nil for \"failure\".")

(defvar icicle-candidate-nb nil
  "Current completion candidate number, or nil if not cycling candidates.
Numbering starts at zero.")

(defvar icicle-cmd-calling-for-completion 'ignore
  "Last command causing display of list of possible completions.")

(defvar icicle-completion-candidates nil "Current list of completion candidates.")

(defvar icicle-completion-help-string ""
  "Description of minibuffer bindings.")

(defvar icicle-extra-candidates nil
  "A list of extra completion candidates (strings).")

(defvar icicle-current-completion-candidate-overlay nil
  "Overlay used to highlight current completion candidate.")

(defvar icicle-complete-input-overlay nil
  "Overlay used to highlight minibuffer input when it is complete.")

(defvar icicle-current-input "" "Current minibuffer input.")

(defvar icicle-default-directory default-directory
  "Local copy of `default-directory'.
Set whenever minibuffer is entered or input is completed.")

(defvar icicle-default-thing-insertion-flipped-p nil
  "Non-nil means a previous `M-.' in this succession was used with `C-u'.
This means that the meaning of `icicle-default-thing-insertion' has
been reversed.")

(defvar icicle-icicle-completing-p nil
  "Non-nil means that input currently uses Icicles completion.
This is used to distinguish direct calls to `old-completing-read' and
`old-read-file-name' from calls to them from the Icicles versions.
Such direct calls occur, for instance, from an `interactive' spec,
such as `(interactive \"bBuffer: \")'.")

(defvar icicle-incremental-completion-p nil
  "Takes the place of `icicle-incremental-completion-flag' during input.
The program updates this to `always' from `t' after *Completions* has
been displayed.")

(defvar icicle-ignored-extensions completion-ignored-extensions
  "Copy of `completion-ignored-extensions', serving as a control flag.
When `completion-ignored-extensions' changes, we remake
`icicle-ignored-extensions-regexp'.")

(defvar icicle-ignored-extensions-regexp
  (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "\\|")
          "\\)\\'")
  "Regular expression matching ignored file extensions.
If this is nil, then no file extensions are ignored.
The ignored file extensions come from `completion-ignored-extensions'.")

(defvar icicle-initial-value ""
  "Initial value used in minibuffer completion.
Any function that reads from the minibuffer and accepts a default
value or initial value should, before reading, put that value in
`icicle-initial-value'.  For example, `completing-read' does that.")

(defvar icicle-insert-string-at-pt-start 0
  "Position of start of text `icicle-insert-string-at-point' inserted.")

(defvar icicle-insert-string-at-pt-end 0
  "Position of end of text `icicle-insert-string-at-point' inserted.")

(defvar icicle-last-completion-candidate ""
  "Last completion candidate used in minibuffer completion.")

;; This is used to be able to ignore `handle-switch-frame'.
(defvar icicle-last-completion-command nil "Last completion command used.")

(defvar icicle-last-input "" "Last minibuffer input.")

(defvar icicle-last-sort-function (or icicle-sort-function 'string-lessp)
  "Local copy of `icicle-sort-function', so we can restore it.")

(defvar icicle-menu-items-alist nil)    ; Defined in `icicles-menu.el'.

(defvar icicle-must-match-regexp nil
  "Nil or a regexp that completion candidates must match.
If nil, then this does nothing.  If a regexp (string), then show only
candidates that match it (and match the user input).
See also `icicle-must-not-match-regexp'.")

(defvar icicle-must-not-match-regexp nil
  "Nil or a regexp that completion candidates must not match.
If nil, then this does nothing.  If a regexp (string), then show only
candidates that do not match it.
See also `icicle-must-match-regexp'.")

(defvar icicle-must-pass-predicate nil
  "Nil or a predicate that completion candidates must satisfy.
If nil, then this does nothing.  Otherwise, this is a function of one
argument, a candidate, and only candidates that satisfy the predicate
are displayed.")

(defvar icicle-nb-of-other-cycle-candidates 0
  "Number of other candidates available for cycling.
This is for use by other libraries, in particular, `icomplete+.el'.")

;; Inspired from `icomplete-post-command-hook'.
(defvar icicle-post-command-hook nil
  "Functions added to `post-command-hook' when in Icicle mode.
Use command `icy-mode' (aka `icicle-mode') to set this up properly.")

;; Inspired from `icomplete-pre-command-hook'.  There is none, by default.
(defvar icicle-pre-command-hook nil
  "Functions added to `pre-command-hook' when in Icicle mode.
Use command `icy-mode' (aka `icicle-mode') to set this up properly.")

(defvar icicle-prompt "")

(defvar icicle-prompt-suffix ""
  "String to append to the input-completion prompt, if there is room.
Intended to remind you how to obtain help on input completion.")

(defvar icicle-saved-completion-candidates nil
  "Completion candidates user saved using `icicle-candidate-set-save'.")

(defvar icicle-saved-ignored-extensions nil
  "Local copy of `icicle-ignored-extensions', so we can restore it.")

(defvar icicle-saved-regexp-search-ring-max regexp-search-ring-max
  "Saved value of `search-ring-max', so it can be restored.")

(defvar icicle-saved-region-background nil
  "Background of `region' face.  Saved so it can be restored.")

(defvar icicle-saved-search-ring-max search-ring-max
  "Saved value of `search-ring-max', so it can be restored.")

(defvar icicle-successive-grab-count 0
  "Number of text things to be grabbed by next `\\<minibuffer-local-map>\
\\[icicle-insert-string-at-point]'.")

(defvar icicle-thing-at-pt-fns-pointer 0
  "Current index into the car of `icicle-thing-at-point-functions'.
This points to the current function in the list.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-var)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-var.el ends here
