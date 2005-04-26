;;; autoloads.el ---

;; Copyright (C) 2004  Free Software Foundation, Inc.

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

;;; Autoloads
(autoload 'boxquote-boxquote "boxquote" "Apply `boxquote-region' to the current boxquote." t nil)
(autoload 'boxquote-buffer "boxquote" "Apply `boxquote-region' to a whole buffer." t nil)
(autoload 'boxquote-defun "boxquote" "Apply `boxquote-region' the current defun." t nil)
(autoload 'boxquote-describe-function "boxquote" "Call `describe-function' and boxquote the output into the current buffer." t nil)
(autoload 'boxquote-describe-key "boxquote" "Call `describe-key' and boxquote the output into the current buffer." t nil)
(autoload 'boxquote-describe-variable "boxquote" "Call `describe-variable' and boxquote the output into the current buffer." t nil)
(autoload 'boxquote-fill-paragraph "boxquote" "Perform a `fill-paragraph' inside a boxquote." t nil)
(autoload 'boxquote-insert-file "boxquote" "Insert the contents of a file, boxed with `boxquote-region'.

If `boxquote-title-files' is non-nil the boxquote will be given a title that
is the result applying `boxquote-file-title-funciton' to FILENAME." t nil)
(autoload 'boxquote-kill "boxquote" "Kill the boxquote and its contents." t nil)
(autoload 'boxquote-kill-ring-save "boxquote" "Like `kill-ring-save' but remembers a title if possible.

The title is acquired by calling `boxquote-kill-ring-save-title'. The title
will be used by `boxquote-yank'." t nil)
(autoload 'boxquote-narrow-to-boxquote "boxquote" "Narrow the buffer to the current boxquote." t nil)
(autoload 'boxquote-narrow-to-boxquote-content "boxquote" "Narrow the buffer to the content of the current boxquote." t nil)
(autoload 'boxquote-paragraph "boxquote" "Apply `boxquote-region' to the current paragraph." t nil)
(autoload 'boxquote-region "boxquote" "Draw a box around the left hand side of a region bounding START and END." t nil)
(autoload 'boxquote-shell-command "boxquote" "Call `shell-command' with COMMAND and boxquote the output." t nil)
(autoload 'boxquote-text "boxquote" "Insert TEXT, boxquoted." t nil)
(autoload 'boxquote-title "boxquote" "Set the title of the current boxquote to TITLE.

If TITLE is an empty string the title is removed. Note that the title will
be formatted using `boxquote-title-format'." t nil)
(autoload 'boxquote-unbox "boxquote" "Remove the boxquote that contains `point'." t nil)
(autoload 'boxquote-unbox-region "boxquote" "Remove a box created with `boxquote-region'." t nil)
(autoload 'boxquote-yank "boxquote" "Do a `yank' and box it in with `boxquote-region'.

If the yanked entry was placed on the kill ring with
`boxquote-kill-ring-save' the resulting boxquote will be titled with
whatever `boxquote-kill-ring-save-title' returned at the time." t nil)
(autoload 'camelCase-mode "camelCase-mode" "Minor mode which overrides word command keys for editing camelCase words.

 Word boundaries in a camelCase name are marked only by letter case.
 For example lowerCapitalUPPERCase has four words.  A word may be
 lowercase, Capitalized, UPPERCASE, or a sequence of digits.  Besides
 non-letter to letter and letter to non-letter word boundaries,
 word boundaries in the middle of a sequence of letters are located at
 lowercaseCapital, CapitalCapital, lowercaseUPPERCASE,
 CapitalUPPERCASE, and UPPERCASECapital boundaries.

 Rebound keys:
   M-f, M-right*,  C-right      camelCase-forward-word
   M-b, M-left*,   C-left       camelCase-backward-word
   M-d, M-delete*, C-delete*    camelCase-forward-kill-word
   M-backspace,    C-backspace* camelCase-backward-kill-word
   M-t                          camelCase-transpose-words
   M-c                          camelCase-capitalize-word
   M-u                          camelCase-upcase-word
   M-l                          camelCase-downcase-word
 (* means only in Gnu Emacs, not in XEMacs; the original binding is not
  to the word command in XEmacs, so it is not overridden)

 camelCase-mode prefix ARG:  0 turns off, 1 turns on, nil toggles mode." t nil)
(autoload 'css-mode "css-mode" "Major mode for editing CSS style sheets.
\\{cssm-mode-map}" t nil)
(autoload 'doxymacs-mode "doxymacs" "mode help" t nil)
(autoload 'ecb-activate "ecb" "Emacs Code Browser" t nil)
(autoload 'expand-member-functions "member-functions" "Expand C++ member function declarations" t)
(autoload 'gnuserv-start "gnuserv-compat" "Allow this Emacs process to be a server for client processes." t)
(autoload 'guile-scheme-mode "guile-scheme" "" t nil)
(autoload 'h4x0r-string "h4x0r" "" t nil)
(autoload 'htmlize-buffer "htmlize" "Provide an html page from the current buffer" t nil)
(autoload 'htmlize-file "htmlize" "Provide an html page from the current file" t nil)
(autoload 'htmlize-many-files "htmlize" "Provide an html page from files" t nil)
(autoload 'htmlize-many-files-dired "htmlize" "Provide an html page from files marked in dired" t nil)
(autoload 'htmlize-region "htmlize" "Provide an html page from the current region" t nil)
(autoload 'isearchb-activate "isearchb" "Activate isearchb" t nil)
(autoload 'keytable "keytable" "Browse key bindings" t nil)
(autoload 'make-regexp "make-regexp" "Return a regexp to match a string item in STRINGS.")
(autoload 'make-regexps "make-regexp" "Return a regexp to REGEXPS.")
(autoload 'map-lines "map-lines" "Map COMMAND over lines matching REGEX." t)
(autoload 'mode-compile "mode-compile" "Command to compile current buffer file based on the major mode" t)
(autoload 'mode-compile-kill "mode-compile" "Command to kill a compilation launched by `mode-compile'" t)
(autoload 'page-break-mode "page-break" "Visible page markers" t nil)
(autoload 'replace-recent-character "rrc" "Replace-recent-character is interactive function for quick corrections of
recenlty typed text. It first prompts for character to search backwards. If
such character is found, following options are shown:
1, repeat the character to search in previous text.
2, M-r for delete of the found character.
3, C-t for trasposition of the found and the following character.
4, TAB for promt for character to insert after the found character.
5, ESC for no operation.
6, Any other insertable character will replace found character." t nil)
(autoload 'rm-exchange-point-and-mark "rect-mark" "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark" "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark" "Copy a rectangular region to the kill ring." t)
(autoload 'rm-set-mark "rect-mark" "Set mark for rectangle." t)
(autoload 'rpm "sb-rpm" "Rpm package listing in speedbar.")
(autoload 'sawfish-mode "sawfish" "sawfish-mode" t)
(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
(autoload 'teyjus "teyjus" "Run an inferior Teyjus process." t)
(autoload 'teyjus-edit-mode "teyjus" "Syntax Highlighting, etc. for Lambda Prolog" t)
(autoload 'turn-on-eldoc-mode "eldoc" "Activate eldoc" t nil)
(autoload 'w3-speedbar-buttons "sb-w3" "s3 specific speedbar button generator.")
(autoload 'zap-following-char "zap-char" "Kill following ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found." t nil)
(autoload 'zap-from-char "zap-char" "Kill from ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found." t nil)
(autoload 'zap-to-char "zap-char" "Kill up to and including ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found." t nil)
(autoload 'zap-upto-char "zap-char" "Kill up to ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found." t nil)
(autoload 'irc "erc-config" "" t nil)
(autoload 'xtla "xtla" "" t nil)
(autoload 'svn-status "psvn-hacked" "" t nil)

(provide 'autoloads)
;;; autoloads.el ends here
