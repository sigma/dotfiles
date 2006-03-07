;;; icicles-opt.el --- User options (variables) for Icicles
;; 
;; Filename: icicles-opt.el
;; Description: User options (variables) for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:22:14 2006
;; Version: 22.0
;; Last-Updated: Sun Mar 05 22:31:59 2006 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 57
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-opt.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;; 
;; Features that might be required by this library:
;;
;;   `cl', `color-theme', `cus-face', `easymenu', `hexrgb',
;;   `thingatpt', `thingatpt+', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  This is a helper library for library `icicles.el'.  It defines
;;  user options (variables).  See `icicles.el' for documentation.
;; 
;;  User options defined here (in Custom group `icicles'):
;;
;;    `icicle-buffer-extras', `icicle-buffer-match-regexp',
;;    `icicle-buffer-no-match-regexp', `icicle-buffer-predicate',
;;    `icicle-buffer-require-match-flag' `icicle-buffer-sort',
;;    `icicle-change-region-background-flag',
;;    `icicle-completion-nospace-flag',
;;    `icicle-cycle-into-subdirs-flag',
;;    `icicle-default-thing-insertion'
;;    `icicle-incremental-completion-flag',
;;    `icicle-inhibit-reminder-prompt-flag', `icicle-init-value-flag',
;;    `icicle-list-join-string', `icicle-sort-function',
;;    `icicle-mark-position-in-candidate',
;;    `icicle-minibuffer-setup-hook',
;;    `icicle-point-position-in-candidate',
;;    `icicle-redefine-standard-commands-flag',
;;    `icicle-regexp-search-ring-max', `icicle-region-background',
;;    `icicle-require-match-flag', `icicle-search-ring-max',
;;    `icicle-show-Completions-initially-flag',
;;    `icicle-thing-at-point-functions',
;;    `icicle-touche-pas-aux-menus-flag',
;;    `icicle-word-completion-key'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 2006/03/05 dadams
;;     Moved from here to icicle-mode.el: icicle-mode, icicle-mode-hook.
;;     Added: icicle-touche-pas-aux-menus-flag.
;; 2006/03/03 dadams
;;     icicle-list-join-string: Changed value to ^G^J.  Clarified doc string.
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

(when (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when, unless

(require 'hexrgb nil t)     ;; (no error if not found): hexrgb-color-values-to-hex,
                            ;; hexrgb-increment-(red|green|blue), hexrgb-rgb-to-hsv,
                            ;; hexrgb-color-values-to-hex, hexrgb-hsv-to-rgb
(require 'thingatpt)        ;; symbol-at-point, thing-at-point, thing-at-point-url-at-point, 
(require 'thingatpt+ nil t) ;; (no error if not found): symbol-name-nearest-point,
                            ;; word-nearest-point

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; User Options (alphabetical, except for dependencies) ---

;;;###autoload
(defcustom icicle-buffer-extras nil
  "*List of additional buffer-name candidates added to the normal list.
List elements are strings."
  :type '(repeat string) :group 'icicles)

;;;###autoload
(defcustom icicle-buffer-match-regexp nil
  "*Nil or a regexp that buffer-name completion candidates must match.
If nil, then this does nothing.  If a regexp, then show only
candidates that match it (and match the user input).
See also `icicle-buffer-no-match-regexp'."
  :type '(choice (const :tag "None" nil) regexp) :group 'icicles)

;;;###autoload
(defcustom icicle-buffer-no-match-regexp nil
  "*Nil or a regexp that buffer-name completion candidates must not match.
If nil, then this does nothing.  If a regexp, then show only
candidates that do not match it.
See also `icicle-buffer-match-regexp'."
  :type '(choice (const :tag "None" nil) regexp) :group 'icicles)

;;;###autoload
(defcustom icicle-buffer-predicate nil
  "*Nil or a predicate that buffer-name candidates must satisfy.
If nil, then this does nothing.  Otherwise, this is a function of one
argument, a candidate, and only candidates that satisfy the predicate
are displayed.  For example, this value will show only buffers that
are associated with files:

  (lambda (bufname) (buffer-file-name (get-buffer bufname)))."
  :type '(choice (const :tag "None" nil) function) :group 'icicles)

;;;###autoload
(defcustom icicle-buffer-require-match-flag nil
  "*Override `icicle-require-match-flag' for `icicle-buffer*' commands.
The possible values are as follows:
- nil means this option imposes nothing on completion;
  the REQUIRE-MATCH argument provided to the function governs behavior
- `no-match-required' means the same as a nil value for REQUIRE-MATCH
- `partial-match-ok' means the same as a t value for REQUIRE-MATCH
- `full-match-required' means the same as a non-nil, non-t value for
  REQUIRE-MATCH

Note: This option is provided mainly for use (binding) in
      `icicle-define-command' and `icicle-define-file-command'.
      You probably do not want to set this globally, but you can."
  :type '(choice
          (const :tag "Do not impose any match behavior"  nil)
          (const :tag "Do not require a match"            no-match-required)
          (const :tag "Require a partial match, with RET" partial-match-ok)
          (const :tag "Require a full match"              full-match-required))
  :group 'icicles)

;;;###autoload
(defcustom icicle-buffer-sort nil
  "*Nil or a sort function for buffer names.
Examples of sort functions are `icicle-buffer-sort-*...*-last' and
`string<'.  If nil, then buffer names are not sorted.  Option
`icicle-sort-function' is bound to `icicle-buffer-sort' by command
`icicle-buffer'."
  :type 'function :group 'icicles)

;; Replace this list by your favorite color themes. Each must be the name of a defined function.
;; By default, this includes all color themes defined globally (variable `color-themes').
;;;###autoload
(defcustom icicle-color-themes
  (and (require 'color-theme nil t)
       (delq 'bury-buffer
             (mapcar (lambda (entry) (list (symbol-name (car entry)))) color-themes)))
  "*List of color themes to cycle through using `M-x icicle-color-theme'."
  :type 'hook :group 'icicles)

;;;###autoload
(defcustom icicle-completion-nospace-flag t
  "*Non-nil means ignore completion candidates that start with a space
unless the input to be completed also starts with a space.
This corresponds roughly to the NOSPACE argument to `all-completions'.
Note: Some Icicles functionalities ignore the value of this option."
  :type 'boolean :group 'icicles)

;;;###autoload
(defcustom icicle-Completions-frame-at-right-flag t
  "*Non-nil means move *Completions* frame to right edge of display.
This is done by `icicle-candidate-action'.
It only happens if *Completions* is alone in its frame.
This can be useful to make *Completions* more visible."
  :type 'boolean :group 'icicles)

;;;###autoload
(defcustom icicle-cycle-into-subdirs-flag nil
  "*Non-nil means minibuffer-input cycling explores subdirectories.
If this is non-nil, then you might want to use a function such as
`icicle-sort-dirs-last' for option `icicle-sort-function', to prevent
cycling into subdirectories depth first."
  :type 'boolean :group 'icicles)

;;;###autoload
(defcustom icicle-default-thing-insertion 'more-of-the-same
  "*Behavior of successive `\\<minibuffer-local-map>\\[icicle-insert-string-at-point]'.
If `alternatives', then the next function in the `car' of
`icicle-thing-at-point-functions' is used to retrieve the text to be
inserted.
If `more-of-the-same', then the function that is the `cdr' of
`icicle-thing-at-point-functions' is used to retrieve the text to be
inserted."
  :type '(choice
          (const :tag "Successive calls to `\\<minibuffer-local-map>\
\\[icicle-insert-string-at-point]' use different text-grabbing functions."
           alternatives)
          (const :tag "Successive calls to `\\<minibuffer-local-map>\
\\[icicle-insert-string-at-point]' grab more text at point."
           more-of-the-same))
  :group 'icicles)

;;;###autoload
(defcustom icicle-incremental-completion-flag t
  "*Non-nil means update *Completions* buffer incrementally, as you type.
t means do nothing if *Completions* is not already displayed.
Non-nil and non-t means display *Completions* and update it.

You can toggle this between t and nil using command
`icicle-toggle-incremental-completion', bound to `C-^' in the
minibuffer."
  :type 'boolean :group 'icicles)

;;;###autoload
(defcustom icicle-inhibit-reminder-prompt-flag nil
  "*Non-nil means do not add reminder to Icicles prompt.
Nil means add a reminder like this: (<S-tab>, TAB: list, C-h: help),
if space permits."
  :type 'boolean :group 'icicles)

;;;###autoload
(defcustom icicle-init-value-flag nil
  "*Non-nil means to use default value as init value when reading input.
This is used by `completing-read', `read-file-name', `read-string',
and `read-from-minibuffer'.  When the default-value argument to one of
these functions is non-nil and the initial-input argument is nil or
\"\", the default value is inserted in the minibuffer as the initial
input.

This has the advantage of not requiring you to use `M-n' to retrieve
the default value.  It has the disadvantage of making you empty the
minibuffer if you do not want to use or edit the default value.

The particular non-nil value determines whether or not the value is
preselected and, if preselected, where the cursor is left: at the
beginning or end of the value.  Possible values:

  nil               - Do not insert default value.
  `insert'          - Insert default value (leave cursor at end).
  `preselect-start' - Insert and preselect default value;
                      leave cursor at beginning.
  `preselect-end'   - Insert and preselect default value;
                      leave cursor at end.

My own preference is `insert'.  This is not the value by default only
because people are not used to it.  I recommend that you try `insert'
for a while, before giving up on it.

Preselection can be useful in Delete Selection mode or PC Selection
mode.  It makes it easy to replace the value by typing characters, or
delete it by hitting `C-d' or `DEL' (backspace).  However, all of the
initial input is lost if you type or hit `C-d' or `DEL'.  That is
inconvenient if you want to keep most of it and edit it only slightly."
  :type '(choice
          (const :tag "Do not insert default value as initial value"     nil)
          (const :tag "Insert (and leave cursor at end)"                 insert)
          (const :tag "Insert, preselect, and leave cursor at beginning" preselect-start)
          (const :tag "Insert, preselect, and leave cursor at end"       preselect-end))
  :group 'icicles)

;;;###autoload
(defcustom icicle-list-join-string "
"
  "*String joining items in a completion that is a list of strings.
When a completion candidate is a list of strings, this string is used
to join the strings in the list, for display and matching purposes.
When completing input, you type regexps that match the strings,
separating them pairwise by the value of `icicle-list-join-string'.
Actually, what you enter is interpreted as a single regexp to be
matched against the joined strings.  Typically, the candidate list
contains two strings: a name and its doc string.

A good value for this option is a string that:
 1) does not normally occur in doc strings,
 2) visually separates the two strings it joins, and
 3) is not too difficult or too long to type.

The default value is \"^G\^J\", that is, control-g followed by
control-j (newline):
 1) ^G does not normally occur in doc strings
 2) a newline visually separates the multiple component strings, which
    helps readability in buffer *Completions*
 3) you can type the value using `C-q C-g C-q C-j'.")

;;;###autoload
(defcustom icicle-mark-position-in-candidate 'input-end
  "*Position of mark when you cycle through completion candidates.
Possible values are those for `icicle-point-position-in-candidate'."
  :type '(choice
          (const :tag "Leave mark at the beginning of the minibuffer input" input-start)
          (const :tag "Leave mark at the end of the minibuffer input" input-end)
          (const :tag "Leave mark at the beginning of the completion root" root-start)
          (const :tag "Leave mark at the end of the completion root" root-end))
  :group 'icicles)

;; Inspired from `icomplete-minibuffer-setup-hook'.
;;;###autoload
(defcustom icicle-minibuffer-setup-hook nil
  "*Functions run at the end of minibuffer setup for Icicle mode."
  :type 'hook :group 'icicles)

;;;###autoload
(defcustom icicle-point-position-in-candidate 'root-end
  "*Position of cursor when you cycle through completion candidates.
Possible values are:
 `input-start': beginning of the minibuffer input
 `input-end':   end of the minibuffer input
 `root-start':  beginning of the completion root
 `root-end':    end of the completion root
When input is expected to be a file name, `input-start' is just after
the directory, which is added automatically during completion cycling.
See also `icicle-mark-position-in-candidate'."
  :type '(choice
          (const :tag "Leave cursor at the beginning of the minibuffer input" input-start)
          (const :tag "Leave cursor at the end of the minibuffer input" input-end)
          (const :tag "Leave cursor at the beginning of the completion root" root-start)
          (const :tag "Leave cursor at the end of the completion root" root-end))
  :group 'icicles)

;;;###autoload
(defcustom icicle-change-region-background-flag
  (and (not (eq icicle-point-position-in-candidate icicle-mark-position-in-candidate))
       (boundp 'delete-selection-mode)
       delete-selection-mode)
  "*Non-nil means the region background is changed during input.
The background is changed to differ only slightly from the minibuffer
background, by default.  The actual region background color used is
`icicle-region-background'"
  :type 'boolean :group 'icicles)

;; This is essentially a version of `doremi-increment-color-component' for value only.
(defun icicle-increment-color-value (color increment)
  "Increase value component (brightness) of COLOR by INCREMENT."
  (unless (string-match "#" color)      ; Convert color name to #hhh...
    (setq color (hexrgb-color-values-to-hex (x-color-values color))))
  ;; Convert RGB to HSV
  (let* ((rgb (x-color-values color))
         (red   (/ (float (nth 0 rgb)) 65535.0)) ; Convert from 0-65535 to 0.0-1.0
         (green (/ (float (nth 1 rgb)) 65535.0))
         (blue  (/ (float (nth 2 rgb)) 65535.0))
         (hsv (hexrgb-rgb-to-hsv red green blue))
         (hue        (nth 0 hsv))
         (saturation (nth 1 hsv))
         (value      (nth 2 hsv)))
    (setq value (+ value (/ increment 100.0)))
    (when (> value 1.0) (setq value (1- value)))
    (hexrgb-color-values-to-hex (mapcar (lambda (x) (floor (* x 65535.0)))
                                        (hexrgb-hsv-to-rgb hue saturation value)))))

;; This is essentially a version of `doremi-increment-color-component' for hue only.
(defun icicle-increment-color-hue (color increment)
  "Increase hue component of COLOR by INCREMENT."
  (unless (string-match "#" color)      ; Convert color name to #hhh...
    (setq color (hexrgb-color-values-to-hex (x-color-values color))))
  ;; Convert RGB to HSV
  (let* ((rgb (x-color-values color))
         (red   (/ (float (nth 0 rgb)) 65535.0)) ; Convert from 0-65535 to 0.0-1.0
         (green (/ (float (nth 1 rgb)) 65535.0))
         (blue  (/ (float (nth 2 rgb)) 65535.0))
         (hsv (hexrgb-rgb-to-hsv red green blue))
         (hue        (nth 0 hsv))
         (saturation (nth 1 hsv))
         (value      (nth 2 hsv)))
    (setq hue (+ hue (/ increment 100.0)))
    (when (> hue 1.0) (setq hue (1- hue)))
    (hexrgb-color-values-to-hex (mapcar (lambda (x) (floor (* x 65535.0)))
                                        (hexrgb-hsv-to-rgb hue saturation value)))))

;;;###autoload
(defcustom icicle-redefine-standard-commands-flag t
  "*Non-nil means Icicle mode redefines some standard Emacs commands."
  :type 'boolean :group 'icicles)

;; You can use `icicle-increment-color-value' in place of `icicle-increment-color-hue', if you
;; prefer highlighting background to be slightly darker instead of a slightly different hue.
;;
;;;###autoload
(defcustom icicle-region-background
  (if (featurep 'hexrgb)
      (icicle-increment-color-hue     ; Use a slightly different hue than frame background.
       (or (and (boundp '1on1-active-minibuffer-frame-background)
                1on1-active-minibuffer-frame-background) ; In `oneonone.el'.
           (cdr (assq 'background-color (frame-parameters)))
           (face-background 'region))
       24)
    (cdr (assq 'background-color (frame-parameters)))) ; Invisible, if no `hexrgb.el'.
  "*Background color to use for region during minibuffer cycling."
  :type 'string :group 'icicles)

;;;###autoload
(defcustom icicle-require-match-flag nil
  "*Control REQUIRE-MATCH arg to `completing-read' and `read-file-name'.
The possible values are as follows:
- nil means this option imposes nothing on completion;
  the REQUIRE-MATCH argument provided to the function governs behavior
- `no-match-required' means the same as a nil value for REQUIRE-MATCH
- `partial-match-ok' means the same as a t value for REQUIRE-MATCH
- `full-match-required' means the same as a non-nil, non-t value for
  REQUIRE-MATCH

Note: This option is provided mainly for use (binding) in
      `icicle-define-command' and `icicle-define-file-command'.
      You probably do not want to set this globally, but you can."
  :type '(choice
          (const :tag "Do not impose any match behavior"  nil)
          (const :tag "Do not require a match"            no-match-required)
          (const :tag "Require a partial match, with RET" partial-match-ok)
          (const :tag "Require a full match"              full-match-required))
  :group 'icicles)

;;;###autoload
(defcustom icicle-search-hook nil
  "*List of hook functions run by `icicle-search' (see `run-hooks')."
  :type 'hook :group 'icicles)

;;;###autoload
(defcustom icicle-search-ring-max (if (boundp 'most-positive-fixnum)
                                      most-positive-fixnum
                                    67108863) ; 1/2 of `most-positive-fixnum' on Windows.
  "*Icicles version of `search-ring-max'."
  :type 'integer :group 'icicles)

;;;###autoload
(defcustom icicle-regexp-search-ring-max (if (boundp 'most-positive-fixnum)
                                             most-positive-fixnum
                                           67108863) ; 1/2 of `most-positive-fixnum' on Windows.
  "*Icicles version of `regexp-search-ring-max'."
  :type 'integer :group 'icicles)

;;;###autoload
(defcustom icicle-show-Completions-initially-flag nil
  "*Non-nil means to show buffer *Completions* with no user input.
nil means that *Completions* is shown upon demand, via `TAB' or
`S-TAB'.

Alternatively, you can set option `icicle-incremental-completion-flag'
to a value that is neither nil nor t.  That will display buffer
*Completions* as soon as you type or delete input (but not
initially)."
  :type 'boolean :group 'icicles)

;;;###autoload
(defcustom icicle-sort-function 'string-lessp
  "*Comparison function passed to `sort', to sort completion candidates.
This sorting determines the order of candidates when cycling and their
order in buffer *Completions*.  If the value nil, no sorting is done.

When `icicle-cycle-into-subdirs-flag' is non-nil, you might want to
use a function such as `icicle-sort-dirs-last' for this option, to
prevent cycling into subdirectories depth first.

You can toggle sorting at any time using command
`icicle-toggle-sorting', bound to `C-,' in the minibuffer"
  :type 'function :group 'icicles)

;;;###autoload
(defcustom icicle-buffer-configs
  `(("All" nil nil nil nil ,icicle-sort-function)
    ("Files" nil nil (lambda (bufname) (buffer-file-name (get-buffer bufname))) nil
     ,icicle-sort-function)
    ("Files and Scratch" nil nil (lambda (bufname) (buffer-file-name (get-buffer bufname)))
     ("*scratch*") ,icicle-sort-function)
    ("All, *...* Buffers Last" nil nil nil nil icicle-buffer-sort-*...*-last))
  "*List of option configurations available for `icicle-buffer-config'.
The form is (CONFIG...), where CONFIG is a list of these items:

 - Configuration name                    (string)
 - `icicle-buffer-match-regexp' value    (regexp string)
 - `icicle-buffer-no-match-regexp' value (regexp string)
 - `icicle-buffer-predicate' value       (function)
 - `icicle-buffer-extras' value          (list of strings)
 - `icicle-buffer-sort' value            (function)

A configuration describes which buffer names are displayed during
completion and their order."
  :type '(repeat (list
                  string                ; Configuration name
                  (choice (const :tag "None" nil) string) ; Match regexp
                  (choice (const :tag "None" nil) string) ; No-match regexp
                  (choice (const :tag "None" nil) function) ; Predicate
                  (choice (const :tag "None" nil) (repeat string)) ; Extras list
                  (choice (const :tag "None" nil) function))) ; Sort function
  :group 'icicles)

(defun icicle-buffer-sort-*...*-last (buf1 buf2)
  "Returns non-nil if BUF1 is `string<' BUF2 or only BUF2 starts with \"*\"."
  (let ((b1 (if completion-ignore-case (downcase buf1) buf1))
        (b2 (if completion-ignore-case (downcase buf2) buf2)))
    (if (string-match "^\\*" b1)
        (and (string-match "^\\*" b2) (string< b1 b2))
      (or (string-match "^\\*" b2) (string< b1 b2)))))

;;;###autoload
(defcustom icicle-thing-at-point-functions
  (cons
   ;; Lisp symbol or file name, word, url.
   (list
    (if (fboundp 'symbol-name-nearest-point)
        'symbol-name-nearest-point
      (lambda () (symbol-name (symbol-at-point))))
    (if (fboundp 'word-nearest-point)
        'word-nearest-point
      (lambda () (thing-at-point 'word)))
    'thing-at-point-url-at-point)
   'forward-word)
  "*Functions that return a string at or near the cursor.
This is a cons cell whose car and cdr may each be empty.

The car of the cons cell is a list of functions that grab different
kinds of strings at or near point.  By default, there are three
functions, which grab 1) the symbol or file name, 2) the word, 3) the
URL at point.  Any number of functions can be used.  They are used in
sequence by command `icicle-insert-string-at-point'.

The cdr of the cons cell is nil or a function that advances point one
text thing.  Each time command `icicle-insert-string-at-point' is
called successively, this is called to grab more things of text (of
the same kind).  By default, successive words are grabbed.

If either the car or cdr is empty, then the other alone determines the
behavior of `icicle-insert-string-at-point'.  Otherwise, option
`icicle-default-thing-insertion' determines whether the car or cdr is
used by `icicle-insert-string-at-point'.  `C-u' with no number
reverses the meaning of `icicle-default-thing-insertion'."
  :type
  '(cons 
    (choice
     (repeat function :tag "Function to grab some text at point and insert it in minibuffer")
     (const :tag "No alternative text-grabbing functions" nil))
    (choice 
     (const :tag "No function to successively grabs more text" nil)
     function :tag "Function to advance point one text thing"))
  :group 'icicles)

;;;###autoload
(defcustom icicle-touche-pas-aux-menus-flag nil
  "*Non-nil means do not add items to menus except Minibuf and Icicles.
This value is used only when Icicles mode is initially established, so
changing this has no effect after Icicles has been loaded.  However,
you can change it and save the new value so it will be used next time."
  :type 'boolean :group 'icicles)

;;;###autoload
(defcustom icicle-word-completion-key [(meta ?\ )]
  "*Key sequence to use for minibuffer word completion.
The value has the same form as a key-sequence arg to `define-key'.

Because file names, in particular, can contain spaces, some people
prefer this to be a non-printable key sequence, such as `M-SPC'.  This
is the default value in Icicles.

But because the spacebar is such a convenient key to hit, other people
prefer to use `SPC' for word completion, and to insert a space some
other way.  The usual way to do that is via `C-q SPC', but command
`icicle-insert-a-space' is provided for convenience.  You can bind
this to `M-SPC', for instance, in `minibuffer-local-completion-map',
`minibuffer-local-completion-map', and
`minibuffer-local-must-match-map'."
  :type 'sexp :group 'icicles)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-opt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-opt.el ends here
