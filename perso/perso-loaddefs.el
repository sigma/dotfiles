;;; loaddefs.el --- automatically extracted autoloads
;; To regenerate this file, simply C-x C-e the following expression
;; (let ((generated-autoload-file "perso-loaddefs.el")) (create-directory-autoloads "."))
;;; Code:
(provide 'perso-loaddefs)

;;;### (autoloads (set-modified-alist modify-alist remove-alist set-alist
;;;;;;  del-alist put-alist vassoc) "alist" "alist.el" (16944 35476))
;;; Generated autoloads from alist.el

(autoload (quote vassoc) "alist" "\
Search VALIST for a vector whose first element is equal to KEY.
See also `assoc'.

\(fn KEY VALIST)" nil nil)

(autoload (quote put-alist) "alist" "\
Modify ALIST to set VALUE to ITEM.
If there is a pair whose car is ITEM, replace its cdr by VALUE.
If there is not such pair, create new pair (ITEM . VALUE) and
return new alist whose car is the new pair and cdr is ALIST.
\[tomo's ELIS like function]

\(fn ITEM VALUE ALIST)" nil nil)

(autoload (quote del-alist) "alist" "\
If there is a pair whose key is ITEM, delete it from ALIST.
\[tomo's ELIS emulating function]

\(fn ITEM ALIST)" nil nil)

(autoload (quote set-alist) "alist" "\
Modify a alist indicated by SYMBOL to set VALUE to ITEM.

\(fn SYMBOL ITEM VALUE)" nil nil)

(autoload (quote remove-alist) "alist" "\
Remove ITEM from the alist indicated by SYMBOL.

\(fn SYMBOL ITEM)" nil nil)

(autoload (quote modify-alist) "alist" "\
Modify alist DEFAULT into alist MODIFIER.

\(fn MODIFIER DEFAULT)" nil nil)

(autoload (quote set-modified-alist) "alist" "\
Modify a value of a symbol SYM into alist MODIFIER.
The symbol SYM should be alist. If it is not bound,
its value regard as nil.

\(fn SYM MODIFIER)" nil nil)

;;;***

;;;### (autoloads (anchored-transpose) "anchored-transpose" "anchored-transpose.el"
;;;;;;  (17025 60014))
;;; Generated autoloads from anchored-transpose.el

(autoload (quote anchored-transpose) "anchored-transpose" "\
Transpose portions of the region around an anchor phrase.

`this phrase but not that word'    can be transposed into
`that word but not this phrase'

I want this phrase but not that word.
       |                            |   This is the entire phrase.
                  |       |             This is the anchor phrase.

First select the entire phrase and type \\[anchored-transpose].  Then select
the anchor phrase and type \\[anchored-transpose] again.  By default the
anchor phrase will automatically include any surrounding whitespace even if
you don't specifically select it.  Also, it won't include certain trailing
punctuation.  See `anchored-transpose-do-fuzzy' for details.

You can select the anchor phrase first followed by the entire phrase if more
convenient.  Typing \\[anchored-transpose] with nothing selected clears any
prior selection.  If both primary and secondary selections are active this
command swaps the 2 selections immediately.

\(fn BEG END FLG &optional BEG2 END2 FLG2)" t nil)

;;;***

;;;### (autoloads (create-directory-autoloads create-file-autoloads)
;;;;;;  "autoloads+" "autoloads+.el" (17029 63453))
;;; Generated autoloads from autoloads+.el

(autoload (quote create-file-autoloads) "autoloads+" "\
Update the autoloads for FILE in `generated-autoload-file'
\(which FILE might bind in its local variables).
If SAVE-AFTER is non-nil (which is always, when called interactively),
save the buffer too.

Return FILE if there was no autoload cookie in it, else nil.

\(fn FILE &optional SAVE-AFTER)" t nil)

(autoload (quote create-directory-autoloads) "autoloads+" "\
Update loaddefs.el with all the current autoloads from DIRS, and no old ones.
This uses `update-file-autoloads' (which see) do its work.
In an interactive call, you must give one argument, the name
of a single directory.  In a call from Lisp, you can supply multiple
directories as separate arguments, but this usage is discouraged.

The function does NOT recursively descend into subdirectories of the
directory or directories specified.

\(fn DIR)" t nil)

;;;***

;;;### (autoloads (babel-as-string babel-region babel) "babel" "babel.el"
;;;;;;  (16944 35476))
;;; Generated autoloads from babel.el

(autoload (quote babel) "babel" "\
Use a web translation service to translate the message MSG.
Display the result in a buffer *babel* unless the optional argument
NO-DISPLAY is nil.

\(fn MSG &optional NO-DISPLAY)" t nil)

(autoload (quote babel-region) "babel" "\
Use a web translation service to translate the current region.

\(fn START END)" t nil)

(autoload (quote babel-as-string) "babel" "\
Use a web translation service to translate MSG, returning a string.

\(fn MSG)" t nil)

;;;***

;;;### (autoloads (bf-test bf-run bf-compile bf-execute) "bf" "bf.el"
;;;;;;  (17029 64580))
;;; Generated autoloads from bf.el

(autoload (quote bf-execute) "bf" "\
BrainFuck interpreter.
This is a internal function which assumes the necessary variables for
a BrainFuck machine are already bound and initialized.

\(fn STRING)" nil nil)

(autoload (quote bf-compile) "bf" "\
BrainFuck compiler.
STRING is the BrainFuck code to be compiled.
This function returns a byte-compiled function with no arguments.
To run the code, simply `funcall' the return value of this function.

\(fn STRING &optional NO-CACHE)" nil nil)

(autoload (quote bf-run) "bf" "\
Not documented

\(fn STRING)" t nil)

(autoload (quote bf-test) "bf" "\
A test for the BrainFuck compiler.
This function compiles a Brainfuck interpreter written in BrainFuck
to native emacs-lisp byte-code and execute the interpreter which itself
executes a simple brainfuck program.
NOTE: The BrainFuck interpreter written in BrainFuck is not written
by me.

\(fn)" t nil)

;;;***

;;;### (autoloads (turn-on-jde-blockcomment-mode turn-on-blockcomment-mode
;;;;;;  jde-blockcomment-mode blockcomment-mode) "block-comm" "block-comm.el"
;;;;;;  (16944 35476))
;;; Generated autoloads from block-comm.el

(autoload (quote blockcomment-mode) "block-comm" "\
Toggle Blockcomment mode.
With a prefix ARG, enable blockcomment mode iff arg is nonzero.

\(fn ARG)" t nil)

(autoload (quote jde-blockcomment-mode) "block-comm" "\
Toggle Blockcomment mode with JDE support.
This will call `jde-javadoc-autodoc-at-line' when a new comment is
created.  With a prefix ARG, enable blockcomment mode iff arg is
nonzero.

\(fn ARG)" t nil)

(autoload (quote turn-on-blockcomment-mode) "block-comm" "\
Turn on Blockcomment mode.
Useful for adding to a major mode hook variable.
Example:
    (add-hook 'c-mode-hook 'turn-on-blockcomment-mode)
to automatically turn on blockcomment mode when opening a C source
file.

\(fn)" nil nil)

(autoload (quote turn-on-jde-blockcomment-mode) "block-comm" "\
Turn on Blockcomment mode with JDE support.
For more information, see `jde-blockcomment-mode'.  Useful for adding
to a major mode hook variable.
Example:
    (add-hook 'jde-mode-hook 'turn-on-blockcomment-mode)

to automatically turn on jde blockcomment mode when opening a JDE source
file.

\(fn)" nil nil)

;;;***

;;;### (autoloads (boxquote-unbox boxquote-unbox-region boxquote-fill-paragraph
;;;;;;  boxquote-kill boxquote-narrow-to-boxquote-content boxquote-narrow-to-boxquote
;;;;;;  boxquote-text boxquote-shell-command boxquote-describe-key
;;;;;;  boxquote-describe-variable boxquote-describe-function boxquote-boxquote
;;;;;;  boxquote-paragraph boxquote-defun boxquote-yank boxquote-kill-ring-save
;;;;;;  boxquote-insert-file boxquote-buffer boxquote-region boxquote-title)
;;;;;;  "boxquote" "boxquote.el" (16944 35476))
;;; Generated autoloads from boxquote.el

(autoload (quote boxquote-title) "boxquote" "\
Set the title of the current boxquote to TITLE.

If TITLE is an empty string the title is removed. Note that the title will
be formatted using `boxquote-title-format'.

\(fn TITLE)" t nil)

(autoload (quote boxquote-region) "boxquote" "\
Draw a box around the left hand side of a region bounding START and END.

\(fn START END)" t nil)

(autoload (quote boxquote-buffer) "boxquote" "\
Apply `boxquote-region' to a whole buffer.

\(fn)" t nil)

(autoload (quote boxquote-insert-file) "boxquote" "\
Insert the contents of a file, boxed with `boxquote-region'.

If `boxquote-title-files' is non-nil the boxquote will be given a title that
is the result applying `boxquote-file-title-funciton' to FILENAME.

\(fn FILENAME)" t nil)

(autoload (quote boxquote-kill-ring-save) "boxquote" "\
Like `kill-ring-save' but remembers a title if possible.

The title is acquired by calling `boxquote-kill-ring-save-title'. The title
will be used by `boxquote-yank'.

\(fn)" t nil)

(autoload (quote boxquote-yank) "boxquote" "\
Do a `yank' and box it in with `boxquote-region'.

If the yanked entry was placed on the kill ring with
`boxquote-kill-ring-save' the resulting boxquote will be titled with
whatever `boxquote-kill-ring-save-title' returned at the time.

\(fn)" t nil)

(autoload (quote boxquote-defun) "boxquote" "\
Apply `boxquote-region' the current defun.

\(fn)" t nil)

(autoload (quote boxquote-paragraph) "boxquote" "\
Apply `boxquote-region' to the current paragraph.

\(fn)" t nil)

(autoload (quote boxquote-boxquote) "boxquote" "\
Apply `boxquote-region' to the current boxquote.

\(fn)" t nil)

(autoload (quote boxquote-describe-function) "boxquote" "\
Call `describe-function' and boxquote the output into the current buffer.

\(fn)" t nil)

(autoload (quote boxquote-describe-variable) "boxquote" "\
Call `describe-variable' and boxquote the output into the current buffer.

\(fn)" t nil)

(autoload (quote boxquote-describe-key) "boxquote" "\
Call `describe-key' and boxquote the output into the current buffer.

\(fn KEY)" t nil)

(autoload (quote boxquote-shell-command) "boxquote" "\
Call `shell-command' with COMMAND and boxquote the output.

\(fn COMMAND)" t nil)

(autoload (quote boxquote-text) "boxquote" "\
Insert TEXT, boxquoted.

\(fn TEXT)" t nil)

(autoload (quote boxquote-narrow-to-boxquote) "boxquote" "\
Narrow the buffer to the current boxquote.

\(fn)" t nil)

(autoload (quote boxquote-narrow-to-boxquote-content) "boxquote" "\
Narrow the buffer to the content of the current boxquote.

\(fn)" t nil)

(autoload (quote boxquote-kill) "boxquote" "\
Kill the boxquote and its contents.

\(fn)" t nil)

(autoload (quote boxquote-fill-paragraph) "boxquote" "\
Perform a `fill-paragraph' inside a boxquote.

\(fn ARG)" t nil)

(autoload (quote boxquote-unbox-region) "boxquote" "\
Remove a box created with `boxquote-region'.

\(fn START END)" t nil)

(autoload (quote boxquote-unbox) "boxquote" "\
Remove the boxquote that contains `point'.

\(fn)" t nil)

;;;***

;;;### (autoloads (bst-mode) "bst" "bst.el" (16944 35476))
;;; Generated autoloads from bst.el

(autoload (quote bst-mode) "bst" "\
Setup for BibTeX style file editing.

\(fn)" t nil)

;;;***

;;;### (autoloads (Buffer-menu-mouse-execute Buffer-menu-mouse-modified
;;;;;;  Buffer-menu-mouse-delete Buffer-menu-mouse-save Buffer-menu-mouse-unmark
;;;;;;  Buffer-menu-mouse-other-window Buffer-menu-mouse-3-menu Buffer-menu-select
;;;;;;  Buffer-menu-execute Buffer-menu-mode buffer-menu) "buff-menu+"
;;;;;;  "buff-menu+.el" (16944 35476))
;;; Generated autoloads from buff-menu+.el

(defvar buffer-menu-font-lock-keywords (quote (("^\\( M.*\\)" 1 buffer-menu-headings-face) ("^\\([.]\\)" 1 buffer-menu-current-buffer-face) ("^\\(>\\)" 1 buffer-menu-view-mark-face) ("^\\(D\\)" 1 buffer-menu-delete-mark-face) ("^.\\(S\\)" 1 buffer-menu-save-mark-face) ("^.\\([*]\\)" 1 buffer-menu-modified-mark-face) ("^..\\(%\\)" 1 buffer-menu-read-only-mark-face) ("^....\\(.+\\)[ 	\n][0-9]" 1 buffer-menu-buffer-name-face) ("^.*[ 	][0-9]+[ 	]+\\([^/\n]+\\)" 1 buffer-menu-mode-face) ("^.*[ 	]\\([0-9]+\\)[ 	]+[^/\n]+" 1 buffer-menu-size-face) ("\\(/.*\\)$" 1 buffer-menu-file-name-face))) "\
Expressions to highlight in Buffer Menu mode.")

(autoload (quote buffer-menu) "buff-menu+" "\
Make a menu of buffers so you can save, delete or select them.
By default (no or null prefix arg), the buffers are listed in order of
last access.  With a non-nil prefix ARG:
  ARG >= 0   => Only buffers visiting files are listed.
  ARG =< 0   => The buffers are listed alphabetically.
 (ARG =  0   => Only buffers visiting files, listed alphabetically.)

Type `?' in buffer \"*Buffer List*\" to get help on available commands.
Type `q' there to quit the buffer menu.

\(fn &optional ARG)" t nil)

(autoload (quote Buffer-menu-mode) "buff-menu+" "\
Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
In Buffer menu mode, chars do not insert themselves, but are commands.
\\<Buffer-menu-mode-map>
\(\"Current line\" here is the line of the text cursor or the mouse.)

Also, pressing `mouse-3' on a buffer name in this mode provides a
popup menu that duplicates most of the functions below.


Display buffers:
---------------
\\[Buffer-menu-mouse-select], \\[Buffer-menu-select] -- Select current line's buffer.
\\[Buffer-menu-mark]	-- Mark current line's buffer `>' to be displayed (via `\\[Buffer-menu-select]').
\\[Buffer-menu-select]	-- Show buffers marked `>'.  Select current line's buffer.
\\[Buffer-menu-1-window]	-- Select current line's buffer (only) in a full-frame window.
\\[Buffer-menu-2-window]	-- Select current line's buffer in one window.
	   Display previous buffer in a second window.
\\[Buffer-menu-switch-other-window]	-- Display current line's buffer in another window. No select.

Mark/Unmark buffers to be Saved/Deleted:
---------------------------------------
\\[Buffer-menu-save]	-- Mark current line's buffer `S' to be saved.    Cursor down.
\\[Buffer-menu-delete]	-- Mark current line's buffer `D' to be deleted.  Cursor down.
\\[Buffer-menu-delete-backwards]	-- Mark current line's buffer `D' to be deleted.  Cursor up.
\\[Buffer-menu-unmark]	-- Unmark current line. Cursor down. (Prefix arg: Cursor up.)
\\[Buffer-menu-backup-unmark]	-- Cursor up, then unmark line.

Save/Delete buffers:
-------------------
\\[Buffer-menu-execute]	-- Save / Delete marked buffers (marks `S', `D').

Miscellaneous:
-------------
\\[Buffer-menu-not-modified]	-- Clear modified-flag on current line's buffer.
\\[Buffer-menu-toggle-read-only]	-- Toggle read-only status of current line's buffer.
\\[Buffer-menu-visit-tags-table]	-- `visit-tags-table' using current line's buffer.


Bindings in Buffer Menu mode:
----------------------------

\\{Buffer-menu-mode-map}

\(fn)" nil nil)

(autoload (quote Buffer-menu-execute) "buff-menu+" "\
Save or delete buffers marked `S' or `D', respectively.
Buffers are so marked using command `\\<Buffer-menu-mode-map>\\[Buffer-menu-save]' or `\\<Buffer-menu-mode-map>\\[Buffer-menu-delete]', respectively.

\(fn)" t nil)

(autoload (quote Buffer-menu-select) "buff-menu+" "\
Select this line's buffer; also display buffers marked with `>'.
You can mark buffers with the \\<Buffer-menu-mode-map>\\[Buffer-menu-mark] command.

\(fn)" t nil)

(autoload (quote Buffer-menu-mouse-3-menu) "buff-menu+" "\
Pop up menu for Mouse-3 for buffer listed in buffer menu.

\(fn EVENT)" t nil)

(autoload (quote Buffer-menu-mouse-other-window) "buff-menu+" "\
Select, in another window, the buffer on whose line you click.

\(fn EVENT)" t nil)

(autoload (quote Buffer-menu-mouse-unmark) "buff-menu+" "\
Cancel all requested operations on buffer.

\(fn EVENT)" t nil)

(autoload (quote Buffer-menu-mouse-save) "buff-menu+" "\
Mark buffer to be saved.
Actual deletion is done via `\\<Buffer-menu-mode-map>\\[Buffer-menu-execute]' or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-execute]'.

\(fn EVENT)" t nil)

(autoload (quote Buffer-menu-mouse-delete) "buff-menu+" "\
Mark buffer to be deleted.
Actual deletion is done via `\\<Buffer-menu-mode-map>\\[Buffer-menu-execute]' or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-execute]'.

\(fn EVENT)" t nil)

(autoload (quote Buffer-menu-mouse-modified) "buff-menu+" "\
Mark buffer as unmodified (no changes to save) if modified, and vice versa.

\(fn EVENT)" t nil)

(autoload (quote Buffer-menu-mouse-execute) "buff-menu+" "\
Save and/or delete buffers marked `S' or `D', respectively.
Buffers can be marked via commands `\\<Buffer-menu-mode-map>\\[Buffer-menu-save]' and `\\<Buffer-menu-mode-map>\\[Buffer-menu-delete]'
\(or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-save]' and `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-delete]').

\(fn EVENT)" t nil)

;;;***

;;;### (autoloads (camelCase-mode) "camelCase-mode" "camelCase-mode.el"
;;;;;;  (17029 62224))
;;; Generated autoloads from camelCase-mode.el

(autoload (quote camelCase-mode) "camelCase-mode" "\
Minor mode which overrides word command keys for editing camelCase words.

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

 camelCase-mode prefix ARG:  0 turns off, 1 turns on, nil toggles mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (cparen-activate) "cparen" "cparen.el" (17031 46603))
;;; Generated autoloads from cparen.el

(autoload (quote cparen-activate) "cparen" "\
Activate coloured parentheses in Lisp modes.
You should also enable `font-lock-mode'.
Please note that there is currently no way to disable cparen, except
by turning `font-lock-mode' off completely.
Also, this function affects only buffers created after it was run;
use \\[normal-mode] after this to enable the colours in an existing
buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (dabbrev-hover-install dabbrev-hover-start dabbrev-hover-introduction)
;;;;;;  "dabbrev-hover" "dabbrev-hover.el" (16944 35476))
;;; Generated autoloads from dabbrev-hover.el

(autoload (quote dabbrev-hover-introduction) "dabbrev-hover" "\
Provides electric help from variable `dabbrev-hover-introduction'.

\(fn)" t nil)

(autoload (quote dabbrev-hover-start) "dabbrev-hover" "\
Not documented

\(fn)" nil nil)

(autoload (quote dabbrev-hover-install) "dabbrev-hover" "\
Not documented

\(fn &optional GLOBALP FANCYP)" t nil)

;;;***

;;;### (autoloads (define-face-const) "def-face-const" "def-face-const.el"
;;;;;;  (17031 46603))
;;; Generated autoloads from def-face-const.el

(autoload (quote define-face-const) "def-face-const" "\
Define a constant variable (via `defconst') naming a new face.
FOREGROUND is either nil or a string naming the new face's foreground color.
BACKGROUND is either nil or a string naming the new face's background color.

FOREGROUND (or BACKGROUND) nil means do not set the foreground (or the
BACKGROUND).  If both are nil, the new variable's value is nil.
The value of the new variable (new face or nil) is returned.

Only colors (strings) satisfying `x-color-defined-p' are accepted.
\"Black\" is used in place of any unacceptable foreground color name.
\"White\" is used in place of any unacceptable background color name.

The name of the new constant variable is as follows:
If both FOREGROUND and BACKGROUND are strings: FOREGROUND-on-BACKGROUND-face
If only FOREGROUND is a string:                FOREGROUND-foreground-face
If only BACKGROUND is a string:                BACKGROUND-background-face

Examples of use:

 (define-face-const \"Blue\" \"Thistle\") => (defconst 'blue-on-thistle-face)
       where (face-foreground 'blue-on-thistle-face) = \"Blue\"
             (face-background 'blue-on-thistle-face) = \"Thistle\"

 (define-face-const \"Blue\" nil) => (defconst 'blue-foreground-face)
       where (face-foreground 'blue-foreground-face) = \"Blue\"

 (define-face-const nil \"Thistle\") => (defconst 'thistle-background-face)
       where (face-background 'thistle-background-face) = \"Thistle\"

If color ZZZZZZ is undefined:

 (define-face-const \"Blue\" \"ZZZZZZ\") => (defconst 'blue-on-white-face)
       where (face-foreground 'blue-on-white-face) = \"Blue\"
             (face-background 'blue-on-white-face) = \"White\"

 (define-face-const \"ZZZZZZ\" \"Pink\") => (defconst 'black-on-pink-face)
       where (face-foreground 'black-on-pink-face) = \"Black\"
             (face-background 'black-on-pink-face) = \"Pink\"

\(fn FOREGROUND BACKGROUND)" nil (quote macro))

;;;***

;;;### (autoloads (exit-minibuffer minibuffer-completion-help+ rebind-minibuffer-completion-maps
;;;;;;  abort-minibuffer-input assoc-tail completing-read buffer-alist)
;;;;;;  "elect-mbuf" "elect-mbuf.el" (16944 35476))
;;; Generated autoloads from elect-mbuf.el

(defvar mbuf-completion-help-face (or (and (boundp (quote darkmagenta-foreground-face)) darkmagenta-foreground-face) (define-face-const "DarkMagenta" nil)) "\
*Face used to highlight minibuffer completion help.")

(defvar mbuf-completion-help-title-face (or (and (boundp (quote red-foreground-face)) red-foreground-face) (define-face-const "Red" nil)) "\
*Face used for instructions introducing completion help.")

(autoload (quote buffer-alist) "elect-mbuf" "\
Alist of (BUF-NAME . BUF) items, where BUF-NAME (a string) names BUF,
which is in (buffer-list), and BUF-NAME does not start with SPACE.

\(fn)" nil nil)

(autoload (quote completing-read) "elect-mbuf" "\
Read string in minibuffer, with completion and default input cycling.
Minibuffer completion help via \\<minibuffer-local-completion-map>\\[minibuffer-completion-help+]. Completion via \\[minibuffer-complete-word], \\[minibuffer-complete] and ESC-TAB.
Cycling of default inputs via \\[previous-default-input] and \\[next-default-input].
Cycling of past inputs via \\[previous-history-element] and \\[next-history-element].
Searching through input history via \\[previous-matching-history-element] and \\[next-matching-history-element].
Case is ignored if `completion-ignore-case' is non-nil.
Type \\[exit-minibuffer] to end your input.

Args: PROMPT, TABLE, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST:

PROMPT is a string to prompt with; normally ends in a colon and space.

TABLE is an alist whose elements' cars are strings, or an obarray.

PREDICATE limits completion to a subset of TABLE.
See `try-completion' and `all-completions' for more details on
completion, TABLE, PREDICATE.

If REQUIRE-MATCH is non-nil, you are not allowed to exit unless the
input is (or completes to) an element of TABLE or is null.  If it is
also not `t', \\[exit-minibuffer] doesn't exit if it effects non-null
completion.  If the input is null, `completing-read' returns an empty
string, regardless of the value of REQUIRE-MATCH.

If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
If it is (STRING . POSITION), the initial input is STRING, but point
is placed POSITION characters into the string.

HIST, if non-nil, specifies a history list, and optionally the initial
position in the list.  It can be a symbol, which is the history list
variable to use, or it can be a cons cell (HISTVAR . HISTPOS).  In
that case, HISTVAR is the history list variable to use, and HISTPOS is
the initial position (the position in the list which INITIAL-INPUT
corresponds to).  Positions are counted starting from 1 at the
beginning of the list.

DEF, if non-nil, is the default value.

If INHERIT-INPUT-METHOD is non-nil, the minibuffer inherits the
current input method and the setting of `enable-multibyte-characters'.

Completion ignores case when`completion-ignore-case' is non-nil.

\(fn PROMPT TABLE &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil)

(autoload (quote assoc-tail) "elect-mbuf" "\
Returns longest (i.e. first) tail of LIST whose caar matches ELEMENT.
Matching is done with function `equal'.

\(fn ELEMENT LIST)" nil nil)

(autoload (quote abort-minibuffer-input) "elect-mbuf" "\
Abort input, or, if in delete selection mode, deactivate the mark.
Iconify \"*Completions*\" frame, if any, before aborting minibuffer
input via `abort-recursive-edit'.

This can be used in delete selection mode to cancel a selection in
the minibuffer without aborting.  (A second C-g will then abort.)
This feature won't work if using icomplete (`icomplete-inhibit' =
nil; see file `icomplete.el').

\(fn)" t nil)

(autoload (quote rebind-minibuffer-completion-maps) "elect-mbuf" "\
Rebind minibuffer completion maps to be able to cycle minibuffer
default inputs.  This should be done after all keys have been bound,
because it redefines minibuffer keymaps.

\(fn)" nil nil)

(autoload (quote minibuffer-completion-help+) "elect-mbuf" "\
Describe minibuffer bindings.  Display list of possible completions
of current minibuffer contents.

\(fn)" t nil)

(defvar command-calling-for-completion nil "\
Last command causing display of list of possible completions.")

(autoload (quote exit-minibuffer) "elect-mbuf" "\
Terminate this minibuffer argument.

\(fn)" t nil)

;;;***

;;;### (autoloads (fold-dwim-toggle fold-dwim-show fold-dwim-hide
;;;;;;  fold-dwim-show-all fold-dwim-hide-all) "fold-dwim" "fold-dwim.el"
;;;;;;  (17029 64885))
;;; Generated autoloads from fold-dwim.el

(autoload (quote fold-dwim-hide-all) "fold-dwim" "\
Hide all folds of various kinds in the buffer

\(fn)" t nil)

(autoload (quote fold-dwim-show-all) "fold-dwim" "\
Show all folds of various kinds in the buffer

\(fn)" t nil)

(autoload (quote fold-dwim-hide) "fold-dwim" "\
Hide one item

\(fn)" nil nil)

(autoload (quote fold-dwim-show) "fold-dwim" "\
If point is in a closed or temporarily open fold,
  open it.  Returns nil if nothing was done

\(fn)" nil nil)

(autoload (quote fold-dwim-toggle) "fold-dwim" "\
Try fold-dwim-show to show any hidden text at point; if no
hidden fold is found, try fold-dwim-hide to hide the construction
at the cursor.

\(fn)" t nil)

;;;***

;;;### (autoloads (ftelnet) "ftelnet" "ftelnet.el" (16944 35476))
;;; Generated autoloads from ftelnet.el
 (add-hook 'same-window-regexps "^\\*telnet-.*\\*\\(\\|<[0-9]+>\\)")

(autoload (quote ftelnet) "ftelnet" "\
Open a network login connection to HOST via the `telnet' program.
Input is sent line-at-a-time to the remote connection.

Communication with the remote host is recorded in a buffer *telnet-HOST*
\(or *telnet-HOST:PORT* if using a nonstandard port number).
If a prefix argument is given and the buffer *telnet-HOST* already exists,
a new buffer with a different connection will be made.

When called from a program, if the optional second argument is a string or
buffer, it names the buffer to use.

The variable `ftelnet-program' contains the name of the actual program to
run.  It can be a relative or absolute path.

The variable `ftelnet-explicit-args' is a list of arguments to give to the
telnet program when starting.  They are added after any arguments given in
INPUT-ARGS.

If the default value of `ftelnet-directory-tracking-mode' is t, then the
default directory in that buffer is set to a remote (FTP) file name to
access your home directory on the remote machine.  Occasionally this causes
an error, if you cannot access the home directory on that machine.  This
error is harmless as long as you don't try to use that default directory.

If `ftelnet-directory-tracking-mode' is neither t nor nil, then the default
directory is initially set up to your (local) home directory.
This is useful if the remote machine and your local machine
share the same files via NFS.  This is the default.

If you wish to change directory tracking styles during a session, use the
function `ftelnet-directory-tracking-mode' rather than simply setting the
variable.

\(fn INPUT-ARGS &optional BUFFER)" t nil)

;;;***

;;;### (autoloads (gnus-alias-determine-identity gnus-alias-use-identity
;;;;;;  gnus-alias-select-identity gnus-alias-init) "gnus-alias"
;;;;;;  "gnus-alias.el" (16944 35476))
;;; Generated autoloads from gnus-alias.el

(autoload (quote gnus-alias-init) "gnus-alias" "\
Add gnus-alias call to message mode hook.

\(fn)" t nil)

(autoload (quote gnus-alias-select-identity) "gnus-alias" "\
Prompt user for an identity and use it.

\(fn)" t nil)

(autoload (quote gnus-alias-use-identity) "gnus-alias" "\
Use an Identity defined in `gnus-alias-identity-alist'.

IDENTITY must be a valid entry in `gnus-alias-identity-alist',
otherwise an error will occur (NOTE: this behavior has changed
significantly from that found in 'gnus-pers').

If called interactively with no identity, user will be prompted for
one.

\(fn &optional IDENTITY)" t nil)

(autoload (quote gnus-alias-determine-identity) "gnus-alias" "\
Function that chooses a Identity based on message headers.

See `gnus-alias-identity-rules' for more information.  Optional
LOOKUP-ONLY is a boolean that, when non-nil, says to determine the
Identity, but don't actually use it (just return it)

\(fn &optional LOOKUP-ONLY)" nil nil)

;;;***

;;;### (autoloads (gnus-cvslog-highlight gnus-cvslog-highlight-maybe)
;;;;;;  "gnus-cvslog" "gnus-cvslog.el" (16944 35476))
;;; Generated autoloads from gnus-cvslog.el

(autoload (quote gnus-cvslog-highlight-maybe) "gnus-cvslog" "\
Highlight CVS log message iff `gnus-cvslog-enabled' is non-nil.
You can use this to toggle CVS log message highlightings via group
parameters.

\(fn)" nil nil)

(autoload (quote gnus-cvslog-highlight) "gnus-cvslog" "\
Highlight CVS log message.
This is the main entry point of this file.

\(fn)" nil nil)

;;;***

;;;### (autoloads (graphviz-dot-mode) "graphviz-dot-mode" "graphviz-dot-mode.el"
;;;;;;  (16944 35476))
;;; Generated autoloads from graphviz-dot-mode.el

(autoload (quote graphviz-dot-mode) "graphviz-dot-mode" "\
Major mode for the dot language. \\<graphviz-dot-mode-map>
TAB indents for graph lines.

\\[graphviz-dot-indent-graph]	- Indentaion function.
\\[graphviz-dot-preview]	- Previews graph in a buffer.
\\[graphviz-dot-view]	- Views graph in an external viewer.
\\[graphviz-dot-indent-line]	- Indents current line of code.
\\[graphviz-dot-complete-word]	- Completes the current word.
\\[electric-graphviz-dot-terminate-line]	- Electric newline.
\\[electric-graphviz-dot-open-brace]	- Electric open braces.
\\[electric-graphviz-dot-close-brace]	- Electric close braces.
\\[electric-graphviz-dot-semi]	- Electric semi colons.

Variables specific to this mode:

  graphviz-dot-dot-program            (default `dot')
       Location of the dot program.
  graphviz-dot-view-command           (default `doted %s')
       Command to run when `graphviz-dot-view' is executed.
  graphviz-dot-view-edit-command      (default nil)
       If the user should be asked to edit the view command.
  graphviz-dot-save-before-view       (default t)
       Automatically save current buffer berore `graphviz-dot-view'.
  graphviz-dot-preview-extension      (default `png')
       File type to use for `graphviz-dot-preview'.
  graphviz-dot-auto-indent-on-newline (default t)
       Whether to run `electric-graphviz-dot-terminate-line' when
       newline is entered.
  graphviz-dot-auto-indent-on-braces (default t)
       Whether to run `electric-graphviz-dot-open-brace' and
       `electric-graphviz-dot-close-brace' when braces are
       entered.
  graphviz-dot-auto-indent-on-semi (default t)
       Whether to run `electric-graphviz-dot-semi' when semi colon
       is typed.
  graphviz-dot-toggle-completions  (default nil)
       If completions should be displayed in the buffer instead of a
       completion buffer when \\[graphviz-dot-complete-word] is
       pressed repeatedly.

This mode can be customized by running \\[graphviz-dot-customize].

Turning on Graphviz Dot mode calls the value of the variable
`graphviz-dot-mode-hook' with no args, if that value is non-nil.

\(fn)" t nil)

;;;***

;;;### (autoloads (gtags-mode) "gtags" "gtags.el" (16944 35476))
;;; Generated autoloads from gtags.el

(autoload (quote gtags-mode) "gtags" "\
Toggle Gtags mode, a minor mode for browsing source code using GLOBAL.

Input tag name and move to the definition.
	\\[gtags-find-tag]
Input tag name and move to the referenced point.
	\\[gtags-find-rtag]
Input symbol and move to the locations.
	\\[gtags-find-symbol]
Input pattern, search with grep(1) and move to the locations.
	\\[gtags-find-with-grep]
Input pattern, search with id-utils(1) and move to the locations.
	\\[gtags-find-with-idutils]
Input pattern and move to the top of the file.
	\\[gtags-find-file]
Get the expression as a tagname around here and move there.
	\\[gtags-find-tag-from-here]
Display current screen on hypertext browser.
	\\[gtags-display-browser]
Get the expression as a tagname around here and move there.
	\\[gtags-find-tag-by-event]
Move to previous point on the stack.
	\\[gtags-pop-stack]
Make tag name list for completion.
	\\[gtags-make-complete-list]

Key definitions:
\\{gtags-mode-map}
Turning on Gtags mode calls the value of the variable `gtags-mode-hook'
with no args, if that value is non-nil.

\(fn &optional FORCES)" t nil)

;;;***

;;;### (autoloads (h4x0r-string h4x0r-word-at-point h4x0r-buffer
;;;;;;  h4x0r-region) "h4x0r" "h4x0r.el" (17029 62414))
;;; Generated autoloads from h4x0r.el

(autoload (quote h4x0r-region) "h4x0r" "\
Convert region to h4x0r-talk.

\(fn BEG END)" t nil)

(autoload (quote h4x0r-buffer) "h4x0r" "\
Convert entire buffer to h4x0r-talk.

\(fn)" t nil)

(autoload (quote h4x0r-word-at-point) "h4x0r" "\
Not documented

\(fn)" t nil)

(autoload (quote h4x0r-string) "h4x0r" "\
Not documented

\(fn H4-INPUT-STRING)" nil nil)

;;;***

;;;### (autoloads (highline-off highline-on highline-local-mode highline-mode-off
;;;;;;  highline-mode-on highline-mode highline-customize) "highline"
;;;;;;  "highline.el" (16944 35476))
;;; Generated autoloads from highline.el

(autoload (quote highline-customize) "highline" "\
Customize highline group.

\(fn)" t nil)

(autoload (quote highline-mode) "highline" "\
Toggle global minor mode to highlight line about point (HL on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system.

\(fn &optional ARG)" t nil)

(autoload (quote highline-mode-on) "highline" "\
Turn on global minor mode to highlight line about point (HL on modeline).

\(fn)" t nil)

(autoload (quote highline-mode-off) "highline" "\
Turn off global minor mode to highlight line about point (HL on modeline).

\(fn)" t nil)

(autoload (quote highline-local-mode) "highline" "\
Toggle local minor mode to highlight the line about point (hl on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system.

\(fn &optional ARG)" t nil)

(autoload (quote highline-on) "highline" "\
Turn on local highlighting of the current line in buffer (hl on modeline).

\(fn)" t nil)

(autoload (quote highline-off) "highline" "\
Turn off local highlighting of the current line in buffer (hl on modeline).

\(fn)" t nil)

;;;***

;;;### (autoloads (global-hl-line-hack-mode hl-line-hack-mode) "hl-line-hack"
;;;;;;  "hl-line-hack.el" (16944 35476))
;;; Generated autoloads from hl-line-hack.el

(autoload (quote hl-line-hack-mode) "hl-line-hack" "\
Minor mode to highlight the line about point in the current window.
With ARG, turn Hl-Line-Hack mode on if ARG is positive, off otherwise.
Uses functions `hl-line-hack-unhighlight' and `hl-line-hack-highlight' on
`pre-command-hook' and `post-command-hook'.

\(fn &optional ARG)" t nil)

(defvar global-hl-line-hack-mode nil "\
Non-nil if Global-Hl-Line-Hack mode is enabled.
See the command `global-hl-line-hack-mode' for a description of this minor-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `global-hl-line-hack-mode'.")

(custom-autoload (quote global-hl-line-hack-mode) "hl-line-hack")

(autoload (quote global-hl-line-hack-mode) "hl-line-hack" "\
Toggle Hl-Line-Hack mode in every buffer.
With prefix ARG, turn Global-Hl-Line-Hack mode on if and only if ARG is positive.
Hl-Line-Hack mode is actually not turned on in every buffer but only in those
in which `(lambda nil (hl-line-hack-mode 1))' turns it on.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (global-hl-sexp-mode hl-sexp-mode) "hl-sexp" "hl-sexp.el"
;;;;;;  (16944 35476))
;;; Generated autoloads from hl-sexp.el

(autoload (quote hl-sexp-mode) "hl-sexp" "\
Minor mode to highlight the sexp about point in the current window.
With ARG, turn Hl-Sexp mode on if ARG is positive, off otherwise.
Uses functions `hl-sexp-unhighlight' and `hl-sexp-highlight' on
`pre-command-hook' and `post-command-hook'.

\(fn &optional ARG)" t nil)

(defvar global-hl-sexp-mode nil "\
Non-nil if Global-Hl-Sexp mode is enabled.
See the command `global-hl-sexp-mode' for a description of this minor-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `global-hl-sexp-mode'.")

(custom-autoload (quote global-hl-sexp-mode) "hl-sexp")

(autoload (quote global-hl-sexp-mode) "hl-sexp" "\
Toggle Hl-Sexp mode in every buffer.
With prefix ARG, turn Global-Hl-Sexp mode on if and only if ARG is positive.
Hl-Sexp mode is actually not turned on in every buffer but only in those
in which `hl-sexp-mode' turns it on.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (http-get) "http-get" "http-get.el" (16944 35476))
;;; Generated autoloads from http-get.el

(autoload (quote http-get) "http-get" "\
Get URL in a buffer, and return the process.
You can get the buffer associated with this process using
`process-buffer'.

The optional HEADERS are an alist where each element has the form
\(NAME . VALUE).  Both must be strings and will be passed along with
the request.

With optional argument SENTINEL, the buffer is not shown.  It is the
responsability of the sentinel to show it, if appropriate.  A sentinel
function takes two arguments, process and message.  It is called when
the process is killed, for example.  This is useful when specifying a
non-persistent connection.  By default, connections are persistent.
Add (\"Connection\" . \"close\") to HEADERS in order to specify a
non-persistent connection.  Usually you do not need to specify a
sentinel, and `ignore' is used instead, to prevent a message being
printed when the connection is closed.

If you want to filter the content as it arrives, bind
`http-filter-pre-insert-hook' and `http-filter-post-insert-hook'.

The optional argument VERSION specifies the HTTP version to use.  It
defaults to version 1.0, such that the connection is automatically
closed when the entire document has been downloaded.  This will then
call SENTINEL, if provided.  If no sentinel is provided, `ignore' will
be used in order to prevent a message in the buffer when the process
is killed.

CONTENT-TYPE is a coding system to use for the encoding of the url param value.  Its upper case print name
will be used for the server.  Possible values are `iso-8859-1' or
`euc-jp' and others.


The coding system of the process is set to `binary', because we need to
distinguish between \\r and \\n.  To correctly decode the text later,
use `decode-coding-region' and get the coding system to use from
`http-headers'.

\(fn URL &optional HEADERS SENTINEL VERSION BUFNAME CONTENT-TYPE)" t nil)

;;;***

;;;### (autoloads (icomplete-completions icomplete-exhibit) "icomplete+"
;;;;;;  "icomplete+.el" (16944 35476))
;;; Generated autoloads from icomplete+.el

(unless (boundp (quote darkgoldenrod-foreground-face)) (define-face-const "DarkGoldenrod" nil))

(unless (boundp (quote seagreen-foreground-face)) (define-face-const "SeaGreen" nil))

(autoload (quote icomplete-exhibit) "icomplete+" "\
Insert icomplete completions display.
Should be run via minibuffer `post-command-hook'.
See `icomplete-mode' and `minibuffer-setup-hook'.

\(fn)" nil nil)

(autoload (quote icomplete-completions) "icomplete+" "\
Identify prospective candidates for minibuffer completion.

The display is updated with each minibuffer keystroke during
minibuffer completion.

Prospective completion suffixes (if any) are displayed, bracketed by
\"()\", \"[]\", or \"{}\".  The choice of brackets is as follows:

  (...) - A single prospect is identified and matching is enforced.
  [...] - A single prospect is identified and matching is optional.
  {...} - Multiple prospects, separated by commas, are indicated,
           and further input is required to distinguish a single one.

The displays for unambiguous matches have \" [ Matched ]\" appended
\(whether complete or not), or \" [ No match ]\", if no eligible
matches exist.  (Keybindings for uniquely matched commands
are exhibited within the square braces.)

\(fn NAME CANDIDATES PREDICATE REQUIRE-MATCH)" nil nil)

;;;***

;;;### (autoloads (idledo-add-to-end-of-list) "idledo" "idledo.el"
;;;;;;  (16944 35476))
;;; Generated autoloads from idledo.el

(autoload (quote idledo-add-to-end-of-list) "idledo" "\
Like add-to-list, but adds at the end, if added at all.

\(fn LIST-VAR ELEMENT)" nil nil)

;;;***

;;;### (autoloads (isearchb-activate) "isearchb" "isearchb.el" (16944
;;;;;;  35476))
;;; Generated autoloads from isearchb.el

(autoload (quote isearchb-activate) "isearchb" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (iswitchb-mode iswitchb-buffer-other-frame iswitchb-display-buffer
;;;;;;  iswitchb-buffer-other-window iswitchb-buffer iswitchb-default-keybindings
;;;;;;  iswitchb-read-buffer) "iswitchb" "iswitchb.el" (16944 35476))
;;; Generated autoloads from iswitchb.el

(autoload (quote iswitchb-read-buffer) "iswitchb" "\
Replacement for the built-in `read-buffer'.
Return the name of a buffer selected.
PROMPT is the prompt to give to the user.
DEFAULT if given is the default buffer to be selected, which will
go to the front of the list.
If REQUIRE-MATCH is non-nil, an existing-buffer must be selected.
If START is a string, the selection process is started with that
string.
If MATCHES-SET is non-nil, the buflist is not updated before
the selection process begins.  Used by isearchb.el.

\(fn PROMPT &optional DEFAULT REQUIRE-MATCH START MATCHES-SET)" nil nil)

(autoload (quote iswitchb-default-keybindings) "iswitchb" "\
Set up default keybindings for `iswitchb-buffer'.
Call this function to override the normal bindings.  This function also
adds a hook to the minibuffer.

Obsolescent.  Use `iswitchb-mode'.

\(fn)" t nil)

(autoload (quote iswitchb-buffer) "iswitchb" "\
Switch to another buffer.

The buffer name is selected interactively by typing a substring.  The
buffer is displayed according to `iswitchb-default-method' -- the
default is to show it in the same window, unless it is already visible
in another frame.
For details of keybindings, do `\\[describe-function] iswitchb'.

\(fn)" t nil)

(autoload (quote iswitchb-buffer-other-window) "iswitchb" "\
Switch to another buffer and show it in another window.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] iswitchb'.

\(fn)" t nil)

(autoload (quote iswitchb-display-buffer) "iswitchb" "\
Display a buffer in another window but don't select it.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] iswitchb'.

\(fn)" t nil)

(autoload (quote iswitchb-buffer-other-frame) "iswitchb" "\
Switch to another buffer and show it in another frame.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] iswitchb'.

\(fn)" t nil)

(defvar iswitchb-mode nil "\
Non-nil if Iswitchb mode is enabled.
See the command `iswitchb-mode' for a description of this minor-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `iswitchb-mode'.")

(custom-autoload (quote iswitchb-mode) "iswitchb")

(autoload (quote iswitchb-mode) "iswitchb" "\
Toggle Iswitchb global minor mode.
With arg, turn Iswitchb mode on if and only iff ARG is positive.
This mode enables switching between buffers using substrings.  See
`iswitchb' for details.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (keytable) "keytable" "keytable.el" (17029 62499))
;;; Generated autoloads from keytable.el

(autoload (quote keytable) "keytable" "\
Display key table.

\(fn &optional PREFIX BUFFER)" t nil)

;;;***

;;;### (autoloads (map-lines) "map-lines" "map-lines.el" (17029 62617))
;;; Generated autoloads from map-lines.el

(autoload (quote map-lines) "map-lines" "\
Map COMMAND over lines matching REGEX.

\(fn COMMAND-C REGEX)" t nil)

;;;***

;;;### (autoloads (margin-mode) "margin" "margin.el" (16944 35476))
;;; Generated autoloads from margin.el

(autoload (quote margin-mode) "margin" "\
Minor mode that displays a margin

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (expand-member-functions) "member-functions" "member-functions.el"
;;;;;;  (17029 62323))
;;; Generated autoloads from member-functions.el

(autoload (quote expand-member-functions) "member-functions" "\
Expand C++ member function declarations into member function definitions, where needed.

When invoked interactively with \\[expand-member-functions], prompts
for a header file, and the corresponding implementation file.

Searches the header file for class declarations.  For every class
declaration found, all of the declared member functions are collected.
Functions defined inline are ignored.

After all declarations are found, the implementation file is searched
for member function definitions.  The list of member function
declarations is then compared to the list of member function
definitions.

Finally, the member function declarations that have no corresponding
definitions are formatted and inserted, with commentary, into the
implementation file.

\(fn HEADER C-FILE)" t nil)

;;;***

;;;### (autoloads (metamail-region metamail-buffer metamail-interpret-body
;;;;;;  metamail-interpret-header) "metamail" "metamail.el" (16944
;;;;;;  35476))
;;; Generated autoloads from metamail.el

(autoload (quote metamail-interpret-header) "metamail" "\
Interpret a header part of a MIME message in current buffer.
Its body part is not interpreted at all.

\(fn)" t nil)

(autoload (quote metamail-interpret-body) "metamail" "\
Interpret a body part of a MIME message in current buffer.
Optional 1st argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional 2nd argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted.
Its header part is not interpreted at all.

\(fn &optional VIEWMODE NODISPLAY)" t nil)

(autoload (quote metamail-buffer) "metamail" "\
Process current buffer through `metamail'.
Optional 1st argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional 2nd argument BUFFER specifies a buffer to be filled (nil
means current).
Optional 3rd argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted.

\(fn &optional VIEWMODE BUFFER NODISPLAY)" t nil)

(autoload (quote metamail-region) "metamail" "\
Process current region through 'metamail'.
Optional 1st argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional 2nd argument BUFFER specifies a buffer to be filled (nil
means current).
Optional 3rd argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted.

\(fn BEG END &optional VIEWMODE BUFFER NODISPLAY)" t nil)

;;;***

;;;### (autoloads (paren-backward-sexp paren-forward-sexp paren-toggle-open-paren-context
;;;;;;  paren-toggle-matching-quoted-paren paren-toggle-matching-paired-delimiter
;;;;;;  paren-deactivate paren-activate) "mic-paren" "mic-paren.el"
;;;;;;  (16944 35476))
;;; Generated autoloads from mic-paren.el

(autoload (quote paren-activate) "mic-paren" "\
Activates mic-paren parenthesis highlighting.
paren-activate deactivates the paren.el and stig-paren.el packages if they are
active !
The following options are available via the customize-feature:
  `paren-priority'
  `paren-overlay-priority'
  `paren-sexp-mode'
  `paren-highlight-at-point'
  `paren-highlight-offscreen'
  `paren-display-message'
  `paren-message-linefeed-display'
  `paren-message-no-match'
  `paren-message-show-linenumber'
  `paren-message-truncate-lines'
  `paren-ding-unmatched'
  `paren-delay'
  `paren-dont-touch-blink'
  `paren-match-face'
  `paren-mismatch-face'
  `paren-no-match-face'
  `paren-bind-modified-sexp-functions'
The following options are settable via toggling functions (look at the
documentation of these options for the names of these functions):
  `paren-match-quoted-paren'
  `paren-match-paired-delimiter'
  `paren-open-paren-context-backward'

\(fn)" t nil)

(autoload (quote paren-deactivate) "mic-paren" "\
Deactivates mic-paren parenthesis highlighting

\(fn)" t nil)

(autoload (quote paren-toggle-matching-paired-delimiter) "mic-paren" "\
Toggle matching paired delimiter, force on with positive arg. Use this in
mode-hooks to activate or deactivate paired delimiter matching. If optional
second argument NO-MESSAGE is not nil then no message is displayed about the
current activation state of the paired-delimiter-matching feature.

\(fn ARG &optional NO-MESSAGE)" t nil)

(autoload (quote paren-toggle-matching-quoted-paren) "mic-paren" "\
Toggle matching quoted parens, force on with positive arg. Use this in
mode-hooks to activate or deactivate quoted paren matching. If optional second
argument NO-MESSAGE is not nil then no message is displayed about the current
activation state of the quoted-paren-matching feature.

\(fn ARG &optional NO-MESSAGE)" t nil)

(autoload (quote paren-toggle-open-paren-context) "mic-paren" "\
Toggle the determining of the context to display of the matching
open-paren, force backward context with positive arg. Use this in mode-hooks.
For a description of the meaning look at `paren-open-paren-context-backward'.

\(fn ARG)" t nil)

(autoload (quote paren-forward-sexp) "mic-paren" "\
Acts like forward-sexp but can also handle quoted parens. Look at
`paren-match-quoted-paren' for a detailed comment.

\(fn &optional ARG)" t nil)

(autoload (quote paren-backward-sexp) "mic-paren" "\
Acts like backward-sexp but can also matching quoted parens. Look at
`paren-match-quoted-paren' for a detailed comment

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (mime-forward-from-gnus-using-mail mime-forward-from-rmail-using-mail
;;;;;;  mime-mode) "mime" "mime.el" (16944 35476))
;;; Generated autoloads from mime.el

(autoload (quote mime-mode) "mime" "\
MIME minor mode for editing the tagged MIME message.

In this mode, basically, the message is composed in the tagged MIME
format.  The message tag looks like:

	`--[[text/plain; charset=ISO-2022-JP][7bit]]'.

The tag specifies the MIME content type, subtype, optional parameters
and transfer encoding of the message following the tag.  Messages
without any tag are treated as `text/plain' by default.  Charset and
transfer encoding are automatically defined unless explicitly
specified.  Binary messages such as audio and image are usually hidden
using selective-display facility.  The messages in the tagged MIME
format are automatically translated into a MIME compliant message when
exiting this mode.

Available charsets depend on Emacs version being used.  The following
lists the available charsets of each emacs.

Emacs18:	US-ASCII is only available.
NEmacs:		US-ASCII and ISO-2022-JP are available.
Emacs19:	US-ASCII and ISO-8859-1 are available.
Mule:		US-ASCII, ISO-8859-* (except for ISO-8859-6),
		ISO-2022-JP, ISO-2022-JP-2 and ISO-2022-INT-1 are available.

ISO-2022-JP-2 and ISO-2022-INT-1 charsets used in Mule is expected to
be used to represent multilingual text in intermixed manner.  Any
languages that has no registered charset are represented as either
ISO-2022-JP-2 or ISO-2022-INT-1 in Mule.

Following commands are available in addition to major mode commands:
\\[mime-insert-text]	insert a text message.
\\[mime-insert-file]	insert a (binary) file.
\\[mime-insert-external]	insert a reference to external body.
\\[mime-insert-voice]	insert a voice message.
\\[mime-insert-signature]	insert a signature file at end.
\\[mime-insert-tag]	insert a new MIME tag.
\\[mime-preview-content]	preview an included content.
\\[mime-mode-exit]	exit and translate into a MIME compliant message.
\\[mime-mode-exit-and-run]		exit, translate and run the original command.
\\[help-mime-mode]	show this help.

Additional commands are available in some major modes:
C-c C-c		exit, translate and run the original command.
C-c C-s		exit, translate and run the original command.

The following is a message example written in the tagged MIME format.
TABs at the beginning of the line are not a part of the message:

	This is a conventional plain text.  It should be translated
	into text/plain.
	--[[text/plain]]
	This is also a plain text.  But, it is explicitly specified as
	is.
	--[[text/plain; charset=ISO-2022-JP]]
	Å≥ÅÏÅœ charset ÅÚ ISO-2022-JP ÅÀÅÿÅÍÅ∑ÅøÅ¸Å‹ÅÏÅŒ plain
	Å»Å«Åπ.
	--[[text/richtext]]
	<center>This is a richtext.</center>
	--[[image/gif][base64]]^M...image encoded in base64 here...
	--[[audio/basic][base64]]^M...audio encoded in base64 here...

User customizable variables (not documented all of them):
 mime-prefix
    Specifies a key prefix for MIME minor mode commands.

 mime-signature-file
    Specifies a signature file to be included as part of a multipart
    message.

 mime-ignore-preceding-spaces
    Preceding white spaces in a message body are ignored if non-nil.

 mime-ignore-trailing-spaces
    Trailing white spaces in a message body are ignored if non-nil.

 mime-auto-fill-header
    Fill header fields that contain encoded-words if non-nil.

 mime-auto-hide-body
    Hide a non-textual body message encoded in base64 after insertion
    if non-nil.

 mime-header-charset-chooser
    Specifies a function to identify charset and encoding of a text in
    a give region of header fields.  The value is a form of (CHARSET .
    ENCODING), where ENCODING is either 'B' or 'Q'.  Nil means no
    encoding is necessary.

 mime-body-charset-chooser
    Specifies a function to identify charset and encoding of a text in
    a given region.  The value is a form of (CHARSET . ENCODING),
    where ENCODING must be a full name, such as base64.

 mime-string-encoder
    Specifies a function to encode a string for given encoding method.
    The method is a form of (CHARSET . ENCODING).

 mime-voice-recorder
    Specifies a function to record a voice message and return a buffer
    that contains it.  The function mime-voice-recorder-for-sun is for
    Sun SparcStations.

 mime-mode-hook
    Turning on MIME mode calls the value of mime-mode-hook, if it is
    non-nil.

 mime-translate-hook
    The value of mime-translate-hook is called just before translating
    the tagged MIME format into a MIME compliant message if it is
    non-nil.  If the hook call the function mime-insert-signature, the
    signature file will be inserted automatically.

 mime-exit-hook
    Turning off MIME mode calls the value of mime-exit-hook, if it is
    non-nil.

\(fn)" t nil)

(fset (quote edit-mime) (quote mime-mode))

(autoload (quote mime-forward-from-rmail-using-mail) "mime" "\
Forward current message in message/rfc822 content-type message from rmail.
The message will be appended if being composed.

\(fn)" t nil)

(autoload (quote mime-forward-from-gnus-using-mail) "mime" "\
Forward current article in message/rfc822 content-type message from GNUS.
The message will be appended if being composed.

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb/send-hook bbdb/send-auto-notes-hook bbdb/send-ignore-some-messages-hook
;;;;;;  bbdb/send-ignore-most-messages-hook) "moy-bbdb" "moy-bbdb.el"
;;;;;;  (16944 35476))
;;; Generated autoloads from moy-bbdb.el

(autoload (quote bbdb/send-ignore-most-messages-hook) "moy-bbdb" "\
For use as the value of `bbdb/send-auto-create-p'.
This will automatically create BBDB entries for messages which match
the bbdb/send-ignore-most-messages-alist (which see) and *no* others.

\(fn &optional INVERT-SENSE)" nil nil)

(autoload (quote bbdb/send-ignore-some-messages-hook) "moy-bbdb" "\
For use as a `bbdb/send-auto-create-hook'.
This will automatically create BBDB entries for messages which do *not*
match the `bbdb/send-ignore-some-messages-alist' (which see).

\(fn)" nil nil)

(autoload (quote bbdb/send-auto-notes-hook) "moy-bbdb" "\
For use as a `bbdb/send-notice-hook'.  This might automatically add
some text to  the notes field of the BBDB  record corresponding to the
current record  based on the header  of the current  message.  See the
documentation  for   the  variables  `bbdb/send-auto-notes-alist'  and
`bbdb/send-auto-notes-ignore'.

\(fn RECORD)" nil nil)

(autoload (quote bbdb/send-hook) "moy-bbdb" "\
Parse headers of outgoing message, insert the addresses of the
  recipients one by one into BBDB if they do not exist already

\(fn)" t nil)

;;;***

;;;### (autoloads (mtp) "mtp" "mtp.el" (16944 35476))
;;; Generated autoloads from mtp.el
 (add-hook 'same-window-regexps "^\\*telnet-.*\\*\\(\\|<[0-9]+>\\)")

(autoload (quote mtp) "mtp" "\
Open a network login connection to HOST via the `telnet' program.
Input is sent line-at-a-time to the remote connection.

Communication with the remote host is recorded in a buffer *telnet-HOST*
\(or *telnet-HOST:PORT* if using a nonstandard port number).
If a prefix argument is given and the buffer *telnet-HOST* already exists,
a new buffer with a different connection will be made.

When called from a program, if the optional second argument is a string or
buffer, it names the buffer to use.

The variable `mtp-program' contains the name of the actual program to
run.  It can be a relative or absolute path.

The variable `mtp-explicit-args' is a list of arguments to give to the
telnet program when starting.  They are added after any arguments given in
INPUT-ARGS.

If the default value of `mtp-directory-tracking-mode' is t, then the
default directory in that buffer is set to a remote (FTP) file name to
access your home directory on the remote machine.  Occasionally this causes
an error, if you cannot access the home directory on that machine.  This
error is harmless as long as you don't try to use that default directory.

If `mtp-directory-tracking-mode' is neither t nor nil, then the default
directory is initially set up to your (local) home directory.
This is useful if the remote machine and your local machine
share the same files via NFS.  This is the default.

If you wish to change directory tracking styles during a session, use the
function `mtp-directory-tracking-mode' rather than simply setting the
variable.

\(fn INPUT-ARGS &optional BUFFER)" t nil)

;;;***

;;;### (autoloads (obfuscate-url) "obfusurl" "obfusurl.el" (16944
;;;;;;  35476))
;;; Generated autoloads from obfusurl.el

(autoload (quote obfuscate-url) "obfusurl" "\
Obfuscate an URL under `point'.

This might be useful if you're writing out an URL for someone but the URL
itself is a spoiler. The URL will still work but it won't be readable (by
most mortals anyway).

\(fn)" t nil)

;;;***

;;;### (autoloads (page-break-mode) "page-break" "page-break.el"
;;;;;;  (16944 35476))
;;; Generated autoloads from page-break.el

(autoload (quote page-break-mode) "page-break" "\
Toggle Page Break mode.

In Page Break mode, page breaks (^L characters) are displayed as a
horizontal line of `page-break-string-char' characters.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (paren-pair-mode-for-html paren-pair-mode) "paren-pair"
;;;;;;  "paren-pair.el" (16944 35476))
;;; Generated autoloads from paren-pair.el

(autoload (quote paren-pair-mode) "paren-pair" "\
Toggle paren pair mode.
With a prefix ARG, enable paren pair mode iff arg is nonzero.

\(fn ARG)" t nil)

(autoload (quote paren-pair-mode-for-html) "paren-pair" "\
Toggle paren pair mode for HTML.
This is a legacy function which was needed when paren-pair mode did
not automatically examine syntax tables.

\(fn)" t nil)

(when (bound-and-true-p mihi-default-keys) (global-set-key (kbd "C-c )") (quote paren-pair-mode)) (add-hook (quote html-mode-hook) (quote paren-pair-mode-for-html)))

;;;***

;;;### (autoloads (fc-eval-and-replace increment-number-at-point
;;;;;;  de-context-kill diff-buffer-with-associated-file my-occur
;;;;;;  kill-syntax-backward kill-syntax-forward totd yank-and-forward-line
;;;;;;  ascii-table unix2dos dos2unix find-file-guessing simplify-blank-lines
;;;;;;  yh/c-rearrange-electrics) "perso-misc" "perso-misc.el" (17030
;;;;;;  5694))
;;; Generated autoloads from perso-misc.el

(autoload (quote yh/c-rearrange-electrics) "perso-misc" "\
Rearrange electric chars according to current c-style

\(fn)" t nil)

(autoload (quote simplify-blank-lines) "perso-misc" "\
Delete extra blank lines

\(fn)" t nil)

(autoload (quote find-file-guessing) "perso-misc" "\
Call find-file with file at point if valid. With a universal argument, force call to find-file

\(fn ARG)" t nil)

(autoload (quote dos2unix) "perso-misc" "\
Not documented

\(fn)" t nil)

(autoload (quote unix2dos) "perso-misc" "\
Not documented

\(fn)" t nil)

(autoload (quote ascii-table) "perso-misc" "\
Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>

\(fn)" t nil)

(autoload (quote yank-and-forward-line) "perso-misc" "\
Not documented

\(fn)" t nil)

(autoload (quote totd) "perso-misc" "\
Not documented

\(fn)" t nil)

(autoload (quote kill-syntax-forward) "perso-misc" "\
Kill characters with syntax at point.

\(fn)" t nil)

(autoload (quote kill-syntax-backward) "perso-misc" "\
Kill characters with syntax at point.

\(fn)" t nil)

(autoload (quote my-occur) "perso-misc" "\
Switch to *Occur* buffer, or run `moccur'.

\(fn)" t nil)

(autoload (quote diff-buffer-with-associated-file) "perso-misc" "\
View the differences between BUFFER and its associated file.
This requires the external program \"diff\" to be in your `exec-path'.
Returns nil if no differences found, 't otherwise.

\(fn)" t nil)

(autoload (quote de-context-kill) "perso-misc" "\
Kill buffer, taking gnuclient into account.

\(fn ARG)" t nil)

(autoload (quote increment-number-at-point) "perso-misc" "\
Not documented

\(fn ARG)" t nil)

(autoload (quote fc-eval-and-replace) "perso-misc" "\
Replace the preceding sexp with its value.

\(fn)" t nil)

;;;***

;;;### (autoloads (pmwiki-edit) "pmwiki-mode" "pmwiki-mode.el" (16944
;;;;;;  35476))
;;; Generated autoloads from pmwiki-mode.el

(autoload (quote pmwiki-edit) "pmwiki-mode" "\
Download the wiki page specified by LINK and BASE-URI.
If BASE-URI is not given, it's taken from `pmwiki-page-uri'.
Then change the buffer's mode to `pmwiki-mode'.
If GOTO-END is nil, wait until the process is done and move point to
the beginning of the buffer.

\(fn LINK &optional BASE-URI GOTO-END HTTP-VER CONTENT-TYPE)" nil nil)

;;;***

;;;### (autoloads (pod-last-sexp-runtime pod-load-file pod-load pod-activate-advice
;;;;;;  pod-deactivate-advice pod-reset-results pod-todo pod-new-features
;;;;;;  pod-commentary pod-introduction pod-quick-start) "pod" "pod.el"
;;;;;;  (16944 35476))
;;; Generated autoloads from pod.el

(autoload (quote pod-quick-start) "pod" "\
Provides electric help regarding `pod-quick-start'.

\(fn)" t nil)

(autoload (quote pod-introduction) "pod" "\
Provides electric help regarding `pod-introduction'.

\(fn)" t nil)

(autoload (quote pod-commentary) "pod" "\
Provides electric help regarding `pod-commentary'.

\(fn)" t nil)

(autoload (quote pod-new-features) "pod" "\
Provides electric help regarding `pod-new-features'.

\(fn)" t nil)

(autoload (quote pod-todo) "pod" "\
Provides electric help regarding `pod-todo'.

\(fn)" t nil)

(autoload (quote pod-reset-results) "pod" "\
Not documented

\(fn)" t nil)

(autoload (quote pod-deactivate-advice) "pod" "\
Not documented

\(fn)" t nil)

(autoload (quote pod-activate-advice) "pod" "\
Not documented

\(fn)" t nil)

(autoload (quote pod-load) "pod" "\
Kinda like load..

\(fn FILE &rest ARGS)" nil nil)

(autoload (quote pod-load-file) "pod" "\
Not documented

\(fn FILE)" t nil)

(autoload (quote pod-last-sexp-runtime) "pod" "\
Not documented

\(fn &optional PT)" t nil)

;;;***

;;;### (autoloads (run-prolog mercury-mode prolog-mode) "prolog"
;;;;;;  "prolog.el" (16944 35476))
;;; Generated autoloads from prolog.el

(autoload (quote prolog-mode) "prolog" "\
Major mode for editing Prolog code.

Blank lines and `%%...' separate paragraphs.  `%'s starts a comment
line and comments can also be enclosed in /* ... */.

If an optional argument SYSTEM is non-nil, set up mode for the given system.

Commands:
\\{prolog-mode-map}
Entry to this mode calls the value of `prolog-mode-hook'
if that value is non-nil.

\(fn &optional SYSTEM)" t nil)

(autoload (quote mercury-mode) "prolog" "\
Major mode for editing Mercury programs.
Actually this is just customized `prolog-mode'.

\(fn)" t nil)

(autoload (quote run-prolog) "prolog" "\
Run an inferior Prolog process, input and output via buffer *prolog*.
With prefix argument ARG, restart the Prolog process if running before.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (svn-status) "psvn" "psvn.el" (17031 46603))
;;; Generated autoloads from psvn.el

(autoload (quote svn-status) "psvn" "\
Examine the status of Subversion working copy in directory DIR.
If ARG then pass the -u argument to `svn status'.

\(fn DIR &optional ARG)" t nil)

;;;***

;;;### (autoloads (rm-mouse-drag-region rm-kill-ring-save rm-kill-region
;;;;;;  rm-exchange-point-and-mark rm-set-mark rm-example-picture-mode-bindings)
;;;;;;  "rect-mark" "rect-mark.el" (17037 41968))
;;; Generated autoloads from rect-mark.el
 (define-key ctl-x-map "r\C-@" 'rm-set-mark)
 (define-key ctl-x-map [?r ?\C-\ ] 'rm-set-mark)
 (define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
 (define-key ctl-x-map "r\C-w" 'rm-kill-region)
 (define-key ctl-x-map "r\M-w" 'rm-kill-ring-save)

(autoload (quote rm-example-picture-mode-bindings) "rect-mark" "\
Example rect-mark keyboard and mouse bindings for picture mode.

\(fn)" nil nil)

(autoload (quote rm-set-mark) "rect-mark" "\
Set mark like `set-mark-command' but anticipates a rectangle.
This arranges for the rectangular region between point and mark
to be highlighted using the same face that is used to highlight
the region in `transient-mark-mode'.  This special state lasts only
until the mark is deactivated, usually by executing a text-modifying
command like \\[kill-rectangle], by inserting text, or by typing \\[keyboard-quit].

With optional argument FORCE, arrange for tabs to be expanded and
for spaces to inserted as necessary to keep the region perfectly
rectangular.  This is the default in `picture-mode'.

\(fn FORCE)" t nil)

(autoload (quote rm-exchange-point-and-mark) "rect-mark" "\
Like `exchange-point-and-mark' but treats region as a rectangle.
See `rm-set-mark' for more details.

With optional argument FORCE, tabs are expanded and spaces are
inserted as necessary to keep the region perfectly rectangular.
This is the default in `picture-mode'.

\(fn FORCE)" t nil)

(autoload (quote rm-kill-region) "rect-mark" "\
Like kill-rectangle except the rectangle is also saved in the kill ring.
Since rectangles are not ordinary text, the killed rectangle is saved
in the kill ring as a series of lines, one for each row of the rectangle.
The rectangle is also saved as the killed rectangle so it is available for
insertion with yank-rectangle.

\(fn START END)" t nil)

(autoload (quote rm-kill-ring-save) "rect-mark" "\
Copies the region like rm-kill-region would but the rectangle isn't killed.

\(fn START END)" t nil)

(autoload (quote rm-mouse-drag-region) "rect-mark" "\
Highlight a rectangular region of text as the the mouse is dragged over it.
This must be bound to a button-down mouse event.

\(fn START-EVENT)" t nil)

;;;***

;;;### (autoloads (rmail-convert-mime-header rmail-show-mime) "rmailmime"
;;;;;;  "rmailmime.el" (16944 35476))
;;; Generated autoloads from rmailmime.el

(autoload (quote rmail-show-mime) "rmailmime" "\
Show a MIME message in current buffer using a View mode.
Optional 1st argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
If an optional 2nd argument DO-HEADER is non-nil, interpret a header part,
too.  Otherwise, a body part is only interpreted.
The contents of current buffer are not changed at all.

\(fn &optional VIEWMODE DO-HEADER)" t nil)

(autoload (quote rmail-convert-mime-header) "rmailmime" "\
Convert MIME header fields of current message into a readable form.
It is expected to be used as rmail-message-filter in Rmail and
vm-message-filter in VM.  Original header is preserved in Rmail.

\(fn)" t nil)

;;;***

;;;### (autoloads (replace-recent-character) "rrc" "rrc.el" (16944
;;;;;;  35476))
;;; Generated autoloads from rrc.el

(autoload (quote replace-recent-character) "rrc" "\
Replace-recent-character is interactive function for quick corrections of
recenlty typed text. It first prompts for character to search backwards. If
such character is found, following options are shown:
1, repeat the character to search in previous text.
2, M-r for delete of the found character.
3, C-t for trasposition of the found and the following character.
4, TAB for promt for character to insert after the found character.
5, ESC for no operation.
6, Any other insertable character will replace found character.

\(fn)" t nil)

;;;***

;;;### (autoloads (sawfish-interaction sawfish-console sawfish-rep-info
;;;;;;  sawfish-info sawfish-complete-symbol sawfish-apropos sawfish-info-variable
;;;;;;  sawfish-info-function sawfish-describe-variable sawfish-describe-function
;;;;;;  sawfish-eval-print-last-sexp sawfish-eval-last-sexp sawfish-eval-expression
;;;;;;  sawfish-eval-defun sawfish-eval-buffer sawfish-eval-region
;;;;;;  sawfish-mode) "sawfish" "sawfish.el" (16944 35476))
;;; Generated autoloads from sawfish.el

(autoload (quote sawfish-mode) "sawfish" "\
Major mode for editing sawfish files and for interacting with sawfish.

Special commands:

\\{sawfish-mode-map}

\(fn)" t nil)

(autoload (quote sawfish-eval-region) "sawfish" "\
Evaluate the region bounded by START and END.

TARGET-BUFFER is the optional target for the return value of the
evaluation.

\(fn START END &optional TARGET-BUFFER)" t nil)

(autoload (quote sawfish-eval-buffer) "sawfish" "\
Evaluate the whole buffer.

\(fn)" t nil)

(autoload (quote sawfish-eval-defun) "sawfish" "\
Evaluate the top level form at or near `point'.

INSERT-VALUE is a prefix parameter, if it is non-NIL the value of the
expression is inserted into the buffer after the form.

\(fn INSERT-VALUE)" t nil)

(autoload (quote sawfish-eval-expression) "sawfish" "\
Evaluate SEXP and display the value in the minibuffer.

If the optional parameter INSERT-VALUE is supplied as a non-NIL value the
value of SEXP will be inserted into the current buffer.

\(fn SEXP &optional INSERT-VALUE)" t nil)

(autoload (quote sawfish-eval-last-sexp) "sawfish" "\
Version of `eval-last-sexp' that interacts with sawfish.

\(fn TO-BUFFER)" t nil)

(autoload (quote sawfish-eval-print-last-sexp) "sawfish" "\
Not documented

\(fn)" t nil)

(autoload (quote sawfish-describe-function) "sawfish" "\
Display the doc-string for FUNCTION.

\(fn FUNCTION)" t nil)

(autoload (quote sawfish-describe-variable) "sawfish" "\
Display the doc-string for VARIABLE.

\(fn VARIABLE)" t nil)

(autoload (quote sawfish-info-function) "sawfish" "\
Display the Info documentation for FUNCTION.

\(fn FUNCTION)" t nil)

(autoload (quote sawfish-info-variable) "sawfish" "\
Display the Info documentation for VARIABLE.

\(fn VARIABLE)" t nil)

(autoload (quote sawfish-apropos) "sawfish" "\
Show all bound sawfish symbols whose names match REGEXP.

\(fn REGEXP)" t nil)

(autoload (quote sawfish-complete-symbol) "sawfish" "\
Attempt to complete the symbol at `point'.

\(fn)" t nil)

(autoload (quote sawfish-info) "sawfish" "\
View the sawfish info file.

\(fn)" t nil)

(autoload (quote sawfish-rep-info) "sawfish" "\
View the librep info file.

\(fn)" t nil)

(autoload (quote sawfish-console) "sawfish" "\
Run the sawfish client as an inferior lisp.

\(fn)" t nil)

(autoload (quote sawfish-interaction) "sawfish" "\
Create a sawfish interaction buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (ssh) "ssh" "ssh.el" (16944 35476))
;;; Generated autoloads from ssh.el
 (add-hook 'same-window-regexps "^\\*ssh-.*\\*\\(\\|<[0-9]+>\\)")

(autoload (quote ssh) "ssh" "\
Open a network login connection via `ssh' with args INPUT-ARGS.
INPUT-ARGS should start with a host name; it may also contain
other arguments for `ssh'.

Input is sent line-at-a-time to the remote connection.

Communication with the remote host is recorded in a buffer `*ssh-HOST*'
\(or `*ssh-USER@HOST*' if the remote username differs).
If a prefix argument is given and the buffer `*ssh-HOST*' already exists,
a new buffer with a different connection will be made.

When called from a program, if the optional second argument BUFFER is
a string or buffer, it specifies the buffer to use.

The variable `ssh-program' contains the name of the actual program to
run.  It can be a relative or absolute path.

The variable `ssh-explicit-args' is a list of arguments to give to
the ssh when starting.  They are prepended to any arguments given in
INPUT-ARGS.

If the default value of `ssh-directory-tracking-mode' is t, then the
default directory in that buffer is set to a remote (FTP) file name to
access your home directory on the remote machine.  Occasionally this causes
an error, if you cannot access the home directory on that machine.  This
error is harmless as long as you don't try to use that default directory.

If `ssh-directory-tracking-mode' is neither t nor nil, then the default
directory is initially set up to your (local) home directory.
This is useful if the remote machine and your local machine
share the same files via NFS.  This is the default.

If you wish to change directory tracking styles during a session, use the
function `ssh-directory-tracking-mode' rather than simply setting the
variable.

The variable `ssh-x-display-follow-current-frame' can be used to specify
how ssh X display tunelling interacts with frames on remote displays.

\(fn INPUT-ARGS &optional BUFFER)" t nil)

;;;***

;;;### (autoloads (swbuff-kill-this-buffer swbuff-switch-to-next-buffer
;;;;;;  swbuff-switch-to-previous-buffer) "swbuff" "swbuff.el" (16944
;;;;;;  35476))
;;; Generated autoloads from swbuff.el

(autoload (quote swbuff-switch-to-previous-buffer) "swbuff" "\
Switch to the previous buffer in the buffer list.

\(fn)" t nil)

(autoload (quote swbuff-switch-to-next-buffer) "swbuff" "\
Switch to the next buffer in the buffer list.

\(fn)" t nil)

(autoload (quote swbuff-kill-this-buffer) "swbuff" "\
Kill the current buffer.
And update the status window if showing.

\(fn)" t nil)

;;;***

;;;### (autoloads (tabbar-local-mode tabbar-mode tabbar-forward-tab
;;;;;;  tabbar-backward-tab tabbar-forward-group tabbar-backward-group
;;;;;;  tabbar-forward tabbar-backward) "tabbar" "tabbar.el" (16944
;;;;;;  35476))
;;; Generated autoloads from tabbar.el

(autoload (quote tabbar-backward) "tabbar" "\
Select the previous available tab.
Depend on the setting of the option `tabbar-cycling-scope'.

\(fn)" t nil)

(autoload (quote tabbar-forward) "tabbar" "\
Select the next available tab.
Depend on the setting of the option `tabbar-cycling-scope'.

\(fn)" t nil)

(autoload (quote tabbar-backward-group) "tabbar" "\
Go to selected tab in the previous available group.

\(fn)" t nil)

(autoload (quote tabbar-forward-group) "tabbar" "\
Go to selected tab in the next available group.

\(fn)" t nil)

(autoload (quote tabbar-backward-tab) "tabbar" "\
Select the previous visible tab.

\(fn)" t nil)

(autoload (quote tabbar-forward-tab) "tabbar" "\
Select the next visible tab.

\(fn)" t nil)

(defvar tabbar-mode nil "\
Non-nil if Tabbar mode is enabled.
See the command `tabbar-mode' for a description of this minor-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `tabbar-mode'.")

(custom-autoload (quote tabbar-mode) "tabbar")

(autoload (quote tabbar-mode) "tabbar" "\
Toggle display of a tab bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\(fn &optional ARG)" t nil)

(autoload (quote tabbar-local-mode) "tabbar" "\
Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When on and tab bar global mode is on, if a buffer local value of
`header-line-format' exists, it is saved, then the local header line
is killed to show the tab bar.  When off, the saved local value of the
header line is restored, hiding the tab bar.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (trivial-cite) "tc" "tc.el" (16944 35476))
;;; Generated autoloads from tc.el

(autoload (quote trivial-cite) "tc" "\
trivial-cite is a simple citation function for use in news/mailreaders.
It parses the headers via the functions defined in tc-header-funs, then
makes a attribution for the citation using tc-make-attribution and indents
the inserted text with tc-indent-citation.
Numeric prefix arguments is how many lines of body to cite (useful for citing
mails with long attachments).
Usage:  (auto-load 'trivial-cite \"tc\" t t)
        (add-hook 'mail-citation-hook 'trivial-cite)
Bugs:  Not very intelligent about old citation marks other than '>'.
Customization:  See variables tc-fill-column, tc-remove-signature,
tc-citation-string, tc-make-attribution and tc-header-funs.

\(fn)" nil nil)

;;;***

;;;### (autoloads (teyjus teyjus-edit-mode teyjus-mode) "teyjus"
;;;;;;  "teyjus.el" (17029 62782))
;;; Generated autoloads from teyjus.el

(autoload (quote teyjus-mode) "teyjus" "\
Major mode for interacting with an inferior teyjus process.
Return after the end of the process' output sends the text from the
    end of process to the end of the current line.
Return before end of process output copies rest of line to end (skipping
    the prompt) and sends it.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

\(fn)" t nil)

(autoload (quote teyjus-edit-mode) "teyjus" "\
Mode for editing Lambda Prolog Files

\(fn)" t nil)

(autoload (quote teyjus) "teyjus" "\
Run an inferior Prolog, with I/O through buffer *teyjus*.
If buffer exists but prolog process is not running, make new prolog.
If buffer exists and prolog process is running, just switch to buffer *teyjus*.
Program used comes from variable explicit-prolog-file-name,
 or (if that is nil) from the PROLOG environment variable.
If a file ~/.emacs_prolog exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in teyjus-mode, giving commands for sending input
and controlling the subjobs of the prolog.  See teyjus-mode.
See also variable lprolog-prompt-pattern.

\(Type \\[describe-mode] in the prolog buffer for a list of commands.)

\(fn)" t nil)

;;;***

;;;### (autoloads (timbuk-mode) "timbuk" "timbuk.el" (16944 35476))
;;; Generated autoloads from timbuk.el

(autoload (quote timbuk-mode) "timbuk" "\
Major mode for editing Timbuk code. \\<timbuk-mode-map>
TAB indents for Timbuk code.  Delete converts tabs to spaces as it moves back.

\\[timbuk-complete-word] completes the word around current point with respect to position in code
\\[timbuk-show-completions] shows all possible completions at this point.

Other useful functions are:

\\[timbuk-mark-defun]	- Mark function.
\\[timbuk-insert-block]	- insert begin ... end;
\\[timbuk-star-comment]	- insert (* ... *)
\\[timbuk-comment-area]	- Put marked area in a comment, fixing nested comments.
\\[timbuk-uncomment-area]	- Uncomment an area commented with \\[timbuk-comment-area].
\\[timbuk-beg-of-defun]	- Move to beginning of current function.
\\[timbuk-end-of-defun]	- Move to end of current function.
\\[timbuk-goto-defun]	- Goto function prompted for in the minibuffer.
\\[timbuk-outline]	- Enter timbuk-outline-mode (see also timbuk-outline).

Variables controlling indentation/edit style:

 timbuk-indent-level (default 3)
    Indentation of Timbuk statements with respect to containing block.
 timbuk-case-indent (default 2)
    Indentation for case statements.
 timbuk-auto-newline (default nil)
    Non-nil means automatically newline after semicolons and the punctuation
    mark after an end.
 timbuk-indent-nested-functions (default t)
    Non-nil means nested functions are indented.
 timbuk-tab-always-indent (default t)
    Non-nil means TAB in Timbuk mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 timbuk-auto-endcomments (default t)
    Non-nil means a comment { ... } is set after the ends which ends cases and
    functions. The name of the function or case will be set between the braces.
 timbuk-auto-lineup (default t)
    List of contexts where auto lineup of :'s or ='s should be done.

See also the user variables timbuk-type-keywords, timbuk-start-keywords and
timbuk-separator-keywords.

Turning on Timbuk mode calls the value of the variable timbuk-mode-hook with
no args, if that value is non-nil.

\(fn)" t nil)

;;;***

;;;### (autoloads (tf-run-with-idle-timer) "timerfunctions" "timerfunctions.el"
;;;;;;  (16944 35476))
;;; Generated autoloads from timerfunctions.el

(autoload (quote tf-run-with-idle-timer) "timerfunctions" "\
Args are SECS, REPEAT, REDOSECS, REDOREPEAT, INCLUDERUNTIME,
FUNCTION and &rest ARGS.
Similar to run-with-idle-timer, except that provides more options.
Suppose you want emacs to run an action every REDOSECS for as long as
emacs remains idle.  Think you can do it with the emacs'
run-with-idle-timer? Think again.. :)   That function will perform the
action exactly once every time emacs goes idle.  This funciton,
tf-run-with-idle-timer *will* allow you to keep performing an action
as long as emacs remains idle.

SECS is the number of seconds to wait once emacs has first gone
idle.

If REDOREPEAT is non-nil, the action is repeated as long emacs remains
idle.  REDOSECS is the number of additional seconds (after the action
has been done) to wait if emacs remains idle before performing the
action again. If INCLUDERUNTIME is non-nil, REDOSECS is the number of
additional seconds to wait after the action has been invoked (not
finished).

If REPEAT is nonnil, the entire cycle is repeated every time emacs
next goes idle.. (as in the default run-with-idle-timer.

\(fn SECS REPEAT REDOSECS REDOREPEAT INCLUDERUNTIME FUNCTION &rest ARGS)" nil nil)

;;;***

;;;### (autoloads (ub-install-example undo-browse ub-movie-history
;;;;;;  ub-movie-backward ub-mode-on ub-introduction) "undo-browse"
;;;;;;  "undo-browse.el" (16944 35476))
;;; Generated autoloads from undo-browse.el

(autoload (quote ub-introduction) "undo-browse" "\
Provides electric help from variable `undo-browse-introduction'.

\(fn)" t nil)

(autoload (quote ub-mode-on) "undo-browse" "\
Not documented

\(fn)" t nil)

(autoload (quote ub-movie-backward) "undo-browse" "\
Run a movie.  By default, backwards, with argument, forward.

\(fn &optional DIRECTION)" t nil)

(autoload (quote ub-movie-history) "undo-browse" "\
Not documented

\(fn)" t nil)

(autoload (quote undo-browse) "undo-browse" "\
Not documented

\(fn)" nil nil)

(defalias (quote undo-movie) (quote ub-movie-history))

(autoload (quote ub-install-example) "undo-browse" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (vcard-parse-region vcard-parse-string vcard-pretty-print
;;;;;;  vcard-standard-filters vcard-pretty-print-function) "vcard"
;;;;;;  "vcard.el" (16944 35476))
;;; Generated autoloads from vcard.el

(defvar vcard-pretty-print-function (quote vcard-format-sample-box) "\
*Formatting function used by `vcard-pretty-print'.")

(custom-autoload (quote vcard-pretty-print-function) "vcard")

(defvar vcard-standard-filters (quote (vcard-filter-html vcard-filter-adr-newlines vcard-filter-tel-normalize vcard-filter-textprop-cr)) "\
*Standard list of filters to apply to parsed vcard data.
These filters are applied sequentially to vcard attributes when
the function `vcard-standard-filter' is supplied as the second argument to
`vcard-parse'.")

(custom-autoload (quote vcard-standard-filters) "vcard")

(autoload (quote vcard-pretty-print) "vcard" "\
Format VCARD into a string suitable for display to user.
VCARD can be an unparsed string containing raw VCF vcard data
or a parsed vcard alist as returned by `vcard-parse-string'.

The result is a string with formatted vcard information suitable for
insertion into a mime presentation buffer.

The function specified by the variable `vcard-pretty-print-function'
actually performs the formatting.  That function will always receive a
parsed vcard alist.

\(fn VCARD)" nil nil)

(autoload (quote vcard-parse-string) "vcard" "\
Parse RAW vcard data as a string, and return an alist representing data.

If the optional function FILTER is specified, apply that filter to each
attribute.  If no filter is specified, `vcard-standard-filter' is used.

Filters should accept two arguments: the property list and the value list.
Modifying in place the property or value list will affect the resulting
attribute in the vcard alist.

Vcard data is normally in the form

    begin:                        vcard
    prop1a:                       value1a
    prop2a;prop2b;prop2c=param2c: value2a
    prop3a;prop3b:                value3a;value3b;value3c
    end:                          vcard

\(Whitespace around the `:' separating properties and values is optional.)
If supplied to this function an alist of the form

    (((\"prop1a\") \"value1a\")
     ((\"prop2a\" \"prop2b\" (\"prop2c\" . \"param2c\")) \"value2a\")
     ((\"prop3a\" \"prop3b\") \"value3a\" \"value3b\" \"value3c\"))

would be returned.

\(fn RAW &optional FILTER)" nil nil)

(autoload (quote vcard-parse-region) "vcard" "\
Parse the raw vcard data in region, and return an alist representing data.
This function is just like `vcard-parse-string' except that it operates on
a region of the current buffer rather than taking a string as an argument.

Note: this function modifies the buffer!

\(fn BEG END &optional FILTER)" nil nil)

;;;***

;;;### (autoloads (winring-rename-configuration winring-delete-configuration
;;;;;;  winring-jump-to-configuration winring-prev-configuration
;;;;;;  winring-next-configuration winring-duplicate-configuration
;;;;;;  winring-new-configuration) "winring" "winring.el" (16944
;;;;;;  35476))
;;; Generated autoloads from winring.el

(autoload (quote winring-new-configuration) "winring" "\
Save the current window configuration and create an empty new one.
The buffer shown in the new empty configuration is defined by
`winring-new-config-buffer-name'.

With \\[universal-argument] prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name.

\(fn &optional ARG)" t nil)

(autoload (quote winring-duplicate-configuration) "winring" "\
Push the current window configuration on the ring, and duplicate it.

With \\[universal-argument] prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name.

\(fn &optional ARG)" t nil)

(autoload (quote winring-next-configuration) "winring" "\
Switch to the next window configuration for this frame.

\(fn)" t nil)

(autoload (quote winring-prev-configuration) "winring" "\
Switch to the previous window configuration for this frame.

\(fn)" t nil)

(autoload (quote winring-jump-to-configuration) "winring" "\
Go to the named window configuration.

\(fn)" t nil)

(autoload (quote winring-delete-configuration) "winring" "\
Delete the current configuration and switch to the next one.
With \\[universal-argument] prompt for named configuration to delete.

\(fn &optional ARG)" t nil)

(autoload (quote winring-rename-configuration) "winring" "\
Rename the current configuration to NAME.

\(fn NAME)" t nil)

;;;***

;;;### (autoloads (zap-to-char zap-upto-char zap-following-char zap-from-char)
;;;;;;  "zap-char" "zap-char.el" (17029 62859))
;;; Generated autoloads from zap-char.el

(autoload (quote zap-from-char) "zap-char" "\
Kill from ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.

\(fn ARG CHAR)" t nil)

(autoload (quote zap-following-char) "zap-char" "\
Kill following ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.

\(fn ARG CHAR)" t nil)

(autoload (quote zap-upto-char) "zap-char" "\
Kill up to ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.

\(fn ARG CHAR)" t nil)

(autoload (quote zap-to-char) "zap-char" "\
Kill to ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.

\(fn ARG CHAR)" t nil)

;;;***

;;;### (autoloads nil nil ("auto-save.el" "autoloads.el" "c-font-lock-keywords.el"
;;;;;;  "color-eldoc.el" "color-moccur.el" "color-mode.el" "dircolors.el"
;;;;;;  "dired-details.el" "elscreen.el" "escreen.el" "flashcard.el"
;;;;;;  "fracc.el" "highlight-current-line.el" "http-post.el" "longlines.el"
;;;;;;  "message-x.el" "moccur-edit.el" "mtorus.el" "multi-region.el"
;;;;;;  "mutt.el" "mycode.el" "mycompletion.el" "nntodo.el" "osd.el"
;;;;;;  "patches.el" "post.el" "project.el" "setnu.el" "sig.el" "tempo-c++.el"
;;;;;;  "tempo-latex.el" "tempo-lisp.el" "todl-mode.el" "todoo.el"
;;;;;;  "typopunct.el" "visible-mark-mode.el" "visual.el" "xcscope.el"
;;;;;;  "xterm-extras.el") (17037 41983 335563))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; loaddefs.el ends here
