;;; loaddefs.el --- automatically extracted autoloads
;; To regenerate this file, simply C-x C-e the following expression
;; (let ((generated-autoload-file "lib-loaddefs.el")) (create-directory-autoloads "."))
;;; Code:
(provide 'lib-loaddefs)

;;;### (autoloads (set-modified-alist modify-alist remove-alist set-alist
;;;;;;  del-alist put-alist vassoc) "alist" "alist.el" (18543 20462))
;;; Generated autoloads from alist.el

(autoload 'vassoc "alist" "\
Search VALIST for a vector whose first element is equal to KEY.
See also `assoc'.

\(fn KEY VALIST)" nil nil)

(autoload 'put-alist "alist" "\
Modify ALIST to set VALUE to ITEM.
If there is a pair whose car is ITEM, replace its cdr by VALUE.
If there is not such pair, create new pair (ITEM . VALUE) and
return new alist whose car is the new pair and cdr is ALIST.
\[tomo's ELIS like function]

\(fn ITEM VALUE ALIST)" nil nil)

(autoload 'del-alist "alist" "\
If there is a pair whose key is ITEM, delete it from ALIST.
\[tomo's ELIS emulating function]

\(fn ITEM ALIST)" nil nil)

(autoload 'set-alist "alist" "\
Modify a alist indicated by SYMBOL to set VALUE to ITEM.

\(fn SYMBOL ITEM VALUE)" nil nil)

(autoload 'remove-alist "alist" "\
Remove ITEM from the alist indicated by SYMBOL.

\(fn SYMBOL ITEM)" nil nil)

(autoload 'modify-alist "alist" "\
Modify alist DEFAULT into alist MODIFIER.

\(fn MODIFIER DEFAULT)" nil nil)

(autoload 'set-modified-alist "alist" "\
Modify a value of a symbol SYM into alist MODIFIER.
The symbol SYM should be alist. If it is not bound,
its value regard as nil.

\(fn SYM MODIFIER)" nil nil)

;;;***

;;;### (autoloads (anchored-transpose) "anchored-transpose" "anchored-transpose.el"
;;;;;;  (18543 20462))
;;; Generated autoloads from anchored-transpose.el

(autoload 'anchored-transpose "anchored-transpose" "\
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
;;;;;;  "autoloads+" "autoloads+.el" (18932 3539))
;;; Generated autoloads from autoloads+.el

(autoload 'create-file-autoloads "autoloads+" "\
Update the autoloads for FILE in `generated-autoload-file'
\(which FILE might bind in its local variables).
If SAVE-AFTER is non-nil (which is always, when called interactively),
save the buffer too.

Return FILE if there was no autoload cookie in it, else nil.

\(fn FILE &optional SAVE-AFTER)" t nil)

(autoload 'create-directory-autoloads "autoloads+" "\
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
;;;;;;  (18543 20462))
;;; Generated autoloads from babel.el

(autoload 'babel "babel" "\
Use a web translation service to translate the message MSG.
Display the result in a buffer *babel* unless the optional argument
NO-DISPLAY is nil.

\(fn MSG &optional NO-DISPLAY)" t nil)

(autoload 'babel-region "babel" "\
Use a web translation service to translate the current region.

\(fn START END)" t nil)

(autoload 'babel-as-string "babel" "\
Use a web translation service to translate MSG, returning a string.

\(fn MSG)" t nil)

;;;***

;;;### (autoloads (bf-test bf-run bf-compile bf-execute) "bf" "bf.el"
;;;;;;  (18543 20462))
;;; Generated autoloads from bf.el

(autoload 'bf-execute "bf" "\
BrainFuck interpreter.
This is a internal function which assumes the necessary variables for
a BrainFuck machine are already bound and initialized.

\(fn STRING)" nil nil)

(autoload 'bf-compile "bf" "\
BrainFuck compiler.
STRING is the BrainFuck code to be compiled.
This function returns a byte-compiled function with no arguments.
To run the code, simply `funcall' the return value of this function.

\(fn STRING &optional NO-CACHE)" nil nil)

(autoload 'bf-run "bf" "\
Not documented

\(fn STRING)" t nil)

(autoload 'bf-test "bf" "\
A test for the BrainFuck compiler.
This function compiles a Brainfuck interpreter written in BrainFuck
to native emacs-lisp byte-code and execute the interpreter which itself
executes a simple brainfuck program.
NOTE: The BrainFuck interpreter written in BrainFuck is not written
by me.

\(fn)" t nil)

;;;***

;;;### (autoloads (blank-global-mode-off blank-global-mode-on blank-global-mode
;;;;;;  blank-mode-off blank-mode-on blank-mode blank-mode-customize)
;;;;;;  "blank-mode" "blank-mode.el" (18543 20467))
;;; Generated autoloads from blank-mode.el

(autoload 'blank-mode-customize "blank-mode" "\
Customize blank-mode options.

\(fn)" t nil)

(autoload 'blank-mode "blank-mode" "\
Toggle blank minor mode visualisation (bl on modeline).

If ARG is null, toggle blank visualisation.
If ARG is a number and is greater than zero, turn on visualisation; otherwise,
turn off visualisation.

\(fn &optional ARG)" t nil)

(autoload 'blank-mode-on "blank-mode" "\
Turn on blank minor mode visualisation (bl on modeline).

\(fn)" t nil)

(autoload 'blank-mode-off "blank-mode" "\
Turn off blank minor mode visualisation (bl on modeline).

\(fn)" t nil)

(autoload 'blank-global-mode "blank-mode" "\
Toggle blank global minor mode visualisation (BL on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system.

\(fn &optional ARG)" t nil)

(autoload 'blank-global-mode-on "blank-mode" "\
Turn on blank global minor mode visualisation (BL on modeline).

\(fn)" t nil)

(autoload 'blank-global-mode-off "blank-mode" "\
Turn off blank global minor mode visualisation (BL on modeline).

\(fn)" t nil)

;;;***

;;;### (autoloads (turn-on-jde-blockcomment-mode turn-on-blockcomment-mode
;;;;;;  jde-blockcomment-mode blockcomment-mode) "block-comm" "block-comm.el"
;;;;;;  (18543 20462))
;;; Generated autoloads from block-comm.el

(autoload 'blockcomment-mode "block-comm" "\
Toggle Blockcomment mode.
With a prefix ARG, enable blockcomment mode iff arg is nonzero.

\(fn ARG)" t nil)

(autoload 'jde-blockcomment-mode "block-comm" "\
Toggle Blockcomment mode with JDE support.
This will call `jde-javadoc-autodoc-at-line' when a new comment is
created.  With a prefix ARG, enable blockcomment mode iff arg is
nonzero.

\(fn ARG)" t nil)

(autoload 'turn-on-blockcomment-mode "block-comm" "\
Turn on Blockcomment mode.
Useful for adding to a major mode hook variable.
Example:
    (add-hook 'c-mode-hook 'turn-on-blockcomment-mode)
to automatically turn on blockcomment mode when opening a C source
file.

\(fn)" nil nil)

(autoload 'turn-on-jde-blockcomment-mode "block-comm" "\
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
;;;;;;  "boxquote" "boxquote.el" (18543 20462))
;;; Generated autoloads from boxquote.el

(autoload 'boxquote-title "boxquote" "\
Set the title of the current boxquote to TITLE.

If TITLE is an empty string the title is removed. Note that the title will
be formatted using `boxquote-title-format'.

\(fn TITLE)" t nil)

(autoload 'boxquote-region "boxquote" "\
Draw a box around the left hand side of a region bounding START and END.

\(fn START END)" t nil)

(autoload 'boxquote-buffer "boxquote" "\
Apply `boxquote-region' to a whole buffer.

\(fn)" t nil)

(autoload 'boxquote-insert-file "boxquote" "\
Insert the contents of a file, boxed with `boxquote-region'.

If `boxquote-title-files' is non-nil the boxquote will be given a title that
is the result applying `boxquote-file-title-funciton' to FILENAME.

\(fn FILENAME)" t nil)

(autoload 'boxquote-kill-ring-save "boxquote" "\
Like `kill-ring-save' but remembers a title if possible.

The title is acquired by calling `boxquote-kill-ring-save-title'. The title
will be used by `boxquote-yank'.

\(fn)" t nil)

(autoload 'boxquote-yank "boxquote" "\
Do a `yank' and box it in with `boxquote-region'.

If the yanked entry was placed on the kill ring with
`boxquote-kill-ring-save' the resulting boxquote will be titled with
whatever `boxquote-kill-ring-save-title' returned at the time.

\(fn)" t nil)

(autoload 'boxquote-defun "boxquote" "\
Apply `boxquote-region' the current defun.

\(fn)" t nil)

(autoload 'boxquote-paragraph "boxquote" "\
Apply `boxquote-region' to the current paragraph.

\(fn)" t nil)

(autoload 'boxquote-boxquote "boxquote" "\
Apply `boxquote-region' to the current boxquote.

\(fn)" t nil)

(autoload 'boxquote-describe-function "boxquote" "\
Call `describe-function' and boxquote the output into the current buffer.

\(fn)" t nil)

(autoload 'boxquote-describe-variable "boxquote" "\
Call `describe-variable' and boxquote the output into the current buffer.

\(fn)" t nil)

(autoload 'boxquote-describe-key "boxquote" "\
Call `describe-key' and boxquote the output into the current buffer.

\(fn KEY)" t nil)

(autoload 'boxquote-shell-command "boxquote" "\
Call `shell-command' with COMMAND and boxquote the output.

\(fn COMMAND)" t nil)

(autoload 'boxquote-text "boxquote" "\
Insert TEXT, boxquoted.

\(fn TEXT)" t nil)

(autoload 'boxquote-narrow-to-boxquote "boxquote" "\
Narrow the buffer to the current boxquote.

\(fn)" t nil)

(autoload 'boxquote-narrow-to-boxquote-content "boxquote" "\
Narrow the buffer to the content of the current boxquote.

\(fn)" t nil)

(autoload 'boxquote-kill "boxquote" "\
Kill the boxquote and its contents.

\(fn)" t nil)

(autoload 'boxquote-fill-paragraph "boxquote" "\
Perform a `fill-paragraph' inside a boxquote.

\(fn ARG)" t nil)

(autoload 'boxquote-unbox-region "boxquote" "\
Remove a box created with `boxquote-region'.

\(fn START END)" t nil)

(autoload 'boxquote-unbox "boxquote" "\
Remove the boxquote that contains `point'.

\(fn)" t nil)

;;;***

;;;### (autoloads (bst-mode) "bst" "bst.el" (18543 20462))
;;; Generated autoloads from bst.el

(autoload 'bst-mode "bst" "\
Setup for BibTeX style file editing.

\(fn)" t nil)

;;;***

;;;### (autoloads (Buffer-menu-mouse-execute Buffer-menu-mouse-modified
;;;;;;  Buffer-menu-mouse-delete Buffer-menu-mouse-save Buffer-menu-mouse-unmark
;;;;;;  Buffer-menu-mouse-other-window Buffer-menu-mouse-3-menu Buffer-menu-select
;;;;;;  Buffer-menu-execute Buffer-menu-delete-flagged Buffer-menu-mode
;;;;;;  buffer-menu) "buff-menu+" "buff-menu+.el" (18543 20473))
;;; Generated autoloads from buff-menu+.el

(unless (> emacs-major-version 21) (defgroup Buffer-Menu-Plus nil "Enhancements to the buffer menu." :link `(url-link :tag "Send Bug Report" ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=buff-menu+.el bug: &body=Describe bug here, starting with `emacs -q'.  Don't forget to mention your Emacs and library versions.")) :link '(url-link :tag "Other Libraries by Drew" "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries") :link '(url-link :tag "Download" "http://www.emacswiki.org/cgi-bin/wiki/buff-menu+.el") :link '(url-link :tag "Description" "http://www.emacswiki.org/cgi-bin/wiki/BufferMenu#BufferMenuPlus") :link '(emacs-commentary-link :tag "Commentary" "buff-menu+") :group 'tools :group 'convenience))

(when (> emacs-major-version 21) (defgroup Buffer-Menu-Plus nil "Enhancements to the buffer menu." :link `(url-link :tag "Send Bug Report" ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=buff-menu+.el bug: &body=Describe bug here, starting with `emacs -q'.  Don't forget to mention your Emacs and library versions.")) :link '(url-link :tag "Other Libraries by Drew" "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries") :link '(url-link :tag "Download" "http://www.emacswiki.org/cgi-bin/wiki/buff-menu+.el") :link '(url-link :tag "Description" "http://www.emacswiki.org/cgi-bin/wiki/BufferMenu#BufferMenuPlus") :link '(emacs-commentary-link :tag "Commentary" "buff-menu+") :group 'Buffer-menu :group 'tools :group 'convenience) (defvar Buffer-menu-buffer+size-computed-width 0 "Max width of all buffer names, plus 4 for initial `CRM '.") (defun buffer-menu-increase-max-buffer+size nil "Increase option `Buffer-menu-buffer+size-width' by one." (interactive) (when (> (1+ Buffer-menu-buffer+size-width) 150) (error "Cannot increase further")) (setq Buffer-menu-buffer+size-width (1+ Buffer-menu-buffer+size-width)) (buffer-menu) (message "New max width: %s" Buffer-menu-buffer+size-width)) (defun buffer-menu-decrease-max-buffer+size nil "Decrease option `Buffer-menu-buffer+size-width' by one." (interactive) (let ((orig Buffer-menu-buffer+size-width)) (condition-case nil (progn (setq Buffer-menu-buffer+size-width (1- Buffer-menu-buffer+size-width)) (buffer-menu) (message "New max width: %s" Buffer-menu-buffer+size-width)) (error (progn (setq Buffer-menu-buffer+size-width orig) (buffer-menu) (error "Cannot decrease further")))))) (define-key Buffer-menu-mode-map "+" 'buffer-menu-increase-max-buffer+size) (define-key Buffer-menu-mode-map "-" 'buffer-menu-decrease-max-buffer+size) (define-key Buffer-menu-mode-map "\230" 'Buffer-menu-delete-flagged) (defun buffer-menu-toggle-time-format nil "Toggle `Buffer-menu-time-format' and redisplay Buffer Menu." (interactive) (buffer-menu-set-default-value 'Buffer-menu-time-format (if (eq 'short Buffer-menu-time-format) 'long 'short)) (buffer-menu)) (defun buffer-menu-toggle-time-column nil "Toggle `Buffer-menu-time-flag' and redisplay Buffer Menu." (interactive) (buffer-menu-set-default-value 'Buffer-menu-time-flag (not Buffer-menu-time-flag)) (buffer-menu)) (defun buffer-menu-toggle-mode-column nil "Toggle `Buffer-menu-mode-flag' and redisplay Buffer Menu." (interactive) (buffer-menu-set-default-value 'Buffer-menu-mode-flag (not Buffer-menu-mode-flag)) (buffer-menu)) (defun buffer-menu-toggle-file-column nil "Toggle `Buffer-menu-file-flag' and redisplay Buffer Menu." (interactive) (buffer-menu-set-default-value 'Buffer-menu-file-flag (not Buffer-menu-file-flag)) (buffer-menu)) (defun buffer-menu-set-default-value (symb val) "Set default value of SYMB to VAL.\nUpdate `buffer-menu-font-lock-keywords' accordingly." (set-default symb val) (setq buffer-menu-font-lock-keywords (buffer-menu-font-lock-keywords))) (defun buffer-menu-font-lock-keywords nil "Returns the list of font lock keywords for the buffer menu." (list (list "^\\(CRM.*\\)" 1 'buffer-menu-headings) (list "^....\\(.*[^ 	\n]\\)[ 	]+[0-9]+[ 	]+\\(.* \\(AM\\|PM\\)\\)?\\([^/\n]+\\)" (list 1 'buffer-menu-buffer-name)) (cond (Buffer-menu-mode-flag (list "^....\\(.*[^ 	\n]\\)[ 	]+[0-9]+[ 	]+[a-zA-Z :0-9]*[ 	]+Dired" 1 'buffer-menu-directory-buffer t t)) (Buffer-menu-file-flag (list "^....\\(.*[^ 	\n]\\)[ 	]+[0-9]+[ 	]+[^/\n]+[ 	\n]\\(\\([~]\\|\\([a-zA-Z]:\\)\\)?/.*/\\)$" 1 'buffer-menu-directory-buffer t t)) (t (list ""))) (list "^....\\(\\*.*[^ 	\n]\\*\\(<[0-9]+>\\)?\\)[ 	]+" 1 'buffer-menu-star-buffer t t) (cond ((and Buffer-menu-time-flag Buffer-menu-mode-flag (eq 'short Buffer-menu-time-format)) (list "^.*[ 	][0-9]+[ 	]+\\([0-2][0-9]:[0-5][0-9]:[0-5][0-9]\\)?\\([^\n]+\\)" (list 1 'buffer-menu-time t t) (list 2 'buffer-menu-mode t t))) ((and Buffer-menu-time-flag Buffer-menu-mode-flag) (list "^.*[ 	][0-9]+[ 	]+\\(.* \\(AM\\|PM\\)\\)?\\([^\n]+\\)" (list 1 'buffer-menu-time t t) (list 3 'buffer-menu-mode t t))) (Buffer-menu-time-flag (if (eq 'short Buffer-menu-time-format) (list "^.*[ 	][0-9]+[ 	]+\\([0-2][0-9]:[0-5][0-9]:[0-5][0-9]\\)?" (list 1 'buffer-menu-time t t)) (list "^.*[ 	][0-9]+[ 	]+\\(.* \\(AM\\|PM\\)\\)?" (list 1 'buffer-menu-time t t)))) (Buffer-menu-mode-flag (list "^.*[ 	][0-9]+[ 	]+\\([^/\n]+\\)" 1 'buffer-menu-mode t t)) (t "")) (list "^.*[ 	]\\([0-9]+\\)\\(  \\|[\n]\\)" 1 'buffer-menu-size t t) (if Buffer-menu-file-flag (list "^.*[ 	][0-9]+[ 	]+[^/\n]+[ 	\n]\\(\\(\\([~]\\|\\([a-zA-Z]:\\)\\)*/.*\\)\\|([^ 	]+).*\\)$" 1 'buffer-menu-file-name t t) "") (list "^\\([.]\\)" 1 'buffer-menu-current-buffer t t) (list "^\\(>\\)" 1 'buffer-menu-view-mark t t) (list "^>...\\(.*[^ 	\n]\\)[ 	]+[0-9]+[ 	]+\\(.* \\(AM\\|PM\\)\\)?\\([^/\n]+\\)" (list 1 'buffer-menu-marked-buffer 'prepend t)) (list "^D...\\(.*[^ 	\n]\\)[ 	]+[0-9]+[ 	]+\\(.* \\(AM\\|PM\\)\\)?\\([^/\n]+\\)" (list 1 'buffer-menu-flagged-buffer t t)) (list "^\\(D\\)" 1 'buffer-menu-delete-mark t t) (list "^..\\(S\\)" 1 'buffer-menu-save-mark t t) (list "^..\\([*]\\)" 1 'buffer-menu-modified-mark t t) (list "^.\\(%\\)" 1 'buffer-menu-read-only-mark t t))) (defcustom Buffer-menu-time-format 'short "*Format for Time column of buffer menu." :type '(choice (const :tag "Short: hh:mm:ss" short) (const :tag "Long: day hh:mm:ss AM/PM" long)) :group 'Buffer-Menu-Plus) (defcustom Buffer-menu-time-flag t "*Non-nil means Buffer Menu displays the last time the buffer was displayed." :type 'boolean :group 'Buffer-Menu-Plus :initialize 'custom-initialize-default :set 'buffer-menu-set-default-value) (defcustom Buffer-menu-mode-flag t "*Non-nil means Buffer Menu displays the buffer's mode." :type 'boolean :group 'Buffer-Menu-Plus :initialize 'custom-initialize-default :set 'buffer-menu-set-default-value) (defcustom Buffer-menu-file-flag t "*Non-nil means Buffer Menu displays the buffer's file." :type 'boolean :group 'Buffer-Menu-Plus :initialize 'custom-initialize-default :set 'buffer-menu-set-default-value) (defcustom Buffer-menu-sort-column 1 "Sorted by (1) visit, (2) buffer, (3) size, (4) time, (5) mode, (6) file.\nClick a column heading to sort by that field and update this option." :type '(choice (const :tag "Sort by time of last visit" 1) (const :tag "Sort by buffer name" 2) (const :tag "Sort by buffer size" 3) (const :tag "Sort by time of last use" 4) (const :tag "Sort by file name" 5)) :group 'Buffer-Menu-Plus) (setq Buffer-menu-sort-column (or Buffer-menu-sort-column 1)) (defun Buffer-menu-buffer+size (name size &optional name-props size-props) (if (> (+ (length name) (length size) 1) Buffer-menu-buffer+size-computed-width) (setq name (if (string-match "<[0-9]+>$" name) (concat (substring name 0 (- Buffer-menu-buffer+size-computed-width (max (length size) 3) (match-end 0) (- (match-beginning 0)) 2)) "[" (match-string 0 name)) (concat (substring name 0 (- Buffer-menu-buffer+size-computed-width (max (length size) 3) 2)) "["))) (setq name (copy-sequence name))) (when name-props (add-text-properties 0 (length name) name-props name)) (when size-props (add-text-properties 0 (length size) size-props size)) (concat name (make-string (- Buffer-menu-buffer+size-computed-width (length name) (length size)) 8388640) size)))

(autoload 'buffer-menu "buff-menu+" "\
Show a menu to let you save, delete or select buffers.
By default (no or null prefix arg), the buffers are listed in order of
last access (visit).  With a non-nil prefix ARG:
  ARG >= 0  means only buffers visiting files are listed.
  ARG <= 0  means the buffers are listed alphabetically.
 (ARG = 0   means only buffers visiting files, listed alphabetically.)

Type `?' in buffer \"*Buffer List*\" for more information.
Type `q' there to quit the buffer menu.

\(fn &optional ARG)" t nil)

(autoload 'Buffer-menu-mode "buff-menu+" "\
Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
In Buffer menu mode, chars do not insert themselves, but are commands.
\\<Buffer-menu-mode-map>
\(\"Current line\" here is the line of the text cursor or the mouse.)


Display Options
---------------
Click `mouse-3' for a context-sensitive menu of buffer operations.

These features are available for Emacs 22 and later:

* You can click a column heading to sort by that column.  Clicking
  again reverses the sort direction.  The current sort column is
  indicated by an underlined or overlined column heading.  Sorting by
  column `CRM' depends on the value of option
  `Buffer-menu-use-frame-buffer-list'.

* You can resize the Buffer and Size columns using `+' and `-'.

* You can toggle the display of columns Time, Mode, and File using
  commands `buffer-menu-toggle-time-column',
  `buffer-menu-toggle-mode-column', and
  `buffer-menu-toggle-file-column'.  You can toggle the Time format
  using command `buffer-menu-toggle-time-format'.

Column `CRM':
 `C' shows `>' if you have marked the buffer to be displayed,
           `D' if you have marked it for deletion, and
           `.' for the buffer from which you came (current).
 `R' shows `%' if the buffer is read-only.
 `M' shows `*' if the buffer is modified, and
           `S' if you have marked it for saving.

The other columns are the Buffer name, its Size in characters, the
last Time the buffer was displayed, its major Mode, and the visited
File name (if any).

Displaying Buffers
------------------
\\[Buffer-menu-mouse-select], \\[Buffer-menu-select], \\[Buffer-menu-this-window] -- Select current line's buffer.
\\[Buffer-menu-mark]	-- Mark current line's buffer `>' to be displayed (via `\\[Buffer-menu-select]').
\\[Buffer-menu-select]	-- Show buffers marked `>'.  Select current line's buffer.
\\[Buffer-menu-1-window]	-- Select current line's buffer (only) in a full-frame window.
\\[Buffer-menu-2-window]	-- Select current line's buffer in one window.
	   Display previous buffer in a second window.
\\[Buffer-menu-switch-other-window]	-- Display current line's buffer in another window.  No select.
\\[Buffer-menu-view]	-- select current line's buffer, but in view-mode.
\\[Buffer-menu-view-other-window]	-- select that buffer in
  another window, in view-mode.
\\[Buffer-menu-toggle-files-only]	-- toggle whether the menu displays only file buffers.

Marking/Unmarking Buffers to be Saved/Deleted
---------------------------------------------
\\[Buffer-menu-save]	-- Mark current line's buffer `S' to be saved.    Cursor down.
\\[Buffer-menu-delete]	-- Mark current line's buffer `D' to be deleted.  Cursor down.
\\[Buffer-menu-delete-backwards]	-- Mark current line's buffer `D' to be deleted.  Cursor up.
\\[Buffer-menu-unmark]	-- Unmark current line.  Cursor down. (Prefix arg: Cursor up.)
\\[Buffer-menu-backup-unmark]	-- Cursor up, then unmark line.

Saving/Deleting Buffers
-----------------------
\\[Buffer-menu-execute]	-- Save / Delete marked buffers (marks `S', `D').
\\[Buffer-menu-delete-flagged]	-- Delete all buffers marked `D', even if modified.

Miscellaneous
-------------
\\[Buffer-menu-revert]	-- Update the list of buffers.
\\[Buffer-menu-not-modified]	-- Clear modified-flag on current line's buffer.
\\[Buffer-menu-toggle-read-only]	-- Toggle read-only status of current line's buffer.
\\[Buffer-menu-visit-tags-table]	-- `visit-tags-table' using current line's buffer.


Bindings in Buffer Menu mode:
----------------------------

\\{Buffer-menu-mode-map}

\(fn)" nil nil)

(autoload 'Buffer-menu-delete-flagged "buff-menu+" "\
Delete all buffers marked `D', even if they have been modified.
If there are any file buffers that have been modified since the last
save, then you must confirm the deletion of all at once.

You can mark a buffer for deletion (`D') using command `\\<Buffer-menu-mode-map>\\[Buffer-menu-delete]'.

\(fn)" t nil)

(autoload 'Buffer-menu-execute "buff-menu+" "\
Save or delete buffers marked `S' (\"save\") or `D' (\"delete\").
Buffers can be so marked using commands `\\<Buffer-menu-mode-map>\\[Buffer-menu-save]' and `\\[Buffer-menu-delete]', respectively.

\(fn)" t nil)

(autoload 'Buffer-menu-select "buff-menu+" "\
Select this line's buffer; also display buffers marked with `>'.
You can mark buffers with command `\\<Buffer-menu-mode-map>\\[Buffer-menu-mark]'.
If the window is `window-dedicated-p', then another window is used;
else, all windows previously in the frame are replaced by this one.

\(fn)" t nil)

(when (> emacs-major-version 21) (defun Buffer-menu-sort (column) "Sort the buffer menu by COLUMN.\nConsecutive executions of the same COLUMN reverse the sort order." (interactive "P") (when column (setq column (prefix-numeric-value column)) (when (= column 0) (setq column 1)) (when (> column 6) (setq column 6)) (when (< column -6) (setq column -6))) (if (equal Buffer-menu-sort-column column) (setq Buffer-menu-sort-column (- column)) (setq Buffer-menu-sort-column column)) (let (buffer-read-only l buf m1 m2) (save-excursion (Buffer-menu-beginning) (while (not (eobp)) (when (buffer-live-p (setq buf (get-text-property (+ (point) 4) 'buffer))) (setq m1 (char-after) m1 (if (memq m1 '(62 68)) m1) m2 (char-after (+ (point) 2)) m2 (if (eq m2 83) m2)) (if (or m1 m2) (push (list buf m1 m2) l))) (forward-line))) (Buffer-menu-revert) (setq buffer-read-only) (save-excursion (Buffer-menu-beginning) (while (not (eobp)) (when (setq buf (assq (get-text-property (+ (point) 4) 'buffer) l)) (setq m1 (cadr buf) m2 (cadr (cdr buf))) (when m1 (delete-char 1) (insert m1) (backward-char 1)) (when m2 (forward-char 2) (delete-char 1) (insert m2))) (forward-line)))) (message "Buffers are now sorted %s%s." (case (abs column) (1 "by time of last visit - see `Buffer-menu-use-frame-buffer-list'") (2 "by buffer name") (3 "by size") (4 "by time of last display") (5 "by major-mode name") (otherwise "by associated file (including path)")) (if (natnump Buffer-menu-sort-column) ", ascending" ", descending"))))

(when (> emacs-major-version 21) (defun Buffer-menu-make-sort-button (name button-column) (let ((the-sort-column-p nil)) (when (equal button-column (abs Buffer-menu-sort-column)) (setq the-sort-column-p t) (setq button-column (- button-column))) (propertize name 'column button-column 'help-echo (case (abs button-column) (1 (if Buffer-menu-use-header-line "mouse-1, mouse-2: sort by time of last visit - see `Buffer-menu-use-frame-buffer-list'" "mouse-2, RET: sort by time of last visit - see `Buffer-menu-use-frame-buffer-list'")) (2 (if Buffer-menu-use-header-line "mouse-1, mouse-2: sort by buffer name" "mouse-2, RET: sort by buffer name")) (4 "mouse-1, mouse-2: sort by time of last display/access") (t (if Buffer-menu-use-header-line (concat "mouse-1, mouse-2: sort by " (downcase name)) (concat "mouse-2, RET: sort by " (downcase name))))) 'mouse-face 'highlight (when the-sort-column-p 'face) (when the-sort-column-p (if (natnump Buffer-menu-sort-column) '(:underline t) '(:overline t))) 'keymap Buffer-menu-sort-button-map))))

(when (> emacs-major-version 21) (defun list-buffers-noselect (&optional files-only buffer-list) "Create and return a buffer with a list of names of existing buffers.\nThe buffer is named `*Buffer List*'.\nNote that buffers with names starting with spaces are omitted.\nNon-null optional arg FILES-ONLY means mention only file buffers.\n\nIf BUFFER-LIST is non-nil, it should be a list of buffers;\nit means list those buffers and no others.\n\nFor more information, see the function `buffer-menu'." (let ((len 0) buf+size) (setq Buffer-menu-buffer+size-computed-width Buffer-menu-buffer+size-width) (dolist (buffer (buffer-list)) (setq buf+size (concat (buffer-name buffer) (number-to-string (buffer-size buffer)))) (when (and (not (string= (substring buf+size 0 1) " ")) (> (length buf+size) len)) (setq len (length buf+size)))) (when (< (+ len 1) Buffer-menu-buffer+size-width) (setq Buffer-menu-buffer+size-computed-width (+ len 1)))) (let* ((old-buffer (current-buffer)) (standard-output standard-output) (mode-end (if Buffer-menu-mode-flag (make-string (- Buffer-menu-mode-width 4) 32) "")) (header (concat (Buffer-menu-make-sort-button "CRM" 1) " " (Buffer-menu-buffer+size (Buffer-menu-make-sort-button "Buffer" 2) (Buffer-menu-make-sort-button "Size" 3)) "  " (and Buffer-menu-time-flag (if (eq 'short Buffer-menu-time-format) (Buffer-menu-make-sort-button "Time   " 4) (Buffer-menu-make-sort-button "Time          " 4))) (and Buffer-menu-time-flag "   ") (and Buffer-menu-mode-flag (Buffer-menu-make-sort-button (concat "Mode" mode-end) 5)) (if Buffer-menu-mode-flag (if Buffer-menu-time-flag " " "  ") (and (not Buffer-menu-time-flag) " ")) (and Buffer-menu-file-flag (Buffer-menu-make-sort-button "File           " 6)) "\n")) list desired-point name buffer-time mode file) (when (and (boundp 'Buffer-menu-use-header-line) Buffer-menu-use-header-line) (let ((pos 0)) (while (string-match "[ 	\n]+" header pos) (setq pos (match-end 0)) (put-text-property (match-beginning 0) pos 'display (list 'space :align-to pos) header) (put-text-property (match-beginning 0) (1- pos) 'mouse-face 'highlight header))) (setq header (concat (propertize " " 'display '(space :align-to 0)) header))) (with-current-buffer (get-buffer-create "*Buffer List*") (setq buffer-read-only nil) (erase-buffer) (setq standard-output (current-buffer)) (unless (and (boundp 'Buffer-menu-use-header-line) Buffer-menu-use-header-line) (let ((underline (eval-when-compile (if (> emacs-major-version 21) 8212 45)))) (insert header (apply 'string (mapcar (lambda (c) (if (memq c '(10 32)) c underline)) header))))) (if buffer-list (setq list buffer-list) (dolist (buffer (or buffer-list (buffer-list (and Buffer-menu-use-frame-buffer-list (selected-frame))))) (with-current-buffer buffer (let ((name (buffer-name)) (file buffer-file-name)) (unless (and (not buffer-list) (or (and (string= (substring name 0 1) " ") (null file)) (and files-only (not file)) (string= name "*Buffer List*"))) (let ((buffer-time (and Buffer-menu-time-flag (cons (or (float-time buffer-display-time) 0) (if buffer-display-time (format-time-string (if (eq 'short Buffer-menu-time-format) "%02H:%02M:%02S" "%_3a %_2l:%02M:%02S %_2p") buffer-display-time) (if (eq 'short Buffer-menu-time-format) "        " "               "))))) (mode (concat (if (string-match "22." emacs-version) (format-mode-line mode-name nil nil buffer) (format-mode-line mode-name)) (and mode-line-process (if (string-match "22." emacs-version) (format-mode-line mode-line-process nil nil buffer) (format-mode-line mode-line-process))))) (bits (string (if (eq buffer old-buffer) 46 32) (if (or (eq buffer standard-output) buffer-read-only) 37 32) (if (buffer-modified-p) 42 32) 32))) (unless file (cond ((and (boundp 'list-buffers-directory) list-buffers-directory) (setq file list-buffers-directory)) ((eq major-mode 'Info-mode) (setq file Info-current-file) (cond ((equal file "dir") (setq file "*Info Directory*")) ((eq file 'apropos) (setq file "*Info Apropos*")) ((eq file 'history) (setq file "*Info History*")) ((eq file 'toc) (setq file "*Info TOC*")) ((not (stringp file)) (setq file nil)) (t (setq file (concat "(" (file-name-nondirectory file) ")" Info-current-node))))))) (push (list buffer bits name (buffer-size) buffer-time mode file) list)))))) (unless (eq -1 Buffer-menu-sort-column) (setq list (nreverse list)))) (dolist (buffer (if (eq 1 (abs Buffer-menu-sort-column)) list (let* ((descending-p (natnump Buffer-menu-sort-column)) (Buffer-menu-sort-column (abs Buffer-menu-sort-column))) (sort list (cond ((eq Buffer-menu-sort-column 3) (if descending-p (lambda (a b) (< (nth 3 a) (nth 3 b))) (lambda (a b) (< (nth 3 b) (nth 3 a))))) ((eq Buffer-menu-sort-column 4) (if descending-p (lambda (a b) (< (car (nth 4 a)) (car (nth 4 b)))) (lambda (a b) (< (car (nth 4 b)) (car (nth 4 a)))))) (t (if descending-p (lambda (a b) (string< (nth Buffer-menu-sort-column a) (nth Buffer-menu-sort-column b))) (lambda (a b) (string< (nth Buffer-menu-sort-column b) (nth Buffer-menu-sort-column a)))))))))) (when (eq (car buffer) old-buffer) (setq desired-point (point))) (insert (cadr buffer) (Buffer-menu-buffer+size (nth 2 buffer) (int-to-string (nth 3 buffer)) `(buffer-name ,(nth 2 buffer) buffer ,(car buffer) ,(if (> emacs-major-version 21) 'font-lock-face 'face) ,(if (facep 'Buffer-menu-buffer-face) 'Buffer-menu-buffer-face 'Buffer-menu-buffer) mouse-face highlight help-echo "mouse-2: select this buffer"))) (when Buffer-menu-time-flag (insert "  " (cdr (nth 4 buffer)))) (when Buffer-menu-mode-flag (insert "  " (if (> (length (nth 5 buffer)) Buffer-menu-mode-width) (substring (nth 5 buffer) 0 Buffer-menu-mode-width) (nth 5 buffer)))) (when (and Buffer-menu-file-flag (nth 6 buffer)) (indent-to (+ Buffer-menu-buffer-column Buffer-menu-buffer+size-computed-width (if Buffer-menu-mode-flag (1+ Buffer-menu-mode-width) 0) (if Buffer-menu-time-flag (if (eq 'short Buffer-menu-time-format) 9 16) 0) 3) 1) (princ (abbreviate-file-name (nth 6 buffer)))) (princ "\n")) (Buffer-menu-mode) (when (and (boundp 'Buffer-menu-use-header-line) Buffer-menu-use-header-line) (setq header-line-format header)) (goto-char (or desired-point (point-min))) (setq Buffer-menu-files-only files-only) (set-buffer-modified-p nil) (current-buffer)))))

(autoload 'Buffer-menu-mouse-3-menu "buff-menu+" "\
Pop up menu for Mouse-3 for buffer listed in buffer menu.

\(fn EVENT)" t nil)

(autoload 'Buffer-menu-mouse-other-window "buff-menu+" "\
Select, in another window, the buffer on whose line you click.

\(fn EVENT)" t nil)

(autoload 'Buffer-menu-mouse-unmark "buff-menu+" "\
Cancel all requested operations on buffer.

\(fn EVENT)" t nil)

(autoload 'Buffer-menu-mouse-save "buff-menu+" "\
Mark buffer to be saved.
Actual deletion is done via `\\<Buffer-menu-mode-map>\\[Buffer-menu-execute]' or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-execute]'.

\(fn EVENT)" t nil)

(autoload 'Buffer-menu-mouse-delete "buff-menu+" "\
Mark buffer to be deleted.
Actual deletion is done via `\\<Buffer-menu-mode-map>\\[Buffer-menu-execute]' or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-execute]'.

\(fn EVENT)" t nil)

(autoload 'Buffer-menu-mouse-modified "buff-menu+" "\
Mark buffer as unmodified (no changes to save) if modified, and vice versa.

\(fn EVENT)" t nil)

(autoload 'Buffer-menu-mouse-execute "buff-menu+" "\
Save and/or delete buffers marked `S' or `D', respectively.
Buffers can be marked via commands `\\<Buffer-menu-mode-map>\\[Buffer-menu-save]' and `\\<Buffer-menu-mode-map>\\[Buffer-menu-delete]'
\(or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-save]' and `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-delete]').

\(fn EVENT)" t nil)

;;;***

;;;### (autoloads (cparen-activate) "cparen" "cparen.el" (18543 20469))
;;; Generated autoloads from cparen.el

(autoload 'cparen-activate "cparen" "\
Activate coloured parentheses in Lisp modes.
You should also enable `font-lock-mode'.
Please note that there is currently no way to disable cparen, except
by turning `font-lock-mode' off completely.
Also, this function affects only buffers created after it was run;
use \\[normal-mode] after this to enable the colours in an existing
buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (crontab-get crontab-mode) "crontab-mode" "crontab-mode.el"
;;;;;;  (18543 20465))
;;; Generated autoloads from crontab-mode.el

(autoload 'crontab-mode "crontab-mode" "\
Major mode for editing crontabs.
Defines commands for getting and applying crontabs for hosts.
Sets up command `font-lock-mode'.

\\{crontab-mode-map}

\(fn)" t nil)

(autoload 'crontab-get "crontab-mode" "\
Get the crontab for the HOST into a buffer.

\(fn HOST)" t nil)

;;;***

;;;### (autoloads (dabbrev-hover-install dabbrev-hover-start dabbrev-hover-introduction)
;;;;;;  "dabbrev-hover" "dabbrev-hover.el" (18543 20462))
;;; Generated autoloads from dabbrev-hover.el

(autoload 'dabbrev-hover-introduction "dabbrev-hover" "\
Provides electric help from variable `dabbrev-hover-introduction'.

\(fn)" t nil)

(autoload 'dabbrev-hover-start "dabbrev-hover" "\
Not documented

\(fn)" nil nil)

(autoload 'dabbrev-hover-install "dabbrev-hover" "\
Not documented

\(fn &optional GLOBALP FANCYP)" t nil)

;;;***

;;;### (autoloads (define-face-const) "def-face-const" "def-face-const.el"
;;;;;;  (18543 20469))
;;; Generated autoloads from def-face-const.el

(autoload 'define-face-const "def-face-const" "\
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

;;;### (autoloads (doc-mode-add-tag-doc doc-mode doc-mode-toggle-tag-doc-folding
;;;;;;  doc-mode-unfold-all doc-mode-fold-all doc-mode-unfold-tag-doc
;;;;;;  doc-mode-unfold-doc doc-mode-fold-tag-doc doc-mode-next-faulty-doc
;;;;;;  doc-mode-check-buffer doc-mode-fix-tag-doc doc-mode-remove-tag-doc
;;;;;;  doc-mode-first-template doc-mode-previous-template doc-mode-next-template)
;;;;;;  "doc-mode" "doc-mode.el" (18543 20471))
;;; Generated autoloads from doc-mode.el

(autoload 'doc-mode-next-template "doc-mode" "\
Jump to the next unfinished documentation template.

\(fn)" t nil)

(autoload 'doc-mode-previous-template "doc-mode" "\
Jump to the previous unfinished documentation template.

\(fn)" t nil)

(autoload 'doc-mode-first-template "doc-mode" "\
Jump to the oldest unfinished documentation template.

\(fn)" t nil)

(autoload 'doc-mode-remove-tag-doc "doc-mode" "\
Remove the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'.

\(fn TAG)" t nil)

(autoload 'doc-mode-fix-tag-doc "doc-mode" "\
Not documented

\(fn TAG)" t nil)

(autoload 'doc-mode-check-buffer "doc-mode" "\
Not documented

\(fn)" t nil)

(autoload 'doc-mode-next-faulty-doc "doc-mode" "\
Jump to the next faulty documentation and print error.

\(fn)" t nil)

(autoload 'doc-mode-fold-tag-doc "doc-mode" "\
Fold the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'.

\(fn TAG)" t nil)

(autoload 'doc-mode-unfold-doc "doc-mode" "\
Unfold the comment before POINT.

\(fn POINT)" t nil)

(autoload 'doc-mode-unfold-tag-doc "doc-mode" "\
Unfold the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'.

\(fn TAG)" t nil)

(autoload 'doc-mode-fold-all "doc-mode" "\
Not documented

\(fn &optional ARG)" t nil)

(autoload 'doc-mode-unfold-all "doc-mode" "\
Not documented

\(fn)" t nil)

(autoload 'doc-mode-toggle-tag-doc-folding "doc-mode" "\
Toggle folding of TAG's documentation.
If called interactively, use the tag given by `doc-mode-current-tag'.

\(fn TAG)" t nil)

(autoload 'doc-mode "doc-mode" "\
Minor mode for editing in-code documentation.

\(fn &optional ARG)" t nil)

(autoload 'doc-mode-add-tag-doc "doc-mode" "\
Not documented

\(fn TAG)" t nil)

;;;***

;;;### (autoloads (fold-dwim-toggle fold-dwim-show fold-dwim-hide
;;;;;;  fold-dwim-show-all fold-dwim-hide-all) "fold-dwim" "fold-dwim.el"
;;;;;;  (18543 20462))
;;; Generated autoloads from fold-dwim.el

(autoload 'fold-dwim-hide-all "fold-dwim" "\
Hide all folds of various kinds in the buffer

\(fn)" t nil)

(autoload 'fold-dwim-show-all "fold-dwim" "\
Show all folds of various kinds in the buffer

\(fn)" t nil)

(autoload 'fold-dwim-hide "fold-dwim" "\
Hide one item

\(fn)" nil nil)

(autoload 'fold-dwim-show "fold-dwim" "\
If point is in a closed or temporarily open fold,
  open it.  Returns nil if nothing was done

\(fn)" nil nil)

(autoload 'fold-dwim-toggle "fold-dwim" "\
Try fold-dwim-show to show any hidden text at point; if no
hidden fold is found, try fold-dwim-hide to hide the construction
at the cursor.

\(fn)" t nil)

;;;***

;;;### (autoloads (gnus-alias-determine-identity gnus-alias-use-identity
;;;;;;  gnus-alias-select-identity gnus-alias-init) "gnus-alias"
;;;;;;  "gnus-alias.el" (18543 20469))
;;; Generated autoloads from gnus-alias.el

(autoload 'gnus-alias-init "gnus-alias" "\
Add gnus-alias call to message mode hook.

\(fn)" t nil)

(autoload 'gnus-alias-select-identity "gnus-alias" "\
Prompt user for an identity and use it.

\(fn)" t nil)

(autoload 'gnus-alias-use-identity "gnus-alias" "\
Use an Identity defined in `gnus-alias-identity-alist'.

IDENTITY must be a valid entry in `gnus-alias-identity-alist',
otherwise an error will occur (NOTE: this behavior has changed
significantly from that found in 'gnus-pers').

If called interactively with no identity, user will be prompted for
one.

\(fn &optional IDENTITY)" t nil)

(autoload 'gnus-alias-determine-identity "gnus-alias" "\
Function that chooses a Identity based on message headers.

See `gnus-alias-identity-rules' for more information.  Optional
LOOKUP-ONLY is a boolean that, when non-nil, says to determine the
Identity, but don't actually use it (just return it)

\(fn &optional LOOKUP-ONLY)" nil nil)

;;;***

;;;### (autoloads (gnus-cvslog-highlight gnus-cvslog-highlight-maybe)
;;;;;;  "gnus-cvslog" "gnus-cvslog.el" (18543 20462))
;;; Generated autoloads from gnus-cvslog.el

(autoload 'gnus-cvslog-highlight-maybe "gnus-cvslog" "\
Highlight CVS log message iff `gnus-cvslog-enabled' is non-nil.
You can use this to toggle CVS log message highlightings via group
parameters.

\(fn)" nil nil)

(autoload 'gnus-cvslog-highlight "gnus-cvslog" "\
Highlight CVS log message.
This is the main entry point of this file.

\(fn)" nil nil)

;;;***

;;;### (autoloads (graphviz-dot-mode) "graphviz-dot-mode" "graphviz-dot-mode.el"
;;;;;;  (18543 20462))
;;; Generated autoloads from graphviz-dot-mode.el

(autoload 'graphviz-dot-mode "graphviz-dot-mode" "\
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

;;;### (autoloads (gtags-mode) "gtags" "gtags.el" (18543 20473))
;;; Generated autoloads from gtags.el

(autoload 'gtags-mode "gtags" "\
Toggle Gtags mode, a minor mode for browsing source code using GLOBAL.

Specify the root directory of project.
	\\[gtags-visit-rootdir]
Input tag name and move to the definition.
	\\[gtags-find-tag]
Input tag name and move to the referenced point.
	\\[gtags-find-rtag]
Input symbol and move to the locations.
	\\[gtags-find-symbol]
Input pattern, search with grep(1) and move to the locations.
	\\[gtags-find-with-grep]
Input pattern, search with idutils(1) and move to the locations.
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

Key definitions:
\\{gtags-mode-map}
Turning on Gtags mode calls the value of the variable `gtags-mode-hook'
with no args, if that value is non-nil.

\(fn &optional FORCES)" t nil)

;;;***

;;;### (autoloads (h4x0r-string h4x0r-word-at-point h4x0r-buffer
;;;;;;  h4x0r-region) "h4x0r" "h4x0r.el" (18543 20462))
;;; Generated autoloads from h4x0r.el

(autoload 'h4x0r-region "h4x0r" "\
Convert region to h4x0r-talk.

\(fn BEG END)" t nil)

(autoload 'h4x0r-buffer "h4x0r" "\
Convert entire buffer to h4x0r-talk.

\(fn)" t nil)

(autoload 'h4x0r-word-at-point "h4x0r" "\
Not documented

\(fn)" t nil)

(autoload 'h4x0r-string "h4x0r" "\
Not documented

\(fn H4-INPUT-STRING)" nil nil)

;;;***

;;;### (autoloads (global-hl-line-hack-mode hl-line-hack-mode) "hl-line-hack"
;;;;;;  "hl-line-hack.el" (18543 20462))
;;; Generated autoloads from hl-line-hack.el

(autoload 'hl-line-hack-mode "hl-line-hack" "\
Minor mode to highlight the line about point in the current window.
With ARG, turn Hl-Line-Hack mode on if ARG is positive, off otherwise.
Uses functions `hl-line-hack-unhighlight' and `hl-line-hack-highlight' on
`pre-command-hook' and `post-command-hook'.

\(fn &optional ARG)" t nil)

(defvar global-hl-line-hack-mode nil "\
Non-nil if Global-Hl-Line-Hack mode is enabled.
See the command `global-hl-line-hack-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-hl-line-hack-mode'.")

(custom-autoload 'global-hl-line-hack-mode "hl-line-hack" nil)

(autoload 'global-hl-line-hack-mode "hl-line-hack" "\
Toggle Hl-Line-Hack mode in every possible buffer.
With prefix ARG, turn Global-Hl-Line-Hack mode on if and only if ARG is positive.
Hl-Line-Hack mode is enabled in all buffers where `(lambda nil (hl-line-hack-mode 1))' would do it.
See `hl-line-hack-mode' for more information on Hl-Line-Hack mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (global-hl-sexp-mode hl-sexp-mode) "hl-sexp" "hl-sexp.el"
;;;;;;  (18543 20469))
;;; Generated autoloads from hl-sexp.el

(autoload 'hl-sexp-mode "hl-sexp" "\
Minor mode to highlight the sexp about point in the current window.
With ARG, turn Hl-Sexp mode on if ARG is positive, off otherwise.
Uses functions `hl-sexp-unhighlight' and `hl-sexp-highlight' on
`pre-command-hook' and `post-command-hook'.

\(fn &optional ARG)" t nil)

(defvar global-hl-sexp-mode nil "\
Non-nil if Global-Hl-Sexp mode is enabled.
See the command `global-hl-sexp-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-hl-sexp-mode'.")

(custom-autoload 'global-hl-sexp-mode "hl-sexp" nil)

(autoload 'global-hl-sexp-mode "hl-sexp" "\
Toggle Hl-Sexp mode in every possible buffer.
With prefix ARG, turn Global-Hl-Sexp mode on if and only if ARG is positive.
Hl-Sexp mode is enabled in all buffers where `hl-sexp-mode' would do it.
See `hl-sexp-mode' for more information on Hl-Sexp mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (htmlize-many-files-dired htmlize-many-files htmlize-file
;;;;;;  htmlize-region htmlize-buffer) "htmlize" "htmlize.el" (18701
;;;;;;  53080))
;;; Generated autoloads from htmlize.el

(autoload 'htmlize-buffer "htmlize" "\
Convert BUFFER to HTML, preserving colors and decorations.

The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the current
window.  The title of the generated document will be set to the buffer's
file name or, if that's not available, to the buffer's name.

Note that htmlize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting HTML will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses.

\(fn &optional BUFFER)" t nil)

(autoload 'htmlize-region "htmlize" "\
Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details.

\(fn BEG END)" t nil)

(autoload 'htmlize-file "htmlize" "\
Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name.

\(fn FILE &optional TARGET)" t nil)

(autoload 'htmlize-many-files "htmlize" "\
Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file.

\(fn FILES &optional TARGET-DIRECTORY)" t nil)

(autoload 'htmlize-many-files-dired "htmlize" "\
HTMLize dired-marked files.

\(fn ARG &optional TARGET-DIRECTORY)" t nil)

;;;***

;;;### (autoloads (http-get) "http-get" "http-get.el" (18543 20462))
;;; Generated autoloads from http-get.el

(autoload 'http-get "http-get" "\
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

;;;### (autoloads nil "icomplete+" "icomplete+.el" (18543 20473))
;;; Generated autoloads from icomplete+.el

(when (< emacs-major-version 23) (defun icomplete-exhibit nil "Insert icomplete completions display.\nShould be run via minibuffer `post-command-hook'.\nSee `icomplete-mode' and `minibuffer-setup-hook'." (when (icomplete-simple-completing-p) (save-match-data (let* ((minibuf-begin (if (< emacs-major-version 21) (point-min) (minibuffer-prompt-end))) (contents (buffer-substring minibuf-begin (point-max))) (buffer-undo-list t)) (save-excursion (goto-char (point-max)) (unless (boundp 'icomplete-eoinput) (make-local-variable 'icomplete-eoinput)) (setq icomplete-eoinput (point)) (when (and (> (point-max) minibuf-begin) (save-excursion (goto-char minibuf-begin) (not (looking-at "\\(\\s-+$\\|\\s-*\\(\\s(\\|\\s\"\\|\\s'\\|\\s<\\|[0-9]\\)\\)"))) (or (> (point-max) icomplete-max-delay-chars) (if minibuffer-completion-table (cond ((numberp minibuffer-completion-table) (< minibuffer-completion-table icomplete-delay-completions-threshold)) ((sequencep minibuffer-completion-table) (< (length minibuffer-completion-table) icomplete-delay-completions-threshold)))) (sit-for icomplete-compute-delay))) (insert (icomplete-completions contents minibuffer-completion-table minibuffer-completion-predicate (not minibuffer-completion-confirm))))) (setq deactivate-mark nil))))))

(when (> emacs-major-version 22) (defun icomplete-exhibit nil "Insert icomplete completions display.\nShould be run via minibuffer `post-command-hook'.  See `icomplete-mode'\nand `minibuffer-setup-hook'." (when (and icomplete-mode (icomplete-simple-completing-p)) (save-match-data (save-excursion (goto-char (point-max)) (when (and (> (point-max) (minibuffer-prompt-end)) buffer-undo-list (save-excursion (goto-char (minibuffer-prompt-end)) (not (looking-at "\\(\\s-+$\\|\\s-*\\(\\s(\\|\\s\"\\|\\s'\\|\\s<\\|[0-9]\\)\\)"))) (or (> (- (point) (field-beginning)) icomplete-max-delay-chars) (and (sequencep minibuffer-completion-table) (< (length minibuffer-completion-table) icomplete-delay-completions-threshold)) (sit-for icomplete-compute-delay))) (let ((text (while-no-input (icomplete-completions (field-string) minibuffer-completion-table minibuffer-completion-predicate (not minibuffer-completion-confirm)))) (buffer-undo-list t) deactivate-mark) (when (stringp text) (move-overlay icomplete-overlay (point) (point) (current-buffer)) (put-text-property 0 1 'cursor t text) (overlay-put icomplete-overlay 'after-string text)))))))))

(when (< emacs-major-version 23) (defun icomplete-completions (name candidates predicate require-match) "Identify prospective candidates for minibuffer completion.\nNAME is the name to complete.\nCANDIDATES are the candidates to match.\nPREDICATE filters matches: they succeed only if this returns non-nil.\nREQUIRE-MATCH non-nil means the input must match a candidate.\n\nThe display is updated with each minibuffer keystroke during\nminibuffer completion.\n\nProspective completion suffixes (if any) are displayed, bracketed by\n\"()\", \"[]\", or \"{}\".  The choice of brackets is as follows:\n\n  (...) - A single prospect is identified and matching is enforced.\n  [...] - A single prospect is identified and matching is optional.\n  {...} - Multiple prospects are indicated, and further input is\n          needed to distinguish a single one.\n\nThe displays for unambiguous matches have \" [ Matched ]\" appended\n(whether complete or not), or \" [ No match ]\", if no eligible\nmatches exist.\nKeybindings for uniquely matched commands are displayed within the [].\n\nWhen more than one completion is available, the total number precedes\nthe suffixes display, like this:\n  M-x forw    14 (ard-) { char line list...}\n\nIf library `icicles.el' is also loaded, then you can cycle\ncompletions.  When you change cycling direction, the number of\nadditional cycle candidates, besides the current one, is displayed\nfollowing the rest of the icomplete info:\n  M-x forward-line   [Matched]  (13 more)." (when (and (listp candidates) (null (car candidates))) (setq candidates nil)) (let ((comps (all-completions name candidates predicate)) (open-bracket-determined (if require-match "(" " [")) (close-bracket-determined (if require-match ") " "] ")) (keys nil) nb-candidates-string) (setq nb-candidates (length comps)) (if (null comps) (format (if (fboundp 'icicle-apropos-complete) "	%sNo prefix matches%s" "	%sNo matches%s") open-bracket-determined close-bracket-determined) (let* ((most-try (try-completion name (mapcar #'list comps))) (most (if (stringp most-try) most-try (car comps))) (most-len (length most)) (determ (and (> most-len (length name)) (concat open-bracket-determined (substring most (length name)) close-bracket-determined))) (open-bracket-prospects "{ ") (close-bracket-prospects " }") (prospects-len 0) prompt prompt-rest prospects most-is-exact comp) (when determ (put-text-property 0 (length determ) 'face 'icompletep-determined determ)) (if (eq most-try t) (setq prospects nil) (while (and comps (< prospects-len icompletep-prospects-length)) (setq comp (substring (car comps) most-len) comps (cdr comps)) (cond ((string-equal comp "") (setq most-is-exact t)) ((member comp prospects)) (t (setq prospects (cons comp prospects) prospects-len (+ (length comp) 1 prospects-len)))))) (setq prompt-rest (if prospects (concat open-bracket-prospects (and most-is-exact ", ") (mapconcat 'identity (sort prospects #'string-lessp) "  ") (and comps "...") close-bracket-prospects) (concat "	[ Matched" (if (setq keys (and icomplete-show-key-bindings (commandp (intern-soft most)) (icomplete-get-keys most))) (concat "; " keys) (setq keys nil)) " ]"))) (put-text-property 0 (length prompt-rest) 'face 'icompletep-choices prompt-rest) (cond ((< nb-candidates 2) (setq prompt (concat "      " determ prompt-rest)) (when (eq last-command this-command) (setq icicle-nb-of-other-cycle-candidates 0))) (t (setq nb-candidates-string (format "%7d " nb-candidates)) (put-text-property (string-match "\\S-" nb-candidates-string) (1- (length nb-candidates-string)) 'face 'icompletep-nb-candidates nb-candidates-string) (setq prompt (concat nb-candidates-string determ prompt-rest)))) (when keys (put-text-property (+ 18 (length determ)) (1- (length prompt)) 'face 'icompletep-keys prompt)) (when (and (boundp 'icicle-last-completion-candidate) (> icicle-nb-of-other-cycle-candidates 0) (= 1 nb-candidates) icicle-last-completion-candidate (not (eq last-command this-command))) (setq nb-candidates-string (format "  (%d more)" icicle-nb-of-other-cycle-candidates)) (put-text-property (string-match "\\S-" nb-candidates-string) (length nb-candidates-string) 'face 'icompletep-nb-candidates nb-candidates-string) (setq prompt (concat prompt nb-candidates-string))) prompt)))))

(when (> emacs-major-version 22) (defun icomplete-completions (name candidates predicate require-match) "Identify prospective candidates for minibuffer completion.\nNAME is the name to complete.\nCANDIDATES are the candidates to match.\nPREDICATE filters matches: they succeed only if it returns non-nil.\nREQUIRE-MATCH non-nil means the input must match a candidate.\n\nThe display is updated with each minibuffer keystroke during\nminibuffer completion.\n\nProspective completion suffixes (if any) are displayed, bracketed by\n\"()\", \"[]\", or \"{}\".  The choice of brackets is as follows:\n\n  (...) - A single prospect is identified, and matching is enforced.\n  [...] - A single prospect is identified, and matching is optional.\n  {...} - Multiple prospects are indicated, and further input is\n          needed to distinguish a single one.\n\nThe displays for unambiguous matches have ` [ Matched ]' appended\n(whether complete or not), or ` [ No matches ]', if no eligible\nmatches exist.  (Keybindings for uniquely matched commands are\nexhibited within the square brackets, [].)\n\nWhen more than one completion is available, the total number precedes\nthe suffixes display, like this:\n  M-x forw    14 (ard-) { char line list...}\n\nIf library `icicles.el' is also loaded, then you can cycle\ncompletions.  When you change cycling direction, the number of\nadditional cycle candidates, besides the current one, is displayed\nfollowing the rest of the icomplete info:\n  M-x forward-line   [Matched]  (13 more)." (when (and (listp candidates) (null (car candidates))) (setq candidates nil)) (let* ((comps (all-completions name candidates predicate)) (nb-candidates (length comps)) (open-bracket (if require-match "(" " [")) (close-bracket (if require-match ") " "] "))) (if (not (consp comps)) (format (if (fboundp 'icicle-apropos-complete) "	%sNo prefix matches%s" "	%sNo matches%s") open-bracket close-bracket) (let* ((most-try (completion-try-completion name comps nil (length name))) (most (if (consp most-try) (car most-try) (if most-try (car comps) ""))) (compare (compare-strings name nil nil most nil nil completion-ignore-case)) (determ (and (not (or (eq t compare) (eq t most-try) (= (setq compare (1- (abs compare))) (length most)))) (concat open-bracket (cond ((= compare (length name)) (substring most compare)) ((< compare 5) most) (t (concat "..." (substring most compare)))) close-bracket))) (prospects-len (+ (string-width (buffer-string)) 8 (length determ) 2 -2 5)) (prospects-max (* (+ icomplete-prospects-height (/ prospects-len (window-width))) (window-width))) (prefix-len (if (eq t (compare-strings (car comps) nil (length most) most nil nil completion-ignore-case)) (length most) (let ((comps-prefix (try-completion "" comps))) (and (stringp comps-prefix) (length comps-prefix))))) (keys nil) prompt nb-candidates-string prompt-rest prospects most-is-exact comp limit) (when determ (put-text-property 0 (length determ) 'face 'icompletep-determined determ)) (if (eq most-try t) (setq prospects nil) (while (and comps (not limit)) (setq comp (if prefix-len (substring (car comps) prefix-len) (car comps)) comps (cdr comps)) (cond ((string-equal comp "") (setq most-is-exact t)) ((member comp prospects)) (t (setq prospects-len (+ (string-width comp) 2 prospects-len)) (if (< prospects-len prospects-max) (push comp prospects) (setq limit t)))))) (setq prompt-rest (if prospects (concat "{ " (and most-is-exact ", ") (mapconcat 'identity (sort prospects #'string-lessp) "  ") (and limit "...") " }") (concat "	[ Matched" (if (setq keys (and icomplete-show-key-bindings (commandp (intern-soft most)) (icomplete-get-keys most))) (concat "; " keys) (setq keys nil)) " ]"))) (put-text-property 0 (length prompt-rest) 'face 'icompletep-choices prompt-rest) (cond ((< nb-candidates 2) (setq prompt (concat "      " determ prompt-rest)) (when (eq last-command this-command) (setq icicle-nb-of-other-cycle-candidates 0))) (t (setq nb-candidates-string (format "%7d " nb-candidates)) (put-text-property (string-match "\\S-" nb-candidates-string) (1- (length nb-candidates-string)) 'face 'icompletep-nb-candidates nb-candidates-string) (setq prompt (concat nb-candidates-string determ prompt-rest)))) (when keys (put-text-property (+ 18 (length determ)) (1- (length prompt)) 'face 'icompletep-keys prompt)) (when (and (boundp 'icicle-last-completion-candidate) (> icicle-nb-of-other-cycle-candidates 0) (= 1 nb-candidates) icicle-last-completion-candidate (not (eq last-command this-command))) (setq nb-candidates-string (format "  (%d more)" icicle-nb-of-other-cycle-candidates)) (put-text-property (string-match "\\S-" nb-candidates-string) (length nb-candidates-string) 'face 'icompletep-nb-candidates nb-candidates-string) (setq prompt (concat prompt nb-candidates-string))) prompt)))))

;;;***

;;;### (autoloads (idledo-add-to-end-of-list) "idledo" "idledo.el"
;;;;;;  (18543 20462))
;;; Generated autoloads from idledo.el

(autoload 'idledo-add-to-end-of-list "idledo" "\
Like add-to-list, but adds at the end, if added at all.

\(fn LIST-VAR ELEMENT)" nil nil)

;;;***

;;;### (autoloads (incr-add-to-alist) "incr" "incr.el" (18543 20467))
;;; Generated autoloads from incr.el

(autoload 'incr-add-to-alist "incr" "\
Not documented

\(fn METHOD &optional BEFORE)" nil nil)

;;;***

;;;### (autoloads (kill-ring-rotate) "kill-ring-rotate" "kill-ring-rotate.el"
;;;;;;  (18543 20473))
;;; Generated autoloads from kill-ring-rotate.el

(autoload 'kill-ring-rotate "kill-ring-rotate" "\
Displays a buffer with the previous and next pieces of text in the
kill ring. RET will insert the current kill-ring text in the buffer
from which the function was called, and C-c C-c or `q' exits.

Navigate through the kill-ring with <up> and <down>

\(fn)" t nil)

;;;***

;;;### (autoloads (kill-ring-search) "kill-ring-search" "kill-ring-search.el"
;;;;;;  (18886 47455))
;;; Generated autoloads from kill-ring-search.el

(autoload 'kill-ring-search "kill-ring-search" "\
Search the kill ring in the minibuffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (map-lines) "map-lines" "map-lines.el" (18543 20462))
;;; Generated autoloads from map-lines.el

(autoload 'map-lines "map-lines" "\
Map COMMAND over lines matching REGEX.

\(fn COMMAND-C REGEX)" t nil)

;;;***

;;;### (autoloads (expand-member-functions) "member-functions" "member-functions.el"
;;;;;;  (18543 20462))
;;; Generated autoloads from member-functions.el

(autoload 'expand-member-functions "member-functions" "\
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
;;;;;;  metamail-interpret-header) "metamail" "metamail.el" (18543
;;;;;;  20469))
;;; Generated autoloads from metamail.el

(autoload 'metamail-interpret-header "metamail" "\
Interpret a header part of a MIME message in current buffer.
Its body part is not interpreted at all.

\(fn)" t nil)

(autoload 'metamail-interpret-body "metamail" "\
Interpret a body part of a MIME message in current buffer.
Optional 1st argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional 2nd argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted.
Its header part is not interpreted at all.

\(fn &optional VIEWMODE NODISPLAY)" t nil)

(autoload 'metamail-buffer "metamail" "\
Process current buffer through `metamail'.
Optional 1st argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional 2nd argument BUFFER specifies a buffer to be filled (nil
means current).
Optional 3rd argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted.

\(fn &optional VIEWMODE BUFFER NODISPLAY)" t nil)

(autoload 'metamail-region "metamail" "\
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
;;;;;;  (18543 20462))
;;; Generated autoloads from mic-paren.el

(autoload 'paren-activate "mic-paren" "\
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

(autoload 'paren-deactivate "mic-paren" "\
Deactivates mic-paren parenthesis highlighting

\(fn)" t nil)

(autoload 'paren-toggle-matching-paired-delimiter "mic-paren" "\
Toggle matching paired delimiter, force on with positive arg. Use this in
mode-hooks to activate or deactivate paired delimiter matching. If optional
second argument NO-MESSAGE is not nil then no message is displayed about the
current activation state of the paired-delimiter-matching feature.

\(fn ARG &optional NO-MESSAGE)" t nil)

(autoload 'paren-toggle-matching-quoted-paren "mic-paren" "\
Toggle matching quoted parens, force on with positive arg. Use this in
mode-hooks to activate or deactivate quoted paren matching. If optional second
argument NO-MESSAGE is not nil then no message is displayed about the current
activation state of the quoted-paren-matching feature.

\(fn ARG &optional NO-MESSAGE)" t nil)

(autoload 'paren-toggle-open-paren-context "mic-paren" "\
Toggle the determining of the context to display of the matching
open-paren, force backward context with positive arg. Use this in mode-hooks.
For a description of the meaning look at `paren-open-paren-context-backward'.

\(fn ARG)" t nil)

(autoload 'paren-forward-sexp "mic-paren" "\
Acts like forward-sexp but can also handle quoted parens. Look at
`paren-match-quoted-paren' for a detailed comment.

\(fn &optional ARG)" t nil)

(autoload 'paren-backward-sexp "mic-paren" "\
Acts like backward-sexp but can also matching quoted parens. Look at
`paren-match-quoted-paren' for a detailed comment

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (mime-forward-from-gnus-using-mail mime-forward-from-rmail-using-mail
;;;;;;  mime-mode) "mime" "mime.el" (18543 20469))
;;; Generated autoloads from mime.el

(autoload 'mime-mode "mime" "\
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

(fset 'edit-mime 'mime-mode)

(autoload 'mime-forward-from-rmail-using-mail "mime" "\
Forward current message in message/rfc822 content-type message from rmail.
The message will be appended if being composed.

\(fn)" t nil)

(autoload 'mime-forward-from-gnus-using-mail "mime" "\
Forward current article in message/rfc822 content-type message from GNUS.
The message will be appended if being composed.

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb/send-hook bbdb/send-auto-notes-hook bbdb/send-ignore-some-messages-hook
;;;;;;  bbdb/send-ignore-most-messages-hook) "moy-bbdb" "moy-bbdb.el"
;;;;;;  (18543 20462))
;;; Generated autoloads from moy-bbdb.el

(autoload 'bbdb/send-ignore-most-messages-hook "moy-bbdb" "\
For use as the value of `bbdb/send-auto-create-p'.
This will automatically create BBDB entries for messages which match
the bbdb/send-ignore-most-messages-alist (which see) and *no* others.

\(fn &optional INVERT-SENSE)" nil nil)

(autoload 'bbdb/send-ignore-some-messages-hook "moy-bbdb" "\
For use as a `bbdb/send-auto-create-hook'.
This will automatically create BBDB entries for messages which do *not*
match the `bbdb/send-ignore-some-messages-alist' (which see).

\(fn)" nil nil)

(autoload 'bbdb/send-auto-notes-hook "moy-bbdb" "\
For use as a `bbdb/send-notice-hook'.  This might automatically add
some text to  the notes field of the BBDB  record corresponding to the
current record  based on the header  of the current  message.  See the
documentation  for   the  variables  `bbdb/send-auto-notes-alist'  and
`bbdb/send-auto-notes-ignore'.

\(fn RECORD)" nil nil)

(autoload 'bbdb/send-hook "moy-bbdb" "\
Parse headers of outgoing message, insert the addresses of the
  recipients one by one into BBDB if they do not exist already

\(fn)" t nil)

;;;***

;;;### (autoloads (oddmuse-kill-url oddmuse-browse-this-page oddmuse-browse-page
;;;;;;  emacswiki-post oddmuse-insert-pagename oddmuse-revert oddmuse-post
;;;;;;  oddmuse-follow oddmuse-edit oddmuse-toggle-minor) "oddmuse"
;;;;;;  "oddmuse.el" (18543 20472))
;;; Generated autoloads from oddmuse.el

(autoload 'oddmuse-toggle-minor "oddmuse" "\
Toggle minor mode state.

\(fn &optional ARG)" t nil)

(autoload 'oddmuse-edit "oddmuse" "\
Edit a page on a wiki.
WIKI is the name of the wiki as defined in `oddmuse-wikis',
PAGENAME is the pagename of the page you want to edit.
Use a prefix argument to force a reload of the page.

\(fn WIKI PAGENAME)" t nil)

(autoload 'oddmuse-follow "oddmuse" "\
Figure out what page we need to visit
and call `oddmuse-edit' on it.

\(fn ARG)" t nil)

(autoload 'oddmuse-post "oddmuse" "\
Post the current buffer to the current wiki.
The current wiki is taken from `oddmuse-wiki'.

\(fn SUMMARY)" t nil)

(autoload 'oddmuse-revert "oddmuse" "\
Revert this oddmuse page.

\(fn)" t nil)

(autoload 'oddmuse-insert-pagename "oddmuse" "\
Insert a PAGENAME of current wiki with completion.

\(fn PAGENAME)" t nil)

(autoload 'emacswiki-post "oddmuse" "\
Post the current buffer to the EmacsWiki.
If this command is invoked interactively: with prefix argument, prompts pagename,
otherwise set pagename as basename of `buffer-file-name'.

This command is intended to post current EmacsLisp program easily.

\(fn &optional PAGENAME SUMMARY)" t nil)

(autoload 'oddmuse-browse-page "oddmuse" "\
Ask a WWW browser to load an oddmuse page.
WIKI is the name of the wiki as defined in `oddmuse-wikis',
PAGENAME is the pagename of the page you want to browse.

\(fn WIKI PAGENAME)" t nil)

(autoload 'oddmuse-browse-this-page "oddmuse" "\
Ask a WWW browser to load current oddmuse page.

\(fn)" t nil)

(autoload 'oddmuse-kill-url "oddmuse" "\
Make the URL of current oddmuse page the latest kill in the kill ring.

\(fn)" t nil)

;;;***

;;;### (autoloads (page-break-mode) "page-break" "page-break.el"
;;;;;;  (18543 20462))
;;; Generated autoloads from page-break.el

(autoload 'page-break-mode "page-break" "\
Toggle Page Break mode.

In Page Break mode, page breaks (^L characters) are displayed as a
horizontal line of `page-break-string-char' characters.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (paredit-mode) "paredit" "paredit.el" (18923 26862))
;;; Generated autoloads from paredit.el

(autoload 'paredit-mode "paredit" "\
Minor mode for pseudo-structurally editing Lisp code.
With a prefix argument, enable Paredit Mode even if there are
  imbalanced parentheses in the buffer.
Paredit behaves badly if parentheses are imbalanced, so exercise
  caution when forcing Paredit Mode to be enabled, and consider
  fixing imbalanced parentheses instead.
\\<paredit-mode-map>

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (pod-last-sexp-runtime pod-load-file pod-load pod-activate-advice
;;;;;;  pod-deactivate-advice pod-reset-results pod-todo pod-new-features
;;;;;;  pod-commentary pod-introduction pod-quick-start) "pod" "pod.el"
;;;;;;  (18543 20462))
;;; Generated autoloads from pod.el

(autoload 'pod-quick-start "pod" "\
Provides electric help regarding `pod-quick-start'.

\(fn)" t nil)

(autoload 'pod-introduction "pod" "\
Provides electric help regarding `pod-introduction'.

\(fn)" t nil)

(autoload 'pod-commentary "pod" "\
Provides electric help regarding `pod-commentary'.

\(fn)" t nil)

(autoload 'pod-new-features "pod" "\
Provides electric help regarding `pod-new-features'.

\(fn)" t nil)

(autoload 'pod-todo "pod" "\
Provides electric help regarding `pod-todo'.

\(fn)" t nil)

(autoload 'pod-reset-results "pod" "\
Not documented

\(fn)" t nil)

(autoload 'pod-deactivate-advice "pod" "\
Not documented

\(fn)" t nil)

(autoload 'pod-activate-advice "pod" "\
Not documented

\(fn)" t nil)

(autoload 'pod-load "pod" "\
Kinda like load..

\(fn FILE &rest ARGS)" nil nil)

(autoload 'pod-load-file "pod" "\
Not documented

\(fn FILE)" t nil)

(autoload 'pod-last-sexp-runtime "pod" "\
Not documented

\(fn &optional PT)" t nil)

;;;***

;;;### (autoloads (run-prolog mercury-mode prolog-mode) "prolog"
;;;;;;  "prolog.el" (18543 20462))
;;; Generated autoloads from prolog.el

(autoload 'prolog-mode "prolog" "\
Major mode for editing Prolog code.

Blank lines and `%%...' separate paragraphs.  `%'s starts a comment
line and comments can also be enclosed in /* ... */.

If an optional argument SYSTEM is non-nil, set up mode for the given system.

Commands:
\\{prolog-mode-map}
Entry to this mode calls the value of `prolog-mode-hook'
if that value is non-nil.

\(fn &optional SYSTEM)" t nil)

(autoload 'mercury-mode "prolog" "\
Major mode for editing Mercury programs.
Actually this is just customized `prolog-mode'.

\(fn)" t nil)

(autoload 'run-prolog "prolog" "\
Run an inferior Prolog process, input and output via buffer *prolog*.
With prefix argument ARG, restart the Prolog process if running before.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (svn-status svn-checkout) "psvn" "psvn.el" (18543
;;;;;;  20469))
;;; Generated autoloads from psvn.el

(autoload 'svn-checkout "psvn" "\
Run svn checkout REPOS-URL PATH.

\(fn REPOS-URL PATH)" t nil)
 (defalias 'svn-examine 'svn-status)

(autoload 'svn-status "psvn" "\
Examine the status of Subversion working copy in directory DIR.
If ARG is -, allow editing of the parameters. One could add -N to
run svn status non recursively to make it faster.
For every other non nil ARG pass the -u argument to `svn status', which
asks svn to connect to the repository and check to see if there are updates
there.

If there is no .svn directory, examine if there is CVS and run
`cvs-examine'. Otherwise ask if to run `dired'.

\(fn DIR &optional ARG)" t nil)

;;;***

;;;### (autoloads (rm-mouse-drag-region rm-kill-ring-save rm-kill-region
;;;;;;  rm-exchange-point-and-mark rm-set-mark rm-example-picture-mode-bindings)
;;;;;;  "rect-mark" "rect-mark.el" (18543 20462))
;;; Generated autoloads from rect-mark.el
 (define-key ctl-x-map "r\C-@" 'rm-set-mark)
 (define-key ctl-x-map [?r ?\C-\ ] 'rm-set-mark)
 (define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
 (define-key ctl-x-map "r\C-w" 'rm-kill-region)
 (define-key ctl-x-map "r\M-w" 'rm-kill-ring-save)

(autoload 'rm-example-picture-mode-bindings "rect-mark" "\
Example rect-mark keyboard and mouse bindings for picture mode.

\(fn)" nil nil)

(autoload 'rm-set-mark "rect-mark" "\
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

(autoload 'rm-exchange-point-and-mark "rect-mark" "\
Like `exchange-point-and-mark' but treats region as a rectangle.
See `rm-set-mark' for more details.

With optional argument FORCE, tabs are expanded and spaces are
inserted as necessary to keep the region perfectly rectangular.
This is the default in `picture-mode'.

\(fn FORCE)" t nil)

(autoload 'rm-kill-region "rect-mark" "\
Like kill-rectangle except the rectangle is also saved in the kill ring.
Since rectangles are not ordinary text, the killed rectangle is saved
in the kill ring as a series of lines, one for each row of the rectangle.
The rectangle is also saved as the killed rectangle so it is available for
insertion with yank-rectangle.

\(fn START END)" t nil)

(autoload 'rm-kill-ring-save "rect-mark" "\
Copies the region like rm-kill-region would but the rectangle isn't killed.

\(fn START END)" t nil)

(autoload 'rm-mouse-drag-region "rect-mark" "\
Highlight a rectangular region of text as the the mouse is dragged over it.
This must be bound to a button-down mouse event.

\(fn START-EVENT)" t nil)

;;;***

;;;### (autoloads (register-list) "register-list" "register-list.el"
;;;;;;  (18543 20473))
;;; Generated autoloads from register-list.el

(autoload 'register-list "register-list" "\
Display a list of registers.
An optional argument TYPE defines a regexp to restrict the
register menu to.  A second optional argument FONTIFICATION
decides if the display preserves original fontification for
values.

The default types are defined in `register-list-default-types',
which see.

The list is displayed in a buffer named `*Register List*' in
`register-list-mode', which see.

\(fn &optional TYPE FONTIFY)" t nil)

;;;***

;;;### (autoloads (rfc-goto-number rfc-index) "rfc" "rfc.el" (18543
;;;;;;  20464))
;;; Generated autoloads from rfc.el

(autoload 'rfc-index "rfc" "\
Show the index of RFC.

\(fn)" t nil)

(autoload 'rfc-goto-number "rfc" "\
Show an RFC article which number is NUMBER.

\(fn NUMBER)" t nil)

;;;***

;;;### (autoloads (rmail-convert-mime-header rmail-show-mime) "rmailmime"
;;;;;;  "rmailmime.el" (18543 20469))
;;; Generated autoloads from rmailmime.el

(autoload 'rmail-show-mime "rmailmime" "\
Show a MIME message in current buffer using a View mode.
Optional 1st argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
If an optional 2nd argument DO-HEADER is non-nil, interpret a header part,
too.  Otherwise, a body part is only interpreted.
The contents of current buffer are not changed at all.

\(fn &optional VIEWMODE DO-HEADER)" t nil)

(autoload 'rmail-convert-mime-header "rmailmime" "\
Convert MIME header fields of current message into a readable form.
It is expected to be used as rmail-message-filter in Rmail and
vm-message-filter in VM.  Original header is preserved in Rmail.

\(fn)" t nil)

;;;***

;;;### (autoloads (replace-recent-character) "rrc" "rrc.el" (18543
;;;;;;  20462))
;;; Generated autoloads from rrc.el

(autoload 'replace-recent-character "rrc" "\
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
;;;;;;  sawfish-mode) "sawfish" "sawfish.el" (18543 20462))
;;; Generated autoloads from sawfish.el

(autoload 'sawfish-mode "sawfish" "\
Major mode for editing sawfish files and for interacting with sawfish.

Special commands:

\\{sawfish-mode-map}

\(fn)" t nil)

(autoload 'sawfish-eval-region "sawfish" "\
Evaluate the region bounded by START and END.

TARGET-BUFFER is the optional target for the return value of the
evaluation.

\(fn START END &optional TARGET-BUFFER)" t nil)

(autoload 'sawfish-eval-buffer "sawfish" "\
Evaluate the whole buffer.

\(fn)" t nil)

(autoload 'sawfish-eval-defun "sawfish" "\
Evaluate the top level form at or near `point'.

INSERT-VALUE is a prefix parameter, if it is non-NIL the value of the
expression is inserted into the buffer after the form.

\(fn INSERT-VALUE)" t nil)

(autoload 'sawfish-eval-expression "sawfish" "\
Evaluate SEXP and display the value in the minibuffer.

If the optional parameter INSERT-VALUE is supplied as a non-NIL value the
value of SEXP will be inserted into the current buffer.

\(fn SEXP &optional INSERT-VALUE)" t nil)

(autoload 'sawfish-eval-last-sexp "sawfish" "\
Version of `eval-last-sexp' that interacts with sawfish.

\(fn TO-BUFFER)" t nil)

(autoload 'sawfish-eval-print-last-sexp "sawfish" "\
Not documented

\(fn)" t nil)

(autoload 'sawfish-describe-function "sawfish" "\
Display the doc-string for FUNCTION.

\(fn FUNCTION)" t nil)

(autoload 'sawfish-describe-variable "sawfish" "\
Display the doc-string for VARIABLE.

\(fn VARIABLE)" t nil)

(autoload 'sawfish-info-function "sawfish" "\
Display the Info documentation for FUNCTION.

\(fn FUNCTION)" t nil)

(autoload 'sawfish-info-variable "sawfish" "\
Display the Info documentation for VARIABLE.

\(fn VARIABLE)" t nil)

(autoload 'sawfish-apropos "sawfish" "\
Show all bound sawfish symbols whose names match REGEXP.

\(fn REGEXP)" t nil)

(autoload 'sawfish-complete-symbol "sawfish" "\
Attempt to complete the symbol at `point'.

\(fn)" t nil)

(autoload 'sawfish-info "sawfish" "\
View the sawfish info file.

\(fn)" t nil)

(autoload 'sawfish-rep-info "sawfish" "\
View the librep info file.

\(fn)" t nil)

(autoload 'sawfish-console "sawfish" "\
Run the sawfish client as an inferior lisp.

\(fn)" t nil)

(autoload 'sawfish-interaction "sawfish" "\
Create a sawfish interaction buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (turn-off-screen-lines-mode turn-on-screen-lines-mode
;;;;;;  screen-lines-mode) "screen-lines" "screen-lines.el" (18543
;;;;;;  20470))
;;; Generated autoloads from screen-lines.el

(autoload 'screen-lines-mode "screen-lines" "\
Toggle Screen Lines minor mode for the current buffer.
With ARG, turn the mode on if ARG is positive, off otherwise.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-screen-lines-mode "screen-lines" "\
Turn on Screen Lines minor mode for the current buffer.

\(fn)" t nil)

(autoload 'turn-off-screen-lines-mode "screen-lines" "\
Turn off Screen Lines minor mode for the current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (ssh) "ssh" "ssh.el" (18543 20469))
;;; Generated autoloads from ssh.el
 (add-hook 'same-window-regexps "^\\*ssh-.*\\*\\(\\|<[0-9]+>\\)")

(autoload 'ssh "ssh" "\
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

;;;### (autoloads (tabbar-local-mode tabbar-mode tabbar-forward-tab
;;;;;;  tabbar-backward-tab tabbar-forward-group tabbar-backward-group
;;;;;;  tabbar-forward tabbar-backward) "tabbar" "tabbar.el" (18543
;;;;;;  20471))
;;; Generated autoloads from tabbar.el

(autoload 'tabbar-backward "tabbar" "\
Select the previous available tab.
Depend on the setting of the option `tabbar-cycling-scope'.

\(fn)" t nil)

(autoload 'tabbar-forward "tabbar" "\
Select the next available tab.
Depend on the setting of the option `tabbar-cycling-scope'.

\(fn)" t nil)

(autoload 'tabbar-backward-group "tabbar" "\
Go to selected tab in the previous available group.

\(fn)" t nil)

(autoload 'tabbar-forward-group "tabbar" "\
Go to selected tab in the next available group.

\(fn)" t nil)

(autoload 'tabbar-backward-tab "tabbar" "\
Select the previous visible tab.

\(fn)" t nil)

(autoload 'tabbar-forward-tab "tabbar" "\
Select the next visible tab.

\(fn)" t nil)

(defvar tabbar-mode nil "\
Non-nil if Tabbar mode is enabled.
See the command `tabbar-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `tabbar-mode'.")

(custom-autoload 'tabbar-mode "tabbar" nil)

(autoload 'tabbar-mode "tabbar" "\
Toggle display of a tab bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\(fn &optional ARG)" t nil)

(autoload 'tabbar-local-mode "tabbar" "\
Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When on and tab bar global mode is on, if a buffer local value of
`header-line-format' exists, it is saved, then the local header line
is killed to show the tab bar.  When off, the saved local value of the
header line is restored, hiding the tab bar.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (trivial-cite) "tc" "tc.el" (18543 20462))
;;; Generated autoloads from tc.el

(autoload 'trivial-cite "tc" "\
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

;;;### (autoloads (tempo-snippets-complete-tag tempo-snippets-insert-template
;;;;;;  tempo-define-snippet tempo-snippets-next-field tempo-snippets-previous-field
;;;;;;  tempo-snippets-clear-latest tempo-snippets-clear-oldest tempo-snippets-clear-all)
;;;;;;  "tempo-snippets" "tempo-snippets.el" (18543 20471))
;;; Generated autoloads from tempo-snippets.el

(autoload 'tempo-snippets-clear-all "tempo-snippets" "\
Clear all tempo-snippet overlays.

\(fn)" t nil)

(autoload 'tempo-snippets-clear-oldest "tempo-snippets" "\
Clear the oldest tempo-snippet overlays.

\(fn)" t nil)

(autoload 'tempo-snippets-clear-latest "tempo-snippets" "\
Clear the latest tempo-snippet overlays.

\(fn)" t nil)

(autoload 'tempo-snippets-previous-field "tempo-snippets" "\
Jump to the previous editable tempo-snippet field.
You can also use `tempo-forward-mark', which will include more points of
interest.

\(fn &optional ARG)" t nil)

(autoload 'tempo-snippets-next-field "tempo-snippets" "\
Jump to the next editable tempo-snippet field.
You can also use `tempo-backward-mark', which will include more points of
interest.

\(fn &optional ARG)" t nil)

(autoload 'tempo-define-snippet "tempo-snippets" "\
`tempo-snippets' version of `tempo-define-template'.
Use with the same arguments as `tempo-define-template'.  The resulting template
will prompt for input right in the buffer instead of the minibuffer.

\(fn NAME ELEMENTS &optional TAG DOCUMENTATION TAGLIST)" nil nil)

(autoload 'tempo-snippets-insert-template "tempo-snippets" "\
`tempo-snippets' version of `tempo-insert-template.'

\(fn TEMPLATE ON-REGION)" nil nil)

(autoload 'tempo-snippets-complete-tag "tempo-snippets" "\
`tempo-snippets' version of `tempo-complete-tag.'

\(fn &optional SILENT)" t nil)

;;;***

;;;### (autoloads (tempo-x-space) "tempo-x" "tempo-x.el" (18543 20471))
;;; Generated autoloads from tempo-x.el

(autoload 'tempo-x-space "tempo-x" "\
Expand tempo if complete in `tempo-local-tags' or insert space.

\(fn)" t nil)

;;;***

;;;### (autoloads (teyjus teyjus-edit-mode teyjus-mode) "teyjus"
;;;;;;  "teyjus.el" (18543 20462))
;;; Generated autoloads from teyjus.el

(autoload 'teyjus-mode "teyjus" "\
Major mode for interacting with an inferior teyjus process.
Return after the end of the process' output sends the text from the
    end of process to the end of the current line.
Return before end of process output copies rest of line to end (skipping
    the prompt) and sends it.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

\(fn)" t nil)

(autoload 'teyjus-edit-mode "teyjus" "\
Mode for editing Lambda Prolog Files

\(fn)" t nil)

(autoload 'teyjus "teyjus" "\
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

;;;### (autoloads (tf-run-with-idle-timer) "timerfunctions" "timerfunctions.el"
;;;;;;  (18543 20462))
;;; Generated autoloads from timerfunctions.el

(autoload 'tf-run-with-idle-timer "timerfunctions" "\
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

;;;### (autoloads (tlfdd-mode) "tlfdd" "tlfdd.el" (18546 31632))
;;; Generated autoloads from tlfdd.el

(autoload 'tlfdd-mode "tlfdd" "\
A major mode for editing Sample files.

\(fn)" t nil)

;;;***

;;;### (autoloads (ub-install-example undo-browse ub-movie-history
;;;;;;  ub-movie-backward ub-mode-on ub-introduction) "undo-browse"
;;;;;;  "undo-browse.el" (18543 20462))
;;; Generated autoloads from undo-browse.el

(autoload 'ub-introduction "undo-browse" "\
Provides electric help from variable `undo-browse-introduction'.

\(fn)" t nil)

(autoload 'ub-mode-on "undo-browse" "\
Not documented

\(fn)" t nil)

(autoload 'ub-movie-backward "undo-browse" "\
Run a movie.  By default, backwards, with argument, forward. 

\(fn &optional DIRECTION)" t nil)

(autoload 'ub-movie-history "undo-browse" "\
Not documented

\(fn)" t nil)

(autoload 'undo-browse "undo-browse" "\
Not documented

\(fn)" nil nil)

(defalias 'undo-movie 'ub-movie-history)

(autoload 'ub-install-example "undo-browse" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (vcard-parse-region vcard-parse-string vcard-pretty-print
;;;;;;  vcard-standard-filters vcard-pretty-print-function) "vcard"
;;;;;;  "vcard.el" (18543 20469))
;;; Generated autoloads from vcard.el

(defvar vcard-pretty-print-function 'vcard-format-sample-box "\
*Formatting function used by `vcard-pretty-print'.")

(custom-autoload 'vcard-pretty-print-function "vcard" t)

(defvar vcard-standard-filters '(vcard-filter-html vcard-filter-adr-newlines vcard-filter-tel-normalize vcard-filter-textprop-cr) "\
*Standard list of filters to apply to parsed vcard data.
These filters are applied sequentially to vcard attributes when
the function `vcard-standard-filter' is supplied as the second argument to
`vcard-parse'.")

(custom-autoload 'vcard-standard-filters "vcard" t)

(autoload 'vcard-pretty-print "vcard" "\
Format VCARD into a string suitable for display to user.
VCARD can be an unparsed string containing raw VCF vcard data
or a parsed vcard alist as returned by `vcard-parse-string'.

The result is a string with formatted vcard information suitable for
insertion into a mime presentation buffer.

The function specified by the variable `vcard-pretty-print-function'
actually performs the formatting.  That function will always receive a
parsed vcard alist.

\(fn VCARD)" nil nil)

(autoload 'vcard-parse-string "vcard" "\
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

(autoload 'vcard-parse-region "vcard" "\
Parse the raw vcard data in region, and return an alist representing data.
This function is just like `vcard-parse-string' except that it operates on
a region of the current buffer rather than taking a string as an argument.

Note: this function modifies the buffer!

\(fn BEG END &optional FILTER)" nil nil)

;;;***

;;;### (autoloads (winring-rename-configuration winring-delete-configuration
;;;;;;  winring-jump-to-configuration winring-prev-configuration
;;;;;;  winring-next-configuration winring-duplicate-configuration
;;;;;;  winring-new-configuration) "winring" "winring.el" (18543
;;;;;;  20462))
;;; Generated autoloads from winring.el

(autoload 'winring-new-configuration "winring" "\
Save the current window configuration and create an empty new one.
The buffer shown in the new empty configuration is defined by
`winring-new-config-buffer-name'.

With \\[universal-argument] prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name.

\(fn &optional ARG)" t nil)

(autoload 'winring-duplicate-configuration "winring" "\
Push the current window configuration on the ring, and duplicate it.

With \\[universal-argument] prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name.

\(fn &optional ARG)" t nil)

(autoload 'winring-next-configuration "winring" "\
Switch to the next window configuration for this frame.

\(fn)" t nil)

(autoload 'winring-prev-configuration "winring" "\
Switch to the previous window configuration for this frame.

\(fn)" t nil)

(autoload 'winring-jump-to-configuration "winring" "\
Go to the named window configuration.

\(fn)" t nil)

(autoload 'winring-delete-configuration "winring" "\
Delete the current configuration and switch to the next one.
With \\[universal-argument] prompt for named configuration to delete.

\(fn &optional ARG)" t nil)

(autoload 'winring-rename-configuration "winring" "\
Rename the current configuration to NAME.

\(fn NAME)" t nil)

;;;***

;;;### (autoloads (zap-to-char zap-upto-char zap-following-char zap-from-char)
;;;;;;  "zap-char" "zap-char.el" (18543 20462))
;;; Generated autoloads from zap-char.el

(autoload 'zap-from-char "zap-char" "\
Kill from ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.

\(fn ARG CHAR)" t nil)

(autoload 'zap-following-char "zap-char" "\
Kill following ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.

\(fn ARG CHAR)" t nil)

(autoload 'zap-upto-char "zap-char" "\
Kill up to ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.

\(fn ARG CHAR)" t nil)

(autoload 'zap-to-char "zap-char" "\
Kill to ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.

\(fn ARG CHAR)" t nil)

;;;***

;;;### (autoloads nil nil ("anything-config.el" "anything.el" "auto-save.el"
;;;;;;  "bibsnarf.el" "c-font-lock-keywords.el" "color-eldoc.el"
;;;;;;  "color-moccur.el" "color-mode.el" "dired-details.el" "dokuwiki.el"
;;;;;;  "elscreen.el" "emacs-type.el" "flashcard.el" "fracc.el" "gdoc-mode.el"
;;;;;;  "gnuslog.el" "google-emacs-utilities.el" "grade.el" "highlight-current-line.el"
;;;;;;  "hl-line+.el" "http-post.el" "ipa.el" "ipython.el" "ll-debug.el"
;;;;;;  "magpie.el" "message-x.el" "moccur-edit.el" "moinmoin-mode.el"
;;;;;;  "multi-region.el" "nnmairix.el" "nntodo.el" "osd.el" "outline-magic.el"
;;;;;;  "physical-line.el" "post.el" "pp-c-l.el" "project.el" "py-complete.el"
;;;;;;  "radio.el" "rcirc-color.el" "rcirc-controls.el" "rcirc-late-fix.el"
;;;;;;  "scroll-in-place.el" "sig.el" "sure-tags.el" "tree-widget.el"
;;;;;;  "typopunct.el" "visible-mark-mode.el" "xcscope.el" "xml-rpc.el"
;;;;;;  "xterm-extras.el") (18958 63615 646191))

;;;***

