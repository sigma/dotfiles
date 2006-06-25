;;; elect-mbuf.el --- Minibuffer completion and default input cycling.
;;
;; Filename: elect-mbuf.el
;; Description: Minibuffer completion and default input cycling.
;; Author: koomen@cs.rochester.edu
;;	   Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2004, Drew Adams, all rights reserved.
;; Created: Tue Aug  1 14:21:16 1995
;; Version: 21.0
;; Last-Updated: Thu Dec 30 23:49:03 2004
;;           By: dradams
;;     Update #: 790
;; Keywords: internal, lisp, extensions, help, abbrev, faces, local
;; Compatibility: GNU Emacs 21.x, GNU Emacs 20.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;;    Extensions to the `completing-read' commands in the minibuffer.
;;    Enables minibuffer cycling of default inputs via the arrow keys,
;;    and more support for input completion.
;;
;;  Main new functions here:
;;
;;    `abort-minibuffer-input', `assoc-tail', `buffer-alist',
;;    `minibuffer-completion-help+', `mod+', `next-default-input',
;;    `previous-default-input', `rebind-minibuffer-completion-maps'.
;;
;;  Main new user options (variables) defined here:
;;
;;    `mbuf-completion-help-face', `mbuf-completion-help-title-face'.
;;
;;  Other new variables defined here:
;;    `command-calling-for-completion', `completing-read-prompt',
;;    `completing-read-prompt-suffix',
;;    `minibuffer-completion-help-string'.
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;  `completing-read' - (See below, and doc string.)
;;  `exit-minibuffer' -
;;     1) Remove *Completion* window.
;;     2) Reset `minibuffer-completion-table' so no icompletion.
;;
;; Hitting `M-n' (`M-p') will yank the next (previous) default value
;; into the buffer, from the current list of completions.  For
;; example, suppose you execute the `switch-to-buffer' command, but
;; the default "other buffer" is not the one you desire.  Instead of
;; typing the desired buffer's name, just hit `M-n' until the desired
;; buffer's name appears in the minibuffer, and hit return.  This also
;; works with any command which reads a function or variable name, or
;; a programmer specified completion list.  It does not work, however,
;; with functions which read a file name.
;; 
;; Key Bindings
;; ------------
;;
;; The following binding is made here for mode `completion-list-mode'.
;; Also, character self-insertion is now inhibited in that mode.
;;
;;    `C-g'        `abort-minibuffer-input'
;;
;; The following bindings are made here for the minibuffer during completion.
;;
;;    `?'          `minibuffer-completion-help+'
;;    NEXT         `next-default-input', where NEXT is any binding
;;                 for `next-line'
;;    PREV         `previous-default-input', where PREV is any binding
;;                 for `previous-line'
;;
;; Command`delete-lines' is bound here to [M-S-backspace] and [M-S-??]
;; in all minibuffer-local-* keymaps.
;;
;; The current buffer's binding for ESC TAB is now also used for the
;; minibuffer.
;;
;;
;; NOTE:
;; Load this file only after all other files are loaded that bind
;; keys, so that the top-level `rebind-minibuffer-completion-maps'
;; here will pick up all those previous definitions.
;;
;;  Library `elect-mbuf' requires these libraries:
;;
;;    `avoid', `frame-cmds', `frame-fns', `icomplete', `icomplete+',
;;    `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 2004/09/21 dadams
;;     Updated to work in Emacs 21 (and 20): 
;;       next-default-input uses delete-minibuffer-contents for 21, but
;;          erase-buffer for 20.
;;       minibuffer-completion-help+: bind inhibit-read-only to t around
;;       erase-buffer.
;; 2001/01/10 dadams
;;     Protected remove-windows-on via fboundp.
;; 1999/09/03 dadams
;;     1. Added: mbuf-completion-help-face, mbuf-completion-help-title-face.
;;     2. minibuffer-completion-help+: use mbuf-*-face's instead of hard-coding.
;;     3. minibuffer-completion-help-string, completing-read-prompt-suffix:
;;          defconst -> defvar.
;; 1999/08/26 dadams
;;     Protected faces via boundp.
;; 1999/04/13 dadams
;;     Bound delete-lines to M-S-DEL and M-S-backspace.
;; 1999/03/17 dadams
;;     protect calls with test fboundp.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/03/26 dadams
;;     minibuffer-completion-help+: concat -> concat-w-faces (color).
;; 1995/12/20 dadams
;;     exit-minibuffer: Iconify *Completion* frame.
;; 1995/12/15 dadams
;;     1. abort-minibuffer-input: Reset minibuffer-completion-table to avoid
;;        icompletion.
;;     2. Defined replacement exit-minibuffer to do the same as #1.
;; 1995/12/01 dadams
;;     1) abort-minibuffer-input: Incorporated delete-selection-mode code.
;;     2) rebind-minibuffer-completion-maps: Added C-g bindings for
;;          minibuffer-local-map, minibuffer-local-ns-map,
;;          minibuffer-local-isearch-map.
;; 1995/10/25 dadams
;;     Put defvar of minibuffer-completion-help-string after do
;;     rebind-minibuffer-completion-maps, so its doc string gives bindings.
;; 1995/10/24 dadams
;;     Mention ESC-TAB completion in completing-read.
;; 1995/10/17 dadams
;;     Let minibuffer use ESC-TAB for completion (of Lisp symbols etc.)
;;     1) completing-read: Minibuffer adopts current buffer's ESC-TAB binding.
;;     2) Added command-calling-for-completion to memorize current command
;;        (done in completion-setup-hook).
;; 1995/09/12 dadams
;;     1) Added abort-minibuffer-input.
;;     2) Define C-g as abort-minibuffer-input in completion-list-mode-map
;;        and minibuffer-local-* maps.
;;     3) No self-insertion for completion-list-mode-map.
;; 1995/08/16 dadams
;;     next-default-input: Fixed bug - skip over repeated alist entries.
;; 1995/08/10 dadams
;;     1) Rewrote minibuffer-completion-help+: Provide help even if no completions.
;;     2) So, added minibuffer-completion-help-string.
;;     3) `?' defined correctly for minibuffer-local-must-match-map.
;; 1995/08/08 dadams
;;     next-default-input: error msg: no hard coding of key seq.
;; 1995/08/02 dadams
;;     Major rewrite.
;;     1) No reminders in prompts.
;;     2) Added minibuffer-completion-help+ to provide help info for *Completions*.
;;
;; Original posting:
;; From koomen@cs.rochester.edu Mon Jun 19 19:27:58 1989
;; To: info-gnu-emacs@prep.ai.mit.edu
;; Cc: Hans <Koomen@cs.rochester.edu>
;; Subject: elect-mbuf.el
;; Date: Tue, 13 Jun 89 15:17:07 -0400
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

(and (< emacs-major-version 21)         ;; caar, push, pop 
     (eval-when-compile (require 'cl))) ;; (plus, for Emacs <20: when, unless)

;; Get macro `define-face-const' when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
(eval-when-compile (require 'def-face-const))

(require 'strings nil t) ;; (no error if not found): concat-w-faces
(require 'frame-cmds nil t) ;; (no error if not found): remove-windows-on


;;;;;;;;;;;;;

(defsubst mod+ (x y mod)
  "Add X and Y and take MOD."
  (setq x (+ y x))
  (if (>= x mod) (setq x (- x mod)))
  (if (< x 0) (setq x (+ x mod)))
  x)

;;; User variables

;;;###autoload
(defvar mbuf-completion-help-face
  (or (and (boundp 'darkmagenta-foreground-face)
           darkmagenta-foreground-face)
      (define-face-const "DarkMagenta" nil))
  "*Face used to highlight minibuffer completion help.")

;;;###autoload
(defvar mbuf-completion-help-title-face
  (or (and (boundp 'red-foreground-face)
           red-foreground-face)
      (define-face-const "Red" nil))
  "*Face used for instructions introducing completion help.")


;;; Internal variables

(defvar minibuffer-last-default nil
  "If `minibuffer-completion-table' is an obarray, then this is the
index in that array of the default value to be read.  If it is an
association list (alist), then this is the default value as a string.

For optimal performance, any function which reads from the minibuffer
and accepts a default value should, before reading, put that value in
`minibuffer-last-default'.  For example, `completing-read' does that.")

;;;###autoload
(defun buffer-alist ()
  "Alist of (BUF-NAME . BUF) items, where BUF-NAME (a string) names BUF,
which is in (buffer-list), and BUF-NAME does not start with SPACE."
  (let (bn-alist)
    (mapcar (function (lambda (buf) (let ((bn (buffer-name buf)))
                                      (unless (equal " " (substring bn 0 1))
                                        (push (cons bn buf) bn-alist)))))
            (buffer-list))
    (reverse bn-alist)))

(defvar completing-read-prompt "")


(or (fboundp 'old-completing-read)
(fset 'old-completing-read (symbol-function 'completing-read)))

;; REPLACES ORIGINAL:
;; (See doc string here and Commentary at beginning of this file.)
;; COMPLETING-READ-PROMPT-SUFFIX is a free variable here.
;; It is defined at the end of this file, so that its doc string is up to date.
;;;###autoload
(defun completing-read
  (prompt table &optional
          predicate require-match initial-input hist def inherit-input-method)
  "Read string in minibuffer, with completion and default input cycling.
Minibuffer completion help via \\<minibuffer-local-completion-map>\
\\[minibuffer-completion-help+]. Completion via \\[minibuffer-complete-word], \
\\[minibuffer-complete] and ESC-TAB.
Cycling of default inputs via \\[previous-default-input] and \
\\[next-default-input].
Cycling of past inputs via \\[previous-history-element] and \
\\[next-history-element].
Searching through input history via \\[previous-matching-history-element] \
and \\[next-matching-history-element].
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

Completion ignores case when`completion-ignore-case' is non-nil."
  (setq minibuffer-completion-table table)
  (unless initial-input (setq initial-input ""))
  (setq initial-input (format "%s" initial-input)) ; Convert (symbol) to string
  (setq minibuffer-last-default (if (arrayp table) 0 initial-input))
  ;; Let minibuffer use the current buffer's binding for ESC TAB.
  (let* ((map (or (current-local-map) (current-global-map)))
         (esc-tab-fn (lookup-key map "\M-\t")))
    (define-key minibuffer-local-completion-map "\M-\t" esc-tab-fn)
    (define-key minibuffer-local-must-match-map "\M-\t" esc-tab-fn))
  ;; Append suffix if prompt not too long.
  (if (> (length initial-input)
         (- (window-width (minibuffer-window)) (length prompt)))
      (setq completing-read-prompt prompt) ; No suffix added.
    (setq completing-read-prompt (concat prompt
                                         completing-read-prompt-suffix)))
  (old-completing-read completing-read-prompt table predicate require-match
                       initial-input hist def inherit-input-method))

(defun next-default-input (&optional incr)
  "Replace input by next possible default input."
  (interactive)
  (setq incr (or incr 1))
  (cond (;;vector keymap
         (arrayp minibuffer-completion-table)
         (if (< emacs-major-version 21)
             (erase-buffer)
           (delete-minibuffer-contents))
         (let ((len (length minibuffer-completion-table))
               next
               orig)
           (cond ((not (and (numberp minibuffer-last-default)
                            (>= minibuffer-last-default 0)
                            (< minibuffer-last-default len)))
                  (setq next 0)
                  (setq orig (- len 1)))
                 (t
                  (setq orig minibuffer-last-default)
                  (setq next (mod+ orig incr len))))
           (while
               (and minibuffer-completion-predicate
                    (not (= next orig))
                    (not (condition-case err
                             (funcall minibuffer-completion-predicate
                                      (aref minibuffer-completion-table next))
                           (error nil))))
             (setq next (mod+ next incr len))) ; Cycle next one.
           (while (and (not (= next orig)) ; Skip to non-empty keymap entry.
                       (eq 0 (aref minibuffer-completion-table next)))
             (setq next (mod+ next incr len)))
           (setq minibuffer-last-default next) ; Set new last default.
           (insert-string
            (prin1-to-string (aref minibuffer-completion-table next)))))
        (;; alist keymap
         (consp minibuffer-completion-table)
         (if (< emacs-major-version 21)
             (erase-buffer)
           (delete-minibuffer-contents))
         (let ((table (if (< incr 0)
                          (reverse minibuffer-completion-table)
                        minibuffer-completion-table))
               next orig remainder)
           (setq orig (assoc-tail minibuffer-last-default table))
           (setq next (cdr orig))
           (while (or (null (car next)) ; Skip entries if null or the same.
                      (string= (caar orig) (caar next)))
             (pop next)
             (unless next (setq next table))) ; Back to table beginning.
           (while (and minibuffer-completion-predicate
                       (not (eq next orig))
                       (not (condition-case err
                                (funcall minibuffer-completion-predicate
                                         (car next))
                              (error nil))))
             (pop next)                 ; Cycle next one.
             (while (null (car next))   ; Skip null alist entries.
               (pop next)
               (unless next (setq next table)))) ; Back to table beginning
           (setq minibuffer-last-default (caar next)) ; Reset last default
           (insert-string minibuffer-last-default)
           (when (> (length minibuffer-last-default)
                    (- (window-width (minibuffer-window))
                       (length completing-read-prompt)))
             (beginning-of-line))))
        (t
         ;; Function name as keymap: e.g. 'read-file-name-internal.
         ;; Cannot cycle.
         (when (fboundp 'flash-ding-minibuffer-frame) (flash-ding-minibuffer-frame))
         (message
          (substitute-command-keys
           "Cannot cycle default inputs. Try \
\\<minibuffer-local-completion-map>\\[minibuffer-complete-word]/\
\\[minibuffer-complete]/\\[minibuffer-completion-help+] to complete.")))))

(defun previous-default-input ()
  "Replace input by previous possible default input."
  (interactive)
  (next-default-input -1))

;;;###autoload
(defun assoc-tail (element list)
  "Returns longest (i.e. first) tail of LIST whose caar matches ELEMENT.
Matching is done with function `equal'."
  (let ((continue t))
    (while continue
      (if (or (null list) (equal element (caar list)))
          (setq continue nil)
        (pop list)))
    list))

;;;###autoload
(defun abort-minibuffer-input ()
  "Abort input, or, if in delete selection mode, deactivate the mark.
Iconify \"*Completions*\" frame, if any, before aborting minibuffer
input via `abort-recursive-edit'.

This can be used in delete selection mode to cancel a selection in
the minibuffer without aborting.  (A second C-g will then abort.)
This feature won't work if using icomplete (`icomplete-inhibit' =
nil; see file `icomplete.el')."
  (interactive)
  (if (and (boundp 'delete-selection-mode) ; Defined in `delsel.el'.
           delete-selection-mode
           transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (fboundp 'remove-windows-on)  ; Defined in `frame-cmds.el'.
      (remove-windows-on "*Completions*"))
    (setq minibuffer-completion-table nil) ; To avoid icompletion by default.
    (abort-recursive-edit)))

;; This is just the macro expansion of the following:
;; `(def-completion-wrapper abort-minibuffer-input :minibuffer-separator)'.
;; Taken from the definition of `def-completion-wrapper' in `completion.el'.  
(put 'abort-minibuffer-input 'completion-function
     'use-completion-minibuffer-separator)

;; `completion-list-mode-map' not to allow normal input, and to abort on `C-g'.
(define-key completion-list-mode-map "\C-g" 'abort-minibuffer-input)
(suppress-keymap completion-list-mode-map) ; Inhibit character self-insertion.

;;;###autoload
(defun rebind-minibuffer-completion-maps ()
  "Rebind minibuffer completion maps to be able to cycle minibuffer
default inputs.  This should be done after all keys have been bound,
because it redefines minibuffer keymaps."

  ;; Default minibuffer map.
  (define-key minibuffer-local-map "\C-g" 'abort-minibuffer-input)
  ;; This key is normally bound globally to `clear-rectangle',
  ;; which has little use in the minibuffer.
  (when (fboundp 'delete-lines)         ; Defined in `misc-cmds.el'.
    (define-key minibuffer-local-map [M-S-backspace] 'delete-lines)
    (define-key minibuffer-local-map [M-S-??] 'delete-lines))

  ;; Default minibuffer map when spaces are not allowed.
  (define-key minibuffer-local-ns-map "\C-g" 'abort-minibuffer-input)
  ;; This key is normally bound globally to `clear-rectangle',
  ;; which has little use in the minibuffer.
  (when (fboundp 'delete-lines)         ; Defined in `misc-cmds.el'.
    (define-key minibuffer-local-ns-map [M-S-backspace] 'delete-lines)
    (define-key minibuffer-local-ns-map [M-S-??] 'delete-lines))

  ;; Minibuffer map for editing isearch strings.
  (define-key minibuffer-local-isearch-map "\C-g" 'abort-minibuffer-input)
  (when (fboundp 'delete-lines)         ; Defined in `misc-cmds.el'.
    (define-key minibuffer-local-isearch-map [M-S-backspace] 'delete-lines)
    (define-key minibuffer-local-isearch-map [M-S-??] 'delete-lines))

  ;; Completion map
  (define-key minibuffer-local-completion-map "\C-g" 'abort-minibuffer-input)
  ;; This key is normally bound globally to `clear-rectangle',
  ;; which has little use in the minibuffer.
  (when (fboundp 'delete-lines)         ; Defined in `misc-cmds.el'.
    (define-key minibuffer-local-completion-map [M-S-backspace] 'delete-lines)
    (define-key minibuffer-local-completion-map [M-S-??] 'delete-lines))
  (define-key minibuffer-local-completion-map "?"
    'minibuffer-completion-help+)
  (mapcar (function (lambda (keyseq)
                      (define-key minibuffer-local-completion-map keyseq
                        'previous-default-input)))
          (where-is-internal 'previous-line nil nil))
  (mapcar (function (lambda (keyseq)
                      (define-key minibuffer-local-completion-map keyseq
                        'next-default-input)))
          (where-is-internal 'next-line nil nil))
  
  ;; Must-match map.
  (define-key minibuffer-local-must-match-map "\C-g" 'abort-minibuffer-input)
  ;; This key is normally bound globally to `clear-rectangle',
  ;; which has little use in the minibuffer.
  (when (fboundp 'delete-lines)         ; Defined in `misc-cmds.el'.
    (define-key minibuffer-local-must-match-map [M-S-backspace] 'delete-lines)
    (define-key minibuffer-local-must-match-map [M-S-??] 'delete-lines))
  (define-key minibuffer-local-must-match-map "?"
    'minibuffer-completion-help+)
  (mapcar (function (lambda (keyseq)
                      (define-key minibuffer-local-must-match-map keyseq
                        'previous-default-input)))
          (where-is-internal 'previous-line nil nil))
  (mapcar (function (lambda (keyseq)
                      (define-key minibuffer-local-must-match-map keyseq
                        'next-default-input)))
          (where-is-internal 'next-line nil nil)))


;; `minibuffer-completion-help-string' is a free variable here.
;; It is defined at the end of this file, so that its doc string is up to date.
;;;###autoload
(defun minibuffer-completion-help+ ()
  "Describe minibuffer bindings.  Display list of possible completions
of current minibuffer contents."
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create "*Completions*"))
    (let ((inhibit-read-only t)) (erase-buffer)))
  (minibuffer-completion-help)
  (switch-to-buffer-other-window "*Completions*")
  (beginning-of-buffer)
  (cond ((eobp)                         ; No completions available.
         (insert minibuffer-completion-help-string)
         (beginning-of-buffer))
        ((fboundp 'concat-w-faces)
         (insert (concat-w-faces
                  (list mbuf-completion-help-face
                        minibuffer-completion-help-string)
                  (list mbuf-completion-help-title-face
                        "For help on completion, see beginning of this buffer.
"))))
        (t
         (insert (concat minibuffer-completion-help-string
                         "For help on completion, see beginning of this buffer.
"))))
  (let ((compl-win (get-buffer-window "*Completions*" 'visible)))
    (when compl-win (set-window-point compl-win (point))))
  (set-buffer-modified-p nil))

;;;###autoload
(defvar command-calling-for-completion nil
  "Last command causing display of list of possible completions.")

;; Remember command that called for completion.
(add-hook 'completion-setup-hook
          (function (lambda ()
                      (setq command-calling-for-completion this-command)))
          'append)


(or (fboundp 'old-exit-minibuffer)
(fset 'old-exit-minibuffer (symbol-function 'exit-minibuffer)))

;; REPLACES ORIGINAL:
;; 1) Remove *Completion* window.
;; 2) Reset `minibuffer-completion-table' so no icompletion unless desired.
;;;###autoload
(defun exit-minibuffer ()
  "Terminate this minibuffer argument."
  (interactive)
  (when (fboundp 'remove-windows-on)    ; Defined in `frame-cmds.el'.
    (remove-windows-on "*Completions*"))
  ;; Don't icomplete by default from now on.
  (setq minibuffer-completion-table nil)
  (old-exit-minibuffer))


;; Do the bindings here and now, for default bindings.
;; May need to be done again (e.g. at startup time) to pick up new bindings.
(rebind-minibuffer-completion-maps)



;;; `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'

;; Do this *after* calling `rebind-minibuffer-completion-maps'
;; so bindings are up-to-date.
(defvar minibuffer-completion-help-string
  (substitute-command-keys
   "\\<minibuffer-local-completion-map>                        \
Minibuffer Completion
                        ---------------------

Your input in the minibuffer can be completed in several ways.
You can:

  * Complete the current input in the minibuffer.
	Complete a word at a time:		\\[minibuffer-complete-word]
	Complete as much as possible:		\\[minibuffer-complete]

  * Choose a default input value.
	Cycle among candidate default values:	\\[previous-default-input], \
\\[next-default-input]

  * Choose a previous input from the minibuffer history.
	Cycle among minibuffer history items:	\\[previous-history-element], \
\\[next-history-element]
	Search among minibuffer history items:	\
\\[previous-matching-history-element], \\[next-matching-history-element]

  * Choose from the possible completions for the current input.
	Show the list, plus this explanation:	\\[minibuffer-completion-help+]
	Choose from the list:			\\<completion-list-mode-map>\
\\[choose-completion], \\[mouse-choose-completion]
                                       	(in buffer *Completions*)
	The list is also available when completion is ambiguous.

  * Modify your input if you like, once chosen or completed.
	Send your finished input to Emacs: 	\
\\<minibuffer-local-completion-map>\\[exit-minibuffer]
	Abandon minibuffer input:		\\[abort-recursive-edit]

Remember: You can always input any character that is bound to a
          command (e.g. \\[minibuffer-complete-word], \
\\[minibuffer-complete], \\[minibuffer-completion-help+]) by preceding it \
with \\<global-map>\\[quoted-insert].

Here are the current bindings for the minibuffer:

\\{minibuffer-local-completion-map}---------------------------------------\
---------------------
")
  "Description of minibuffer bindings.")

;; Do *after* calling `rebind-minibuffer-completion-maps' for default suffix.
(defvar completing-read-prompt-suffix
  (substitute-command-keys
   "(\\<minibuffer-local-completion-map>\\[minibuffer-completion-help+] for \
help)  ")
  "String to append to `completing-read's prompt, if there is room.
Intended to remind you how to obtain input completion help.
Set this to nil or to \"\" to append nothing.")

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'elect-mbuf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elect-mbuf.el ends here
