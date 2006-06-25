;;; todl-mode.el --- For editing TODO Lists and simple hypertext.

;; Copyright (C) 2004 Joe Corneli <jcorneli@math.utexas.edu>
;;                    Sacha Chua  <sacha@free.net.ph>

;; Time-stamp: <21/04/2004 10:10:13 Yann Hodique>

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; TODL is a knowledge-management system that helps you organize your
;; information into lists.  These lists can refer to one another.
;;
;; One of the possible uses of this mode is to maintain a hierarchical
;; TODO List.  For example, if your afternoon's tasks include shopping
;; for groceries and returning your library books, you might have a
;; set of todo lists that look like this:
;;
;; Main tasks
;;  return library books
;;  go to store
;;  cook dinner
;;
;; At store
;;  buy cauliflower
;;  buy potatoes
;;  buy spinach
;;
;; With TODL mode, you would represent these lists this way:
;;
;; Top-level node: Main tasks
;;     return library books
;; *   go to store
;;     cook dinner
;;
;; Top-level node: go to store
;;   buy
;;     cauliflower
;;     potatoes
;;     spinach
;;
;; We refer to the basic elements of the TODL language as "nodes" and
;; "edges".  Edges are labels, like "buy" in the above example, that
;; tell you what the connection is between a two given nodes; between,
;; "cauliflower" and "go to store" for example.  When nodes are not
;; connected by a labeled edge, as is the case with "Main tasks" and
;; the three nodes it refers to, the connection is considered to be
;; made by an unlabeled edge.  Similarly, edges that are not
;; associated with any nodes are considered to connect to `nil'.

;; Notice that in the example we speak of "return library books" and
;; "cook dinner" as nodes, even though their definitions have not been
;; developed.  When developed nodes are referred to, a star ("*")
;; appears in the left margin.  This is particularly helpful when
;; running TODL mode with `todl-keep-narrowed' set to true, because,
;; when this variable is set, only one node is visible in the buffer
;; at a time, so the *'s are the only way to see what is going on in
;; the neighborhood of the current node.
;;
;; TODL mode can be used for plenty of other things besides todo
;; lists.  For example, it could be used to sketch out a semantic
;; network, to document software, or to take notes in class.  What
;; distinguishes it from e.g. a wiki is its almost complete lack of
;; markup, the fact that each network of nodes and edges is contained
;; in one buffer only (at least for now), and the fact that everything
;; is viewed as a list.
;;
;; This last feature is something that TODL has in common with LISP.
;; Indeed, this is why we chose the name TODL to describe this
;; language.  We hope that, one day, with a smart enough interpreter,
;; TODL might "do to LISP what LISP has done to machine language".

;; A user manual and developer's notes in both the TODL source format
;; and in HTML produced from these sources by TODL mode's
;; `todl-to-html' function are available from this URL:
;;
;; http://www.ma.utexas.edu/~jcorneli/y/todl/
;;
;; Some comments for on how you can set up Emacs to use TODL mode
;; follow.
;;
;; You can put this file in a directory called ~/site-lisp/ and add
;; these lines to your .emacs:
;;
;; (add-to-list 'load-path "~/site-lisp/")
;; (load "todl-mode")
;;
;; Files with the .todl suffix or a "-*- Mode: Todl -*-" string in the
;; first line will then be loaded in TODL mode.  You can also switch
;; into TODL mode with the command M-x todl-mode.

;;; History:

;; TODL was prototyped in early 2003 as a bash script with the name
;; "todo".  This system inherits ideas from a variety of other simple
;; hypertext systems.  The current implementation benefits
;; tremendously from running under Emacs.

;;; Code:

(defvar todl-has-run nil)

(defun todl-print-welcome-message ()
  (unless todl-has-run
    (message todl-welcome-message)
    (setq todl-has-run t)))

;; This info prints out *roughly* in reverse when you run
;; `describe-mode'.  Not quite, though, and I think perhaps there is
;; an emacs bug lurking in the way it is printed, since it would be
;; nice to be able to control the order.

(defvar todl-mode-map
 (let ((map (make-sparse-keymap)))
   (define-key map "\C-c\C-i" 'todl-generate-index)
   (define-key map "\C-c\C-p" 'todl-to-html)
   (define-key map "\C-cm" 'todl-markup-buffer)
   (define-key map "\C-\M-e" 'todl-end-of-node)
   (define-key map "\C-\M-a" 'todl-beginning-of-node)
   (define-key map "\C-ch" 'todl-clear-history)
   (define-key map "\C-cb" 'todl-scroll-history-backwards)
   (define-key map "\C-cf" 'todl-scroll-history-forwards)
   (define-key map "\C-cn" 'todl-enter-next-node)
   (define-key map "\C-cp" 'todl-enter-previous-node)
   (define-key map "\C-cg" 'todl-goto-node)
   (define-key map "\C-cw" 'todl-toggle-narrowing)
   (define-key map "\C-cd" 'todl-delete-current-node)
   (define-key map "\C-cr" 'todl-change-current-list-name)
   (define-key map "\C-c\C-c" 'todl-open-node)
   (define-key map "\C-o" 'todl-insert-node)
   (define-key map "\M-o" 'todl-insert-edge)
   (define-key map "\C-\M-o" 'todl-create-top-level-node)
   map)
 "Keymap for TODL major mode.")

(add-to-list 'auto-mode-alist '("\\.todl\\'" . todl-mode))

(defconst todl-font-lock-keywords-1
  (list '("^Top-level node:" . font-lock-builtin-face)
        ;; titles
        '("\\(^Top-level node: \\)\\(.*\\)" 2 font-lock-type-face)
        ;; edges
        '("^  [^ ].*" . font-lock-keyword-face)
        ;; stars
        '("\\(^*\\)\\(   \\)\\([^ ]\\)" 1 font-lock-comment-face))
  "Minimal highlighting expressions for TODL mode.")

(defconst todl-font-lock-keywords-2
  (append todl-font-lock-keywords-1
        (list
         ;; links
         '("\\(^*\\)\\(   \\)\\([^ ].*\\)" 3 font-lock-doc-face)))
  "Gaudy highlighting expressions for TODL mode.")

;; at some point, more adjustments to the syntax table might be done.
(defvar todl-mode-syntax-table
  (let ((todl-mode-syntax-table text-mode-syntax-table))
    todl-mode-syntax-table))

;;; User-level variables

;; TODO: make these into customization groups

;; `todl:hooks'

(defvar todl-change-history-hook '(todl-history-to-hist-buffer)
  "*Functions to run when the history is cleared.")

(defvar todl-switch-nodes-hook nil
  "*Functions to run when switching from node to node.")

(defvar todl-node-created-hook nil
  "*Functions to run after a new node is created.")

(defvar todl-node-deleted-hook nil
  "*Functions to run after a new node is created.")

(defvar todl-mode-hook '(todl-print-welcome-message
                         todl-set-default-locale-language)
  "*Functions to run when `todl-mode' runs.")

;; `todl:localization'

(defvar todl-locale-language "English"
  "*The operating language for `todl-mode' buffers.
Set this variable using `todl-set-locale-language'.  Currently
only English is supported.")

;; `todl:provenance'

(defvar todl-provenance-track-origination t
  "*Whether to keep track of creation of new nodes.")

(defvar todl-provenance-track-modification t
  "*Whether to keep track of modifications to existing nodes.")

(defvar todl-sign-edits nil
  "*Whether or not to sign your edits.")

(defvar todl-signature (user-login-name)
  "*The string that is used to sign edits.")

(defvar todl-date-edits t
  "*Whether or not to sign your edits.")

(defvar todl-time-string-format
  "%Y-%m-%d %T %Z"
  "*Time format string that is used to date edits.
For details see the function `format-time-string'.")

;; todl strives to be universal.
(defvar todl-use-universal-time t
  "*If non-nil, use Universal Time, Coordinated (UTC).")

;; `todl:general'

(defvar todl-pub-directory "~/WebTodl/"
  "*Directory for HTML files published using `todl-to-html'.")

(defvar todl-font-lock-keywords todl-font-lock-keywords-1
  "*Default highlighting expressions for TODL mode.
Can be `todl-font-lock-keywords-1' or `todl-font-lock-keywords-2'.")

(defvar todl-keep-narrowed nil
"If non-nil, you only see one definition at a time.
Otherwise you see them all.  Toggle with `todl-toggle-narrowing'.")

(defvar todl-show-history-buffer nil
"If non-nil, show movements through history in a separate buffer.
Toggle with `todl-toggle-hist-buffer'.")

;; Some unimplemented ideas

;; This is a nice idea for interchange between people working with
;; different languages, but is it *possible* to rewrite buffer
;; substrings so they are suitable for any locale OTF? In such a way
;; so as to not actually change the buffer's content?  It sounds
;; entirely plausible.  This should go along with a function that
;; detects the language that is being used in the file.
;(defvar todl-use-locale-language nil
;  ;; Currently there is one keyword: `todl-top-level-node-phrase'.
;  "If non-nil, translate keywords into the local language.")

;; the idea here is that the user may want to write down tasks from a
;; generic buffer (as with the planner/remember suite). See comments
;; on `todl-source-directory'.
;(defvar todl-default-todo-list "~/TODO"
;  "When you need a todo list, and none was specified, use this one.")

;; The user may specify one of these files to add a node to.  For
;; example, if this directory contains a file
;; "emacs-feature-requests.todl", the user may want to add a node
;; NEW-FEATURE without having to hunt for this file.  So they will be
;; prompted for a file name, a node name, and maybe then given a
;; pop-up buffer to edit in or something.  Maybe other editing
;; commands can be done "remotely" as well.
;(defvar todl-source-directory "~/todl/"
;  "Main directory for `todl-mode' sources.")

;;; I18N support

;; Strings that are seen by the user should be defined in this
;; section, together with their default (English) values.  These
;; strings are to be cited by name when they are used in the code. To
;; add or examine translations into new languages, see
;; `todl-set-locale-language'.

(defvar todl-language-list (list "English")
  "Options allowed in `todl-set-locale-language'.")

(defvar todl-welcome-message
  "Welcome to information retrieval."
  "Printed when the user first runs `todl-mode'.")

(defvar todl-top-level-node-phrase
  "Top-level node"
  "The name used for the basic objects in TODL.")

(defvar todl-default-node-name-prompt
  "Node name: "
  "The default prompt when reading a nodename from the minibuffer.")

(defvar todl-new-node-name-prompt
  "New top-level node: "
  "Prompt used in `todl-create-top-level-node'.")

(defvar todl-first-visited-node-message
  "You are at the first visited node."
  "Printed when the user goes to the beginning of history.")

(defvar todl-most-recently-visited-node-message
  "You are at the most recently visited node."
  "Printed when the user goes to the end of history.")

(defvar todl-open-node-howto-message
  "Issue this command with cursor positioned on a node."
  "Printed when the command `todl-open-node' is issued incorrectly.")

(defvar todl-processing-prefix-message
  "Processing"
  "Start of message printed for nodes processed by `todl-to-html'.")

(defvar todl-mode-done-processing-message
  "Done."
  "Printed when `todl-to-html' is done running.")

(defvar todl-node-created-by-string
  "node created by"
  "Inserted when creating a node if `todl-sign-edits' is non-nil.")

(defvar todl-node-created-at-string
  "node created at"
  "Inserted when creating a node if `todl-date-edits' is non-nil.")

(defvar todl-language-prompt
  "Language: "
  "Prompt used by `todl-set-locale-language'.
See also `todl-language-list'.")

(defvar todl-language-not-supported-message
  "Sorry, that language is not supported!"
  "Inserted when creating a node if `todl-date-edits' is non-nil.")

(defvar todl-no-more-links-message
  "No more links!"
  "Printed by `todl-next-link' if there are no more links here.")

(defvar todl-new-name-prompt
  "New name: "
  "Printed by `todl-change-current-list-name'.")

(defvar todl-node-exists-string
  "Node \"%s\" already exists! Go to it? "
  "Printed upon entering REPL in `todl-create-top-level-node'.
Expects a node name as a separate argument to `format'.")

;; the following derived variables are not likely to need to be set
;; for a given localization.

(defvar todl-top-level-node-string
  (concat todl-top-level-node-phrase ": ")
  "The full string used to mark the beginning of a basic TODL object.")
(defvar todl-beginning-of-node-regexp
  (concat "^" todl-top-level-node-phrase ":\\s-+")
  "Regexp that marks the beginning of a basic TODL object.")

(defun todl-set-top-level-node-vars ()
  "Set `todl-top-level-node-string', `todl-beginning-of-node-regexp'.
Called by`todl-set-locale-language'."
  (setq todl-top-level-node-string
        (concat todl-top-level-node-phrase ": ")
        todl-beginning-of-node-regexp
        (concat "^" todl-top-level-node-phrase ":\\s-+")))

(defun todl-set-locale-language (lang)
  "Set `todl-mode' operating language to LANG.
Currently only English is supported."
  ;; a completing read, with the set of supported languages as the
  ;; possible completions
  (interactive (list (let ((completion-ignore-case t))
                       (completing-read todl-language-prompt
                                        todl-language-list))))
  (cond
   ;; add new languages and translations following a similar form
   ((or (equal lang "English")
        (equal lang "english"))
    (setq todl-locale-language "English")
    (setq todl-welcome-message
          "Welcome to information retrieval."
          todl-top-level-node-phrase
          "Top-level node"
          todl-default-node-name-prompt
          "Node name: "
          todl-new-node-name-prompt
          "New top-level node: "
          todl-first-visited-node-message
          "You are at the first visited node."
          todl-most-recently-visited-node-message
          "You are at the most recently visited node."
          todl-open-node-howto-message
          "Issue this command with cursor positioned on a node."
          todl-processing-prefix-message
          "Processing"
          todl-mode-done-processing-message
          "Done."
          todl-node-created-by-string
          "node created by"
          todl-node-created-at-string
          "node created at"
          todl-language-not-supported-message
          "Sorry, that language is not supported!"
          todl-no-more-links-message
          "No more links!"
          todl-language-list
          (list "English"))
    (todl-set-top-level-node-vars))
   (t
    (message todl-language-not-supported-message))))

(defun todl-set-default-locale-language ()
  "Run at startup to set the default language.
This default, `todl-locale-language', can be set in
your initialization file."
  (todl-set-locale-language todl-locale-language))

;;; Main functions:

(defun todl-toggle-narrowing ()
  "View one node at a time or all nodes at once."
  (interactive)
  (setq todl-keep-narrowed (not todl-keep-narrowed))
  (if todl-keep-narrowed
      (todl-narrow-to-current-list)
    (widen)))

;; later may have some special functionality when these things are
;; called *interactively*.  Right now, all the special stuff happen
;; when they are called *non-interactively*.

(defun todl-insert-node (&optional node-name mark)
  "Add a node that neighbors the current node.
If called from a program, optionally add the NODE-NAME;
if MARK is non-nil, add a * in the left margin"
  (interactive)
  (end-of-line)
  (insert (concat (if mark
                      "\n*   "
                    "\n    ")  node-name)))

(defun todl-insert-edge (&optional edge-name)
  "Add an edge going out from the current node.
If called from a program, optionally add the EDGE-NAME."
  (interactive)
  (end-of-line)
  (insert (concat "\n  " edge-name)))

;; nodes and edges are assumed to have labels that begin with
;; something other than space this is just me being strict for some
;; unknown reason

;; I'm adding the possibility of starting a line with a `*'.  This
;; is supposed to be used by TODL to indicate which nodes are
;; actually linked to.

(defun todl-on-node-p ()
  "Test to use to decide whether we are sitting on a node."
  (save-excursion (beginning-of-line)
                  (looking-at "[* ]   [^ ]")))

;; For now `*' is used here as well - maybe `#' should be used
;; instead to set things off visually. Note that you currently
;; can't link from an edge, this should be changed later.
(defun todl-on-edge-p ()
  "Test to use to decide whether we are sitting on an edge."
  (save-excursion (beginning-of-line)
                  (looking-at "[* ] [^ ]")))


;; Note that once you delete a node it will be dropped from
;; history; even if you undo the editing actions, it will not
;; be restored to the history list
(defun todl-delete-current-node ()
  "Delete the node we are currently visiting."
  (interactive)
  (let ((name (todl-grab-current-list-name))
        (beg (save-excursion (todl-beginning-of-node)
                             (point)))
        (end (save-excursion (todl-end-of-node)
                             (point))))
    (delete-region beg end)
    (delete-blank-lines)
    ;; this gives a warning, but I'm not sure what the problem
    ;; with it is.
    (setq todl-history (remove-if
                        (lambda (item) (equal (car item)
                                              name))
                        todl-history))
    (run-hooks 'todl-node-deleted-hook 'todl-change-history-hook)))

(defun todl-create-top-level-node (new-node-name &optional recreate)
  "Create a node called NEW-NODE-NAME.
If called interactively and node already exists, offer to go to
the node. If RECREATE is non-nil, delete the current version of
the node and make a new version."
  (interactive (list (read-string todl-new-node-name-prompt)))
  (if (or (not (node-exists-p new-node-name))
          (and recreate
               (goto-char (todl-find-node new-node-name))))
      (progn
        (if recreate
            (todl-delete-current-node))
        ;; this all looks a bit excessively complex to me
        (goto-char (point-max))
        (delete-blank-lines)
        (newline 1)
        (delete-horizontal-space)
        (insert (concat todl-top-level-node-string
                        new-node-name))
        (if todl-provenance-track-origination
            (todl-mark-new))
        (delete-blank-lines)
        (newline)
        (backward-char 1)
        (todl-maybe-revise-history new-node-name 0)
        (if todl-keep-narrowed
            (todl-narrow-to-current-list))
        ;; this hook gives the user a chance to add arbitrary new
        ;; provenance information programmatically with ease.  TODO:
        ;; perhaps we should use this to add our built-in provenance
        ;; stuff to illustrate the method, or, alternatively, come up
        ;; with some new "library" provenance stuff and hook it in
        ;; here.
        (run-hooks 'todl-node-created-hook 'todl-change-history-hook))
    (if (and (interactive-p)
             (y-or-n-p (format todl-node-exists-string
                               new-node-name)))
        (todl-goto-node new-node-name))))

(defun todl-end-of-node ()
  "Move point to the end of the current node."
  (interactive)
  (or (eobp)
      (if (search-forward-regexp "^\\s-*$" nil t)
                 (beginning-of-line)
               (goto-char (point-max)))))

(defun todl-beginning-of-node ()
  "Move point to the beginning of the current node."
  (interactive)
  (or (bobp)
      (progn (end-of-line)
             (search-backward-regexp
              todl-beginning-of-node-regexp nil t)
             (beginning-of-line))))

(defun todl-next-link ()
  "Moves cursor to the next link in the current node."
  (interactive)
  (save-restriction
    (todl-narrow-to-current-list)
    (and (save-excursion (end-of-line)
                         (search-forward-regexp
                          "\\(^*   \\)" nil t))
         (goto-char (match-beginning 1)))))

(defun todl-previous-link ()
  "Move cursor to previous link in the current node if there is one."
  (interactive)
  (save-restriction
    (todl-narrow-to-current-list)
    (and (save-excursion (beginning-of-line)
                         (search-backward-regexp
                          "\\(^*   \\)" nil t))
         (goto-char (match-beginning 1)))))

(defun todl-enter-next-node ()
  "Essentially `todl-next-link' followed by `todl-open-node'."
  (interactive)
  (if (save-excursion (end-of-line)
                      (todl-next-link))
      (todl-open-node)
    (message todl-no-more-links-message)))

(defun todl-enter-previous-node ()
  "Essentially `todl-previous-link' followed by `todl-open-node'."
  (interactive)
  (if (save-excursion (beginning-of-line)
                      (todl-previous-link))
      (todl-open-node)
    (message todl-no-more-links-message)))

;; currently only called if we know we are looking at a node.
(defun todl-grab-current-node-name ()
  "Get the buffer substring that labels the node we are looking at.
To grab the name of the node we are sitting on, use the function
`todl-grab-current-list-name'."
  (save-excursion
    (beginning-of-line)
    ;; this means that the function *could* also be used to pick up
    ;; edge names.  Also, it will treat indented code samples as if
    ;; their name begins with first non-space character.
    (skip-chars-forward "* ")
    (regexp-quote (buffer-substring (point) (line-end-position)))))

;; this gets the name of the top-level node we are a part of.
;; the name of this function may not be the best.
(defun todl-grab-current-list-name ()
  "Get the buffer substring that labels the list we are in."
  (save-excursion
    (end-of-line)
    (if (search-backward-regexp (concat
                                 todl-beginning-of-node-regexp
                                 "\\(.*\\)")
                                nil t)
      ;; `regexp-quote' is a way to get plain text.
      ;; the fact that we are regexp-quoting here may mean that it does
      ;; not have be be used elsewhere as much
        (regexp-quote (match-string 1)))))

;; note: it is possible that one might want to change the name of
;; a list without updating the links, but that seems to be a more
;; special need.

;; TODO: this should update the history.  Compare
;; `todl-delete-current-node'.  Also, make sure that it actually works
;; when `todl-keep-narrowed' is t.
(defun todl-change-current-list-name (new-name)
  "Change name of the current list to NEW-NAME.
Links that point to the current list are updated as well."
  (interactive (list (todl-read-node-name todl-new-name-prompt)))
  (save-excursion
    (end-of-line)
    (if (search-backward-regexp (concat
                                 todl-beginning-of-node-regexp
                                 "\\(.*\\)")
                                nil t)
        (let ((old-name (regexp-quote (match-string 1))))
          (replace-match new-name t t nil 1)
          (todl-do-forall-lists
           (lambda ()
             (goto-char (point-min))
             (while (re-search-forward (concat
                                        "^[* ]   "
                                        old-name) nil t)
               (replace-match (concat "*   " new-name)))))
          ;; we can't just do this; we have to check to see
          ;; whether the current node's name corresponds to
          ;; anything in the history, then change that element's
          ;; name component.
          ;;;(setcar (car todl-history) new-name)
          (todl-change-history new-name old-name)
          (run-hooks 'todl-change-history-hook)))))

(defun todl-change-history (new-name old-name)
  "Change occurances of OLD-NAME in `todl-history' to NEW-NAME."
  (mapcar (lambda (item)
            (if (equal (car item) old-name)
                (setcar item new-name)))
          todl-history))

(defun todl-find-node (node-name)
  "Return the position of a node given NODE-NAME.
If there is no developed node with this name, return nil."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (search-forward-regexp
             (concat todl-beginning-of-node-regexp
                     "\\(" (regexp-quote node-name) "\\)"
                     ;; nodes are not allowed to have trailing
                     ;; spaces as part of their name
                     "\\s-*$") nil t)
        (match-beginning 1)))))

;; if we returned the point, this would be essentially the same as
;; `todl-find-node'.  I'm not sure whether there is much of a case for
;; using two functions when they are so similar.
(defun node-exists-p (node-name)
  "Determine whether a node with name NODE-NAME exists."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (search-forward-regexp
           (concat todl-beginning-of-node-regexp
                   (regexp-quote node-name)
                   ;; here it is useful to match (and only match)
                   ;; trailing whitespace, since this rules out just
                   ;; matching node who's name contains our goal
                   ;; node's name as a prefix
                   "\\s-*$")
           nil t)
          t))))

(defun todl-read-node-name (&optional prompt)
  "Read a node name from the minibuffer with completion.
If user sees `todl-default-node-name-prompt' or,
if present, PROMPT."
  (completing-read
   (or prompt todl-default-node-name-prompt)
   (todl-grab-list-of-list-names)))

;; General comments on the following function

;; (1) This is pretty good, but it assumes that we are working with
;;     one buffer.  We should change the functions `todl-find-node'
;;     and `todl-goto-node' to work across several buffers.
;; (2) Probably `todl-open-node' should mark the node, but if this
;;     proves to be a problem the behavior can be changed.

(defun todl-open-node ()
  "Open the definition of the node at point.
The node is created if it does not yet exist."
  (interactive)
  ;; make sure we are on a node
  (if (todl-on-node-p)
      ;; grab the current nodename and see if this node has been
      ;; developed
      (let* ((node-name (todl-grab-current-node-name))
             (node-location (todl-find-node node-name)))
        (todl-revise-history (todl-grab-current-list-name)
                             (todl-cursor-offset))
        (todl-adjust-leading-char " " "*")
        (todl-visit-or-create node-name node-location)
        (if todl-keep-narrowed
            (todl-narrow-to-current-list))
        (run-hooks 'todl-switch-nodes-hook 'todl-change-history-hook))
    (message todl-open-node-howto-message)))

;; this is called from `todl-scroll-history-backwards' with
;; (todl-get-node (car record) nil (cadr record) t)
(defun todl-get-node (node-name &optional offset position no-update)
  "Move to new node NODE-NAME.  If POSITION is known, go there.
Otherwise find the position by search.  Move cursor forward by
OFFSET if non-nil.  Update history unless NO-UPDATE is non-nil."
  (widen)
  ;; in cases when we actually know the position of the node, it is
  ;; faster to pass that info to this function.
  (if position
      (goto-char position)
    (goto-char (todl-find-node node-name)))
  (let ((cursor-pos offset))
    (if cursor-pos
        ;; maybe we should be tracking the offset from something else
        ;; so we don't have to go to the beginning of the node here
        (progn (todl-beginning-of-node)
               (forward-char cursor-pos))
      (setq cursor-pos 0))
    (unless no-update (todl-maybe-revise-history node-name
                                                 cursor-pos))))

(defun todl-visit-or-create (node-name node-location)
  "Retreive or create a node with name NODE-NAME.
This function will create a new node iff NODE-LOCATION is nil."
  ;; if node exists, go to it
  (if node-location
      ;; TODO: we could check to see whether node-name is on the
      ;; history list, and if it is, send the corresponding cursor
      ;; offset
      (todl-get-node node-name)
    ;; if it does not exist, create it
    (todl-create-top-level-node node-name)))

(defun todl-goto-node (node-name)
  "Interactively select a node by its name, NODE-NAME.
If the node does not exist, it will be created.
The actual function that is called to do this work is
`todl-visit-or-create'."
  (interactive (list (todl-read-node-name
                      todl-default-node-name-prompt)))
  (let ((node-location (todl-find-node node-name)))
    (todl-visit-or-create node-name node-location)))

;; TODO: (1) fix up behavior around beginning of buffer
;;       (2) should this function widen first?
(defun todl-narrow-to-current-list ()
  "Find and narrow to the list containing point."
  (save-excursion
    (end-of-line)
    (if (search-backward-regexp todl-beginning-of-node-regexp nil t)
        (let ((beg (match-beginning 0))
              (end (or
                    ;; allow blank lines between developed nodes to
                    ;; have some whitespace in them
                    (search-forward-regexp "^\\s-*$" nil t)
                    ;; it is quite useful to have this other condition
                    ;; - it means that there needn't be a trailing
                    ;; space at the end of the buffer.
                    (point-max))))
          (narrow-to-region beg end)))))

;; A function for generating the *hierarchy* of nodes might be based
;; on this.

;; BUG: this seems to be broken when narrowing is in effect
(defun todl-generate-index ()
  "Create an index of all nodes in the current buffer."
  (interactive)
  (widen)
  (todl-create-top-level-node "index" t)
  ;; this str should not be const
  (todl-insert-edge "list")
  (mapcar (lambda (name)
            (todl-insert-node name t))
          ;; sort entries before inserting.
          (sort (todl-grab-list-of-list-names) 'string<)))

(defun todl-grab-list-of-list-names ()
  (let (list-of-list-names)
      (todl-do-forall-lists
       (lambda ()
         (setq list-of-list-names
               (cons (todl-grab-current-list-name)
                     list-of-list-names))))
      list-of-list-names))

;;; History support:

;; This is loosely modeled on history support in Info.
;;
;; The function `todl-scroll-history-backwards' lets us move back
;; in time. Functions for moving to new nodes should maintain the
;; historical record appropriately by checking to see whether we
;; are coming back along the same historical development when we
;; move next time.  If progression is like this:
;;
;;     (A) B C -- A (B) C -- A B (C) -- A (B) C -- A B (C)
;;
;; simply set the history offset to zero. However if development is
;; like this:
;;
;;     (A) B C -- A (B) C -- A B (C) -- A (B) C -- A B C (D)
;;
;; then history should become A B D, i.e. we forget we ever visited
;; C.  The function `todl-scroll-history-forwards' just moves us
;; forward in time.

;; Functions outside of this section that have an effect on the
;; history are: `todl-get-node' and `todl-create-top-level-node'
;; (which call the function `todl-maybe-revise-history' every time
;; they are used) and `todl-open-node' (which calls the function
;; `todl-revise-history' the first time a node is opened, so we can
;; get the starting node into the history list).

;; TODO: (1) extend this to work on multiple files (like info)
;;       (2) write an interface that will show the history someplace
;;           useful (e.g. another buffer, the ratpoison bar, etc.).
;;           More generally, we should be able to display a "stack"
;;           of tasks that the user is supposed to be working on,
;;           or more complicated hierarchies.
;;       (3) an "advanced" feature would be to change the way
;;           history is handled to be aware of different branches -
;;           so you could go "forward" in several directions
;;           (compare `pop-to-buffer')
;;       (4) the way we deal with history should be modified to deal
;;           with "drift" that happens when the user moves around
;;           the buffer using the normal editing functions

;; If we are only going to be working with single buffer KB's, this
;; should probably be a local variable. But since in fact I'm
;; planning to extend the mode to work with multi-buffer KB's
;; pretty soon, I'm not going to worry about that.  However,
;; the idea that multi-buffer capabilities may be added is
;; what keeps me from making the elements of this list cons
;; cells.
(defvar todl-history nil
  "List of todl nodes user has visited.
Each element of is a list (NODENAME POS-REL-BEG).
Here NODENAME is the name of the node, and POS-REL-BEG
is the position of the cursor relative to the node's
beginning.")

(defvar todl-history-offset 0
  "Units of time back in history.")

(defun todl-history-record-current-node ()
  "Add the current node name and cursor offset to history.
See `todl-history' for details."
  (setq todl-history
        (cons (list (todl-grab-current-node-name)
                    (todl-cursor-offset))
              todl-history)))

(defun todl-cursor-offset ()
  "Calculate the offset of the cursor from the node's beginning."
  (- (point)
     (save-excursion (todl-beginning-of-node)
                     (point))))

(defun todl-scroll-history-backwards ()
  "Move one node backwards in time (if we aren't at the first node)."
  (interactive)
  (if (not (eq todl-history-offset (1- (length todl-history))))
      (progn
        (setq todl-history-offset (1+ todl-history-offset))
        (let ((record (nth todl-history-offset todl-history)))
          (todl-get-node (car record) (cadr record)  nil t))
        (if todl-keep-narrowed
            (todl-narrow-to-current-list))
        (run-hooks 'todl-switch-nodes-hook 'todl-change-history-hook))
    (message todl-first-visited-node-message)))

(defun todl-scroll-history-forwards ()
  "Move one node forwards in time (if we aren't at the last node)."
  (interactive)
  (if (> todl-history-offset 0)
      (progn
        (setq todl-history-offset (1- todl-history-offset))
        ;; check this (compare above)
        (let ((record (nth todl-history-offset todl-history)))
          (todl-get-node (car record) (cadr record) nil  t))
        (if todl-keep-narrowed
            (todl-narrow-to-current-list))
        (run-hooks 'todl-switch-nodes-hook 'todl-change-history-hook))
    (message todl-most-recently-visited-node-message)))

;; called by todl-open-node as
;; (todl-revise-history (todl-grab-current-list-name)
;;                      (todl-cursor-offset))
(defun todl-revise-history (node-name pos-rel-beg)
  "If NODE-NAME is the current node, make cursor offset POS-REL-BEG.
Otherwise add NODE-NAME and POS-REL-BEG to history iff they
do not duplicate the most recent entry."
  (if (equal node-name (caar todl-history))
      (setcar (cdar todl-history) pos-rel-beg)
    (unless (equal node-name (caadr todl-history))
      (setq todl-history
            (cons (list node-name pos-rel-beg)
                  (nthcdr todl-history-offset todl-history)))))
  (setq todl-history-offset 0))

(defun todl-maybe-revise-history (node-name pos-rel-beg)
  "Possibly add NODE-NAME and cursor's POS-REL-BEG to the history list.
Only do this if we have gone off on a new path.  Otherwise,
just adjust the history offset."
  (if (and (> todl-history-offset 0)
           (eq node-name (car (nth todl-history-offset
                                   todl-history))))
      ;; just a little bit of history repeating itself
      (setq todl-history-offset (1- todl-history-offset))
    ;; we have for the first time in all history created a garden
    ;; of pure ideology
    (todl-revise-history node-name pos-rel-beg)))

(defun todl-clear-history ()
  "Delete the record of where we've been."
  (interactive)
  (setq todl-history nil)
  (run-hooks 'todl-change-history-hook))

(defun todl-history-to-hist-buffer ()
  "Show a log of our movements through history."
  (interactive)
  (if todl-show-history-buffer
  (save-restriction
    (save-excursion
      (let ((buf (current-buffer))
            (cn (todl-grab-current-node-name)))
        ;; This buffer should be made to be small and out of the way.
        ;; user should control the number of lines of history that are
        ;; shown. So, ir, there should be a call to a more complicated
        ;; setting-up function here.  In particular, would be nice to
        ;; make this work better w/ separate frames.
        (pop-to-buffer (get-buffer-create "*TODL History*") t)
        ;; this is preliminary - there is no real support for
        ;; multi-buffer todl-mode at present. Currently this
        ;; is just used to get font-lock in this buffer.
        (todl-mode)
        (save-excursion
          (delete-region (point-min) (point-max)))
        (insert
         (replace-regexp-in-string " [0-9]+$"              ""
         (replace-regexp-in-string "^"                     "    "
         (replace-regexp-in-string "\\(^((\\)"             ""
         (replace-regexp-in-string "\\())$\\)\\|\\() (\\)" "\n"
                 (format "%s" todl-history))))))
        (goto-char (point-min))
        ;; is there a way to keep the ghost cursor from being
        ;; displayed?
        (insert
         "We are planning to make this buffer more servicable soon.")
        (newline 2)
        (if todl-history
            (save-excursion (next-line todl-history-offset)
                            (beginning-of-line)
                            (todl-adjust-leading-char " " "*")
                            ;; it would be nice to align the numbers
                            ;; and maybe show more data, e.g. name of
                            ;; buffer (when that matters)
                            ))
        (goto-char (point-max))
        (beginning-of-line)
        (pop-to-buffer buf))))))

(defun todl-activate-hist-buffer ()
  "If ARG is non-nil, show hist-buffer.
Otherwise get rid of it."
  (if todl-show-history-buffer
      (progn (add-hook 'todl-clear-history-hook
                       'todl-history-to-hist-buffer)
             (add-hook 'todl-switch-nodes-hook
                       'todl-history-to-hist-buffer)
             (add-hook 'todl-node-created-hook
                       'todl-history-to-hist-buffer)
             (add-hook 'todl-node-deleted-hook
                       'todl-history-to-hist-buffer)
             (get-buffer-create "*TODL History*")
             (todl-history-to-hist-buffer))
    (progn (remove-hook 'todl-clear-history-hook
                     'todl-history-to-hist-buffer)
           (remove-hook 'todl-switch-nodes-hook
                     'todl-history-to-hist-buffer)
           (remove-hook 'todl-node-created-hook
                     'todl-history-to-hist-buffer)
           (remove-hook 'todl-node-deleted-hook
                     'todl-history-to-hist-buffer)
           (delete-windows-on "*TODL History*")
           (kill-buffer "*TODL History*"))))

(defun todl-toggle-hist-buffer ()
  (interactive)
  (setq todl-show-history-buffer (not todl-show-history-buffer))
  (todl-activate-hist-buffer))

;;; Provenance:

;; The idea of `provenance' is a very important in "advanced" TODL.
;; We want to keep track of lots of different things. Time stamps
;; and usernames are just two of these things.

;; For example, at some point we may wish to begin keeping track of
;; back-and-forth links.  I.e., if we add a link from A to B via an
;; edge E, we would also add a link from B to A labeled `via E'. This
;; backlink would have its own provenance.  Because this sort of
;; information would tend to proliferate rapidly, many of these things
;; should be hidden from the user in day-to-day operation.

;; Here are a few thoughts about implementing `backlinking'.  Any time
;; we actually follow a link, we know that there is something at the
;; beginning and something at the end.  We could record a backlink at
;; that time.  Also, when we remove a node, we could remove all the
;; hard links to this node.  (This doesn't really require
;; backlinking.)

;; The question is: what is the gain. If we can just implement
;; backlinking (and more interesting things) using an after-the-fact
;; interpreter, we probably don't need to worry about hard coding this
;; stuff into the buffer.

(defun todl-timestamp ()
  "Timestamp to use when dating edits.
Uses the format specified by `todl-time-string-format'."
  (let ((date (format-time-string todl-time-string-format
                                  nil todl-use-universal-time)))
  (if todl-use-universal-time
      ;; being sanctimonious... I guess this is the right thing to do
      (replace-regexp-in-string
       "GMT" "UTC"
       date)
    date)))

;; (1) if a "homepage" for this signature exists, the sig will appear
;;     as a link

;; (2) it is unlikely that a separate page for this timestamp occurs,
;;     so we don't even bother looking.  But it might be worthwhile if
;;     we were dealing with *tasks* to add this task to some other
;;     list of task.  But on the other hand, maybe this would be best
;;     done by hand, with backlinking.

;; General question: I'm not sure what the costs of a search for a
;; single string are.  Not so bad, I guess....
(defun todl-mark-new ()
  "Called to add provenance at creation of a node."
  (if todl-sign-edits
      (progn (todl-insert-edge)
             (insert todl-node-created-by-string)
             ;; See (1) above
             (if (node-exists-p todl-signature)
                 (todl-insert-node todl-signature t)
               (todl-insert-node todl-signature))))
  (if todl-date-edits
      (progn (todl-insert-edge)
             (insert todl-node-created-at-string)
             (todl-insert-node)
             ;; See (2) above
             (insert (todl-timestamp)))))

;; probably should just insert

;; [modified
;;    [username]
;;    [timestamp]]

;; we can have multiple `modified' edges emanating from the current
;; node...  or the syntax could be

;; [modified
;;    [username]
;;    [timestamp]
;;     ........
;;    [username]
;;    [timestamp]]

;; but this would be confusing if some users didn't want to sign their
;; entries.

;; For just one user, it would be OK, and would probably look like

;; [modified
;;    [timestamp]
;;     .........
;;    [timestamp]]

;; However, we could try to keep all of the modified edges for a given
;; node next to each other in the buffer.

;; we would also want to make a "destructive" version,

;; [last modified
;;    [username]
;;    [timestamp]]

;; this could be used together with or instead of the above.

;; (defun todl-mark-modified ()
;;   "Add provenance information upon modification of a node."
;;   (save-excursion
;;     (save-restriction
;;       (todl-narrow-to-current-list)
;;       (goto-char (point-min))
;;       (if todl-sign-edits
;;           (progn (search-forward-regexp "^[* ] node created by")
;;                  (next-line)
;;                  (let ((originator (todl-grab-current-node-name)))
;;                    (if (not (eq originator
;;                                 todl-signature))
;;                        (search-forward-regexp "^[* ] node modified"))
;;                   (if todl-date-edits
;;                       (progn (todl-insert-edge)
;;                              (insert "node created at")
;;                              (todl-insert-node)
;;                              (insert todl-timestamp)))))))))

;; we may also want to add

;; [last viewed
;;    [username]
;;    [timestamp]]

;; For actually using this to maintain a todo list, it would be
;; helpful to have some other key words...

;; [must be completed by
;;    [time]]

;; for example.

;; probably the user should be able to specify their own list of
;; strings and bindings for them, and have the insertion function
;; be generated automatically

;; Semantics for this could include increasingly agressive reminders.
;; I'm not sure that these sorts of things have ever worked for me,
;; but then again, maybe they just weren't agressive enough.

;; probably the best thing to do would be to make a list of edges that
;; must be added; maybe the nodes (fields) they point to would be
;; entered interactively through the minibuffer.

;;; Markup functions:

(defun todl-markup-node ()
  "Mark up a node according to whether or not it has been developed."
  (if (todl-on-node-p)
    ;; grab the current nodename and see if this node has been
    ;; developed note that the next three lines or so could be
    ;; refactored into a function which would return the location or
    ;; nil if the node has not been developed
    (let* ((node-name (todl-grab-current-node-name))
           (new-point (todl-find-node node-name)))
      (save-excursion
        (goto-char (line-beginning-position))
        (if (looking-at "[* ]")
            ;; this used to call `todl-adjust-leading-char', but I'm
            ;; not sure the latter is actually at all useful.
            (replace-match (if new-point "*" " ")))))))


(defun todl-adjust-leading-char (reg rep)
  "Change the first character in the line.
Expects REG and REP to be strings of length one."
  (save-excursion (beginning-of-line)
                  (if (looking-at reg)
                      (progn
                        (delete-char 1)
                        (insert rep)))))

(defun todl-markup-buffer ()
  "Mark up all the nodes in a buffer.
See documentation for `todl-markup-node'."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (not (eobp))
        (todl-markup-node)
        (forward-line)))))

;; BUG: this function frequently had to be called twice for it to take
;; effect.  But maybe has been fixed by swapping `save-excursion' and
;; `save-restriction'?
(defun todl-markup-list ()
  "Mark up all the nodes in a list.
See documentation for `todl-markup-node'."
  (interactive)
  (save-excursion
    (save-restriction
      (todl-narrow-to-current-list)
      (goto-char (point-min))
      (while (not (eobp))
        (todl-markup-node)
        (forward-line)))))

(defun todl-unmarkup-buffer ()
  "Unmark all the nodes in a buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^\\*" nil t)
        (replace-match " " t t)))))

;;; Exporting:

(defun todl-on-link-p ()
  "This is the test to use to decide whether we are sitting on a link."
  (save-excursion (beginning-of-line)
                  ;; for speed: is there a way to do this test without
                  ;; modifying match data?
                  (looking-at "*   ")))

;; Use of this function: compatibility with XEmacs.
;; Q. Why isn't something like this added to the core of GNU Emacs,
;; rather than having to reproduce it all the time in packages?
(defun todl-replace-regexp-in-string (regexp replacement text
                                             &optional substitute)
  "Replace REGEXP with REPLACEMENT in TEXT.
If SUBSTITUTE is non-nil, use `replace-match'-style substitutions."
  (cond
   ((fboundp 'replace-in-string)
    (let ((case-fold-search nil))
      (replace-in-string text regexp replacement (not substitute))))
   ((fboundp 'replace-regexp-in-string)
    (let ((case-fold-search nil))
      (replace-regexp-in-string
       regexp replacement text t (not substitute))))
   (t (while (string-match regexp text)
        (setq text (replace-match replacement t (not substitute) text)))
      text)))

(defun todl-to-html ()
  "Create <file>--<node>.html for each developed node."
  (interactive)
  (let ((todlname (file-name-sans-extension (buffer-name))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward todl-beginning-of-node-regexp nil t)
          (save-excursion
            (save-restriction
              (todl-narrow-to-current-list)
              (let* ((nodetext (buffer-substring (point-min)
                                                 (point-max)))
                     (nodename (todl-grab-current-list-name)))
                (message (concat todl-processing-prefix-message
                                 " "
                                 nodename
                                 "."))
                (with-temp-file (expand-file-name
                                 (concat
                                  todlname "--"
                                  (todl-markup-text-for-url nodename)
                                  ".html")
                                 todl-pub-directory)
                  (insert nodetext)
                  (goto-char (point-min))
                  (todl-html-markup-engine todlname)
                  (goto-char (point-min))
                   (todl-replace-regexp-in-string
                    (concat todl-beginning-of-node-regexp
                            ".*\n")
                    (concat "<!-- This page was created with TODL -->
<html>
<head>
<title>" todlname ":" nodename "</title>
</head>
<H3><b>" nodename "</b></H3>") nodetext))
                )))))))
  (message todl-mode-done-processing-message))

;; Perhaps this should be a macro? Or maybe not.
(defun todl-do-forall-lists (&rest args)
  "Run ARGS on each of the nodes separately."
  ;; Perhaps there is a minor problem with the save-excursion stuff
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (while (re-search-forward todl-beginning-of-node-regexp nil t)
        (save-excursion
          (save-restriction
            (todl-narrow-to-current-list)
            ;; it isn't really necessary to be at the *start* of the
            ;; current list
            ;;;(goto-char (point-min))
            (mapcar 'funcall args)))))))

(defun todl-html-markup-engine (todlname)
  "Add HTML markup to the body of a node.
TODLNAME is the name of the buffer this node lives in."
  (let (;; these variables keep track of what kind of thing we're
        ;; looking at and/or were just looking at
        (edgep nil)
        (nodep nil)
        ;; local binding to make the `next-line' that is coming up
        ;; in just a minute behave as desired
        (goal-column 0))
    (while (not (eobp))
      (next-line)
      ;; edge case
      (cond ((looking-at "^\\(  \\)\\([^ ].*\\)")
             ;; if we are just starting out, begin a list
             (cond ((and (not edgep)
                         (not nodep))
                    (replace-match "<ul>\n<li>\\2"))
                   ;; if we were just looking at a node, end a list
                   ;; of nodes, and add what we are looking at to
                   ;; the list of edges
                   (nodep
                    (replace-match " </ul>\n<li>\\2"))
                   ;; if we are looking at another edge, just add
                   ;; to the list
                   (edgep
                    (replace-match "<li>\\2")))
             ;; update log
             (setq edgep t)
             (setq nodep nil))
            ;; node case
            ((looking-at "^\\(    \\)\\([^ ].*\\)")
             ;; if we are just starting out, begin a list
            (cond ((and (not edgep)
                        (not nodep))
                   (replace-match " <ul>\n <li>\\2"))
                  ;; if we were just looking at a node and add what
                  ;; we are looking at to the list of nodes
                  (nodep
                   (replace-match " <li>\\2"))
                  ;; if we were looking at an edge, similarly, add
                  ;; to the list of nodes
                  (edgep
                   (replace-match " <ul>\n <li>\\2")))
            ;; update log
            (setq edgep nil)
            (setq nodep t))
            ;; this is like the case just above, but with URL text
            ;; to deal with
            ((looking-at "^\\(*   \\)\\([^ ].*\\)")
            (let* ((str (buffer-substring (match-beginning 2)
                                          (match-end 2)))
                   (url (concat "<a href=\"./"
                                todlname
                                "--"
                                (todl-markup-text-for-url str)
                                ".html\">")))
              (cond ((and (not edgep)
                          (not nodep))
                     (replace-match
                      (concat " <ul>\n <li>" url "\\2</a><br>")))
                    (nodep
                     (replace-match
                      (concat " <li>" url "\\2</a><br>")))
                    (edgep
                     (replace-match
                      (concat " <ul>\n <li>" url "\\2</a><br>")))))
            (setq edgep nil)
            (setq nodep t))
            ;; anything indented further than normal is treated
            ;; verbatim
            ((looking-at "^\\(     \\)\\(.*\\)")
             (replace-match "<pre>\n\\&")
             (let ((end (search-forward-regexp
                         "\\(^    [^ ]\\)\\|\\(^  [^ ]\\)" nil t)))
               (if end
                   (goto-char (1- end))
                 (setq end (search-forward-regexp "\\(^$\\)" nil t))
                 (goto-char end)))
             (beginning-of-line)
             (insert "</pre>")
             (newline))
            ((looking-at "^$")
             (insert " </ul>\n</ul>"))))))

;; This should eventually contain all the needed rewritings; I think
;; it does now -- but I'm not totally sure.
(defun todl-markup-text-for-url (str)
  "Hexify dangerous characters in STR."
  (save-match-data
    (apply (function concat)
           (mapcar (lambda (ch)
                     (cond
                      ;; newline
                      ((eq ch ?\n)
                       "%0D%0A")
                      ;; xxx?
                      ((string-match
                        "[-a-zA-Z0-9_:;'/.<>]"
                        (char-to-string ch))
                       (char-to-string ch))
                      ;; space
                      ((char-equal ch ?\x20)
                       "+")
                      ;; otherwise - escape
                      (t
                       (format "%%%02x" ch))))
                   ;; arguments: our string, as a list of characters
                   (string-to-list str)))))

;; here, for the record, is an alist of the trouble characters strings
;; and their official/popular replacements.

;;  '(("%" . "%25") ("[$]" . "%24") ("&" . "%26")
;;    ("+" . "%2B") ("," . "%2C")
;;    ("/" . "%2F") (":" . "%3A")
;;    (";" . "%3B") ("=" . "%3D")
;;    ("?" . "%3F") ("@" . "%40")
;;    (" " . "_")   ("\"" . "%22")
;;    ("<" . "%3C") (">" . "%3E") ("#" . "%23")
;;    ("{" . "%7B") ("}" . "%7D") ("|" . "%7C")
;;    ("\\\\" . "%5C") ("\\^" . "%5E")
;;    ("~" . "%7E") ("\\[" . "%5B") ("\\]" . "%5D")
;;    ("`" . "%60")
;;    ;; not sure if this last set is needed or not
;;   ("'" . "%27") ("!" . "%27") ("*" . "%27")))

;;; Contrib:

;; stuff that doesn't really belong in the main body of todl-mode.el

;; if you have any neat additions to todl-mode that you think should
;; go here please let us know

(defun todl-history-to-rp-bar ()
  "Give a summary of movements through history in the Ratpoison bar.
Needless to say this only works if you are using the Ratpoison
window manager."
  (interactive)
  (let ((hist-string
         (replace-regexp-in-string " [0-9]+$"              ""
         (replace-regexp-in-string "^"                     " "
         (replace-regexp-in-string "\\())$\\)\\|\\(^((\\)" ""
         (replace-regexp-in-string "\\() (\\)" "\n"
                 (format "%s" todl-history)))))))
    (with-temp-buffer
      (insert hist-string)
      (goto-char (point-min))
      (if todl-history
          (save-excursion
            (next-line todl-history-offset)
            (beginning-of-line)
            (todl-adjust-leading-char " " "*")
            ;; it would be nice to align the numbers
            ;; and maybe show more data, e.g. name of
            ;; buffer (when that matters)
            (ratpoison-echo
             (buffer-substring (point-min) (point-max))))))))

;;; Conclusion:

(defun todl-mode ()
  "TODL helps you organize knowledge/information into lists.
These lists can refer to one another.  This facilitates the
creation of simple hypertext documents.  The terminology we use
to describe the structure of these documents comes from graph
theory: each of the lists is a \"node\", and these nodes are
connected by \"edges\".  Concretely, what this means is that your
`todl-mode' documents consist of lots of little sections that
look like this:

Top-level node: node name
  edge label
    another node
    some other node
  some other edge label
    a third node

These sections can be navigated using `todl-open-node',
`todl-scroll-history-backwards', `todl-scroll-history-forwards',
or the familiar `text-mode' movement commands.

Commands:
\\{todl-mode-map}
Entry to this mode calls the value of `todl-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table todl-mode-syntax-table)
  (use-local-map todl-mode-map)
  (setq major-mode 'todl-mode)
  ;; this makes sure we are inserting spaces (not tabs) when
  ;; pressing TAB (important for regexp matching used in this
  ;; mode).
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'font-lock-defaults)
       '(todl-font-lock-keywords))
  (setq major-mode 'todl-mode)
  (setq mode-name "Todl")
  ;; run hook before beginning
  (run-hooks 'todl-mode-hook))

;; I couldn't get this to work.

; (define-derived-mode todl-mode text-mode "Todl"
;   "Major mode for editing TODL code.
;
; TODL is a knowledge-management system that helps you organize
; your information into lists.
;
; Commands:
; \\{todl-mode-map}
; "
;   (set (make-local-variable 'font-lock-defaults)
;       '(todl-font-lock-keywords))
;   ;; this makes sure we are inserting spaces (not tabs) when
;   ;; pressing TAB
;   (set (make-local-variable 'indent-tabs-mode) nil))

(provide 'todl-mode)

;;; Notes:

;; About the quotes used in this code.

;; "Welcome to information retrieval."
;;    said to Sam Lowry when he gets his promotion in "Brazil" (1985)

;; "just a little bit of history repeating itself"
;;    traditional, but now part of a hit single by the Propellerheads
;;    (1998)

;; "we have for the first time in all history created a garden of pure
;; ideology"
;;    from the commercial for the original Macintosh computer (1984)

;;; todl-mode.el ends here
