;;; paren-pair.el --- Insert and jump over closing parentheses

;; Copyright (C) 2003, 2004 Michael Schierl

;; Author: Michael Schierl <schierlm-public@gmx.de>
;; Created: 11 August 2003
;; Keywords: Java, C, HTML, Paren pair, Insertion, Skeleton
;; Version: 0.2

(defconst paren-pair-version "0.2"
  "Version of paren pair mode.")

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Designed as a minor mode for Java/C/HTML editing. Idea is taken
;; from a recent Eclipse version.

;; When coding Java or C(++), it often happens that you just forget to
;; close a parenthesis. Paren highlighting helps (if you think another
;; paren could be missing), but just showing these parentheses would
;; be even better). An approach to this is
;; `skeleton-pair-insert-maybe', but this is not DWIM enough for me.

;; When this mode is enabled, typing an open parenthesis will insert
;; an closing parenthesis as well, iff the cursor is either at the end
;; of a line or the character right of the cursor is a "closing
;; parenthesis" according to Emacs' current syntax table. In that
;; case, the closing parenthesis is marked as created by paren pairing
;; (in a text property) so that when you type a closing parenthesis
;; and the character right of the cursor is exactly this character and
;; marked as paren paired, it is jumped over and unmarked as paren
;; paired.

;; Additionally, when you press backspace and there is a paren pair
;; around cursor (and the closing paren is marked as such), both
;; parens will be removed. This is useful if you accidentally type an
;; open parenthesis as the closing parenthesis will disappear
;; automatically.

;; This does not only work for "round" parentheses, but for all
;; characters that are marked either as open or closed parentheses. By
;; default, braces are excluded - see `paren-pair-def-list' on how you
;; can customize this behaviour.

;; Additionally, all characters marked as "string quote" or "paired
;; delimiter" are handled similar to braces, except they are used as
;; both opening and closing parenthesis and are only paired if the
;; character before point is no "escape character".

;; All keys are hooked so that they still work if other minor modes
;; hook the same keys and are toggled in some "irregular" order. For
;; more information, see `paren-pair-run-old-key'.

;; to use this file, (auto)load it and add `turn-on-paren-pair-mode'
;; to the hook of your java/c/html mode.

;;; History:

;; 0.2 - changed the way keys are hooked
;;     - added autoload cookies
;;     - added code to autodetect which chars are parens
;;     - added support for mmm-mode

;; 0.1 initial version

;;; Code:

(defvar paren-pair-mode nil)

(make-variable-buffer-local 'paren-pair-mode)
(or (assq 'paren-pair-mode minor-mode-alist)
              (setq minor-mode-alist
                    (cons '(paren-pair-mode " ()")
			  minor-mode-alist)))

(defvar paren-pair-mode-map (make-sparse-keymap))
(setq minor-mode-map-alist (cons (cons 'paren-pair-mode paren-pair-mode-map)
				 minor-mode-map-alist))

(define-key paren-pair-mode-map (kbd "DEL") 'paren-pair-backspace)

(defcustom paren-pair-def-list '(127 . "[}{]")
  "*Which paren pairs to recognize.
This is either a list of lists containing start and end paren, or a
cons cell consisting of a number (the maximal syntax entry to check)
and a regexp (characters matching that regexp are ignored)."
:type '(choice
       (repeat :tag "List of Entries"
	       (list :tag "Entry"
		     (string :tag "Open paren")
		     (string :tag "Close paren")))
       (cons :tag "Generate from syntax table"
	     (character :tag "Last char")
	     (regexp :tag "Exclude regexp"))))

(defvar paren-pair-list nil)
(make-variable-buffer-local 'paren-pair-list)

;;;###autoload
(defun paren-pair-mode (arg)
  "Toggle paren pair mode.
With a prefix ARG, enable paren pair mode iff arg is nonzero."
  (interactive "P")
  (setq paren-pair-mode
        (if (null arg)
            (not paren-pair-mode)
          (> (prefix-numeric-value arg) 0)))
  (if paren-pair-mode
      (paren-pair-set-list))
  (force-mode-line-update)
  (if (interactive-p)
      (if paren-pair-mode
          (message "Paren pair mode enabled")
        (message "Paren pair mode disabled"))))

;;;###autoload
(defun paren-pair-mode-for-html ()
  "Toggle paren pair mode for HTML.
This is a legacy function which was needed when paren-pair mode did
not automatically examine syntax tables."
  (interactive)
  (turn-on-paren-pair-mode))

(defun turn-on-paren-pair-mode ()
    "Turn on paren-pair mode.
Useful for adding to a major mode hook variable.
Example:
    (add-hook 'c-mode-hook 'turn-on-paren-pair-mode)
to automatically turn on paren pair mode when opening a C file."
  (paren-pair-mode 1))

(defun paren-pair-build-list (synt count ign)
  "Build a list of paren pairs from a syntax table.
SYNT is the syntax table to use, COUNT is the index of the last
character to test, IGN is a regexp of characters to ignore."
  (let ((i 0) result ent chrstr)
    (while (< i count)
      (condition-case nil
	  (setq chrstr (string i))
	(error (setq chrstr nil)))
      (setq ent (aref synt i))
      (unless (or (null chrstr)
		  (string-match ign chrstr))
	(if (and (consp ent) (= (car ent) 4))
	    (add-to-list 'result (list (string i) (string (cdr ent)))))
	(if (and (consp ent) (or (= (car ent) 7) (= (car ent) 8)))
	    (add-to-list 'result (list (string i) (string i)))))
      (setq i (1+ i)))
    result))

(defun paren-pair-set-list ()
  "Set up a list of paren pairs from `paren-pair-def-list'."
  (setq paren-pair-list
	(if (and (consp paren-pair-def-list)
		 (consp (car paren-pair-def-list)))
	    paren-pair-def-list
	  (paren-pair-build-list (syntax-table) (car paren-pair-def-list)
				 (cdr paren-pair-def-list))))
  (mapc
   (lambda (pair)
     (define-key paren-pair-mode-map (read-kbd-macro (car pair))
       'paren-pair-insert-maybe)
     (unless (string= (car pair) (car (cdr pair)))
       (define-key paren-pair-mode-map (read-kbd-macro (car (cdr pair)))
	 'paren-pair-insert-maybe)))
    paren-pair-list))

(defun paren-pair-backspace (arg)
  "Run when user presses the backspace key.
Checks if point is between a paren pair and delete it.
With a prefix ARG, no special actions are done."
  (interactive "*P")
  (when (and (null arg) (get-text-property (point) 'paren-paired))
    (let ((open (buffer-substring-no-properties (1- (point)) (point)))
	  (close (buffer-substring-no-properties (point) (1+ (point))))
	  (matching nil))
      (mapc
       (lambda (pair)
	 (if (and (string= open (car pair))
		  (string= close (car (cdr pair))))
	     (setq matching t)))
       paren-pair-list)
      (if matching
	  (delete-region (point) (1+ (point))))))
  (let ((paren-pair-mode nil))
    (call-interactively (key-binding (kbd "DEL")))))

(defun paren-pair-insert-maybe (arg)
  "Run when user enters a parenthesis.
Checks which kind of parenthesis this is and calls `paren-pair-open',
`paren-pair-close' or `paren-pair-symmetric'.  With a prefix ARG, no
special actions are done."
(interactive "*P")
  (let (used)
    (mapc
     (lambda (pair)
       (cond
	((and (string= (string last-command-char) (car pair))
	      (string= (car pair) (car (cdr pair))))
	 (paren-pair-symmetric arg (car pair))
	 (setq used t))
	((string= (string last-command-char) (car pair))
	 (paren-pair-open arg "\\s)\\|[;\n]" (car (cdr pair)))
	 (setq used t))
	((string= (string last-command-char) (car (cdr pair)))
	 (paren-pair-close arg (car (cdr pair)))
	 (setq used t))
	(t nil)))
     paren-pair-list)
    (unless used
      (paren-pair-run-old-key))))

(defun paren-pair-open (arg after close)
  "Insert an open parenthesis.
ARG AFTER CLOSE."
  (cond
   ((or arg (paren-pair-embedded-mode))
    (paren-pair-run-old-key))
   ((or (= (point) (point-max)) (looking-at after))
    (paren-pair-run-old-key)
    (put-text-property 0 (length close) 'paren-paired t close)
      (save-excursion
	(insert close)))
   (t
    (paren-pair-run-old-key))))

(defun paren-pair-close (arg close)
  "Insert a closing parenthesis.
ARG CLOSE"
  (cond
   ((or arg (paren-pair-embedded-mode))
    (paren-pair-run-old-key))
   ((and (looking-at close) (get-text-property (point) 'paren-paired))
    (put-text-property (point) (+ (point) (length close)) 'paren-paired nil)
    (paren-pair-run-right-key))
   (t
    (paren-pair-run-old-key))))

(defun paren-pair-symmetric (arg char)
  "Insert a \"symmetric\" parenthesis.
ARG CHAR."
  (cond
   ((or arg (paren-pair-embedded-mode))
    (paren-pair-run-old-key))
   ((and (not (bobp))
	 (= (char-syntax (string-to-char
			  (buffer-substring-no-properties (1- (point))
							  (point))))
	     92)) ; escape character
    (paren-pair-run-old-key))
   ((and (looking-at char) (get-text-property (point) 'paren-paired))
    (put-text-property (point) (+ (point) (length char)) 'paren-paired nil)
    (paren-pair-run-right-key))
   (t
    (paren-pair-run-old-key)
    (put-text-property 0 (length char) 'paren-paired t char)
    (save-excursion
      (insert char)))))

(defun paren-pair-embedded-mode ()
  "Check whether an \"embedded\" mode is active.
This can happen when you use the command `mmm-mode'."
  (string-match "\\]$" mode-name))

(defun paren-pair-run-old-key ()
  "Do what the last key would have done if paren pair mode were not enabled.
This is determined by temporarily disabling paren pair mode and
running the old `key-binding' then.  The mode must remain disabled
while the key binding is run so that two modes using this \"trick\"
won't call each other infinitely."
  (let ((paren-pair-mode nil))
    (call-interactively
     (key-binding (vector last-command-char)))))

(defun paren-pair-run-right-key ()
  "Do what the \"right-arrow\" key does."
  (call-interactively
   (key-binding (kbd "<right>"))))

;;;###autoload
(when (bound-and-true-p mihi-default-keys)
  (global-set-key (kbd "C-c )") 'paren-pair-mode)
  (add-hook 'html-mode-hook 'paren-pair-mode-for-html))

(provide 'paren-pair)

;;; paren-pair.el ends here
