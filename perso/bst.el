;;; -*-emacs-lisp-*-
;;; bst.el --- major mode for editing BibTeX style files
;;;
;;; Copyright (C) 1999 Free Software Foundation, Inc.
;;;
;; Author: Nelson H. F. Beebe <beebe@math.utah.edu>
;; Maintainer: Nelson H. F. Beebe <beebe@math.utah.edu>
;; Created: 18 December 1999
;; Version: 1.00
;; Keywords: .bst file, BibTeX style file

;; This file is part of GNU Emacs.

;;; Commentary:

;;; ====================================================================
;;;  @Emacs-Lisp-file{
;;;     author          = "Nelson H. F. Beebe",
;;;     version         = "1.01",
;;;     date            = "28 December 1999",
;;;     time            = "08:03:18 MST",
;;;     filename        = "bst.el",
;;;     address         = "Center for Scientific Computing
;;;                        University of Utah
;;;                        Department of Mathematics, 322 INSCC
;;;                        155 S 1400 E RM 233
;;;                        Salt Lake City, UT 84112-0090
;;;                        USA",
;;;     telephone       = "+1 801 581 5254",
;;;     FAX             = "+1 801 585 1640, +1 801 581 4148",
;;;     URL             = "http://www.math.utah.edu/~beebe",
;;;     checksum        = "36771 181 665 5975",
;;;     email           = "beebe@math.utah.edu, beebe@acm.org,
;;;                        beebe@ieee.org (Internet)",
;;;     codetable       = "ISO/ASCII",
;;;     keywords        = ".bst file, BibTeX style file, Emacs Lisp",
;;;     supported       = "yes",
;;;     docstring       = "This short Emacs Lisp file defines a BibTeX
;;;                        style file editing mode for *.bst files, as
;;;                        a simple extension of fundmental mode.
;;;
;;;                        The checksum field above contains a CRC-16
;;;                        checksum as the first value, followed by the
;;;                        equivalent of the standard UNIX wc (word
;;;                        count) utility output of lines, words, and
;;;                        characters.  This is produced by Robert
;;;                        Solovay's checksum utility.",
;;;  }
;;; ====================================================================

(defconst bst-version "1.01 [28-Dec-1999]" "Version of bst (BibTeX style) library")

;;; Revision history (reverse chronological order):
;;;
;;; 1.01 [28-Dec-1999]
;;;	Add make-bst-TAGS-file function.
;;;
;;; 1.00 [18-Dec-1999]
;;;	Initial version.
;;;

(provide 'bst)

(defconst bst-font-lock-keywords (purecopy
  (list
   '("^[ \t]*\\<\\(ENTRY\\|Automaton\\)\\>[ \t]*\\([a-z]\\)"
     1 font-lock-keyword-face)
;;    '("^[ \t]*\\(TRS\\|Automaton\\)\\>[ \t]*\\([a-z][a-z0-9_]*\\)"
;;      3 font-lock-function-name-face t)
;;    (cons (concat "\\<\\(States\\|Final\\|Ops\\|Vars\\|"
;; 		 "Transitions\\|Description\\)\\>")
;; 	 'font-lock-type-face)
;;    '("\\<\\([0-9]+\\)[ \t]*:" 1 font-lock-variable-name-face)
)))
  "Additional expressions to highlight in Bst mode.")
(put 'bst-mode 'font-lock-defaults '(bst-font-lock-keywords nil t))

(defvar bst-comment-prefix "%%: "
  "*Comment prefix string for bst-comment and bst-uncomment.  This is
used in regular-expression matching by bst-uncomment, so it should
NOT contain any regular-expression pattern characters like . or *.")

;;;###autoload
(defun bst-mode ()
  "Setup for BibTeX style file editing."
  (interactive)
  (setq mode-name "BibTeX-style")
  (local-set-key [tab] 'indent-for-tab-command)
  (setq indent-tabs-mode nil)
  (auto-fill-mode nil)			; auto fill is undesirable in
					; programming language text
  (setq comment-end "")
  (setq comment-start "% ")
  (setq comment-start-skip "%+ *")

  (local-set-key "\C-cu" 'bst-uncomment-region)
  (local-set-key "\C-c%" 'bst-comment-region)

  ;; Font lock support
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(bst-font-lock-keywords nil t))

  ;; Finally, run the user mode hooks
  (run-hooks 'bst-mode-hook 'BibTeX-style-mode-hook 'bibtex-style-mode-hook))


(fset 'BibTeX-style-mode 'bst-mode)
(fset 'bibtex-style-mode 'bst-mode)


(defun bst-comment-region ()
  "Insert a distinctive comment prefix at the start of each line in
the current region."
  (interactive)
  (let ((start (region-beginning)) (end (region-end)))
    (goto-char start)
    (if (bolp)
	t
      (forward-line 1)
      (setq start (point)))
    (goto-char end)
    (if (bolp)
	t
      (beginning-of-line)
      (setq end (1+ (point))))
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (and (bolp) (< (point) (point-max)))
	(insert bst-comment-prefix)
	(forward-line 1)))))


(defun bst-uncomment-region ()
  "Remove a distinctive comment prefix at the start of each line in
the current region."
  (interactive)
  (let ((start (region-beginning)) (end (region-end)) (n (length bst-comment-prefix)))
    (goto-char start)
    (if (bolp)
	t
      (forward-line 1)
      (setq start (point)))
    (goto-char end)
    (if (bolp)
	t
      (beginning-of-line)
      (setq end (1+ (point))))
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (and (bolp) (< (point) (point-max)))
	(if (looking-at bst-comment-prefix)
	    (delete-char n))
	(forward-line 1)))))


(defun make-bst-TAGS-file ()
  "Create a TAGS file in the current directory for the BibTeX style
file in the current buffer.

Because BibTeX style files share most of their functions, it is not
useful to create a TAGS file for all *.bst files in the current
directory.

This is a stopgap function until the standalone etags utility can be
updated to recognize BibTeX style files."
  (interactive)
  (if (not (string-equal mode-name "BibTeX-style"))
      (error "This buffer is not in BibTeX-style mode"))
  (let ((line-number 1) (start (point)) (start-name) (tags-buffer))
    (save-excursion
      (find-file "./TAGS")
      (delete-region (point-min) (point-max))
      (setq tags-buffer (current-buffer)))
    (goto-char (point-min))
    (princ (format "\f\n%s,0\n" buffer-file-name) tags-buffer)
    (while (< (point) (point-max))
      (if (looking-at "^ *FUNCTION *{[^}]*}")
	  (progn
	    (save-excursion
	      (save-match-data
		(goto-char (match-beginning 0))
		(search-forward "{")
		(setq start-name (point))))
	    (princ (format "%s\177%s\001%d,%d\n"
			   (buffer-substring (match-beginning 0) (match-end 0))
			   (buffer-substring start-name (1- (match-end 0)))
			   line-number
			   (1- (match-beginning 0)))
		   tags-buffer)))
      (forward-line 1)
      (setq line-number (1+ line-number)))
    (save-excursion
      (set-buffer tags-buffer)
      (save-buffer 0)
      (visit-tags-table (buffer-file-name)))
    (goto-char start)))
