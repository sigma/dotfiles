;;; mycompletion.el --- code for completing

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Yann Hodique <Yann.Hodique@lifl.fr>
;; Keywords: abbrev, languages, tools

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

;; Put in this place everything related to completion

;;; Code:

;; explicit name :-)
(defun gpl-header ()
  "Includes a header in the edited file."
  (let ((name (file-name-nondirectory (buffer-file-name))))
    (with-temp-buffer
      (insert (format "%-75s\n\n" "/*  Time-stamp: <>  */"))
      (insert (format "/**\n%-75s\n" (concat " *  @file " name)))
      (insert (format "%-75s\n" (concat " *  @date " (calendar-date-string (calendar-current-date)))))
      (insert (format "%-75s\n" (concat " *  @author " user-full-name " <" user-mail-address ">\n */")))
      (insert (format "
/************************************************************************
 *                                                                      *
 * This program is free software; you can redistribute it and/or modify *
 * it under the terms of the GNU General Public License as published by *
 * the Free Software Foundation; either version 2 of the License, or    *
 * (at your option) any later version.                                  *
 *                                                                      *
 ************************************************************************/

"))
      (buffer-string))))

  ;;;;;;;;;;;;;;;;
  ;; Auto-insert
  ;;

(setq auto-insert-alist
      (quote (

	      ;; C/C++ header
	      (("\\.\\([Hh]\\|hh\\|hpp\\|hxx\\)\\'" . "C / C++ header")
	       (upcase (let* ((name buffer-file-name)
                              (n (file-name-sans-extension (file-name-nondirectory name)))
                              (e (file-name-extension name)))
                         (concat "_" n "_" e "_")))
	       (concat (gpl-header) "#ifndef ") str n "#define " str ?\n ?\n _ ?\n ?\n "#endif /* " str " */")

	      ;; C/C++ implem
	      (("\\.\\([Cc]\\|cc\\|cpp\\|cxx\\)\\'" . "C / C++ program")
	       nil
	       (concat (gpl-header) "#include \"")
	       (let ((stem (file-name-sans-extension buffer-file-name)))
		 (cond ((file-exists-p (concat stem ".h")) (file-name-nondirectory (concat stem ".h")))
                       ((file-exists-p (concat stem ".hpp")) (file-name-nondirectory (concat stem ".hpp")))
                       ((file-exists-p (concat stem ".hxx")) (file-name-nondirectory (concat stem ".hxx")))
		       ((file-exists-p (concat stem ".hh")) (file-name-nondirectory (concat stem ".hh")))))
	       & "\"" | -10
               ?\n ?\n _)

	      ;; Makefile
	      ((makefile-mode . "Makefile")
               nil "### Makefile" ?\n ?\n "## Author: " user-full-name ?\n ?\n _ ?\n ?\n
               "### Makefile ends here" ?\n)

              ;; .pro
              ("\\.pro" . "pro")

	      ;; Html
;	      (html-mode lambda nil (sgml-tag "html"))
              (html-mode . (lambda () (template-expand-template (expand-file-name "~/.templates/TEMPLATE.html.tpl"))))

	      ;; Plain Tex
	      (plain-tex-mode . "tex-insert.tex")

	      ;; Bibtex
	      (bibtex-mode . "tex-insert.tex")

	      (latex-mode nil
                          "\\documentclass[" ("options, %s: " str & ",") & -1 "]{" (read-string "class: ") "}" ?\n
			  ("package, %s: " "\\usepackage[" ("options, %s: " str & ",") & -1 "]{" str "}" ?\n) ?\n
                          "\\begin{document}" ?\n ?\n _ ?\n ?\n "\\end{document}" ?\n ?\n
                          "%%% Local Variables:" ?\n "%%% mode: latex"  ?\n "%%% TeX-master: "
                          (let ((entry (read-string "Master: ")))
                            (if (not (equal entry ""))
                                (concat "\"" entry "\"") "t")) ?\n "%%% End:" ?\n)

	      ;; Ada
	      (ada-mode . ada-header)

	      ;; Elisp
	      (("\\.el\\'" . "Emacs Lisp header")
	       "Short description: " ";;; "
	       (file-name-nondirectory (buffer-file-name))
	       " --- " str "

;; Copyright (C) "
	       (substring (current-time-string) -4) "  "
	       (getenv "ORGANIZATION") | "Free Software Foundation, Inc." "

;; Author: " (user-full-name)
	     (quote (if (search-backward "&" (line-beginning-position) t)
			(replace-match (capitalize (user-login-name)) t t)))
	     (quote (end-of-line 1)) " <" (progn user-mail-address) ">
;; Keywords: "
	     (quote (require (quote finder)))
	     (quote (setq v1 (mapcar (lambda (x) (list (symbol-name (car x)))) finder-known-keywords) v2 (mapconcat (lambda (x) (format "%10.0s:  %s" (car x) (cdr x))) finder-known-keywords "
")))
	     ((let ((minibuffer-help-form v2)) (completing-read "Keyword, C-h: " v1 nil t)) str ", ") & -2 "

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

;; "
	     _ ?\n ?\n ";;; Code:

" _ "

(provide '"
(file-name-sans-extension (file-name-nondirectory (buffer-file-name))) ")
;;; "
(file-name-nondirectory (buffer-file-name)) " ends here
"))))

; Hippie expand enables completion of filenames/dirs in buffers
; Control Return makes the completion
;; Thanks to Klaus Berndl for code
(require 'hippie-exp)
(setq hippie-expand-try-functions-list
      '(tempo-complete-tag
	try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-list
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-whole-kill))

;; the expand-function. Called with a positive prefix <P> it jumpes direct
;; to the <P>-th try-function.
(defun my-hippie-expand (arg)
  (interactive "P")
  ;; hippie-expand does not have a customization-feature (like
  ;; dabbrev-expand) to search case-sensitive for completions.
  ;; So we must set 'case-fold-search' temp. to nil!
  (let ((old-case-fold-search case-fold-search))
    (setq case-fold-search nil)
    (hippie-expand arg)
    (setq case-fold-search old-case-fold-search)))

;; all expansion is done by 'my-hippie-expand bound to C-Return!
(global-set-key (quote [(control return)]) (quote my-hippie-expand))

;; Tempo

(defun tempo-insert-prompt-default (prompt default &optional save-name no-insert)
  "Prompt for a text string and insert it in the current buffer.
If the variable `tempo-interactive' is non-nil the user is prompted
for a string in the minibuffer, which is then inserted in the current
buffer. If `tempo-interactive' is nil, the current point is placed on
`tempo-mark'.

PROMPT is the prompt string, DEFAULT is the default value, SAVE-NAME is a name
to save the inserted text under. If the optional argument NO-INSERT is non-nil,
no text i inserted. This can be useful when there is a SAVE-NAME.

If there already is a value for SAVE-NAME, it is used and the user is
never prompted."
  (let (insertion
	(previous (and save-name
		       (tempo-lookup-named save-name))))
    (cond
     ;; Insert  previous value, unless no-insert is non-nil
     ((and previous
	   (not no-insert))
      (tempo-insert-named save-name)) ; A double lookup here, but who
				      ; cares
     ;; If no-insert is non-nil, don't insert the previous value. Just
     ;; keep it
     (previous
      nil)
     ;; No previous value. Prompt or insert mark
     (tempo-interactive
      (if (not (stringp prompt))
	  (error "tempo: The prompt (%s) is not a string" prompt))
      (setq insertion (read-string prompt (eval default)))
      (or no-insert
	  (insert insertion))
      (if save-name
	  (tempo-save-named save-name insertion)))
     (t
      (tempo-insert-mark (point-marker))))))

(defvar tempo-initial-pos nil
  "Initial position in template after expansion")

(defadvice tempo-insert( around tempo-insert-pos act )
  "Define initial position."
  (cond ((eq element '~)
         (setq tempo-initial-pos (point-marker)))
        ((and (consp element)
              (eq (car element) 'z)) (apply 'tempo-insert-prompt-default
                                            (cdr element)))
        (t
         ad-do-it)))

(defadvice tempo-insert-template( around tempo-insert-template-pos act )
  "Set initial position when defined. ChristophConrad"
  (setq tempo-initial-pos nil)
  ad-do-it
  (if tempo-initial-pos
      (progn
        (put template 'no-self-insert t)
        (goto-char tempo-initial-pos))
    (put template 'no-self-insert nil)))

(defun tempo-space ()
  (interactive "*")
  (or (tempo-expand-if-complete)
      (insert " ")))

(defun yh/tempo-define-template (name key list elements &optional tag documentation taglist)
  (tempo-define-template name elements tag documentation taglist)
  (add-to-list list (cons name key)))

(defun yh/tempo-build-local-map (alist)
  (mapcar (lambda (arg)
            (local-set-key
             (concat "\C-c\C-m" (cdr arg))
             (intern (concat "tempo-template-" (car arg)))))
          alist))

;; (require 'template)
;; (template-initialize)

(provide 'mycompletion)
;;; mycompletion.el ends here
