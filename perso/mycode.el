;;; mycode.el --- everything I need to code around

;; Copyright (C) 2003  Free Software Foundation, Inc.

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

(require 'cc-cmds)
(require 'patches)

(if (not (fboundp 'emacs-type-is-regular))
    (defun emacs-type-is-regular () t))

;---------------------------------------------------------------------
; C++ mode modifications
;

;; Define a new regexp for font-lock-mode
;; DONT'T MESS WITH IT
(if (emacs-type-is-regular)
    (defconst c++-new-font-lock-keywords
      (list
       '("\\<[0-9]+\\.[0-9]+\\>" (0 font-lock-floatnumber-face))
       '("^#[ 	]*error[ 	]+\\(.+\\)"
         (1 font-lock-warning-face prepend))
       '("^#[ 	]*\\(import\\|include\\)[ 	]*\\(<[^>\"\n]*>?\\)"
         (2 font-lock-string-face))
       '("^#[ 	]*define[ 	]+\\(\\sw+\\)("
         (1 font-lock-function-name-face))
       '("^#[ 	]*\\(elif\\|if\\)\\>"
         ("\\<\\(defined\\)\\>[ 	]*(?\\(\\sw+\\)?" nil nil
          (1 font-lock-builtin-face)
          (2 font-lock-variable-name-face nil t)))
       '("^#[ 	]*\\(\\sw+\\)\\>[ 	!]*\\(\\sw+\\)?"
         (1 font-lock-builtin-face)
         (2 font-lock-variable-name-face nil t))
       '("\\<\\(public\\|private\\|protected\\)\\>[ \t]+\\(\\<\\(signals\\|slots\\)\\>\\)[ \t]*:"
         (1 font-lock-type-face)
         (2 font-lock-type-face)
         )
       '("\\<\\(class\\|public\\|private\\|protected\\|typename\\|signals\\|slots\\)\\>[ 	]*\\(\\(\\sw+\\)\\>\\([ 	]*<\\([^>\n]+\\)[ 	*&]*>\\)?\\([ 	]*::[ 	*~]*\\(\\sw+\\)\\)*\\)?"
         (1 font-lock-type-face)
         (3
          (if
              (match-beginning 6)
              font-lock-type-face font-lock-function-name-face)
          nil t)
         (5 font-lock-function-name-face nil t)
         (7 font-lock-function-name-face nil t))
       '("^\\(\\sw+\\)\\>\\([ 	]*<\\([^>\n]+\\)[ 	*&]*>\\)?\\([ 	]*::[ 	*~]*\\(\\sw+\\)\\)*[ 	]*("
         (1
	  (if
	      (or
	       (match-beginning 2)
	       (match-beginning 4))
	      font-lock-type-face font-lock-function-name-face))
         (3 font-lock-function-name-face nil t)
         (5 font-lock-function-name-face nil t))
       '("\\<\\(auto\\|bool\\|c\\(har\\|o\\(mplex\\|nst\\)\\)\\|double\\|e\\(num\\|x\\(p\\(licit\\|ort\\)\\|tern\\)\\)\\|f\\(loat\\|riend\\)\\|in\\(line\\|t\\)\\|long\\|mutable\\|namespace\\|register\\|s\\(hort\\|igned\\|t\\(atic\\|ruct\\)\\)\\|t\\(emplate\\|ypedef\\)\\|u\\(n\\(ion\\|signed\\)\\|sing\\)\\|v\\(irtual\\|o\\(id\\|latile\\)\\)\\|Q[A-Z][a-zA-Z_]*\\|Q[a-z][A-Z][a-zA-Z_]*\\|uint\\|ulong\\|string\\)\\>"
         (0 font-lock-type-face))
       '("\\<\\(operator\\)\\>[ 	]*\\(!=\\|%=\\|&[&=]\\|()\\|\\*=\\|\\+[+=]\\|-\\(>\\*\\|[=>-]\\)\\|/=\\|<\\(<=\\|[<=]\\)\\|==\\|>\\(>=\\|[=>]\\)\\|\\[\\]\\|\\^=\\||[=|]\\|[!%&*+,/<=>|~^-]\\)?"
         (1 font-lock-keyword-face)
         (2 font-lock-builtin-face nil t))
       '("\\<\\(case\\|goto\\)\\>[ 	]*\\(-?\\sw+\\)?"
         (1 font-lock-keyword-face)
         (2 font-lock-constant-face nil t))
       '(":"
	 ("^[ 	]*\\(\\sw+\\)[ 	]*:\\($\\|[^:]\\)"
	  (beginning-of-line)
	  (end-of-line)
	  (1 font-lock-constant-face)))
       '("\\<\\(asm\\|break\\|c\\(atch\\|on\\(st_cast\\|tinue\\)\\)\\|d\\(elete\\|o\\|ynamic_cast\\)\\|else\\|for\\|if\\|new\\|re\\(interpret_cast\\|turn\\)\\|s\\(izeof\\|tatic_cast\\|witch\\)\\|t\\(h\\(is\\|row\\)\\|ry\\)\\|while\\)\\>"
	 (0 font-lock-keyword-face))
       '("\\<\\(false\\|true\\)\\>"
	 (0 font-lock-constant-face))
       '("\\<\\(auto\\|bool\\|c\\(har\\|o\\(mplex\\|nst\\)\\)\\|double\\|e\\(num\\|x\\(p\\(licit\\|ort\\)\\|tern\\)\\)\\|f\\(loat\\|riend\\)\\|in\\(line\\|t\\)\\|long\\|mutable\\|namespace\\|register\\|s\\(hort\\|igned\\|t\\(atic\\|ruct\\)\\)\\|t\\(emplate\\|ypedef\\)\\|u\\(n\\(ion\\|signed\\)\\|sing\\)\\|v\\(irtual\\|o\\(id\\|latile\\)\\)\\|JBF[a-zA-Z0-9_]*\\|eZ[a-zA-Z0-9_]*\\|Q[a-zA-Z_]*\\|uint\\|ulong\\|string\\)\\>\\([ 	]*<\\([^>\n]+\\)[ 	*&]*>\\)?\\([ 	]*::[ 	*~]*\\(\\sw+\\)\\)*\\([ 	*&]+\\(\\sw+\\)\\>\\([ 	]*<\\([^>\n]+\\)[ 	*&]*>\\)?\\([ 	]*::[ 	*~]*\\(\\sw+\\)\\)*\\)*"
	 (font-lock-match-c-style-declaration-item-and-skip-to-next
	  (goto-char
	   (or
	    (match-beginning 20)
	    (match-end 1)))
	  (goto-char
	   (match-end 1))
	  (1
	   (cond
	    ((or
	      (match-beginning 2)
	      (match-beginning 4))
	     font-lock-type-face)
	    ((match-beginning 6)
	     font-lock-function-name-face)
	    (t font-lock-variable-name-face)))
	  (3 font-lock-function-name-face nil t)
	  (5
	   (if
	       (match-beginning 6)
	       font-lock-function-name-face font-lock-variable-name-face)
	   nil t)))
       '("\\(}\\)[ 	*]*\\sw"
	 (font-lock-match-c-style-declaration-item-and-skip-to-next
	  (goto-char
	   (match-end 1))
	  nil
	  (1
	   (if
	       (match-beginning 6)
	       font-lock-function-name-face font-lock-variable-name-face))))
       '("^\\(\\(\\sw+\\)\\>\\([ 	]*<\\([^>\n]+\\)[ 	*&]*>\\)?\\([ 	]*::[ 	*~]*\\(\\sw+\\)\\)*[ 	*&]*\\)+"
	 (font-lock-match-c-style-declaration-item-and-skip-to-next
	  (goto-char
	   (match-beginning 1))
	  (goto-char
	   (match-end 1))
	  (1
	   (cond
	    ((or
	      (match-beginning 2)
	      (match-beginning 4))
	     font-lock-type-face)
	    ((match-beginning 6)
	     font-lock-function-name-face)
	    (t font-lock-variable-name-face)))
	  (3 font-lock-function-name-face nil t)
	  (5
	   (if
	       (match-beginning 6)
	       font-lock-function-name-face font-lock-variable-name-face)
	   nil t)))
       '("[{}()<>=;:+\\*\\/\\[]\\|\\]\\|\\-" (0 font-lock-keys-face))
       '("\\<[0-9]+\\>" (0 font-lock-number-face))
       '("\\<0x[0-9a-fA-F]+\\>" (0 font-lock-hexnumber-face))
					;     ((concat "\\<"
					; 	     (regexp-opt '("Q_OBJECT" "emit" "connect" "disconnect" "SIGNAL" "SLOT" "Q_EXPORT"))
					; 	     "\\>" )
					;      (0 font-lock-qt-face))
       '("\\<\\(Q_\\(EXPORT\\|OBJECT\\|PROPERTY\\)\\|S\\(IGNAL\\|LOT\\)\\|connect\\|disconnect\\|emit\\)\\>"
         (0 font-lock-qt-face))
       )))

(dolist (mode '(c-mode c++-mode java-mode php-mode)) (font-lock-add-keywords mode c++-new-font-lock-keywords))

(defun my-c-mode-common-hook()
  (interactive)
  ;; offset customizations not in my-c-style
  (c-set-offset 'member-init-intro '++)
  ;; Regular expression for the outline mode.
  ;; Enable outline mode with M-x outline-minor-mode
  (setq outline-regexp (concat
			"^"		; beginning of line is required
			"\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
			"\\([a-zA-Z0-9_:]+[ \t]+\\)?" ; type specs; there can be no
			"\\([a-zA-Z0-9_:]+[ \t]+\\)?" ; more than 3 tokens, right?

			"\\("		; last type spec including */&
			"[a-zA-Z0-9_:]+"
			"\\([ \t]*[*&]+[ \t]*\\|[ \t]+\\)" ; either pointer/ref sign or whitespace
			"\\)?"		; if there is a last type spec
			"\\("		; name; take that into the imenu entry
			"[a-zA-Z0-9_:~]+" ; member function, ctor or dtor...
					; (may not contain * because then
					; "a::operator char*" would become "char*"!)
			"\\|"
			"\\([a-zA-Z0-9_:~]*::\\)?operator"
			"[^a-zA-Z1-9_][^(]*" ; ...or operator
			" \\)"
			"[ \t]*([^)]*)[ \t\n]*[^ ;]" ; require something other than a ; after
			))
  ;; Figure out this one later
;;  (setq outline-heading-end-regexp "^{\n")

  ;; We want spaces instead of real tabs.
  (setq indent-tabs-mode nil)
  ;; other customizations

  ;; Allow c++-files only
  (make-local-variable 'buffer-include-regexp)
  (if c++-buffers-only
      (setq buffer-include-regexp '()))
  (setq buffer-include-regexp (cons c++-buffer-include-regexp buffer-include-regexp))

  (setq tab-width 4)
  ;; we like hungry-delete
  (c-toggle-hungry-state 1)
  ;; uncomment for those who like auto-newline
  ;; (c-toggle-auto-state 1)

  ;; keybindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, and idl-mode-map inherit from it.

  (local-set-key [S-f4] 'align)
  (outline-minor-mode)
  (define-key esc-map "\t" 'project-expand-symbol)
)

(defun my-php-mode-hook()
  (interactive)
  ;; offset customizations not in my-c-style
  (c-set-offset 'member-init-intro '+)
  ;; Regular expression for the outline mode.
  ;; Enable outline mode with M-x outline-minor-mode
  (setq outline-regexp "^[ \t\n\r\f]*function[ \t\n\r\f]+[a-zA-Z_0-9]+([^)]*)")

  ;; We want spaces instead of real tabs.
  (setq indent-tabs-mode nil)

  ;; Allow c++-files only
  (make-local-variable 'buffer-include-regexp)
  (if c++-buffers-only
      (setq buffer-include-regexp '()))
  (setq buffer-include-regexp (cons php-buffer-include-regexp buffer-include-regexp))

  (setq tab-width 4)
  ;; we like hungry-delete
  (c-toggle-hungry-state t)

  ;;Newline and indent source for enter.
  (local-set-key [RET] 'newline-and-indent)
  (c-set-style "ezphp")
)

(add-hook 'php-mode-hook 'my-php-mode-hook)

(add-hook 'c-mode-common-hook
          (lambda ()
            (my-c-mode-common-hook)
            ;; activate glasses
            (glasses-mode)
            (camelCase-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
		 (lambda ()
		   (not (eq (get-text-property (point) 'face)
			    'font-lock-comment-face))))))

(defvar c++-source-extension-list '("c" "cc" "C" "cpp" "c++"))
(defvar c++-header-extension-list '("h" "hh" "H" "hpp"))

(defun toggle-source-header()
  "Switches to the source buffer if currently in the header buffer and vice versa."
  (interactive)
  (let ((name (file-name-nondirectory (buffer-file-name)))
	(hc (header-source)))
    (if (equal name (car hc))
        (if (cdr hc)
            (find-file (cdr hc)))
      (if (car hc)
          (find-file (car hc))))))

;; Rehacked by me
(defun header-source ()
  "Returns a pair containing header and source name"
  (interactive)
  (let ((buf (current-buffer))
	(name (file-name-nondirectory (buffer-file-name)))
	file
	offs)
    (setq offs (string-match c++-header-ext-regexp name))
    (if offs
	(let ((lst c++-source-extension-list)
	      (ok nil)
	      ext)
	  (setq file (substring name 0 offs))
	  (while (and lst (not ok))
	    (setq ext (car lst))
	    (cond
             ((file-exists-p (concat file "." ext))
              (setq ok t))
             ((file-exists-p (concat "../src/" file "." ext))
              (setq file (concat "../src/" file))
              (setq ok t))
             ((file-exists-p (concat "../sources/" file "." ext))
              (setq file (concat "../sources/" file))
              (setq ok t)))
	    (setq lst (cdr lst)))
	  (when ok
            (cons name (concat file "." ext))))
      (progn
	(setq offs (string-match c++-source-ext-regexp name))
	(when offs
          (let ((lst c++-header-extension-list)
                (ok nil)
                ext)
            (setq file (substring name 0 offs))
            (while (and lst (not ok))
              (setq ext (car lst))
              (cond ((file-exists-p (concat file "." ext))
                     (setq ok t))
                    ((file-exists-p (concat "../inc/" file "." ext))
                     (setq file (concat "../inc/" file))
                     (setq ok t))
                    ((file-exists-p (concat "../include/" file "." ext))
                     (setq file (concat "../include/" file))
                     (setq ok t)))
              (setq lst (cdr lst)))
            (when ok
              (cons (concat file "." ext)  name))))))))

; C++ member functions;
(add-hook 'c++-mode-hook (lambda () (local-set-key "\C-cm" (lambda ()
                                                             (interactive)
                                                             (let ((hc (header-source))
                                                                   (dir (file-name-directory (buffer-file-name))))
                                                               (expand-member-functions (concat dir (car hc)) (concat dir (cdr hc))))))))

(defconst my-c-style
  ;; Always indent c/c++ sources, never insert tabs
  '((c-tab-always-indent        . t)
    ;; Offset for line only comments
    (c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    ;; Controls the insertion of newlines before and after braces.
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (defun-open after)
                                   (class-open after)
                                   (inline-open after)
                                   (block-open after)
				   (brace-list-open after)
                                   (extern-lang-open after)
                                   (namespace-open after)))
    ;; Controls the insertion of newlines before and after certain colons.
    (c-hanging-colons-alist     . ((member-init-intro before)
				   (inher-intro)
				   (case-label after)
				   (label after)
				   (access-label after)))
    ;; List of various C/C++/ObjC constructs to "clean up".
    (c-cleanup-list             . (scope-operator
				   empty-defun-braces
				   defun-close-semi))
    ;; Association list of syntactic element symbols and indentation offsets.
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
				   (substatement-open . 0)
				   (case-label        . +)
				   (block-open        . 0)
				   (access-label      . -)
				   (label	      . 0)
				   (knr-argdecl-intro . -)))
					;	(c-echo-syntactic-information-p . t)
    )
  "My C/C++ Programming Style")

(defconst ba-c-style
  ;; Always indent c/c++ sources, never insert tabs
  '((c-tab-always-indent        . t)
    ;; Offset for line only comments
    (c-basic-offset . 4)
    (indent-tabs-mode . t)
    (c-comment-only-line-offset . 0)
    ;; Controls the insertion of newlines before and after braces.
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (defun-open after)
                                   (class-open after)
                                   (inline-open after)
                                   (block-open after)
				   (brace-list-open after)
                                   (extern-lang-open after)
                                   (namespace-open after)))
    ;; Controls the insertion of newlines before and after certain colons.
    (c-hanging-colons-alist     . ((member-init-intro before)
				   (inher-intro)
				   (case-label after)
				   (label after)
				   (access-label after)))
    ;; List of various C/C++/ObjC constructs to "clean up".
    (c-cleanup-list             . (scope-operator
				   empty-defun-braces
				   defun-close-semi
                                   brace-else-brace
                                   brace-elseif-brace))
    ;; Association list of syntactic element symbols and indentation offsets.
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
				   (substatement-open . 0)
				   (case-label        . +)
				   (block-open        . 0)
				   (access-label      . -)
				   (label	      . 0)
				   (knr-argdecl-intro . -)
                                   (innamespace . nil)))
					;	(c-echo-syntactic-information-p . t)
    )
  "Blind-Assist C/C++ Programming Style")

(defconst camille-c-style
  ;; Always indent c/c++ sources, never insert tabs
  '((c-tab-always-indent        . t)
    ;; Offset for line only comments
    (c-basic-offset . 4)
    (indent-tabs-mode . t)
    (c-comment-only-line-offset . 0)
    ;; Controls the insertion of newlines before and after braces.
    (c-hanging-braces-alist     . (
;                                   (substatement-open after)
;                                   (defun-open after)
;                                   (class-open after)
                                   (inline-open after)
;                                   (block-open after)
;                                  (brace-list-open after)
                                   (extern-lang-open after)
;                                   (namespace-open after)
))
    ;; Controls the insertion of newlines before and after certain colons.
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    ;; List of various C/C++/ObjC constructs to "clean up".
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi
                                   brace-else-brace
                                   brace-elseif-brace))
    ;; Association list of syntactic element symbols and indentation offsets.
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (arglist-cont-nonempty . 0)
                                   (substatement-open . 0)
                                   (case-label        . +)
                                   (block-open        . 0)
                                   (access-label      . -)
                                   (label             . 0)
                                   (knr-argdecl-intro . -)
                                   (innamespace . nil)))
                                        ;       (c-echo-syntactic-information-p . t)
    )
  "Camille C/C++ Programming Style")

(defconst ezsystems-c-style
  ;; Always indent c/c++ sources, never insert tabs
  '((c-tab-always-indent        . t)
    (c-basic-offset . 4)
    ;; Offset for line only comments
    (c-comment-only-line-offset . 0)
    ;; Controls the insertion of newlines before and after braces.
    (c-hanging-braces-alist     . ((substatement-open after)
				   (brace-list-open)))
    ;; Controls the insertion of newlines before and after certain colons.
    (c-hanging-colons-alist     . ((member-init-intro before)
				   (inher-intro)
				   (case-label after)
				   (label after)
				   (access-label after)))
    ;; List of various C/C++/ObjC constructs to "clean up".
    (c-cleanup-list             . (scope-operator
				   empty-defun-braces
				   defun-close-semi))
    ;; Association list of syntactic element symbols and indentation offsets.
    (c-offsets-alist            . (
				   (arglist-close . c-lineup-arglist)
				   (substatement-open . 0)
				   (case-label        . +)
				   (block-open        . 0)
				   (access-label      . -)
				   (label	      . 0)
				   (knr-argdecl-intro . -)))
					;	(c-echo-syntactic-information-p . t)
    )
  "eZ systems Programming Style")

;; PHP related stuff

;(require 'php-mode)

(defconst ezsystems-php-style
  ;; Always indent c/c++ sources, never insert tabs
  '((c-tab-always-indent        . t)
    (c-basic-offset . 4)
    ;; Offset for line only comments
    (c-comment-only-line-offset . 0)
    ;; Controls the insertion of newlines before and after braces.
    (c-hanging-braces-alist     . ((substatement-open after)
				   (brace-list-open)))
    ;; Controls the insertion of newlines before and after certain colons.
    (c-hanging-colons-alist     . ((member-init-intro before)
				   (inher-intro)
				   (case-label after)
				   (label after)
				   (access-label after)))
    ;; List of various C/C++/ObjC constructs to "clean up".
    (c-cleanup-list             . (scope-operator
				   empty-defun-braces
				   defun-close-semi))
    ;; Association list of syntactic element symbols and indentation offsets.
    (c-offsets-alist            . (
				   (arglist-close . c-lineup-arglist)
				   (substatement-open . 0)
				   (case-label        . +)
				   (block-open        . 0)
				   (access-label      . -)
				   (label	      . 0)
				   (knr-argdecl-intro . -)
				   (inline-open . 0)))
					;	(c-echo-syntactic-information-p . t)
    )
  "eZ systems PHP Programming Style")

;; add my personal style.
(c-add-style "personal" my-c-style)
(c-add-style "bassist" ba-c-style)
(c-add-style "camille" camille-c-style)
(c-add-style "eZSystems" ezsystems-c-style)
(c-add-style "eZPHP" ezsystems-php-style)

(provide 'mycode)
;;; mycode.el ends here
