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

;; (require 'cc-cmds)
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
  (local-set-key (kbd "M-RET") (lambda ()
                                 (interactive)
                                 (progn
                                   (newline-and-indent)
                                   (insert "* "))))
  (outline-minor-mode)
  (define-key esc-map "\t" 'project-expand-symbol)

  (setq c-opt-access-key "\\(p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)\\|slots\\|signals\\)[ 	\n\f]*\\(?:[ 	\n\f]slots\\)?:")
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
  (local-set-key "\r" 'newline-and-indent)
)

(add-hook 'php-mode-user-hook
          (lambda ()
            (my-php-mode-hook)
            (glasses-mode 1)
            (camelCase-mode 1)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (my-c-mode-common-hook)
            ;; activate glasses
            (glasses-mode 1)
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

;; awful hack but no choice (5E comes before 5C)
(defun c-guess-basic-syntax ()
  "Return the syntactic context of the current line.
This function does not do any hidden buffer changes."
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (c-save-buffer-state
	  ((indent-point (point))
;	   (case-fold-search nil)
	   (paren-state (c-parse-state))
	   literal containing-sexp char-before-ip char-after-ip lim
	   c-syntactic-context placeholder c-in-literal-cache step-type
	   tmpsymbol keyword injava-inher special-brace-list
	   ;; narrow out any enclosing class or extern "C" block
	   (inclass-p (c-narrow-out-enclosing-class paren-state
						    indent-point))
	   ;; `c-state-cache' is shadowed here so that we don't
	   ;; throw it away due to the narrowing that might be done
	   ;; by the function above.  That means we must not do any
	   ;; changes during the execution of this function, since
	   ;; `c-invalidate-state-cache' then would change this local
	   ;; variable and leave a bogus value in the global one.
	   (c-state-cache (if inclass-p
			      (c-whack-state-before (point-min) paren-state)
			    paren-state))
	   (c-state-cache-start (point-min))
	   inenclosing-p macro-start in-macro-expr
	   ;; There's always at most one syntactic element which got
	   ;; a relpos.  It's stored in syntactic-relpos.
	   syntactic-relpos
	   (c-stmt-delim-chars c-stmt-delim-chars))
	;; Check for meta top-level enclosing constructs such as
	;; extern language definitions.
	(save-excursion
	  (save-restriction
	    (widen)
	    (when (and inclass-p
		       (progn
			 (goto-char (aref inclass-p 0))
			 (looking-at c-other-decl-block-key)))
	      (setq inenclosing-p (match-string 1))
	      (if (string-equal inenclosing-p "extern")
		  ;; Compatibility with legacy choice of name for the
		  ;; extern-lang syntactic symbols.
		  (setq inenclosing-p "extern-lang")))))

	;; Init some position variables:
	;;
	;; containing-sexp is the open paren of the closest
	;; surrounding sexp or nil if there is none that hasn't been
	;; narrowed out.
	;;
	;; lim is the position after the closest preceding brace sexp
	;; (nested sexps are ignored), or the position after
	;; containing-sexp if there is none, or (point-min) if
	;; containing-sexp is nil.
	;;
	;; c-state-cache is the state from c-parse-state at
	;; indent-point, without any parens outside the region
	;; narrowed by c-narrow-out-enclosing-class.
	;;
	;; paren-state is the state from c-parse-state outside
	;; containing-sexp, or at indent-point if containing-sexp is
	;; nil.  paren-state is not limited to the narrowed region, as
	;; opposed to c-state-cache.
	(if c-state-cache
	    (progn
	      (setq containing-sexp (car paren-state)
		    paren-state (cdr paren-state))
	      (if (consp containing-sexp)
		  (progn
		    (setq lim (cdr containing-sexp))
		    (if (cdr c-state-cache)
			;; Ignore balanced paren.  The next entry
			;; can't be another one.
			(setq containing-sexp (car (cdr c-state-cache))
			      paren-state (cdr paren-state))
		      ;; If there is no surrounding open paren then
		      ;; put the last balanced pair back on paren-state.
		      (setq paren-state (cons containing-sexp paren-state)
			    containing-sexp nil)))
		(setq lim (1+ containing-sexp))))
	  (setq lim (point-min)))

	;; If we're in a parenthesis list then ',' delimits the
	;; "statements" rather than being an operator (with the
	;; exception of the "for" clause).  This difference is
	;; typically only noticeable when statements are used in macro
	;; arglists.
	(when (and containing-sexp
		   (eq (char-after containing-sexp) ?\())
	  (setq c-stmt-delim-chars c-stmt-delim-chars-with-comma))

	;; cache char before and after indent point, and move point to
	;; the most likely position to perform the majority of tests
	(goto-char indent-point)
	(c-backward-syntactic-ws lim)
	(setq char-before-ip (char-before))
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(setq char-after-ip (char-after))

	;; are we in a literal?
	(setq literal (c-in-literal lim))

	;; now figure out syntactic qualities of the current line
	(cond
	 ;; CASE 1: in a string.
	 ((eq literal 'string)
	  (c-add-syntax 'string (c-point 'bopl)))
	 ;; CASE 2: in a C or C++ style comment.
	 ((and (memq literal '(c c++))
	       ;; This is a kludge for XEmacs where we use
	       ;; `buffer-syntactic-context', which doesn't correctly
	       ;; recognize "\*/" to end a block comment.
	       ;; `parse-partial-sexp' which is used by
	       ;; `c-literal-limits' will however do that in most
	       ;; versions, which results in that we get nil from
	       ;; `c-literal-limits' even when `c-in-literal' claims
	       ;; we're inside a comment.
	       (setq placeholder (c-literal-limits lim)))
	  (c-add-syntax literal (car placeholder)))
	 ;; CASE 3: in a cpp preprocessor macro continuation.
	 ((and (save-excursion
		 (when (c-beginning-of-macro)
		   (setq macro-start (point))))
	       (/= macro-start (c-point 'boi))
	       (progn
		 (setq tmpsymbol 'cpp-macro-cont)
		 (or (not c-syntactic-indentation-in-macros)
		     (save-excursion
		       (goto-char macro-start)
		       ;; If at the beginning of the body of a #define
		       ;; directive then analyze as cpp-define-intro
		       ;; only.  Go on with the syntactic analysis
		       ;; otherwise.  in-macro-expr is set if we're in a
		       ;; cpp expression, i.e. before the #define body
		       ;; or anywhere in a non-#define directive.
		       (if (c-forward-to-cpp-define-body)
			   (let ((indent-boi (c-point 'boi indent-point)))
			     (setq in-macro-expr (> (point) indent-boi)
				   tmpsymbol 'cpp-define-intro)
			     (= (point) indent-boi))
			 (setq in-macro-expr t)
			 nil)))))
	  (c-add-syntax tmpsymbol macro-start)
	  (setq macro-start nil))
	 ;; CASE 11: an else clause?
	 ((looking-at "else\\>[^_]")
	  (c-beginning-of-statement-1 containing-sexp)
	  (c-add-stmt-syntax 'else-clause nil t nil
			     containing-sexp paren-state))
	 ;; CASE 12: while closure of a do/while construct?
	 ((and (looking-at "while\\>[^_]")
	       (save-excursion
		 (prog1 (eq (c-beginning-of-statement-1 containing-sexp)
			    'beginning)
		   (setq placeholder (point)))))
	  (goto-char placeholder)
	  (c-add-stmt-syntax 'do-while-closure nil t nil
			     containing-sexp paren-state))
	 ;; CASE 13: A catch or finally clause?  This case is simpler
	 ;; than if-else and do-while, because a block is required
	 ;; after every try, catch and finally.
	 ((save-excursion
	    (and (cond ((c-major-mode-is 'c++-mode)
			(looking-at "catch\\>[^_]"))
		       ((c-major-mode-is 'java-mode)
			(looking-at "\\(catch\\|finally\\)\\>[^_]")))
		 (and (c-safe (c-backward-syntactic-ws)
			      (c-backward-sexp)
			      t)
		      (eq (char-after) ?{)
		      (c-safe (c-backward-syntactic-ws)
			      (c-backward-sexp)
			      t)
		      (if (eq (char-after) ?\()
			  (c-safe (c-backward-sexp) t)
			t))
		 (looking-at "\\(try\\|catch\\)\\>[^_]")
		 (setq placeholder (point))))
	  (goto-char placeholder)
	  (c-add-stmt-syntax 'catch-clause nil t nil
			     containing-sexp paren-state))
	 ;; CASE 18: A substatement we can recognize by keyword.
	 ((save-excursion
	    (and c-opt-block-stmt-key
		 (if (c-mode-is-new-awk-p)
                     (c-awk-prev-line-incomplete-p containing-sexp) ; ACM 2002/3/29
                   (not (eq char-before-ip ?\;)))
		 (not (memq char-after-ip '(?\) ?\] ?,)))
		 (or (not (eq char-before-ip ?}))
		     (c-looking-at-inexpr-block-backward c-state-cache))
		 (> (point)
		    (progn
		      ;; Ought to cache the result from the
		      ;; c-beginning-of-statement-1 calls here.
		      (setq placeholder (point))
		      (while (eq (setq step-type
				       (c-beginning-of-statement-1 lim))
				 'label))
		      (if (eq step-type 'previous)
			  (goto-char placeholder)
			(setq placeholder (point))
			(if (and (eq step-type 'same)
				 (not (looking-at c-opt-block-stmt-key)))
			    ;; Step up to the containing statement if we
			    ;; stayed in the same one.
			    (let (step)
			      (while (eq
				      (setq step
					    (c-beginning-of-statement-1 lim))
				      'label))
			      (if (eq step 'up)
				  (setq placeholder (point))
				;; There was no containing statement afterall.
				(goto-char placeholder)))))
		      placeholder))
		 (if (looking-at c-block-stmt-2-key)
		     ;; Require a parenthesis after these keywords.
		     ;; Necessary to catch e.g. synchronized in Java,
		     ;; which can be used both as statement and
		     ;; modifier.
		     (and (zerop (c-forward-token-2 1 nil))
			  (eq (char-after) ?\())
		   (looking-at c-opt-block-stmt-key))))
	  (if (eq step-type 'up)
	      ;; CASE 18A: Simple substatement.
	      (progn
		(goto-char placeholder)
		(cond
		 ((eq char-after-ip ?{)
		  (c-add-stmt-syntax 'substatement-open nil nil nil
				     containing-sexp paren-state))
		 ((save-excursion
		    (goto-char indent-point)
		    (back-to-indentation)
		    (looking-at c-label-key))
		  (c-add-stmt-syntax 'substatement-label nil nil nil
				     containing-sexp paren-state))
		 (t
		  (c-add-stmt-syntax 'substatement nil nil nil
				     containing-sexp paren-state))))
	    ;; CASE 18B: Some other substatement.  This is shared
	    ;; with case 10.
	    (c-guess-continued-construct indent-point
					 char-after-ip
					 placeholder
					 lim
					 paren-state)))
	 ;; CASE 4: In-expression statement.  C.f. cases 7B, 16A and
	 ;; 17E.
	 ((and (or c-opt-inexpr-class-key
		   c-opt-inexpr-block-key
		   c-opt-lambda-key)
	       (setq placeholder (c-looking-at-inexpr-block
				  (c-safe-position containing-sexp paren-state)
				  containing-sexp)))
	  (setq tmpsymbol (assq (car placeholder)
				'((inexpr-class . class-open)
				  (inexpr-statement . block-open))))
	  (if tmpsymbol
	      ;; It's a statement block or an anonymous class.
	      (setq tmpsymbol (cdr tmpsymbol))
	    ;; It's a Pike lambda.  Check whether we are between the
	    ;; lambda keyword and the argument list or at the defun
	    ;; opener.
	    (setq tmpsymbol (if (eq char-after-ip ?{)
				'inline-open
			      'lambda-intro-cont)))
	  (goto-char (cdr placeholder))
	  (back-to-indentation)
	  (c-add-stmt-syntax tmpsymbol nil t nil
			     (c-most-enclosing-brace c-state-cache (point))
			     (c-whack-state-after (point) paren-state))
	  (unless (eq (point) (cdr placeholder))
	    (c-add-syntax (car placeholder))))
	 ;; CASE 5: Line is at top level.
	 ((null containing-sexp)
	  (cond
	   ;; CASE 5A: we are looking at a defun, brace list, class,
	   ;; or inline-inclass method opening brace
	   ((setq special-brace-list
		  (or (and c-special-brace-lists
			   (c-looking-at-special-brace-list))
		      (eq char-after-ip ?{)))
	    (cond
	     ;; CASE 5A.1: Non-class declaration block open.
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t")
		(and (c-safe (c-backward-sexp 2) t)
		     (looking-at c-other-decl-block-key)
		     (setq keyword (match-string 1)
			   placeholder (point))
		     (if (string-equal keyword "extern")
			 ;; Special case for extern-lang-open.  The
			 ;; check for a following string is disabled
			 ;; since it doesn't disambiguate anything.
			 (and ;;(progn
			      ;;  (c-forward-sexp 1)
			      ;;  (c-forward-syntactic-ws)
			      ;;  (eq (char-after) ?\"))
			      (setq tmpsymbol 'extern-lang-open))
		       (setq tmpsymbol (intern (concat keyword "-open"))))
		     ))
	      (goto-char placeholder)
	      (c-add-syntax tmpsymbol (c-point 'boi)))
	     ;; CASE 5A.2: we are looking at a class opening brace
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t{")
		(let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		  (and decl
		       (setq placeholder (aref decl 0)))
		  ))
	      (c-add-syntax 'class-open placeholder))
	     ;; CASE 5A.3: brace list open
	     ((save-excursion
		(c-beginning-of-decl-1 lim)
		(while (looking-at c-specifier-key)
		  (goto-char (match-end 1))
		  (c-forward-syntactic-ws indent-point))
		(setq placeholder (c-point 'boi))
		(or (consp special-brace-list)
		    (and (or (save-excursion
			       (goto-char indent-point)
			       (setq tmpsymbol nil)
			       (while (and (> (point) placeholder)
					   (zerop (c-backward-token-2 1 t))
					   (/= (char-after) ?=))
				 (and c-opt-inexpr-brace-list-key
				      (not tmpsymbol)
				      (looking-at c-opt-inexpr-brace-list-key)
				      (setq tmpsymbol 'topmost-intro-cont)))
			       (eq (char-after) ?=))
			     (looking-at c-brace-list-key))
			 (save-excursion
			   (while (and (< (point) indent-point)
				       (zerop (c-forward-token-2 1 t))
				       (not (memq (char-after) '(?\; ?\()))))
			   (not (memq (char-after) '(?\; ?\()))
			   ))))
	      (if (and (not c-auto-newline-analysis)
		       (c-major-mode-is 'java-mode)
		       (eq tmpsymbol 'topmost-intro-cont))
		  ;; We're in Java and have found that the open brace
		  ;; belongs to a "new Foo[]" initialization list,
		  ;; which means the brace list is part of an
		  ;; expression and not a top level definition.  We
		  ;; therefore treat it as any topmost continuation
		  ;; even though the semantically correct symbol still
		  ;; is brace-list-open, on the same grounds as in
		  ;; case B.2.
		  (progn
		    (c-beginning-of-statement-1 lim)
		    (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
		(c-add-syntax 'brace-list-open placeholder)))
	     ;; CASE 5A.4: inline defun open
	     ((and inclass-p (not inenclosing-p))
	      (c-add-syntax 'inline-open)
	      (c-add-class-syntax 'inclass inclass-p paren-state))
	     ;; CASE 5A.5: ordinary defun open
	     (t
	      (goto-char placeholder)
	      (if (or inclass-p macro-start)
		  (c-add-syntax 'defun-open (c-point 'boi))
		;; Bogus to use bol here, but it's the legacy.
		(c-add-syntax 'defun-open (c-point 'bol)))
	      )))
	   ;; CASE 5B: first K&R arg decl or member init
	   ((c-just-after-func-arglist-p lim)
	    (cond
	     ;; CASE 5B.1: a member init
	     ((or (eq char-before-ip ?:)
		  (eq char-after-ip ?:))
	      ;; this line should be indented relative to the beginning
	      ;; of indentation for the topmost-intro line that contains
	      ;; the prototype's open paren
	      ;; TBD: is the following redundant?
	      (if (eq char-before-ip ?:)
		  (forward-char -1))
	      (c-backward-syntactic-ws lim)
	      ;; TBD: is the preceding redundant?
	      (if (eq (char-before) ?:)
		  (progn (forward-char -1)
			 (c-backward-syntactic-ws lim)))
	      (if (eq (char-before) ?\))
		  (c-backward-sexp 1))
	      (setq placeholder (point))
	      (save-excursion
		(and (c-safe (c-backward-sexp 1) t)
		     (looking-at "throw[^_]")
		     (c-safe (c-backward-sexp 1) t)
		     (setq placeholder (point))))
	      (goto-char placeholder)
	      (c-add-syntax 'member-init-intro (c-point 'boi))
	      ;; we don't need to add any class offset since this
	      ;; should be relative to the ctor's indentation
	      )
	     ;; CASE 5B.2: K&R arg decl intro
	     ((and c-recognize-knr-p
		   (c-in-knr-argdecl lim))
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'knr-argdecl-intro (c-point 'boi))
	      (if inclass-p
		  (c-add-class-syntax 'inclass inclass-p paren-state)))
	     ;; CASE 5B.3: Inside a member init list.
	     ((c-beginning-of-member-init-list lim)
	      (c-forward-syntactic-ws)
	      (c-add-syntax 'member-init-cont (point)))
	     ;; CASE 5B.4: Nether region after a C++ or Java func
	     ;; decl, which could include a `throws' declaration.
	     (t
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'func-decl-cont (c-point 'boi))
	      )))
	   ;; CASE 5E: we are looking at a access specifier
	   ((and inclass-p
		 c-opt-access-key
		 (looking-at c-opt-access-key))
	    (setq placeholder (c-add-class-syntax 'inclass inclass-p
						  paren-state))
	    ;; Append access-label with the same anchor point as inclass gets.
	    (c-append-syntax 'access-label placeholder))
	   ;; CASE 5C: inheritance line. could be first inheritance
	   ;; line, or continuation of a multiple inheritance
	   ((or (and (c-major-mode-is 'c++-mode)
		     (progn
		       (when (eq char-after-ip ?,)
			 (skip-chars-forward " \t")
			 (forward-char))
		       (looking-at c-opt-postfix-decl-spec-key)))
		(and (or (eq char-before-ip ?:)
			 ;; watch out for scope operator
			 (save-excursion
			   (and (eq char-after-ip ?:)
				(c-safe (forward-char 1) t)
				(not (eq (char-after) ?:))
				)))
		     (save-excursion
		       (c-backward-syntactic-ws lim)
		       (if (eq char-before-ip ?:)
			   (progn
			     (forward-char -1)
			     (c-backward-syntactic-ws lim)))
		       (back-to-indentation)
		       (looking-at c-class-key)))
		;; for Java
		(and (c-major-mode-is 'java-mode)
		     (let ((fence (save-excursion
				    (c-beginning-of-statement-1 lim)
				    (point)))
			   cont done)
		       (save-excursion
			 (while (not done)
			   (cond ((looking-at c-opt-postfix-decl-spec-key)
				  (setq injava-inher (cons cont (point))
					done t))
				 ((or (not (c-safe (c-forward-sexp -1) t))
				      (<= (point) fence))
				  (setq done t))
				 )
			   (setq cont t)))
		       injava-inher)
		     (not (c-crosses-statement-barrier-p (cdr injava-inher)
							 (point)))
		     ))
	    (cond
	     ;; CASE 5C.1: non-hanging colon on an inher intro
	     ((eq char-after-ip ?:)
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )
	     ;; CASE 5C.2: hanging colon on an inher intro
	     ((eq char-before-ip ?:)
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      (if inclass-p
		  (c-add-class-syntax 'inclass inclass-p paren-state)))
	     ;; CASE 5C.3: in a Java implements/extends
	     (injava-inher
	      (let ((where (cdr injava-inher))
		    (cont (car injava-inher)))
		(goto-char where)
		(cond ((looking-at "throws\\>[^_]")
		       (c-add-syntax 'func-decl-cont
				     (progn (c-beginning-of-statement-1 lim)
					    (c-point 'boi))))
		      (cont (c-add-syntax 'inher-cont where))
		      (t (c-add-syntax 'inher-intro
				       (progn (goto-char (cdr injava-inher))
					      (c-beginning-of-statement-1 lim)
					      (point))))
		      )))
	     ;; CASE 5C.4: a continued inheritance line
	     (t
	      (c-beginning-of-inheritance-list lim)
	      (c-add-syntax 'inher-cont (point))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )))
	   ;; CASE 5D: this could be a top-level initialization, a
	   ;; member init list continuation, or a template argument
	   ;; list continuation.
	   ((c-with-syntax-table (if (c-major-mode-is 'c++-mode)
				     c++-template-syntax-table
				   (syntax-table))
	      (save-excursion
		;; Note: We use the fact that lim is always after any
		;; preceding brace sexp.
		(while (and (zerop (c-backward-token-2 1 t lim))
			    (not (looking-at "[;<,=]"))))
		(or (memq (char-after) '(?, ?=))
		    (and (c-major-mode-is 'c++-mode)
			 (zerop (c-backward-token-2 1 nil lim))
			 (eq (char-after) ?<)))))
	    (goto-char indent-point)
	    (setq placeholder
		  (c-beginning-of-member-init-list lim))
	    (cond
	     ;; CASE 5D.1: hanging member init colon, but watch out
	     ;; for bogus matches on access specifiers inside classes.
	     ((and placeholder
		   (save-excursion
		     (setq placeholder (point))
		     (c-backward-token-2 1 t lim)
		     (and (eq (char-after) ?:)
			  (not (eq (char-before) ?:))))
		   (save-excursion
		     (goto-char placeholder)
		     (back-to-indentation)
		     (or
		      (/= (car (save-excursion
				 (parse-partial-sexp (point) placeholder)))
			  0)
		      (and
		       (if c-opt-access-key
			   (not (looking-at c-opt-access-key)) t)
		       (not (looking-at c-class-key))
		       (if c-opt-bitfield-key
			   (not (looking-at c-opt-bitfield-key)) t))
		      )))
	      (goto-char placeholder)
	      (c-forward-syntactic-ws)
	      (c-add-syntax 'member-init-cont (point))
	      ;; we do not need to add class offset since relative
	      ;; point is the member init above us
	      )
	     ;; CASE 5D.2: non-hanging member init colon
	     ((progn
		(c-forward-syntactic-ws indent-point)
		(eq (char-after) ?:))
	      (skip-chars-forward " \t:")
	      (c-add-syntax 'member-init-cont (point)))
	     ;; CASE 5D.3: perhaps a template list continuation?
	     ((and (c-major-mode-is 'c++-mode)
		   (save-excursion
		     (save-restriction
		       (c-with-syntax-table c++-template-syntax-table
			 (goto-char indent-point)
			 (setq placeholder (c-up-list-backward (point)))
			 (and placeholder
			      (eq (char-after placeholder) ?<))))))
	      ;; we can probably indent it just like an arglist-cont
	      (goto-char placeholder)
	      (c-beginning-of-statement-1 lim t)
	      (c-add-syntax 'template-args-cont (c-point 'boi)))
	     ;; CASE 5D.4: perhaps a multiple inheritance line?
	     ((and (c-major-mode-is 'c++-mode)
		   (save-excursion
		     (c-beginning-of-statement-1 lim)
		     (setq placeholder (point))
		     (if (looking-at "static\\>[^_]")
			 (c-forward-token-2 1 nil indent-point))
		     (and (looking-at c-class-key)
			  (zerop (c-forward-token-2 2 nil indent-point))
			  (if (eq (char-after) ?<)
			      (c-with-syntax-table c++-template-syntax-table
				(zerop (c-forward-token-2 1 t indent-point)))
			    t)
			  (eq (char-after) ?:))))
	      (goto-char placeholder)
	      (c-add-syntax 'inher-cont (c-point 'boi)))
	     ;; CASE 5D.5: Continuation of the "expression part" of a
	     ;; top level construct.
	     (t
	      (while (and (eq (car (c-beginning-of-decl-1 containing-sexp))
			      'same)
			  (save-excursion
			    (c-backward-syntactic-ws)
			    (eq (char-before) ?}))))
	      (c-add-stmt-syntax
	       (if (eq char-before-ip ?,)
		   ;; A preceding comma at the top level means that a
		   ;; new variable declaration starts here.  Use
		   ;; topmost-intro-cont for it, for consistency with
		   ;; the first variable declaration.  C.f. case 5N.
		   'topmost-intro-cont
		 'statement-cont)
	       nil nil nil containing-sexp paren-state))
	     ))
	   ;; CASE 5F: Close of a non-class declaration level block.
	   ((and inenclosing-p
		 (eq char-after-ip ?}))
	    (c-add-syntax (intern (concat inenclosing-p "-close"))
			  (aref inclass-p 0)))
	   ;; CASE 5G: we are looking at the brace which closes the
	   ;; enclosing nested class decl
	   ((and inclass-p
		 (eq char-after-ip ?})
		 (save-excursion
		   (save-restriction
		     (widen)
		     (forward-char 1)
		     (and (c-safe (c-backward-sexp 1) t)
			  (= (point) (aref inclass-p 1))
			  ))))
	    (c-add-class-syntax 'class-close inclass-p paren-state))
	   ;; CASE 5H: we could be looking at subsequent knr-argdecls
	   ((and c-recognize-knr-p
		 (not (eq char-before-ip ?}))
		 (save-excursion
		   (setq placeholder (cdr (c-beginning-of-decl-1 lim)))
		   (and placeholder
			;; Do an extra check to avoid tripping up on
			;; statements that occur in invalid contexts
			;; (e.g. in macro bodies where we don't really
			;; know the context of what we're looking at).
			(not (and c-opt-block-stmt-key
				  (looking-at c-opt-block-stmt-key)))))
		 (< placeholder indent-point))
	    (goto-char placeholder)
	    (c-add-syntax 'knr-argdecl (point)))
	   ;; CASE 5I: ObjC method definition.
	   ((and c-opt-method-key
		 (looking-at c-opt-method-key))
	    (c-beginning-of-statement-1 lim)
	    (c-add-syntax 'objc-method-intro (c-point 'boi)))
           ;; CASE 5P: AWK pattern or function or continuation
           ;; thereof.
           ((c-mode-is-new-awk-p)
            (setq placeholder (point))
            (c-add-stmt-syntax
             (if (and (eq (c-beginning-of-statement-1) 'same)
                      (/= (point) placeholder))
                 'topmost-intro-cont
               'topmost-intro)
             nil nil nil
             containing-sexp paren-state))
	   ;; CASE 5N: At a variable declaration that follows a class
	   ;; definition or some other block declaration that doesn't
	   ;; end at the closing '}'.  C.f. case 5D.5.
	   ((progn
	      (c-backward-syntactic-ws lim)
	      (and (eq (char-before) ?})
		   (save-excursion
		     (let ((start (point)))
		       (if paren-state
			   ;; Speed up the backward search a bit.
			   (goto-char (car (car paren-state))))
		       (c-beginning-of-decl-1 containing-sexp)
		       (setq placeholder (point))
		       (if (= start (point))
			   ;; The '}' is unbalanced.
			   nil
			 (c-end-of-decl-1)
			 (>= (point) indent-point))))))
	    (goto-char placeholder)
	    (c-add-stmt-syntax 'topmost-intro-cont nil nil nil
			       containing-sexp paren-state))
	   ;; CASE 5J: we are at the topmost level, make
	   ;; sure we skip back past any access specifiers
	   ((progn
	      (while (and inclass-p
			  c-opt-access-key
			  (not (bobp))
			  (save-excursion
			    (c-safe (c-backward-sexp 1) t)
			    (looking-at c-opt-access-key)))
		(c-backward-sexp 1)
		(c-backward-syntactic-ws lim))
	      (or (bobp)
                  (if (c-mode-is-new-awk-p)
                      (not (c-awk-prev-line-incomplete-p))
                    (memq (char-before) '(?\; ?})))
		  (and (c-major-mode-is 'objc-mode)
		       (progn
			 (c-beginning-of-statement-1 lim)
			 (eq (char-after) ?@)))))
	    ;; real beginning-of-line could be narrowed out due to
	    ;; enclosure in a class block
	    (save-restriction
	      (widen)
	      (c-add-syntax 'topmost-intro (c-point 'bol))
	      ;; Using bol instead of boi above is highly bogus, and
	      ;; it makes our lives hard to remain compatible. :P
	      (if inclass-p
		  (progn
		    (goto-char (aref inclass-p 1))
		    (or (= (point) (c-point 'boi))
			(goto-char (aref inclass-p 0)))
		    (if inenclosing-p
			(c-add-syntax (intern (concat "in" inenclosing-p))
				      (c-point 'boi))
		      (c-add-class-syntax 'inclass inclass-p paren-state))
		    ))
	      (when (and c-syntactic-indentation-in-macros
			 macro-start
			 (/= macro-start (c-point 'boi indent-point)))
		(c-add-syntax 'cpp-define-intro)
		(setq macro-start nil))
	      ))
	   ;; CASE 5K: we are at an ObjC method definition
	   ;; continuation line.
	   ((and c-opt-method-key
		 (progn
		   (c-beginning-of-statement-1 lim)
		   (beginning-of-line)
		   (looking-at c-opt-method-key)))
	    (c-add-syntax 'objc-method-args-cont (point)))
	   ;; CASE 5L: we are at the first argument of a template
	   ;; arglist that begins on the previous line.
	   ((eq (char-before) ?<)
	    (c-beginning-of-statement-1 (c-safe-position (point) paren-state))
	    (c-add-syntax 'template-args-cont (c-point 'boi)))
	   ;; CASE 5M: we are at a topmost continuation line
	   (t
	    (c-beginning-of-statement-1 (c-safe-position (point) paren-state))
	    (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
	   ))
	 ;; (CASE 6 has been removed.)
	 ;; CASE 7: line is an expression, not a statement.  Most
	 ;; likely we are either in a function prototype or a function
	 ;; call argument list
	 ((not (or (and c-special-brace-lists
			(save-excursion
			  (goto-char containing-sexp)
			  (c-looking-at-special-brace-list)))
		   (eq (char-after containing-sexp) ?{)))
	  (cond
	   ;; CASE 7A: we are looking at the arglist closing paren.
	   ;; C.f. case 7F.
	   ((memq char-after-ip '(?\) ?\]))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (if (and (c-safe (backward-up-list 1) t)
		     (>= (point) placeholder))
		(progn
		  (forward-char)
		  (skip-chars-forward " \t"))
	      (goto-char placeholder))
	    (c-add-stmt-syntax 'arglist-close (list containing-sexp) t nil
			       (c-most-enclosing-brace paren-state (point))
			       (c-whack-state-after (point) paren-state)))
	   ;; CASE 7B: Looking at the opening brace of an
	   ;; in-expression block or brace list.  C.f. cases 4, 16A
	   ;; and 17E.
	   ((and (eq char-after-ip ?{)
		 (progn
		   (setq placeholder (c-inside-bracelist-p (point)
							   c-state-cache))
		   (if placeholder
		       (setq tmpsymbol '(brace-list-open . inexpr-class))
		     (setq tmpsymbol '(block-open . inexpr-statement)
			   placeholder
			   (cdr-safe (c-looking-at-inexpr-block
				      (c-safe-position containing-sexp
						       paren-state)
				      containing-sexp)))
		     ;; placeholder is nil if it's a block directly in
		     ;; a function arglist.  That makes us skip out of
		     ;; this case.
		     )))
	    (goto-char placeholder)
	    (back-to-indentation)
	    (c-add-stmt-syntax (car tmpsymbol) nil t nil
			       (c-most-enclosing-brace paren-state (point))
			       (c-whack-state-after (point) paren-state))
	    (if (/= (point) placeholder)
		(c-add-syntax (cdr tmpsymbol))))
	   ;; CASE 7C: we are looking at the first argument in an empty
	   ;; argument list. Use arglist-close if we're actually
	   ;; looking at a close paren or bracket.
	   ((memq char-before-ip '(?\( ?\[))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (when (and (c-safe (backward-up-list 1) t)
		       (>= (point) placeholder))
	      (forward-char)
	      (skip-chars-forward " \t")
	      (setq placeholder (point)))
	    (c-add-syntax 'arglist-intro placeholder))
	   ;; CASE 7D: we are inside a conditional test clause. treat
	   ;; these things as statements
	   ((progn
	      (goto-char containing-sexp)
	      (and (c-safe (c-forward-sexp -1) t)
		   (looking-at "\\<for\\>[^_]")))
	    (goto-char (1+ containing-sexp))
	    (c-forward-syntactic-ws indent-point)
	    (if (eq char-before-ip ?\;)
		(c-add-syntax 'statement (point))
	      (c-add-syntax 'statement-cont (point))
	      ))
	   ;; CASE 7E: maybe a continued ObjC method call. This is the
	   ;; case when we are inside a [] bracketed exp, and what
	   ;; precede the opening bracket is not an identifier.
	   ((and c-opt-method-key
		 (eq (char-after containing-sexp) ?\[)
		 (progn
		   (goto-char (1- containing-sexp))
		   (c-backward-syntactic-ws (c-point 'bod))
		   (if (not (looking-at c-symbol-key))
		       (c-add-syntax 'objc-method-call-cont containing-sexp))
		   )))
	   ;; CASE 7F: we are looking at an arglist continuation line,
	   ;; but the preceding argument is on the same line as the
	   ;; opening paren.  This case includes multi-line
	   ;; mathematical paren groupings, but we could be on a
	   ;; for-list continuation line.  C.f. case 7A.
	   ((progn
	      (goto-char (1+ containing-sexp))
	      (skip-chars-forward " \t")
	      (and (not (eolp))
		   (not (looking-at "\\\\$"))))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (if (and (c-safe (backward-up-list 1) t)
		     (>= (point) placeholder))
		(progn
		  (forward-char)
		  (skip-chars-forward " \t"))
	      (goto-char placeholder))
	    (c-add-stmt-syntax 'arglist-cont-nonempty (list containing-sexp)
			       t nil
			       (c-most-enclosing-brace c-state-cache (point))
			       (c-whack-state-after (point) paren-state)))
	   ;; CASE 7G: we are looking at just a normal arglist
	   ;; continuation line
	   (t (c-forward-syntactic-ws indent-point)
	      (c-add-syntax 'arglist-cont (c-point 'boi)))
	   ))
	 ;; CASE 8: func-local multi-inheritance line
	 ((and (c-major-mode-is 'c++-mode)
	       (save-excursion
		 (goto-char indent-point)
		 (skip-chars-forward " \t")
		 (looking-at c-opt-postfix-decl-spec-key)))
	  (goto-char indent-point)
	  (skip-chars-forward " \t")
	  (cond
	   ;; CASE 8A: non-hanging colon on an inher intro
	   ((eq char-after-ip ?:)
	    (c-backward-syntactic-ws lim)
	    (c-add-syntax 'inher-intro (c-point 'boi)))
	   ;; CASE 8B: hanging colon on an inher intro
	   ((eq char-before-ip ?:)
	    (c-add-syntax 'inher-intro (c-point 'boi)))
	   ;; CASE 8C: a continued inheritance line
	   (t
	    (c-beginning-of-inheritance-list lim)
	    (c-add-syntax 'inher-cont (point))
	    )))
	 ;; CASE 9: we are inside a brace-list
	 ((and (not (c-mode-is-new-awk-p))  ; Maybe this isn't needed (ACM, 2002/3/29)
               (setq special-brace-list
                     (or (and c-special-brace-lists
                              (save-excursion
                                (goto-char containing-sexp)
                                (c-looking-at-special-brace-list)))
                         (c-inside-bracelist-p containing-sexp paren-state))))
	  (cond
	   ;; CASE 9A: In the middle of a special brace list opener.
	   ((and (consp special-brace-list)
		 (save-excursion
		   (goto-char containing-sexp)
		   (eq (char-after) ?\())
		 (eq char-after-ip (car (cdr special-brace-list))))
	    (goto-char (car (car special-brace-list)))
	    (skip-chars-backward " \t")
	    (if (and (bolp)
		     (assoc 'statement-cont
			    (setq placeholder (c-guess-basic-syntax))))
		(setq c-syntactic-context placeholder)
	      (c-beginning-of-statement-1
	       (c-safe-position (1- containing-sexp) paren-state))
	      (c-forward-token-2 0)
	      (while (looking-at c-specifier-key)
		(goto-char (match-end 1))
		(c-forward-syntactic-ws))
	      (c-add-syntax 'brace-list-open (c-point 'boi))))
	   ;; CASE 9B: brace-list-close brace
	   ((if (consp special-brace-list)
		;; Check special brace list closer.
		(progn
		  (goto-char (car (car special-brace-list)))
		  (save-excursion
		    (goto-char indent-point)
		    (back-to-indentation)
		    (or
		     ;; We were between the special close char and the `)'.
		     (and (eq (char-after) ?\))
			  (eq (1+ (point)) (cdr (car special-brace-list))))
		     ;; We were before the special close char.
		     (and (eq (char-after) (cdr (cdr special-brace-list)))
			  (zerop (c-forward-token-2))
			  (eq (1+ (point)) (cdr (car special-brace-list)))))))
	      ;; Normal brace list check.
	      (and (eq char-after-ip ?})
		   (c-safe (goto-char (c-up-list-backward (point))) t)
		   (= (point) containing-sexp)))
	    (if (eq (point) (c-point 'boi))
		(c-add-syntax 'brace-list-close (point))
	      (setq lim (c-most-enclosing-brace c-state-cache (point)))
	      (c-beginning-of-statement-1 lim)
	      (c-add-stmt-syntax 'brace-list-close nil t t lim
				 (c-whack-state-after (point) paren-state))))
	   (t
	    ;; Prepare for the rest of the cases below by going to the
	    ;; token following the opening brace
	    (if (consp special-brace-list)
		(progn
		  (goto-char (car (car special-brace-list)))
		  (c-forward-token-2 1 nil indent-point))
	      (goto-char containing-sexp))
	    (forward-char)
	    (let ((start (point)))
	      (c-forward-syntactic-ws indent-point)
	      (goto-char (max start (c-point 'bol))))
	    (c-skip-ws-forward indent-point)
	    (cond
	     ;; CASE 9C: we're looking at the first line in a brace-list
	     ((= (point) indent-point)
	      (if (consp special-brace-list)
		  (goto-char (car (car special-brace-list)))
		(goto-char containing-sexp))
	      (if (eq (point) (c-point 'boi))
		  (c-add-syntax 'brace-list-intro (point))
		(setq lim (c-most-enclosing-brace c-state-cache (point)))
		(c-beginning-of-statement-1 lim)
		(c-add-stmt-syntax 'brace-list-intro nil t t lim
				   (c-whack-state-after (point) paren-state))))
	     ;; CASE 9D: this is just a later brace-list-entry or
	     ;; brace-entry-open
	     (t (if (or (eq char-after-ip ?{)
			(and c-special-brace-lists
			     (save-excursion
			       (goto-char indent-point)
			       (c-forward-syntactic-ws (c-point 'eol))
			       (c-looking-at-special-brace-list (point)))))
		    (c-add-syntax 'brace-entry-open (point))
		  (c-add-syntax 'brace-list-entry (point))
		  ))
	     ))))
	 ;; CASE 10: A continued statement or top level construct.
	 ((and (if (c-mode-is-new-awk-p)
                   (c-awk-prev-line-incomplete-p containing-sexp) ; ACM 2002/3/29
                 (and (not (memq char-before-ip '(?\; ?:)))
                      (or (not (eq char-before-ip ?}))
                          (c-looking-at-inexpr-block-backward c-state-cache))))
	       (> (point)
		  (save-excursion
		    (c-beginning-of-statement-1 containing-sexp)
		    (setq placeholder (point))))
	       (/= placeholder containing-sexp))
	  ;; This is shared with case 18.
	  (c-guess-continued-construct indent-point
				       char-after-ip
				       placeholder
				       containing-sexp
				       paren-state))
	 ;; CASE 14: A case or default label
	 ((looking-at c-label-kwds-regexp)
	  (goto-char containing-sexp)
	  (setq lim (c-most-enclosing-brace c-state-cache containing-sexp))
	  (c-backward-to-block-anchor lim)
	  (c-add-stmt-syntax 'case-label nil t nil
			     lim paren-state))
	 ;; CASE 15: any other label
	 ((looking-at c-label-key)
	  (goto-char containing-sexp)
	  (setq lim (c-most-enclosing-brace c-state-cache containing-sexp))
	  (save-excursion
	    (setq tmpsymbol
		  (if (and (eq (c-beginning-of-statement-1 lim) 'up)
			   (looking-at "switch\\>[^_]"))
		      ;; If the surrounding statement is a switch then
		      ;; let's analyze all labels as switch labels, so
		      ;; that they get lined up consistently.
		      'case-label
		    'label)))
	  (c-backward-to-block-anchor lim)
	  (c-add-stmt-syntax tmpsymbol nil t nil
			     lim paren-state))
	 ;; CASE 16: block close brace, possibly closing the defun or
	 ;; the class
	 ((eq char-after-ip ?})
	  ;; From here on we have the next containing sexp in lim.
	  (setq lim (c-most-enclosing-brace paren-state))
	  (goto-char containing-sexp)
	    (cond
	     ;; CASE 16E: Closing a statement block?  This catches
	     ;; cases where it's preceded by a statement keyword,
	     ;; which works even when used in an "invalid" context,
	     ;; e.g. a macro argument.
	     ((c-after-conditional)
	      (c-backward-to-block-anchor lim)
	      (c-add-stmt-syntax 'block-close nil t nil
				 lim paren-state))
	     ;; CASE 16A: closing a lambda defun or an in-expression
	     ;; block?  C.f. cases 4, 7B and 17E.
	     ((setq placeholder (c-looking-at-inexpr-block
				 (c-safe-position containing-sexp paren-state)
				 nil))
	      (setq tmpsymbol (if (eq (car placeholder) 'inlambda)
				  'inline-close
				'block-close))
	      (goto-char containing-sexp)
	      (back-to-indentation)
	      (if (= containing-sexp (point))
		  (c-add-syntax tmpsymbol (point))
		(goto-char (cdr placeholder))
		(back-to-indentation)
		(c-add-stmt-syntax tmpsymbol nil t nil
				   (c-most-enclosing-brace paren-state (point))
				   (c-whack-state-after (point) paren-state))
		(if (/= (point) (cdr placeholder))
		    (c-add-syntax (car placeholder)))))
	     ;; CASE 16B: does this close an inline or a function in
	     ;; a non-class declaration level block?
	     ((setq placeholder (c-search-uplist-for-classkey paren-state))
	      (c-backward-to-decl-anchor lim)
	      (back-to-indentation)
	      (if (save-excursion
		    (goto-char (aref placeholder 0))
		    (looking-at c-other-decl-block-key))
		  (c-add-syntax 'defun-close (point))
		(c-add-syntax 'inline-close (point))))
	     ;; CASE 16F: Can be a defun-close of a function declared
	     ;; in a statement block, e.g. in Pike or when using gcc
	     ;; extensions, but watch out for macros followed by
	     ;; blocks.  Let it through to be handled below.
	     ;; C.f. cases B.3 and 17G.
	     ((and (not inenclosing-p)
		   lim
		   (save-excursion
		     (and (not (c-looking-at-bos))
			  (eq (c-beginning-of-statement-1 lim nil nil t) 'same)
			  (setq placeholder (point))
			  ;; Look for a type or identifier followed by a
			  ;; symbol, i.e. the start of a function declaration.
			  ;; Doesn't work for declarations like "int *foo()
			  ;; ..."; we'd need to refactor the more competent
			  ;; analysis in `c-font-lock-declarations' for that.
			  (c-forward-type)
			  (progn
			    (c-forward-syntactic-ws)
			    (looking-at c-symbol-start)))))
	      (back-to-indentation)
	      (if (/= (point) containing-sexp)
		  (goto-char placeholder))
	      (c-add-stmt-syntax 'defun-close nil t nil
				 lim paren-state))
	     ;; CASE 16C: if there an enclosing brace that hasn't
	     ;; been narrowed out by a class, then this is a
	     ;; block-close.  C.f. case 17H.
	     ((and (not inenclosing-p) lim)
	      ;; If the block is preceded by a case/switch label on
	      ;; the same line, we anchor at the first preceding label
	      ;; at boi.  The default handling in c-add-stmt-syntax is
	      ;; really fixes it better, but we do like this to keep
	      ;; the indentation compatible with version 5.28 and
	      ;; earlier.
	      (while (and (/= (setq placeholder (point)) (c-point 'boi))
			  (eq (c-beginning-of-statement-1 lim) 'label)))
	      (goto-char placeholder)
	      (if (looking-at c-label-kwds-regexp)
		  (c-add-syntax 'block-close (point))
		(goto-char containing-sexp)
		;; c-backward-to-block-anchor not necessary here; those
		;; situations are handled in case 16E above.
		(c-add-stmt-syntax 'block-close nil t nil
				   lim paren-state)))
	     ;; CASE 16D: find out whether we're closing a top-level
	     ;; class or a defun
	     (t
	      (save-restriction
		(narrow-to-region (point-min) indent-point)
		(let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		  (if decl
		      (c-add-class-syntax 'class-close decl paren-state)
		    (goto-char containing-sexp)
		    (c-backward-to-decl-anchor lim)
		    (back-to-indentation)
		    (c-add-syntax 'defun-close (point)))))
	      )))
	 ;; CASE 17: Statement or defun catchall.
	 (t
	  (goto-char indent-point)
	  ;; Back up statements until we find one that starts at boi.
	  (while (let* ((prev-point (point))
			(last-step-type (c-beginning-of-statement-1
					 containing-sexp)))
		   (if (= (point) prev-point)
		       (progn
			 (setq step-type (or step-type last-step-type))
			 nil)
		     (setq step-type last-step-type)
		     (/= (point) (c-point 'boi)))))
	  (cond
	   ;; CASE 17B: continued statement
	   ((and (eq step-type 'same)
		 (/= (point) indent-point))
	    (c-add-stmt-syntax 'statement-cont nil nil nil
			       containing-sexp paren-state))
	   ;; CASE 17A: After a case/default label?
	   ((progn
	      (while (and (eq step-type 'label)
			  (not (looking-at c-label-kwds-regexp)))
		(setq step-type
		      (c-beginning-of-statement-1 containing-sexp)))
	      (eq step-type 'label))
	    (c-add-stmt-syntax (if (eq char-after-ip ?{)
				   'statement-case-open
				 'statement-case-intro)
			       nil t nil containing-sexp paren-state))
	   ;; CASE 17D: any old statement
	   ((progn
	      (while (eq step-type 'label)
		(setq step-type
		      (c-beginning-of-statement-1 containing-sexp)))
	      (eq step-type 'previous))
	    (c-add-stmt-syntax 'statement nil t nil
			       containing-sexp paren-state)
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ;; CASE 17I: Inside a substatement block.
	   ((progn
	      ;; The following tests are all based on containing-sexp.
	      (goto-char containing-sexp)
	      ;; From here on we have the next containing sexp in lim.
	      (setq lim (c-most-enclosing-brace paren-state containing-sexp))
	      (c-after-conditional))
	    (c-backward-to-block-anchor lim)
	    (c-add-stmt-syntax 'statement-block-intro nil t nil
			       lim paren-state)
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ;; CASE 17E: first statement in an in-expression block.
	   ;; C.f. cases 4, 7B and 16A.
	   ((setq placeholder (c-looking-at-inexpr-block
			       (c-safe-position containing-sexp paren-state)
			       nil))
	    (setq tmpsymbol (if (eq (car placeholder) 'inlambda)
				'defun-block-intro
			      'statement-block-intro))
	    (back-to-indentation)
	    (if (= containing-sexp (point))
		(c-add-syntax tmpsymbol (point))
	      (goto-char (cdr placeholder))
	      (back-to-indentation)
	      (c-add-stmt-syntax tmpsymbol nil t nil
				 (c-most-enclosing-brace c-state-cache (point))
				 (c-whack-state-after (point) paren-state))
	      (if (/= (point) (cdr placeholder))
		  (c-add-syntax (car placeholder))))
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ;; CASE 17F: first statement in an inline, or first
	   ;; statement in a top-level defun. we can tell this is it
	   ;; if there are no enclosing braces that haven't been
	   ;; narrowed out by a class (i.e. don't use bod here).
	   ((save-excursion
	      (save-restriction
		(widen)
		(c-narrow-out-enclosing-class paren-state containing-sexp)
		(not (c-most-enclosing-brace paren-state))))
	    (c-backward-to-decl-anchor lim)
	    (back-to-indentation)
	    (c-add-syntax 'defun-block-intro (point)))
	   ;; CASE 17G: First statement in a function declared inside
	   ;; a normal block.  This can occur in Pike and with
	   ;; e.g. the gcc extensions, but watch out for macros
	   ;; followed by blocks.  C.f. cases B.3 and 16F.
	   ((save-excursion
	      (and (not (c-looking-at-bos))
		   (eq (c-beginning-of-statement-1 lim nil nil t) 'same)
		   (setq placeholder (point))
		   ;; Look for a type or identifier followed by a
		   ;; symbol, i.e. the start of a function declaration.
		   ;; Doesn't work for declarations like "int *foo()
		   ;; ..."; we'd need to refactor the more competent
		   ;; analysis in `c-font-lock-declarations' for that.
		   (c-forward-type)
		   (progn
		     (c-forward-syntactic-ws)
		     (looking-at c-symbol-start))))
	    (back-to-indentation)
	    (if (/= (point) containing-sexp)
		(goto-char placeholder))
	    (c-add-stmt-syntax 'defun-block-intro nil t nil
			       lim paren-state))
	   ;; CASE 17H: First statement in a block.  C.f. case 16C.
	   (t
	    ;; If the block is preceded by a case/switch label on the
	    ;; same line, we anchor at the first preceding label at
	    ;; boi.  The default handling in c-add-stmt-syntax is
	    ;; really fixes it better, but we do like this to keep the
	    ;; indentation compatible with version 5.28 and earlier.
	    (while (and (/= (setq placeholder (point)) (c-point 'boi))
			(eq (c-beginning-of-statement-1 lim) 'label)))
	    (goto-char placeholder)
	    (if (looking-at c-label-kwds-regexp)
		(c-add-syntax 'statement-block-intro (point))
	      (goto-char containing-sexp)
	      ;; c-backward-to-block-anchor not necessary here; those
	      ;; situations are handled in case 17I above.
	      (c-add-stmt-syntax 'statement-block-intro nil t nil
				 lim paren-state))
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ))
	 )
	;; now we need to look at any modifiers
	(goto-char indent-point)
	(skip-chars-forward " \t")
	;; are we looking at a comment only line?
	(when (and (looking-at c-comment-start-regexp)
		   (/= (c-forward-token-2 0 nil (c-point 'eol)) 0))
	  (c-append-syntax 'comment-intro))
	;; we might want to give additional offset to friends (in C++).
	(when (and c-opt-friend-key
		   (looking-at c-opt-friend-key))
	  (c-append-syntax 'friend))

	;; Set syntactic-relpos.
	(let ((p c-syntactic-context))
	  (while (and p
		      (if (integerp (car-safe (cdr-safe (car p))))
			  (progn
			    (setq syntactic-relpos (car (cdr (car p))))
			    nil)
			t))
	    (setq p (cdr p))))

	;; Start of or a continuation of a preprocessor directive?
	(if (and macro-start
		 (eq macro-start (c-point 'boi))
		 (not (and (c-major-mode-is 'pike-mode)
			   (eq (char-after (1+ macro-start)) ?\"))))
	    (c-append-syntax 'cpp-macro)
	  (when (and c-syntactic-indentation-in-macros macro-start)
	    (if in-macro-expr
		(when (or
		       (< syntactic-relpos macro-start)
		       (not (or
			     (assq 'arglist-intro c-syntactic-context)
			     (assq 'arglist-cont c-syntactic-context)
			     (assq 'arglist-cont-nonempty c-syntactic-context)
			     (assq 'arglist-close c-syntactic-context))))
		  ;; If inside a cpp expression, i.e. anywhere in a
		  ;; cpp directive except a #define body, we only let
		  ;; through the syntactic analysis that is internal
		  ;; in the expression.  That means the arglist
		  ;; elements, if they are anchored inside the cpp
		  ;; expression.
		  (setq c-syntactic-context nil)
		  (c-add-syntax 'cpp-macro-cont macro-start))
	      (when (and (eq macro-start syntactic-relpos)
			 (not (assq 'cpp-define-intro c-syntactic-context))
			 (save-excursion
			   (goto-char macro-start)
			   (or (not (c-forward-to-cpp-define-body))
			       (<= (point) (c-point 'boi indent-point)))))
		;; Inside a #define body and the syntactic analysis is
		;; anchored on the start of the #define.  In this case
		;; we add cpp-define-intro to get the extra
		;; indentation of the #define body.
		(c-add-syntax 'cpp-define-intro)))))
	;; return the syntax
	c-syntactic-context))))

(provide 'mycode)
;;; mycode.el ends here
