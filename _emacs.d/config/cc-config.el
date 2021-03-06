;;; cc-config.el ---

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

(eval-when (load)
  (require 'patches))

;; Default extension for c++ header files.
(defvar c++-default-header-ext "hpp")
;; Default extension for c++ source files.
(defvar c++-default-source-ext "cpp")
;; Default regexp for c++ header files.
(defvar c++-header-ext-regexp "\\.\\(hpp\\|h\\|\hh\\|H\\)$")
;; Default regexp for c++ source files.
(defvar c++-source-ext-regexp "\\.\\(cpp\\|c\\|\cc\\|C\\)$")
;; Default regexp for includes
(defvar project-c++-include-regexp "#include[ \t]+\\(\\(<[^>]*>\\)\\|\\(\"[^\"]*\"\\)\\)[ \t]*\n")
;; Default regexp for class declarations
(defvar project-c++-class-decl-regexp "class[ \t]+\\([A-Za-z][A-Za-z0-9_]*\\);[ \t]*\n")

;; Default list of paths to check for include files, add your own to the list
;; by adding the line (setq project-include-paths (cons "/my/dir" project-include-paths))
;; somewhere in your .emacs file
(defvar project-include-paths '( "/usr/include"
				 "/usr/include/stl"
				 "/usr/local/include"
				 "$QTDIR/include" ))

;; If non-nil automaticly insert/remove include files in c++ files
(defvar c++-auto-include-add nil)
(defvar c++-auto-include-remove nil)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; new access protection clauses for qt
(defconst c-qt-access-key "\\<\\(public\\|protected\\|private\\)\\>[ \t]+\\<slots\\>")

(defconst c-qt-C++-access-key (concat "\\("
				      "\\<\\(public\\|protected\\|private\\|signals\\|slots\\)\\>"
 				      "\\)\\|\\("
;; 				      c-qt-access-key
 				      "\\)[ \t]*:"))

;; Which buffers to include, default all.
(defvar buffer-include-regexp '(".*"))

;; Which buffers to exclude, default *blah* buffers.
(defvar buffer-exclude-regexp '("\\*[^\\*]+\\*"))

;; Define c++ regexp
(defvar c++-buffer-include-regexp (concat "[^.]*\\.\\("
					  (regexp-opt '( "cpp" "cc" "c" "C" "hpp" "hh" "h" "H" ) t)
					  "\\)"))

;; Define c++ regexp
(defvar php-buffer-include-regexp (concat "[^.]*\\.\\("
					  (regexp-opt '( "php" "php3" "php4" ) t)
					  "\\)"))

;; Define project regexp
(defvar project-buffer-include-regexp (concat "[^.]*\\.\\("
					      (regexp-opt '( "pro" ) t)
					      "\\)"))
;; non-nil means browse only cpp files
(defvar c++-buffers-only nil)

;; Wanted c++ style
(defvar wanted-c++-style "eZSystems")

(request 'xcscope)

;---------------------------------------------------------------------
; C++ mode modifications
;

;; Define a new regexp for font-lock-mode
;; DONT'T MESS WITH IT
(defconst c++-new-font-lock-keywords
  (list
   '("\\<\\(asm\\|break\\|c\\(atch\\|on\\(st_cast\\|tinue\\)\\)\\|d\\(elete\\|o\\|ynamic_cast\\)\\|else\\|for\\|if\\|new\\|re\\(interpret_cast\\|turn\\)\\|s\\(izeof\\|tatic_cast\\|witch\\)\\|t\\(h\\(is\\|row\\)\\|ry\\)\\|while\\)\\>"
     (0 font-lock-keyword-face))
   '("\\<\\(false\\|true\\)\\>"
     (0 font-lock-constant-face))
   '("[{}()<>=;,:+\\*\\/\\[]\\|\\]\\|\\-" (0 font-lock-keys-face))
   '("\\<[0-9]+\\>" (0 font-lock-number-face))
   '("\\<0x[0-9a-fA-F]+\\>" (0 font-lock-hexnumber-face))
   '("\\\\\\(?:[abfnrtv'\"?\\0]\\|x[a-fA-F]\\{2\\}\\|[0-7]\\{3\\}\\)"
     (0 font-lock-escape-char-face prepend))))

(dolist (mode '(c-mode c++-mode java-mode php-mode))
  (font-lock-add-keywords mode c++-new-font-lock-keywords))

(add-hook
 'java-mode-hook
 '(lambda () "Treat Java 1.5 @-style annotations as comments."
    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(defun my-c-mode-common-hook()
  (interactive)
  (hs-minor-mode 1)

  ;; offset customizations not in my-c-style
  (c-set-offset 'member-init-intro '++)

  ;; We want spaces instead of real tabs.
  (setq indent-tabs-mode nil)
  ;; other customizations

  ;; Allow c++-files only
  (make-local-variable 'buffer-include-regexp)
  (if c++-buffers-only
      (setq buffer-include-regexp '()))
  (setq buffer-include-regexp (cons c++-buffer-include-regexp buffer-include-regexp))

  (setq tab-width 4)
  ;; uncomment for those who like hungry-delete
  ;; (c-toggle-hungry-state 1)
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
  (define-key esc-map "\t" 'project-expand-symbol)

  ;; Qt specific : correct indentation with "public slots" and friends
  (setq c-opt-access-key "\\(p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)\\|slots\\|signals\\)[ 	\n\f]*\\(?:[ 	\n\f]slots\\)?:"))

(defun my-php-mode-hook()
  (interactive)
  ;; offset customizations not in my-c-style
  (c-set-offset 'member-init-intro '+)

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
            (subword-mode 1)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (my-c-mode-common-hook)
            ;; activate glasses
            (glasses-mode 1)
            (subword-mode 1)
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

(require 'ffap)
(setq ffap-c-path
      (append '("../include" "../inc") ffap-c-path))

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

(defconst jtop-c-style
  ;; Always indent c/c++ sources, never insert tabs
  '((c-tab-always-indent        . t)
    ;; Offset for line only comments
    (c-basic-offset . 2)
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

(defconst vmware-c-style
  ;; Always indent c/c++ sources, never insert tabs
  '((c-tab-always-indent        . t)
    ;; Offset for line only comments
    (c-basic-offset . 3)
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
				   (knr-argdecl-intro . -))))
  "VMware C/C++ Programming Style")

(defconst ulteo-c-style
  ;; Always indent c/c++ sources, never insert tabs
  '((c-tab-always-indent        . t)
    (indent-tabs-mode . t)
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
                                   (label             . 0)
                                   (knr-argdecl-intro . -))))
  "Ulteo C/C++ Programming Style")

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defconst linux-tabs-c-style
  '("linux"
    (c-offsets-alist
     (arglist-cont-nonempty
      c-lineup-gcc-asm-reg
      c-lineup-arglist-tabs-only))))

;; add my personal style.
(c-add-style "personal" my-c-style)
(c-add-style "bassist" ba-c-style)
(c-add-style "camille" camille-c-style)
(c-add-style "eZSystems" ezsystems-c-style)
(c-add-style "eZPHP" ezsystems-php-style)
(c-add-style "jtop" jtop-c-style)
(c-add-style "vmware" vmware-c-style)
(c-add-style "ulteo" ulteo-c-style)
(c-add-style "linux-tabs-only" linux-tabs-c-style)

(provide 'cc-config)
;;; mycode.el ends here
