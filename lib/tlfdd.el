;;; tlfdd.el --- 

;; Copyright (C) 2008  Free Software Foundation, Inc.

;; Author: Yann Hodique <yann.hodique@trusted-logic.fr>
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

(require 'cl)

(defgroup tlfdd nil
  "TL-FDD mode."
  :tag "TL-FDD"
  :group 'languages)

(defmacro tlfdd-defvar (var &optional def doc &rest args)
  (declare (indent 1))
  (cond ((boundp var)
         `(setq ,var ,def))
        (args
         `(defcustom ,var ,def ,doc ,@args))
        (t
         `(defvar ,var ,def ,doc))))

(defmacro tlfdd-defface (face spec doc &rest args)
  (declare (indent 1))
  `(progn
     (defface ,face ,spec ,doc ,@args)
     (defvar ,face ',face )))

(tlfdd-defvar tlfdd-mode-map
  (let ((map (make-sparse-keymap)))
    ;; ...
    map)
  "Keymap for `tlfdd-mode'.")

(tlfdd-defvar tlfdd-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_  "w"   st) ; _ as a word constituent
    (modify-syntax-entry ?\" "."   st) ; ; " as a simple punctuation
    (modify-syntax-entry ?`  "\""  st) ; `tt`
    (modify-syntax-entry ?$  "\" 1234"  st) ; $tt$ and $$verbatim$$
    
    ;; double-slash single line comments
    (modify-syntax-entry ?/ ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for `tlfdd-mode'.")

(tlfdd-defvar tlfdd-imenu-generic-expression
  nil
  "The regex pattern to use for creating a buffer index.")

(defun tlfdd-create-index ()
  nil)

(tlfdd-defvar tlfdd-outline-regexp
  nil
  "Regular expression to match the beginning of a heading.")

(tlfdd-defvar tlfdd-list-rx
  (rx (* whitespace)
      (group (seq (any "+-") (* whitespace)))
      (* nonl))
  "Regular expression to match a list item.")

(tlfdd-defvar tlfdd-paragraph-start
  (rx (* whitespace) (group (or "@" "-" "+" "[" "%" "$$")))
  "Regexp for beginning of a line that starts OR separates paragraphs.")

(tlfdd-defvar tlfdd-paragraph-separate
  (rx (* whitespace) (? "$$") eol)
  "Regexp for beginning of a line that separates paragraphs.")

(tlfdd-defvar tlfdd-markups
  '(("@document" :type block :end "@end" :args any)
    ("@view" :type block :end "@end")
    ("@introduction" :type block :end "@end")
    ("@validation" :type block :end "@end")
    ("@title" :type line :args any)
    ("@label" :type line :args any)
    ("@date" :type line :args any)
    ("@header" :type line :args any)
    ("@docproperty" :type line :args aff)
    ("@role" :type line :args any)
    ("@name" :type line :args any)
    ("@chapter" :type line :args any)
    ("@inline" :type line :args spec)
    ("@copyright" :type line :args any)
    ("@localtoc" :type single)
    ("@pages" :type single)
    ("@annex" :type single)
    ("@end" :type single)
    ("#include" :type prepro)
    ("#define" :type prepro)
    ("#ifdef" :type prepro)
    ("#ifndef" :type prepro)
    ("#if" :type prepro)
    ("#elif" :type prepro)
    ("#else" :type prepro)
    ("#endif" :type prepro)
    ("#error" :type prepro)
    ("#undef" :type prepro)
    ("#pragma" :type prepro))
  "List of supported markups, and associated types.")

(defmacro tlfdd--get-markups-filter (&rest filters)
  (let ((members
         (loop for f in filters collect
               (list 'eq (cadr f) `(plist-get (cdr markup) ,(car f))))))
    `(delete nil
             (loop for markup in tlfdd-markups collect 
                   (when (and ,@members) 
                     (car markup))))))

(tlfdd-defvar tlfdd-block-markups
  (tlfdd--get-markups-filter (:type 'block)))

(tlfdd-defvar tlfdd-prepro-markups
  (tlfdd--get-markups-filter (:type 'prepro)))

(tlfdd-defvar tlfdd-args-markups
  (tlfdd--get-markups-filter (:args 'any)))

(tlfdd-defvar tlfdd-spec-markups
  (tlfdd--get-markups-filter (:args 'spec)))

(tlfdd-defvar tlfdd-aff-markups
  (tlfdd--get-markups-filter (:args 'aff)))

(tlfdd-defvar tlfdd-simple-markups
  (tlfdd--get-markups-filter (:args nil)))

(tlfdd-defvar tlfdd-end-markups
  (delete-duplicates
   (delete nil
           (loop for markup in tlfdd-markups collect 
                 (plist-get (cdr markup) :end)))
   :test 'equal))

(tlfdd-defvar tlfdd-indent-step 2)

(tlfdd-defface tlfdd-preprocessor-command-face
  '((default (:inherit font-lock-preprocessor-face)))
  "Face used for tlfdd preprocessor commands"
  :group 'tlfdd)

(tlfdd-defface tlfdd-preprocessor-arg-face
  '((default (:inherit font-lock-string-face)))
  "Face used for tlfdd preprocessor arguments"
  :group 'tlfdd)

(tlfdd-defface tlfdd-markup-face
  '((default (:inherit font-lock-builtin-face)))
  "Face used for tlfdd markups"
  :group 'tlfdd)

(tlfdd-defface tlfdd-argument-face
  '((default (:inherit font-lock-function-name-face)))
  "Face used for tlfdd markups"
  :group 'tlfdd)

(tlfdd-defface tlfdd-operator-face
  '((default (:inherit font-lock-comment-delimiter-face :slant normal)))
  "Face used for tlfdd operators"
  :group 'tlfdd)

(tlfdd-defface tlfdd-constant-face
  '((default (:inherit font-lock-constant-face)))
  "Face used for tlfdd constants"
  :group 'tlfdd)

(defun tlfdd--compute-font-lock-keywords ()
  (list
   (list
    (eval `(rx (seq (group (or ,@tlfdd-prepro-markups)) (1+ whitespace) 
                    (group (* nonl)))))
    '(1 tlfdd-preprocessor-command-face)
    '(2 tlfdd-preprocessor-arg-face))
   (list
    (eval `(rx (seq (group (or ,@tlfdd-args-markups)) (1+ whitespace) 
                    (group (* nonl)))))
    '(1 tlfdd-markup-face)
    '(2 tlfdd-argument-face))
   (list
    (eval `(rx (seq (group (or ,@tlfdd-spec-markups)))))
    '(1 tlfdd-markup-face)
    '(":" nil nil (0 tlfdd-operator-face)))
   (list
    (eval `(rx (seq (group (or ,@tlfdd-aff-markups)) (1+ whitespace)
                    (* (any alpha)) (group "=")
                    (group (* nonl)))))
    '(1 tlfdd-markup-face)
    '(2 tlfdd-operator-face)
    '(3 tlfdd-constant-face))
   (list
    (eval `(rx (seq (group (or ,@tlfdd-simple-markups)))))
    '(1 tlfdd-markup-face))
   (list (rx (seq (group "[") (group (1+ (not (any "]")))) (group "]"))) 
         '(1 tlfdd-operator-face)
         '(2 tlfdd-constant-face)
         '(3 tlfdd-operator-face))
   (list (rx (seq (group "{") (group (1+ (not (any "}")))) (group "}"))) 
         '(1 tlfdd-operator-face)
         '(2 tlfdd-constant-face)
         '(3 tlfdd-operator-face))
   (list (rx (seq (group "<u:") (group (1+ (not (any ">")))) (group ">"))) 
         '(1 tlfdd-operator-face)
         '(2 tlfdd-constant-face)
         '(3 tlfdd-operator-face))))

(tlfdd-defvar tlfdd-font-lock-keywords
  (tlfdd--compute-font-lock-keywords)
  "Keyword highlighting specification for `tlfdd-mode'.")

;;; Indentation

(defun tlfdd-indent-line ()
  "Indent current line."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (tlfdd-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun tlfdd-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (equal (line-beginning-position) 0) 0
      (let ((not-indented t) 
            (start-rx (eval `(rx (seq (0+ whitespace) (or ,@tlfdd-block-markups)))))
            (end-rx (eval `(rx (seq (0+ whitespace) (or ,@tlfdd-end-markups)))))
            cur-indent)
        (if (looking-at end-rx)
            (progn
              (save-excursion
                (backward-paragraph)
                (setq cur-indent (- (current-indentation) tlfdd-indent-step)))
              (if (< cur-indent 0)
                  (setq cur-indent 0)))
          (save-excursion 
            (let ((at-item (looking-at tlfdd-list-rx))) 
              (while not-indented
                (backward-paragraph)
                (cond ((looking-at end-rx) 
                       (setq cur-indent (current-indentation))
                       (setq not-indented nil))
                      ((looking-at start-rx) 
                       (setq cur-indent (+ (current-indentation) tlfdd-indent-step))
                       (setq not-indented nil))
                      ((looking-at tlfdd-list-rx) 
                       (setq cur-indent 
                             (if at-item (current-indentation)
                               (+ (current-indentation) 
                                  (- (match-end 1) (match-beginning 1)))))
                       (setq not-indented nil))
                      ((bobp)
                       (setq cur-indent 0)
                       (setq not-indented nil)))))))
        cur-indent))))

(defun tlfdd-fill-paragraph-function (&rest ignore)
  (save-excursion
    (mark-paragraph)
    (when (looking-at (rx (* whitespace) eol))
      (forward-line 1))
    (skip-syntax-forward " ")
    (when (looking-at (rx "$$"))
        (forward-line 1))
    (fill-region (point) (mark))))

;;;###autoload
(define-derived-mode tlfdd-mode fundamental-mode "TLFDD"
  "A major mode for editing Sample files."
  :syntax-table tlfdd-mode-syntax-table
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-start-skip) "//+\\s-*")
  (set (make-local-variable 'font-lock-defaults)
       '(tlfdd-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'tlfdd-indent-line)
  (set (make-local-variable 'imenu-generic-expression)
       tlfdd-imenu-generic-expression)
  (set (make-local-variable 'imenu-create-index-function)
       'tlfdd-create-index)
  (set (make-local-variable 'outline-regexp) tlfdd-outline-regexp)
  (set (make-local-variable 'paragraph-start) tlfdd-paragraph-start)
  (set (make-local-variable 'paragraph-separate) tlfdd-paragraph-separate)
  (set (make-local-variable 'fill-paragraph-function) 'tlfdd-fill-paragraph-function))

(provide 'tlfdd)
;;; tlfdd.el ends here

;;; Local Variables:
;;; eval: (font-lock-add-keywords nil '(("\\<\\(tlfdd-def\\(?:var\\|face\\)\\)\\>[      ']*\\(\\sw+\\)?" (1 font-lock-keyword-face) (2 font-lock-constant-face nil t))))
;;; End:
