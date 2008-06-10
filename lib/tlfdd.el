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

;;; Install :

;; (require 'tlfdd)
;; (add-to-list 'auto-mode-alist '("\\.fdd\\'" . tlfdd-mode))
;; (add-to-list 'auto-mode-alist '("\\.tlfdd\\'" . tlfdd-mode))
;; (add-to-list 'auto-mode-alist '("\\.tldoc\\'" . tlfdd-mode))

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

(tlfdd-defvar tlfdd-outline-regexp
  nil
  "Regular expression to match the beginning of a heading.")

(tlfdd-defvar tlfdd-list-rx
  (rx (and (* space)
	   (group (and (any "+-") (* space)))
	   (* not-newline)))
  "Regular expression to match a list item.")

(tlfdd-defvar tlfdd-paragraph-start
  (rx (and (* space) (group (or "#" "@" "-" "+" "[" "%" "$$" (and "<" (* (any "/a-zA-Z")) ">")))))
  "Regexp for beginning of a line that starts OR separates paragraphs.")

(tlfdd-defvar tlfdd-paragraph-separate
  (rx (and (* space) (or (and (? "$$") line-end)
                         (and (any "@") (* not-newline))
                         (and "<" (* (any "/a-zA-Z")) ">"))))
  "Regexp for beginning of a line that separates paragraphs.")


(tlfdd-defvar tlfdd-markups
  '(
    ("#define" :type prepro)
    ("#elif" :type prepro :align ("#if" "#ifdef" "#ifndef"))
    ("#else" :type prepro :align ("#if" "#ifdef" "#ifndef"))
    ("#endif" :type prepro :align ("#if" "#ifdef" "#ifndef") :close t)
    ("#error" :type prepro)
    ("#if" :type prepro)
    ("#ifdef" :type prepro)
    ("#ifndef" :type prepro)
    ("#include" :type prepro)
    ("#pragma" :type prepro)
    ("#undef" :type prepro)
    ;;DOCUMENTATION MARKUPS
    ("@abstract" :type block :end ("@end" "@endabstract") :args any)
    ("@annex" :type line :indent 1 :toc inhibit)
    ("@batch" :type block :end ("@end" "@endbatch") :args any)
    ("@add" :type line :args any)
    ("@begin" :type block :end "@end" :container t)
    ("@chapter" :type line :args any :indent 1)
    ("@content" :type block :end ("@end" "@endcontent") :args any :container t)
    ("@copyright" :type line :args any :indent 1 :toc inhibit)
    ("@date" :type line :args any)
    ("@diffusionlist" :type block :end ("@end" "@enddiffusionlist") :args any)
    ("@docproperty" :type line :args aff)
    ("@document" :type block :end ("@end" "@enddocument") :args any :indent 0)
    ("@example" :type block :end ("@end" "@endexample") :args any)
    ("@figure" :type block :end ("@end" "@endfigure") :args any)
    ("@header" :type line :args any)
    ("@index" :type line :args any)
    ("@inline" :type line :args spec)
    ("@inlinesection" :type line :args spec :indent =)
    ("@inlinechapter" :type line :args spec :indent =)
    ("@inlineshort" :type line :args spec)
    ("@introduction" :type line :indent 1)
    ("@label" :type line :args any)
    ("@localtoc" :type single)
    ("@logo" :type single)
    ("@main" :type line :args any)
    ("@name" :type line :args any)
    ("@pages" :type single)
    ("@role" :type line :args any)
    ("@section" :type line :args any :indent 2)
    ("@subsection" :type line :args any :indent 3)
    ("@subsubsection" :type line :args any :indent 4)
    ("@sysproperty" :type line :args any :indent 1 :toc inhibit)
    ("@table" :type block :end ("@end" "@endtable") :args any)
    ("@title" :type line :args any)
    ("@toc" :type line :args any)
    ("@topic" :type block :end ("@end" "@endtopic") :args any)
    ("@validation" :type block :end ("@end" "@endvalidation"))
    ("@view" :type block :end ("@end" "@endview") :args single :container t)
    ("@window" :type line :args any)
    ;;DEFINITION MARKUPS
    ("@apdu" :type block :end ("@end" "@endapdu") :args any)
    ("@external" :type block :end ("@end" "@endexternal") :args any)
    ("@objective" :type block :end ("@end" "@endobjective") :args any :container t)
    ("@requirement" :type block :end ("@end" "@endrequirement") :args any :container t)
    ("@feature" :type block :end ("@end" "@endfeature") :args any :container t)
    ("@system" :type block :end ("@end" "@endsystem") :args any :container t)
    ("@construct" :type block :end ("@end" "@endconstruct") :args any)
    ("@module" :type block :end ("@end" "@endmodule") :args any :container t)
    ("@subobjective" :type line :args any)
    ("@subrequirement" :type line :args any)
    ("@subfeature" :type line :args any)
    ("@subsystem" :type line :args any)
    ("@level" :type line :args any)
    ("@state" :type block :end ("@end" "@endstate") :args any)
    ("@base" :type line :args any)
    ("@service" :type block :end ("@end" "@endservice") :args any)
    ("@security" :type single)
    ("@interface" :type line :args any)
    ("@rationale" :type block :end ("@end" "@endrationale") :args any)
    ("@coverage" :type line :args any)
    ("@testcoverage" :type line :args any)
    ("@testgoal" :type block :end ("@end" "@endtestgoal") :args any :container t)
    ("@target" :type line :args any)
    ("@test" :type block :end ("@end" "@endtest") :args any :container t)
    ("@pre" :type line :args any)
    ("@post" :type line :args any)
    ("@param" :type line :args any)
    ("@procedure" :type line :args any)
    ("@return" :type line :args any)
    ("@impl" :type line :args any)
    ("@testcase" :type block :end ("@end" "@endtestcase") :args any :container t)
    ("@private" :type single)
    ("@url" :type line :args any)
    ("@concept" :type single)
    ("@author" :type line :args any)
    ("@version" :type line :args any)
    ("@date" :type line :args any)
    ("@class" :type line :args any)
    ("@status" :type line :args any)
    ("@acronym" :type line :args any)
    ("@import" :type line :args any)
    ("@constraint" :type line :args any)
    ("@depend" :type line :args any)
    ("@refine" :type line :args any)
    ("@implement" :type line :args any)
    ("@parent" :type line :args any)
    ("@see" :type line :args any)
    ("@references" :type line :args any)
    ("@alias" :type line :args any)
    ;;XML MARKUPS
    ("<pre>" :type block :end "</pre>")
    ("<div>" :type block :end "</div>")
    ("<ul>" :type block :end "</ul>")
    ("<ol>" :type block :end "</ol>"))
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

(tlfdd-defvar tlfdd-single-markups
  (tlfdd--get-markups-filter (:args 'single)))

(tlfdd-defvar tlfdd-container-markups
  (tlfdd--get-markups-filter (:container t)))

(tlfdd-defvar tlfdd-implicit-block-markups
  (delete nil
          (loop for markup in tlfdd-markups collect 
                (when (and (plist-get (cdr markup) :indent)
                           (equal (plist-get (cdr markup) :type) 'line))
                  (car markup)))))

(tlfdd-defvar tlfdd-toc-markups
  (delete nil
          (loop for markup in tlfdd-markups collect 
                (when (plist-get (cdr markup) :indent)
                  (car markup)))))

(tlfdd-defvar tlfdd-prepro-unindent-markups
  (delete nil
          (loop for markup in tlfdd-markups collect 
                (when (and (equal (plist-get (cdr markup) :type)
                                  'prepro) 
                           (plist-get (cdr markup) :align))
                  (car markup)))))

(tlfdd-defvar tlfdd-prepro-end-markups
  (delete nil
          (loop for markup in tlfdd-markups collect 
                (when (and (equal (plist-get (cdr markup) :type)
                                  'prepro) 
                           (plist-get (cdr markup) :close))
                  (car markup)))))

(tlfdd-defvar tlfdd-prepro-skip-markups
  (delete nil
          (loop for markup in tlfdd-markups collect 
                (when (and (equal (plist-get (cdr markup) :type)
                                  'prepro) 
                           (plist-get (cdr markup) :align)
                           (not (plist-get (cdr markup) :close)))
                  (car markup)))))

(defun tlfdd--flatten-list (list)
  (cond ((consp list)
	 (apply 'append (mapcar 'tlfdd--flatten-list list)))
	(list
	 (list list))))

(tlfdd-defvar tlfdd-prepro-block-markups
  (delete-duplicates
   (tlfdd--flatten-list 
    (delete nil
            (loop for markup in tlfdd-markups collect 
                  (when (equal (plist-get (cdr markup) :type) 'prepro)
                    (plist-get (cdr markup) :align)))))
   :test 'equal))

(tlfdd-defvar tlfdd-end-markups
  (delete-duplicates
   (tlfdd--flatten-list 
    (delete nil
            (loop for markup in tlfdd-markups collect 
                  (plist-get (cdr markup) :end))))
   :test 'equal))

(tlfdd-defvar tlfdd-simple-markups
  (append
   tlfdd-end-markups
   (tlfdd--get-markups-filter (:args nil))))

(tlfdd-defvar tlfdd-indent-step 2)

(tlfdd-defface tlfdd-preprocessor-command-face
  '((t (:inherit font-lock-warning-face)))
  "Face used for tlfdd preprocessor commands"
  :group 'tlfdd)

(tlfdd-defface tlfdd-preprocessor-arg-face
  '((t (:inherit font-lock-string-face)))
  "Face used for tlfdd preprocessor arguments"
  :group 'tlfdd)

(tlfdd-defface tlfdd-markup-face
  '((t (:inherit font-lock-builtin-face)))
  "Face used for tlfdd markups"
  :group 'tlfdd)

(tlfdd-defface tlfdd-argument-face
  '((t (:inherit font-lock-function-name-face)))
  "Face used for tlfdd markups"
  :group 'tlfdd)

(tlfdd-defface tlfdd-operator-face
  '((t (:inherit font-lock-comment-delimiter-face :slant normal)))
  "Face used for tlfdd operators"
  :group 'tlfdd)

(tlfdd-defface tlfdd-constant-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for tlfdd constants"
  :group 'tlfdd)

(defun tlfdd--looking-at (regexp)
  (let ((case-fold-search t))
    (looking-at regexp)))

(defun tlfdd--re-search-backward (regexp &optional bound noerror count)
  (let ((case-fold-search t))
    (re-search-backward regexp bound noerror count)))

(defun tlfdd--re-search-forward (regexp &optional bound noerror count)
  (let ((case-fold-search t))
    (re-search-forward regexp bound noerror count)))

(defvar tlfdd--position nil)

(defun tlfdd--store-position ()
  (setq tlfdd--position (point)))

(defun tlfdd--restore-position ()
  (goto-char tlfdd--position)
  (setq tlfdd--position nil))

(defun tlfdd--compute-font-lock-keywords ()
  (list
   (list
    (eval `(rx (and (group (or ,@tlfdd-prepro-markups)) word-end
                    (group (* not-newline)))))
    '(1 tlfdd-preprocessor-command-face)
    '(2 tlfdd-preprocessor-arg-face))
   (list
    (eval `(rx (and (group (or ,@tlfdd-args-markups)) word-end
                    (group (* not-newline)))))
    '(1 tlfdd-markup-face)
    '(2 tlfdd-argument-face))
   (list
    (eval `(rx (and (group (or ,@tlfdd-single-markups)) word-end
                    (group (* not-newline)))))
    '(1 tlfdd-markup-face)
    '(2 tlfdd-constant-face))
   (list
    (eval `(rx (and (group (or ,@tlfdd-spec-markups)))))
    '(1 tlfdd-markup-face)
    '(":" nil nil(0 tlfdd-operator-face)))
   (list
    (eval `(rx (and (group (or ,@tlfdd-aff-markups)) (1+ space)
                    (group (* (any "a-zA-Z"))) 
                    (group "=")
                    (group (* not-newline)))))
    '(1 tlfdd-markup-face)
    '(2 tlfdd-argument-face)
    '(3 tlfdd-operator-face)
    '(4 tlfdd-constant-face))
   (list
    (eval `(rx (and (group (or ,@tlfdd-simple-markups)))))
    '(1 tlfdd-markup-face))
   (list (rx (and (group "[") (group (1+ (not (any "]")))) (group "]"))) 
         '(1 tlfdd-operator-face t)
         '(2 tlfdd-constant-face t)
         '(3 tlfdd-operator-face t))
   (list (rx (and (group "{") (group (1+ (not (any "}")))) (group "}"))) 
         '(1 tlfdd-operator-face t)
         '(2 tlfdd-constant-face t)
         '(3 tlfdd-operator-face t))
   (list (rx (and (group (and "<" (? (or "u:" "/")))) 
                  (group (1+ (not (any "\n>")))) 
                  (group (and (? "/") ">")))) 
         '(1 tlfdd-operator-face t)
         '(2 tlfdd-constant-face t)
         '(3 tlfdd-operator-face t))))

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

(tlfdd-defvar tlfdd--implicit-block-rx 
  (eval `(rx (and (0+ space) (group (or ,@tlfdd-implicit-block-markups))))))

(tlfdd-defvar tlfdd--toc-rx 
  (eval `(rx (and (0+ space) (group (or ,@tlfdd-toc-markups))))))

(tlfdd-defvar tlfdd--start-rx
  (eval `(rx (and (0+ space) (or ,@tlfdd-block-markups ,@tlfdd-implicit-block-markups)))))

(tlfdd-defvar tlfdd--end-rx
  (eval `(rx (and (0+ space) (or ,@tlfdd-end-markups)))))

(tlfdd-defvar tlfdd--container-rx
  (eval `(rx (and (0+ space) (or ,@tlfdd-container-markups)))))

(tlfdd-defvar tlfdd--prepro-unindent-rx
  (eval `(rx (and (0+ space) (or ,@tlfdd-prepro-unindent-markups)))))

(tlfdd-defvar tlfdd--prepro-end-rx
  (eval `(rx (and (0+ space) (or ,@tlfdd-prepro-end-markups)))))

(defun tlfdd--internal-indentation ()
  (save-excursion 
    (let ((not-indented t) 
          (at-item (tlfdd--looking-at tlfdd-list-rx))
          (at-prepro (tlfdd--looking-at (rx (and (* space) "#"))))
          (init-point (point))
          cur-indent)
      (while not-indented
        (tlfdd--re-search-backward (concat "^" tlfdd-paragraph-start))
        (cond ((tlfdd--looking-at tlfdd--prepro-end-rx)
               (tlfdd--goto-matching-block-start t)) 
              ((tlfdd--looking-at tlfdd--prepro-unindent-rx)
               nil)
              ((tlfdd--looking-at tlfdd--end-rx) 
               (setq cur-indent (current-indentation))
               (setq not-indented nil))
              ((tlfdd--looking-at tlfdd--start-rx) 
               (setq cur-indent (+ (current-indentation) tlfdd-indent-step))
               (setq not-indented nil))
              ((tlfdd--looking-at tlfdd-list-rx) 
               (setq cur-indent 
                     (if (or at-item
                             at-prepro
                             (save-excursion
                               (re-search-forward "^[[:space:]]*$" init-point t))) 
                         (current-indentation)
                       (+ (current-indentation) 
                          (- (match-end 1) (match-beginning 1)))))
               (setq not-indented nil))
              ((bobp)
               (setq cur-indent 0)
               (setq not-indented nil))
              (t
               (setq cur-indent (current-indentation))
               (setq not-indented nil))))
      cur-indent)))

(defun tlfdd--get-toc-level (markup)
  (plist-get (cdr (assoc (downcase markup) tlfdd-markups)) :indent))

(defun tlfdd--goto-matching-block-start (&optional prepro)
  (let ((start-markups (if prepro tlfdd-prepro-block-markups tlfdd-block-markups))
        (skip-markups (if prepro tlfdd-prepro-skip-markups))
        (end-markups (if prepro tlfdd-prepro-unindent-markups tlfdd-end-markups)))
    (let ((start-rx (eval `(rx (and line-start (* space) (or ,@start-markups)))))
          (end-rx (eval `(rx (and line-start (* space) (or ,@end-markups)))))
          (both-rx (eval `(rx (and line-start (* space) (or ,@start-markups ,@end-markups)))))
          (skip-rx (when skip-markups (eval `(rx (and line-start (* space) (or ,@skip-markups))))))
          (found nil))
      (while (not found)
        (tlfdd--re-search-backward both-rx)
        (cond ((tlfdd--looking-at start-rx)
               (setq found t))
              ((and skip-rx (tlfdd--looking-at skip-rx))
               nil)
              ((tlfdd--looking-at end-rx)
               (tlfdd--goto-matching-block-start prepro)))))))

(defun tlfdd--in-pre-p ()
  (or (nth 4 (syntax-ppss))
      (let ((open (save-excursion (tlfdd--re-search-backward (rx "<pre>") nil t)))
            (close (save-excursion (tlfdd--re-search-backward (rx "</pre>") nil t))))
        (and open 
             (or (not close)
                 (< close open))))))

(defun tlfdd--implicit-block-indent (markup)
  (save-excursion
    (let ((level (tlfdd--get-toc-level markup))
          (found nil)
          (indent 0))
      (while (not found)
        (if (tlfdd--re-search-backward (concat "^" tlfdd--toc-rx "\\|" tlfdd--end-rx "\\|" tlfdd--container-rx))
            (cond ((nth 4 (syntax-ppss)) 
                   (beginning-of-line))
                  ((tlfdd--looking-at tlfdd--end-rx)
                   (tlfdd--goto-matching-block-start))
                  ((tlfdd--looking-at tlfdd--container-rx)
                   (setq found t 
                         indent (+ (current-indentation) tlfdd-indent-step)))
                  (t (let ((l (tlfdd--get-toc-level (match-string 1))))
                       (cond ((eq l '=)
                              nil)
                             ((eq level '=)
                              (setq found t indent (current-indentation)))
                             ((> l level)
                              nil) 
                             (t
                              (setq found t indent (+ (current-indentation)
                                                      (if (= l level) 0 tlfdd-indent-step))))))))
          (setq found t)))
      indent)))

(defun tlfdd-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond ((equal (line-beginning-position) 0) 
           0)
          ((tlfdd--in-pre-p)
           (cond ((looking-at (rx (and (* space) "$$")))
                  (re-search-backward (rx "$$") nil t)
                  (current-indentation))
                 ((tlfdd--looking-at (rx "</pre>"))
                  (tlfdd--re-search-backward (rx "<pre>") nil t)
                  (current-indentation))
                 ((not (looking-at (rx (and (* space) line-end))))
                  (current-indentation))
                 (t
                  (forward-line -1)
                  (current-indentation)))) 
          ((tlfdd--looking-at tlfdd--prepro-unindent-rx)
           (tlfdd--goto-matching-block-start t)
           (current-indentation))
          (t
           (let (cur-indent)
             (cond ((tlfdd--looking-at tlfdd--end-rx)
                    (progn
                      (save-excursion
                        (tlfdd--goto-matching-block-start) 
                        (setq cur-indent (current-indentation)))))
                   ((tlfdd--looking-at tlfdd--implicit-block-rx)
                    (setq cur-indent (tlfdd--implicit-block-indent (match-string 1))))
                   (t 
                    (setq cur-indent (tlfdd--internal-indentation))))
             (if (< cur-indent 0)
                 0
               cur-indent))))))

(defun tlfdd-fill-paragraph-function (&rest ignore)
  (save-excursion
    (mark-paragraph)
    (when (looking-at (rx (* whitespace) eol))
      (forward-line 1))
    (skip-syntax-forward " ")
    (when (tlfdd--looking-at (rx (or "$$" "<pre>")))
      (forward-line 1))
    (let ((point (point))
             indented
             tmp)      
         (while (setq tmp (re-search-forward ";;" (mark) t))
           (setq indented (looking-at "$"))
           (fill-region point tmp)
           (if indented
               (setq point tmp)
             (backward-char)
             (newline-and-indent)
             (setq point (1+ tmp))))
         (fill-region point (mark)))))

(defun tlfdd-create-index ()
  (let ((toc-rx (eval `(rx (and (group (or ,@tlfdd-toc-markups))
                                (group (* not-newline))))))
        toc)
    (save-excursion
      (goto-char (point-min))
      (while (tlfdd--re-search-forward toc-rx nil t)
        (let* ((s (match-string 0))
               (markup (match-string 1))
               (prop (/ (current-indentation) tlfdd-indent-step)))
          (set-text-properties 0 (length s) nil s)
          (unless (eq (plist-get (cdr (assoc markup tlfdd-markups)) :toc) 'inhibit)
            (push (cons (concat (make-string prop ?\ ) s) (point)) toc)))
        (end-of-line)))
    (reverse toc)))

;;;###autoload
(define-derived-mode tlfdd-mode fundamental-mode "TLFDD"
  "A major mode for editing Sample files."
  :syntax-table tlfdd-mode-syntax-table
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-start-skip) "//+\\s-*")
  (set (make-local-variable 'font-lock-defaults)
       '(tlfdd-font-lock-keywords nil t))
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
