;;;; dbide-data.el --- DocBook font lock keywords
;; $Id: dbide-font.el,v 1.2 2000/03/29 19:12:16 nwalsh Exp $

;; Copyright (C) 2000 Norman Walsh
;; Based extensively on (one might go so far as to say "totally hacked
;; from") Tony Graham's xslide.

;; Author: Norman Walsh <ndw@nwalsh.com>
;; Created: 29 March 2000
;; Version: $Revision: 1.2 $
;; Keywords: languages, xml, docbook

;;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;;; Commentary:

;; Font lock mode keywords for use when editing DOCBOOK stylesheets

;; Send bugs to docbookide-bug@menteith.com


;; before we begin
(eval-and-compile
  (autoload 'make-regexp "make-regexp"))
(eval-and-compile
  (autoload 'make-regexps "make-regexp"))

;;;; Variables

;;(setq font-lock-face-attributes
;;      (append
;;       (list
;;	'(docbook-docbook-element-face "Khaki" nil nil nil)
;;	'(docbook-docbook-attribute-face "LightGray"))
;;       font-lock-face-attributes))

(defvar docbook-font-lock-face-attributes
  (list
   '(docbook-docbook-main-face "light slate gray")
   '(docbook-docbook-alternate-face "light slate gray")
   '(docbook-other-element-face "blue"))
  "*List of DOCBOOK-specific font lock faces and their attributes")

;; Something to do when we read this file
(while (car docbook-font-lock-face-attributes)
  (let ((face-attributes (car docbook-font-lock-face-attributes)))
    (setq docbook-font-lock-face-attributes
	  (cdr docbook-font-lock-face-attributes))
    (let* ((face (car face-attributes))
	   (foreground (cadr face-attributes)))
    (make-face face)
    (set-face-foreground face foreground)
    ;; The font-lock stuff requires a variable with the same name as
    ;; the face
    (set face face))))

;;;; Constants

(defvar docbook-font-lock-keywords
  (list
   ;;
   ;; Reserved XML Processing instruction
   ;;
   '(
    "\\(<\\?\\)\\(xml\\)\\(\\s-+version\\s-*=\\s-*\\('[^']+'\\|\"[^\"]+\"\\)\\)?\\(\\s-+encoding\\s-*=\\s-*\\('[^']+'\\|\"[^\"]+\"\\)\\)?\\(\\s-+standalone\\s-*=\\s-*\\('\\(yes\\|no\\)'\\|\"\\(yes\\|no\\)\"\\)\\)?\\s-*\\(\\?>\\)"
    (1 font-lock-keyword-face)
    (2 font-lock-type-face nil)
    (3 font-lock-type-face nil t)
    (5 font-lock-type-face nil t)
    (7 font-lock-type-face nil t)
    (11 font-lock-keyword-face))
   ;;
   ;; Non-reserved XML Processing instruction
   ;; Any XML PI that doesn't start with "<?xml"
   ;;
   '("\\(<\\?\\)\\([^ \t?>]+\\)[ \t]*\\([^?>]\\|\\?[^>]\\|>[^\n\r]\\)*\\(\\?>\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face)
     (4 font-lock-keyword-face))
   ;;
   ;; Entity references
   ;; These come early so entity references as the names in element, etc.
   ;; declarations retain their colour and don't get turned into
   ;; font-lock-variable-name-face.  E.g:
   ;; <!ENTITY % %entity; "..." >
   ;;
   '("[%&][^; \t]+;" . font-lock-reference-face)
   ;;
   ;; Marked section start
   ;;
   '("\\(<!\\[\\)[^[]*\\(\\[\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-keyword-face))
;   ;;
;   ;; Content of tags
;   ;;
;   (list
;    (concat ">\\([^<]+\\)<")
;    '(1 font-lock-string-face keep))
   ;;
   ;; DOCBOOK elements (group 1)
   ;;
   (list
    (concat "\\(</?\\)\\("
	    (make-regexp
	     (append
	      (mapcar 'car docbook-element-symbol-alist)
	      (mapcar 'car docbook-element-symbol-alist-1)
	      (mapcar 'car docbook-element-symbol-alist-2)
	      (mapcar 'car docbook-element-symbol-alist-3)))
	    "\\)\\(\\s-+\\([^/>]\\|/[^>]\\)+\\)*\\(/?>\\)")
    '(1 docbook-docbook-main-face)
    '(2 docbook-docbook-alternate-face))
   ;;
   ;; DOCBOOK elements (group 2)
   ;;
   (list
    (concat "\\(</?\\)\\("
	    (make-regexp
	     (append
	      (mapcar 'car docbook-element-symbol-alist-4)
	      (mapcar 'car docbook-element-symbol-alist-5)
	      (mapcar 'car docbook-element-symbol-alist-6)
	      (mapcar 'car docbook-element-symbol-alist-7)
	      (mapcar 'car docbook-element-symbol-alist-8)))
	    "\\)\\(\\s-+\\([^/>]\\|/[^>]\\)+\\)*\\(/?>\\)")
    '(1 docbook-docbook-main-face)
    '(2 docbook-docbook-alternate-face))
   (list
    (concat "</?" "\\([^/>]\\|/[^>]\\)*\\(/?>\\)")
    '(2 docbook-docbook-main-face))
   ;;
   ;; DOCBOOK attributes
   ;;
   (make-regexps "\\b"
		 (list (mapcar 'car docbook-attributes-alist)
		       docbook-docbook-alternate-face)
		 "[ \t]*"
		 '(("=[ \t]*\"") docbook-docbook-alternate-face)
		 '("\\([^\"<>]*\\)" 1 font-lock-variable-name-face)
		 '(("\"") docbook-docbook-alternate-face))
   (make-regexps "\\b"
		 (list (mapcar 'car docbook-attributes-alist)
		       docbook-docbook-alternate-face)
		 "[ \t]*"
		 '(("=[ \t]*'") docbook-docbook-alternate-face)
		 '("\\([^'<>]*\\)" 1 font-lock-variable-name-face)
		 '(("'") docbook-docbook-alternate-face))
   ;;
   ;; Mark the start and end of literals, but don't do anything to their
   ;; contents
   ;;
   '("\\('\\)[^']*\\('\\)"
     (1 font-lock-string-face)
     (2 font-lock-string-face))
   '("\\(\"\\)[^\"]*\\(\"\\)"
     (1 font-lock-string-face)
     (2 font-lock-string-face))
   ;;
   ;; Put comment patterns last so they mask anything
   ;; that might be inside the comment
   ;;
   '("\\(<!--[^-]*\\(-[^-]+\\)*-->\\)"
     (1 font-lock-comment-face t))
   )
  "Additional expressions to highlight in DOCBOOK mode.")

;;;; Code:
(defun docbook-font-lock-mark-block-function ()
  "Function to mark the area of text we want to fontify.

Used with font-lock-fontify-block.  Set font-lock-mark-block-function
to this function for this function to take effect.

This function marks the area beginning five \"<\" before point and five
\">\" at ends of lines after point.  The default without a function like
this is to fontify 16 lines before and after point, but then the region
often starts or ends partway through a comment or declaration, turning
that half white because the keywords didn't match, and it just looks so
ugly."
  (let ((current-point (point)))
    (re-search-forward ">$" (point-max) 'limit 5)
    (set-mark (point))
    (goto-char current-point)
    (re-search-backward "^<" (point-min) 'limit 5)))

(defun docbook-font-lock-region-point-min ()
  "Return the start point of the region we want to fontify"
  (save-excursion
    (re-search-backward "^<" (point-min) 'limit 5)
    (point)))

(defun docbook-font-lock-region-point-max ()
  "Return the start point of the region we want to fontify"
  (save-excursion
    (re-search-forward ">$" (point-max) 'limit 5)
    (point)))

(provide 'dbide-font)

;; end of dbide-font.el
