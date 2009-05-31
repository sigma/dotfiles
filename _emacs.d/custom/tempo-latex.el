;;; tempo-latex.el --- Templates for LaTeX with help of tempo.el and AucTeX

;; Author: Guy Yeterian
;;
;; Created: 01 July 1998
;; Version: 1.0
;; Version: 1.1 January 99
;; Keywords: Templates for LaTeX Mode with auctex

;; Copyright (C) 1998  GYe

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;  This package is for use with AucTeX. You can add your TeX macros or add
;;  template for AucTeX.
;;  In version 1.1 we modify (with help of advice.el) tempo-insert. This
;;  function can now run TeX-insert-macro (look at the code for see how to do
;;  this.
;;

;;

;;; Description:
;; tempo-latex.el makes it easier to write LaTeX documents. This mode handles
;; inserting LaTeX codes in a variety of ways (keybindings, completion in the
;; buffer, running auctex functions both for command and environment ).

(defvar latex-tempo-env-tags nil
  "List of LaTeX environments tags.")

(defvar latex-tempo-com-tags nil
  "List of LaTeX commands tags.")


;;; latex-mode-hook
(require 'tempo)
(require 'advice)
(require 'mycompletion)

(setq-default
 tempo-match-finder "\\b\\([^\b]+\\)\\=")  ;; The definition in tempo.el is false.

;;; install latex-mode and define list for environements and commands.
(add-hook 'LaTeX-mode-hook
	  '(lambda ()
	     (tempo-use-tag-list 'latex-tempo-env-tags)
	     (tempo-use-tag-list 'latex-tempo-com-tags)
	     ))

(add-hook 'LaTeX-math-mode-hook
 '(lambda ()
    (tempo-use-tag-list 'latex-tempo-env-tags)
    (tempo-use-tag-list	'latex-tempo-com-tags)
    ))

;;
;; modification of tempo for running auctex function
(defadvice tempo-insert (before run-function (element on-region))
  "run (auctex) lisp  function"
  (if (and (consp element)
	   (eq (car element) 'f))
      (progn
	(apply (nth 1 element) (nthcdr 2 element))
	(ad-set-arg 0 nil))))

;;
;; activate the new tempo-insert
(ad-activate 'tempo-insert t)


;;; LaTeX function uses later

;;
;; Insert a environment of the amsthm package.
;; pour completer mes macros persos sous amsthm
(defun LaTeX-env-thm (environment)
  "Insert ENVIRONMENT with optional argument
   usage with AMSTHM ."
  (let ((titre (read-string "Title : ")))
    (LaTeX-insert-environment environment
			      (concat
			       (if (not (zerop(length titre)))
				   (format "[%s]" titre))))))


;;
;;  tempo-user-elements
(defvar tempo-user-elements nil)


;;; tempo function's here.

;;
;;; function to construct the environments templates.
(defun latex-tempo-env (l)
  "Construct tempo-template for LaTeX environment"
  (let* ((tag (car l))
	 (nom (nth 1 l))
	 (element (nth 2 l)))
    (tempo-define-template nom element tag  nil 'latex-tempo-env-tags)))


;;; function to construct LaTeX commands
(defun latex-tempo-com (l)
  "Construct tempo-template for LaTeX commands."
  (let* ((tag (car l))
	 (element (nth 1 l)))
    (tempo-define-template tag element tag nil 'latex-tempo-com-tags)))




;;; The templates.

;;; here you install your templates for environments
;;; LaTeX environments tags
(mapcar
 'latex-tempo-env
 '(("enu" "enumerate"    ((LaTeX-env-item "enumerate")))
   ("ite" "itemize"      ((LaTeX-env-item "itemize")))
   ("des"  "description" ((LaTeX-env-item "description")))
   ("dis" "displaymath" ((LaTeX-environment-menu "displaymath")))
   ("equ" "equation"    ((LaTeX-environment-menu "equation")))
   ("mul" "multicols"   ((LaTeX-environment-menu "multicols")))
   ("rem" "remarque"    ((LaTeX-environment-menu "remarque")))
   ("eno" "enonce"      ((LaTeX-environment-menu "enonce")))
   ("the" "theorem"     ((LaTeX-environment-menu "theorem")))
   ("pro" "proposition" ((LaTeX-environment-menu "proposition")))
   ("def"  "definition" ((LaTeX-environment-menu "definition")))
   ("cor"  "corollaire" ((LaTeX-environment-menu "corollaire")))
   ("tab"  "tabular"    ((LaTeX-environment-menu "tabular")))
   ("cen"  "center"     ((LaTeX-environment-menu "center")))
   ));;Here LaTeX environments tags



;;; Here you install your LaTeX commands tags
(mapcar
 'latex-tempo-com
 '(("slide" ("\\begin{slide}{" (p "title: ") "}" n> p n "\\end{slide}"))
   ("int1"  ("\\int_{" (p "liminf:") "}^{" (p "limsup:") "}" p
             "\\;d"(p "var :")))
   ("ab" ("\\abs{"p"}"))
   ("df" ("\\dfrac{"p"}{"p"}"))
   ("no" ("\\norm{"p"}"))
   ("bi" ("\\binom{"p"}{"p"}"))
   ("fr" ("\\frac{"p"}{"p"}"))
   ("sq" ("\\sqrt{"p"}"))
   ("su" ("\\sum_{"p"}^{"p"}"))
   ("lr" ("\\left("p"\\right)"))
   ("lr5" ("\\left["p"\\right]"))
   ("lr4" ("\\left{"p"\\right}"))
   ("er" ("$\\R$"p))
   ("ez" ("$\\Z$"p))
   ("eq" ("$\\Q$"p))
   ("ec" ("$\\C$"p))
   ("en" ("$\\N$"p))
   ("ek" ("$\\K$"p))
   ("se" ("\\set{"p"}"))
   ("ov" ("\\overline{"p"}"))
   ("db" ("\\dbinom{"p"}{"p"}"))
   ("te" ("\\text{"p"}"))
   ("ve" ("\\vecteur{"p"}"))
   ("mq" ("montrer que"))
   ("Mq" ("Montrer que"))
   ("cr" ("courbe représentative"))
   ("Et" ("\\'Etudier"))
   ("rep2" ("$(O,\\vec{i},\\vec{j}$"))
   ("rep3" ("$(O,\\vec{i},\\vec{j},\\vec{k}$"))
   ("got" ("\\textgoth{"p"}"))
   ("fra" ("\\textfrak{"p"}"))
   ("ssi" ("si et seulement si "))
   ("ou"  ("o\\`u"))
   ("ro"  ("repère orthonormé "))
   ("act" ((act)))
   ("lv"  ((f TeX-insert-macro "limv")))
   ("ca"  ((f TeX-insert-macro "card")))
   ("fr"  ((f TeX-insert-macro "frac")))
   ));;Here LaTeX commands tags


;; some local key.
;; The most important is f3 to complete your tag's.
(add-hook 'LaTeX-mode-hook
  (function
   (lambda ()
     (define-key LaTeX-mode-map [C-M-right] 'tempo-forward-mark)
     (define-key LaTeX-mode-map [C-M-left]  'tempo-backward-mark)
     (define-key LaTeX-mode-map [f3] 'tempo-complete-tag))))

(provide 'tempo-latex)
;;; latex-tempo ends here
