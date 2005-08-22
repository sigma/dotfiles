;;; muse-config.el --- Configuration for muse

;; Copyright (C) 2004  Free Software Foundation, Inc.

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

;;; Muse

(defconst muse-config-latex-header "\\documentclass{article}

\\usepackage[english]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}
\\usepackage[pdftex]{graphicx}

\\begin{document}

\\title{<lisp>(muse-publishing-directive \"title\")</lisp>}
\\author{<lisp>(muse-publishing-directive \"author\")</lisp>}
\\date{<lisp>(muse-publishing-directive \"date\")</lisp>}

\\maketitle

<lisp>(and muse-publish-generate-contents
	   \"\\\\tableofcontents
\\\\newpage\")</lisp>

")

(when (request 'muse-mode)
  (progn
    (when (request 'muse-html)
      (setq muse-html-charset-default "utf-8"
            muse-html-encoding-default (quote utf-8)))

    (when (request 'muse-latex)
      (setq muse-latex-header muse-config-latex-header))

    (request 'muse-texinfo)
    (request 'muse-docbook)
    (request 'muse-wiki)))

(add-to-list 'auto-mode-alist '("\\.muse$"  . muse-mode))

(defun local-file-url-transform (target &rest ignored)
  (if (and (string-match muse-file-regexp target)
           (not (or (string-match muse-url-regexp target)
                    (string-match muse-image-regexp target))))
      (expand-file-name target)
    target))

(add-to-list 'muse-publish-url-transforms 'local-file-url-transform)

(defun gnus-url-transform (target)
  (save-match-data
    (if (string-match "^gnus://" target)
        ""
      target)))

;(add-to-list 'muse-publish-url-transforms 'gnus-url-transform)

(provide 'muse-config)
;;; muse-config.el ends here
