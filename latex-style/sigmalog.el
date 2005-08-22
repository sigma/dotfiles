;;; sigmalog.el ---

;; Copyright (C) 2005  Free Software Foundation, Inc.

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

(defun LaTeX-env-sigmalog (environment)
  (LaTeX-insert-environment environment)
  (end-of-line 0)
  (delete-char 1)
  (delete-horizontal-space)
  (newline-and-indent))

(defun LaTeX-sigmalog-install-syntactic-keywords ()
  (add-to-list 'font-latex-verbatim-environments-local
               "sigmalog")
  (font-latex-set-syntactic-keywords))

(TeX-add-style-hook
 "sigmalog"
 (lambda ()
   (LaTeX-sigmalog-install-syntactic-keywords)
   (LaTeX-add-environments
    '("sigmalog" LaTeX-env-sigmalog))))

(provide 'sigmalog)
;;; sigmalog.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
