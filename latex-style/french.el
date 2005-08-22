;;; french.el --- Setup AUCTeX for editing french text.

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Davide G. M. Salvetti <salve@debian.org>
;; Maintainer: Davide G. M. Salvetti <salve@debian.org>
;; Created: 2004-05-12
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;;; Code:

(defvar LaTeX-french-open-quote "«~"
  "Initial value of `TeX-open-quote' for `french.el'")
(defvar LaTeX-french-close-quote "~»"
  "Initial value of `TeX-close-quote' for `french.el'")
(defvar TeX-language-fr-hook nil
  "Hook run for french texts.")

(TeX-add-style-hook
 "french"
 (lambda ()
   (unless (local-variable-p 'TeX-open-quote (current-buffer))
     (set (make-local-variable 'TeX-open-quote)
	  LaTeX-french-open-quote))
   (unless (local-variable-p 'TeX-close-quote (current-buffer))
     (set (make-local-variable 'TeX-close-quote)
	  LaTeX-french-close-quote))
   (run-hooks 'TeX-language-fr-hook)))

;;; french.el ends here
