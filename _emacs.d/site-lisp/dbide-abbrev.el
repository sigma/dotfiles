;;;; dbide-abbrev.el --- Abbrev table definitions for docbookide
;; $Id: dbide-abbrev.el,v 1.2 2000/03/29 19:12:16 nwalsh Exp $

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

;; 

;; Send bugs to docbookide-bug@menteith.com
;; Use `docbook-submit-bug-report' to submit a bug report


;;;; Variables:

(defvar docbook-mode-abbrev-table nil
  "Abbrev table used while in DOCBOOK mode.")

;;; define docbook-mode-abbrev-table if not already defined
(if docbook-mode-abbrev-table
    ()
  ;; remember current state of abbrevs-changed so we can restore it after
  ;; defining some abbrevs
  (let ((ac abbrevs-changed))
    (define-abbrev-table 'docbook-mode-abbrev-table ())

    ;; Generate abbrevs for DOCBOOK from
    ;; data in docbook-all-elements-alist
    (mapcar (lambda (x)
	      (define-abbrev docbook-mode-abbrev-table
		(nth 3 x) (car x) nil))
	    docbook-all-elements-alist)

    ;; Generate abbrevs for attributes for DOCBOOK
    (mapcar (lambda (x)
	      (define-abbrev docbook-mode-abbrev-table
		(nth 1 x)
		(concat (car x) "=\"\"")
		'backward-char))
	    (append
	     docbook-attributes-alist
	     '()))

    ;; restore abbrevs-changed
    (setq abbrevs-changed ac)))

(provide 'dbide-abbrev)
