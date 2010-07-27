;;;; dbide-data.el --- Process document with DOCBOOK
;; $Id: dbide-process.el,v 1.2 2000/03/29 19:12:16 nwalsh Exp $

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

;; Copied almost wholesale from psgml.el by Lennart Staflin

;; Send bugs to docbookide-bug@menteith.com
;; Use `docbook-submit-bug-report' for bug reports


;;;; Variables:

(defvar docbook-offer-save t
  "*If non-nil, ask about saving modified buffers before \\[docbook-process] is run.")

(defvar docbook-process-command "java com.jclark.docbook.Driver %s %s %s"
  "*The shell command to process an DOCBOOK document.

This is a `format' control string that by default should contain two
`%s' conversion specifications: the first will be replaced by the
value of `docbook-xml-source' \(or the empty string, if nil\); the
second will be replaced by the current buffer's file name \(or the
empty string, if nil\).

If `docbook-process-files' is non-nil, the format string should contain
one `%s' conversion specification for each element of its result.

If docbook-process-command is a list, then every element should be a
string.  The strings will be tried in order and %-sequences in the
string will be replaced according to the list below, if the string contains
%-sequences with no replacement value the next string will be tried.

%b means the visited file of the current buffer
%s means the DOCBOOK declaration specified in the docbook-xml-source variable
%d means the file containing the DOCTYPE declaration, if not in the buffer 
")

(defvar docbook-process-files nil
  "If non-nil, a function of no arguments that returns a list of file names.
These file names will serve as the arguments to the `docbook-process-command'
format control string instead of the defaults.")

(defvar docbook-process-error-regexps
  '(("file:\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3)
    ("file:/\\(\\([A-Za-z]:\\)?[^:]+\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?" 1 3 5))
  "Alist of regexps to recognize error messages from `docbook-process'.
See `compilation-error-regexp-alist'.")

(defvar docbook-xml-source nil
  "*If non-nil, this is the name of the XML source file.")
(put 'docbook-xml-source 'docbook-type 'string)

(defvar docbook-docbook-result nil
  "*If non-nil, this is the name of the DOCBOOK result file.")
(put 'docbook-docbook-result 'docbook-type 'string)

(defvar docbook-process-command-history nil
  "The minibuffer history list for `docbook-process''s COMMAND argument.")

(eval-and-compile
  (autoload 'compile-internal "compile" ""))

(defun docbook-default-process-command ()
  (apply 'format docbook-process-command
	 (if docbook-process-files
	     (funcall docbook-process-files)
	   (list (or docbook-xml-source "")
		 (let ((name (buffer-file-name)))
		   (if name
		       (file-name-nondirectory name)
		     ""))
		 (or docbook-docbook-result "")))))

(defun docbook-process (command)
  "Process an DOCBOOK stylesheet.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *DOCBOOK process*.
You can then use the command \\[next-error] to find the next error message
and move to the line in the DOCBOOK document that caused it."
  (interactive
   (list (read-from-minibuffer "Process command: "
			       (docbook-default-process-command)
			       nil nil 'docbook-process-command-history)))
  (if docbook-offer-save
      (save-some-buffers nil nil))
  (compile-internal command "No more errors" "DOCBOOK process"
		    nil
		    docbook-process-error-regexps))

(provide 'dbide-process)
