;;; nnir-grepmail.el --- A grepmail plugin for nnir.el

;; Copyright (C) 2001 by Nevin Kapur

;; Author: Nevin Kapur <nevin@jhu.edu>
;; Keywords: mail, matching
;; Version: 1.4
;; X-URL: http://www.mts.jhu.edu/~kapur/emacs/nnir-grepmail.el

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; This is a plugin for nnir.el to use grepmail as its search engine.
;; To use it, put the following in your ~/.gnus:
;;
;;	(require 'nnir-grepmail)
;;	(setq nnir-search-engine 'grepmail)
;;
;; If you have only one nnml and only one nnfolder backend and they
;; are both part of your gnus-secondary-select-methods, then that
;; should be it. Otherwise, you may need to customize the variables
;; nnir-grepmail-nnml-backend and nnir-grepmail-nnfolder-backend. The
;; defaults are guessed from your gnus-secondary-select-methods which
;; may or may not be right.
;;
;; To learn more about nnir.el, read its Commentary section in
;; ftp://ls6-ftp.cs.uni-dortmund.de/pub/src/emacs/nnir.el.
;; To learn more about grepmail, visit http://grepmail.sourceforge.net.
;; If you use grepmail 4.48 or greater, you should add -H to
;; nnir-grepmail-switches. This will greatly reduce the size of the
;; buffer created by nnir.

;;; Changes:

;; o Fixed bug that could result in (expand-file-name nil). Thanks to
;;   Niels Olof Bouvin <bouvin@daimi.au.dk> for the bug report.

;; o No dependence on the Xref: header since that can be misleading.
;; o Improved error checking. I've tried to make sure that you never
;;   get an error.
;; o Fixed a bug that could cause problems if one has groups like
;;   mail.misc.
;; o Try and make a guess for the values for
;;   nnir-gremail-nn(folder|ml)-backend so that minimal customization
;;   from the user is required.

;; o We can now switch between nnml and nnfolder backends on the fly.

;;; Know bugs:

;; o If the search produces no results, for some reason
;;   gnus-group-make-nnir-group gets called *twice*. I can't figure
;;   out what the problem is.

;;; Todo:

;; o Have a more intelligent way of specifying "Where to search:".
;;   Currently the only way is to specify a directory or file. It
;;   would be nice if we could specify regexs and still have
;;   completion of some sort.
;; o Support multiple nnfolder, nnml backends.

;;; $Id: nnir-grepmail.el,v 1.2 2004/06/15 16:02:05 sigma Exp $


;;; Code:

(require 'gnus-group)
(require 'nnml)
(require 'nnfolder)
(require 'nnir)


;;; Variables

(defconst nnir-grepmail-version "1.4" "nnir-grepmail version.")

(defcustom nnir-grepmail-command "grepmail"
  "*The command to invoke grepmail."
  :type '(string)
  :group 'nnir)

(defcustom nnir-grepmail-switches '("-R" "-M" "-q" "-u" "-i" "-H")
  "*A list of strings, to be given as arguments to grepmail."
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-grepmail-nnfolder-backend
  (let ((l (1- (length gnus-secondary-select-methods)))
	(candidate nil))
    (while (>= l 0)
      (progn
	(if (string= (car (nth l gnus-secondary-select-methods)) "nnfolder")
	    (setq candidate (nth l gnus-secondary-select-methods)))
	(setq l (1- l))))
    candidate)
  "*The specification for your nnfolder backend."
  :group 'nnir)

(defcustom nnir-grepmail-nnml-backend
  (let ((l (1- (length gnus-secondary-select-methods)))
	(candidate nil))
    (while (>= l 0)
      (progn
	(if (string= (car (nth l gnus-secondary-select-methods)) "nnml")
	    (setq candidate (nth l gnus-secondary-select-methods)))
	(setq l (1- l))))
    candidate)
  "*The specification for your nnml backend."
  :group 'nnir)


;;; Functions
(defun nnir-run-grepmail (query &optional group)
  "Run grepmail."
  (save-excursion
    (let* ((backend (completing-read "Backend: "
				     (mapcar 'list '("nnfolder"
						     "nnml"))))
	   (folders
	    (mapcar 'expand-file-name
		    (split-string
		     (read-file-name
		      "Where to search: "
		      (cond
		       ((string= backend "nnfolder")
			(or 
			 (cadr (assq 'nnfolder-directory
				     nnir-grepmail-nnfolder-backend))
			 nnfolder-directory))
		       ((string= backend "nnml")
			(or
			 (cadr (assq 'nnml-directory
				     nnir-grepmail-nnml-backend))
			 nnml-directory)))
		      nil nil))))
	   (switches (split-string
		      (read-string "Switches: "
				   (mapconcat 'identity
					      nnir-grepmail-switches
					      " "))))
	   (article-number nil)
	   (matched-group nil)
	   (artlist nil)
	   (qstring (cdr (assq 'query query))))
      (when group
        (error "This backend does not support process marked searching."))
      (cond
       ((string= backend "nnfolder")
	(progn
	  (unless nnir-grepmail-nnfolder-backend
	    (error
	     "Please make sure that nnir-grepmail-nnfolder-backend is set correctly."))
	    (setq nnir-mail-backend nnir-grepmail-nnfolder-backend)))
       ((string= backend "nnml")
	(progn
	  (unless nnir-grepmail-nnml-backend
	    (error
	     "Please make sure that nnir-grepmail-nnml-backend is set correctly.")))
	(setq nnir-mail-backend nnir-grepmail-nnml-backend)))
      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (let* ((cp-list
	      `(,nnir-grepmail-command
		nil
		t
		nil
		,@switches
		"-m"
		"-e"
		,qstring
		,@folders))
	     (exitstatus
	      (progn
		(message "%s args: %s" nnir-grepmail-command
			 (mapconcat 'identity (cddddr cp-list) " "))
		(apply 'call-process cp-list))))
	     (unless (or (null exitstatus)
			 (zerop exitstatus))
	       (nnheader-report "Couldn't run grepmail: %s"
				exitstatus)
	       (when (> gnus-verbose 6)
		 (display-buffer nnir-tmp-buffer))))
      ;; Now we are in the buffer with the results
      (message "Gleaning groups and article numbers.")
      (goto-char (point-min))
      (cond
       ((string= backend "nnml")
	(while (re-search-forward
		"X-Mailfolder: +\\(.+\\)/\\([0-9]+\\)" nil t)
	  (setq matched-group (nnir-grepmail-path-to-group
			       (match-string 1)
			       nnir-grepmail-nnml-backend))
	  (setq article-number (match-string 2))
	  (if (and
	       matched-group article-number
	       (file-directory-p (nnmail-group-pathname
				    matched-group nnml-directory)))
	      (push (vector matched-group
			    (string-to-int article-number)
			    1) artlist))))
       ((string= backend "nnfolder")
	(while (re-search-forward
		"X-Gnus-Article-Number: +\\([0-9]+\\)" nil t)
	  (setq article-number (match-string 1))
	  (re-search-forward "X-Mailfolder: +\\(.+\\)" nil t)
	  (setq matched-group (nnir-grepmail-path-to-group
			       (match-string 1)
			       nnir-grepmail-nnfolder-backend))
	  (if (and
	       matched-group article-number
	       (file-exists-p (nnfolder-group-pathname matched-group)))
	      (push (vector matched-group
			    (string-to-int article-number)
			    1) artlist)))))
       artlist)))

(defun nnir-grepmail-path-to-group (path backend)
  "Converts a path to a group."
  (let* ((method (car backend))
	 (prefix (cond
		  ((string= method "nnfolder")
		   (expand-file-name
		    (concat
		     (file-name-as-directory
		      (expand-file-name
		       (or 
			(cadr (assq 'nnfolder-directory
				    nnir-grepmail-nnfolder-backend))
			nnfolder-directory)))
		     (cadr backend))))
		  ((string= method "nnml")
		   (expand-file-name
		    (concat (file-name-as-directory
			     (expand-file-name
			      (or 
			       (cadr (assq 'nnml-directory
					   nnir-grepmail-nnml-backend))
			       nnml-directory)))
			    (cadr backend))))))
	 (relative (file-relative-name path prefix)))
    (cond
     ((or (string= relative path)
	  (string-match "^\\.\\." relative))
      (progn
	(message "Couldn't parse match: %s. Ignoring." path)
	nil))
     (t
      (progn
	(unless nnmail-use-long-file-names
	  (nnheader-replace-chars-in-string relative ?/ ?.)))))))
	      

(add-to-list 'nnir-engines
	     '(grepmail nnir-run-grepmail
			((label . "Label for this search: "))))
				  

(provide 'nnir-grepmail)

;;; nnir-grepmail.el ends here
