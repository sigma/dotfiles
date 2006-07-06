;;; emacs-type.el --- determine the emacs type in an easy way

;; Copyright (C) 2001 Jan Borsodi
;; Author: Jan Borsodi <jb@ez.no>

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; emacs-type.el lets you pick out which emacs version is running
;; and do your magic according to the type.
;; This file is small so looking at the functions should give you
;; an idea of how to use it.

(defun emacs-type ()
  "Returns the type of emacs in use.
Returns:
emacs - for regular Emacs in a console
emacs-window - for regular Emacs in a window(X)
xemacs - for XEmacs in a console
xemacs-window - for XEmacs in a window(X)
emacs-nt - for regular Emacs on Windows NT/9x in a console (NOT IMPLEMENTED YET)
emacs-nt-window - for regular Emacs on Windows NT/9x in a window(W32)
xemacs-nt - for XEmacs on Windows NT/9x in a console (NOT IMPLEMENTED YET)
xemacs-nt-window - for XEmacs on Windows NT/9x in a window(W32)
emacs-mac-window - for regular Emacs on Mac(??) in a window
emacs-msdos - for MS-DOS
unknown - Something unsupported"
  (cond
	((string= "w32" window-system)
	 'emacs-nt-window)
	((string= "mswindows" window-system)
	 'xemacs-nt-window)
	((and (eq "msdos" system-type)
	      (string-match "GNU" (emacs-version)))
	 'emacs-msdos)
	((or (and (string-match "Macintosh" (emacs-version))
		  (string-match "GNU" (emacs-version)))
	     (boundp 'macintosh))
	 'emacs-mac-window)
	((or (eq system-type 'gnu/linux)
	     (eq system-type 'linux))
	 (if (string-match "XEmacs" emacs-version)
	     (if window-system
		 'xemacs-window
	       'xemacs)
	   (if window-system
	       'emacs-window
	     'emacs)))
	((or (eq system-type 'windows)
	     (string-match "GNU" (emacs-version)))
	 'emacs-nt-window)
	(t 'unknown)))

(defun emacs-type-overall ()
  "Returns whether regular Emacs or XEmacs is in use"
  (let ((type (emacs-type)))
    (cond ((or (eq type 'emacs)
	       (eq type 'emacs-window)
	       (eq type 'emacs-nt)
	       (eq type 'emacs-nt-window)
	       (eq type 'emacs-mac-window)
	       (eq type 'emacs-msdos))
	   'emacs)
	  ((or (eq type 'xemacs)
	       (eq type 'xemacs-window)
	       (eq type 'xemacs-nt)
	       (eq type 'xemacs-nt-window))
	   'xemacs))))

(defun emacs-type-is-regular ()
  "Returns t if regular Emacs is in use"
  (let ((type (emacs-type-overall)))
    (cond ((eq type 'emacs)
	   t)
	  ((eq type 'xemacs)
	   nil)
	  (t nil))))

(provide 'emacs-type)
