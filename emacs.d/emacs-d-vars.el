;;; emacs-d-vars.el ---  default variables

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Yann Hodique <Yann.Hodique@lifl.fr>
;; Keywords: internal

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

;; Default emacs variables

(defvar auto-compile-lisp t)

(defvar main-frame nil)

(defvar default-color-style nil)
(defvar used-color-style nil)

;; reg-exp used for finding projects
(defvar project-regexp "\\.pro$")

;; non-nil means ask for confirmation when deleting redundant class files
(defvar project-delete-confirm t)
;; non-nil means delete redundant class files
(defvar project-delete-redundant t)

;; non-nil means ask before loading projects from disk
(defvar project-ask-load t)

;; Variables used in classname replacement
(defvar project-normal-name-match "|||")
(defvar project-downcase-name-match "!!!")
(defvar project-upcase-name-match "@@@")
(defvar project-deriveclass-match "%%%")
(defvar project-params-match "$$$")
(defvar project-params-init-match "===")

;; Debugger to be used for debugging
(defvar project-debugger "gdb")

(defvar project-mail-account "Yann.Hodique@lifl.fr")

;; Use autoinsert for cpp/hpp files or create classes with class-add, non-nil means use autoinsert
(defvar project-use-auto-insert t)

;; Default extension for c++ header files.
(defvar c++-default-header-ext "hpp")
;; Default extension for c++ source files.
(defvar c++-default-source-ext "cpp")
;; Default regexp for c++ header files.
(defvar c++-header-ext-regexp "\\.\\(hpp\\|h\\|\hh\\|H\\)$")
;; Default regexp for c++ source files.
(defvar c++-source-ext-regexp "\\.\\(cpp\\|c\\|\cc\\|C\\)$")
;; Default regexp for includes
(defvar project-c++-include-regexp "#include[ \t]+\\(\\(<[^>]*>\\)\\|\\(\"[^\"]*\"\\)\\)[ \t]*\n")
;; Default regexp for class declarations
(defvar project-c++-class-decl-regexp "class[ \t]+\\([A-Za-z][A-Za-z0-9_]*\\);[ \t]*\n")

;; Default list of paths to check for include files, add your own to the list
;; by adding the line (setq project-include-paths (cons "/my/dir" project-include-paths))
;; somewhere in your .emacs file
(defvar project-include-paths '( "/usr/include"
				 "/usr/include/stl"
				 "/usr/local/include"
				 "$QTDIR/include" ))

;; If non-nil automaticly insert/remove include files in c++ files
(defvar c++-auto-include-add nil)
(defvar c++-auto-include-remove nil)

;; new access protection clauses for qt
(defconst c-qt-access-key "\\<\\(public\\|protected\\|private\\)\\>[ \t]+\\<slots\\>")

(defconst c-qt-C++-access-key (concat "\\("
				      "\\<\\(public\\|protected\\|private\\|signals\\|slots\\)\\>"
 				      "\\)\\|\\("
;; 				      c-qt-access-key
 				      "\\)[ \t]*:"))

;; Which buffers to include, default all.
(defvar buffer-include-regexp '(".*"))

;; Which buffers to exclude, default *blah* buffers.
(defvar buffer-exclude-regexp '("\\*[^\\*]+\\*"))

;; Define c++ regexp
(defvar c++-buffer-include-regexp (concat "[^.]*\\.\\("
					  (regexp-opt '( "cpp" "cc" "c" "C" "hpp" "hh" "h" "H" ) t)
					  "\\)"))

;; Define c++ regexp
(defvar php-buffer-include-regexp (concat "[^.]*\\.\\("
					  (regexp-opt '( "php" "php3" "php4" ) t)
					  "\\)"))

;; Define project regexp
(defvar project-buffer-include-regexp (concat "[^.]*\\.\\("
					      (regexp-opt '( "pro" ) t)
					      "\\)"))
;; non-nil means browse only cpp files
(defvar c++-buffers-only nil)

;; Wanted c++ style
(defvar wanted-c++-style "eZSystems")

(provide 'emacs-d-vars)
;;; emacs-d-vars.el ends here
