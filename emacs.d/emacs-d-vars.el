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

;; Auto insert customization
(defvar default-auto-insert-directory "~/.emacs.d/autoinsert/")
(defvar ezsystems-auto-insert-directory "~/.emacs.d/autoinsert.ez/")
(defvar project-default-autoinsert-alist '(("\\main.cpp$"	. ["main.cpp" auto-update-header-file])
					   ("\\main.cc$"	. ["main.cc" auto-update-header-file])
					   ("\\.cpp$"		. ["cpp" auto-update-header-file])
					   ("\\.cc$"		. ["cc" auto-update-header-file])
					   ("\\.hpp$"		. ["hpp" auto-update-header-file])
					   ("\\.hh$"		. ["hh" auto-update-header-file])
					   ("\\.h$"		. ["h" auto-update-header-file])
					   ("\\.php$"		. ["php" auto-update-php-file])
					   ("\\.pro$"		. ["pro" auto-update-project-file])))

(defvar project-autoinsert-alist '(project-default-autoinsert-alist))

;; Change this if you want another directory, if it's not found the default is used
(defvar project-auto-insert-directory default-auto-insert-directory)

(defvar option-smooth-scroll t
  "Internal: Is true if the smooth scrolling is enabled, never modify this directly, instead use `option-smooth-scroll-enable' or `option-smooth-scroll-toggle'")

(defvar option-save-alist '((option-line-smooth-scroll)
			    c++-auto-include-add
			    c++-auto-include-remove
			    project-ask-load
			    project-use-auto-insert
			    project-delete-confirm
			    project-delete-redundant

			    resize-minibuffer-mode
			    resize-minibuffer-window-max-height
			    inhibit-startup-message
			    mouse-yank-at-point
			    project-mail-account

			    option-package-load-ibuffer
			    option-package-load-blank-mode
			    option-package-load-blinking-cursor
			    option-package-load-recentf
			    option-package-load-rect-mark
			    option-package-load-revive
			    option-package-load-save-history
			    option-package-load-mwheel
			    option-package-load-jde
			    option-package-load-speedbar
			    option-package-load-completion
			    option-package-load-autorevert
			    option-package-load-line-highlight
			    )
  "A list of options to be saved by `option-save-to-file',
it consists of a list of ITEMS,
each ITEM looks like (FUNCTION BODY). If FUNCTION is non-nil the
BODY is evaluated and stored, if FUNCTION is nil the BODY is
expected to be variable and it's content is stored.")

(defvar option-config-file "~/.emacs-options"
  "The global file to save options to.")
(if (not (emacs-type-is-regular))
    (setq option-config-file "~/.xemacs-options"))
(defvar option-local-config-file ".emacs-options"
  "The local file to save options to, the location of the file is dynamicly calculated.")
(if (not (emacs-type-is-regular))
    (setq option-local-config-file ".xemacs-options"))
(defvar option-local-config-dir-func nil
  "Points to a function which returns the directory for saving a local config,
nil means current directory.")

(defvar option-config-dirty nil
  "Internal: Is non-nil if one or more of the options has been changed but not saved,
don't set this directly but rather use `option-config-validate' or `option-config-invalidate'.")

(defconst option-config-major-version 0)
(defconst option-config-minor-version 7)
(defconst option-config-release 7)

(defun option-config-version ()
  "The current version of the emacs config."
  (concat (number-to-string option-config-major-version)
	  "."
	  (number-to-string option-config-minor-version)
	  "."
	  (number-to-string option-config-release)))


(defvar option-save-history-flag nil)

(defvar option-package-ibuffer
  '(ibuffer
    option-package-available-ibuffer
    option-package-load-ibuffer
    nil
    nil))
(defvar option-package-blank-mode
  '(blank-mode
    option-package-available-blank-mode
    option-package-load-blank-mode
    (blank-mode-on)
    (blank-mode-off)))
(defvar option-package-blinking-cursor
  '(blinking-cursor
    option-package-available-blinking-cursor
    option-package-load-blinking-cursor
    (progn
      (blinking-cursor-mode 1)
      (if used-color-style
	  (setq blinking-cursor-colors (caddr used-color-style))))
    (blinking-cursor-mode -1)))
(defvar option-package-recentf
  '(recentf
    option-package-available-recentf
    option-package-load-recentf
    (recentf-mode 1)
    (recentf-mode -1)))
(defvar option-package-rect-mark
  '(rect-mark
    option-package-available-rect-mark
    option-package-load-rect-mark
    (option-package-start-rect-mark)
    (option-package-end-rect-mark)))
(defvar option-package-revive
  '(revive
    option-package-available-revive
    option-package-load-revive
    (option-package-start-revive)
    (option-package-end-revive)))
(defvar option-package-save-history
  '(save-history
    option-package-available-save-history
    option-package-load-save-history
    (option-save-history-enable t)
    (option-save-history-enable nil)))
(defvar option-package-mwheel
  '(mwheel
    option-package-available-mwheel
    option-package-load-mwheel
    nil
    nil))
(defvar option-package-jde
  '(jde
    option-package-available-jde
    option-package-load-jde
    nil
    nil))
(defvar option-package-speedbar
  '(speedbar
    option-package-available-speedbar
    option-package-load-speedbar
    (speedbar 1)
    (speedbar -1)))
(defvar option-package-completion
  '(completion
    option-package-available-completion
    option-package-load-completion
    (initialize-completions)
    nil))
(defvar option-package-autorevert
  '(autorevert
    option-package-available-autorevert
    option-package-load-autorevert
    (global-auto-revert-mode 1)
    (global-auto-revert-mode -1)))
(defvar option-package-line-highlight
  '(highlight-current-line
    option-package-available-line-highlight
    option-package-load-line-highlight
    (progn
      (highlight-current-line-on t)
      (if used-color-style
	  (highlight-current-line-set-bg-color (cadr used-color-style))))
    (highlight-current-line-on nil)))

(defvar option-package-load-ibuffer nil)
(defvar option-package-load-blank-mode nil)
(defvar option-package-load-blinking-cursor nil)
(defvar option-package-load-recentf nil)
(defvar option-package-load-rect-mark nil)
(defvar option-package-load-revive nil)
(defvar option-package-load-save-history nil)
(defvar option-package-load-mwheel nil)
(defvar option-package-load-jde nil)
(defvar option-package-load-speedbar nil)
(defvar option-package-load-completion nil)
(defvar option-package-load-autorevert nil)
(defvar option-package-load-line-highlight nil)

(defvar option-package-available-ibuffer nil)
(defvar option-package-available-blank-mode nil)
(defvar option-package-available-blinking-cursor nil)
(defvar option-package-available-recentf nil)
(defvar option-package-available-rect-mark nil)
(defvar option-package-available-revive nil)
(defvar option-package-available-save-history t)
(defvar option-package-available-mwheel nil)
(defvar option-package-available-jde nil)
(defvar option-package-available-speedbar nil)
(defvar option-package-available-completion nil)
(defvar option-package-available-autorevert nil)
(defvar option-package-available-line-highlight nil)

(defvar option-package-available-alist
  '(option-package-ibuffer
    option-package-blank-mode
    option-package-blinking-cursor
    option-package-recentf
    option-package-rect-mark
    option-package-revive
    option-package-save-history
    option-package-mwheel
    option-package-jde
    option-package-speedbar
    option-package-completion
    option-package-autorevert
    option-package-line-highlight))

(provide 'emacs-d-vars)
;;; emacs-d-vars.el ends here
