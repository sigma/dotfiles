;;; highlight-current-line.el --- highlight line where the cursor is.

;; Copyright (c) 1997-2000 Christoph Conrad Time-stamp: <12.03.2002 07:53:58>

;; Author: Christoph Conrad <Christoph.Conrad@gmx>
;; Created: 10 Oct 1997
;; Version: 0.53
;; Keywords: faces

;; This file is not yet part of any Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 
;;; Commentary:

;; Highlights the line the cursor is inYou can change colors of foreground
;; (text) and background. Highlighting is (currently) switched on in ALL
;; buffers including minibuffers. Default behaviour is to set only background
;; color, so that font-lock fontification colors remain visible (syntax
;; coloring).  See functions `highlight-current-line-on',
;; `highlight-current-line-set-fg-color',
;; `highlight-current-line-set-bg-color'. There's a special color "none"
;; defined to set no color.

;; You can select whether the whole line (from left to right window border)
;; is marked or only the really filled parts of the line (from left window
;; border to the last char in the line). The second behaviour is suitable if
;; its important for you to see trailing spaces or tabs in a line. See
;; function `highlight-current-line-whole-line-on'. In XEmacs this is hardly
;; to recognize, cause there the region is a notch more extended.

;; You can ignore buffers, whose buffer-name match some regular expression,
;; so they never get highlighted. Some buffers are ignored by default, see
;; variable `highlight-current-line-ignore-regexp'. You can extend or
;; redefine this regexp. This works together with the default ignore function
;; `highlight-current-line-ignore-function'. You can redefine this function
;; to implement your own criterias.
 
;;; People which made contributions or suggestions:

;; This list is ordered by time. Latest in time first.
;; - Masatake Yamato <jet@gyve.org>
;; - Hrvoje Niksic  <hniksic@srce.hr>
;; - Jari Aalto   <jari.aalto@ntc.nokia.com>
;; - Shawn Ostermann <sdo@picard.cs.OhioU.Edu>
;; - Peter Ikier  <p_ikier@infoac.rmi.de>
;;   Many thanks to him for the idea. He liked this behaviour in another
;;   editor ("Q").
 
;;; Installation:

;; e.g. in .emacs
;; (require 'highlight-current-line)
;; ;; If you want to mark only to the end of line:
;; (highlight-current-line-whole-line-on nil)
;; ;; switch highlighting on
;; (highlight-current-line-on t)
;;
;; ;; If you want to change default-foreground/background color add something
;; ;; like:
;; (highlight-current-line-set-fg-color "red")
;; (highlight-current-line-set-bg-color "white")
;; ;; There's a special color "none" defined to set no color.
;;
;; ;; Ignore no buffer
;; (setq highlight-current-line-ignore-regexp nil) ; or set to ""
;; ;; alternate way to ignore no buffers
;; (fmakunbound 'highlight-current-line-ignore-function)
;; ;; Ignore more buffers
;; (setq highlight-current-line-ignore-regexp
;;      (concat "Dilberts-Buffer\\|"
;;       highlight-current-line-ignore-regexp))
;;
;; Put a copy of highlight-current-line.el/.elc into some path of
;; `load-path'. To show `load-path': <C-h v> load-path RET
;;
 
;;; Troubleshooting:

;; - Q: I do not see matching parens from paren.el any more!
;; - A: Check the colors from highlight-current-line or from show-paren-face
;;   and choose some combination which works together.
 
;;; ToDo:

;; - highlight paragraphs, functions etc... (suggestion by Daniel Lundin
;;   <daniel@emacs.org> 19 Dec 1999)
;; - provide overlay priorities
;;   (overlay-put highlight-current-line-overlay 'priority 60)
;; - better way to switch off 'ignore buffer'
;; - face fore/backgroundcolor depending on major-mode
;; - better way to detect xemacs

;; - some suggestions for default keys
;; - highlight-current-line as minor mode. Suggested by Shawn Ostermann.
 
;;; Change log:

;; 12 Mar 2002 - v0.53
;; - updated email address

;; 05 Feb 2001
;; - highlight-current-line-ignore-regexp: better regexp for minibuffers

;; 15 Jul 2000 - v0.52:
;; - Masatake YAMATO: added emacsclient / gnudoit support. Invoking emacs
;;   to load a file from external, highlight-current-line couldn't
;;   initially show the line of the loaded file highlighted.

;; 19 Oct 1997 - v0.51:
;; - uses defcustom-library if available. Suggested by Jari Aalto and Hrvoje
;;   Niksic.
;; - logic error in if-condition of post-command-hook. All Buffers were
;;   ignored if highlight-current-line-ignore-function was unbound.

;; 18 Oct 1997 - v0.5:
;; - GNU General Public License
;; - ignore user-definable buffernames which are ignored for
;;   highlighting. Suggested by Jari Aalto.
;; - works with XEmacs, at least version 19.15. Mark whole line doesnt work
;;   yet. Suggested by Jari Aalto.
;; - highlight-current-line-set-fg/bg-color understand "none" as color
;; - overlay-put moved from post-command-hook to initialization-code
;; - version-variable: `highlight-current-line-version'. Always
;;   "major.minor". Suggested by Jari Aalto.

;; 11 Oct 1997 - v0.4:
;; - Possibility to highlight whole line (from left to right windowborder) or
;;   only from left window border to the last char in the line.
;;
;; 20 Aug 1997 - v0.3:
;; - First public released version.
 
;;; Code:


;; Initialization for XEmacs

;; XEmacs needs overlay emulation package.
;; Old XEmacs won't have the package and we must quit.
(eval-and-compile
  (if (boundp 'xemacs-logo)
      (if (not (load "overlay" 'noerr))
   (error "\
highlight-current-line.el: ** This package requires overlays.  Abort"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; can be set by user

;; Compatibility blob for those without the custom library:
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

(defgroup highlight-current-line nil
  "Highlight line where the cursor is."
  :group 'faces) ;; or 'matching??

(defcustom highlight-current-line-ignore-regexp
  (concat
   "Faces\\|Colors\\| \\*Mini"
   ;; for example:
   ;; "\\|RMAIL.*summary\\|\\*Group\\|\\*Summary"
   )
  "*Regexps for buffers to ignore.
Used by `highlight-current-line-ignore-function'."
  :type  'regexp
  :group 'highlight-current-line)


(defcustom highlight-current-line-whole-line t
  "*If non-nil, mark up to `end-of-line'.  If nil, mark up to window-border.
Use `highlight-current-line-whole-line-on' to set this value."
  :type  'boolean
  :group 'highlight-current-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; should not be set by user

(defconst highlight-current-line-version "0.53"
  "Version number." )

(defconst highlight-current-line-no-color (if (boundp 'xemacs-logo)
       '[]
       nil)
  "'color' value that represents \"no color\".")



;; Create default-face for highlighting.
(make-face 'highlight-current-line-face)

(set-face-foreground 'highlight-current-line-face
       highlight-current-line-no-color)
(set-face-background 'highlight-current-line-face
       "wheat")

(defvar highlight-current-line-overlay
  ;; Dummy initialization
  (make-overlay 1 1)
  "Overlay for highlighting."
  )

;; Set face-property of overlay
(overlay-put highlight-current-line-overlay
      'face 'highlight-current-line-face)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Internal function for test
(defun highlight-current-line-reload ()
  "Reload library highlight-current-line for test purposes."
  (interactive)
  (unload-feature 'highlight-current-line)
  (load-library "highlight-current-line"))


;; Decide whether to highlight the buffer.
(defun highlight-current-line-ignore-function  ()
  "Default ignore function.
If the return value is nil, buffer can be highlighted."
  (if (or (equal "" highlight-current-line-ignore-regexp)
   (not highlight-current-line-ignore-regexp))
      nil
       (string-match highlight-current-line-ignore-regexp (buffer-name))))


;; Set foregroundcolor of cursor-line.
(defun highlight-current-line-set-fg-color (color)
  "Set foregroundcolor for highlighting cursor-line to COLOR.
Key: \\[highlight-current-line-set-fg-color]"
  (interactive "sForeground color (\"none\" means no color): ")
  (if (equal "none" color)
      (setq color highlight-current-line-no-color))
  (set-face-foreground 'highlight-current-line-face color))


;; Set backgroundcolor of cursor-line.
(defun highlight-current-line-set-bg-color (color)
  "Set backgroundcolor for highlighting cursor-line to COLOR.
Key: \\[highlight-current-line-set-bg-color]"
  (interactive "sBackground color (\"none\" means no color): ")
  (if (equal "none" color)
      (setq color highlight-current-line-no-color))
  (set-face-background 'highlight-current-line-face color))


;; Post-Command-Hook for highlighting
(defun highlight-current-line-hook ()
  "Post-Command-Hook for highlighting."
  (condition-case ()
      (if (not (and (fboundp 'highlight-current-line-ignore-function)
                    (highlight-current-line-ignore-function)))

          (let ((current-point (point)))

            ;; Set overlay
            (move-overlay highlight-current-line-overlay
                          (progn (beginning-of-line)
                                 (point))
                          (progn (if highlight-current-line-whole-line
                                     (forward-line 1)
                                   (end-of-line))
                                 (point))
                          (current-buffer))

            (goto-char current-point)))
    (error nil)))

;; Enable/Disable Highlighting
(defun highlight-current-line-on (&optional on-off)
  "Switch highlighting of cursor-line ON-OFF.
Key: \\[highlight-current-line-on]"
  (interactive (list (y-or-n-p "Highlight line with cursor? ")))

  (if on-off
      (progn
        (add-hook 'post-command-hook 'highlight-current-line-hook)
        (if (boundp 'server-switch-hook)
            (add-hook 'server-switch-hook 'highlight-current-line-hook))
        (if (boundp 'gnuserv-visit-hook)
            (add-hook 'gnuserv-visit-hook 'highlight-current-line-hook)))
    (if  (boundp 'server-switch-hook)
        (remove-hook 'server-switch-hook 'highlight-current-line-hook))
    (if (boundp 'gnuserv-visit-hook)
        (remove-hook 'gnuserv-visit-hook 'highlight-current-line-hook))
    (remove-hook 'post-command-hook 'highlight-current-line-hook)
    (delete-overlay highlight-current-line-overlay)))

;; Enable/Disable whole line marking
(defun highlight-current-line-whole-line-on (&optional on-off)
  "Switch highlighting of whole line ON-OFF.
Key: \\[highlight-current-line-whole-line-on]"
  (interactive (list (y-or-n-p "Highlight whole line? ")))

  (setq highlight-current-line-whole-line on-off))

(provide 'highlight-current-line)

;;; highlight-current-line.el ends here