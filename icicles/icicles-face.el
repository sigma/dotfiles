;;; icicles-face.el --- Faces for Icicles
;; 
;; Filename: icicles-face.el
;; Description: Faces for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:19:43 2006
;; Version: 22.0
;; Last-Updated: Mon Feb 27 13:26:00 2006 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 12
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-face.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  This is a helper library for library `icicles.el'.  It defines
;;  faces.  See `icicles.el' for documentation.
;; 
;;  Faces defined here (in Custom group `icicles'):
;;
;;    `icicle-complete-input', `icicle-historical-candidate',
;;    `icicle-prompt-suffix', `icicle-root-highlight-Completions',
;;    `icicle-root-highlight-minibuffer'.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Faces (alphabetical) -----------------------------------

(defgroup icicles nil
  "Minibuffer input completion and cycling of completion candidates."
  :prefix "icicle-"
  :group 'completion :group 'convenience :group 'help :group 'apropos
  :group 'dabbrev :group 'matching :group 'minibuffer :group 'recentf
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Commentary" "icicles")
  )

(defface icicle-complete-input '((t (:foreground "dark green")))
  "*Face used to highlight input when it is complete."
  :group 'icicles :group 'faces)

(defface icicle-Completions-instruction-1 '((t (:foreground "Blue")))
  "*Face used to highlight first line of *Completions* buffer."
  :group 'icicles :group 'faces)

(defface icicle-Completions-instruction-2 '((t (:foreground "Red")))
  "*Face used to highlight second line of *Completions* buffer."
  :group 'icicles :group 'faces)

(defface icicle-historical-candidate '((t (:foreground "Blue")))
  "*Face used to highlight *Completions* candidates that have been used."
  :group 'icicles :group 'faces)

; Value is from `custom-button-pressed-face', with :foreground from `minibuffer-prompt'.
(defface icicle-prompt-suffix
    '((((type x w32 mac) (class color))
       (:box (:line-width 2 :style pressed-button) :foreground "dark blue"))
        ;;; :background "lightgrey" :foreground "black"
      (t (:inverse-video t)))
  "*Face used to highlight `icicle-prompt-suffix'."
  :group 'icicles :group 'faces)

(defface icicle-root-highlight-Completions '((t (:foreground "Red3")))
  "*Face used to highlight root that was completed, in minibuffer."
  :group 'icicles :group 'faces)

(defface icicle-root-highlight-minibuffer '((t (:underline t)))
  "*Face used to highlight root that was completed, in *Completions*."
  :group 'icicles :group 'faces)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-face.el ends here
