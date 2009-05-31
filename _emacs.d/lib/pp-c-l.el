;;; pp-c-l.el --- Display Control-l characters in a pretty way
;;
;; Filename: pp-c-l.el
;; Description: Display Control-l characters in a buffer in a pretty way
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2007, Drew Adams, all rights reserved.
;; Created: Thu Feb 08 20:28:09 2007
;; Version: 1.0
;; Last-Updated: Thu Feb 08 23:41:40 2007 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 101
;; URL: http://www.emacswiki.org/cgi-bin/wiki/pp-c-l.el
;; Keywords: display, convenience, faces
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
;;  Faces defined here:
;;
;;    `pp^L-highlight'.
;;
;;  User options defined here:
;;
;;    `pp^L-^L-string', `pp^L-^L-string-post', `pp^L-^L-string-pre',
;;    `pretty-control-l-mode'.
;;
;;  Commands defined here:
;;
;;    `pp^l', `pretty-control-l-mode'.
;;
;;  Non-interactive functions defined here:
;;
;;   `pp^L-^L-display-table-entry', `pp^L-make-glyph-code'.
;;
;;
;;  To use this library, add this to your initialization file
;;  (~/.emacs or ~/_emacs):
;;
;;    (require 'pp-c-l)           ; Load this library.
;;    (pretty-control-l-mode 1)   ; Turn on pretty display of `^L'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2007/02/08 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;

;; Convenience function suggested by Kim Storm to emacs-devel@gnu.org, in response to
;; my email 2007-02-05, subject: "cannot understand Elisp manual node Glyphs".
;; Perhaps to be added to Emacs as `make-glyph-code' after Emacs 22?
(defun pp^L-make-glyph-code (char &optional face)
  "Return a glyph code representing char CHAR with face FACE."
  (if face (logior char (lsh (face-id face) 19)) char))

(defgroup Pretty-Control-L nil
  "Options to define pretty display of Control-l (`^L') characters."
  :prefix "pp^L-" :group 'convenience :group 'wp
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
pp-c-l.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download" "http://www.emacswiki.org/cgi-bin/wiki/pp-c-l.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/PrettyControlL")
  :link '(emacs-commentary-link :tag "Commentary" "pp-c-l"))

(defface pp^L-highlight
    (if (> emacs-major-version 21)
        '((((type x w32 mac graphic) (class color))
           (:box (:line-width 3 :style pressed-button)))
          (t (:inverse-video t)))
      '((((type x w32 mac graphic) (class color))
         (:foreground "Blue" :background "DarkSeaGreen1"))
        (t (:inverse-video t))))
  "*Face used to highlight `pp^L-^L-vector'."
  :group 'Pretty-Control-L :group 'faces)

(defcustom pp^L-^L-string "          Section (Printable Page)          "
  "*Highlighted string displayed in place of each Control-l (^L) character."
  :type 'string :group 'Pretty-Control-L)

(defcustom pp^L-^L-string-pre (if (> emacs-major-version 21) "\n" "")
  "*String displayed just before `pp^L-^L-string'.
This text is not highlighted."
  :type 'string :group 'Pretty-Control-L)

(defcustom pp^L-^L-string-post ""
  "*String displayed just after `pp^L-^L-string'.
This text is not highlighted."
  :type 'string :group 'Pretty-Control-L)

(unless (fboundp 'define-minor-mode)    ; Emacs 20.
  (defcustom pretty-control-l-mode nil
    "*Toggle pretty display of Control-l (`^L') characters.
Setting this variable directly does not take effect;
use either \\[customize] or command `pretty-control-l-mode'."
    :set (lambda (symbol value) (pretty-control-l-mode (if value 1 -1)))
    :initialize 'custom-initialize-default
    :type 'boolean :group 'Pretty-Control-L))

(defun pp^L-^L-display-table-entry ()
  "Returns the display-table entry for the Control-l (`^L') character.
A vector determining how a Control-l character is displayed.
Either a vector of characters or nil.  The characters are displayed in
place of the Control-l character.  nil means `^L' is displayed.

In effect, this concatenates `pp^L-^L-string-pre', `pp^L-^L-string',
and `pp^L-^L-string-post'."
  (vconcat (mapconcat (lambda (c) (list c)) pp^L-^L-string-pre "")
           (mapcar (lambda (c) (pp^L-make-glyph-code c 'pp^L-highlight)) pp^L-^L-string)
           (mapconcat (lambda (c) (list c)) pp^L-^L-string-post "")))

(defalias 'pp^l 'pretty-control-l-mode)

(if (fboundp 'define-minor-mode)
    ;; Emacs 21 and later.
    ;; We eval this so that even if the library is byte-compiled with Emacs 20,
    ;; loading it into Emacs 21+ will define variable `pretty-control-l-mode'.
    (eval '(define-minor-mode pretty-control-l-mode
            "Toggle pretty display of Control-l (`^L') characters.
With ARG, turn pretty display of `^L' on if and only if ARG is positive."
            :init-value nil :global t :group 'Pretty-Control-L
            :link `(url-link :tag "Send Bug Report"
                    ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
pp-c-l.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
            :link '(url-link :tag "Other Libraries by Drew"
                    "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
            :link '(url-link :tag "Download"
                    "http://www.emacswiki.org/cgi-bin/wiki/pp-c-l.el")
            :link '(url-link :tag "Description"
                    "http://www.emacswiki.org/cgi-bin/wiki/PrettyControlL")
            :link '(emacs-commentary-link :tag "Commentary" "pp-c-l")
            (cond (pretty-control-l-mode
                   (unless standard-display-table ; Default value is nil!
                     (setq standard-display-table (make-display-table)))
                   (aset standard-display-table ?\014 (pp^L-^L-display-table-entry)))
                  (t
                   (unless standard-display-table ; Default value is nil!
                     (setq standard-display-table (make-display-table)))
                   (aset standard-display-table ?\014 nil)))))

  ;; Emacs 20
  (defun pretty-control-l-mode (&optional arg)
    "Toggle pretty display of Control-l (`^L') characters.
With ARG, turn pretty display of `^L' on if and only if ARG is positive."
    (interactive "P")
    (setq pretty-control-l-mode
          (if arg (> (prefix-numeric-value arg) 0) (not pretty-control-l-mode)))
    (cond (pretty-control-l-mode
           (unless standard-display-table ; Default value is nil!
             (setq standard-display-table (make-display-table)))
           (aset standard-display-table ?\014 (pp^L-^L-display-table-entry))
           (message "Pretty display of `^L' is now ON"))
          (t
           (unless standard-display-table ; Default value is nil!
             (setq standard-display-table (make-display-table)))
           (aset standard-display-table ?\014 nil)
           (message "Pretty display of `^L' is now OFF")))))

;;;;;;;;;;;;;;;;;;;;

(provide 'pp-c-l)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pp-c-l.el ends here
