;;; anchored-transpose.el --- Transposes a phrase around an anchor phrase

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Rick Bielawski <rbielaws@i1.net>
;; Keywords: tools convenience

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;;; Commentary:

;; `anchored-transpose' is an interactive autoload function to transpose
;; portions of a region around an anchor phrase.
;;
;; `this phrase but not that word'    can be transposed into
;; `that word but not this phrase'
;;
;; I want this phrase but not that word.
;;        |                            |   This is the entire phrase.
;;                    |     |              This is the anchor phrase.
;;
;; First select the entire phrase and type \\[anchored-transpose].  Then
;; select the anchor phrase `but not' and type \\[anchored-transpose] again.
;; By default the anchor phrase will automatically include any surrounding
;; whitespace even if you don't specifically select it.  Also, it won't
;; include certain trailing punctuation.  See `anchored-transpose-do-fuzzy'.
;;
;; You can select the anchor phrase first followed by the phrase to be
;; transposed if more convenient.  Typing \\[anchored-transpose] with nothing
;; selected clears any prior selection.
;;
;; And now you can also select the 2 phrases to be transposed.  I.E.
;; "this phrase" then "that word" in the example above.  In either order.

;;; Installing:

;; 1) Put anchored-transpose.el on your load path.
;; 2) Put the following in your .emacs
;;    (global-set-key [?\C-x ?t] 'anchored-transpose)
;;    (autoload 'anchored-transpose "anchored-transpose" nil t)

;;; History:

;; 2004-09-24 RGB Seems useable enough to release.
;; 2004-10-15 RGB Only comments and doc strings were updated.
;; 2004-10-22 RGB Added support for 2 phrase selection.
;; 2004-12-01 RGB Added secondary selection support.

;;; Code:

(defvar anchored-transpose-anchor ()
  "begin/end when `anchored-transpose' is in progress else nil")

;;;###autoload
(defun anchored-transpose (beg end flg &optional beg2 end2 flg2)
  "Transpose portions of the region around an anchor phrase.

`this phrase but not that word'    can be transposed into
`that word but not this phrase'

I want this phrase but not that word.
       |                            |   This is the entire phrase.
                  |       |             This is the anchor phrase.

First select the entire phrase and type \\[anchored-transpose].  Then select
the anchor phrase and type \\[anchored-transpose] again.  By default the
anchor phrase will automatically include any surrounding whitespace even if
you don't specifically select it.  Also, it won't include certain trailing
punctuation.  See `anchored-transpose-do-fuzzy' for details.

You can select the anchor phrase first followed by the entire phrase if more
convenient.  Typing \\[anchored-transpose] with nothing selected clears any
prior selection.  If both primary and secondary selections are active this
command swaps the 2 selections immediately."
  (interactive `(,(region-beginning) ,(region-end)
                 ,current-prefix-arg
                 ,@anchored-transpose-anchor))
  (setq anchored-transpose-anchor nil deactivate-mark t)
  (when (and mouse-secondary-overlay
             mark-active
             (eq (overlay-buffer mouse-secondary-overlay)
                 (current-buffer)
             )
             (/= (overlay-start mouse-secondary-overlay)
                 (overlay-end mouse-secondary-overlay)
             )
        )
    (setq beg2 (overlay-start mouse-secondary-overlay))
    (setq end2 (overlay-end mouse-secondary-overlay))
    (setq flg2 nil)
    (delete-overlay mouse-secondary-overlay)
  )
  (if mark-active
      (if end2  ; then both regions are marked.  swap them.
          (if (and (< beg beg2)
                   (> end end2))
              (apply 'anchored-transpose-swap
                     (anchored-transpose-do-fuzzy
                      beg beg2 end2 end flg flg2))
            (if (and (> beg beg2)
                     (< end end2))
                (apply 'anchored-transpose-swap
                        (anchored-transpose-do-fuzzy
                         beg2 beg end end2 flg2 flg))
              (if (< end beg2)
                  (apply 'anchored-transpose-swap
                         (anchored-transpose-do-fuzzy
                          beg end beg2 end2 flg flg2))
              (if (< end2 beg)
                  (apply 'anchored-transpose-swap
                         (anchored-transpose-do-fuzzy
                          beg2 end2 beg end flg2 flg))
                (error "Regions have invalid overlap")))))
        ;; 1st of 2 regions.  Save it and wait for the other.
        (setq anchored-transpose-anchor (list beg end flg))
        (message "Select other part (anchor or region)"))
      (error "Command requires a marked region")))

(defun anchored-transpose-do-fuzzy (r1beg r1end r2beg r2end flg1 flg2)
"Returns the arguments supplied after adjusting their value if necessary.

I want this phrase but not that word.
       |                            |   This is the entire phrase.
                  |       |             This is the anchor phrase.
     r1beg      r1end   r2beg     r2end

r1beg and r1end define the first region to be transposed.  r1end and r2beg
define the anchor region.  The second region to be transposed is defined by
r2beg and r2end.

flg1 is the prefix arg when the entire phrase to be transposed was selected.
flg2 is the prefix arg when the anchor phrase was selected.  If the phrases
to be transposed were selected independently then flg1 is associated with
r1beg r1end and flg2 with r2beg r2end.  There is no way to tell the order
of the original selections.

If flg1 is nil, r1beg is adjusted to ignore leading whitespace and r2end
is adjusted if necessary to exclude trailing punctuation so it's not part
of what gets transposed.  Specifically, .!?

If flg2 is nil, this function adjusts r1end and r2beg so whitespace
surrounding the anchor phrase is also anchored."
  (list
   (if flg1 r1beg
     (anchored-transpose-fuzzy-begin r1beg r1end "[\t ]+"))
   (if flg2 r1end
     (anchored-transpose-fuzzy-end   r1beg r1end "\\s +"))
   (if flg2 r2beg
     (anchored-transpose-fuzzy-begin r2beg r2end "[\t ]+"))
   (if flg1 r2end
     (anchored-transpose-fuzzy-end   r2beg r2end "\\s *[.!?]"))
   flg1 flg2))

(defun anchored-transpose-fuzzy-end (beg end what)
  "Returns a possibly new value for `end' based on the regexp `what'.
`beg' and `end' are buffer positions defining a region.  If that region ends
with `what' then `end' is adjusted to exclude that matching text.

NOTE: The regexp is applied differently than `looking-back' applies a regexp.

Example: if (buffer-string beg end) contains `1234' the regexp `432' matches
it, not `234' as `looking-back' would.  Also, your regexp never sees the char
at beg so the match will always leave at least 1 character to transpose."
  (let ((str (concat
              (reverse (append (buffer-substring (1+ beg) end) nil)))))
    (if (string-match (concat "^" what) str)
        (- end (length (match-string 0 str)))
      end)))

(defun anchored-transpose-fuzzy-begin (beg end what)
  "Returns a possibly new value for `beg' based on the regexp `what'.
`beg' and `end' are buffer positions defining a region.  If the region begins
with `what' then `beg' is adjusted to exclude the matching text.

NOTE: Your regexp never sees the last char defined by beg/end.  This insures
at least 1 char is always left to transpose."
  (let ((str (buffer-substring beg (1- end))))
    (if (string-match (concat "^" what) str)
        (+ beg (length (match-string 0 str)))
      beg)))

(defun anchored-transpose-swap (r1beg r1end r2beg r2end flg flg2)
  "Swaps region r1beg/r1end with r2beg/r2end. Flags are currently ignored.
Point is left at r1end."
  (let ((reg1 (buffer-substring r1beg r1end))
        (reg2 (delete-and-extract-region r2beg r2end)))
    (goto-char r2beg)
    (insert reg1)
    (save-excursion  ;; I want to leave point at the end of phrase 2.
      (goto-char r1beg)
      (delete-region r1beg r1end)
      (insert reg2))))

(provide 'anchored-transpose)

;;; anchored-transpose.el ends here.
