; h4x0r.el 0.13
; Time-stamp: <02/07/2004 20:05:57 Yann Hodique>

; by Charles Sebold <csebold@livingtorah.org>
;
; thanks to Alex Schroeder for typo fix and feature suggestions (which
; I have not begun to implement yet)

; 0.13 - fixed a bug in sometimes-replace code, added h4x0r-dyslexic

; 0.12 - finally implemented h4x0r-replace-with-symbols-p and added a few
;        more "always-replace" words.  Also set the replacements to use
;        regexp rather than substring matches.

;;; Copyright: (C) 2000, 2001, 2003 Charles Sebold
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with GNU Emacs; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; Latest version should be available at:
;;    <URL:http://www.livingtorah.org/~csebold/emacs/h4x0r.el>
;;

(require 'cl)

(setq h4x0r-always-replace
      '(("hacker" . "h4x0r") ("hack" . "h4x0r") ("elite" . "31337")
        ("fear" . "ph33r") ("skills" . "sk1llz") ("ule" . "00l")
        ("ck\\(s\\)?\\(\\W\\)" . "x0r\\1\\2")))

(setq h4x0r-sometimes-replace
      '(("ea" "33") ("er" "0r") ("a" "4") ("b" "8") ("d" "|>")
        ("e" "3" "E") ("f" "ph" "|=") ("h" "|-|") ("i" "1" "|") ("k" "|<" "x")
        ("l" "1" "|_") ("m" "|\\/|") ("n" "|\\|") ("ou" "00" "u0") ("o" "0")
        ("q" "@") ("r" "|2") ("s" "5" "Z" "$") ("t" "+" "7") ("u" "U")
        ("v" "\\/") ("x" "X" "><") ("y" "j")))

(defvar h4x0r-unreadable 5) ; 0-10, 0 being no "sometimes" changes,
                            ; 10 being "change everything possible"
(defvar h4x0r-dyslexic 2) ; 0-10, 0 being no letter swapping, 10 being
                          ; "swap the last two letters of every word"

(defvar h4x0r-replace-with-symbols-p nil)

(defun h4x0r-region (beg end)
  "Convert region to h4x0r-talk."
  (interactive "r")
  (save-excursion
    (let ((starting-buffer (current-buffer)))
      (set-buffer (get-buffer-create "h4x0r-temp"))
      (insert-buffer-substring starting-buffer beg end)
      (downcase-region (point-min) (point-max))
      (beginning-of-buffer)
      (while (re-search-forward "\\(\\w\\)\\(\\w\\)\\(\\W\\)" nil t)
        (if (< (random 9) h4x0r-dyslexic)
            (replace-match "\\2\\1\\3")))
      (dotimes (i (length h4x0r-always-replace))
        (beginning-of-buffer)
        (let ((old-word (car (nth i h4x0r-always-replace)))
              (new-word (cdr (nth i h4x0r-always-replace))))
          (while (re-search-forward old-word nil t)
            (replace-match new-word))))
      (dotimes (i (length h4x0r-sometimes-replace))
        (if (< (random 9) h4x0r-unreadable)
            (progn
              (beginning-of-buffer)
              (let ((old-char (car (nth i h4x0r-sometimes-replace))))
                (let ((new-char (h4x0r-assoc old-char)))
                  (while (re-search-forward old-char nil t)
                    (replace-match new-char nil t)))))))
      (set-buffer starting-buffer)
      (delete-region beg end)))
  (insert-buffer "h4x0r-temp")
  (message "%s" "J00 h4v3 b33n h4x0r3d!")
  (kill-buffer "h4x0r-temp"))

(defun h4x0r-assoc (normal-char)
  (let ((h4-tmp '())
        (h4-out (cdr (assoc normal-char h4x0r-sometimes-replace))))
    (if (nlistp h4-out)
        (setq h4-out (list h4-out)))
    (if (not h4x0r-replace-with-symbols-p)
        (while (> (length h4-out) 0)
          (unless (string-match "\\W" (car h4-out))
            (if (eq nil h4-tmp)
                (setq h4-tmp (list (car h4-out)))
              (append h4-tmp (list (car h4-out)))))
          (setq h4-out (cdr h4-out))))
    (if h4-tmp
        (if (nlistp h4-tmp)
            h4-tmp
          (nth (random (length h4-tmp)) h4-tmp))
      normal-char)))

(defun h4x0r-buffer ()
  "Convert entire buffer to h4x0r-talk."
  (interactive)
  (save-excursion
    (h4x0r-region (point-max) (point-min))))

(defun h4x0r-word-at-point ()
  (interactive)
  (save-excursion
    (forward-word -1)
    (insert (h4x0r-string (current-word)))
    (kill-word 1)))

(defun h4x0r-string (h4-input-string)
  (save-excursion
    (let ((starting-buffer (current-buffer)))
      (set-buffer (get-buffer-create "h4x0r-string-temp"))
      (insert h4-input-string)
      (h4x0r-buffer)
      (setq h4-input-string (buffer-string))
      (kill-buffer "h4x0r-string-temp")
      (set-buffer starting-buffer)))
  h4-input-string)

(provide 'h4x0r)
