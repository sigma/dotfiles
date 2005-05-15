;;; keytable.el --- tabular alternative to describe-bindings, where-is

;; Author: Sandip V. Chitale
;; Created: Sep 12 2003
;; Keywords: tabular describe-bindings where-is

;; This file is not part of GNU Emacs yet.

;;; Commentary:
;; Here is tabular alternative to `describe-bindings' and `where-is'.
;;
;; To install, put the file on your Emacs-Lisp load path and add the
;; following into your ~/.emacs startup file:
;;
;;  (require 'keytable)
;;
;; To use type,
;;
;; M-x keytable RET
;;
;; It shows the key bindings in tabular form.  The bound keys are
;; shown with `underline' face.  Use TAB/SHIFT TAB to next and
;; previous bound keys.  Mouse over bound keys shows the bound
;; function name.  RET or mouse-click over a bound key describes the
;; bound function using `describe-function'.  C-RET over a bound key
;; shows all the keys bound to the same function using `where-is'.
;;
;;; Code:
(defun keytable-binding-by-xref-at-point ()
  "Describe the binding using `describe-function' based on the xref at
point."
  (interactive)
  (let ((keymap (get-text-property (point) 'keymap))
        (buffer (get-text-property (point) 'buffer))
        (binding (get-text-property (point) 'binding)))
    (if buffer
        (keytable keymap buffer)
      (if binding
          (progn
            (message "")
            (if (get-buffer-window "*Help*")
                (delete-window (get-buffer-window "*Help*")))
            (describe-function binding)
            (shrink-window-if-larger-than-buffer (get-buffer-window
                                                  "*Help*"))
            (if (sit-for 5)
                (delete-window (get-buffer-window "*Help*"))))))))

(defun keytable-where-is-binding-by-xref-at-point ()
  "Show `where-is' the binding based on the xref at point."
  (interactive)
  (let ((binding (get-text-property (point) 'binding)))
    (if binding
 (where-is binding)
 )
    )
  )

;; For tabbing through buffer.
(defun keytable-next-ref ()
  "Find the next binding in the buffer."
  (interactive)
  (let (pos)
    (while (not pos)
      (if (get-text-property (point) 'binding) ; move off reference
   (goto-char (or (next-single-property-change (point) 'xref)
    (point))))
      (cond ((setq pos (next-single-property-change (point) 'xref))
      (if pos (goto-char pos)))
     ((bobp)
      (message "No cross references in the buffer.")
      (setq pos t))
     (t    ; be circular
      (goto-char (point-min)))))))

(defun keytable-previous-ref ()
  "Find the previous binding in the buffer."
  (interactive)
  (let (pos)
    (while (not pos)
      (if (get-text-property (point) 'binding) ; move off reference
   (goto-char (or (previous-single-property-change (point) 'xref)
                         (point))))
      (cond ((setq pos (previous-single-property-change (point)
       'xref))
      (if pos (goto-char pos)))
     ((bobp)
      (message "No cross references in the buffer.")
      (setq pos t))
     (t    ; be circular
      (goto-char (point-max)))))))

;;;###autoload
(defun keytable (&optional prefix buffer)
  "Display key table."
  (interactive)
  (let ((index 0)
 (cha (vconcat prefix [x]))
 (chad)
 (binding)
 (help-echo)
 (subkeymaps)
 (refbuffer (if buffer buffer (current-buffer)))
 )
    (set-buffer (get-buffer-create "*keytable*"))
    (toggle-read-only -1)
    (erase-buffer)
    (insert "Parent Keymaps:\n")
    (dotimes (subprefixindex (length prefix))
      (let* ((subprefix (subseq prefix 0 (- (length prefix) (1+ subprefixindex))))
      (desc (key-description subprefix))
      )
 (insert (propertize (format "%-20s" (if (> (length subprefix) 0)
     (concat desc " Keymap")
     (progn

       "Root Keymap")))
       'help-echo (if (> (length subprefix) 0) (concat  desc " Keymap") "Root Keymap")
       'face 'custom-button-face
       'xref t
       'keymap subprefix
       'buffer refbuffer))
 (if (> (length subprefix) 0) (insert " in "))))
    (insert "\n\n")
    (dolist (ch (list 'f1 'f2 'f3 'f4 'f5 'f6 'f7 'f8 'f9 'f10 'home
        'end 'kp-home 'kp-end 'up 'down 'right 'left 'kp-up 'kp-down 'kp-right
        'kp-left 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51
        52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
        75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97
        98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
        116 117 118 119 120 121 122 123 124 125 126 127 1 2 3 4 5 6 7 8 9 10
        11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 128 129
        130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146
        147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163
        164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
        181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197
        198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214
        215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231
        232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248
        249 250 251 252 253 254 255))

      (if (= 0  (% index (- (window-height) (+ 3
            next-screen-context-lines))))
   (progn
     (insert (propertize (concat
     "key                    |"
     "C-key                  |"
     "M-key                  |"
     "M-C-key                |"
     "S-C-key                |"
     "S-M-C-key              |")
    'face 'menu))
     (insert "\n")))

      (dolist (mod (list ()
    '(control)
    '(meta)
    '(meta control)
    '(shift control)
    '(shift meta control)))
 (aset cha (1- (length cha)) (nconc (copy-list mod) (list ch)))
 (save-excursion
   (set-buffer refbuffer)
   (setq binding (key-binding cha)))
 (if (symbolp binding)
     (setq help-echo (symbol-name binding))
     (setq help-echo ""))
 (if binding
     (if (keymapp binding)
  (progn
    (if (not (member (copy-seq cha) subkeymaps))
        (setq subkeymaps (add-to-list 'subkeymaps (copy-seq cha))))
    (setq chad (propertize (key-description cha)
     'help-echo help-echo
     'face 'underline
     'xref t
     'keymap (copy-seq cha)
     'buffer refbuffer)))
  (setq chad (propertize (key-description cha)
           'help-echo help-echo
           'face 'underline
           'xref t
           'binding binding))
  )
     (setq chad (propertize (key-description cha)
       'face 'default)))
 (insert chad)
 (insert (make-string (- 24 (length chad)) 32))
 )
      (insert "\n")
      (setq index (1+ index)))
    (goto-char (point-min))
    (insert "Sub Keymaps:\n")
    (setq index 0)
    (dolist (subkeymap subkeymaps)
      (insert (propertize (format "%-20s" (concat (key-description subkeymap) " Keymap"))
     'help-echo (concat (key-description subkeymap) " Keymap")
     'face 'custom-button-face
     'xref t
     'keymap subkeymap
     'buffer refbuffer))
      (insert "\t")
      (setq index (1+ index))
      (if (= (mod index 5) 0)
   (insert "\n"))
      ))
      (insert "\n")
  (switch-to-buffer "*keytable*")
  (delete-trailing-whitespace)
  (toggle-read-only 1)
  (local-set-key [(q)] 'bury-buffer)
  (local-set-key [(return)] 'keytable-binding-by-xref-at-point)
  (local-set-key [(control return)]
   'keytable-where-is-binding-by-xref-at-point)
  (local-set-key [(down-mouse-1)]
   'keytable-binding-by-xref-at-point)
  (local-set-key [(tab)] 'keytable-next-ref)
  (local-set-key [(shift tab)] 'keytable-previous-ref))
(provide 'keytable)
;;; end of keytable.el



