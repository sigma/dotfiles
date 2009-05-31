;;; color-mode.el --- minor mode for colorizing lines of a file

;; Don Knuth, August 2000
;; (I use this for files that inventory my library and preprints etc.)

;; When using this mode you can colorize the current line with various
;; colors called color-@, color-a, color-b, etc., by saying
;; "C-c@", "C-ca", "C-cb", etc. And C-c DEL will uncolorize a colorized line.

;; (The Emacs Lisp Reference Manual says that minor modes should not
;; bind these keys --- they are "reserved for users" --- but it offers no
;; decent alternatives. The "c" in "C-c" stands for "color", so I'm using it.)

;; The colors are defined below. I could of course make them
;; more customizable, but since I'm the only user I didn't bother.

;; The stored file begins with "Color-Mode\n" and has ^@ or ^A or ^B
;; or ... at the beginning of each colorized line. The displayed file
;; uses the conventions of format.el in a simple way, sort of like a very
;; impoverished kind of enriched text.

;; I wrote and tested this with Emacs 20.4. It may fail with other versions.

;; It should be possible to use color mode together with enriched mode,
;; at least in simple files that don't depend on justification.
;; But you can't do that with Emacs 20.4 because of a bug in write-region
;; that I have just reported to GNU.

(make-empty-face 'color-mode-face-@)
(make-empty-face 'color-mode-face-a)
(make-empty-face 'color-mode-face-b)
(make-empty-face 'color-mode-face-c)
(make-empty-face 'color-mode-face-d)
(make-empty-face 'color-mode-face-e)
(make-empty-face 'color-mode-face-f)
(make-empty-face 'color-mode-face-g)
(make-empty-face 'color-mode-face-h)
;; params here are: foreground background stipple bold ital underline inverse
(modify-face 'color-mode-face-@ nil "darkolivegreen" nil nil nil nil nil)
(modify-face 'color-mode-face-a "green" nil nil nil nil nil nil)
(modify-face 'color-mode-face-b "orange" nil nil nil nil nil nil)
(modify-face 'color-mode-face-c "red" nil nil nil nil nil nil)
(modify-face 'color-mode-face-d "violet" nil nil nil nil nil nil)
(modify-face 'color-mode-face-e "lightblue" nil nil nil nil nil nil)
(modify-face 'color-mode-face-f "blue" "orange" nil nil nil nil nil)
(modify-face 'color-mode-face-g "brown" "white" nil nil nil t nil)
(modify-face 'color-mode-face-h "black" "brown" nil nil nil t nil)

(defconst color-mode-faces
  '(color-mode-face-@ color-mode-face-a color-mode-face-b
    color-mode-face-c color-mode-face-d color-mode-face-e
    color-mode-face-f color-mode-face-g color-mode-face-h)
    "List of the faces available in color mode.")

(defvar color-mode nil
  "True if Color mode is in use.")
(make-variable-buffer-local 'color-mode)
(put 'color-mode 'permanent-local t) ;; preserve color across major mode change
(setq-default color-mode nil)

(or (assq 'color-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(color-mode " Color") minor-mode-alist)))

(defvar color-mode-map nil
  "Keymap for Color Mode.")

(if (null color-mode-map)
    (fset 'color-mode-map (setq color-mode-map (make-sparse-keymap))))

(if (not (assq 'color-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
          (cons (cons 'color-mode color-mode-map)
                minor-mode-map-alist)))

(define-key color-mode-map "\C-c@" 'color-mode-paint-line-with-color-@)
(define-key color-mode-map "\C-ca" 'color-mode-paint-line-with-color-a)
(define-key color-mode-map "\C-cb" 'color-mode-paint-line-with-color-b)
(define-key color-mode-map "\C-cc" 'color-mode-paint-line-with-color-c)
(define-key color-mode-map "\C-cd" 'color-mode-paint-line-with-color-d)
(define-key color-mode-map "\C-ce" 'color-mode-paint-line-with-color-e)
(define-key color-mode-map "\C-cf" 'color-mode-paint-line-with-color-f)
(define-key color-mode-map "\C-cg" 'color-mode-paint-line-with-color-g)
(define-key color-mode-map "\C-ch" 'color-mode-paint-line-with-color-h)
(define-key color-mode-map "\C-c\C-?" 'color-mode-unpaint-line)

(setq format-alist (cons
  '(colormode "Color-Mode format" "Color-Mode\n"
              color-decode color-encode t color-mode)
  format-alist))

(defun color-mode-paint-line-with-color-@ ()
  (interactive)(color-mode-paint-line 0))
(defun color-mode-paint-line-with-color-a ()
  (interactive)(color-mode-paint-line 1))
(defun color-mode-paint-line-with-color-b ()
  (interactive)(color-mode-paint-line 2))
(defun color-mode-paint-line-with-color-c ()
  (interactive)(color-mode-paint-line 3))
(defun color-mode-paint-line-with-color-d ()
  (interactive)(color-mode-paint-line 4))
(defun color-mode-paint-line-with-color-e ()
  (interactive)(color-mode-paint-line 5))
(defun color-mode-paint-line-with-color-f ()
  (interactive)(color-mode-paint-line 6))
(defun color-mode-paint-line-with-color-g ()
  (interactive)(color-mode-paint-line 7))
(defun color-mode-paint-line-with-color-h ()
  (interactive)(color-mode-paint-line 8))

;; In color mode the normal stickiness conventions are reversed, because
;; we want to type at the beginning of a colorized line and inherit the
;; color of that line. Similarly, we want to type at the beginning of
;; a non-colorized line without inheriting the color of the newline
;; that ends the previous line. (I want the newline character to be
;; colorized, so that painting always extends to the right margin.)
;; The newline is made rear-nonsticky. All characters of a painted line
;; are made front-sticky; I don't want to do this just to the first character,
;; because the user can delete it.

;; If this mode is going to be used seriously in connection with other modes,
;; I could be more careful to confine the stickiness switching to the
;; text properties I control and not to any other attributes. But the
;; design of Emacs Lisp makes that rather painful, so I simply set
;; front-sticky and rear-nonsticky to `t' in this version of the code.
;; I'll let other people hack more generality into the routine if they
;; think it is worth their time.

(defun color-mode-paint-line (n)
  "Displays the current line using the nth face of color-mode-faces."
  (save-excursion
    (let (beg end)
      (beginning-of-line 1) (setq beg (point))
      (beginning-of-line 2) (setq end (point))
      (put-text-property beg end 'face (nth n color-mode-faces))
      (put-text-property beg end 'color-mode n)
      (put-text-property beg end 'front-sticky t)
      (put-text-property (1- end) end 'rear-nonsticky t))))

(defun color-mode-unpaint-line ()
  "Removes the color-mode face, if any, associated with the current line."
  (interactive)
  (save-excursion
    (let (beg end)
      (beginning-of-line 1) (setq beg (point))
      (if (get-text-property beg 'color-mode)
          (progn
            (beginning-of-line 2) (setq end (point))
            (remove-text-properties beg end '(face nil color-mode nil))
            (remove-text-properties beg (1+ beg) '(front-sticky nil))
            (remove-text-properties (1- end) end '(rear-nonsticky nil)))))))

(defun color-mode (&optional arg)
  "Minor mode for editing colorized files.

\\<color-mode-map>\\{color-mode-map}"
  (interactive "P")
  (let ((mod (buffer-modified-p)))
    (cond ((or (<= (prefix-numeric-value arg) 0)
               (and color-mode (null arg)))
           (setq color-mode nil) ;;; negative arg turns mode off
           (setq buffer-file-format (delq 'colormode buffer-file-format)))
          (color-mode nil) ;;; if already on, we don't do anything
          (t (setq color-mode t) ;;; otherwise turn it on
             (add-to-list 'buffer-file-format 'colormode)))
    (set-buffer-modified-p mod)
    (force-mode-line-update)))

(defun color-decode (from to)
 (let ((save-undos buffer-undo-list)) (setq buffer-undo-list t)
  (save-restriction
    (narrow-to-region from to)
    (goto-char from)
    (delete-char 11) ;; delete "Color-Mode\n"
    (while (not (eobp))
      (let ((ch (following-char)))
        (if (< ch 9) ;; test if first char of line is ^@, ^A, ... or ^H
            (progn
              (delete-char 1)
              (color-mode-paint-line ch)))
        (next-line 1)))
 (setq buffer-undo-list save-undos)
    (point-max))))

(defun color-encode (from to orig-buf)
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (insert "Color-Mode\n")
      (while (not (eobp))
        (let* ((pos (point)) (ch (get-text-property pos 'color-mode)))
          (while ch
            (if (= (char-before pos) ?\n) (insert ch))
            (beginning-of-line 2)
            (setq pos (point) ch (get-text-property pos 'color-mode)))
          (goto-char (or (next-single-property-change pos 'color-mode)
                         (point-max)))))
    (point-max)))