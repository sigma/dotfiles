;; This was hacked together by Jorgen Schäfer
;; And hacked again by Yann Hodique
;; Donated to the public domain. Use at your own risk.

(defgroup visible-mark nil
  "Show the position of your mark."
  :group 'convenience
  :prefix "visible-mark-")

(defface visible-mark-face
  `((((type tty) (class color))
     (:background "blue" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "blue"))
    (((class color) (background light))
     (:background "lightblue"))
    (t (:background "gray")))
  "Face for the mark."
  :group 'visible-mark)

(defvar visible-mark-overlay nil
  "The overlay used in this buffer.")
(make-variable-buffer-local 'visible-mark-overlay)

(defun visible-mark-move-overlay ()
  "Move the overlay in `visible-mark-overlay' to a new position."
  (move-overlay visible-mark-overlay
                (mark)
                (1+ (mark))))

(require 'easy-mmode)

(define-minor-mode visible-mark-mode
  "A mode to make the mark visible."
  nil nil nil
  :group 'visible-mark
  (if visible-mark-mode
      (progn
        (unless (mark)
          (set-mark (point-min)))
        (unless visible-mark-overlay
          (setq visible-mark-overlay (make-overlay (mark)
                                                   (1+ (mark))))
          (overlay-put visible-mark-overlay 'face 'visible-mark-face)
          (add-hook 'post-command-hook 'visible-mark-move-overlay)))
      (when visible-mark-overlay
        (delete-overlay visible-mark-overlay)
        (setq visible-mark-overlay nil))))

(easy-mmode-define-global-mode
 global-visible-mark-mode visible-mark-mode visible-mark-mode :group visible-mark)

(provide 'visible-mark-mode)
