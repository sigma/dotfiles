;;; visible-mark-mode.el --- Make marks visible.

;;; Commentary:

;; This was hacked together by Jorgen Schäfer
;; And hacked again by Yann Hodique
;; Donated to the public domain. Use at your own risk.

;;; History:
;; 2008-01-31  MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * visible-mark.el: Create formal emacs lisp file from
;;        http://www.emacswiki.org/cgi-bin/wiki/VisibleMark.
;;        Yann Hodique and Jorgen Schäfer are original author.
;;        Addded function to make multiple marks visible.
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup visible-mark nil
  "Show the position of your mark."
  :group 'convenience
  :prefix "visible-mark-")

(defface visible-mark-default-face
  `((((type tty) (class color))
     (:background "gray" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "gray"))
    (((class color) (background light))
     (:background "grey80"))
    (t (:background "gray")))
  "Face for the mark."
  :group 'visible-mark)

(defface visible-mark-face1
  `((((type tty) (class color))
     (:background "gray" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "gray"))
    (((class color) (background light))
     (:background "wheat3"))
    (t (:background "gray")))
  "Face for the 1st mark."
  :group 'visible-mark)


(defface visible-mark-face2
  `((((type tty) (class color))
     (:background "gray" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "gray"))
    (((class color) (background light))
     (:background "wheat2"))
    (t (:background "gray")))
  "Face for the 2nd mark."
  :group 'visible-mark)

(defface visible-mark-face3
  `((((type tty) (class color))
     (:background "gray" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "gray"))
    (((class color) (background light))
     (:background "wheat1"))
    (t (:background "gray")))
  "Face for the 3rd mark."
  :group 'visible-mark)

(defvar visible-mark-faces
  '(visible-mark-face1 visible-mark-face2 visible-mark-face3)
  "The list of mark faces.")

(defvar visible-mark-overlays nil
  "The overlays used in this buffer.")
(make-variable-buffer-local 'visible-mark-overlays)

(defvar visible-mark-max 5
  "A number of mark to be visible.")

(defun visible-mark-move-overlays ()
  "Move the overlay in `visible-mark-overlay' to a new position."
  (let ((marks (cons (mark-marker) mark-ring))
        (overlays visible-mark-overlays))
    (dotimes (i visible-mark-max)
      (let ((mark (car-safe marks))
            (overlay (car overlays)))
        (if (and mark (marker-position mark))
            (let ((pos (marker-position mark)))
              (save-excursion
                (goto-char pos)
                (if (and (eolp)
                         (goto-char (1- pos))
                         (not (eolp)))
                    (decf pos)))
              (move-overlay overlay pos (1+ pos))
              (setq marks (cdr marks))))
        (setq overlays (cdr overlays))))))

(require 'easy-mmode)

(defcustom global-visible-mark-mode-exclude-alist nil
  "A list of buffer names to be excluded"
  :group 'visible-mark
  :type '(repeat regexp))

(defun visible-mark-mode-maybe ()
  (when (cond
         ((minibufferp (current-buffer)) nil)
         ((flet ((fun (arg)
                      (if (null arg) nil
                        (or (string-match (car arg) (buffer-name))
                            (fun (cdr arg))))))
            (fun global-visible-mark-mode-exclude-alist)) nil)
         (t t))
    (visible-mark-mode)))

(define-minor-mode visible-mark-mode
  "A mode to make the mark visible."
  nil nil nil
  :group 'visible-mark
  (if visible-mark-mode
      (progn
        (dotimes (i visible-mark-max)
          (let ((overlay (make-overlay (point-min) (point-min))))
            (overlay-put overlay 'face (or (nth i visible-mark-faces) 'visible-mark-default-face))
            (push overlay visible-mark-overlays)))
        (setq visible-mark-overlays (nreverse visible-mark-overlays))
        (visible-mark-move-overlays)
        (add-hook 'post-command-hook 'visible-mark-move-overlays nil t))
    (mapcar 'delete-overlay visible-mark-overlays)
    (setq visible-mark-overlays nil)
    (remove-hook 'post-command-hook 'visible-mark-move-overlays t)))

(define-global-minor-mode
  global-visible-mark-mode visible-mark-mode visible-mark-mode-maybe
  :group 'visible-mark)

(provide 'visible-mark-mode)