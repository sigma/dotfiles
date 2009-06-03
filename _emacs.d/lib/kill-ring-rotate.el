;;; kill-ring-rotate.el --- Interactive rotation of the kill-ring

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it with no restriction,

;;; Commentary:

;; Version 1.5 : October 27, 2005
;; - set mark before yanking. (suggested by Florent Georges)
;;
;; Version 1.4 : September 08, 2005
;; - Bugfix: Don't consider inter-application pasting in kill-ring-rotate.
;;
;; Version 1.3 : April 4th, 2005
;; - Q: new binding
;;
;; Version 1.2 : December 30, 2003
;; - Removed the dependency on boxquote.el (Replaced the boxquote by a
;; face.)
;; - Can now actually move in the buffer (particularly using C-s ...)
;; and type RET to move to the current kill-ring entry.
;;
;; Version 1.1 : December 26, 2003
;; - bindings for the keys 'n', 'p', 'prior', 'next'
;;
;; Version 1.0 : December 25, 2003
;;
;;; Code:

(defvar kill-ring-rotate-number-of-lines 20
"*Number of kill-ring elements displayed by kill-ring-rotate")

(defvar kill-ring-rotate-buffer nil)

(defface kill-ring-rotate-highlight-face
  '((((type tty) (class color)) (:foreground "green"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Face to highlight the current element of the kill-ring")

;;;###autoload

(defun kill-ring-rotate ()
  "Displays a buffer with the previous and next pieces of text in the
kill ring. RET will insert the current kill-ring text in the buffer
from which the function was called, and C-c C-c or `q' exits.

Navigate through the kill-ring with <up> and <down>"
  (interactive)
  (let ((previous-buffer (current-buffer)))
    (switch-to-buffer
     (get-buffer-create "*Kill ring rotate*"))
    (kill-ring-rotate-display)
    (local-set-key (kbd "<up>") 'kill-ring-rotate-up)
    (local-set-key (kbd "<down>") 'kill-ring-rotate-down)
    (local-set-key (kbd "p") 'kill-ring-rotate-up)
    (local-set-key (kbd "n") 'kill-ring-rotate-down)
    (local-set-key (kbd "<prior>") 'kill-ring-rotate-up-fast)
    (local-set-key (kbd "<next>") 'kill-ring-rotate-down-fast)
    (local-set-key (kbd "RET") 'kill-ring-rotate-go-back)
    (local-set-key (kbd "SPC") 'kill-ring-rotate-display)
    (local-set-key (kbd "C-c C-c") 'kill-ring-rotate-escape)
    (local-set-key (kbd "q") 'kill-ring-rotate-escape)
    (local-set-key (kbd "Q") 'kill-ring-rotate-exit)
    (local-set-key (kbd "0") 'digit-argument)
    (local-set-key (kbd "1") 'digit-argument)
    (local-set-key (kbd "2") 'digit-argument)
    (local-set-key (kbd "3") 'digit-argument)
    (local-set-key (kbd "4") 'digit-argument)
    (local-set-key (kbd "5") 'digit-argument)
    (local-set-key (kbd "6") 'digit-argument)
    (local-set-key (kbd "7") 'digit-argument)
    (local-set-key (kbd "8") 'digit-argument)
    (local-set-key (kbd "9") 'digit-argument)
    (make-local-variable 'kill-ring-rotate-buffer)
    (setq kill-ring-rotate-buffer previous-buffer)
    ))

(defun kill-ring-rotate-go-back ()
  (interactive)
  (let ((n (get-text-property (point) 'kill-ring-rotate-number)))
    (if (= n 0)
        (progn
          (switch-to-buffer kill-ring-rotate-buffer)
          (let (interprogram-paste-function)
            (set-mark (point))
            (insert (current-kill 0))))
      (kill-ring-rotate-num n))))

(defun kill-ring-rotate-escape ()
  (interactive)
  (switch-to-buffer kill-ring-rotate-buffer))

(defun kill-ring-rotate-exit ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun kill-ring-rotate-down (arg)
  (interactive "p")
  (kill-ring-rotate-num (or arg 1)))

(defun kill-ring-rotate-up (arg)
  (interactive "p")
  (kill-ring-rotate-num (- (or arg 1))))

(defun kill-ring-rotate-down-fast (arg)
  (interactive "p")
  (kill-ring-rotate-num (* kill-ring-rotate-number-of-lines
                           (or arg 1))))

(defun kill-ring-rotate-up-fast (arg)
  (interactive "p")
  (kill-ring-rotate-num (* kill-ring-rotate-number-of-lines
                           (- (or arg 1)))))

(defun kill-ring-rotate-num (n)
  (interactive)
  (current-kill n)
  (kill-ring-rotate-display)
  )

(defun kill-ring-rotate-display-n (n sig index)
  (if (/= n 0)
      (progn
        (let ((begin (point))
              (end (progn
                     (insert (current-kill sig))
                     (insert  "\n=======================\n")
                     (point))))
          (put-text-property begin end 'kill-ring-rotate-number index))
        (kill-ring-rotate-display-n (- n 1) sig (+ index sig)))))

(defvar kill-ring-rotate-point nil)

(defun kill-ring-rotate-display ()
  (interactive)
  (delete-region (point-min)(point-max))
  (current-kill (- -1 kill-ring-rotate-number-of-lines))
  (kill-ring-rotate-display-n kill-ring-rotate-number-of-lines 1
                              (- kill-ring-rotate-number-of-lines))
  (let* ((begin (point))
         (end-face (progn (insert (current-kill 1))
                          (newline)
                          (point)))
         (end-prop (progn (insert  "=======================\n")
                          (point))))
    (put-text-property begin end-prop 'kill-ring-rotate-number 0)
    (put-text-property begin end-face 'face 'kill-ring-rotate-highlight-face)
    (kill-ring-rotate-display-n kill-ring-rotate-number-of-lines 1
                                1)
    (current-kill (- kill-ring-rotate-number-of-lines))
    (goto-char begin))
)

(provide 'kill-ring-rotate)
;;; kill-ring-rotate.el ends here
