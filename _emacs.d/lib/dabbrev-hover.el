;;; dabbrev-hover.el --- Tooltip-show the next completion, as does openoffice
;; Time-stamp: <22/06/2004 15:45:54 Yann Hodique>
;; Copyright (C) 2004 D. Goel
;; Emacs Lisp Archive entry
;; Filename: dabbrev-hover.el
;; Package: dabbrev-hover
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:
;; Version:  0.3DEV
;; URL: http://gnufans.net/~deego

;; Acknowledgements:
;; Uwe Brauer
;; Kevin Rodgers

;; For latest version:

(defconst dabbrev-hover-home-page
  "http://gnufans.net/~deego")

;; This file is NOT (yet) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; See also:

;; Quick start:
(defconst dabbrev-hover-quick-start
  " Type M-x dabbrev-hover-introduction.

"
)

(defun dabbrev-hover-quick-start ()
  "Provides electric help from variable `dabbrev-hover-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert dabbrev-hover-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst dabbrev-hover-introduction
  "With dh-mode, when you are typing a long word, like
\\begin{equation}, you see a possible completion hovering next to what
you type. Press TAB to complete the completion.  This is similar to
Openoffice's behavior. The key TAB continues to work normally
otherwise.

Add something like the following to .emacs, according to your tastes.
\(require 'cl)
\(require 'dabbrev-hover)
\(dabbrev-hover-install t t)

The first argument of t installs this mode globally, for all buffers.
The second argument of t installs the fancy TAB option described
above.  If you don't install the fancy version (or if you work on
non-windowed systems where the fancy version doesn't work at this
time), you can still dedicate a special key to do M-x dh-complete,
which completes to the shown expansion.

 (global-set-key \"\\C-cc\" 'dh-complete) to your .emacs.

To uninstall in a running emacs, M-x dabbrev-hover-uninstall
"
)

;;;###autoload
(defun dabbrev-hover-introduction ()
  "Provides electric help from variable `dabbrev-hover-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert dabbrev-hover-introduction) nil) "*doc*"))

;;; Commentary:
(defconst dabbrev-hover-commentary
  "Help..."
)

(defun dabbrev-hover-commentary ()
  "Provides electric help from variable `dabbrev-hover-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert dabbrev-hover-commentary) nil) "*doc*"))

;;; History:

;; Acknowledgements:

;; Uwe Brauer for helping with Xemacs compatibility (doesn't mean we
;;  are necc. compatible yet).

;;; Bugs:
;; IS AN INITIAL RELEASE, NO IDEA IF IT BREAKS THE FUNCTIONALITY OF DABBREV.EL

;; Buglet: You see undesirable "Scanning buffer" messages.  This
;; comes from dabbrev-mode, we don't know how to turn this off.

;; Bug : Uses dabbrev.el, No idea how it reacts  with dabbrev.el.

;; Bug: Fancy mode doesn't work in -nw'ed mode for some reason.

;; Does it make more sense to do our stuff in post-command-hook for
;; self-insert commands, instead of a timer?

;; TODO: Make tooltip stay longer.
;; TODO: Change tooltip color to blue.. like openoffice
;; TODO: min-length etc.   .. more generally into a function..

;;; New features:

(defconst dabbrev-hover-new-features
  "New since last post: The timer is now an idle-timer by
default.  That should prevent any timer-floodings, which may
otherwise occur on slower systems.  Heh, a bunch of flooding of
lots of eye-candy in my Emacs lately, dabbrev-hover,
highlight-tail from Rafal Jedruszek, undo-browse, and the normal
syntax-highlighting, they all seem to work together.."  )

(defun dabbrev-hover-new-features ()
  "Provides electric help from variable `dabbrev-hover-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert dabbrev-hover-new-features) nil) "*doc*"))

;;; TO DO:
(defconst dabbrev-hover-todo
  "Help..."
)

(defun dabbrev-hover-todo ()
  "Provides electric help from variable `dabbrev-hover-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert dabbrev-hover-todo) nil) "*doc*"))

(defconst dabbrev-hover-version "0.3dev")
(defun dabbrev-hover-version (&optional arg)
   "Display dabbrev-hover's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "dabbrev-hover version %s" dabbrev-hover-version))
    (message "dabbrev-hover version %s" dabbrev-hover-version)))

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))
(require 'dabbrev)
(when window-system
  (require 'tooltip))
;;; Code:

(defgroup dabbrev-hover nil
  "The group dabbrev-hover."
  :group 'applications)
(defcustom dabbrev-hover-before-load-hook nil
  "Hook to run before loading dabbrev-hover."
  :group 'dabbrev-hover)
(defcustom dabbrev-hover-after-load-hook nil
  "Hook to run after loading dabbrev-hover."
  :group 'dabbrev-hover)
(run-hooks 'dabbrev-hover-before-load-hook)

(defcustom dabbrev-hover-verbosity 0
  "How verbose to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
rest are for debugging."
  :type 'integer
  :group 'dabbrev-hover)
(defcustom dabbrev-hover-interactivity 0
  "How interactive to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'integer
  :group 'dabbrev-hover)
(defcustom dabbrev-hover-y-or-n-p-function 'dabbrev-hover-y-or-n-p
  "Function to use for interactivity-dependent  `y-or-n-p'.
Format same as that of `dabbrev-hover-y-or-n-p'."
  :type 'function
  :group 'dabbrev-hover)
(defcustom dabbrev-hover-n-or-y-p-function 'dabbrev-hover-y-or-n-p
  "Function to use for interactivity-dependent `n-or-y-p'.
Format same as that of `dabbrev-hover-n-or-y-p'."
  :type 'function
  :group 'dabbrev-hover)
(defun dabbrev-hover-message (points &rest args)
  "Signal message, depending on POINTS anddabbrev-hover-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points dabbrev-hover-verbosity))
    (apply #'message args)))
(defun dabbrev-hover-y-or-n-p (add prompt)
  "Query or assume t, based on `dabbrev-hover-interactivity'.
ADD is added to `dabbrev-hover-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add dabbrev-hover-interactivity))
        t
      (funcall 'y-or-n-p prompt)))
(defun dabbrev-hover-n-or-y-p (add prompt)
  "Query or assume t, based on `dabbrev-hover-interactivity'.
ADD is added to `dabbrev-hover-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add dabbrev-hover-interactivity))
      nil
    (funcall 'y-or-n-p prompt)))

;;; Real Code:

(defun dh-point-position ()
  "An ugly hack until such time as emacs provides us this
  functionality.  Should return (<buffername> x . y). "
  (require 'avoid)
  (mouse-avoidance-point-position))

(defun dh-point-pixel-position ()
  "An ugly hack until such time as emacs provides us this
  functionality.  Should return (<buffername> x . y). "
  (let* ((mouse-old-pos (mouse-position))
	 ans)
    (dh-set-mouse-position
     (dh-point-position))
    (setq ans (mouse-pixel-position))
    (dh-set-mouse-position mouse-old-pos)
    ans))

(defun dh-set-mouse-position (fxy)
  (let* ((f (car fxy))
	 (xy (cdr fxy))
	 (x (car xy))
	 (y (cdr xy)))
    (set-mouse-position f x y)))

(defun dh-point-pixel-position-old ()
  "An ugly hack until such time as emacs provides us this
  functionality.  Should return (<buffername> x . y). "
  (let* (
	 (okp t)
	 ptpos fram x y
	 mp mpxy
	 mpx
	 mpy
	 mpp
	 mppxy
	 mppx
	 mppy
	 pxratio
	 pyratio)
    (setq ptpos (dh-point-position))
    (setq fram (first ptpos))
    (setq x (second ptpos))
    (setq y (cdr (last ptpos)))
    (setq mp (mouse-position))
    (setq mpxy (last mp))
    (setq mpx (car mpxy))
    (setq mpy (cdr mpxy))
    (setq mpp (mouse-pixel-position))
    (setq mppxy (last mpp))
    (setq mppx (car mppxy))
    (setq mppy (cdr mppxy))
    (setq okp (and (numberp x) (numberp y) (numberp mpx) (numberp mpy)
		   (numberp mppx) (integerp mppy)
		   (> mpx 0) (> mpy 0)))
    (cond
     (okp
      (setq pxratio (/ (* 1.0 mppx) mpx))
      (setq pyratio (/ (* 1.0 mppy) mpy))
      (cons (selected-frame)
	    (cons (round (* x pxratio)) (round (* y pyratio)))))
     (t (cons (selected-frame) (cons nil nil))))))

(defcustom dh-mode-string " DH" "")

(defvar dh-mode-map
  (make-sparse-keymap))

(easy-mmode-define-minor-mode
 dh-mode
 "The mode to inherit minibuffer keybindings"
 nil
 dh-mode-string
 ;; 3 means C-c
 ;; 16 means C-p
 'dh-mode-map)

(defalias 'dabbrev-hover-mode-off 'dh-mode-off)

(defvar dabbrev-mode-on-hook)
(defvar dabbrev-mode-off-hook)

(add-hook 'dh-mode-on-hook
	   'dh-start-timer-maybe)
(add-hook 'dh-mode-off-hook
	   'dh-stop-timer)

(defvar dh-timer nil)

(defcustom dh-interval 0.2 "")
(defcustom dh-interval-initial 0.2 "")
(defcustom dh-use-timer-p t
  "Whether to use timers instead of post-command hook for doing the
dh thng.  t is the only option to use at this time")

(defun dh-start-timer ()
  (dh-stop-timer)
  (setq dh-timer
	(dh-run-with-timer dh-timer-type
			   (eval dh-interval-initial) (eval  dh-interval)
			   'dh-once)))

(defcustom dh-timer-type 'idle
  "Choices are 'idle, 'timer.  When idle, we use an idle timer to do
  our thing.

The author prefers the 'idle type.")

(defun dh-run-with-timer (timer-type interval-initial interval
				     function &rest args)
  (setq dh-timer
	(case dh-timer-type
	  ('idle
	   (apply 'run-with-idle-timer
		  (eval dh-interval-initial) (eval  dh-interval)
		  'dh-once args))
	  (t
	   (apply 'run-with-timer
		  (eval dh-interval-initial) (eval  dh-interval)
		  'dh-once args)))))

(defun dh-start-timer-maybe ()
  (when dh-use-timer-p
    (dh-start-timer)))

;;;###autoload
(defun dabbrev-hover-start ()
  (dh-start-timer)
  (dh-mode 1))

(defun dabbrev-hover-stop ()
  (dh-stop-timer)
  (dh-mode -1))

(defun dabbrev-hover-start-globally ()
  (dabbrev-hover-start)
  (setq-default dh-mode 1))

(defun dh-stop-timer ()
  (when (timerp dh-timer)
    (cancel-timer dh-timer)))

(defvar dh-last-point nil)
(defvar dh-last-completion  nil)

(defcustom dh-abbrev-at-point-function 'dh-abbrev-at-point "")
(defun dh-abbrev-at-point ()
  (save-match-data
    (save-excursion
      (let ((pt (point))
	    res answer)
	;;(backward-char 1)
	(cond
	 ((search-backward-regexp dh-abbrev-regexp
				  nil t)
	  (buffer-substring-no-properties
	   (match-end 0) pt))
	 (t (buffer-substring-no-properties (point-min) pt)))))))

(defcustom dh-abbrev-regexp "[\t\n ]" "")

(defcustom dh-user-conditions-p
  nil
  "A list of functions.  Only when each of those functions return
  non-nil, do we supply a tooltip at any time. ")

(defvar dh-debug-p nil)
;; (setq dh-debug-p t)
(defun dh-once ()
  "Runs only when we are at a new point. "
  (interactive)
  ;;(message "running!")

  (cond
   (
    (or dh-debug-p
	(and
	 (equal last-command 'self-insert-command)
	 dh-mode
	 (not (equal (point) dh-last-point))
	 (funcall 'every
		  'identity
		  (mapcar 'funcall
			  dh-user-conditions-p))
	 (member (following-char)
		 '(0  ;; end of file
		   32 ;; space
		   9 ;; tab
		   10 ;; newline
		   ))))
    (setq dh-last-point (point))
    (dabbrev--reset-global-variables)
    (let* ((abbrev  (ignore-errors (funcall dh-abbrev-at-point-function)))
	   (completion
	    (and (stringp abbrev)
		 (>= (length abbrev) dh-min-length-abbrev)
		 (dh-dabbrev--find-expansion abbrev))))
      (when
	  (and (stringp completion)
	       (>= (length completion) dh-min-length-expansion))
	(setq dh-last-completion completion)
	(dh-tooltip-show-at-point completion)
	(run-hooks 'dh-after-hover-hook))))))

(defcustom dh-min-length-abbrev 3 "")
(defcustom dh-min-length-expansion 6 "")

(defcustom dh-after-hover-hook
  nil "")

(defun dh-insert-space (&rest args)
  (insert " "))

(defun dh-dabbrev--find-expansion (abbrev)
  (dabbrev--find-expansion
   abbrev
   (or dabbrev--last-direction 0)
   (and (if (eq dabbrev-case-fold-search 'case-fold-search)
	    case-fold-search
	  dabbrev-case-fold-search)
	(or (not dabbrev-upcase-means-case-search)
	    (string= abbrev (downcase abbrev))))
   ))

(defcustom dh-offset-px
  ;;310
  40
  "Pixel offset for x")

;; (setq dh-offset-px 0)
;; (setq dh-offset-py 0)

(defcustom dh-offset-py
  ;;200
  15
  "Pixel offset for y")

(defun dh-tooltip-show-at-point-old (str)
  (let* ((bxy (dh-point-pixel-position))
	 (xy (last bxy))
	 (x (car xy)) (y (cdr xy))
	 (tooltip-frame-parameters
	  (cond
	   ((and (integerp x) (integerp y))
	    (append
	     (list (cons 'left (+ x dh-offset-px))
		   (cons 'top (+ y dh-offset-py)))
	     (and window-system tooltip-frame-parameters)))
	   (t (and window-system tooltip-frame-parameters)))))
    (cond
     ((and dh-tooltip-type
	   window-system)
      (tooltip-show str))
     (t (message "Completion: %s" str)))))

(defun dh-tooltip-show-at-point (str)
  (cond
   ((and dh-tooltip-type
	 window-system)
    (let* ((tooltip-x-offset dh-offset-px)
	   (tooltip-y-offset dh-offset-py)
	   ;; mouse position old
	   (mpo (mouse-position)))
      (dh-set-mouse-position (dh-point-position))
      (tooltip-show str)
      (dh-set-mouse-position mpo)))
   (t (message "Completion: %s" str))))

(defvar dh-tooltip-type t
  "When nil, we use the minibuffer. ")

(defvar dh-tooltip-last-msg nil)

(defun dh-complete ()
  (interactive)
  (let ((gop nil)
	(pt (point))
	(dlc dh-last-completion)
	(abbrev (funcall dh-abbrev-at-point-function)))
    (when
	(and
	 (equal dh-last-point pt)
	 (stringp dh-last-completion)
	 (equal 0 (string-match (regexp-quote abbrev) dlc)))

      (insert (substring dlc (length abbrev)))
      (run-hook-with-args 'dh-complete-after-insert-hook abbrev
			  dlc))))

(defcustom dh-complete-after-insert-hook nil

  "The 2 arguments each of the functions gets here, are abbrev and
  the completion.
 You might want to set this to   '(dh-insert-space) depending on your taste.
")

;;;###autoload
(defun dabbrev-hover-install (&optional globalp fancyp)
  (interactive)
  (cond
   (globalp (dabbrev-hover-start-globally))
   (t (dabbrev-hover-start)))
  (cond
   (fancyp
    (add-hook 'post-command-hook 'dh-fancy-doing-mode-off)
    (add-hook 'dh-after-hover-hook 'dh-fancy-doing-mode-on)
    ;;
    ;; the above won't work for window system, so:
    ;;(unless window-system
    ;;(global-set-key (kbd "C-TAB") 'dh-complete))
    )
   (t
    (global-set-key (kbd "C-<return>") 'dh-complete)
    ;;(global-set-key (kbd "C-TAB") 'dh-complete)
    ))
  (dabbrev-hover-message 0 "dabbrev-hover installed. "))

(defun dabbrev-hover-uninstall ()
  (interactive)
  (setq-default dh-mode nil)
  (save-excursion
    (mapcar
     (lambda (arg)
       (set-buffer arg)
       (dh-mode -1)
       (dh-fancy-doing-mode -1))
     (buffer-list)))
  (setq-default dh-fancy-doing-mode -1)
  (dh-stop-timer)
  (remove-hook 'dh-after-hover-hook 'dh-fancy-doing-mode-on)
  (remove-hook 'post-command-hook 'dh-fancy-doing-mode-off)
  )

(defvar dh-fancy-doing-mode-map
  '(keymap))

;;(define-key dh-fancy-doing-mode-map (kbd "RET") 'dh-complete)
(define-key dh-fancy-doing-mode-map (kbd "TAB") 'dh-complete)

(defcustom dh-fancy-doing-mode-string nil "")
;; (setq dh-fancy-doing-mode-string "DEBUG FANCY")
(easy-mmode-define-minor-mode dh-fancy-doing-mode
 "The mode to inherit minibuffer keybindings" nil
 dh-fancy-doing-mode-string
 ;; 3 means C-c
 ;; 16 means C-p
 'dh-fancy-mode-map)
(defun dh-fancy-doing-mode-off ()
  (dh-fancy-doing-mode -1))

(defun dh-fancy-doing-mode-on ()
  (dh-fancy-doing-mode 1))

(provide 'dabbrev-hover)
(run-hooks 'dabbrev-hover-after-load-hook)

;;; dabbrev-hover.el ends here
