;;; isearchb.el --- a marriage between iswitchb and isearch

;; Copyright (C) 2004 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Created: 16 Apr 2004
;; Version: 1.3
;; Keywords: lisp
;; X-URL: http://www.newartisans.com/johnw/emacs.html

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This module allows you to switch to buffers even faster than with
;; iswitchb!  It is not intended to replace it, however, as it works
;; well only with buffers whose names don't typically overlap.  You'll
;; have to try it first, and see how your mileage varies.
;;
;; The first way to use isearchb is by holding down a modifier key, in
;; which case every letter you type while holding it searches for any
;; buffer matching what you're typing (using the same ordering scheme
;; employed by iswitchb).  To use it this way, add to your .emacs:
;;
;;   (isearchb-set-keybindings 'super)  ; s-x s-y s-z now finds "xyz"
;;
;; The other way is by using a command that puts you into "search"
;; mode, just like with isearch.  I use C-z for this.  The binding in
;; my .emacs looks like:
;;
;;   (define-key global-map [(control ?z)] 'isearchb-activate)
;;
;; Now, after pressing C-z (for example), each self-inserting
;; character thereafter will search for a buffer containing those
;; characters.  For instance, typing "C-z xyz" will switch to the
;; first buffer containing "xyz".  Once you press a non-self-inserting
;; character (such as any control key sequence), the search will end.
;;
;; C-z after C-z toggles between the previously selected buffer and
;; the current one.
;;
;; C-g aborts the search and returns you to your original buffer.
;;
;; TAB, after typing in a few characters (after C-z), will jump into
;; iswitchb, using the prefix you've typed so far.  This is handy when
;; you realize that isearchb is not powerful enough to find the buffer
;; you're looking for.
;;
;; C-s and C-r move forward and backward in the buffer list.  If
;; `isearchb-show-completions' is non-nil (the default), a list of
;; possible completions is shown in the minibuffer.
;;
;; If `isearchb-idle-timeout' is set to a number (the default is 1),
;; isearchb will quit after that many seconds of idle time.  Thus, if
;; you switch to a buffer and wait for a second, you can start typing
;; characters without manually exiting isearchb.

(require 'iswitchb)

(defgroup isearchb nil
  "Switch between buffers using a mechanism like isearch."
  :group 'iswitchb)

(defcustom isearchb-idle-timeout 1
  "*Number of idle seconds before isearchb turns itself off.
If nil, don't use a timeout."
  :type '(choice (integer :tag "Seconds")
		 (const :tag "Disable" nil))
  :group 'isearchb)

(defcustom isearchb-show-completions t
  "*If non-nil, show possible completions in the minibuffer."
  :type 'boolean
  :group 'isearchb)

(defvar isearchb-start-buffer nil)
(defvar isearchb-idle-timer nil)

(defun isearchb ()
  "Switch to buffer matching a substring, based on chars typed."
  (interactive)
  (unless (eq last-command 'isearchb)
    (setq iswitchb-text nil))
  (if (null iswitchb-text)
      (iswitchb-make-buflist iswitchb-default))
  (if last-command-char
      (setq iswitchb-text
	    (concat iswitchb-text (char-to-string last-command-char))))
  (iswitchb-set-matches)
  (let* ((match (car iswitchb-matches))
	 (buf (and match (get-buffer match))))
    (if buf
	(progn
	  (switch-to-buffer buf)
	  (if isearchb-show-completions
	      (message "isearchb: %s%s" iswitchb-text
		       (iswitchb-completions iswitchb-text nil))
	    (if (= 1 (length iswitchb-matches))
		(message "isearchb: %s (only match)" iswitchb-text)
	      (message "isearchb: %s" iswitchb-text))))
      (message "No buffer matching \"%s\"" iswitchb-text)
      (setq iswitchb-text
	    (substring iswitchb-text 0 (1- (length iswitchb-text)))))))

(defun isearchb-set-keybindings (modifier)
  "Setup isearchb on the given MODIFIER."
  (dotimes (i 128)
    (if (eq 'self-insert-command
	    (lookup-key global-map (vector i)))
	(define-key global-map (vector (list modifier i)) 'isearchb))))

(defun isearchb-read-buffer (prompt &optional default require-match)
  "Replacement for the built-in `read-buffer'.
Return the name of a buffer selected.
PROMPT is the prompt to give to the user.  DEFAULT if given is the default
buffer to be selected, which will go to the front of the list.
If REQUIRE-MATCH is non-nil, an existing-buffer must be selected."
  (let (buf-sel
	iswitchb-final-text
	(icomplete-mode nil) ;; prevent icomplete starting up
	;; can only use fonts if they have been bound.
	(iswitchb-use-fonts (and iswitchb-use-fonts
				 (boundp 'font-lock-comment-face)
				 (boundp 'font-lock-function-name-face))))
    (iswitchb-define-mode-map)
    (setq iswitchb-exit nil)
    (setq iswitchb-default (if (bufferp default)
			       (buffer-name default)
			     default))
    (let ((minibuffer-local-completion-map iswitchb-mode-map)
	  ;; Record the minibuffer depth that we expect to find once
	  ;; the minibuffer is set up and iswitchb-entryfn-p is called.
	  (iswitchb-minibuf-depth (1+ (minibuffer-depth)))
	  (iswitchb-require-match require-match))
      ;; prompt the user for the buffer name
      (setq iswitchb-final-text (completing-read
				 prompt		  ;the prompt
				 '(("dummy" . 1)) ;table
				 nil		  ;predicate
				 nil ;require-match [handled elsewhere]
				 iswitchb-text ;initial-contents
				 'iswitchb-history)))
    (if (and (not (eq iswitchb-exit 'usefirst))
	     (get-buffer iswitchb-final-text))
	;; This happens for example if the buffer was chosen with the mouse.
	(setq iswitchb-matches (list iswitchb-final-text)))
    ;; Handling the require-match must be done in a better way.
    (if (and require-match (not (iswitchb-existing-buffer-p)))
	(error "Must specify valid buffer"))
    (if (or (eq iswitchb-exit 'takeprompt)
	    (null iswitchb-matches))
	(setq buf-sel iswitchb-final-text)
      ;; else take head of list
      (setq buf-sel (car iswitchb-matches)))
    ;; Or possibly choose the default buffer
    (if  (equal iswitchb-final-text "")
	(setq buf-sel
	      (car iswitchb-matches)))
    buf-sel))

(defun isearchb-jump-to-iswitchb ()
  (interactive)
  (let* ((prompt "iswitch ")
	 (buf (isearchb-read-buffer prompt)))
    (if (eq iswitchb-exit 'findfile)
	(call-interactively 'find-file)
      (if buf
	  (if (get-buffer buf)
	      ;; buffer exists, so view it and then exit
	      (iswitchb-visit-buffer buf)
	    ;; else buffer doesn't exist
	    (iswitchb-possible-new-buffer buf))))))

(defun isearchb-timeout ()
  (setq isearchb-idle-timer nil)
  (setq last-command 'ignore)
  (message nil))

(defun isearchb-abort ()
  (switch-to-buffer isearchb-start-buffer)
  (when isearchb-idle-timer
    (cancel-timer isearchb-idle-timer)
    (setq isearchb-idle-timer nil))
  (setq this-command 'ignore))

(defun isearchb-follow-char ()
  (let (keys)
    (when (and (memq last-command '(isearchb isearchb-activate))
	       (setq keys (this-command-keys))
	       (= 1 (length keys)))
      (cond
       ((or (equal keys "\C-h") (equal keys "\C-?")
	    (equal keys [backspace]) (equal keys [delete]))
	(setq iswitchb-text
	      (substring iswitchb-text 0 (1- (length iswitchb-text))))
	(if (= 0 (length iswitchb-text))
	    (isearchb-abort)
	  (setq last-command-char nil)
	  (setq this-command 'isearchb)))
       ((or (equal keys "\C-i") (equal keys [tab]))
	(setq this-command 'isearchb-jump-to-iswitchb))
       ((equal keys "\C-s")
	(iswitchb-next-match)
	(setq last-command-char nil)
	(setq this-command 'isearchb))
       ((equal keys "\C-r")
	(iswitchb-prev-match)
	(setq last-command-char nil)
	(setq this-command 'isearchb))
       ((equal keys "\C-g")
	(isearchb-abort))
       ((eq (lookup-key global-map keys) 'self-insert-command)
	(setq this-command 'isearchb)))
      (if (and isearchb-idle-timeout
	       (null isearchb-idle-timer))
	(setq isearchb-idle-timer
	      (run-with-idle-timer isearchb-idle-timeout nil
				   'isearchb-timeout))))))

;;;###autoload
(defun isearchb-activate ()
  (interactive)
  (cond
   ((eq last-command 'isearchb)
    (setq this-command 'ignore))
   ((eq last-command 'isearchb-activate)
    (switch-to-buffer (car iswitchb-buflist))
    (setq this-command 'ignore))
   (t
    (message "isearchb: ")
    (setq isearchb-start-buffer (current-buffer))
    (setq iswitchb-rescan t)
    (setq iswitchb-text "")
    (iswitchb-make-buflist iswitchb-default)
    (iswitchb-set-matches)
    (add-hook 'pre-command-hook 'isearchb-follow-char))))

(provide 'isearchb)

;;; isearchb.el ends here
