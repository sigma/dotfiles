;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen.el
;;
(defconst elscreen-version "1.2.4 (August 28, 2002)")
;;
;; Author:   Naoto Morishima <naoto@morishima.net>
;;              Nara Institute of Science and Technology, Japan
;; Based on: screens.el
;;              by Heikki T. Suopanki <suopanki@stekt1.oulu.fi>
;; Created:  June 22, 1996
;; Revised:  August 28, 2002

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'elscreen)
(require 'alist)

;
; user custamizable options
;
(defvar elscreen-prefix-key "\C-z"
  "*ElScreen command prefix-key.")

(defvar elscreen-show-screen-number t
  "*If non-nil, show the number of the current screen in the mode line.")

(defvar elscreen-buffer-to-screen-alist
  '(("[Ss]hell" . "shell")
    ("compilation" . "compile")
    ("-telnet" . "telnet")
    ("dict" . "OnlineDict"))
  "*Alist composed of the pair of regular expression of buffer-name and corresponding screen-name.")

(defvar elscreen-mode-to-screen-alist
  '(("^wl-" . "Wanderlust")
    ("^mew-" . "Mew")
    ("^irchat-" . "IRChat")
    ("^liece-" . "Liece")
    ("^dired-mode$" . "Dired")
    ("^Info-mode$" . "Info"))
  "*Alist composed of the pair of mode-name and corresponding screen-name.")

;
; variables
;
(defvar elscreen-mode-line (and elscreen-show-screen-number "[0] ")
  "*Shows the screen number in the mode line.")

(setq global-mode-string
      (cond
       ((not (listp global-mode-string))
	(list "" 'elscreen-mode-line global-mode-string))
       ((not (memq 'elscreen-mode-line global-mode-string))
	(append '("" elscreen-mode-line) global-mode-string))
       (t
	global-mode-string)))

(defvar elscreen-confs-alist nil
  "*Alist that contains the information about screen configurations.")

(defvar elscreen-name-alist nil
  "*List of window name specified by user.")

(defvar elscreen-scratch-buffer "*scratch*"
  "*Buffer name of scratch-buffer.")

(defvar elscreen-last-message "Welcome to ElScreen!"
  "*Last shown message.")

(defvar elscreen-help "ElScreen keys:
       \\[elscreen-create]    Create a new screen
       \\[elscreen-kill]    Kill the current screen
       \\[elscreen-previous]    Switch to the previous screen
       \\[elscreen-next]    Switch to the next screen
       \\[elscreen-toggle]    Toggle screen
       \\[elscreen-goto]    Jump to the specified screen
       \\[elscreen-jump-0]
         :      Jump to the screen #
       \\[elscreen-jump-9]
       \\[elscreen-show-list]    Show list of screens
       \\[elscreen-name]    Name the current screen
       \\[elscreen-show-last-message]    Show last message
       \\[elscreen-show-time]    Show time
       \\[elscreen-show-version]    Show ElScreen version
       \\[elscreen-find-file]    Create new screen and open file
       \\[elscreen-number-mode-line]    Show/hide the screen number in the mode line
       \\[elscreen-help]    Show this help"
  "*Help shown by elscreen-help-mode")

;
; key definition
;
(defvar elscreen-map (make-sparse-keymap)
  "*Keymap for ElScreen.")
(global-set-key elscreen-prefix-key elscreen-map)

(define-key elscreen-map  "\C-c" 'elscreen-create)
(define-key elscreen-map  "c"    'elscreen-create)
(define-key elscreen-map  "\C-k" 'elscreen-kill)
(define-key elscreen-map  "k"    'elscreen-kill)
(define-key elscreen-map  "\C-p" 'elscreen-previous)
(define-key elscreen-map  "p"    'elscreen-previous)
(define-key elscreen-map  "\C-n" 'elscreen-next)
(define-key elscreen-map  "n"    'elscreen-next)
(define-key elscreen-map  "\C-a" 'elscreen-toggle)
(define-key elscreen-map  "a"    'elscreen-toggle)
(define-key elscreen-map  "g"    'elscreen-goto)
(define-key elscreen-map  "0"    'elscreen-jump)
(define-key elscreen-map  "1"    'elscreen-jump)
(define-key elscreen-map  "2"    'elscreen-jump)
(define-key elscreen-map  "3"    'elscreen-jump)
(define-key elscreen-map  "4"    'elscreen-jump)
(define-key elscreen-map  "5"    'elscreen-jump)
(define-key elscreen-map  "6"    'elscreen-jump)
(define-key elscreen-map  "7"    'elscreen-jump)
(define-key elscreen-map  "8"    'elscreen-jump)
(define-key elscreen-map  "9"    'elscreen-jump)
(define-key elscreen-map  "?"    'elscreen-help)
(define-key elscreen-map  "\C-f" 'elscreen-find-file)
(define-key elscreen-map  "b"    'elscreen-switch-to-buffer)
(define-key elscreen-map  "\C-w" 'elscreen-show-list)
(define-key elscreen-map  "w"    'elscreen-show-list)
(define-key elscreen-map  "\C-m" 'elscreen-show-last-message)
(define-key elscreen-map  "m"    'elscreen-show-last-message)
(define-key elscreen-map  "t"    'elscreen-show-time)
(define-key elscreen-map  "A"    'elscreen-name)
(define-key elscreen-map  "v"    'elscreen-show-version)
(define-key elscreen-map  "i"    'elscreen-number-mode-line)
(define-key elscreen-map  "l"    'elscreen-link)
(define-key elscreen-map  "s"    'elscreen-split)

;(add-hook 'minibuffer-setup-hook 'elscreen-minibuffer-setup)
;(add-hook 'minibuffer-exit-hook 'elscreen-minibuffer-exit)

(defun elscreen-minibuffer-setup ()
  "Disable elscreen-prefix-key when minibuffer become active."
  (global-set-key elscreen-prefix-key 'elscreen-minibuffer-message))

(defun elscreen-minibuffer-exit ()
  "Enable elscreen-prefix-key when minibuffer become inactive."
  (global-set-key elscreen-prefix-key elscreen-map))

;
; code
;
(defun get-alist (key alist)
  (cdr (assoc key alist)))

(defun elscreen-alloc-confs (frame)
  (set-alist 'elscreen-confs-alist
	     frame (list (list 1 0 nil)
			 (list (cons 0 (current-window-configuration))))))

(defun elscreen-free-confs (frame)
  (remove-alist 'elscreen-confs-alist frame))

(if (boundp 'create-frame-hook)
    ; XEmacs
    (add-hook 'create-frame-hook 'elscreen-alloc-confs)
  ; GNU Emacs
  (add-hook 'after-make-frame-functions 'elscreen-alloc-confs))
(add-hook 'delete-frame-hook 'elscreen-free-confs)
(elscreen-alloc-confs (selected-frame))

(defun elscreen-get-status-list (&optional frame)
  (let ((frame (or frame (selected-frame))))
    (nth 0 (get-alist frame elscreen-confs-alist))))

(defun elscreen-status-index (status)
  (cond
   ((eq status 'open-screens) 0)
   ((eq status 'current-screen) 1)
   ((eq status 'previous-screen) 2)
   (t -1)))

(defun elscreen-get-status (status &optional frame)
  (let* ((frame (or frame (selected-frame)))
	 (status-list (elscreen-get-status-list frame))
	 (status-index (elscreen-status-index status)))
    (nth status-index status-list)))

(defun elscreen-set-status (status value &optional frame)
  (let* ((frame (or frame (selected-frame)))
	 (status-list (elscreen-get-status-list frame))
	 (winconfs-alist (elscreen-get-winconfs-alist frame))
	 (status-index (elscreen-status-index status)))
    (setcar (nthcdr status-index status-list) value)
    (set-alist 'elscreen-confs-alist frame (list status-list winconfs-alist))))

(defun elscreen-get-open-screens (&optional frame)
  (let ((frame (or frame (selected-frame))))
    (elscreen-get-status 'open-screens frame)))

(defun elscreen-set-open-screens (open-screens-value &optional frame)
  (let ((frame (or frame (selected-frame))))
    (elscreen-set-status 'open-screens open-screens-value)))

(defun elscreen-get-current-screen (&optional frame)
  (let ((frame (or frame (selected-frame))))
    (elscreen-get-status 'current-screen)))

(defun elscreen-set-current-screen (current-screen-value &optional frame)
  (let ((frame (or frame (selected-frame))))
    (elscreen-set-status 'current-screen current-screen-value)))

(defun elscreen-get-previous-screen (&optional frame)
  (let ((frame (or frame (selected-frame))))
    (elscreen-get-status 'previous-screen)))

(defun elscreen-set-previous-screen (previous-screen-value &optional frame)
  (let ((frame (or frame (selected-frame))))
    (elscreen-set-status 'previous-screen previous-screen-value)))

(defun elscreen-get-winconfs-alist (&optional frame)
  (let ((frame (or frame (selected-frame))))
    (nth 1 (get-alist frame elscreen-confs-alist))))

(defun elscreen-set-winconfs-alist (winconfs-alist &optional frame)
  (let* ((frame (or frame (selected-frame)))
	 (status-list (elscreen-get-status-list frame)))
    (set-alist 'elscreen-confs-alist frame (list status-list winconfs-alist))))

(defun elscreen-get-winconf (screen &optional frame)
  (get-alist screen (elscreen-get-winconfs-alist frame)))

(defun elscreen-set-winconf (screen winconf &optional frame)
  (let* ((frame (or frame (selected-frame)))
	 (winconfs-alist (elscreen-get-winconfs-alist frame)))
    (set-alist 'winconfs-alist screen winconf)
    (elscreen-set-winconfs-alist winconfs-alist frame)))

(defun elscreen-remove-winconf (screen &optional frame)
  (let* ((frame (or frame (selected-frame)))
	 (winconfs-alist (elscreen-get-winconfs-alist frame)))
    (remove-alist 'winconfs-alist screen)
    (elscreen-set-winconfs-alist winconfs-alist frame)))

(defun elscreen-minibuffer-message ()
  "Show message when minibuffer is active."
  (interactive)
  (elscreen-message "Sorry, minibuffer is active..."))

(defun elscreen-create ()
  "Create a new screen."
  (interactive)
      (cond
       ((>= (elscreen-get-open-screens) 10)
	(elscreen-message "Can't create any more screen"))
       (t
	(elscreen-set-winconf (elscreen-get-current-screen)
			      (current-window-configuration))
	(elscreen-set-open-screens (1+ (elscreen-get-open-screens)))
	(let ((i 0)
	      (flag t))
	  (while (and flag (< i 9))
	    (if (not (elscreen-get-winconf i))
		(progn
		  (elscreen-set-previous-screen (elscreen-get-current-screen))
		  (setq flag nil))
	      (setq i (+ i 1))))
	  (elscreen-set-current-screen i))
	(delete-other-windows)
	(switch-to-buffer elscreen-scratch-buffer)
	(lisp-interaction-mode)
	(elscreen-mode-line-update)
	(elscreen-set-winconf (elscreen-get-current-screen)
			      (current-window-configuration)))))

(defun elscreen-kill ()
  "Kill the current screen."
  (interactive)
  (cond
   ((= (elscreen-get-open-screens) 1)
    (elscreen-message "There is only one screen, cannot kill"))
   (t
    (elscreen-set-open-screens (1- (elscreen-get-open-screens)))
    (elscreen-remove-winconf (elscreen-get-current-screen))
    (if (get-alist (elscreen-get-current-screen) elscreen-name-alist)
	(del-alist (elscreen-get-current-screen) elscreen-name-alist))
    (let ((i 0)
	  (flag t)
	  (next-screen))
      (if (elscreen-get-previous-screen)
	  (setq next-screen (elscreen-get-previous-screen))
	(setq next-screen (elscreen-get-current-screen))
	(while (and flag (< i 9))
	  (setq next-screen (+ next-screen 1))
	  (if (< 9 next-screen)
	      (setq next-screen 0))
	  (if (elscreen-get-winconf next-screen)
	      (setq flag nil)
	    (setq i (+ i 1)))))
      (elscreen-set-current-screen nil)
      (elscreen-goto next-screen)))))

(defun elscreen-goto (screen)
  (interactive "NGoto screen number: ")
  (elscreen-goto0 screen)
  (redraw-frame (selected-frame)))

(defun elscreen-goto0 (screen)
  "Jump to the specified screen."
  (if (elscreen-get-winconf screen)
      (progn
	(if (elscreen-get-current-screen)
	    (elscreen-set-winconf (elscreen-get-current-screen)
				  (current-window-configuration)))
	(elscreen-set-previous-screen (elscreen-get-current-screen))
	(elscreen-set-current-screen screen)
	(delete-other-windows)
	(switch-to-buffer elscreen-scratch-buffer)
	(set-window-configuration
	 (elscreen-get-winconf (elscreen-get-current-screen)))
	(elscreen-mode-line-update))
    (elscreen-message (format "No screen %d" screen))))

(defun elscreen-next ()
  "Switch to the next screen."
  (interactive)
  (elscreen-set-winconf (elscreen-get-current-screen)
			(current-window-configuration))
  (let ((i 0)
	(flag t)
	(next-screen (elscreen-get-current-screen)))
    (while (and flag (< i 10))
      (setq next-screen (1+ next-screen))
      (if (< 9 next-screen)
	  (setq next-screen 0))
      (if (elscreen-get-winconf next-screen)
	  (setq flag nil))
      (setq i (1+ i)))
    (cond
     ((eq next-screen (elscreen-get-current-screen))
      (elscreen-message
       (format "You cannot escape from screen %d!"
	       (elscreen-get-current-screen))))
     (t
      (elscreen-goto next-screen)))))

(defun elscreen-previous ()
  "Switch to the previous screen."
  (interactive)
  (elscreen-set-winconf (elscreen-get-current-screen)
			(current-window-configuration))
  (let ((i 0)
	(flag t)
	(next-screen (elscreen-get-current-screen)))
    (while (and flag (< i 10))
      (setq i (1+ i))
      (setq next-screen (1- next-screen))
      (if (< next-screen 0)
	  (setq next-screen 9))
      (if (elscreen-get-winconf next-screen)
	  (setq flag nil)))
    (cond
     ((eq next-screen (elscreen-get-current-screen))
      (elscreen-message
       (format "You cannot escape from screen %d!"
	       (elscreen-get-current-screen))))
     (t
      (elscreen-goto next-screen)))))

(defun elscreen-toggle ()
  "Toggle to last screen."
    (interactive)
    (cond
     ((= (elscreen-get-open-screens) 1)
      (elscreen-message
       (format "You cannot escape from screen %d!"
	       (elscreen-get-current-screen))))
     ((elscreen-get-previous-screen)
      (elscreen-goto (elscreen-get-previous-screen)))
     (t
      (let ((i 0)
	    (flag t)
	    (next-screen (elscreen-get-current-screen)))
	(while (and flag (< i 10))
	  (setq next-screen (1+ next-screen))
	  (if (< 9 next-screen)
	      (setq next-screen 0))
	  (if (elscreen-get-winconf next-screen)
	      (setq flag nil))
	  (setq i (1+ i)))
	(elscreen-goto next-screen)))))

(defun elscreen-jump ()
  "Switch to specified screen."
  (interactive)
  (let ((next-screen (string-to-number (string last-command-char))))
    (if (not (or (< 9 next-screen)
		 (< next-screen 0)))
	(elscreen-goto next-screen))))

(defun elscreen-get-screen-buffer-list ()
  (let ((screen-buffer-list nil))
    (walk-windows
     '(lambda (window)
	(add-to-list 'screen-buffer-list (current-buffer)))
     'other 'other)
    screen-buffer-list))


(defun elscreen-get-screen-create (buffer)
  (let ((current-screen (elscreen-get-current-screen))
	(previous-screen (elscreen-get-previous-screen))
	(target-screen nil)
	(flag t)
	(i 0))
    (while (and flag (< i 10))
      (if (and (elscreen-get-winconf i)
	       (elscreen-goto0 i))
	  (if (member buffer (elscreen-get-screen-buffer-list))
	      (progn
		(setq target-screen i)
		(setq flag nil))))
      (setq i (+ i 1)))
    (elscreen-goto0 current-screen)
    (elscreen-set-previous-screen previous-screen)
    (if target-screen
	(progn
	  (elscreen-goto target-screen)
	  (select-window (get-buffer-window buffer)))
      (if (< (elscreen-get-open-screens) 10)
	  (progn
	    (elscreen-create)
	    (switch-to-buffer buffer))
	(split-window)
	(switch-to-buffer-other-window buffer)))))


(defun elscreen-number-mode-line ()
  "Toggle the screen number in the mode line."
  (interactive)
  (setq elscreen-show-screen-number (null elscreen-show-screen-number))
  (elscreen-mode-line-update)
  (switch-to-buffer (current-buffer)))

(defun elscreen-show-list ()
  "Show list of screens."
  (interactive)
  (let ((elscreen-tmp-this-screen (elscreen-get-current-screen))
	(elscreen-tmp-previous-screen (elscreen-get-previous-screen))
	(elscreen-list-message nil)
	(elscreen-tmp-name-alist nil)
	(elscreen-buffer-names nil)
	(elscreen-mode-names nil)
	(i 0)
	(j)
	(flag))
    (while (< i 10)
      (if (elscreen-get-winconf i)
	  (progn
	    (elscreen-goto0 i)
	    (setq elscreen-buffer-names (elscreen-get-buffer-name-list))
	    (setq elscreen-mode-names (elscreen-get-mode-list))
	    (setq flag t)
	    (or
	     (progn ; examine buffer name
	       (setq j 0)
	       (while (and flag (< j (length elscreen-buffer-to-screen-alist)))
		 (if (string-match
		      (car (nth j elscreen-buffer-to-screen-alist))
		      elscreen-buffer-names)
		     (progn
		       (set-alist
			'elscreen-tmp-name-alist
			i (cdr (nth j elscreen-buffer-to-screen-alist)))
		       (setq flag nil)))
		 (setq j (+ j 1)))
	       (null flag))
	     (progn ; buffer-name didn't give it, so let's try mode-name
	       (setq j 0)
	       (while (and flag (< j (length elscreen-mode-to-screen-alist)))
		 (if (string-match
		      (car (nth j elscreen-mode-to-screen-alist))
		      elscreen-mode-names)
		     (progn
		       (set-alist
			'elscreen-tmp-name-alist
			i (cdr (nth j elscreen-mode-to-screen-alist)))
		       (setq flag nil)))
		 (setq j (+ j 1)))
	       (null flag))
	     ; neither buffer-name and mode-name didn't give it...
	     (set-alist 'elscreen-tmp-name-alist i elscreen-buffer-names))))
      (setq i (+ i 1)))
	     ; making elscreen-list-message is completed.
    (elscreen-goto0 elscreen-tmp-this-screen)
    (elscreen-set-previous-screen elscreen-tmp-previous-screen)
    (setq i 0)
    (while (< i 10)
      (if (elscreen-get-winconf i)
          (setq elscreen-list-message
		(concat elscreen-list-message
			(format "%d%s %s" i
				(or (and (eq i (elscreen-get-current-screen))
					 "+")
				    (and (eq i (elscreen-get-previous-screen))
					 "-")
				    "")
				(or (get-alist i elscreen-name-alist)
				    (get-alist i elscreen-tmp-name-alist)))
			"  ")))
      (setq i (+ i 1)))
    (elscreen-message elscreen-list-message)))

(defun elscreen-get-buffer-name-list ()
  (let ((org-window (selected-window))
	(buffer-list (buffer-name)))
    (while (progn (other-window 1)
		  (not (eq org-window (selected-window))))
      (setq buffer-list (concat buffer-list ":" (buffer-name))))
    buffer-list))

(defun elscreen-get-mode-list ()
  (let ((org-window (selected-window))
	(mode-list (symbol-name major-mode)))
    (while (progn (other-window 1)
		  (not (eq org-window (selected-window))))
      (setq mode-list (concat mode-list ":" mode-name)))
    mode-list))


(defun elscreen-name (new-name)
  "Specify screen name."
  (interactive "sSet window's title to: ")
  (set-alist 'elscreen-name-alist (elscreen-get-current-screen) new-name)
  (if (eq (length new-name) 0)
      (del-alist (elscreen-get-current-screen) elscreen-name-alist)))


(defun elscreen-find-file (filename)
  "Edit file FILENAME.
Switch to a screen visiting file FILENAME,
creating one if none already exists."
  (interactive "FFind file in new screen: ")
  (elscreen-get-screen-create (find-file-noselect filename)))

(defun elscreen-switch-to-buffer (buffer)
  "Switch to a screen that has the window with buffer BUFNAME,
creating one if none already exists."
  (interactive "BSwitch to buffer in new screen: ")
  (elscreen-get-screen-create (get-buffer buffer)))

(defun elscreen-help ()
  "Help about screen functions."
  (interactive)
  (with-output-to-temp-buffer "*ElScreen Help*"
    (princ (documentation 'elscreen-help-mode))
    (print-help-return-message)))

(defun elscreen-add-help (&optional additional-help)
  (if additional-help
      (setq elscreen-help (concat elscreen-help
				  (format "\n\n")
				  additional-help)))
  (let ((help-function
	 (list 'defun 'elscreen-help-mode '()
	       elscreen-help)))
    (eval help-function)))

(elscreen-add-help)


(defun elscreen-show-last-message ()
  "Show last message."
  (interactive)
  (elscreen-message elscreen-last-message 5))

(defun elscreen-show-time ()
  "Show time."
  (interactive)
  (elscreen-message
   (concat (current-time-string) " " (nth 1 (current-time-zone))) 3))

(defun elscreen-show-version ()
  "Show ElScreen version."
  (interactive)
  (elscreen-message (concat "ElScreen version " elscreen-version)))


(defun elscreen-mode-line-update ()
  (setq elscreen-mode-line
	(and elscreen-show-screen-number
	     (format "[%d]" (elscreen-get-current-screen)))))


(defun elscreen-message (message &optional sec)
  (when message
    (setq elscreen-last-message message)
    (message "%s" message)
    (sit-for (or sec 3)))
  (message nil))

;
; not supported
;
(defun elscreen-link ()
  (interactive)
  (cond
   ((null (one-window-p))
    (elscreen-message "current screen must not have two or more window!"))
   ((or (null (elscreen-get-previous-screen))
	(= (elscreen-get-open-screens) 1))
    (elscreen-message "must specify previous screen!"))
   ((and (elscreen-goto (elscreen-get-previous-screen))
	 (null (one-window-p)))
    (elscreen-goto (elscreen-get-previous-screen))
    (elscreen-message "previous screen must not have two or more window!"))
   (t
    (let ((elscreen-link-buffer (current-buffer)))
      (elscreen-kill)
      (switch-to-buffer-other-window elscreen-link-buffer)))))

(defun elscreen-split ()
  (interactive)
  (if (and (null (one-window-p))
	   (< (elscreen-get-open-screens) 10))
      (let ((elscreen-split-buffer (current-buffer)))
	(delete-window)
	(elscreen-create)
	(switch-to-buffer elscreen-split-buffer)
	(elscreen-goto (elscreen-get-previous-screen)))
    (elscreen-message "cannot split screen!")))
