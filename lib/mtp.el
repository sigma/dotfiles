;;; mtp.el --- <Mtp> Chat! client for emacs

;; Copyright (C) 2002 Yann Hodique (aka Sigma)
;; Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Noah S. Friedman

;; Author: Yann Hodique
;; Maintainer: Yann Hodique <Yann.Hodique@lifl.fr>
;; Status: Works in Emacs 19.27 and later.
;; Keywords: unix, comm

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
;; along with this program; if not, write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue.; Cambridge, MA 02139, USA.

;;; Commentary:

;; This code originally derived from ftelnet.el, and was then
;; modified substantially for use as <Mtp> Chat! client.


;;; Code:

(require 'comint)
(require 'shell)

(defvar mtp-version "MtpEmacs 0.1"
  "*Version of the program")

(defvar mtp-program "telnet"
  "*Name of program to invoke telnet")

(defvar mtp-explicit-args nil
  "*List of arguments to pass to telnet on the command line.")

(defvar mtp-mode-hook nil
  "*Hooks to run after setting current buffer to mtp-mode.")

(defvar mtp-process-connection-type nil
  "*If non-`nil', use a pty for the local telnet process.
If `nil', use a pipe (if pipes are supported on the local system).

Generally it is better not to waste ptys on systems which have a static
number of them.  On the other hand, some implementations of `telnet' assume
a pty is being used, and errors will result from using a pipe instead.")

(defvar mtp-directory-tracking-mode 'local
  "*Control whether and how to do directory tracking in a telnet buffer.

nil means don't do directory tracking.

t means do so using an ftp remote file name.

Any other value means do directory tracking using local file names.
This works only if the remote machine and the local one
share the same directories (through NFS).  This is the default.

This variable becomes local to a buffer when set in any fashion for it.

It is better to use the function of the same name to change the behavior of
directory tracking in a telnet session once it has begun, rather than
simply setting this variable, since the function does the necessary
re-synching of directories.")

(make-variable-buffer-local 'mtp-directory-tracking-mode)

(defvar mtp-host nil
  "The name of the remote host.  This variable is buffer-local.
There is usually no need to set this yourself.
")

(defvar mtp-remote-user nil
  "The username used on the remote host.
This variable is buffer-local and defaults to your local user name.
There is usually no need to set this yourself.")

;; Initialize telnet mode map.
; (setq mtp-mode-map nil)
(defvar mtp-mode-map '())

(cond
 ((null mtp-mode-map)
  (setq mtp-mode-map (if (consp shell-mode-map)
			 (cons 'keymap shell-mode-map)
		       (copy-keymap shell-mode-map)))
  
  (define-key mtp-mode-map "\C-m" 'mtp-send)
  (define-key mtp-mode-map "\C-ca" 'mtp-away)
  (define-key mtp-mode-map "\C-ct" 'mtp-tell)
  (define-key mtp-mode-map "\C-cr" 'mtp-reply)
  (define-key mtp-mode-map "\C-ce" 'mtp-emote)
  (define-key mtp-mode-map "\C-cw" 'mtp-who)
  (define-key mtp-mode-map "\C-cf" 'mtp-finger)
  (define-key mtp-mode-map "\C-ck" 'mtp-kick)
  (define-key mtp-mode-map "\C-cq" 'mtp-quit)
  (define-key mtp-mode-map "\C-cu" 'mtp-url)
  (define-key mtp-mode-map "\C-xk" 'no-kill)
))

(defvar mtp-history nil
  "*History ring for mtp input arguments.")


(defface mtp-default-face '((t (:foreground "LightBlue")))
  "Default color")

(defface mtp-aboutme-face '((t (:foreground "blue")))
  "Default color")

(defface mtp-Mtp-face '((t (:foreground "yellow")))
  "Default color")

(defface mtp-me-face '((t (:foreground "grey")))
  "Default color")

(defface mtp-tell-face '((t (:foreground "red")))
  "Default color")

(defface mtp-url-face '((t (:foreground "green")))
  "Default color")

(defvar mtp-default-face 'mtp-default-face)
(defvar mtp-aboutme-face 'mtp-aboutme-face)
(defvar mtp-Mtp-face 'mtp-Mtp-face)
(defvar mtp-me-face 'mtp-me-face)
(defvar mtp-tell-face 'mtp-tell-face)
(defvar mtp-url-face 'mtp-url-face)

;; See comments near mtp-pop-to-buffer for an explanation.
;;;###autoload (add-hook 'same-window-regexps "^\\*telnet-.*\\*\\(\\|<[0-9]+>\\)")

;;;###autoload
(defun mtp (input-args &optional buffer)
  "Open a network login connection to HOST via the `telnet' program.
Input is sent line-at-a-time to the remote connection.

Communication with the remote host is recorded in a buffer *telnet-HOST*
\(or *telnet-HOST:PORT* if using a nonstandard port number\).
If a prefix argument is given and the buffer *telnet-HOST* already exists,
a new buffer with a different connection will be made.

When called from a program, if the optional second argument is a string or
buffer, it names the buffer to use.

The variable `mtp-program' contains the name of the actual program to
run.  It can be a relative or absolute path.

The variable `mtp-explicit-args' is a list of arguments to give to the
telnet program when starting.  They are added after any arguments given in
INPUT-ARGS.

If the default value of `mtp-directory-tracking-mode' is t, then the
default directory in that buffer is set to a remote (FTP) file name to
access your home directory on the remote machine.  Occasionally this causes
an error, if you cannot access the home directory on that machine.  This
error is harmless as long as you don't try to use that default directory.

If `mtp-directory-tracking-mode' is neither t nor nil, then the default
directory is initially set up to your (local) home directory.
This is useful if the remote machine and your local machine
share the same files via NFS.  This is the default.

If you wish to change directory tracking styles during a session, use the
function `mtp-directory-tracking-mode' rather than simply setting the
variable."
  (interactive (list
		(read-from-minibuffer "Telnet host: "
                                      nil nil nil 'mtp-history)
		current-prefix-arg))
  (let* ((process-connection-type mtp-process-connection-type)
         (args (if mtp-explicit-args
                   (append (mtp-parse-words input-args)
                           mtp-explicit-args)
                 (mtp-parse-words input-args)))
         ;; skip args starting with `-'
         (nonopt-args (let ((l args))
                        (while (= ?- (aref (car l) 0))
                          (setq l (cdr l)))
                        l))
	 (host (car nonopt-args))
	 (port (car (cdr nonopt-args)))
         (buffer-name "*<Mtp> Chat*")
	 proc)

    (cond ((null buffer))
	  ((stringp buffer)
	   (setq buffer-name buffer))
          ((bufferp buffer)
           (setq buffer-name (buffer-name buffer)))
          ((numberp buffer)
           (setq buffer-name (format "%s<%d>" buffer-name buffer)))
          (t
           (setq buffer-name (generate-new-buffer-name buffer-name))))

    (setq buffer (get-buffer-create buffer-name))
    (mtp-pop-to-buffer buffer-name)
    (setq buffer-read-only t)

    (cond
     ((comint-check-proc buffer-name))
     (t
      (comint-exec buffer buffer-name mtp-program nil args)
      (setq proc (get-buffer-process buffer))
      ;; Set process-mark to point-max in case there is text in the
      ;; buffer from a previous exited process.
      (set-marker (process-mark proc) (point-max))

      ;; comint-output-filter-functions is treated like a hook: it is
      ;; processed via run-hooks or run-hooks-with-args in later versions
      ;; of emacs.
      ;; comint-output-filter-functions should already have a
      ;; permanent-local property, at least in emacs 19.27 or later.
      (cond
       ((fboundp 'make-local-hook)
        (make-local-hook 'comint-output-filter-functions)
        (add-hook 'comint-output-filter-functions 'mtp-carriage-filter
                  nil t))
       (t
        (make-local-variable 'comint-output-filter-functions)
        (add-hook 'comint-output-filter-functions 'mtp-carriage-filter)))

      (mtp-mode)

      ;; initial filter to get remote user name if connecting to a telnet
      ;; login port.

      (add-hook 'comint-output-filter-functions
		'mtp-login-output-filter)
      (add-hook 'comint-output-filter-functions
		'mtp-user-output-filter)



      (add-hook 'comint-input-filter-functions
		'mtp-user-input-filter)



      (make-local-variable 'mtp-host)
      (setq mtp-host host)
      (make-local-variable 'mtp-remote-user)
      (setq mtp-remote-user nil)

      (cond
       ((eq t mtp-directory-tracking-mode))
       ((null mtp-directory-tracking-mode))
       (t
        (cd-absolute (concat comint-file-name-prefix "~/"))))))))

(defun mtp-mode ()
  "Set major-mode for mtp sessions.
If `mtp-mode-hook' is set, run it."
  (interactive)
  (kill-all-local-variables)
  (shell-mode)
  (setq major-mode 'mtp-mode)
  (setq mode-name "mtp")
  (use-local-map mtp-mode-map)
  (setq shell-dirtrackp mtp-directory-tracking-mode)
  (make-local-variable 'comint-file-name-prefix)
  (run-hooks 'mtp-mode-hook))

(defun mtp-directory-tracking-mode (&optional prefix)
  "Do remote or local directory tracking, or disable entirely.

If called with no prefix argument or a unspecified prefix argument (just
``\\[universal-argument]'' with no number) do remote directory tracking via
ange-ftp.  If called as a function, give it no argument.

If called with a negative prefix argument, disable directory tracking
entirely.

If called with a positive, numeric prefix argument, e.g.

         \\[universal-argument] 1 \\[mtp-directory-tracking-mode]

then do directory tracking but assume the remote filesystem is the same as
the local system.  This only works in general if the remote machine and the
local one share the same directories (through NFS)."
  (interactive "P")
  (cond
   ((or (null prefix)
        (consp prefix))
    (setq mtp-directory-tracking-mode t)
    (setq shell-dirtrackp t)
    (setq comint-file-name-prefix
          (concat "/" mtp-remote-user "@" mtp-host ":")))
   ((< prefix 0)
    (setq mtp-directory-tracking-mode nil)
    (setq shell-dirtrackp nil))
   (t
    (setq mtp-directory-tracking-mode 'local)
    (setq comint-file-name-prefix "")
    (setq shell-dirtrackp t)))
  (cond
   (shell-dirtrackp
    (let* ((proc (get-buffer-process (current-buffer)))
           (proc-mark (process-mark proc))
           (current-input (buffer-substring proc-mark (point-max)))
           (orig-point (point))
           (offset (and (>= orig-point proc-mark)
                        (- (point-max) orig-point))))
      (unwind-protect
          (progn
            (delete-region proc-mark (point-max))
            (goto-char (point-max))
            (shell-resync-dirs))
        (goto-char proc-mark)
        (insert current-input)
        (if offset
            (goto-char (- (point-max) offset))
          (goto-char orig-point)))))))

;; Parse a line into its constituent parts (words separated by
;; whitespace).  Return a list of the words.
(defun mtp-parse-words (line)
  (let ((list nil)
	(posn 0)
        (match-data (match-data)))
    (while (string-match "[^ \t\n]+" line posn)
      (setq list (cons (substring line (match-beginning 0) (match-end 0))
                       list))
      (setq posn (match-end 0)))
    (store-match-data (match-data))
    (nreverse list)))

;; Starting in Emacs 19.29, the variable same-window-regexps modifies how
;; pop-to-buffer works; in particular, if the name of the buffer being
;; switched to matches one of the regexps in same-window-regexps (which may
;; be buffer-local to the current buffer), then pop-to-buffer acts like
;; switch-to-buffer.  This gives users more control.
;; This also explains the related autoload cookie near the top of the file.
(defun mtp-pop-to-buffer (buffer)
  (if (boundp 'same-window-regexps)
      (pop-to-buffer buffer)
    (switch-to-buffer buffer)))


;; This should go on comint-output-filter-functions initially.
;; Once it detects that a username has been prompted for, it adds an input
;; filter that saves the username.
(defun mtp-login-output-filter (s)


  (cond
   ((string-match "<Mtp> Login:" s)
    (let ((s (setq mtp-remote-user (read-from-minibuffer "Login: "))))
      (and s (comint-send-string (get-buffer-process (current-buffer)) (concat s "\n")))
      (set-keywords (case-unsensitive s))
      (mtp-mode))
    )
   ((string-match "<Mtp> Password:" s)
    (let ((s (concat (comint-read-noecho "Password: " t) "\n")))
      (and s (comint-send-string (get-buffer-process (current-buffer)) s)))
    ;(remove-hook 'comint-output-filter-functions 'mtp-login-output-filter)
    ;(comint-send-string (get-buffer-process (current-buffer)) (format "set client %s\n" mtp-version))
    )
   ((string-match "<Mtp> Welcome" s)
    (remove-hook 'comint-output-filter-functions 'mtp-login-output-filter)
    (comint-send-string (get-buffer-process (current-buffer)) (format "set client %s\n" mtp-version))
    )
   )
)

(defun mtp-user-output-filter (s)
;  (cond
;   ((string-match "^<\\sw+> /.*$" s)
;    (comint-send-string (get-buffer-process (current-buffer)) 
;			(format "kick %s\n" (substring s (+ (string-match "^<\\(\\sw+\\)>" s) 1) (- (match-end 0) 1)) ))
;    )
;   )
)

(defun mtp-user-input-filter (s)

)

(defun mtp-carriage-filter (string)
  (let* ((point-marker (point-marker))
         (end (process-mark (get-buffer-process (current-buffer))))
         (beg (or (and (boundp 'comint-last-output-start)
                       comint-last-output-start)
                  (- end (length string)))))
    (goto-char beg)
    (while (search-forward "\C-m" end t)
      (delete-char -1))
    (goto-char point-marker)))

;; Definitions for keybindings

(defun mtp-message (s) 
  (interactive)
  (message s)
  (and s (process-send-string (get-buffer-process (current-buffer)) s))
)

(defun mtp-send ()
  (interactive)
  (let ((s (concat (read-from-minibuffer "") "\n")))
    (mtp-message s)))

(defun mtp-away ()
  (interactive)
  (mtp-message "switch away\n"))

(defun mtp-tell ()
  (interactive)
  (let ((s (concat "tell " (format (read-from-minibuffer "tell ")) "\n")))
    (mtp-message s)))

(defun mtp-reply ()
  (interactive)
  (let ((s (concat "reply " (read-from-minibuffer "reply ") "\n")))
    (mtp-message s)))

(defun mtp-emote ()
  (interactive)
  (let ((s (concat "emote " (read-from-minibuffer "emote ") "\n")))
    (mtp-message s)))

(defun mtp-who ()
  (interactive)
  (let ((s "who all\n"))
    (mtp-message s)))

(defun mtp-finger (beg end)
  (interactive "r")
  (let ((s (concat "finger " (buffer-substring beg end) "\n")))
    (mtp-message s))
  (keyboard-quit))

(defun mtp-kick (beg end)
  (interactive "r")
  (let ((s (concat "kick " (buffer-substring beg end) " " (read-from-minibuffer (concat "kick " (buffer-substring beg end))) "\n")))
    (mtp-message s))
  (keyboard-quit))

(defun mtp-quit ()
  (interactive)
  (let ((s "quit\n"))
    (mtp-message s))
  (kill-buffer nil)
)

(defun mtp-url (beg end)
  (interactive "r")
  (browse-url (buffer-substring beg end))
  (keyboard-quit))

(defun no-kill ()
  (interactive)
  (message "Sorry, no kill allowed :-)")
)

(defun mtp-tab-or-complete ()
  "Complete file name if doing directory tracking, or just insert TAB."
  (interactive)
  (if mtp-directory-tracking-mode
      (comint-dynamic-complete)
    (insert "\C-i")))

;; font lock

(defvar mtp-keywords '())

(defun set-keywords (s)
  (setq mtp-keywords `(
		       ((concat "^\\(.*" ,s ".*\\)$") 1 mtp-aboutme-face)
		       ("^\\(<Mtp>.*\\)$" 1 mtp-Mtp-face)
		       ((concat "^\\(<" ,s ">.*\\)$") 1 mtp-me-face)
		       ("^\\(<Mtp>\\s-\\sw+\\s-tells\\s-.*\\)$" 1 mtp-tell-face)
		       ("^\\(|.*\\)$" 1 mtp-tell-face)
		       ("^\\(<Mtp>\\s-You\\s-.*\\)$" 1 mtp-me-face)
		       ("\\(http://[^ \n()]*\\).*$" 1 mtp-url-face)
		       ("^\\(.*\\)$" 1 mtp-default-face)
		       ))
  )

(defun mtp-font-lock ()
  "Turn on font-lock for Mtp keywords."
  (interactive)
  (setq font-lock-keywords mtp-keywords)
  (setq font-lock-keywords-alist nil)
  )

;; Usefull functions :
(defun case-unsensitive (s)
  (let ((up (upcase s)) (down (downcase s)))
    (generate-regexp (string-to-list up) (string-to-list down)))
)

(defun generate-regexp (s1 s2)
  (and s1
       (concat (concat "[" (char-to-string (car s1)) (char-to-string (car s2)) "]")
	       (generate-regexp (cdr s1) (cdr s2))
	       )))


(provide 'mtp)

;; local variables:
;; vc-make-backup-files: t
;; end:

;;; mtp.el ends here
