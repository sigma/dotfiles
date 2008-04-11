;;; rcirc-config.el ---

;; Copyright (C) 2007  Free Software Foundation, Inc.

;; Author: Yann Hodique <yann.hodique@trusted-logic.fr>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(eval-after-load 'rcirc
  '(progn
     ;; Don't print /away messages.
     ;; This does not require rcirc to be loaded already,
     ;; since rcirc doesn't define a 301 handler (yet).
     (defun rcirc-handler-301 (process cmd sender args)
       "/away message handler.")

     ;; Turn on spell checking.
     (add-hook 'rcirc-mode-hook (lambda ()
                                  (flyspell-mode 1)))

     ;; Keep input line at bottom.
     (add-hook 'rcirc-mode-hook
               (lambda ()
                 (set (make-local-variable 'scroll-conservatively)
                      8192)))

     (require 'rcirc-color)

     (defun-rcirc-command reconnect (arg)
       "Reconnect the server process."
       (interactive "i")
       (unless process
         (error "There's no process for this target"))
       (let* ((server (car (process-contact process)))
              (port (process-contact process :service))
              (nick (rcirc-nick process))
              channels query-buffers)
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (when (eq process (rcirc-buffer-process))
               (remove-hook 'change-major-mode-hook
                            'rcirc-change-major-mode-hook)
               (if (rcirc-channel-p rcirc-target)
                   (setq channels (cons rcirc-target channels))
                 (setq query-buffers (cons buf query-buffers))))))
         (delete-process process)
         (rcirc-connect server port nick
                        rcirc-default-user-name
                        rcirc-default-user-full-name
                        channels)))

     ;; Minimal logging to `~/.rcirc-logs/channel'
     ;; by courtesy of Trent Buck.
     (add-hook 'rcirc-print-hooks 'rcirc-write-log)
     (defcustom rcirc-log-directory "~/.rcirc-logs"
       "Where logs should be kept.  If nil, logs aren't kept.")
     (defun rcirc-write-log (process sender response target text)
       (when rcirc-log-directory
         (with-temp-buffer
           ;; Sometimes TARGET is a buffer :-(
           (when (bufferp target)
             (setq target (with-current-buffer buffer rcirc-target)))

           ;; Sometimes buffer is not anything at all!
           (unless (or (null target) (string= target ""))
             ;; Print the line into the temp buffer.
             (insert (format-time-string "%Y-%m-%d %H:%M "))
             (insert (format "%-16s " (rcirc-user-nick sender)))
             (unless (string= response "PRIVMSG")
               (insert "/" (downcase response) " "))
             (insert text "\n")

             ;; Append the line to the appropriate logfile.
             (let ((coding-system-for-write 'no-conversion))
               (write-region (point-min) (point-max)
                             (concat rcirc-log-directory "/" (downcase target))
                             t 'quietly))))))

     (rcirc-track-minor-mode 1)

     (defun rcirc-connect (server &optional port nick user-name full-name
                                  startup-channels)
       (save-excursion
         (message "Connecting to %s..." server)
         (let* ((inhibit-eol-conversion)
                (port-number (if port
                                 (if (stringp port)
                                     (string-to-number port)
                                   port)
                               rcirc-default-port))
                (nick (or nick rcirc-default-nick))
                (user-name (or user-name rcirc-default-user-name))
                (full-name (or full-name rcirc-default-full-name))
                (startup-channels startup-channels)
                (process (make-network-process :name server :host server :service port-number)))
           ;; set up process
           (set-process-coding-system process 'raw-text 'raw-text)
           (switch-to-buffer (rcirc-generate-new-buffer-name process nil))
           (set-process-buffer process (current-buffer))
           (rcirc-mode process nil)
           (set-process-sentinel process 'rcirc-sentinel)
           (set-process-filter process 'rcirc-filter)
           (make-local-variable 'rcirc-process)
           (setq rcirc-process process)
           (make-local-variable 'rcirc-server)
           (setq rcirc-server server)
           (make-local-variable 'rcirc-server-name)
           (setq rcirc-server-name server) ; update when we get 001 response
           (make-local-variable 'rcirc-buffer-alist)
           (setq rcirc-buffer-alist nil)
           (make-local-variable 'rcirc-nick-table)
           (setq rcirc-nick-table (make-hash-table :test 'equal))
           (make-local-variable 'rcirc-nick)
           (setq rcirc-nick nick)
           (make-local-variable 'rcirc-process-output)
           (setq rcirc-process-output nil)
           (make-local-variable 'rcirc-startup-channels)
           (setq rcirc-startup-channels startup-channels)
           (make-local-variable 'rcirc-last-server-message-time)
           (setq rcirc-last-server-message-time (current-time))
           (make-local-variable 'rcirc-timeout-timer)
           (setq rcirc-timeout-timer nil)
           (make-local-variable 'rcirc-user-disconnect)
           (setq rcirc-user-disconnect nil)
           (make-local-variable 'rcirc-connecting)
           (setq rcirc-connecting t)

           (add-hook 'auto-save-hook 'rcirc-log-write)

           ;; identify
           (rcirc-send-string process (concat "PASS " public-passwd))
           (rcirc-send-string process (concat "NICK " nick))
           (rcirc-send-string process (concat "USER " user-name
                                              " hostname servername :"
                                              full-name))


           ;; setup ping timer if necessary
           (unless rcirc-keepalive-timer
             (setq rcirc-keepalive-timer
                   (run-at-time 0 (/ rcirc-timeout-seconds 2) 'rcirc-keepalive)))

           (message "Connecting to %s...done" server)

           ;; return process object
           process)))

     (defun rcirc-complete-nick ()
       "Cycle through nick completions from list of nicks in channel."
       (interactive)
       (if (eq last-command this-command)
           (setq rcirc-nick-completions
                 (append (cdr rcirc-nick-completions)
                         (list (car rcirc-nick-completions))))
         (setq rcirc-nick-completion-start-offset
               (- (save-excursion
                    (if (re-search-backward " " rcirc-prompt-end-marker t)
                        (1+ (point))
                      rcirc-prompt-end-marker))
                  rcirc-prompt-end-marker))
         (setq rcirc-nick-completions
               (let ((completion-ignore-case t))
                 (all-completions
                  (buffer-substring
                   (+ rcirc-prompt-end-marker
                      rcirc-nick-completion-start-offset)
                   (point))
                  (append (rcirc-channel-nicks (rcirc-buffer-process)
                                               rcirc-target)
                          (rcirc-commands))))))
       (let ((completion (car rcirc-nick-completions)))
         (when completion
           (rcirc-put-nick-channel (rcirc-buffer-process) completion rcirc-target)
           (delete-region (+ rcirc-prompt-end-marker
                             rcirc-nick-completion-start-offset)
                          (point))
           (insert (concat completion
                           (if (= (+ rcirc-prompt-end-marker
                                     rcirc-nick-completion-start-offset)
                                  rcirc-prompt-end-marker)
                               (if (eq (aref completion 0) ?/) " " ": ")))))))

     ;; FIXME: This needs a drastic speedup or some caching
     (defun rcirc-commands ()
       "Return a list of defined IRC commands.
If a command called rcirc-cmd-foo exists, the IRC command /FOO
will be part of the list returned."
       (let ((commands))
         (mapatoms (lambda (sym)
                     (let ((name (symbol-name sym)))
                       (when (and (commandp sym)
                                  (string= (substring name 0 (min (length name) 10))
                                           "rcirc-cmd-"))
                         (setq commands (cons (concat"/" (downcase (substring name 10)))
                                              commands))))))
         commands))

     (require 'rcirc-late-fix)
     ))

(provide 'rcirc-config)
;;; rcirc-config.el ends here
