;; -*- mode: emacs-lisp; mode: hi-lock; mode: page-break; auto-compile-lisp: nil; -*-
;; $Id$

;; Hi-lock: (("^;;; \\(.*\\)" (1 'hi-black-hb t)))
;; Hi-lock: (("^ +;;; \\(.*\\)" (1 'hi-black-b t)))
;; Hi-lock: ((";; \\(.*\\)" (1 'italic append)))
;; Hi-lock: end

;;; Basis
;; Load site-specific stuff: paths, accounts, projects...
(if (file-exists-p (expand-file-name "~/.emacs-local"))
    (load-file (expand-file-name "~/.emacs-local")))

;; Fix various "bad" default behaviors
(require 'patches)

(request 'visual)

;; My customizations are in a separate file
(if (file-exists-p (expand-file-name "~/.emacs-cust"))
    (load-file (expand-file-name "~/.emacs-cust")))

;; Hacked scroll margin
(set-scroll-margin 5 5 '("*eshell*" "*compile*" "*Calendar*"))
(setq scroll-step 1
      scroll-conservatively 15)

;; Save minibuffer history between sessions
(require 'save-history)

;; Date in mode line
(display-time)

;; Set window title according to buffer name
(setq frame-title-format '("emacs: %f"))

(when (request 'time-stamp)
  ;; Turn on time stamping
  (setq time-stamp-active t)
  ;; Sets new format for the time stamp, also used with the creation tag.
  (setq time-stamp-format "%02d/%02m/%:y %02H:%02M:%02S %U")

  (add-hook 'before-save-hook 'time-stamp))

;; Default mode
(setq default-major-mode 'indented-text-mode)
(add-hook 'indented-text-mode-hook 'turn-on-auto-fill)

;; We don't want to insert newlines when reaching end of buffer
;; and we want to kill the whole line when doing a C-k
(setq next-line-add-newlines nil
      kill-whole-line t
      kill-read-only-ok t)

(setq-default comment-style 'extra-line)

;; Auto-reload file when modified from external app
(global-auto-revert-mode 1)

;; Enable dynamic highlighting
(hi-lock-mode 1)

;; Throw out the mouse when typing
(mouse-avoidance-mode 'exile)

;; Load the emacs type verifier first (gnu emacs, xemacs, ...)
(request 'emacs-type)

(when (request 'desktop)
  (progn
    (desktop-save-mode 1)
    (desktop-read)))

;; Save place by default
(when (request 'saveplace)
  (setq-default save-place t))

;; Use Gnus for mail
(setq mail-user-agent 'gnus-user-agent)
(setq read-mail-command 'gnus)

;; Load default classes
(request 'emacs-d-vars)

;; open compressed files
(request 'jka-compr)
(if (not (jka-compr-installed-p)) (auto-compression-mode))

;; ignore case for finding files
(setq read-file-name-completion-ignore-case t)

;; modifier for navigating through windows with arrow keys
(windmove-default-keybindings 'alt)

(request 'autoloads)


;;; Charsets & languages

(add-to-list 'ispell-dictionary-alist '("latin" "[A-Za-z]" "[^A-Za-z]" "[']" nil nil "~tex" iso-8859-1))
(setq ispell-program-name "aspell")

;; (set-language-environment 'latin-1)
;; (prefer-coding-system 'latin-1)
(prefer-coding-system 'mule-utf-8)

(setq unibyte-display-via-language-environment t)
(setq-default ctl-arrow 'latin-9)

(when (request 'ucs-tables)
  (progn
    (unify-8859-on-encoding-mode 1)
    (unify-8859-on-decoding-mode 1)))

(defun sk-insert-euro (&optional arg) "Insert €"
  (interactive "*P")
  (if arg
      (insert (make-char 'mule-unicode-0100-24ff 116 76))
    (insert (make-char 'latin-iso8859-15 164))))

(global-set-key (kbd "H-5") 'sk-insert-euro)

(defun sk-insert-oe (&optional arg) "Insert œ"
  (interactive "*P")
  (insert (make-char 'latin-iso8859-15 #xBD)))

(global-set-key (kbd "H-o H-e") 'sk-insert-oe)


;;; Packages configuration

(defun current-configuration ()
  (if (member "gnus" command-line-args)
      'mail
    'code))

(defmacro when-configuration (config &rest body)
  "run BODY when in given configuration"
  (declare (indent 1))
  `(when (equal ,config (current-configuration))
     ,@body))

(defmacro unless-configuration (config &rest body)
  "run BODY unless in given configuration"
  (declare (indent 1))
  `(unless (equal ,config (current-configuration))
     ,@body))

;; common
(request 'buffer-config)

;; unless minimal
(unless-configuration 'minimal
  (request 'muse-config)
  (request 'tramp-config)
  (request 'eshell-config)
  (request 'paren-config)
  (request 'moccur-config)
  (request 'changelog-config)
  (request 'ediff-config)
  (request 'lispy-config)
  (request 'speedbar-config)
  (request 'cedet-config)
  (request 'tabbar-config)
  (request 'doxymacs-config)
  (request 'sawfish-config)
  (request 'highlight-changes-config)
  (request 'latex-config)
  (request 'fracc-config)
  (request 'pmwiki-config)
  (request 'lisp-config)
  (request 'shell-config)
  (request 'mycode)
  (request 'project-config)
  (request 'completion-config)
  (request 'crontab-config)
  (request 'flashcard-config))

;; mail only
(when-configuration 'mail
  (request 'calendar-config))

;;; Utils/Functions

(make-double-command my-home ()
  "Go to beginning of line, or beginning of buffer."
  nil
  (beginning-of-line)
  (beginning-of-buffer))

(make-double-command my-end ()
  "Go to end of line, or end of buffer."
  nil
  (end-of-line)
  (end-of-buffer))

;; TODO: needs some work
(defun yh/c-rearrange-electrics ()
  "Rearrange electric chars according to current c-style"
  (interactive)
  (save-excursion
    (mapcar (lambda (symb)
              (goto-char (point-min))
              (while (search-forward (car symb) (point-max) t)
                (let ((p (- (point) 1)))
                  (back-to-indentation)
                  (if (equal p (point))
                      (progn
                        (delete-indentation)
                        (if (eq (cdr symb) '+)
                            (forward-char)))
                    (goto-char p))
                  (delete-char 1)
                  (execute-kbd-macro (car symb)))))
            '(("{" +) ("}" -)))))

(defun simplify-blank-lines ()
  "Delete extra blank lines"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (string ?^ 10 10) nil t)
      (backward-char)
      (delete-blank-lines))))

(defun find-file-guessing (arg)
  "Call find-file with file at point if valid. With a universal argument, force call to find-file"
  (interactive "P")
  (let ((target (and (not arg) (request 'ffap) (ffap-guesser))))
    (if target
        (ffap)
      (call-interactively 'find-file))))

;; byte-compile current elisp buffer
(defun byte-compile-elisp ()
  "Byte compile the current buffer if possible"
  (if auto-compile-lisp
      (cond
       ((eq major-mode 'sawfish-mode)
        (sawfish-compile-this-file))
       ((eq major-mode 'emacs-lisp-mode)
        (byte-compile-file (buffer-file-name)))
       )))

;; make sure modifs are taken into account (use with caution)
(add-hook 'after-save-hook 'byte-compile-elisp)

;; I hate trailing whitespaces (use with caution)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; I also hate additional blank-lines (use with extreme caution)
;;(add-hook 'write-file-hooks 'simplify-blank-lines)

;; convert a buffer from dos ^M end of lines to unix end of lines
(defun dos2unix ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t) (replace-match ""))))

;; vice versa
(defun unix2dos ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "\r\n"))))

;; ASCII table function
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)  (switch-to-buffer "*ASCII*")  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)      (setq i (1+ i))
           (insert (format "%4d %c\n" i i))))  (beginning-of-buffer))

(defun yank-and-forward-line ()
  (interactive)
  (let ((old-col (current-column)))
    (yank)
    (forward-line)
    (while (and (not (eolp)) (> old-col 0))
      (forward-char)
      (setq old-col (1- old-col)))))

(defun totd ()
  (interactive)
  (with-output-to-temp-buffer "*Tip of the day*"
    (let* ((commands (loop for s being the symbols
                           when (commandp s) collect s))
           (command (nth (random (length commands)) commands)))
      (princ
       (concat "Your tip for the day is:\n========================\n\n"
               (describe-function command)
               "\n\nInvoke with:\n\n"
               (with-temp-buffer
                 (where-is command t)
                 (buffer-string)))))))

(defun kill-syntax-forward ()
  "Kill characters with syntax at point."
  (interactive)
  (let ((beg (point)))
    (skip-syntax-forward (string (char-syntax (char-after))))
    (kill-region beg (point))))

(defun kill-syntax-backward ()
  "Kill characters with syntax at point."
  (interactive)
  (let ((beg (point)))
    (skip-syntax-backward (string (char-syntax (char-before))))
    (kill-region beg (point))))

;; Adaptative "exit" behavior
(defun exit-no-gnuserv-or-frame ()
  "If in a gnuserv-edit session, close it. If in a secondary frame, close it. Else, die"
  (interactive)
  (cond ((and (boundp 'gnuserv-minor-mode) gnuserv-minor-mode) (gnuserv-edit))
	((or (not main-frame) (eq (selected-frame) main-frame)) (save-buffers-kill-emacs))
	((delete-frame))))

;; make active frame the main one
(defun make-main-frame ()
  "Make the current frame the primary one"
  (interactive)
  (setq main-frame (selected-frame)))

;; My init function for main window
(defun init ()
  (interactive)
  (let ((act (request 'winring)))
    (ecb-activate)
    (when act
      (progn
        (ecb-winman-winring-enable-support)
        (winring-initialize)))))

(defun ecb-vc-dir-managed-by-SVN (directory)
 "Return 'SVN if DIRECTORY is managed by SVN. nil if not."
 (and (file-exists-p (concat directory "/.svn/"))
      'SVN))

(request 'server)
(defun root-portal ()
  "Function to populate background"
  (interactive)
  ;; frame settings
  (xterm-mouse-mode 1)
  ;; prepare
  (delete-other-windows)
  ;; need to start this first for calendar buffer to end up lowest
  (calendar)
  ;; build window configuration
  (other-window 1)
  (split-window-horizontally)
  (other-window 1)
  ;; terminal window width
  (enlarge-window-horizontally
   (- 80 (window-width)))
  (split-window-vertically)
  (ansi-term "/bin/bash" "top")
  (term-exec (current-buffer) "top" "top" nil nil)
  (other-window -1)
  (find-file (concat planner-directory "/" planner-default-page))
  (split-window-vertically)
  (set-window-text-height (selected-window) 72)
  (other-window 1)
  (eshell)
  (other-window -1)
)

(make-main-frame)

(defun my-occur ()
  "Switch to *Occur* buffer, or run `moccur'."
  (interactive)
  (if (get-buffer "*Moccur*")
      (switch-to-buffer "*Moccur*")
    (call-interactively 'moccur)))

;; Do what I mean when I compile
;; ie chose the right compilation command
(defvar dwim-compile-alist '(("^\\(?:GNU\\)?[Mm]akefile$" . "make")
                             ("\\.pro$" . "qmake")
                             ("^SConstruct$" . "scons"))
  "Association list between filename patterns and building method")

(defun dwim-compile-check (motif)
  "Test a regexp against every file in the current directory. Tries to be smart about what \"current directory\" is."
  (let* ((filename (buffer-file-name (current-buffer)))
         (dir (if filename
                  (file-name-directory filename)
                default-directory))
         (l (directory-files dir)))
    (mapcond (lambda (f) (string-match motif f))
             'identity
             l)))

;; Use previous function to adapt compile-command value
(setq compile-command '(or (mapcond (lambda (e) (dwim-compile-check (car e))) 'cdr dwim-compile-alist)
                           "make"))

;; I hate to mix different compilations contexts (see make -C.. for example...)
(make-variable-buffer-local 'compile-history)

(defun diff-buffer-with-associated-file ()
  "View the differences between BUFFER and its associated file.
This requires the external program \"diff\" to be in your `exec-path'.
Returns nil if no differences found, 't otherwise."
  (interactive)
  (let ((buf-filename buffer-file-name)
        (buffer (current-buffer)))
    (unless buf-filename
      (error "Buffer %s has no associated file" buffer))
    (let ((diff-buf (get-buffer-create
                     (concat "*Assoc file diff: "
                             (buffer-name)
                             "*"))))
      (with-current-buffer diff-buf
        (setq buffer-read-only nil)
        (erase-buffer))
      (let ((tempfile (make-temp-file "buffer-to-file-diff-")))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (write-region (point-min) (point-max) tempfile nil 'nomessage))
              (if (zerop
                   (apply #'call-process "diff" nil diff-buf nil
                          (append
                           (when (and (boundp 'ediff-custom-diff-options)
                                      (stringp ediff-custom-diff-options))
                             (list ediff-custom-diff-options))
                           (list buf-filename tempfile))))
                  (progn
                    (message "No differences found")
                    nil)
                (progn
                  (with-current-buffer diff-buf
                    (goto-char (point-min))
                    (if (fboundp 'diff-mode)
                        (diff-mode)
                      (fundamental-mode)))
                  (display-buffer diff-buf)
                  t)))
          (when (file-exists-p tempfile)
            (delete-file tempfile)))))))

;; tidy up diffs when closing the file
(defun kill-associated-diff-buf ()
  (let ((buf (get-buffer (concat "*Assoc file diff: "
                             (buffer-name)
                             "*"))))
    (when (bufferp buf)
      (kill-buffer buf))))

(add-hook 'kill-buffer-hook 'kill-associated-diff-buf)

(defun de-context-kill (arg)
  "Kill buffer, taking gnuclient into account."
  (interactive "p")
  (when (and (buffer-modified-p)
             buffer-file-name
             (not (string-match "\\*.*\\*" (buffer-name)))
             ;; erc buffers will be automatically saved
             (not (eq major-mode 'erc-mode))
             (= 1 arg))
    (let ((differences 't))
    (when (file-exists-p buffer-file-name)
      (setq differences (diff-buffer-with-associated-file)))
    (error (if differences
               "Buffer has unsaved changes"
             "Buffer has unsaved changes, but no differences wrt. the file"))))
  (if (and (boundp 'gnuserv-minor-mode)
             gnuserv-minor-mode)
      (gnuserv-edit)
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))))


;;; Global key bindings

(global-set-key (kbd "<C-backspace>") 'kill-syntax-backward)

(global-set-key (kbd "<f3>") 'ecb-toggle-compile-window)
;; Depending on your keyboard you may want another one binding
(global-set-key (kbd "C-x ~") 'previous-error)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c k") 'mode-compile-kill)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c o") 'my-occur)

(global-set-key (kbd "<C-tab>") 'other-window)

;; These were traditional bindings, why did they change??
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;; "intelligent" home and end
(global-set-key (kbd "<C-home>") 'my-home)
(global-set-key (kbd "<C-end>") 'my-end)

;; mouse scrolling
(unless (featurep 'mwheel)
  (global-set-key (kbd "<mouse-4>") 'scroll-down)
  (global-set-key (kbd "<mouse-5>") 'scroll-up)
  (global-set-key (kbd "<C-mouse-4>") 'scroll-down-one-line)
  (global-set-key (kbd "<C-mouse-5>") 'scroll-up-one-line))

;; rectangle bindings. don't mix with registers! :)
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)
(global-set-key (kbd "C-x r C-y") 'yank-rectangle)

(global-set-key (kbd "C-x C-c") 'exit-no-gnuserv-or-frame)
(global-set-key (kbd "C-x 5 3") 'make-main-frame)
(global-set-key (kbd "C-c d") 'diff-buffer-with-associated-file)
(global-set-key (kbd "C-x k") 'de-context-kill)
(global-set-key (kbd "C-x K") (lambda () (interactive) (dolist (buf (buffer-list)) (kill-buffer buf))))

(global-set-key (kbd "C-c m") (lambda () (interactive) (gnus 2)))
(global-set-key (kbd "C-c x") 'chmod-file)
(global-set-key (kbd "C-c i") 'init)
(global-set-key (kbd "C-c h") 'auto-insert)
(global-set-key (kbd "C-x C-f") 'find-file-guessing)

(define-key global-map (kbd "H-s") 'isearchb-activate)

(global-set-key (kbd "<f5>")
                (lambda () (interactive)
                  (if (and (boundp 'ecb-minor-mode) ecb-minor-mode)
                      (ecb-deactivate)
                    (ecb-activate))))

(global-set-key (kbd "H-z") 'zap-upto-char)
(global-set-key (kbd "H-M-z") 'zap-to-char)
(global-set-key (kbd "H-Z") 'zap-following-char)
(global-set-key (kbd "H-M-Z") 'zap-from-char)

;; Project related keys
(global-set-key (kbd "<C-f1>") 'toggle-source-header)

;; replace a recently typed wrong character
(global-set-key (kbd "H-r")  'replace-recent-character)

;; use multiple selections
(when (request 'multi-region)
  (global-set-key (kbd "<H-return>") multi-region-map))

;; versioning keys
(request 'psvn-hacked)
(global-set-key (kbd "<M-f12>") 'svn-status)
(global-set-key (kbd "<C-f12>") 'cvs-update)

;; xterm settings
(when (and (string= "xterm" (getenv "TERM"))
           (request 'xterm-extras))
  (xterm-extra-keys))

(request 'elscreen)

(request 'w3m-load)

;; (request 'erc-config)
(request 'emacs-wiki-config)
;; (request 'remember-config)
(request 'planner-config)


;;; Experimental

(defun wrap-yank (beg end &optional arg)
  (interactive "rP")
  (save-excursion
    (goto-char end)
    (yank)
    (when arg
      (newline))
    (goto-char beg)
    (yank 2)
    (when arg
      (newline))))

(global-set-key (kbd "C-M-y") 'wrap-yank)

(autoload 'graphviz-dot-mode "graphviz-dot-mode" "" t nil)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

(when (request 'fold-dwim)
  (unless (boundp 'folding-mode)
    (setq folding-mode nil))
  (setq fold-dwim-outline-style-default 'nested)
  (global-set-key (kbd "<f7>")      'fold-dwim-toggle)
  (global-set-key (kbd "<M-f7>")    'fold-dwim-hide-all)
  (global-set-key (kbd "<S-M-f7>")  'fold-dwim-show-all))

(when (request 'emms)
  (request 'emms-default)
  (emms-setup 'cvs "~/music" "~/mp3")
  ;; Show the current track each time EMMS
  ;; starts to play a track with "NP : "
  (add-hook 'emms-player-started-hook 'emms-show)
  (setq emms-show-format "NP: %s")

  ;; When asked for emms-play-directory,
  ;; always start from this one
  ;;(setq emms-source-file-default-directory "~/music/")

  ;; Want to use alsa with mpg321 ?
  ;;(setq emms-player-mpg321-parameters '("-o" "alsa"))
  )

(add-to-list 'auto-mode-alist '("\\.hlal\\'" . c-mode))

;; (setq sgml-warn-about-undefined-entities nil)

;;************************************************************
;; configure HTML editing
;;************************************************************

(autoload 'php-mode "php-mode" "" t nil)

(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '2)
;;
(add-hook 'php-mode-user-hook 'turn-on-font-lock)

(when (request 'mmm-mode)
  (setq mmm-global-mode 'maybe)
  ;;
  ;; set up an mmm group for fancy html editing
  (mmm-add-group
   'fancy-html
   '(
     (html-php-tagged
      :submode php-mode
      :face mmm-code-submode-face
      :front "<[?]php"
      :back "^[ \t]*[?]>")
     (html-css-attribute
      :submode css-mode
      :face mmm-declaration-submode-face
      :front "style=\""
      :back "\"")))
  ;;
  ;; What files to invoke the new html-mode for?
  (add-to-list 'auto-mode-alist '("\\.inc\\'" . html-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . html-mode))
  (add-to-list 'auto-mode-alist '("\\.php[34]?\\'" . html-mode))
  (add-to-list 'auto-mode-alist '("\\.[sj]?html?\\'" . html-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-mode))
  ;;
  ;; What features should be turned on in this html-mode?
  (add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-js))
  (add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil embedded-css))
  (add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil fancy-html))
  ;;
  ;; Not exactly related to editing HTML: enable editing help with mouse-3 in all sgml files
  (defun go-bind-markup-menu-to-mouse3 ()
    (define-key sgml-mode-map [(down-mouse-3)] 'sgml-tags-menu))
  ;;
  (add-hook 'sgml-mode-hook 'go-bind-markup-menu-to-mouse3))

(add-to-list 'auto-mode-alist '("\\.[hi]\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.pro\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("SCons\\(cript\\|truct\\)\\'" . python-mode))

(message ".emacs loaded")
