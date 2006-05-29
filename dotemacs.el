;; -*- mode: emacs-lisp; mode: hi-lock; auto-compile-lisp: nil; -*-
;; $Id$

;; Hi-lock: (("^;;;_\\* \\(.*\\)" (1 'hi-black-hb t)))
;; Hi-lock: (("^;;;_ \\+ \\(.*\\)" (1 'hi-black-b t)))
;; Hi-lock: ((";; \\(.*\\)" (1 'italic append)))
;; Hi-lock: end

(toggle-debug-on-error)

;;;_* Basis

(if (file-exists-p (expand-file-name (format "~/.emacs-%d" emacs-major-version)))
    (load-file (expand-file-name (format "~/.emacs-%d" emacs-major-version))))

(if (file-exists-p (expand-file-name (format "~/.emacs-%d-%d" emacs-major-version emacs-minor-version)))
    (load-file (expand-file-name (format "~/.emacs-%d-%d" emacs-major-version emacs-minor-version))))

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
;; (setq scroll-margin 5
;;       scroll-conservatively 50
;;       scroll-step 1)


;; Save minibuffer history between sessions
(when (request 'savehist)
  (savehist-load))

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
(if (functionp 'global-hi-lock-mode)
            (global-hi-lock-mode 1)
          (hi-lock-mode 1))

;; Throw out the mouse when typing
(mouse-avoidance-mode 'exile)

;; Load the emacs type verifier first (gnu emacs, xemacs, ...)
(request 'emacs-type)

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

(request 'autoloads)

;;;_* Charsets & languages

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

;;;_* Packages configuration

(defun current-configuration ()
  (cond ((member "gnus" command-line-args) 'mail)
        ((equal (car command-line-args) "vi") 'minimal)
        ((member "proof-splash-display-screen" command-line-args) 'proof)
        (t 'code)))

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
(request 'cedet-config)
(request 'org-config)
(request 'calendar-config)

(unless-configuration 'proof
  (request 'ido-config))

;; unless minimal
(unless-configuration 'minimal
  (unless-configuration 'proof
    ;; (request 'muse-config)
    (request 'ecb-config)
    ;; (request 'planner-config)
    (request 'tramp-config)
    (request 'eshell-config)
    (request 'help-config)
    (request 'moccur-config)
    (request 'lispy-config)
    (request 'tabbar-config)
    (request 'sawfish-config)
    (request 'pmwiki-config)
    (request 'psvn-config)))

(unless-configuration 'minimal
  (unless-configuration 'mail
    (request 'changelog-config)
    (request 'compile-config)
    (request 'ediff-config)
    (request 'speedbar-config)
    (request 'doxymacs-config)
    (request 'latex-config)
    (request 'highlight-changes-config)
    (request 'fracc-config)
    (request 'paren-config)
    (request 'lisp-config)
    (request 'shell-config)
    (request 'mycode)
    (request 'project-config)
    (request 'completion-config)
    (request 'crontab-config)
    (request 'flashcard-config)
    (request 'vc-config)))

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

;; byte-compile current elisp buffer
(defun byte-compile-elisp ()
  "Byte compile the current buffer if possible"
  (if (and auto-compile-lisp
           (not (backup-file-name-p (buffer-file-name))))
      (cond
       ((eq major-mode 'sawfish-mode)
        (sawfish-compile-this-file))
       ((eq major-mode 'emacs-lisp-mode)
        (byte-compile-file (buffer-file-name)))
       )))

;; make sure modifs are taken into account (use with caution)
(add-hook 'after-save-hook 'byte-compile-elisp)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; I hate trailing whitespaces (use with caution)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; I also hate additional blank-lines (use with extreme caution)
;;(add-hook 'write-file-hooks 'simplify-blank-lines)

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
  (other-window -1))

(make-main-frame)

;; tidy up diffs when closing the file
(defun kill-associated-diff-buf ()
  (let ((buf (get-buffer (concat "*Assoc file diff: "
                             (buffer-name)
                             "*"))))
    (when (bufferp buf)
      (kill-buffer buf))))

(add-hook 'kill-buffer-hook 'kill-associated-diff-buf)

;;;_* Global key bindings

(global-set-key (kbd "<C-backspace>") 'kill-syntax-backward)

(global-set-key (kbd "C-c +") 'increment-number-at-point)

(global-set-key (kbd "<f3>") 'ecb-toggle-compile-window)
;; Depending on your keyboard you may want another one binding
(global-set-key (kbd "C-x ~") 'previous-error)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c k") 'mode-compile-kill)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c o") 'my-occur)
(global-set-key (kbd "C-c e") 'fc-eval-and-replace)
(global-set-key (kbd "C-c f") 'find-function)
(global-set-key (kbd "C-c F") 'find-function-on-key)

(global-set-key (kbd "<C-tab>") 'other-window)

;; These were traditional bindings, why did they change??
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "<delete>") 'delete-char)

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
(global-set-key (kbd "C-x K") (lambda () (interactive) (dolist (buf (buffer-list)) (when (buffer-file-name buf) (kill-buffer buf)))))

(global-set-key (kbd "C-c m") (lambda () (interactive) (gnus 2)))
(global-set-key (kbd "C-c x") 'chmod-file)
(global-set-key (kbd "C-c i") 'init)
(global-set-key (kbd "C-c h") 'auto-insert)
;;(global-set-key (kbd "C-x C-f") 'find-file-guessing)

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
(global-set-key (kbd "<M-f12>") 'svn-status)
(global-set-key (kbd "<C-f12>") 'cvs-update)

;; xterm settings
(when (and (string= "xterm" (getenv "TERM"))
           (request 'xterm-extras))
  (xterm-extra-keys))

(request 'elscreen)

(request 'w3m-load)

;;;_* Experimental

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
  (emms-setup 'advanced "~/music")
  ;; Show the current track each time EMMS
  ;; starts to play a track with "NP : "
;  (add-hook 'emms-player-started-hook 'emms-show)
;  (setq emms-show-format "NP: %s")

  ;; When asked for emms-play-directory,
  ;; always start from this one
  ;;(setq emms-source-file-default-directory "~/music/")

  ;; Want to use alsa with mpg321 ?
  ;;(setq emms-player-mpg321-parameters '("-o" "alsa"))
  )

(add-to-list 'auto-mode-alist '("\\.hlal\\'" . c-mode))

(request 'bookmark)

;; (setq sgml-warn-about-undefined-entities nil)

;;************************************************************
;; configure HTML editing
;;************************************************************

(autoload 'php-mode "php-mode" "" t nil)
(add-to-list 'auto-mode-alist '("\\.php[34]?\\'" . php-mode))

(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '2)
;;
(add-hook 'php-mode-user-hook 'turn-on-font-lock)

;; (when (request 'mmm-mode)
;;   (setq mmm-global-mode 'maybe)
;;   ;;
;;   ;; set up an mmm group for fancy html editing
;;   (mmm-add-group
;;    'fancy-html
;;    '(
;;      (html-php-tagged
;;       :submode php-mode
;;       :face mmm-code-submode-face
;;       :front "<[?]php"
;;       :back "^[ \t]*[?]>")
;;      (html-css-attribute
;;       :submode css-mode
;;       :face mmm-declaration-submode-face
;;       :front "style=\""
;;       :back "\"")))
;;   ;;
;;   ;; What files to invoke the new html-mode for?
;;   (add-to-list 'auto-mode-alist '("\\.inc\\'" . html-mode))
;;   (add-to-list 'auto-mode-alist '("\\.phtml\\'" . html-mode))
;;   (add-to-list 'auto-mode-alist '("\\.php[34]?\\'" . html-mode))
;;   (add-to-list 'auto-mode-alist '("\\.[sj]?html?\\'" . html-mode))
;;   (add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-mode))
;;   ;;
;;   ;; What features should be turned on in this html-mode?
;;   (add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-js))
;;   (add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil embedded-css))
;;   (add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil fancy-html))
;;   ;;
;;   ;; Not exactly related to editing HTML: enable editing help with mouse-3 in all sgml files
;;   (defun go-bind-markup-menu-to-mouse3 ()
;;     (define-key sgml-mode-map [(down-mouse-3)] 'sgml-tags-menu))
;;   ;;
;;   (add-hook 'sgml-mode-hook 'go-bind-markup-menu-to-mouse3))

(add-to-list 'auto-mode-alist '("\\.[hi]\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.djava\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.pro\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("SCons\\(cript\\|truct\\)\\'" . python-mode))

(when (request 'type-break)
  (setq type-break-file-name nil)
  (type-break-mode 1))

;(require 'allout)
;(allout-init t)

(global-set-key (kbd "C-x t") 'anchored-transpose)
(autoload 'anchored-transpose "anchored-transpose" nil t)

(autoload 'run-acl2 "top-start-inferior-acl2" "Begin ACL2 in an inferior ACL2 mode buffer." t)

;;; SLIME & Lisp
(when (request 'slime)
  ;; default
  (setq inferior-lisp-program "cmucl")
  (setq slime-edit-definition-fallback-function 'find-tag)
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (slime-setup :autodoc t)
  (global-set-key (kbd "<f12>") 'slime-selector))

(require 'proof-site)
;(setq coq-version-is-V8-1 t)

(when-configuration 'code
;;  (request 'icicles-config)
  (try
   (server-mode 1)))

;;; Startup code
(when (file-exists-p org-default-notes-file)
  (find-file org-default-notes-file)
  (setq default-directory "~/")
  (require 'calendar)
  (when (require 'org nil t)
    (call-interactively 'org-agenda-list)))

(message ".emacs loaded")
