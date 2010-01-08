;; -*- mode: emacs-lisp; mode: hi-lock; mode: orgstruct; auto-compile-lisp: nil; -*-
;; $Id: dotemacs.el 230 2007-04-09 09:10:18Z yann $

;; (toggle-debug-on-error)

;; load code specific to some major version
(if (file-exists-p (expand-file-name (format "~/.emacs-%d" emacs-major-version)))
    (load-file (expand-file-name (format "~/.emacs-%d" emacs-major-version))))

;; load code specific to some minor version
(if (file-exists-p (expand-file-name
                    (format "~/.emacs-%d-%d"
                            emacs-major-version emacs-minor-version)))
    (load-file (expand-file-name
                (format "~/.emacs-%d-%d"
                        emacs-major-version emacs-minor-version))))

;; Load site-specific stuff: paths, accounts, passwords, projects...
;; see http://gist.github.com/97984 for an example
(if (file-exists-p (expand-file-name "~/.emacs-local"))
    (load-file (expand-file-name "~/.emacs-local")))

(require 'undo-tree)

;; Customizations are in a separate file
(if (file-exists-p (expand-file-name "~/.emacs-cust"))
    (load-file (expand-file-name "~/.emacs-cust")))

;; Fix various "bad" default behaviors
;; add some personal features
(require 'patches)

;; How emacs should look like
(request 'visual)

(request 'cedet)

(when (request 'package)
  (package-initialize))

;; Hacked scroll margin
;; (set-scroll-margin 5 5 '("*eshell*" "*compile*" "*Calendar*"))
;; (setq scroll-step 1
;;       scroll-conservatively 50)
(setq ;; scroll-margin 5
 scroll-conservatively 50
 scroll-step 1)

;; Save minibuffer history between sessions
(when (request 'savehist)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'kill-ring))

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

;; Throw away the mouse when typing
(mouse-avoidance-mode 'exile)

;; Load the emacs type verifier (gnu emacs, xemacs, ...)
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

(require 'autoloads)

;;;_* Charsets & languages

(eval-after-load 'ispell
  '(progn
     (add-to-list 'ispell-dictionary-alist
                  '("latin" "[A-Za-z]" "[^A-Za-z]" "[']"
                    nil nil "~tex" iso-8859-1))
     (setq ispell-program-name "aspell")))

;; (set-language-environment 'utf-8)
;; (prefer-coding-system 'latin-1)
(prefer-coding-system 'mule-utf-8)

(setq unibyte-display-via-language-environment t)
(setq-default ctl-arrow 'latin-9)

(when (request 'ucs-tables)
  (progn
    (unify-8859-on-encoding-mode 1)
    (unify-8859-on-decoding-mode 1)))

(defun sk-insert-euro (&optional arg) "Insert euro symbol"
  (interactive "*P")
  (if arg
      (insert (make-char 'mule-unicode-0100-24ff 116 76))
    (insert (make-char 'latin-iso8859-15 164))))

(global-set-key (kbd "H-5") 'sk-insert-euro)

(defun sk-insert-oe (&optional arg) "Insert oe"
  (interactive "*P")
  (insert (make-char 'latin-iso8859-15 #xBD)))

(global-set-key (kbd "H-o H-e") 'sk-insert-oe)

;;;_* Packages configuration

;; I use emacs in 4 different ways from the command line:
;; - as a mail reader (gnus)
;; - as my $EDITOR (aliased as "vi" :p)
;; - as a proof environment (ProofGeneral)
;; - as my default coding environment
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
(request 'command-config)
(request 'buffer-config)
(request 'outline-config)
(request 'org-config)
(request 'remember-config)
(request 'calendar-config)
(request 'hideshow-config)

(unless-configuration 'proof
  (request 'ido-config))

;; unless minimal
(unless-configuration 'minimal
  (unless-configuration 'proof
    ;; (request 'muse-config)
    (require 'bbdb-config)
    (request 'ecb-config)
    (request 'winring-config)
    ;; (request 'planner-config)
    ;; (request 'circe-config)
    ;; (request 'rcirc-config)
    (request 'erc-config)
    (request 'tramp-config)
    (request 'eshell-config)
    (request 'help-config)
    (request 'moccur-config)
    ;; (request 'lispy-config)
    (request 'tabbar-config)
    ;; (request 'sawfish-config)
    ;; (request 'pmwiki-config)
    (request 'psvn-config)))

(when-configuration 'code
  ;; (request 'bm-config)
  (request 'changelog-config)
  (request 'cc-config)
  (request 'compile-config)
  (request 'ediff-config)
  ;; (request 'speedbar-config)
  ;; (request 'doxymacs-config)
  (request 'latex-config)
  ;; (request 'highlight-changes-config)
  ;; (request 'fracc-config)
  (request 'paren-config)
  (request 'lisp-config)
  (request 'slime-config)
  (request 'shell-config)
  (request 'python-config)
  (request 'mycode)
  (request 'project-config)
  (request 'completion-config)
  (request 'crontab-config)
  ;; (request 'flashcard-config)
  (request 'vc-config)
  (request 'yasnippet-config))

;; _* Utils/Functions

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

;; make my scripts executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; I hate trailing whitespaces (use with caution)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; I also hate additional blank-lines (use with extreme caution)
;;(add-hook 'write-file-hooks 'simplify-blank-lines)

;; Adaptative "exit" behavior
(defun exit-no-gnuserv-or-frame ()
  "If in a gnuserv-edit session, close it. If in a secondary
frame, close it. Else, die"
  (interactive)
  (cond ((and (boundp 'gnuserv-minor-mode) gnuserv-minor-mode)
         (gnuserv-edit))
        ((or (not main-frame) (eq (selected-frame) main-frame))
         (save-buffers-kill-emacs))
        ((delete-frame))))

;; make active frame the main one
(defun make-main-frame ()
  "Make the current frame the primary one"
  (interactive)
  (setq main-frame (selected-frame)))

;; My init function for main window
(defun init ()
  (interactive)
  (when (request 'ecb)
    (let ((act (request 'winring)))
      (ecb-activate)
      (when act
        (progn
          (ecb-winman-winring-enable-support)
          (winring-initialize))))))

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
                                        ;(init)
                                        ;
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

(global-set-key (kbd "C-c +") 'incr-dwim)
(global-set-key (kbd "C-c -") 'decr-dwim)

;(global-set-key (kbd "<f3>") 'ecb-toggle-compile-window)
;; Depending on your keyboard you may want another one binding
(global-set-key (kbd "C-x ~") 'previous-error)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c o") 'my-occur)
(global-set-key (kbd "C-c u") 'remember)
(global-set-key (kbd "C-c e") 'fc-eval-and-replace)
(global-set-key (kbd "C-c f") 'find-function)
(global-set-key (kbd "C-c F") 'find-function-on-key)
(global-set-key (kbd "C-c v") 'find-variable)

;; Enter a recursive edit. C-M-c will bring back exactly there
(global-set-key (kbd "C-c r") (lambda ()
                                (interactive)
                                (save-window-excursion
                                  (save-excursion
                                    (recursive-edit)))))

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

;; viewport scrolling

(global-set-key (kbd "<M-up>")
                (lambda (arg)
                  (interactive "p")
                  (or arg (setq arg 1))
                  (let ((top (line-number-at-pos (window-start)))
                        (cur (line-number-at-pos (point))))
                    (when (>= (- cur top) arg)
                      (scroll-up arg)))))

(global-set-key (kbd "<M-down>")
                (lambda (arg)
                  (interactive "p")
                  (or arg (setq arg 1))
                  (let ((cur (line-number-at-pos (point)))
                        (bot (+ (line-number-at-pos (window-start))
                                (window-text-height)
                                -1)))
                    (when (>= (- bot cur) arg)
                      (scroll-down arg)))))

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
(global-set-key (kbd "C-x K")
                (lambda () (interactive)
                  (dolist (buf (buffer-list))
                    (when (buffer-file-name buf)
                      (kill-buffer buf)))))

(global-set-key (kbd "C-c i") 'init)
(global-set-key (kbd "C-c h") 'auto-insert)

(global-set-key (kbd "H-s") 'isearchb-activate)

(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

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
(defvar yh/vcs-backends
  '((git . magit-status)
    (svn . svn-status)
    (cvs . cvs-status)))

(defun yh/vcs-backend (file)
  (cond ((vc-git-root file)
         'git)
        ((vc-svn-registered file)
         'svn)
        ((vc-cvs-registered file)
         'cvs)
        (t nil)))

(defun yh/vcs-status ()
  (interactive)
  (let ((backend (yh/vcs-backend (buffer-file-name))))
    (call-interactively (cdr (assoc backend yh/vcs-backends)))))

(global-set-key (kbd "<f12>") 'yh/vcs-status)

;;; windmove :)
(mapc #'eval
      (mapcar #'(lambda (dir)
                  (let ((name (symbol-name dir)))
                    `(global-set-key (kbd ,(format "C-x <%s>" name))
                                     ',(intern (concat "windmove-" name)))))
              '(left right up down)))

;; xterm settings
(when (and (string= "xterm" (getenv "TERM"))
           (request 'xterm-extras))
  (xterm-extra-keys))

(request 'w3m-load)

;;;_* Experimental

;; (require 'epa-setup)

(require 'tlfdd)
(add-to-list 'auto-mode-alist '("\\.fdd\\'" . tlfdd-mode))
(add-to-list 'auto-mode-alist '("\\.tlfdd\\'" . tlfdd-mode))
(add-to-list 'auto-mode-alist '("\\.tldoc\\'" . tlfdd-mode))

;; Doc-mode
;; (require 'doc-mode)
;; (add-hook 'c++-mode-hook 'doc-mode)
;; (add-hook 'java-mode-hook 'doc-mode)

;; Anything
;; (require 'anything)
;; (require 'anything-config)

;; Compagny-mode
(request 'company-autoloads)

;; Just in case compose is broken...
(define-key key-translation-map (kbd "<Multi_key>") 'iso-transl-ctl-x-8-map)
(autoload 'iso-transl-ctl-x-8-map "iso-transl"
  "Keymap for C-x 8 prefix." t 'keymap)

(when (request 'pp-c-l)
  (pretty-control-l-mode 1))

(autoload 'graphviz-dot-mode "graphviz-dot-mode" "" t nil)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

(add-to-list 'auto-mode-alist '("\\.hlal\\'" . c-mode))

(when (request 'incr)
  (delq 'rotate incr-enable-feature))

;; (setq sgml-warn-about-undefined-entities nil)

;;************************************************************
;; configure HTML editing
;;************************************************************

(autoload 'php-mode "php-mode" "" t nil)
(add-to-list 'auto-mode-alist '("\\.php[34]?\\'" . php-mode))

(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '2)

(add-hook 'php-mode-user-hook 'turn-on-font-lock)

(add-to-list 'auto-mode-alist '("\\.djava\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.pro\\'" . makefile-mode))

(global-set-key (kbd "C-x t") 'anchored-transpose)
(autoload 'anchored-transpose "anchored-transpose" nil t)

(setq server-socket-file "/tmp/emacs1000/server")
(unless (file-exists-p server-socket-file)
  (server-start))

;;; Startup code
(require 'org)
(when (and (boundp 'org-default-notes-file)
           (file-exists-p org-default-notes-file))
  (find-file org-default-notes-file)
  (require 'calendar)
  (call-interactively 'org-agenda-list)
  (eshell))

(autoload 'predictive-mode "predictive" "predictive" t)
(set-default 'predictive-auto-add-to-dict t)
(setq predictive-main-dict 'dict-english
      predictive-auto-learn t
      predictive-add-to-dict-ask nil
      predictive-use-auto-learn-cache nil
      predictive-which-dict t)

;; setup *scratch* correctly
(kill-scratch-buffer)

(when (fboundp 'indent-region-mode)
  (indent-region-mode -1))

(require 'hl-line+)
(defun hl-line-toggle-when-idle (&optional arg)
  "Turn on or off using `global-hl-line-mode' when Emacs is idle.
When on, use `global-hl-line-mode' whenever Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off."
  (interactive "P")
  (setq hl-line-when-idle-p
        (if arg (> (prefix-numeric-value arg) 0) (not hl-line-when-idle-p)))
  (cond (hl-line-when-idle-p
         (timer-activate-when-idle hl-line-idle-timer)
         (message "Turned ON using `global-hl-line-mode' when Emacs is idle."))
        (t
         (cancel-timer hl-line-idle-timer)
         (message "Turned OFF using `global-hl-line-mode' when
         Emacs is idle."))))

(defun hl-line-highlight-now ()
  "Turn on `global-hl-line-mode' and highlight current line now."
  (unless global-hl-line-mode
    (global-hl-line-mode 1)
    (global-hl-line-highlight)
    (add-hook 'pre-command-hook 'hl-line-unhighlight-now)
    ))

(defun hl-line-unhighlight-now ()
  "Turn off `global-hl-line-mode' and unhighlight current line now."
  (global-hl-line-mode -1)
  (global-hl-line-unhighlight)
  (remove-hook 'pre-command-hook 'hl-line-unhighlight-now))

(toggle-hl-line-when-idle 1)
(hl-line-when-idle-interval 1)


;; (require 'gtags)
;; (add-hook 'c-mode-common-hook 'gtags-mode)
(require 'etags)
(require 'etags-kill)

(require 'bookmark)
(defun switch-to-bookmark (bname)
  "Interactively switch to bookmark as `iswitchb' does."
  (interactive (list (flet ((ido-make-buffer-list
                             (default)
                             (bookmark-all-names)))
                       (ido-read-buffer "Jump to bookmark: " nil t))))
  (bookmark-jump bname))

(global-set-key (kbd "C-x B") 'switch-to-bookmark)
(request 'bookmark+)

;; (require 'radio)

;; (autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(eval-after-load "js2-mode"
  '(add-hook 'js2-mode (lambda ()
                         (glasses-mode 1)
                         (c-subword-mode 1))))

(setq js2-use-font-lock-faces t)

(require 'epa-dired)
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(add-hook 'mail-mode-hook 'epa-mail-mode)

(when (request 'compile-bookmarks)
  (compile-bookmarks-mode 1))

;; (require 'auto-dictionary)
;; (add-hook 'flyspell-mode-hook '(lambda () (auto-dictionary-mode 1)))

;; (request 'fringe-helper)
;; (request 'flymake)

;; (defvar flymake-fringe-overlays nil)
;; (make-variable-buffer-local 'flymake-fringe-overlays)

;; (defadvice flymake-make-overlay (after add-to-fringe first
;;                                        (beg end tooltip-text face mouse-face)
;;                                        activate compile)
;;   (push (fringe-helper-insert-region
;;          beg end
;;          (fringe-lib-load (if (eq face 'flymake-errline)
;;                               fringe-lib-exclamation-mark
;;                             fringe-lib-question-mark))
;;          'left-fringe 'font-lock-warning-face)
;;         flymake-fringe-overlays))

;; (defadvice flymake-delete-own-overlays (after remove-from-fringe activate
;;                                               compile)
;;   (mapc 'fringe-helper-remove flymake-fringe-overlays)
;;   (setq flymake-fringe-overlays nil))

(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))
(global-set-key "\M-\C-y" 'kill-ring-search)

(when (request 'haskell-mode)
  (add-hook 'haskell-mode-hook
            #'(lambda ()
                (setq comment-padding " ")
                (setq comment-start "--"))))

(request 'magit-config)

;; update agenda file after changes to org files
(defun th-org-mode-init ()
  (let* ((name (buffer-file-name))
         (current (and name (expand-file-name (buffer-file-name))))
         (all (mapcar 'expand-file-name
                      (org-agenda-files t))))
    (when (member current all)
      (add-hook 'after-save-hook 'th-org-update-agenda-file t t))))

(add-hook 'org-mode-hook 'th-org-mode-init)

;; that's the export function
(defun th-org-update-agenda-file (&optional force)
  (interactive)
  (save-excursion
    (save-window-excursion
      (let ((file "/tmp/org-agenda.txt"))
        (org-agenda-list)
        (org-write-agenda file)))))

;; do it once at startup
(th-org-update-agenda-file t)

(request 'magit)
(request 'ipa)

;; (eval-after-load "info" '(require 'info+))

(eval-after-load 'nxml-mode
  '(defun bf-pretty-print-xml-region (begin end)
     "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
     (interactive "r")
     (save-excursion
       (nxml-mode)
       (goto-char begin)
       (while (search-forward-regexp "\>[ \\t]*\<" nil t)
         (backward-char) (insert "\n"))
       (indent-region begin end))
     (message "Ah, much better!")))

(when (request 'test-case-mode)
  (add-hook 'find-file-hook 'enable-test-case-mode-if-test))

;; This must be last
(request 'smex)

(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.

Set it to nil using let in around-advice for functions where the
original completing-read is required.  For example, if a function
foo absolutely must use the original completing-read, define some
advice like this:

(defadvice foo (around original-completing-read-only activate)
  (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
(defadvice completing-read
  (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (boundp 'ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                                     allcomp
                                     nil require-match initial-input hist def))
        ad-do-it))))

(message ".emacs loaded")