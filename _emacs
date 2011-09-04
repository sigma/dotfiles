;; -*- mode: emacs-lisp; mode: hi-lock; mode: orgstruct; auto-compile-lisp: nil; -*-

;; (toggle-debug-on-error)

(defun yh/load-configuration-file (filename)
  (let ((file (expand-file-name filename)))
    (if (file-exists-p file)
        (load-file file))))

;; load code specific to some major version
(yh/load-configuration-file (format "~/.emacs-%d" emacs-major-version))

;; load code specific to some minor version
(yh/load-configuration-file (format "~/.emacs-%d-%d"
                                    emacs-major-version emacs-minor-version))

;; Load site-specific stuff: paths, accounts, passwords, projects...
;; see http://gist.github.com/97984 for an example
(yh/load-configuration-file "~/.emacs-local")

;; Customizations are in a separate file
(yh/load-configuration-file "~/.emacs-cust")

;; Fix various "bad" default behaviors
;; add some personal features
(require 'patches)

(request 'undo-tree)

;; How emacs should look like
(request 'visual)

(request 'cedet)

(when (request 'package)
  (setq package-archives
        '(("ELPA" . "http://tromey.com/elpa/")
          ("gnu" . "http://elpa.gnu.org/packages/")
          ("marmalade" . "http://marmalade-repo.org/packages/")))
  (package-initialize))

(setq scroll-margin 0
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
(setq-default major-mode 'indented-text-mode)
(add-hook 'indented-text-mode-hook 'turn-on-auto-fill)

;; I don't want to insert newlines when reaching end of buffer
;; and I want to kill the whole line when doing a C-k
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

;; Save place by default
(when (request 'saveplace)
  (setq-default save-place t))

;; Use Gnus for mail
(setq mail-user-agent 'gnus-user-agent)
(setq read-mail-command 'gnus)

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

(prefer-coding-system 'mule-utf-8)

(setq unibyte-display-via-language-environment t)

(when (request 'ucs-tables)
  (progn
    (unify-8859-on-encoding-mode 1)
    (unify-8859-on-decoding-mode 1)))

;;;_* Packages configuration

(request 'command-config)
(request 'buffer-config)
(request 'outline-config)
(request 'org-config)
(request 'remember-config)
(request 'calendar-config)
(request 'hideshow-config)
(request 'ido-config)
(require 'bbdb-config)
(request 'ecb-config)
(request 'winring-config)
;; (request 'rcirc-config)
(request 'erc-config)
(request 'tramp-config)
(request 'eshell-config)
(request 'help-config)
(request 'moccur-config)
;; (request 'lispy-config)
;; (request 'tabbar-config)
(request 'psvn-config)
(request 'bm-config)
(request 'changelog-config)
(request 'cc-config)
(request 'compile-config)
(request 'ediff-config)
(request 'hl-line-config)
(request 'info-config)
;; (request 'speedbar-config)
;; (request 'doxymacs-config)
(request 'latex-config)
(request 'highlight-changes-config)
(request 'paren-config)
(request 'lisp-config)
(request 'slime-config)
(request 'shell-config)
(request 'python-config)
(request 'mycode)
(request 'tags-config)
(request 'completion-config)
(request 'crontab-config)
;; (request 'flashcard-config)
(request 'vc-config)
(request 'yasnippet-config)
(request 'web-config)
(request 'magit-config)
(request 'scratch-config)

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
(defvar auto-compile-lisp t)

(defun byte-compile-elisp ()
  "Byte compile the current buffer if possible"
  (if (and auto-compile-lisp
           (not (backup-file-name-p (buffer-file-name))))
      (cond
       ((eq major-mode 'sawfish-mode)
        (sawfish-compile-this-file))
       ((eq major-mode 'emacs-lisp-mode)
        (byte-compile-file (buffer-file-name))))))

;; make sure modifs are taken into account (use with caution)
(add-hook 'after-save-hook 'byte-compile-elisp)

;; make my scripts executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; I hate trailing whitespaces (use with caution)
;;(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; I also hate additional blank-lines (use with extreme caution)
;;(add-hook 'write-file-hooks 'simplify-blank-lines)

(defvar main-frame nil)

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

(request 'server)

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

(global-set-key (kbd "C-c +") 'incr-dwim)
(global-set-key (kbd "C-c -") 'decr-dwim)

;; Depending on your keyboard you may want another one binding
(global-set-key (kbd "C-x ~") 'previous-error)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c o") 'my-occur)
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

(global-set-key (kbd "C-c h") 'auto-insert)

(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

;; versioning keys
(defvar yh/vcs-backends
  '((git . magit-status)
    (svn . svn-status)
    (cvs . cvs-status)))

;; fallback to git by default
(require 'vc-git)
(require 'vc-svn)
(require 'vc-cvs)
(defun yh/vcs-backend (file)
  (cond ((null file) 'git)
        ((vc-git-root file) 'git)
        ((vc-svn-registered file) 'svn)
        ((vc-cvs-registered file) 'cvs)
        (t 'git)))

(defun yh/vcs-status ()
  (interactive)
  (let ((backend (yh/vcs-backend (or buffer-file-name default-directory))))
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

;;;_* Experimental

(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

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

(add-to-list 'auto-mode-alist '("\\.djava\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.pro\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.make\\'" . makefile-mode))

(global-set-key (kbd "C-x t") 'anchored-transpose)
(autoload 'anchored-transpose "anchored-transpose" nil t)

(setq server-socket-file "/tmp/emacs1000/server")
(unless (file-exists-p server-socket-file)
  (server-start))

;;; Startup code
(require 'org-config)
(when (and (boundp 'org-default-notes-file)
           (file-exists-p org-default-notes-file))
  (find-file org-default-notes-file)
  (require 'calendar)
  (call-interactively 'org-agenda-list))

(eval-after-load "js2-mode"
  '(add-hook 'js2-mode (lambda ()
                         (glasses-mode 1)
                         (subword-mode 1))))

(setq js2-use-font-lock-faces t)

(require 'epa-dired)
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(add-hook 'mail-mode-hook 'epa-mail-mode)

(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))
(global-set-key "\M-\C-y" 'kill-ring-search)

(when (request 'haskell-mode)
  (add-hook 'haskell-mode-hook
            #'(lambda ()
                (setq comment-padding " ")
                (setq comment-start "--"))))

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

(when (and (request 'folding)
           (request 'fold-dwim))
  (global-set-key (kbd "<A-tab>") 'fold-dwim-toggle))


;; Detect endianness of UTF-16 containing a Byte Order Mark U+FEFF
(add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE" . utf-16-le) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF" . utf-16-be) t)
;; Add missing support functions
(defun utf-16-le-pre-write-conversion (start end) nil)
(defun utf-16-be-pre-write-conversion (start end) nil)

;; Detect endianness of UTF-16 containing a Byte Order Mark U+FEFF
;; Detect EOL mode by looking for CR/LF on the first line
(add-to-list 'auto-coding-regexp-alist
             '("^\xFF\xFE.*\x0D\x00$" . utf-16-le-dos) t)
(add-to-list 'auto-coding-regexp-alist
             '("^\xFE\xFF.*\x0D\x00$" . utf-16-be-dos) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE" . utf-16-le) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF" . utf-16-be) t)

(let ((arch-regexp "\\.\\(war\\|ear\\|sar\\|egg\\|dar\\|package\\)\\'"))
  (add-to-list 'auto-mode-alist `(,arch-regexp . archive-mode))
  (modify-coding-system-alist 'file arch-regexp 'no-conversion))

(setq auto-coding-functions (delete 'sgml-xml-auto-coding-function
                                    auto-coding-functions))

(defvar indirect-mode-name nil
  "Mode to set for indirect buffers.")
(make-variable-buffer-local 'indirect-mode-name)

(defun indirect-region (start end)
  "Edit the current region in another buffer.
    If the buffer-local variable `indirect-mode-name' is not set, prompt
    for mode name to choose for the indirect buffer interactively.
    Otherwise, use the value of said variable as argument to a funcall."
  (interactive "r")
  (let ((buffer-name (generate-new-buffer-name "*indirect*"))
        (mode
         (if (not indirect-mode-name)
             (setq indirect-mode-name
                   (intern
                    (completing-read
                     "Mode: "
                     (mapcar (lambda (e)
                               (list (symbol-name e)))
                             (apropos-internal "-mode$" 'commandp))
                     nil t)))
           indirect-mode-name)))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (funcall mode)
    (narrow-to-region start end)
    (goto-char (point-min))
    (shrink-window-if-larger-than-buffer)))

(global-set-key (kbd "C-x n N") 'indirect-region)

(define-key minibuffer-local-completion-map " " nil)

(require 'data-debug)
(global-set-key "\M-:" 'data-debug-eval-expression)

;; This must be last
(when (request 'smex)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c M-x") 'execute-extended-command))

(request 'gist)

(message ".emacs loaded")
