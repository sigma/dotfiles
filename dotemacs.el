;; -*- mode: emacs-lisp; mode: hi-lock; mode: page-break; auto-compile-lisp: nil; -*-
;; $Id: dotemacs.el,v 1.64 2004/11/19 10:18:50 sigma Exp $

;; Hi-lock: (("^;;; \\(.*\\)" (1 'hi-black-hb t)))
;; Hi-lock: (("^ +;;; \\(.*\\)" (1 'hi-black-b t)))
;; Hi-lock: ((";; \\(.*\\)" (1 'italic append)))
;; Hi-lock: end

(toggle-debug-on-error)

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
;; and we want to kill the whole line when doing a Ctrl-k
(setq next-line-add-newlines nil
      kill-whole-line t
      kill-read-only-ok t)

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

(defun sk-insert-euro (&optional arg) "Insert Euro ISO 8859-15."
  (interactive "*P")
  (if arg
      (insert (make-char 'mule-unicode-0100-24ff 116 76))
    (insert (make-char 'latin-iso8859-15 164))))

(global-set-key (kbd "H-5") 'sk-insert-euro)

(defun sk-insert-oe (&optional arg) "Insert oe"
  (interactive "*P")
  (insert (make-char 'latin-iso8859-15 #xBD)))

(global-set-key (kbd "H-o H-e") 'sk-insert-oe)


;;; Buffers

(eval-after-load "buff-menu" '(request 'buff-menu+))
(global-set-key (kbd "C-x C-b") 'buffer-menu)

(define-key dired-mode-map (kbd "C-c C-c") 'wdired-change-to-wdired-mode)


;;; Packages configuration

;;; Calendar

(when (request 'calendar)
  (setq european-calendar-style t))

;;; Muse

(when (request 'muse-mode)
  (progn
    (request 'muse-html)
    (request 'muse-latex)
    (request 'muse-texinfo)
    (request 'muse-docbook)))

(add-to-list 'auto-mode-alist '("\\.muse$"  . muse-mode))

;;; Changelog

(when (request 'project)
  (defun yh/project-changelog-file ()
    (let ((rep (file-name-directory (buffer-file-name)))
          (projects (delete-if 'not (mapcar (lambda (p) (yh/project-get p 'root)) (yh/project-list)))))
      (mapcond (lambda (s) (string-match (expand-file-name s) rep))
               (lambda (s) (expand-file-name (concat s "/Changelog")))
               projects)))

  (defadvice add-change-log-entry (around ad-add-change-log-entry act)
    "Override default ChangeLog file according to project directory"
    (let ((change-log-default-name (yh/project-changelog-file)))
      ad-do-it))
  )

(add-hook 'change-log-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c")
                                    (lambda () (interactive) (save-buffer) (kill-this-buffer)))))

;;; Ediff

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Some custom configuration to ediff
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")

(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")

(defcustom my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my-ediff-bwin-config (current-window-configuration))
  (set-register my-ediff-bwin-reg
                (list my-ediff-bwin-config (point-marker))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my-ediff-awin-config (current-window-configuration))
  (set-register my-ediff-awin-reg
                (list my-ediff-awin-config (point-marker))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash)
(add-hook 'ediff-quit-hook 'my-ediff-qh)

(defun ediff-add-changelog (&optional key)
  (interactive)
  (with-current-buffer
      (ediff-get-buffer
       (ediff-char-to-buftype (or key last-command-char)))
    (add-change-log-entry-other-window)))

(add-hook 'ediff-keymap-setup-hook (lambda ()
                                     (define-key ediff-mode-map ".a" 'ediff-add-changelog)
                                     (define-key ediff-mode-map ".b" 'ediff-add-changelog)
                                     (define-key ediff-mode-map ".c" 'ediff-add-changelog)))

;;; Tramp : transparent remote editing

(when (request 'tramp)
  (setq tramp-default-method "ssh"))


;;; Lispy : client for telnet-based chat server

(autoload 'lispy "lispy-session" "" t nil)
(eval-after-load "lispy"
  '(progn
     (request 'lispy-commands)
     (request 'lispy-history)
     (request 'lispy-font-lock)
     (request 'lispy-occur)
     (request 'lispy-h4x0r)
     (request 'lispy-osd)
     (request 'lispy-autoreconnect)
     (request 'lispy-limit)
     ))

;;; Eshell : Emacs shell

(add-hook 'eshell-post-command-hook 'eshell-show-maximum-output)

;;; Speed bar : usefull for displaying various informations

(request 'speedbar)
;; Texinfo fancy chapter tags
(add-hook 'texinfo-mode-hook (lambda () (request 'sb-texinfo)))

;; HTML fancy chapter tags
(add-hook 'html-mode-hook (lambda () (request 'sb-html)))

;;; Tabbar

(when (request 'tabbar)
  (tabbar-mode)
  (global-set-key '[(shift right)] 'tabbar-forward-tab)
  (global-set-key '[(shift left)]  'tabbar-backward-tab)
  (global-set-key '[(shift up)]    'tabbar-forward-group)
  (global-set-key '[(shift down)]  'tabbar-backward-group))

;;; CEDET
(setq semantic-load-turn-useful-things-on t)
(request 'cedet)

;;; Doxymacs : great documentation system

(add-hook 'c-mode-common-hook 'doxymacs-mode)
(eval-after-load "doxymacs"
  '(progn
     (setq doxymacs-relative-path "Doc/html"
           doxymacs-use-external-xml-parser t)
     (defun my-doxymacs-font-lock-hook ()
       (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
           (font-lock-add-keywords nil doxymacs-doxygen-keywords)
         ))
     (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)))

;;; Sawfish : the ultimate window manager

;; Open sawfish-realted files with the proper mode
(setq auto-mode-alist (append '(("\\.sawfishrc$"  . sawfish-mode)
                                ("\\.jl$"         . sawfish-mode)
                                ("\\.sawfish/rc$" . sawfish-mode)) auto-mode-alist))

;; TODO: investigate why this code is not loaded from sawfish.el
(eval-after-load "sawfish"
  '(font-lock-add-keywords 'sawfish-mode
                           (list
                            ;; highlight define-*
                            (list
                             sawfish-defines-regexp
                             '(1 font-lock-keyword-face)
                             `(,(regexp-opt-depth sawfish-defines-regexp)
                               font-lock-variable-name-face nil t))
                            ;; extra keywords
                            (if sawfish-extra-keyword-list
                                (list (concat "\\<"
                                              `,(regexp-opt sawfish-extra-keyword-list)
                                              "\\>")
                                      '(0 font-lock-keyword-face)))
                            ;; highlight warnings
                            (if sawfish-warning-keyword-list
                                (list (concat "\\<"
                                              `,(regexp-opt sawfish-warning-keyword-list)
                                              "\\>")
                                      '(0 font-lock-warning-face prepend))))))

;;; Highlight-changes
(add-hook 'highlight-changes-enable-hook
          (lambda ()
            (local-set-key "\C-c+" 'highlight-changes-next-change)
            (local-set-key "\C-c-" 'highlight-changes-previous-change)
            (local-set-key (kbd "C-c DEL")
                           (lambda ()
                             (interactive)
                             (let ((mod (buffer-modified-p)))
                               (highlight-changes-remove-highlight (point-min) (point-max))
                               (restore-buffer-modified-p mod))))
            (local-set-key "\C-c_" 'highlight-changes-rotate-faces)))

;;; LaTeX

;; (request 'typopunct)
;; (typopunct-change-language 'francais t)
(eval-after-load "latex"
  '(add-to-list 'LaTeX-style-list '("prosper")))

(setq reftex-plug-into-AUCTeX t)
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)

(when (request 'tex-site)
  (progn
    (setq-default TeX-master t)
    ;; reftex helps managing references, toc, ...
    (add-hook 'LaTeX-mode-hook 'reftex-mode)
    ;; show/hide parts of your document
    (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
    ;; preview-latex is great
    (when (request 'preview)
      (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup))
    ;; point my typos
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    ;; use abbrev
    (add-hook 'LaTeX-mode-hook 'abbrev-mode)
    ;; Most people don't want that... I do
    ;; highlight *any* change, color rotation
    (add-hook 'LaTeX-mode-hook 'highlight-changes-mode)
    ;;       ;; DWIM with quotes
                                        ;       (add-hook 'LaTeX-mode-hook 'typopunct-mode)
    (defun my-LaTeX-hook ()
      ;; I like to have my own verbatim contructions well indented
      (setq font-lock-defaults
            '((font-latex-keywords font-latex-keywords-1 font-latex-keywords-2)
              nil nil
              ((40 . ".")
               (41 . ".")
               (36 . "\""))
              nil
              (font-lock-comment-start-regexp . "%")
              (font-lock-mark-block-function . mark-paragraph)
              (font-lock-syntactic-keywords
               ("^\\\\begin *{verbatim\\*?}\\(.?\\).*\\(\n\\)"
                (1 "<")
                (2 "|"))
               ("\\(\n\\)\\\\end *{verbatim\\*?}\\(.?\\)"
                (1 "|")
                (2 "<"))
               ("^\\\\begin *{sigmalog\\*?}\\(.?\\).*\\(\n\\)"
                (1 "<")
                (2 "|"))
               ("\\(\n\\)\\\\end *{sigmalog\\*?}\\(.?\\)"
                (1 "|")
                (2 "<"))
               ("\\\\verb\\*?\\([^a-z@]\\).*?\\(\\1\\)"
                (1 "\"")
                (2 "\""))))))

    (add-hook 'LaTeX-mode-hook 'my-LaTeX-hook)
    ))

;;; Parenthesis

;; Use mic-paren in replacement of standard paren.el
(when (and (or (string-match "XEmacs\\|Lucid" emacs-version) window-system)
           (request 'mic-paren))
  (paren-activate)                      ; activating
  (add-hook 'c-mode-common-hook
            (function (lambda ()
                        (paren-toggle-open-paren-context 1))))
  ;; In LaTeX-mode we want this
  (add-hook 'LaTeX-mode-hook
            (function (lambda ()
                        (paren-toggle-matching-quoted-paren 1)
                        (paren-toggle-matching-paired-delimiter 1)))))

;; Fancy paren highlighting
(when (request 'cparen)
  (cparen-activate))

;;; Moccur

;; use colored and editable moccur
(when (request 'color-moccur)
  (request 'moccur-edit))

;;; Fracc : french accent mode

(autoload 'fracc-mode "fracc" "" t nil)
(defun install-french-accent-mode-if-needed ()
  "Install French accent mode if the buffer seems to contain French text.
The guess is made by computing the proportion of letters with accents. If
there are more than 1% of such letters then turn French accent mode on."
  (save-excursion
    (goto-char (point-min))
    (let ((n 0)(size (- (point-max) (point-min))))
      (while (re-search-forward "\\\\['^`][eauo]" (point-max) t)
        (setq n (+ n 1)) )
      (while (re-search-forward "[éèàùçêë]" (point-max) t)
        (setq n (+ n 1)) )
      (message "diacritic/normal ratio = %d/%d" n size)
      (cond ((> (* n 100) size)
             (fracc-mode fracc-8bits-tex-encoding))))))

(add-hook 'tex-mode-hook 'install-french-accent-mode-if-needed)
(add-hook 'LaTeX-mode-hook 'install-french-accent-mode-if-needed)
(add-hook 'text-mode-hook 'install-french-accent-mode-if-needed)

;;; pmwiki

;; TODO: more features!!!
(defun mywiki-open (name)
  (interactive "sName (default Main.WikiSandbox): ")
  (if (request 'pmwiki-mode)
      (if (pmwiki-URIp name)
          (pmwiki-edit (pmwiki-loc 'link name) (pmwiki-loc 'base name))
        (pmwiki-edit (pmwiki-name-to-link
                      (pmwiki-default-string name "Main.WikiSandbox"))
                     (pmwiki-loc 'base mywiki-sandbox-uri)))
    (message "pmwiki-mode not found")))

;;; Lisp

(request 'color-eldoc)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'emacs-lisp-byte-compile)
(font-lock-add-keywords 'emacs-lisp-mode
                        `((,(concat "\\<" (regexp-opt '("add-hook" "add-mhook"
                                                        "autoload" "defmadvice"
                                                        "aset" "set" "fset"
                                                        "remove-hook" "clear-hook"
                                                        "request" "make-double-command") t)
                                    "\\>[ 	']*\\(\\sw+\\)?")
                           (1 font-lock-keyword-face)
                           (2 font-lock-constant-face nil t))
                          ))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (request 'guile-c)
              (define-key c-mode-map "\C-c\C-g\C-p" 'guile-c-insert-define)
              (define-key c-mode-map "\C-c\C-g\C-e" 'guile-c-edit-docstring)
              (define-key c-mode-map "\C-c\C-g\C-d" 'guile-c-deprecate-region)
              )))

(font-lock-add-keywords 'guile-scheme-mode
                        `((,(concat "\\<" (regexp-opt '("defun" "defvar" "defmacro" "defmacro*") t)
                                    "\\>[ 	']*\\(\\sw+\\)?")
                           (1 font-lock-keyword-face)
                           (2 font-lock-constant-face nil t))
                          ))

;;; Shell

(font-lock-add-keywords 'sh-mode '(
                                   ("\\<--\\w+\\>" (0 font-lock-keyword-face))
                                   ("[-{}()<>=;:+[.]\\|\\]" (0 font-lock-keys-face))
                                   ("\\\\$" (0 font-lock-warning-face))
                                   ))

;;; C/C++/PHP

;; see inside for more details...
(request 'mycode)

(when (request 'project)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (let ((rep (when (buffer-file-name) (file-name-directory (buffer-file-name)))))
                (when rep
                  (mapcond (lambda (s) (string-match (expand-file-name (yh/project-get s 'root)) rep))
                           (lambda (s) (c-set-style (or (yh/project-get s 'style) "personal")))
                           (yh/project-list)))))))

;;; Completion

(request 'mycompletion)

;;; Crontab
(autoload 'crontab-mode "crontab-mode" "" t nil)
(setq auto-mode-alist
      (cons '("crontab\\'" . crontab-mode) auto-mode-alist))

;;; FlashCard
(autoload 'flashcard-mode "flashcard" "" t nil)
(add-to-list 'auto-mode-alist '("\\.deck\\'" . flashcard-mode))


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

(defun yh/tabbar-inhibit-function ()
  (or (window-dedicated-p (selected-window))
      (member (buffer-name)
              '("*Group*" "*Calendar*"))
      (string-match "\\*Summary" (buffer-name))))

(defun simplify-blank-lines ()
  "Delete extra blank lines"
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
    (while (< i 254)      (setq i (+ i 1))
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
  (interactive)
  (cond ((and (boundp 'gnuserv-minor-mode) gnuserv-minor-mode) (gnuserv-edit))
	((or (not main-frame) (eq (selected-frame) main-frame)) (save-buffers-kill-emacs))
	((delete-frame))))

;; make active frame the main one
(defun make-main-frame ()
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


;;; Global key bindings

(global-set-key [(f3)] 'ecb-toggle-compile-window)
;; Depending on your keyboard you may want another one binding
(global-set-key (kbd "C-x ~") 'previous-error)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c c") 'mode-compile)
(global-set-key (kbd "C-c k") 'mode-compile-kill)
(global-set-key [\C-tab] 'other-window)

;; These were traditional bindings, why did they change??
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

;; "intelligent" home and end
(global-set-key [\C-home] 'my-home)
(global-set-key [\C-end] 'my-end)

;; mouse scrolling
(unless (featurep 'mwheel)
  (global-set-key [mouse-4] 'scroll-down)
  (global-set-key [mouse-5] 'scroll-up)
  (global-set-key [\C-mouse-4] 'scroll-down-one-line)
  (global-set-key [\C-mouse-5] 'scroll-up-one-line))

;; rectangle bindings. don't mix with registers! :)
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)
(global-set-key (kbd "C-x r C-y") 'yank-rectangle)

(global-set-key (kbd "C-x C-c") 'exit-no-gnuserv-or-frame)
(global-set-key (kbd "C-x 5 3") 'make-main-frame)
(global-set-key (kbd "C-x k") (lambda (arg) (interactive "P")
                                (if (null arg)
                                    (kill-this-buffer)
                                  (mapcar (lambda (buf) (kill-buffer buf)) (buffer-list)))))
(global-set-key (kbd "C-c m") (lambda () (interactive) (gnus 2)))
(global-set-key (kbd "C-c x") 'chmod-file)
(global-set-key (kbd "C-c i") 'init)
(global-set-key (kbd "C-c h") 'auto-insert)
(global-set-key (kbd "C-x C-f") 'find-file-guessing)

(define-key global-map (kbd "H-s") 'isearchb-activate)

(define-key-after (lookup-key global-map [menu-bar tools])
  [speedbar] '("Speedbar" . speedbar-frame-mode) [calendar])

(global-set-key [(f5)]
                (function
                 (lambda () (interactive)
                   (if (and (boundp 'ecb-minor-mode) ecb-minor-mode)
                       (ecb-deactivate)
                     (ecb-activate)))))

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

;; cvs keys
(global-set-key (kbd "<f12>") 'cvs-examine)
(global-set-key (kbd "<C-f12>") 'cvs-status)
(global-set-key (kbd "<M-f12>") 'cvs-update)


;;; Autoloads
(autoload 'boxquote-boxquote "boxquote" "Apply `boxquote-region' to the current boxquote." t nil)
(autoload 'boxquote-buffer "boxquote" "Apply `boxquote-region' to a whole buffer." t nil)
(autoload 'boxquote-defun "boxquote" "Apply `boxquote-region' the current defun." t nil)
(autoload 'boxquote-describe-function "boxquote" "Call `describe-function' and boxquote the output into the current buffer." t nil)
(autoload 'boxquote-describe-key "boxquote" "Call `describe-key' and boxquote the output into the current buffer." t nil)
(autoload 'boxquote-describe-variable "boxquote" "Call `describe-variable' and boxquote the output into the current buffer." t nil)
(autoload 'boxquote-fill-paragraph "boxquote" "Perform a `fill-paragraph' inside a boxquote." t nil)
(autoload 'boxquote-insert-file "boxquote" "Insert the contents of a file, boxed with `boxquote-region'.

If `boxquote-title-files' is non-nil the boxquote will be given a title that
is the result applying `boxquote-file-title-funciton' to FILENAME." t nil)
(autoload 'boxquote-kill "boxquote" "Kill the boxquote and its contents." t nil)
(autoload 'boxquote-kill-ring-save "boxquote" "Like `kill-ring-save' but remembers a title if possible.

The title is acquired by calling `boxquote-kill-ring-save-title'. The title
will be used by `boxquote-yank'." t nil)
(autoload 'boxquote-narrow-to-boxquote "boxquote" "Narrow the buffer to the current boxquote." t nil)
(autoload 'boxquote-narrow-to-boxquote-content "boxquote" "Narrow the buffer to the content of the current boxquote." t nil)
(autoload 'boxquote-paragraph "boxquote" "Apply `boxquote-region' to the current paragraph." t nil)
(autoload 'boxquote-region "boxquote" "Draw a box around the left hand side of a region bounding START and END." t nil)
(autoload 'boxquote-shell-command "boxquote" "Call `shell-command' with COMMAND and boxquote the output." t nil)
(autoload 'boxquote-text "boxquote" "Insert TEXT, boxquoted." t nil)
(autoload 'boxquote-title "boxquote" "Set the title of the current boxquote to TITLE.

If TITLE is an empty string the title is removed. Note that the title will
be formatted using `boxquote-title-format'." t nil)
(autoload 'boxquote-unbox "boxquote" "Remove the boxquote that contains `point'." t nil)
(autoload 'boxquote-unbox-region "boxquote" "Remove a box created with `boxquote-region'." t nil)
(autoload 'boxquote-yank "boxquote" "Do a `yank' and box it in with `boxquote-region'.

If the yanked entry was placed on the kill ring with
`boxquote-kill-ring-save' the resulting boxquote will be titled with
whatever `boxquote-kill-ring-save-title' returned at the time." t nil)
(autoload 'camelCase-mode "camelCase-mode" "Minor mode which overrides word command keys for editing camelCase words.

 Word boundaries in a camelCase name are marked only by letter case.
 For example lowerCapitalUPPERCase has four words.  A word may be
 lowercase, Capitalized, UPPERCASE, or a sequence of digits.  Besides
 non-letter to letter and letter to non-letter word boundaries,
 word boundaries in the middle of a sequence of letters are located at
 lowercaseCapital, CapitalCapital, lowercaseUPPERCASE,
 CapitalUPPERCASE, and UPPERCASECapital boundaries.

 Rebound keys:
   M-f, M-right*,  C-right      camelCase-forward-word
   M-b, M-left*,   C-left       camelCase-backward-word
   M-d, M-delete*, C-delete*    camelCase-forward-kill-word
   M-backspace,    C-backspace* camelCase-backward-kill-word
   M-t                          camelCase-transpose-words
   M-c                          camelCase-capitalize-word
   M-u                          camelCase-upcase-word
   M-l                          camelCase-downcase-word
 (* means only in Gnu Emacs, not in XEMacs; the original binding is not
  to the word command in XEmacs, so it is not overridden)

 camelCase-mode prefix ARG:  0 turns off, 1 turns on, nil toggles mode." t nil)
(autoload 'css-mode "css-mode" "Major mode for editing CSS style sheets.
\\{cssm-mode-map}" t nil)
(autoload 'doxymacs-mode "doxymacs" "mode help" t nil)
(autoload 'ecb-activate "ecb" "Emacs Code Browser" t nil)
(autoload 'expand-member-functions "member-functions" "Expand C++ member function declarations" t)
(autoload 'gnuserv-start "gnuserv-compat" "Allow this Emacs process to be a server for client processes." t)
(autoload 'guile-scheme-mode "guile-scheme" "" t nil)
(autoload 'h4x0r-string "h4x0r" "" t nil)
(autoload 'htmlize-buffer "htmlize" "Provide an html page from the current buffer" t nil)
(autoload 'htmlize-file "htmlize" "Provide an html page from the current file" t nil)
(autoload 'htmlize-many-files "htmlize" "Provide an html page from files" t nil)
(autoload 'htmlize-many-files-dired "htmlize" "Provide an html page from files marked in dired" t nil)
(autoload 'htmlize-region "htmlize" "Provide an html page from the current region" t nil)
(autoload 'isearchb-activate "isearchb" "Activate isearchb" t nil)
(autoload 'keytable "keytable" "Browse key bindings" t nil)
(autoload 'make-regexp "make-regexp" "Return a regexp to match a string item in STRINGS.")
(autoload 'make-regexps "make-regexp" "Return a regexp to REGEXPS.")
(autoload 'map-lines "map-lines" "Map COMMAND over lines matching REGEX." t)
(autoload 'mode-compile "mode-compile" "Command to compile current buffer file based on the major mode" t)
(autoload 'mode-compile-kill "mode-compile" "Command to kill a compilation launched by `mode-compile'" t)
(autoload 'page-break-mode "page-break" "Visible page markers" t nil)
(autoload 'replace-recent-character "rrc" "Replace-recent-character is interactive function for quick corrections of
recenlty typed text. It first prompts for character to search backwards. If
such character is found, following options are shown:
1, repeat the character to search in previous text.
2, M-r for delete of the found character.
3, C-t for trasposition of the found and the following character.
4, TAB for promt for character to insert after the found character.
5, ESC for no operation.
6, Any other insertable character will replace found character." t nil)
(autoload 'rm-exchange-point-and-mark "rect-mark" "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark" "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark" "Copy a rectangular region to the kill ring." t)
(autoload 'rm-set-mark "rect-mark" "Set mark for rectangle." t)
(autoload 'rpm "sb-rpm" "Rpm package listing in speedbar.")
(autoload 'sawfish-mode "sawfish" "sawfish-mode" t)
(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
(autoload 'teyjus "teyjus" "Run an inferior Teyjus process." t)
(autoload 'teyjus-edit-mode "teyjus" "Syntax Highlighting, etc. for Lambda Prolog" t)
(autoload 'turn-on-eldoc-mode "eldoc" "Activate eldoc" t nil)
(autoload 'w3-speedbar-buttons "sb-w3" "s3 specific speedbar button generator.")
(autoload 'zap-following-char "zap-char" "Kill following ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found." t nil)
(autoload 'zap-from-char "zap-char" "Kill from ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found." t nil)
(autoload 'zap-to-char "zap-char" "Kill up to and including ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found." t nil)
(autoload 'zap-upto-char "zap-char" "Kill up to ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found." t nil)

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
(autoload 'graphviz-dot-mode "graphviz-dot-mode" "" t nil)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

(when (request 'fold-dwim)
  (unless (boundp 'folding-mode)
    (setq folding-mode nil))
  (setq fold-dwim-outline-style-default 'nested)
  (global-set-key (kbd "<f7>")      'fold-dwim-toggle)
  (global-set-key (kbd "<M-f7>")    'fold-dwim-hide-all)
  (global-set-key (kbd "<S-M-f7>")  'fold-dwim-show-all))

(request 'emms)
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
;(setq emms-player-mpg321-parameters '("-o" "alsa"))


;; (setq sgml-warn-about-undefined-entities nil)

;;************************************************************
;; configure HTML editing
;;************************************************************

(request 'php-mode)

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

(message ".emacs loaded")

