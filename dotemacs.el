;; -*- mode: emacs-lisp; auto-compile-lisp: nil; -*-
;; Time-stamp: <15/06/2004 18:24:47 Yann Hodique>

;; Use this one instead of require to ignore errors
(defun request (pack)
  "Fail to require silently"
  (condition-case nil
    (require pack)
  (error nil)))

;; Load site-specific stuff
(if (file-exists-p (expand-file-name "~/.emacs-local"))
    (load-file (expand-file-name "~/.emacs-local")))

;; My customizations are in a separate file
(if (file-exists-p (expand-file-name "~/.emacs-cust"))
    (load-file (expand-file-name "~/.emacs-cust")))

;; Fix various "bad" default behaviors
(require 'patches)

;; Hacked scroll margin
(set-scroll-margin 5 5 '("*eshell*" "*compile*"))
(setq scroll-step 1
      scroll-conservatively 15)

;; Save minibuffer history between sessions
(request 'save-history)

;; Date in mode line
(display-time)

;; Set window title according to buffer name
(setq frame-title-format '("emacs: %f"))

;; Turn on time stamping
(setq time-stamp-active t)
;; Sets new format for the time stamp, also used with the creation tag.
(setq time-stamp-format "%02d/%02m/%:y %02H:%02M:%02S %U")

;; We don't want to insert newlines when reaching end of buffer
;; and we want to kill the whole line when doing a Ctrl-k
(setq next-line-add-newlines nil
      kill-whole-line t)

;; Auto-reload file when modified from external app
(global-auto-revert-mode 1)

;; Enable dynamic highlighting
(hi-lock-mode 1)

;; Throw out the mouse when typing
(mouse-avoidance-mode 'exile)

;; Load the emacs type verifier first (gnu emacs, xemacs, ...)
(request 'emacs-type)

;;;;;;;;;;;;;
;; Charsets

(set-language-environment 'latin-1)
(prefer-coding-system 'latin-1)

(setq unibyte-display-via-language-environment t)
(setq-default ctl-arrow 'latin-9)

(require 'ucs-tables)
(unify-8859-on-encoding-mode 1)
(unify-8859-on-decoding-mode 1)

(defun sk-insert-euro (&optional arg) "Insert Euro ISO 8859-15."
  (interactive "*P")
  (if arg
      (insert (make-char 'mule-unicode-0100-24ff 116 76))
    (insert (make-char 'latin-iso8859-15 164))))

(global-set-key (kbd "H-5") 'sk-insert-euro)

(defun sk-insert-oe (&optional arg) "Insert oe"
  (interactive "*P")
;  (if arg
;     (insert (make-char 'mule-unicode-0100-24ff 116 76))
    (insert (make-char 'latin-iso8859-15 #xBD)))

(global-set-key (kbd "H-o H-e") 'sk-insert-oe)

;;;;;;;;;;;;;;
;; Variables
;;

;; Load default classes
(if (file-exists-p (expand-file-name "~/.emacs.d/emacs-d-vars.el"))
    (load-file (expand-file-name "~/.emacs.d/emacs-d-vars.el")))

;; open compressed files
(autoload 'jka-compr-installed-p "jka-compr")
(if (not (jka-compr-installed-p)) (auto-compression-mode))


;;;;;;;;;;;;;
;; Packages
;;
  ;;;;;;;;;;;;;
  ;; Calendar
  ;;

(request 'calendar)
(setq european-calendar-style t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Tramp : transparent remote editing
  ;;

(request 'tramp2)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Mtp : client for telnet-based chat server
  ;;

(require 'lispy)

(require 'lispy-commands)
(require 'lispy-history)
(require 'lispy-font-lock)
(require 'lispy-occur)
(require 'lispy-session)
(require 'lispy-osd)

  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Eshell : Emacs shell
  ;;

(add-hook 'eshell-post-command-hook 'eshell-show-maximum-output)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Speed bar : usefull for displaying various informations
  ;;

(request 'speedbar)
;; Texinfo fancy chapter tags
(add-hook 'texinfo-mode-hook (lambda () (request 'sb-texinfo)))

;; HTML fancy chapter tags
(add-hook 'html-mode-hook (lambda () (request 'sb-html)))

;; For any verison of emacs on a linux RPM based system:
(autoload 'rpm "sb-rpm" "Rpm package listing in speedbar.")

;; w3 link listings
(autoload 'w3-speedbar-buttons "sb-w3" "s3 specific speedbar button generator.")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; MTorus : multiple buffer rings
  ;;

(if (request 'mtorus)
    (progn
      (mtorus-init)
      (global-set-key '[(shift right)] 'mtorus-next-marker)
      (global-set-key '[(shift left)]  'mtorus-prev-marker)
      (global-set-key '[(shift up)]    'mtorus-next-ring)
      (global-set-key '[(shift down)]  'mtorus-prev-ring)
      ;; ring handling: f11
      (global-set-key '[(f11)]
                      'mtorus-new-ring)
      (global-set-key '[(shift f11)]
                      'mtorus-delete-ring)
      (global-set-key '[(control f11)]
                      'mtorus-notify)
      ;; marker handling: f12
      (global-set-key '[(f12)]
                      'mtorus-new-marker)
      (global-set-key '[(shift f12)]
                      'mtorus-delete-current-marker)
      (global-set-key '[(control f12)]
                      'mtorus-update-current-marker)
      ))

  ;;;;;;;;;;
  ;; CEDET
  ;;
(require 'eldoc)
(load-file "~/.emacs.d/cedet/common/cedet.el")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Ecb : Emacs Code Browser
  ;;

(if (request 'ecb)
    (progn
      (add-hook 'ecb-activate-hook
                (lambda ()
                  (ecb-toggle-compile-window -1)))))

  ;;;;;;;;;;;;;;;
  ;; Completion
  ;;

;(request 'completion)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Doxymacs : great documentation system
  ;;

(if (request 'doxymacs)
    (progn
      (setq doxymacs-relative-path "Doc/html"
            doxymacs-use-external-xml-parser t)
      (add-hook 'c-mode-common-hook 'doxymacs-mode)
      (defun my-doxymacs-font-lock-hook ()
        (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
            (doxymacs-font-lock)))
      (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

      ;; hack of doxymacs : determines where documentation is from location of current_buffer
      (defun mydox ()
        (interactive)
        (custom-set-variables '(doxymacs-doxygen-root
                                (concat "file://"
                                        (if (string= (substring default-directory 0 2) home-directory)
                                            (concat home-directory (substring default-directory 1))
                                          default-directory)
                                        doxymacs-relative-path)))
        (doxymacs-rescan-tags))
      (define-key doxymacs-mode-map "\C-cdr" 'mydox)
      ))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Sawfish : the ultimate window manager
  ;;

;; Open sawfish-realted files with the proper mode
(setq auto-mode-alist (cons '("\\.sawfishrc$"  . sawfish-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.jl$"         . sawfish-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.sawfish/rc$" . sawfish-mode) auto-mode-alist))

  ;;;;;;;;;;
  ;; Latex
  ;;

(request 'typopunct)
(typopunct-change-language 'francais t)
(eval-after-load "latex"
  '(add-to-list 'LaTeX-style-list '("prosper")))

(if
    (request 'tex-site)
    (progn
      (setq-default TeX-master t)
      ;; reftex helps managing references, toc, ...
      (add-hook 'LaTeX-mode-hook 'reftex-mode)
      ;; show/hide parts of your document
      (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
      ;; preview-latex is great
      (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
      ;; point my typos
      (add-hook 'LaTeX-mode-hook 'flyspell-mode)
      ;; produce justified paragraphs
      (add-hook 'LaTeX-mode-hook (lambda () (setq default-justification 'full)))
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
                  (2 "\""))))
              ))

      (add-hook 'LaTeX-mode-hook 'my-LaTeX-hook)
      ))

  ;;;;;;;;;;;;;;;;;;;;;;
  ;; Highlight-changes
  ;;
(add-hook 'highlight-changes-enable-hook (lambda ()
                                           (local-set-key "\C-c+" 'highlight-changes-next-change)
                                           (local-set-key "\C-c-" 'highlight-changes-previous-change)
                                           (local-set-key (kbd "C-c DEL") (lambda () (interactive) (highlight-changes-remove-highlight (point-min) (point-max))))
                                           (local-set-key "\C-c_" 'highlight-changes-rotate-faces)))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Fracc : french accent mode
  ;;

(if (request 'fracc)
    (progn
      (defun install-french-accent-mode-if-needed ()
        "Install French accent mode if the buffer seems to contain French text.
The guess is made by computing the proportion of letters with accents. If
there are more than 1% of such letters then turn French accent mode on."
        (save-excursion
          (goto-char (point-min))
          (let ((n 0)(size (- (point-max) (point-min))))
            (while (re-search-forward "\\\\['^`][eauo]" (point-max) t)
              (setq n (+ n 1)) )
            (while (re-search-forward "[�������]" (point-max) t)
              (setq n (+ n 1)) )
            (message "diacritic/normal ratio = %d/%d" n size)
            (cond ((> (* n 100) size)
             (fracc-mode fracc-8bits-tex-encoding) ) ) ) ) )
;;; and install it
      (add-hook 'tex-mode-hook 'install-french-accent-mode-if-needed)
      (add-hook 'LaTeX-mode-hook 'install-french-accent-mode-if-needed)
      (add-hook 'text-mode-hook 'install-french-accent-mode-if-needed)
      ))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Boxquotes : nice boxes for quotations
  ;;

(request 'boxquote)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Keytable : browse keybindings
  ;;

(request 'keytable)

  ;;;;;;;;;;;;;;;;;;
  ;; Various hacks
  ;;

;; see inside for more details...
(request 'mycode)

(defun yh-c-rearrange-electrics ()
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

(defadvice delete-trailing-whitespace( after open-line-after act )
  "Also delete extra blank lines"
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (string ?^ 10 10) nil t)
      (backward-char)
      (delete-blank-lines))))

(add-hook 'c-mode-common-hook (lambda ()
                                (let ((rep (file-name-directory (buffer-file-name))))
                                  (cond ((string-match (expand-file-name "~/Projects/bassist") rep)
                                         (c-set-style "bassist"))
                                        ((string-match (expand-file-name "~/cvs/Camille") rep)
                                         (c-set-style "camille"))
                                        (t
                                         (c-set-style "personal")))
                                  )) t)

(request 'mycompletion)
(request 'page-break)


;;;;;;;;;;;;;;;;;;;;
;; Utils/Functions
;;

;; byte-compile current elisp buffer
(defun byte-compile-elisp ()
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

;; apply chmod over the current file (usually a+x for scripts)
(defun chmod-file ()
  (interactive)
  (require 'dired)
  (call-process dired-chmod-program nil nil nil (read-from-minibuffer "Mode: " "a+x") (buffer-file-name))
  )

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
  (insert (format "ASCII characters up to number %d.\n" 254))  (let ((i 0))
    (while (< i 254)      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))  (beginning-of-buffer))

;; explicit name :-)
(defun gpl-header ()
  "Includes a header in the edited file."
  (let ((name (buffer-name)))
    (with-temp-buffer
      (insert (format "/*\n%-75s\n" (concat " *  File: " name)))
      (insert (format "%-75s\n" (concat " *  Created: " (calendar-date-string (calendar-current-date)))))
      (insert (format "%-75s\n" " *  Time-stamp: <>"))
      (insert (format "%-75s\n" (concat " *  Copyright: " user-full-name)))
      (insert (format "%-75s\n */\n" (concat " *  Email: " user-mail-address)))
      (insert (format "
/************************************************************************
 *                                                                      *
 * This program is free software; you can redistribute it and/or modify *
 * it under the terms of the GNU General Public License as published by *
 * the Free Software Foundation; either version 2 of the License, or    *
 * (at your option) any later version.                                  *
 *                                                                      *
 ************************************************************************/

"))
      (buffer-string))))

(defun yank-and-forward-line ()
  (interactive)
  (let ((old-col (current-column)))
    (yank)
    (forward-line)
    (while (and (not (eolp)) (> old-col 0))
      (forward-char)
      (setq old-col (1- old-col)))))

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

;; initialize "code" workspace : speedbar with set frame name
(defun code-init (&optional STRING)
  (interactive)
  (let ((f (selected-frame))
	(params speedbar-frame-parameters))
    (when (stringp STRING) (setq speedbar-frame-parameters (cons (cons 'name STRING) speedbar-frame-parameters)))
    (speedbar 1)
    (setq speedbar-frame-parameters params)
    (select-frame speedbar-frame t)
    (select-frame f)
    )
  )

;; My init function for main window
(defun init ()
  (interactive)
  (make-main-frame)
;  (gnuserv-start)
                                        ;  (code-init "Code")
  (ecb-activate)
                                        ;  (select-frame main-frame)
  )


;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
;;

(global-set-key [(f3)] 'ecb-toggle-compile-window)
;; Depending on your keyboard you may want another one binding
(global-set-key (kbd "C-x ~") 'previous-error)
(global-set-key (kbd "H-c s") 'eshell)
(global-set-key (kbd "H-c c") 'mode-compile)
(global-set-key (kbd "H-c k") 'mode-compile-kill)
(global-set-key [\C-tab] 'other-window)

;; These were traditional bindings, why did they change??
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
(global-set-key [\C-home] 'beginning-of-line)
(global-set-key [\C-end] 'end-of-line)

;; mouse scrolling
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)
(global-set-key [\C-mouse-4] 'scroll-down-one-line)
(global-set-key [\C-mouse-5] 'scroll-up-one-line)

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
                                  (kill-some-buffers))))
(global-set-key [(f4)] 'speedbar-get-focus)
(global-set-key (kbd "H-c m") (lambda () (interactive) (gnus 4)))
(global-set-key (kbd "H-c x") 'chmod-file)
(global-set-key (kbd "H-c i") 'init)
(global-set-key (kbd "H-c h") 'auto-insert)
(global-set-key (kbd "H-x H-f") (lambda () (interactive) (require 'ffap) (find-file (ffap-guesser))))

(define-key-after (lookup-key global-map [menu-bar tools])
      [speedbar] '("Speedbar" . speedbar-frame-mode) [calendar])

(global-set-key [(f5)]
  (function
   (lambda () (interactive)
     (if (and (boundp 'ecb-minor-mode) ecb-minor-mode)
	 (ecb-deactivate)
       (ecb-activate)))))

;;;;;;;;;;;;;;
;; Autoloads
;;
(autoload 'expand-member-functions "member-functions" "Expand C++ member function declarations" t)
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
;; Rectangles operations
(autoload 'rm-set-mark "rect-mark"
  "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark"
  "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark"
      "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark"
  "Copy a rectangular region to the kill ring." t)
(autoload 'gnuserv-start "gnuserv-compat"
  "Allow this Emacs process to be a server for client processes."
  t)
(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
(autoload 'trivial-cite "tc" t t)
(autoload 'teyjus "teyjus" "Run an inferior Teyjus process." t)
(autoload 'teyjus-edit-mode "teyjus" "Syntax Highlighting, etc. for Lambda Prolog" t)
(autoload 'sawfish-mode "sawfish" "sawfish-mode" t)
(autoload 'map-lines "map-lines"
  "Map COMMAND over lines matching REGEX."
  t)
(autoload 'htmlize-buffer "htmlize" "" t nil)
(autoload 'htmlize-region "htmlize" "" t nil)
(autoload 'htmlize-file "htmlize" "" t nil)
(autoload 'htmlize-many-files "htmlize" "" t nil)
(autoload 'make-regexp "make-regexp"
  "Return a regexp to match a string item in STRINGS.")
(autoload 'make-regexps "make-regexp"
  "Return a regexp to REGEXPS.")
(autoload 'camelCase-mode "camelCase-mode" nil t)

(require 'planner-config)
(require 'emacs-wiki-config)

(when (string= "xterm" (getenv "TERM"))
  (require 'xterm-extras)
  (xterm-extra-keys))

(require 'multi-region)
(define-key global-map (kbd "C-M-m") multi-region-map)

(require 'isearchb)
(define-key global-map [(hyper ?s)] 'isearchb-activate)

(require 'zap-char)
(global-set-key [(hyper z)]               'zap-upto-char)
(global-set-key [(hyper meta z)]          'zap-to-char)
(global-set-key [(shift hyper z)]         'zap-following-char)
(global-set-key [(shift hyper meta z)]    'zap-from-char)

(require 'elscreen)
;(require 'url)
;(require 'w3)

(request 'w3m-load)

(require 'longlines)
(require 'cparen)
(cparen-activate)

(require 'color-moccur)
(require 'moccur-edit)

(add-to-list 'ispell-dictionary-alist '("latin" "[A-Za-z]" "[^A-Za-z]" "[']" nil nil "~tex" iso-8859-1))

(message ".emacs loaded")
