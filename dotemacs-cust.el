;; -*- mode: emacs-lisp; auto-compile-lisp: nil; -*-

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (quote ("/usr/share/info")))
 '(LaTeX-command-style (quote (("omega" "lambda") ("." "latex"))))
 '(LaTeX-section-label nil)
 '(LaTeX-verbatim-regexp "\\(verbatim\\*?\\|sigmalog\\)")
 '(Man-notify-method (quote bully))
 '(TeX-command-list (quote (("TeX" "tex '\\nonstopmode\\input %t'" TeX-run-TeX nil t) ("TeX Interactive" "tex %t" TeX-run-interactive nil t) ("LaTeX" "%l '\\nonstopmode\\input{%t}'" TeX-run-LaTeX nil t) ("LaTeX Interactive" "%l %t" TeX-run-interactive nil t) ("Omega" "lambda '\\nonstopmode\\input{%t}'" TeX-run-LaTeX nil "nil") ("Omega Interactive" "lambda %t" TeX-run-interactive nil "nil") ("LaTeX2e" "latex2e '\\nonstopmode\\input{%t}'" TeX-run-LaTeX nil t) ("View" "%v " TeX-run-silent t nil) ("Print" "%p %r " TeX-run-command t nil) ("Queue" "%q" TeX-run-background nil nil) ("File" "dvips %d -o %f " TeX-run-command t nil) ("BibTeX" "bibtex %s" TeX-run-BibTeX nil nil) ("Index" "makeindex %s" TeX-run-command nil t) ("Check" "lacheck %s" TeX-run-compile nil t) ("Spell" "<ignored>" TeX-run-ispell-on-document nil nil) ("Other" "" TeX-run-command t t) ("LaTeX PDF" "pdflatex '\\nonstopmode\\input{%t}'" TeX-run-LaTeX nil t) ("Makeinfo" "makeinfo %t" TeX-run-compile nil t) ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil t) ("AmSTeX" "amstex '\\nonstopmode\\input %t'" TeX-run-TeX nil t) ("Generate Preview" "See `preview-LaTeX-command'" TeX-inline-preview nil t))))
 '(ange-ftp-ftp-program-args (quote ("-i" "-n" "-g" "-v" "-u")))
 '(auto-insert nil)
 '(auto-insert-mode nil nil (autoinsert))
 '(auto-insert-query (quote function))
 '(auto-revert-check-vc-info t)
 '(bbdb-complete-name-allow-cycling t)
 '(bbdb-dwim-net-address-allow-redundancy t)
 '(blink-cursor nil)
 '(browse-url-netscape-program "mozilla-firefox")
 '(c++-font-lock-extra-types (quote ("Q[a-zA-Z]*" "uint" "ulong" "string")))
 '(c-default-style (quote ((java-mode . "java") (other . "personal"))))
 '(calendar-week-start-day 1)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(compilation-read-command t)
 '(compilation-scroll-output t)
 '(compilation-window-height 10)
 '(compile-command "make")
 '(custom-file "~/.emacs-cust")
 '(default-justification (quote left))
 '(desktop-save-mode nil nil (desktop))
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(doxymacs-doxygen-style "Qt")
 '(doxymacs-doxygen-tags "DOXYTAGS")
 '(ecb-auto-activate nil)
 '(ecb-compilation-buffer-names (quote (("*Calculator*") ("*vc*") ("*vc-diff*") ("*Backtrace*") ("*shell*") ("*eshell*") ("*compilation*") ("*bsh*") ("*grep*") ("*Compile-Log*"))))
 '(ecb-compile-window-height 10)
 '(ecb-compile-window-temporally-enlarge nil)
 '(ecb-eshell-auto-activate t)
 '(ecb-eshell-enlarge-when-eshell nil)
 '(ecb-eshell-enlarge-when-selecting nil)
 '(ecb-eshell-fit-window-to-command-output nil)
 '(ecb-eshell-synchronize nil)
 '(ecb-layout-name "sigma")
 '(ecb-layout-window-sizes (quote (("sigma" (0.1761904761904762 . 0.8589743589743589) (0.19523809523809524 . 0.2564102564102564) (0.10476190476190476 . 0.3076923076923077) (0.09047619047619047 . 0.3076923076923077) (0.19523809523809524 . 0.2948717948717949)) ("left9" (0.1419753086419753 . 0.8225806451612904)))))
 '(ecb-major-modes-deactivate (quote (hide-all-except-activated . "\\(Info\\|custom\\)-mode")))
 '(ecb-options-version "2.25")
 '(ecb-other-window-behavior (quote only-edit))
 '(ecb-tip-of-the-day nil)
 '(enable-multibyte-characters t)
 '(eshell-ask-to-save-history (quote always))
 '(eshell-prefer-to-shell t nil (eshell))
 '(fill-column 80)
 '(flyspell-default-dictionary "francais")
 '(flyspell-issue-welcome-flag nil)
 '(glasses-face (quote bold))
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "")
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-hack-mode t nil (hl-line-hack))
 '(global-semantic-highlight-by-attribute-mode t nil (semantic-util-modes))
 '(global-semantic-highlight-edits-mode nil nil (semantic-util-modes))
 '(global-semantic-idle-scheduler-mode t nil (semantic-idle))
 '(global-semantic-show-parser-state-mode t nil (semantic-util-modes))
 '(global-semantic-show-tag-boundaries-mode t nil (semantic-util-modes))
 '(global-semantic-show-unmatched-syntax-mode nil nil (semantic-util-modes))
 '(global-semantic-stickyfunc-mode nil nil (semantic-util-modes))
 '(global-senator-minor-mode t nil (senator))
 '(global-visible-mark-mode-exclude-alist (quote ("^\\*")))
 '(gud-chdir-before-run nil)
 '(hl-line-hack-exceptions (quote ("*eshell*")))
 '(hl-line-hack-face (quote highlight))
 '(htmlize-head-tags "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">")
 '(htmlize-html-major-mode (quote html-mode))
 '(indent-tabs-mode nil)
 '(inhibit-startup-message t)
 '(kill-whole-line t)
 '(line-number-mode t)
 '(mark-diary-entries-in-calendar t)
 '(max-lisp-eval-depth 500)
 '(max-specpdl-size 1000)
 '(mouse-avoidance-threshold 15)
 '(mouse-yank-at-point t)
 '(next-line-add-newlines nil)
 '(osd-args (quote ("--delay=3" "--age=3" "--pos=bottom" "--offset=70" "--outline=5" "--outlinecolour=grey" "--font=-microsoft-verdana-medium-r-normal--10-*-*-*-*-*-*")))
 '(parens-require-spaces nil)
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t)
 '(pcomplete-autolist t)
 '(pcomplete-cycle-completions nil)
 '(prolog-system (quote swi))
 '(recentf-mode t nil (recentf))
 '(reftex-plug-into-AUCTeX t)
 '(save-place-limit 100)
 '(sawfish-extra-keyword-list (quote ("add-frame-style" "call-after-load" "call-after-property-changed" "call-after-state-changed" "custom-set-property" "define" "define-structure" "export" "open")))
 '(sawfish-warning-keyword-list (quote ("fixme" "FIXME" "Fixme" "fix me" "Fix me" "!!!" "Grrr" "Bummer" "todo" "TODO" "Todo")))
 '(scroll-bar-mode nil)
 '(semanticdb-default-file-name ".semantic.cache")
 '(semanticdb-default-save-directory "~/.semantic")
 '(semanticdb-global-mode t nil (semanticdb))
 '(show-paren-mode t nil (paren))
 '(speedbar-frame-parameters (quote ((width . 20) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t))))
 '(speedbar-use-images nil)
 '(template-auto-insert nil)
 '(tempo-insert-region nil)
 '(tempo-interactive t)
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-gud-tips-p t)
 '(truncate-partial-width-windows nil)
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(view-diary-entries-initially t)
 '(visible-bell t)
 '(which-function-mode nil nil (which-func)))

;;;;;;;;;;;;
;; Visuals
;;

;; New faces
(defvar font-lock-number-face 'font-lock-number-face)
(defvar font-lock-hexnumber-face 'font-lock-hexnumber-face)
(defvar font-lock-floatnumber-face 'font-lock-floatnumber-face)
(defvar font-lock-keys-face 'font-lock-keys-face)
(defvar font-lock-qt-face 'font-lock-qt-face)

(defface font-lock-number-face
  '(
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:background "hotpink"))
    (((class color) (background dark)) (:foreground "black" :background "hotpink"))
    (t (:italic t)))
  "blah"
  :group 'font-lock-highlighting-faces)

(defface font-lock-hexnumber-face
  '(
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:background "darkblue"))
    (((class color) (background dark)) (:foreground "black" :background "darkblue"))
    (t (:italic t)))
  "blah"
  :group 'font-lock-highlighting-faces)

(defface font-lock-floatnumber-face
  '(
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:background "darkgreen"))
    (((class color) (background dark)) (:foreground "black" :background "darkgreen"))
    (t (:italic t)))
  "blah"
  :group 'font-lock-highlighting-faces)

(defface font-lock-keys-face
  '(
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:background "yellow"))
    (((class color) (background dark)) (:foreground "black" :background "yellow"))
    (t (:italic t)))
  "blah"
  :group 'font-lock-highlighting-faces)

(defface font-lock-qt-face
  '(
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:background "brown"))
    (((class color) (background dark)) (:foreground "green" :background "brown"))
    (t (:italic t)))
  "blah"
  :group 'font-lock-highlighting-faces)

(defface minibuffer-face '((t (:bold t :foreground "LightBlue")))
  "Face used to color the minibuffer.")

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "yellow"))) t)
 '(dircolors-face-objet ((t (:foreground "Gray"))) t)
 '(ecb-default-highlight-face ((((class color) (background dark)) (:inherit ecb-default-general-face :background "slateblue"))))
 '(ecb-directory-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face))))
 '(ecb-history-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face))))
 '(ecb-method-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face))))
 '(ecb-source-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face))))
 '(ecb-token-header-face ((((class color) (background dark)) (:background "SeaGreen4"))))
 '(font-lock-floatnumber-face ((((class color) (background dark)) (:foreground "yellow4"))))
 '(font-lock-hexnumber-face ((((class color) (background dark)) (:foreground "cyan"))))
 '(font-lock-keys-face ((((class color) (background dark)) (:foreground "yellow"))))
 '(font-lock-number-face ((t (:foreground "yellow3"))))
 '(font-lock-pvs-function-type-face ((t (:foreground "blue"))))
 '(font-lock-pvs-parens-face ((t (:foreground "yellow"))))
 '(font-lock-pvs-record-parens-face ((t (:foreground "red"))))
 '(font-lock-pvs-set-brace-face ((t (:foreground "darkred"))))
 '(font-lock-pvs-table-face ((t (:foreground "black"))))
 '(font-lock-qt-face ((((class color) (background dark)) (:foreground "green4"))))
 '(gnus-group-mail-low-empty-face ((((class color) (background dark)) (:foreground "aquamarine3"))))
 '(gnus-group-mail-low-face ((t (:foreground "aquamarine3" :weight bold))))
 '(gnus-header-content-face ((t (:foreground "green" :slant italic))))
 '(gnus-header-name-face ((((class color) (background dark)) (:foreground "LightGreen"))))
 '(highlight ((t (:background "slateblue"))))
 '(menu ((((type x-toolkit)) (:background "lightgrey" :foreground "black" :box (:line-width 1 :style released-button)))))
 '(message-header-name-face ((((class color) (background dark)) (:foreground "green3"))))
 '(message-header-other-face ((((class color) (background dark)) (:foreground "#dd0000"))))
 '(mode-line ((t (:background "lightgrey" :foreground "black" :box (:line-width 1 :style released-button)))))
 '(region ((t (:background "slategrey"))))
 '(show-paren-match-face ((t (:background "black" :foreground "red" :weight bold))))
 '(show-paren-mismatch-face ((t (:background "magenta" :foreground "white" :weight bold))))
 '(underline ((t (:foreground "seagreen3" :underline t))))
 '(w3-style-face-00001 ((t (:underline nil :weight normal :height 150))))
 '(w3-style-face-00002 ((t (:underline nil))))
 '(w3-style-face-00003 ((t (:underline nil :weight normal :height 150))))
 '(w3-style-face-00004 ((t (:underline nil :weight normal :height 150))))
 '(w3-style-face-00005 ((t (:underline nil :height 150))))
 '(w3-style-face-00007 ((t (:underline nil :height 150))))
 '(w3-style-face-00008 ((t (:underline nil :weight normal :height 150)))))

(setq font-lock-support-mode 'lazy-lock-mode)
(setq lazy-lock-stealth-time    1
      lazy-lock-stealth-verbose nil); je veux pas voir le status du fontify

(set-face-foreground 'bold        "lightcoral")    ;avant red
(set-face-foreground 'italic      "Orchid")        ;lightslateblue
(set-face-foreground 'underline   "seagreen3")
(set-face-foreground 'bold-italic "orange")

(set-face-underline-p 'underline nil)
(set-face-background  'region    "slategrey")
(set-face-background  'highlight "slateblue") ;nil pour ispell

(set-face-background 'show-paren-match-face "slateblue")
(set-face-background 'show-paren-mismatch-face "magenta")

(set-face-foreground font-lock-comment-face "gray")       ;red    Firebrick
(set-face-foreground font-lock-string-face  "green3")     ;green4

(set-face-foreground font-lock-function-name-face "lightblue2") ;blue3

(set-face-foreground font-lock-variable-name-face "lightblue3") ;blue3

(set-face-foreground 'font-lock-builtin-face "coral")
(set-face-foreground font-lock-type-face     "palegreen") ;orange

(defun Set-face-foreground (f c) (make-face f) (set-face-foreground f c))
(defun Set-face-background (f c) (make-face f) (set-face-background f c))

(Set-face-foreground 'font-lock-number-face      "yellow3") ;"purple"
(Set-face-foreground 'font-lock-punctuation-face "cyan");   "green4"

(request 'dircolors)

;; Frame appearence
(set-default-font "fixed")
(if (eq window-system 'x)
    (setq default-frame-alist
          (append default-frame-alist
                  '(
                    (tool-bar-lines . 0)
                    (menu-bar-lines . 0)
                    (width . 100)
                    (height . 50)
                    (foreground-color . "wheat")
                    (background-color . "darkslategray")
;;                    (cursor-color . "orchid")
                    (font . "fixed")
                    )))
)

(if (> emacs-major-version 20) (custom-set-variables '(tool-bar-mode nil nil (tool-bar))))
(if (> emacs-major-version 20) (set-scroll-bar-mode nil))
(menu-bar-mode -1)
