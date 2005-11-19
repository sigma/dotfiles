;; -*- mode: emacs-lisp; auto-compile-lisp: nil; -*-

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(1on1-*Completions*-frame-flag nil)
 '(1on1-*Help*-frame-flag nil)
 '(1on1-minibuffer-frame-alist (quote ((foreground-color . "Red") (background-color . "LightBlue") (font . "fixed") (mouse-color . "Black") (cursor-color . "Black") (menu-bar-lines) (height . 2) (icon-type) (minibuffer . only) (user-position . t) (vertical-scroll-bars) (name . "Emacs Minibuffer"))))
 '(Man-notify-method (quote bully))
 '(add-log-keep-changes-together t)
 '(ange-ftp-ftp-program-args (quote ("-i" "-n" "-g" "-v" "-u")))
 '(apropos-do-all t)
 '(apropos-sort-by-scores t)
 '(auto-insert nil)
 '(auto-insert-mode nil)
 '(auto-insert-query (quote function))
 '(auto-revert-check-vc-info t)
 '(bbdb-complete-name-allow-cycling t)
 '(bbdb-dwim-net-address-allow-redundancy t)
 '(blink-cursor-mode nil nil (frame))
 '(browse-url-browser-function (quote w3m-browse-url))
 '(browse-url-netscape-program "mozilla-firefox")
 '(c++-font-lock-extra-types (quote ("Q[a-zA-Z]*" "uint" "ulong" "string")))
 '(c-default-style (quote ((java-mode . "java") (other . "personal"))))
 '(calendar-week-start-day 1)
 '(canlock-password "86b712369f839f776688a36513969db03cf50eb2")
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(compilation-read-command t)
 '(compilation-scroll-output t)
 '(compilation-window-height 10)
 '(compile-command "make")
 '(custom-file "~/.emacs-cust")
 '(default-justification (quote left))
 '(desktop-save (quote if-exists))
 '(desktop-save-mode nil nil (desktop))
 '(dired-omit-files "^\\.?#\\|^\\.$")
 '(display-battery-mode nil)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(doxymacs-doxygen-style "JavaDoc")
 '(doxymacs-doxygen-tags "DOXYTAGS")
 '(ecb-auto-activate nil)
 '(ecb-compilation-buffer-names (quote (("*Calculator*") ("*vc*") ("*vc-diff*") ("*Backtrace*") ("*shell*") ("*eshell*") ("*compilation*") ("*bsh*") ("*grep*") ("*Compile-Log*"))))
 '(ecb-compile-window-height 10)
 '(ecb-compile-window-temporally-enlarge nil)
 '(ecb-directories-menu-user-extension-function (quote ignore))
 '(ecb-enlarged-compilation-window-max-height 10)
 '(ecb-eshell-auto-activate t)
 '(ecb-eshell-enlarge-when-eshell nil)
 '(ecb-eshell-enlarge-when-selecting nil)
 '(ecb-eshell-fit-window-to-command-output nil)
 '(ecb-eshell-synchronize nil)
 '(ecb-history-menu-user-extension-function (quote ignore))
 '(ecb-layout-name "sigma")
 '(ecb-layout-window-sizes (quote (("sigma" (0.1761904761904762 . 0.8589743589743589) (0.19523809523809524 . 0.2564102564102564) (0.10476190476190476 . 0.3076923076923077) (0.09047619047619047 . 0.3076923076923077) (0.19523809523809524 . 0.2948717948717949)) ("left9" (0.1419753086419753 . 0.8225806451612904)))))
 '(ecb-major-modes-deactivate (quote (hide-all-except-activated . "\\(Info\\|custom\\)-mode")))
 '(ecb-methods-menu-user-extension-function (quote ignore))
 '(ecb-options-version "2.32")
 '(ecb-other-window-behavior (quote only-edit))
 '(ecb-sources-menu-user-extension-function (quote ignore))
 '(ecb-tip-of-the-day nil)
 '(ecb-vc-supported-backends (quote ((ecb-vc-dir-managed-by-CVS . vc-state) (ecb-vc-dir-managed-by-RCS . vc-state) (ecb-vc-dir-managed-by-SCCS . vc-state) (ecb-vc-dir-managed-by-SVN . vc-state))))
 '(enable-multibyte-characters t)
 '(erc-bbdb-auto-create-on-whois-p t)
 '(erc-echo-timestamps t)
 '(erc-input-line-position -2)
 '(erc-modules (quote (autojoin bbdb fill irccontrols netsplit noncommands pcomplete completion ring scrolltobottom services stamp track)))
 '(erc-track-exclude-types (quote ("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE")))
 '(erc-track-shorten-start 2)
 '(erc-track-showcount t)
 '(eshell-ask-to-save-history (quote always))
 '(eshell-cmpl-cycle-completions nil)
 '(eshell-prefer-to-shell t nil (eshell))
 '(eshell-save-history-on-exit t)
 '(fill-column 79)
 '(fill-nobreak-predicate (quote (fill-french-nobreak-p fill-single-word-nobreak-p)))
 '(flyspell-default-dictionary "francais")
 '(flyspell-issue-welcome-flag nil)
 '(gc-cons-threshold 4000000)
 '(glasses-face (quote bold))
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "")
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-hack-mode t)
 '(global-semantic-decoration-mode t nil (semantic-decorate-mode))
 '(global-semantic-highlight-by-attribute-mode t nil (semantic-util-modes))
 '(global-semantic-highlight-edits-mode nil nil (semantic-util-modes))
 '(global-semantic-idle-scheduler-mode t nil (semantic-idle))
 '(global-semantic-show-parser-state-mode t nil (semantic-util-modes))
 '(global-semantic-show-tag-boundaries-mode t nil (semantic-util-modes))
 '(global-semantic-show-unmatched-syntax-mode nil nil (semantic-util-modes))
 '(global-semantic-stickyfunc-mode nil nil (semantic-util-modes))
 '(global-senator-minor-mode t nil (senator))
 '(global-visible-mark-mode-exclude-alist (quote ("^\\*")))
 '(gnus-cacheable-groups ".*")
 '(gnus-uncacheable-groups nil)
 '(graphviz-dot-preview-extension "ps")
 '(gud-chdir-before-run nil)
 '(hl-line-hack-exceptions (quote ("*eshell*" "*Calendar*")))
 '(hl-line-hack-face (quote highlight))
 '(htmlize-head-tags "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">")
 '(htmlize-html-major-mode (quote html-mode))
 '(ido-confirm-unique-completion t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-message t)
 '(log-edit-common-indent -7)
 '(mark-diary-entries-in-calendar t)
 '(max-lisp-eval-depth 500)
 '(max-specpdl-size 1000)
 '(minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(mouse-1-click-follows-link nil)
 '(mouse-1-click-in-non-selected-windows nil)
 '(mouse-avoidance-threshold 15)
 '(mouse-drag-copy-region nil)
 '(mouse-yank-at-point t)
 '(next-line-add-newlines nil)
 '(osd-args (quote ("--delay=3" "--age=3" "--pos=bottom" "--offset=70" "--outline=5" "--outlinecolour=grey" "--font=-microsoft-verdana-medium-r-normal--10-*-*-*-*-*-*")))
 '(paren-dont-load-timer nil)
 '(parens-require-spaces nil)
 '(partial-completion-mode t)
 '(pcomplete-autolist t)
 '(pcomplete-cycle-completions nil)
 '(planner-default-page "TaskPool")
 '(planner-backend (quote muse))
 '(planner-multi-copy-tasks-to-page "TaskPool")
 '(planner-tasks-file-behavior (quote (quote close)))
 '(planner-use-other-window nil)
 '(planner-xtla-log-edit-include-files-flag nil)
 '(planner-xtla-log-edit-notice-commit-function t)
 '(preview-default-option-list (quote ("displaymath" "floats" "graphics" "textmath" "footnotes")))
 '(prolog-system (quote swi))
 '(proof-assistants (quote (isar coq acl2)))
 '(read-quoted-char-radix 16)
 '(recentf-exclude (quote (":\\|#")))
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs-recentf")
 '(reftex-revisit-to-follow t)
 '(save-abbrevs (quote silently))
 '(save-place-limit 100)
 '(sawfish-extra-keyword-list (quote ("add-frame-style" "call-after-load" "call-after-property-changed" "call-after-state-changed" "custom-set-property" "define" "define-structure" "export" "open")))
 '(sawfish-warning-keyword-list (quote ("fixme" "FIXME" "Fixme" "fix me" "Fix me" "!!!" "Grrr" "Bummer" "todo" "TODO" "Todo")))
 '(scroll-bar-mode nil)
 '(semanticdb-global-mode t nil (semanticdb))
 '(sh-shell-file "/bin/bash")
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(slime-header-line-p nil)
 '(slime-kill-without-query-p t)
 '(speedbar-frame-parameters (quote ((width . 20) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t))))
 '(speedbar-use-images nil)
 '(tabbar-inhibit-functions (quote (tabbar-default-inhibit-function yh/tabbar-inhibit-function)))
 '(tc-make-attribution (quote kai-tc-simple-attribution))
 '(tc-mouse-overlays t)
 '(temp-buffer-resize-mode t)
 '(tempo-insert-region nil)
 '(tempo-interactive t)
 '(tla-number-of-dead-process-buffer 0)
 '(tla-switch-to-buffer-mode (quote single-window))
 '(tla-tips-enabled nil)
 '(tla-use-arrow-keys-for-navigation nil)
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-frame-parameters (quote ((name . "tooltip") (internal-border-width . 1) (border-width . 0))))
 '(tooltip-gud-tips-p t)
 '(tramp-default-method-alist (quote (("%" "" "smb") ("" "\\`\\(anonymous\\|ftp\\)\\'" "ftp") ("\\`ftp" "" "ftp") ("\\`localhost\\'" "\\`root\\'" "su"))))
 '(truncate-partial-width-windows nil)
 '(type-break-good-rest-interval 300)
 '(type-break-interval 5400)
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(view-diary-entries-initially t)
 '(visible-bell t)
 '(w3m-icon-directory "~/.emacs.d/emacs-w3m/icons")
 '(wdired-allow-to-change-permissions (quote advanced))
 '(which-function-mode nil nil (which-func))
 '(x-select-enable-clipboard nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(bold ((t (:foreground "lightcoral" :weight bold))))
 '(bold-italic ((t (:foreground "orange" :slant italic :weight bold))))
 '(cparen-around-andor-face ((t (:foreground "red" :weight bold))))
 '(cparen-around-begin-face ((t (:foreground "red"))))
 '(cparen-around-define-face ((t (:foreground "lightblue" :weight bold))))
 '(cparen-around-quote-face ((t (:foreground "brown"))))
 '(cursor ((t (:background "yellow"))) t)
 '(dircolors-face-objet ((t (:foreground "Gray"))) t)
 '(ecb-default-highlight-face ((((class color) (background dark)) (:inherit ecb-default-general-face :background "slateblue"))))
 '(ecb-directory-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face))))
 '(ecb-history-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face))))
 '(ecb-method-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face))))
 '(ecb-source-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face))))
 '(ecb-token-header-face ((((class color) (background dark)) (:background "SeaGreen4"))))
 '(erc-direct-msg-face ((t (:foreground "cyan"))))
 '(erc-input-face ((t (:foreground "red" :weight bold))))
 '(erc-keyword-face ((t (:foreground "green" :weight bold))))
 '(font-latex-bold-face ((t (:inherit bold))))
 '(font-latex-italic-face ((t (:inherit italic))))
 '(font-latex-math-face ((((class color) (background dark)) (:inherit font-lock-number-face))))
 '(font-latex-string-face ((((class color) (background dark)) (:inherit font-lock-string-face))))
 '(font-latex-verbatim-face ((((class color) (background dark)) (:foreground "burlywood"))))
 '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "lightsteelblue"))))
 '(font-lock-comment-face ((((class color) (background dark)) (:foreground "gray"))))
 '(font-lock-doc-face ((t (:foreground "lightgreen"))))
 '(font-lock-floatnumber-face ((((class color) (background dark)) (:foreground "yellow4"))))
 '(font-lock-function-name-face ((((class color) (background dark)) (:foreground "lightblue2"))))
 '(font-lock-hexnumber-face ((((class color) (background dark)) (:foreground "cyan"))))
 '(font-lock-number-face ((t (:foreground "yellow3"))))
 '(font-lock-pvs-function-type-face ((t (:foreground "blue"))))
 '(font-lock-pvs-parens-face ((t (:foreground "yellow"))))
 '(font-lock-pvs-record-parens-face ((t (:foreground "red"))))
 '(font-lock-pvs-set-brace-face ((t (:foreground "darkred"))))
 '(font-lock-pvs-table-face ((t (:foreground "black"))))
 '(font-lock-qt-face ((((class color) (background dark)) (:foreground "green2" :weight bold))))
 '(font-lock-string-face ((((class color) (background dark)) (:foreground "green3"))))
 '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "LightSkyBlue"))))
 '(gnus-group-mail-low-empty-face ((((class color) (background dark)) (:foreground "aquamarine3"))))
 '(gnus-group-mail-low-face ((t (:foreground "aquamarine3" :weight bold))))
 '(gnus-header-content-face ((t (:foreground "green" :slant italic))))
 '(gnus-header-name-face ((((class color) (background dark)) (:foreground "LightGreen"))))
 '(highlight ((((class color) (background dark) (type x)) (:background "#223939"))))
 '(hl-sexp-face ((((class color) (background dark)) (:inherit highlight))))
 '(icicle-prompt-suffix ((((type x w32 mac) (class color)) (:foreground "dark blue"))))
 '(italic ((t (:foreground "Orchid" :slant italic))))
 '(menu ((((type x-toolkit)) (:background "lightgrey" :foreground "black" :box (:line-width 1 :style released-button)))))
 '(message-header-name-face ((((class color) (background dark)) (:foreground "green3"))))
 '(message-header-other-face ((((class color) (background dark)) (:foreground "#dd0000"))))
 '(mmm-code-submode-face ((t (:background "darkgreen"))))
 '(mmm-default-submode-face ((t (:background "black"))))
 '(mode-line ((t (:background "lightgrey" :foreground "black" :box (:line-width 1 :style released-button)))))
 '(muse-header-1 ((t (:foreground "green" :weight bold))))
 '(muse-header-2 ((t (:foreground "lightblue" :weight bold))))
 '(muse-header-3 ((t (:foreground "grey" :weight bold))))
 '(muse-header-4 ((t (:weight bold))))
 '(planner-note-headline-face ((t (:foreground "turquoise" :weight bold))))
 '(region ((t (:background "#3b6363"))))
 '(show-paren-match ((t (:background "black" :foreground "red" :weight bold))))
 '(show-paren-mismatch ((t (:background "magenta" :foreground "white" :weight bold))))
 '(tabbar-default-face ((t (:inherit variable-pitch :background "gray72" :foreground "gray30" :height 0.8))))
 '(tabbar-selected-face ((t (:inherit tabbar-default-face :foreground "blue" :box (:line-width 2 :color "white" :style pressed-button)))))
 '(tabbar-unselected-face ((t (:inherit tabbar-default-face :box (:line-width 2 :color "white" :style released-button)))))
 '(underline ((t (:foreground "seagreen3" :underline t))))
 '(w3-style-face-00001 ((t (:underline nil :weight normal :height 150))))
 '(w3-style-face-00002 ((t (:underline nil))))
 '(w3-style-face-00003 ((t (:underline nil :weight normal :height 150))))
 '(w3-style-face-00004 ((t (:underline nil :weight normal :height 150))))
 '(w3-style-face-00005 ((t (:underline nil :height 150))))
 '(w3-style-face-00007 ((t (:underline nil :height 150))))
 '(w3-style-face-00008 ((t (:underline nil :weight normal :height 150)))))

;; not honoured by custom-set-property. TODO: investigate why
(setq-default semanticdb-default-save-directory "~/.semantic")
