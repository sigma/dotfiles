;;; tl-fdd-mode-el -- Major mode for editing TL-FDD files

;; Author: Guillaume Phan <guillaume.phan@trusted-logic.fr>

;; References:
;; - Tutorial: http://two-wugs.net/emacs/mode-tutorial.html

;;---------------------------------------------
;; MODE HOOK
;;---------------------------------------------

(defvar tl-fdd-mode-hook nil)

;;---------------------------------------------
;; MODE KEY MAP
;;---------------------------------------------

(defvar tl-fdd-mode-map
  (let ((tl-fdd-mode-map (make-keymap)))
    ; (define-key tl-fdd-mode-map "\C-c\C-c" 'comment-region)
    (define-key tl-fdd-mode-map "\C-l" 'tl-fdd-refresh)
    (define-key tl-fdd-mode-map "\C-f" 'tl-fdd-fill)
    tl-fdd-mode-map)
  "Keymap for TL-FDD major mode")

(defun tl-fdd-refresh ()
  (interactive)
  (font-lock-fontify-buffer)
  (recenter)
  )

(defun tl-fdd-fill ()
  (interactive)
  (if mark-active
      (fill-individual-paragraphs (mark) (point))
    (save-excursion
      (beginning-of-line)
      (set-mark (point))
      (end-of-line)
      (fill-individual-paragraphs (mark) (point))))
  )

;;---------------------------------------------
;; FILE EXTENSIONS
;;---------------------------------------------

;(add-to-list 'auto-mode-alist '("\\.fdd\\'" . tl-fdd-mode))
;(add-to-list 'auto-mode-alist '("\\.tlfdd\\'" . tl-fdd-mode))
;(add-to-list 'auto-mode-alist '("\\.tldoc\\'" . tl-fdd-mode))

;;---------------------------------------------
;; FONT LOCK MODE
;;---------------------------------------------

;; ABSTRACTION LEVELS

(make-face 'tl-fdd-level-hld)
(make-face-bold-italic 'tl-fdd-level-hld)
(set-face-foreground 'tl-fdd-level-hld "DeepSkyBlue3")

(make-face 'tl-fdd-level-lld)
(make-face-bold-italic 'tl-fdd-level-lld)
(set-face-foreground 'tl-fdd-level-lld "MediumSeaGreen")

;; PREPROCESSING

(make-face 'tl-fdd-preprocessing-cmd)
(make-face-bold 'tl-fdd-preprocessing-cmd)
(set-face-foreground 'tl-fdd-preprocessing-cmd "red")

(make-face 'tl-fdd-preprocessing-cond)
(make-face-bold 'tl-fdd-preprocessing-cond)

;; MARKUPS

(make-face 'tl-fdd-face-markup-1)
(make-face-bold 'tl-fdd-face-markup-1)

(make-face 'tl-fdd-face-markup-2)
(make-face-bold 'tl-fdd-face-markup-2)
(set-face-foreground 'tl-fdd-face-markup-2 "gray60")

(make-face 'tl-fdd-face-markup-3)
(make-face-bold-italic 'tl-fdd-face-markup-3)
(set-face-foreground 'tl-fdd-face-markup-3 "DeepSkyBlue3")

(make-face 'tl-fdd-face-markup-4)
(make-face-bold-italic 'tl-fdd-face-markup-4)
(set-face-foreground 'tl-fdd-face-markup-4 "VioletRed2")

(make-face 'tl-fdd-face-markup-5)
(make-face-bold-italic 'tl-fdd-face-markup-5)
(set-face-foreground 'tl-fdd-face-markup-5 "VioletRed3")
5
(make-face 'tl-fdd-face-markup-6)
(make-face-bold-italic 'tl-fdd-face-markup-6)
(set-face-foreground 'tl-fdd-face-markup-6 "BlueViolet")

(make-face 'tl-fdd-face-markup-7)
(make-face-bold-italic 'tl-fdd-face-markup-7)
(set-face-foreground 'tl-fdd-face-markup-7 "MediumSeaGreen")

(defconst tl-fdd-fontification nil)

(unless tl-fdd-fontification
  (setq tl-fdd-fontification
	(list

   ;; SPECIFIC RULES

   ;;;; ABSTRACTION LEVEL: HLD, LLD, ...
   '("\\(HLD\\)" (1 'tl-fdd-level-hld))
   '("\\(LLD\\)" (1 'tl-fdd-level-lld))

   ;;;; PREPROCESSING
   '("^[ \t]*\\(#include\\|#define\\|#ifdef\\|#ifndef\\|#if\\|#elif\\|#else\\|#endif\\|#error\\|#undef\\|#pragma\\)\\(.*\\)$" (1 'tl-fdd-preprocessing-cmd) (2 'tl-fdd-preprocessing-cond))

   ;;;; MARKUPS
   '("\\(@document\\|@introduction\\|@table\\|@figure\\|@example\\|@begin\\|@state\\|@topic\\|@abstract\\)" 1 'tl-fdd-face-markup-1)
   '("\\(@chapter\\|@section\\|@subsection\\|@subsubsection\\|@annex\\|@copyright\\)" 1 'tl-fdd-face-markup-1)
   '("\\(@name\\|@title\\|@label\\|@toc\\|@localtoc\\|@pages\\|@header\\|@window\\|@logo\\|@docproperty\\|@docfile\\)" 1 'tl-fdd-face-markup-2)
   '("\\(@inlineshort\\|@inlinesection\\|@inline\\)" 1 'tl-fdd-face-markup-3)
   '("\\(@endRequirement\\|@endFeature\\|@endSystem\\|@endTestGoal\\|@endTestCase\\|@endTest\\|@endConstruct\\|@endModule\\)" 1 'tl-fdd-face-markup-4)
   '("\\(@requirement\\|@feature\\|@system\\|@testgoal\\|@testcase\\|@test\\|@construct\\|@module\\)" 1 'tl-fdd-face-markup-4)
   '("\\(@endService\\)" 1 'tl-fdd-face-markup-5)
   '("\\(@service\\)" 1 'tl-fdd-face-markup-5)
   '("\\(@endExternal\\)" 1 'tl-fdd-face-markup-6)
   '("\\(@external\\)" 1 'tl-fdd-face-markup-6)
   '("\\(@endTestGoal\\|@endTestCase\\|@endTest\\)" 1 'tl-fdd-face-markup-7)
   '("\\(@testgoal\\|@testcase\\|@test\\)" 1 'tl-fdd-face-markup-7)
   '("\\(@end\\)" 1 'tl-fdd-face-markup-1)

   ;; DEFAULT RULES

	 (cons "//.*$" '(0 font-lock-comment-face t))
	 (cons "^[ \t]*#\\([a-zA-Z]+\\)" '(1 font-lock-builtin-face))
	 (cons "^[ \t]*@\\([a-zA-Z]+\\)" '(1 font-lock-keyword-face))
	 (cons "^[ \t]*\\([A-Z]+\\):"  '(1 font-lock-builtin-face))
	 (cons "\{\\([a-z]+:\\)\\([^\}]+\\)\}" '((1 font-lock-keyword-face) (2 font-lock-function-name-face)))
	 (cons "\{\\([^\}]+\\)\}" '(1 font-lock-function-name-face))
	 (cons "\\[\\([^]]+\\)\\]" '(1 font-lock-function-name-face))
	 (cons "\\[*$@`<>\]" font-lock-builtin-face)
	 (cons ";;" font-lock-builtin-face)
	 (cons "</?[a-z:0-9]+>" font-lock-type-face)
	 (cons "\\[:" font-lock-type-face)
	 (cons ":\\]" font-lock-type-face)
	 (cons "::" font-lock-type-face)
	 (cons "[![ lcr]*!]" font-lock-type-face)
	 )))

(defvar tl-fdd-font-lock-keywords tl-fdd-fontification
  "Default highlighting expressions for TL-FDD mode.")

;;---------------------------------------------
;; INDENTATION
;;---------------------------------------------

;;OK
(defun tl-fdd-indent-line ()
  "Indent current line as TL-FDD code."
  (interactive)
  (beginning-of-line)
  (if (bobp) (indent-line-to 0)
    (let (M pM)
      (setq M (tl-fdd-markup-current))
      (setq pM (tl-fdd-markup-previous))
      (if M
          (indent-line-to (tl-fdd-markup-indent M (car pM) (nth 1 pM)))
        (indent-line-to (tl-fdd-text-indent (car pM) (nth 1 pM))))))
  )

;;OK
(defun tl-fdd-is-block-open-markup (M)
  "
 Whether passed markup is block-opening markup
  "
  (interactive "sM:\n")
  (or
   (equal M "@document")
   (equal M "@introduction")
   (equal M "@copyright")
   (equal M "@chapter")
;   (equal M "@section")
;   (equal M "@subsection")
;   (equal M "@subsubsection")
   (equal M "@begin")
   (equal M "@view")
   (equal M "@content")
   (equal M "@abstract")
   (equal M "@table")
   (equal M "@figure")
   (equal M "@example")
   (equal M "@requirement")
   (equal M "@feature")
   (equal M "@system")
   (equal M "@construct")
   (equal M "@module")
   (equal M "@external")
   (equal M "@topic")
   (equal M "@state")
   (equal M "@service")
   (equal M "@testgoal")
   (equal M "@test")
   (equal M "@testcase")
   (equal M "@apdu")
;   (equal M "@accept")
;   (equal M "@parameters")
;   (equal M "@process")
;   (equal M "@errors")
;   (equal M "@procedure")
;   (equal M "@param")
;   (equal M "@return")
   (equal M "@rationale")
;   (equal M "@pre")
;   (equal M "@constraint")
;   (equal M "@references")
   )
  );;defun

;;OK
(defun tl-fdd-is-block-close-markup (M)
  "
 Whether passed markup is block-closing markup
  "
  (interactive "sM:\n")
  (or
   (equal M "@end")
   (equal M "@endRequirement")
   (equal M "@endFeature")
   (equal M "@endSystem")
   (equal M "@endConstruct")
   (equal M "@endModule")
   (equal M "@endExternal")
   (equal M "@endService")
   (equal M "@endTestGoal")
   (equal M "@endTestCase")
   (equal M "@endTest")
   (equal M "@endApdu")
   )
  );;defun

;;OK
(defun tl-fdd-text-indent
  ;; previous markup
  ;; previous markup indent
  (pM pMI)
  "
 Text indentation,
 based on association with markups.
  "
  (interactive "spM: \nnpMI: \n")
  (if (bobp) 0

  (cond
   ;;
   ( (not pM) 0 )
   ;; block markups
   ( (tl-fdd-is-block-open-markup pM)
     (+ pMI default-tab-width) )
   ;; default
   ( pMI )
   ) ;; cond
  )) ;; defun

;;WORKS
(defun tl-fdd-markup-current ()
  "
 Get markup (only) on current line, or nil.
  "
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^[ \t]*\\(@[a-zA-Z0-9_]+\\).*$")
        (buffer-substring-no-properties (match-beginning 1) (match-end 1))
      nil)
    )
  )

(defun tl-fdd-markup-previous ()
  "
 Get previous markup data (markup indent).
  "
  (interactive)
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (while (not (or (bobp) (looking-at "^[ \t]*@.*$")))
      (forward-line -1)
      (beginning-of-line)
      )
    (if (bobp) nil
      (list (tl-fdd-markup-current) (current-indentation))))
  )

(defun tl-fdd-previous-indent ()
  "
 Get previous significant indent data (markup indent).
  "
  (interactive)
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (while (not (or (bobp) (looking-at "^[ \t]*[^ \t\n\r]")))
      (forward-line -1)
      (beginning-of-line)
      )
    (list (tl-fdd-markup-current) (current-indentation)))
  )

(defun tl-fdd-markup-indent
  ;; previous markup
  ;; previous markup indent
  (M pM pMI)
  "
 Markup indentation,
 based on association with markups.
  "
  (interactive "sM: \nspM: \nnpMI")
  (if (bobp) 0

  (cond
   ;; no previous markup
   ( (not pM) 0 )

   ;; previous markup is a block-closing markup
   ( (tl-fdd-is-block-close-markup M)
     (- (tl-fdd-text-indent pM pMI) default-tab-width) )

   ;; previous markup is a block-opening markup
   ( (tl-fdd-is-block-open-markup pM)
     (+ pMI default-tab-width) )

   ;; default
   ( pMI )
   ) ;; cond
  )) ;; defun

;(defun tl-fdd-markup-indent
;  ;; previous markup
;  ;; previous markup indent
;  (M pM pMI)
;  "
; Text indentation,
; based on association with markups.
;  "
;  (interactive "sM: \nspM: \nnpMI")
;  (if (bobp) 0
;
;  (cond
;   ;; no previous markup
;   ( (not pM) 0 )
;
;   ;; @end
;   ;;( (equal M "@end") (let ( (tmp (nth 1 (tl-fdd-previous-indent)) ) ) (if (> tmp 0) (- tmp default-tab-width) 0) ) )
;   ( (equal M "@end") (+ (current-indentation) default-tab-width) )
;
;   ;; previous markup is @end
;   ( (equal pM "@end") pMI )
;
;   ;; @document
;   ( (equal M "@document")
;     (let ( (tmp (tl-fdd-markup-last-occurrence (list "@batch"))) )
;       (if tmp (+ (nth 1 tmp) default-tab-width) nil))) ;; incr. indent
;
;   ;; @annex
;   ( (equal M "@annex")
;     (let ( (tmp (tl-fdd-markup-last-occurrence (list "@document"))) )
;       (if tmp (+ (nth 1 tmp) default-tab-width) nil))) ;; incr. indent
;
;   ;; @copyright
;   ( (equal M "@copyright")
;     (let ( (tmp (tl-fdd-markup-last-occurrence (list "@document"))) )
;       (if tmp (+ (nth 1 tmp) default-tab-width) nil))) ;; incr. indent
;
;   ;; @chapter
;   ( (equal M "@chapter")
;     (let ( (tmp (tl-fdd-markup-last-occurrence (list "@document" "@view"))) )
;       (if tmp (+ (nth 1 tmp) default-tab-width) nil))) ;; incr. indent
;
;   ;; @section
;   ( (equal M "@section")
;     (let ( (tmp (tl-fdd-markup-last-occurrence (list "@document" "@view" "@requirement" "@feature" "@system" "@module"))) )
;       (if tmp (+ (nth 1 tmp) default-tab-width) nil))) ;; incr. indent
;
;   ;; @subsection
;   ( (equal M "@subsection")
;     (let ( (tmp (tl-fdd-markup-last-occurrence (list "@section"))) )
;       (if tmp (+ (nth 1 tmp) default-tab-width) nil))) ;; incr. indent
;
;   ;; @subsubsection
;   ( (equal M "@subsubsection")
;     (let ( (tmp (tl-fdd-markup-last-occurrence (list "@subsection"))) )
;       (if tmp (+ (nth 1 tmp) default-tab-width) nil))) ;; incr. indent
;
;   ;; @begin
;   ( (equal M "@begin")
;     (let ( (tmp (tl-fdd-markup-last-occurrence (list "@section"))) )
;       (if tmp (nth 1 tmp) nil))) ;; same indent
;
;   ;; @requirement, @feature, @testgoal, @rationale
;   ( (or (equal M "@requirement")
;         (equal M "@feature")
;         (equal M "@testgoal"))
;     (if (equal pMI "@rationale")
;         (+ pMI default-tab-width)
;       (save-excursion
;         (forward-line -1)
;         (beginning-of-line)
;         (while (and (not (bobp)) (looking-at "^[ \t]*$"))
;           (forward-line -1)
;           (beginning-of-line))
;         (current-indentation))))
;
;   ;; previous markup is a "block" markup
;   ( (tl-fdd-is-block-open-markup pM)
;     (+ pMI default-tab-width) )
;
;   ;; default
;   ( pMI )
;   ) ;; cond
;  )) ;; defun

(defun tl-fdd-markup-last-occurrence
  ;; markup(s) to lookup for
  (L)
  "
 Lookup backward for specified markup data (markup indent)
  "
  (let (tmp)
    (save-excursion
      (setq tmp (tl-fdd-markup-previous-noexcursionsaving))
      (while (and tmp (not (tl-fdd-list-contains L (car tmp))))
        (setq tmp (tl-fdd-markup-previous-noexcursionsaving)))
      (if tmp tmp nil)))
  )

(defun tl-fdd-list-contains (L S)
  "
 Whether list L contains string S.
  "
  (let ( (tmpS (car L)) (tmpL L) )
    (while (and tmpS (not (equal tmpS S)))
      (setq tmpL (cdr tmpL))
      (setq tmpS (car tmpL))
      )
    (if tmpS t nil))
  )

(defun tl-fdd-markup-previous-noexcursionsaving ()
  "
 Get previous markup data (markup indent).
  "
  (interactive)
  (forward-line -1)
  (beginning-of-line)
  (while (not (or (bobp) (looking-at "^[ \t]*@.*$")))
    (forward-line -1)
    (beginning-of-line)
    )
  (if (bobp) nil
    (list (tl-fdd-markup-current) (current-indentation)))
  )


;;---------------------------------------------
;; SYNTAX TABLE
;;---------------------------------------------

(defvar tl-fdd-mode-syntax-table
  (let ((tl-fdd-mode-syntax-table (make-syntax-table)))

    ; This is added so entity names with underscores can be more easily parsed
  (modify-syntax-entry ?_  "w"   tl-fdd-mode-syntax-table) ; _ as a word constituent
  (modify-syntax-entry ?\" "."   tl-fdd-mode-syntax-table) ; ; " as a simple punctuation
  (modify-syntax-entry ?`  "\""  tl-fdd-mode-syntax-table) ; `tt`
  (modify-syntax-entry ?$  "\" 1234"  tl-fdd-mode-syntax-table) ; $tt$ and $$verbatim$$

	; double-slash single line comments
  (modify-syntax-entry ?/ ". 12b" tl-fdd-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" tl-fdd-mode-syntax-table)
	tl-fdd-mode-syntax-table)
  "Syntax table for tl-fdd-mode")

;;---------------------------------------------
;; SETUP MODE
;;---------------------------------------------

(defun tl-fdd-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map tl-fdd-mode-map)
  (set-syntax-table tl-fdd-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(tl-fdd-font-lock-keywords))
  ;; Register indentation function
  (set (make-local-variable 'indent-line-function) 'tl-fdd-indent-line)
  ;; Filling paragraphs
  (set (make-local-variable 'paragraph-start) "[ \t]*\\(@\\|-\\|\\+\\|\\[\\|%\\)")
  ;;
  (setq major-mode 'tl-fdd-mode)
  (setq mode-name "TL-FDD")
  (run-hooks 'tl-fdd-mode-hook))

(provide 'tl-fdd)

;;; tl-fdd-mode.el ends here
