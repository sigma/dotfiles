;; -*- mode: emacs-lisp; mode: hi-lock; mode: page-break; auto-compile-lisp: nil; -*-
;; $Id$

;; Hi-lock: (("^;;; \\(.*\\)" (1 'hi-black-hb t)))
;; Hi-lock: (("^ +;;; \\(.*\\)" (1 'hi-black-b t)))
;; Hi-lock: ((";; \\(.*\\)" (1 'italic append)))
;; Hi-lock: end

;;; Basis
;; Load site-specific stuff
(if (file-exists-p (expand-file-name "~/.gnus-local.el"))
    (load-file (expand-file-name "~/.gnus-local.el")))

(require 'message-x)

;; Define url catchers
(setq browse-url-browser-function '(("^mailto:" . gnus-url-mailto)
				    ("." . browse-url-netscape)))

;; No primary select method
(setq
 gnus-select-method
 '(nnnil ""))

;; use bbdb
(setq nbc-bbdb t)

;; use nnir
(setq nbc-nnir nil)

;; global parameters
(setq
 ;; don't annoy me with confirmation requests
 gnus-expert-user t
 ;; 5 lines of signature
 gnus-signature-limit '(5.0 "^---*Forwarded article"))

(setq
 gnus-check-new-newsgroups t
 gnus-read-active-file 'some
 gnus-save-killed-list t
 gnus-save-newsrc-file nil
 gnus-read-newsrc-file nil
 gnus-subscribe-newsgroup-method 'gnus-subscribe-topics
 message-generate-headers-first t
 gnus-gcc-mark-as-read t
 gnus-inhibit-startup-message t
 gnus-use-cache t
 gnus-agent t
 ;; Split mails
 nnmail-split-methods 'nnmail-split-fancy)

(add-hook 'gnus-select-article-hook 'gnus-agent-fetch-selected-article)

(defun my-gnus-expiry-target (group)
  (concat my-archived-group-backend ":"
          group
          "-archive."
          (format-time-string "%m-%Y" (my-gnus-get-article-date))))

(defun my-gnus-get-article-date ()
  "Extracts the date from the current article and converts it to Emacs time"
  (save-excursion
    (goto-char (point-min))
    (gnus-date-get-time (message-fetch-field "date"))))

(setq
 ;; archiving backend
 my-archived-group-backend "nnml"
 ;; set expiry target to a function call
 nnmail-expiry-target 'my-gnus-expiry-target)

(setq
;;  gnus-use-procmail t
;;  nnmail-spool-file 'procmail
;;  nnmail-procmail-directory "~/.mail/"
;;  nnmail-procmail-suffix ""
;;  mail-sources
;;  (list '(directory
;; 	 :path "~/.mail/"
;; 	 :suffix ""
;; 	 )
;;        )

 gnus-auto-expirable-newsgroups "mail.\\(root\\|delete\\)"
 nnmail-use-long-file-names t
 gnus-uncacheable-groups "^nnml"
)

;; increase score for most read groups
(add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)

;; Use topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq
 gnus-thread-hide-subtree t
 gnus-suppress-duplicates t
 gnus-auto-select-first nil)

(setq
 gnus-home-score-file "all.SCORE"
 gnus-permanently-visible-groups ".*"
 gnus-large-newsgroup 100)


;;; Summary buffer
(add-hook 'gnus-summary-mode-hook
	  (lambda ()
	    (if (or (gnus-news-group-p gnus-newsgroup-name)
		    (string-match "^nnml:list" gnus-newsgroup-name))
		(setq gnus-thread-sort-functions
		      '(gnus-thread-sort-by-subject
			gnus-thread-sort-by-total-score))
	      (setq gnus-thread-sort-functions
		    '(gnus-thread-sort-by-date)))))

(setq
 gnus-extra-headers '(Newsgroups X-Spam-Status)
 nnmail-extra-headers gnus-extra-headers)

;; Personal threading view
(defun gnus-user-format-function-Z (ok)
  (format "%s%s" (replace-regexp-in-string "\\(.*\\)    " "\\1   >" gnus-tmp-indentation) gnus-tmp-subject-or-nil))

;; this corresponds to a topic line format of "%n %A"
(defun gnus-user-format-function-topic-line (dummy)
  (let ((topic-face (if (zerop total-number-of-articles)
                        'italic
                      'bold)))
    (propertize
     (format "%s %d" name total-number-of-articles)
     'face topic-face)))

;; Some line format
(when window-system
 (setq gnus-sum-thread-tree-root "\x4912f ")
 (setq gnus-sum-thread-tree-single-indent "\x4912e ")
 (setq gnus-sum-thread-tree-leaf-with-other "\x4903c\x49020\x490fa ")
 (setq gnus-sum-thread-tree-vertical "\x49022")
 (setq gnus-sum-thread-tree-single-leaf "\x490b0\x49020\x490fa "))

(setq gnus-summary-same-subject "")

(copy-face 'default 'mysubject)
(setq gnus-face-1 'mysubject)

(copy-face 'default 'mytime)
(set-face-foreground 'mytime "red")
(setq gnus-face-2 'mytime)

(copy-face 'default 'mythreads)
(set-face-foreground 'mythreads "red")
(setq gnus-face-3 'mythreads)

(copy-face 'default 'mygrey)
(set-face-foreground 'mygrey "grey")
(setq gnus-face-4 'mygrey)

(copy-face 'default 'myblack)
(set-face-foreground 'myblack "grey60")
(setq gnus-face-5 'myblack)

(copy-face 'default 'mybiggernumbers)
(set-face-foreground 'mybiggernumbers "red")
(setq gnus-face-6 'mybiggernumbers)

(setq gnus-summary-line-format (concat
                                "%*%5{%U%R%z%}"
                                "%4{|%}"
                                "%2{%-10&user-date;%}"
                                "%4{|%}"
                                "%4{|%}"
                                "%2{ %}%(%-24,24n"
                                "%4{|%}"
                                "%2{%5i%}"
                                "%4{|%}"
                                "%2{%6k %}%)"
                                "%4{|%}"
                                "%2{ %}%3{%B%}%1{%s%}\n"))

(setq
 gnus-group-line-format "%M%S%p%P%5y: %(%G%) (%t)\n"
 gnus-group-mode-line-format "Gnus: %%b"
; gnus-summary-line-format "%U%R%z %[%-15,15n%] : %-55,55uZ (%d)\n"
 gnus-summary-mode-line-format "Gnus: %g [%r/%U]"
 gnus-article-mode-line-format "Gnus: %g [%r/%U] %m"
 gnus-topic-line-format "%i[ %u&topic-line; ] %v\n")

(if (eq window-system 'x)
    (progn
      (gnus-add-configuration
       `(group
	 (vertical 1.0
		   (horizontal 9
			       ("*BBDB*" 0.3)
			       ("*Calendar*" 1.0))
		   (horizontal 1.0
			       (group 0.3 point)
			       (,planner-default-page 1.0)))))

      (gnus-add-configuration
       '(info
         (vertical 1.0
                   (horizontal 9
                               ("*BBDB*" 0.3)
                               ("*Calendar*" 1.0))
                   (horizontal 1.0
                               (group 0.3 point)
                               (info 1.0)))))

      (gnus-add-configuration
       '(article
	 (vertical 1.0
		   (horizontal 9
			       ("*BBDB*" 0.3)
			       (summary 1.0 point))
		   (horizontal 1.0
			       (group 0.3)
			       (article 1.0)))))

      (gnus-add-configuration
       '(reply-yank
	 (vertical 1.0
		   (horizontal 9
			       ("*BBDB*" 0.3)
			       (summary 1.0))
		   (horizontal 1.0
			       (group 0.3)
			       (message 1.0 point)))))

      (gnus-add-configuration
       '(forward
	 (vertical 1.0
		   (horizontal 9
			       ("*BBDB*" 0.3)
			       (summary 1.0))
		   (horizontal 1.0
			       (group 0.3)
			       (message 1.0 point)))))

      (gnus-add-configuration
       '(summary
	 (vertical 1.0
		   (horizontal 9
			       ("*BBDB*" 0.3)
			       (summary 1.0 point))
		   (horizontal 1.0
			       (group 0.3)
			       ("*scratch*" 1.0)))))

      (gnus-add-configuration
       '(reply
	 (vertical 1.0
                   (horizontal 9
			       ("*BBDB*" 0.3)
			       (summary 1.0))
		   (horizontal 1.0
			       (group 0.3)
			       (vertical 1.0
                                         (article 0.3)
                                         (message 1.0 point))))))
      )
  )

(setq
 nbc-gnus-visible-headers
 '("^From:\\|^Organization:\\|^To:\\|^Cc:\\|^Reply-To:\\|^Subject:\\|^Sender:"
   "^Newsgroups:.+[,]+.*$"
   "^X-Mailer:\\|^X-Newsreader:\\|^user-Agent\\|^X-Posting-Agent"
   "^Followup-To:\\|^Date:"))

(setq
 gnus-boring-article-headers '(empty followup-to reply-to))

;; Format display
(add-hook 'gnus-article-display-hook 'gnus-article-highlight)
(add-hook 'gnus-article-display-hook 'gnus-article-hide-headers-if-wanted)
(add-hook 'gnus-article-display-hook 'gnus-article-hide-boring-headers)
(add-hook 'gnus-article-display-hook 'gnus-article-de-quoted-unreadable)
(add-hook 'gnus-article-display-hook 'gnus-article-strip-leading-blank-lines)
(add-hook 'gnus-article-display-hook 'gnus-article-remove-trailing-blank-lines)
(add-hook 'gnus-article-display-hook 'gnus-article-strip-multiple-blank-lines)
(add-hook 'gnus-article-display-hook 'gnus-article-emphasize)

;; ignore vcards
(setq gnus-ignored-mime-types '("text/x-vcard"))

;; I want plain/text mails
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

(add-hook 'gnus-message-setup-hook 'font-lock-fontify-buffer)

(setq nnmail-message-id-cache-file (concat nbc-gnus-dir "nnmail-cache"))
(setq nnmail-message-id-cache-length 5000)
(setq nnmail-cache-accepted-message-ids t)

(setq nnmail-treat-duplicates 'warn)


;;; Mail sending

(add-hook 'message-mode-hook 'turn-on-auto-fill)

(setq
 message-cite-function 'trivial-cite

 gnus-signature-separator  '("^-- $"        ; The standard
			     "^--$"		; Die OE Die !
			     "^-- *$"        ; A common mangling
			     "^-------*$"    ; Many people just use a looong
					; line of dashes.  Shame!
			     "^ *--------*$" ; Double-shame!
			     "^________*$"   ; Underscores are also popular
			     "^========*$" ; Pervert!
			     ))


;;; Posting Styles

(setq
 gnus-posting-styles
 '(
   ;; For news
   (message-this-is-news
    (address "y_hodique@yahoo.fr")
    )
   ;; For mail
   (message-this-is-mail
    (address "Yann.Hodique@lifl.fr")
    )
))

(defun nbc-message-insert-citation-line ()
  "Function that inserts a simple citation line."
  (when message-reply-headers
    (insert (mail-header-from message-reply-headers)
	    " wrote:\n\n")))

(setq message-citation-line-function 'nbc-message-insert-citation-line)

;; Tell gnus into which group to store messages
(setq gnus-message-archive-group
       '((if (message-news-p)
 	     "news"
 	   (concat "mail." (format-time-string "%Y-%m" (current-time))))))

(setq
 gnus-prompt-before-saving t
 gnus-default-article-saver  'gnus-summary-save-in-rmail
 gnus-split-methods
 '(("^Newsgroups:.*\\(unix\\|linux\\|bsd\\)" "unix-stuff")
   ("^Newsgroups:.*\\(tex\\|xml\\)" "xml-stuff")
   ("^Newsgroups:.*perl" "perl-stuff")
   ("^Newsgroups:.*emacs\\|^Newsgroups:.*gnus" "emacs-stuff")
   (".*" "misc")))

;;; Misc

(defadvice gnus-summary-reply (around reply-in-news activate)
  (interactive)
  (when (or (not (gnus-news-group-p gnus-newsgroup-name))
            (y-or-n-p "Really reply to author ? "))
    ad-do-it))

(defun my-gnus-summary-show-thread ()
  "Show thread without changing cursor positon."
  (interactive)
  (gnus-summary-show-thread)
  (beginning-of-line)
  (forward-char 1))

(define-key gnus-summary-mode-map [(right)] 'my-gnus-summary-show-thread)
(define-key gnus-summary-mode-map [(left)]  'gnus-summary-hide-thread)


;;; BBDB

(when nbc-bbdb
  (require 'bbdb)
  (bbdb-initialize 'gnus 'message 'sc)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

  (bbdb-insinuate-message)

  (setq bbdb-file (concat nbc-gnus-dir "bbdb"))

  (add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)
  (add-hook 'message-setup-hook 'bbdb-define-all-aliases)

  (setq
   bbdb-offer-save 'yes
   bbdb-electric-p t
   bbdb-pop-up-target-lines 5
   bbdb-use-pop-up nil
   bbdb-north-american-phone-numbers-p nil)

  (add-hook 'message-mode-hook
	    (lambda () (local-set-key [(meta tab)] 'bbdb-complete-name)))

  (add-hook 'bbdb-list-hook 'my-bbdb-display-xface)
  (defun my-bbdb-display-xface ()
    "Search for face properties and display the faces."
    (when (or (gnus-image-type-available-p 'xface)
              (gnus-image-type-available-p 'pbm))
      (save-excursion
        (goto-char (point-min))
        (let ((inhibit-read-only t); edit the BBDB buffer
              (default-enable-multibyte-characters nil); prevents corruption
              pbm faces)
          (while (re-search-forward "^           face: \\(.*\\)" nil t)
            (setq faces (match-string 1))
            (replace-match "" t t nil 1)
            (dolist (data (split-string faces ", "))
              (condition-case nil
                  (insert-image (create-image (gnus-convert-face-to-png data) nil t))
                (error
                 (insert-image (gnus-create-image (uncompface data) nil t :face 'tooltip))))
              (insert " ")))))))
  (add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
  (setq bbdb-auto-notes-alist '(("X-Face" (".+" face 0 'replace))
                                ("Face" (".+" face 0 'replace))))
)


;;; Gnus extensions

(when nbc-nnir
  (require 'nnir)
;  (setq nnir-swish-e-index-file "~/.emacs.d/index.swish-e")
  (setq nnir-mail-backend (nth 0 gnus-secondary-select-methods)
	nnir-search-engine 'swish-e))

(add-hook 'message-mode-hook 'flyspell-mode)

(require 'osd)
(osd-init)

(defvar osd-group-list '("INBOX.thesis" "INBOX.teaching" "INBOX.lifl"))

(defun osd-gnus-display-from ()
  (goto-char (point-min))
  (if (member group-in osd-group-list)
      (osd-display (concat "Mail received for group " group-in))))

(add-hook 'nnmail-read-incoming-hook 'osd-gnus-display-from)
(add-hook 'gnus-exit-gnus-hook 'osd-close)

;;
;; automatic mail scan without manual effort.
;;
;; level-specified group scanner.
(defun gnus-demon-scan-mail-or-news-and-update (level)
"Scan for new mail, updating the *Group* buffer."
;  (let ((win (current-window-configuration)))
    (unwind-protect
        (save-window-excursion
            (when (gnus-alive-p)
              (save-excursion
                (set-buffer gnus-group-buffer)
                (gnus-group-get-new-news level)))))
;      (set-window-configuration win))
)

;;
;; level 2: only mail groups are scanned.
(defun gnus-demon-scan-mail-and-update ()
  "Scan for new mail, updating the *Group* buffer."
  ;(osd-display "Scan for new mail")
  (gnus-demon-scan-mail-or-news-and-update 2))

;;
;; level 3: mail and local news groups are scanned.
(defun gnus-demon-scan-news-and-update ()
  "Scan for new mail, updating the *Group* buffer."
  ;(osd-display "Scan for new news")
  (gnus-demon-scan-mail-or-news-and-update 3))

(setq gnus-use-demon t)

(defun yh-gnus-demon-install ()
  (interactive)
  ;; scan for news every 20 minutes
  (gnus-demon-add-handler 'gnus-demon-scan-news-and-update 20 2)
  ;; scan for mails every 10 minutes
  (gnus-demon-add-handler 'gnus-demon-scan-mail-and-update 10 2))

(defun yh-gnus-demon-uninstall ()
  (interactive)
  (gnus-demon-cancel))

(setq message-signature 'fortune)

(yh-gnus-demon-install)

(defvar fortune-program nil
  "*Program used to generate epigrams, default \"fortune\".")

(defvar fortune-switches nil
  "*List of extra arguments when `fortune-program' is invoked.")

(setq fortune-program "/usr/games/fortune")
(add-to-list 'fortune-switches "chapterhouse-dune")
(add-to-list 'fortune-switches "children-of-dune")
(add-to-list 'fortune-switches "dune")
(add-to-list 'fortune-switches "dune-messiah")
(add-to-list 'fortune-switches "god-emperor")
(add-to-list 'fortune-switches "heretics-of-dune")
(add-to-list 'fortune-switches "house-atreides")
(add-to-list 'fortune-switches "house-harkonnen")

(defun fortune (&optional long-p)
  "Generate a random epigram.
An optional prefix argument generates a long epigram.
The epigram is inserted at point if called interactively."
  (interactive "*P")
  (let ((fortune-buffer (generate-new-buffer " fortune"))
        (fortune-string "Have an adequate day."))
    (unwind-protect
        (save-excursion
          (set-buffer fortune-buffer)
          (apply 'call-process
                 (append (list (or fortune-program "fortune") nil t nil)
                         fortune-switches (list (if long-p "-l" "-s"))))
          (dos2unix)
          (skip-chars-backward "\n\t ")
          (setq fortune-string (buffer-substring (point-min) (point))))
      (kill-buffer fortune-buffer))
    (if (interactive-p)
        (insert fortune-string))
    fortune-string))

(setq mm-text-html-renderer 'w3m)

;; gnus alias to switch identity
;; (require 'gnus-alias)
;; (gnus-alias-init)
;; (setq gnus-alias-default-identity "Lifl"
;;       gnus-alias-identity-alist '(("Yahoo" "" "Yann Hodique <y_hodique@yahoo.fr>" "" nil "" "")
;;                                   ("Lifl" "" "Yann Hodique <Yann.Hodique@lifl.fr>" "ENS Cachan / Lifl" nil "" ""))
;;       gnus-alias-identity-rules '(("News" message-news-p "Yahoo")
;;                                   ("Mails" message-mail-p "Lifl")))

(setq gnus-group-highlight
      '(((and (= unread 0) (not mailp) (eq level 1)) . gnus-group-news-1-empty-face)
	((and (not mailp) (eq level 1)) . gnus-group-news-1-face)
	((and (= unread 0) (not mailp) (eq level 2)) . gnus-group-news-2-empty-face)
	((and (not mailp) (eq level 2)) . gnus-group-news-2-face)
	((and (= unread 0) (not mailp) (eq level 3)) . gnus-group-news-3-empty-face)
	((and (not mailp) (eq level 3)) . gnus-group-news-3-face)
	((and (= unread 0) (not mailp) (eq level 4)) . gnus-group-news-4-empty-face)
	((and (not mailp) (eq level 4)) . gnus-group-news-4-face)
	((and (= unread 0) (not mailp) (eq level 5)) . gnus-group-news-5-empty-face)
	((and (not mailp) (eq level 5)) . gnus-group-news-5-face)
	((and (= unread 0) (not mailp) (eq level 6)) . gnus-group-news-6-empty-face)
	((and (not mailp) (eq level 6)) . gnus-group-news-6-face)
	((and (= unread 0) (not mailp)) . gnus-group-news-low-empty-face)
	((and (not mailp)) . gnus-group-news-low-face)
	((and (= unread 0) (eq level 1)) . gnus-group-mail-1-empty-face)
	((eq level 1) . gnus-group-mail-1-face)
	((and (= unread 0) (eq level 2)) . gnus-group-mail-2-empty-face)
	((eq level 2) . gnus-group-mail-2-face)
	((and (= unread 0) (eq level 3)) . gnus-group-mail-3-empty-face)
	((eq level 3) . gnus-group-mail-3-face)
	((= unread 0) . gnus-group-mail-low-empty-face)
	(t . gnus-group-mail-low-face)))

(defun my-setup-hl-line ()
  (hl-line-mode 1)
  (setq cursor-type nil) ; make the cursor invisible
  )

(add-hook 'gnus-summary-mode-hook 'my-setup-hl-line)
(add-hook 'gnus-group-mode-hook 'my-setup-hl-line)


;;; Supercite

;; Supercite Settings
(autoload 'sc-cite-original     "supercite" "Supercite 3.1" t)
(autoload 'sc-submit-bug-report "supercite" "Supercite 3.1" t)
(add-hook 'mail-citation-hook 'sc-cite-original)

;; Do not insert the original author's signature when citing with supercite
(add-hook 'sc-pre-hook
	  (lambda ()
	    (save-excursion
	      (let ((start (point))
		    (end (mark t)))
		(goto-char end)
		(when (re-search-backward "^-- $" start t)
;		(when (re-search-backward gnus-signature-separator start t)
		  (forward-line -1)
		  (while (looking-at "[ \t]*$")
		    (forward-line -1))
		  (forward-line 1)
		  (delete-region (point) end))))))

;; Attibutions to use by preference - the first non-nil string wins
(setq sc-preferred-attribution-list '("x-attribution"
				      "firstname"
				      "initials"
				      "sc-lastchoice"
				      "lastname"))


;;; SpamAssassin-related stuff for Gnus

;; Show the SpamAssassin score of mails in mail groups

(defvar nix-spam-checked-groups '("nnml:spam" "nnml:mbox")
  "The set of groups in which spam-checked mails may be placed.")

(defun nix-select-group-show-spam-score ()
  "Show the SpamAssassin score of mails in this group, if it is a mail group.
Only those groups given in `nix-spam-checked-groups' are checked.

Must be called from the `gnus-select-group-hook'."
  (and (member gnus-newsgroup-name nix-spam-checked-groups)
       (not (gnus-group-find-parameter (or gnus-newsgroup-name "")
                                       'gnus-summary-line-format))
       (gnus-group-add-parameter gnus-newsgroup-name
                                 '(gnus-summary-line-format "%U%R%2,2ub%5,5us%5t%z%I%(%[%4L: %-20,20uB%]%) %s\n"))))

(defun nix-return-spamassassin-score (header)
  "Given a Gnus message header HEADER, return an indication of the spam score."
  (let ((sa-score-header (gnus-extra-header 'X-Spam-Status header)))
    ;; Is the header is a real SA header?
    (save-match-data
      (if (string-match "hits=\\([^ ]*\\)" sa-score-header)
          (substring sa-score-header (match-beginning 1) (match-end 1))
        " "))))

(defalias 'gnus-user-format-function-s 'nix-return-spamassassin-score)

;(add-hook 'gnus-select-group-hook 'nix-select-group-show-spam-score)

;; Mail-specific stuff.

;; Allow easy reporting of mail to duplicate-checking agents (Razor, Pyzor, DCC, &c)
;; via SpamAssassin, and removal from the SpamAssassin autowhitelist.

(defsubst nix-pipe-given-article-somewhere (article command &optional report-buffer temp-buffer background)
  "Pipe the given ARTICLE through some COMMAND.
Optionally use a specific TEMP-BUFFER and report the output
in a particular REPORT-BUFFER.  If the piping happens in the BACKGROUND,
output is discarded."
  (let ((temp-buffer (or temp-buffer " *article pipe temp*"))
        (report-buffer (or report-buffer "*Article Pipe Output*"))
        (command (if background
                     (concat "perl -e 'my $mail; while (<>) { $mail .= $_; } open STDIN, \"/dev/null\" or die \"Cannot read from /dev/null\"; open STDOUT, \">/dev/null\" or die \"Cannot write to /dev/null\"; defined (my $ret=fork()) or die \"Cannot fork: $!\"; exit if $ret; setsid; open STDERR, \">&STDOUT\" or die \"Cannot dup stdout: $!\"; open OUT,\"|"
                             command "\" or die \"Subprocess fork failed: $!\"; print OUT $mail;'")
                   command)))
    (save-excursion
      (if (gnus-request-article article gnus-newsgroup-name
                                (get-buffer-create temp-buffer))
          (unwind-protect
              (progn
                (set-buffer temp-buffer)
                (shell-command-on-region (point-min) (point-max) command
                                         (get-buffer-create report-buffer)))
            (kill-buffer temp-buffer))))))

;; All the sshing is to cater for a single host where SA is known to work;
;; bugs in Perl 5.8.0 and UltraSPARC prevent it from working on most of my
;; machines. (Why didn't they write it in elisp? ;} )

(defun nix-report-spam ()
  "Report the current message as spam."
  (interactive)
;  (nix-pipe-given-article-somewhere gnus-current-article "spamassassin -r" "*Spam Reporting*" nil t))
  (nix-pipe-given-article-somewhere gnus-current-article "sa-learn --spam --no-rebuild --single" "*Spam Reporting*" nil t))

(defun nix-report-spam-all ()
  "Report all unread messages in the current group as spam."
  (interactive)
  (save-excursion
    (when (gnus-summary-first-subject t t)
      (while (and
              (progn
                                        ;                  (gnus-summary-mark-article-as-read gnus-read-mark)
                (let ((gnus-visible-headers "^From: \\|^Subject: "))
                  (gnus-summary-select-article))
                (message "Reporting #%i" gnus-current-article)
                (redisplay-device)
;                (nix-pipe-given-article-somewhere (gnus-summary-article-number) "spamassassin -r" "*Spam Reporting*" nil nil))
                (nix-pipe-given-article-somewhere (gnus-summary-article-number) "sa-learn --spam --no-rebuild --single" "*Spam Reporting*" nil nil))
              (gnus-summary-find-next t nil nil))))))

(defun nix-revoke-spam ()
  "Report that current message is not spam after all.
Only revokes from Razor 2 and the Bayes database, as other mechanisms have
no revocation mechanism and because revocation is not worthwhile for
mechanisms with no trust web."
  (interactive)
  ;(nix-pipe-given-article-somewhere gnus-current-article "spamassassin -d | razor-revoke" "*Spam Reporting*")
  (nix-pipe-given-article-somewhere gnus-current-article "sa-learn --forget --single --no-rebuild" "*Spam Reporting*"))

(defun nix-excise-addresses ()
  "Excise the addresses in the current message from the autowhitelists."
  (interactive)
  (nix-pipe-given-article-somewhere gnus-current-article "spamassassin -R" "*Spam Reporting*"))

;; When we expire or delete mails from groups where SA-scanned mails end up,
;; we should --forget them from the Bayes database too.

(defun nix-select-group-expire-forgets ()
  "When a mail is deleted, SA and other learning systems should forget about it.
This only applies to those groups given in `nix-spam-checked-groups'.

Must be called from the `gnus-select-group-hook'."
  (and (member gnus-newsgroup-name nix-spam-checked-groups)
       (not (gnus-group-find-parameter (or gnus-newsgroup-name "")
                                       'gnus-summary-line-format))
       (gnus-group-add-parameter gnus-newsgroup-name
                                 '(expiry-target 'nix-forget-mail))))

(defun nix-forget-mail (group)
  "Forget the mail in the current message."
  (shell-command-on-region (point-min) (point-max) "sa-learn --forget --single --no-rebuild")
  (and (boundp 'nix-sa-db-rebuild-needed) (setq nix-sa-db-rebuild-needed t))
  (message "Forgotten a message.")
  'delete)

(defadvice gnus-request-expire-articles (around nix-rebuild-after-group-expire-articles activate preactivate)
  "Rebuild the SA Bayes database if expiry took place."
  (let ((nix-sa-db-rebuild-needed))
    ad-do-it
    (if nix-sa-db-rebuild-needed
        (shell-command "sa-learn --rebuild > /dev/null"))))

(add-hook 'gnus-select-group-hook 'nix-select-group-expire-forgets)

(define-key gnus-summary-mode-map (kbd "M $") 'nix-report-spam)
(define-key gnus-summary-mode-map (kbd "M a") 'nix-excise-addresses)
;(define-key gnus-summary-mode-map (kbd "") 'nix-report-spam-all)
(define-key gnus-summary-mode-map (kbd "M M-$") 'nix-revoke-spam)


;;; PGG
(require 'pgg)
;; verify/decrypt only if mml knows about the protocl used
(setq mm-verify-option 'known)
(setq mm-decrypt-option 'known)

;;Here we make button for the multipart
(setq gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed"))

;; Automatically sign when sending mails
;(add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

;; Enough explicit settings
(setq pgg-passphrase-cache-expiry 600)

;; Tells Gnus to inline the part
(eval-after-load "mm-decode"
  '(add-to-list 'mm-inlined-types "application/pgp$"))
;; Tells Gnus how to display the part when it is requested
(eval-after-load "mm-decode"
  '(add-to-list 'mm-inline-media-tests '("application/pgp$"
                                         mm-inline-text identity)))
;; Tell Gnus not to wait for a request, just display the thing
;; straight away.
(eval-after-load "mm-decode"
  '(add-to-list 'mm-automatic-display "application/pgp$"))
;; But don't display the signatures, please.
(eval-after-load "mm-decode"
  (quote (setq mm-automatic-display (remove "application/pgp-signature"
                                            mm-automatic-display))))

(add-hook 'gnus-message-setup-hook 'mml-secure-message-sign-pgpmime)

(require 'gnus-sum)
(require 'nntodo)

(autoload 'bbdb/send-hook "moy-bbdb"
  "Function to be added to `message-send-hook' to notice records when sending messages" t)

(add-hook 'message-send-hook 'bbdb/send-hook)

(autoload 'trivial-cite "tc" t t)

;; (server-start)
;(calendar)
(find-file (concat planner-directory "/" planner-default-page))