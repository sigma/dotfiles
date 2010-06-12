;;
;; dot-gnus-sa.el --- SpamAssassin-related stuff for Gnus
;;

;; Show the SpamAssassin score of mails in mail groups

(defvar nix-spam-checked-groups '("nnml:Mailbox"
                                  "nnml:blockbox"
                                  "nnml:spambox")
  "The set of groups in which spam-checked mails may be placed.")

(setq gnus-extra-headers '(X-Spam-Status)
      nnmail-extra-headers gnus-extra-headers)

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

(add-hook 'gnus-select-group-hook 'nix-select-group-show-spam-score)

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
  (nix-pipe-given-article-somewhere gnus-current-article "spamassassin -r" "*Spam Reporting*" nil t))

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
                (nix-pipe-given-article-somewhere (gnus-summary-article-number) "spamassassin -r" "*Spam Reporting*" nil nil))
              (gnus-summary-find-next t nil nil))))))

(defun nix-revoke-spam ()
  "Report that current message is not spam after all.
Only revokes from Razor 2 and the Bayes database, as other mechanisms have
no revocation mechanism and because revocation is not worthwhile for
mechanisms with no trust web."
  (interactive)
  (nix-pipe-given-article-somewhere gnus-current-article "spamassassin -d | razor-revoke" "*Spam Reporting*")
  (nix-pipe-given-article-somewhere gnus-current-article "sa-learn --forget --single --rebuild >/dev/null" "*Spam Reporting*"))

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

(define-key gnus-summary-mode-map (kbd "A-s r") 'nix-report-spam)
(define-key gnus-summary-mode-map (kbd "A-s w") 'nix-excise-addresses)
(define-key gnus-summary-mode-map (kbd "<XF86Mail>") 'nix-report-spam)
(define-key gnus-summary-mode-map (kbd "M-<XF86Mail>") 'nix-report-spam-all)
(define-key gnus-summary-mode-map (kbd "A-<XF86Mail>") 'nix-revoke-spam)

(provide 'dot-gnus-sa)