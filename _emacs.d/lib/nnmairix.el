;;; nnmairix.el --- Mairix back end for Gnus, the Emacs newsreader

;; Copyright (C) 2007  David Engster

;; Author: David Engster <dengste2@gwdg.de>
;; Keywords: gnus mairix mail searching 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; THIS IS BETA (ALPHA?) SOFTWARE! This back end should not mess up or
;; even delete your mails, but having a backup is always a good idea.

;; This is a back end for using the mairix search engine with
;; Gnus. Mairix is a tool for searching words in locally stored
;; mail. Mairix is very fast which allows using it efficiently for
;; "smart folders", e.g. folders which are associated with search
;; queries. Of course, you can also use this back end just for
;; calling mairix with some search query.
;;
;; Mairix is written by Richard Curnow. More information can be found at
;; http://www.rpcurnow.force9.co.uk/mairix/ 
;;
;; For details about setting up mairix&Gnus&nnmairix.el, look at the
;; emacswiki: 
;;
;; http://www.emacswiki.org/cgi-bin/wiki/GnusMairix
;;
;; The newest version of nnmairix.el can be found at
;; 
;; http://www.emacswiki.org/cgi-bin/emacs/nnmairix.el

;; For impatient people, here's the setup in a nutshell:
;;
;; This back end requires an installed mairix binary which is
;; configured to index your mail folder. You don't have to specify a
;; search folder (but it does no harm, either). Visit the man page of
;; mairix and mairixrc for details.
;;
;; Put nnmairix.el into your search path and "(require 'nnmarix)" into
;; your .gnus. Then call nnmairix-create-default-group (or 'G b
;; c'). This function will ask for all necessary information to create
;; a mairix server in Gnus with the default search folder. This
;; default search folder will be used for all temporary searches: call
;; nnmairix-search ('G b s') and enter a mairix query (like
;; f:test@example.com). To create a mairix group for one specific
;; search query, use 'G b g'. See the emacswiki or the source for more
;; information.

;; Commentary on the code: nnmairix sits between Gnus and the "real"
;; back end which handles the mail (currently nnml, nnimap and
;; nnmaildir were tested). I know this is all a bit hacky, but so far
;; it works for me. This is the first back end I've written for Gnus,
;; so I'd appreciate any comments, suggestions, bug reports (and, of
;; course, patches) for improving nnmairix.

;; nnmairix does not use an active file, since I wanted to contain the
;; back end "inside Gnus" as much as possible without the need of an
;; external file. It stores the query/folder information in the group
;; parameters instead. This also implies that once you kill a mairix
;; group, it's gone for good. I don't think that this is really
;; problematic, since I don't see the need in unsubscribing and
;; re-subscribing search groups

;; Every mairix server is "responsible" for one backend, i.e. you can
;; have several mairix servers for different back ends which call
;; different mairix configurations. Not that I think anyone will
;; actually do this, but I thought it would be a "nice to have
;; feature"...

;; KNOWN BUGS: 
;; * When using Maildir: path and filename of a mail can change due to
;; reading/replying/etc. This can lead to dangling symlinks in
;; nnmairix groups and it depends on the back end how well it deals
;; with that (some IMAP servers out there may not be amused). Update the
;; database ('G b u') and the group to fix it.
;; * Wrong article counts when using nnml.

;; TODO/MISSING FEATURES:
;; * More error checking
;; * Support of more back ends (nnmh, nnfolder, nnmbox...)
;; * Maybe use an active file instead of group parameters?
;; * Use "-a" when updating groups which are not newly created
;; * Jump to original message in back end
;; * Total number of articles
 
;; Change log:
;;
;; 03/10/2007 - version 0.1 - first release


;;; Code:

(require 'nnoo)
(require 'gnus-group)
(require 'gnus-sum)
(require 'message)
(require 'nnml)

(nnoo-declare nnmairix)

;;; === Keymaps

;; Group mode
(defun nnmairix-group-mode-hook ()
  (define-key gnus-group-mode-map
    (kbd "G b") (make-sparse-keymap))
  (define-key gnus-group-mode-map 
    (kbd "G b g") 'nnmairix-create-search-group)
  (define-key gnus-group-mode-map 
    (kbd "G b c") 'nnmairix-create-server-and-default-group)
  (define-key gnus-group-mode-map 
    (kbd "G b q") 'nnmairix-group-change-query-this-group)
  (define-key gnus-group-mode-map 
    (kbd "G b t") 'nnmairix-group-toggle-threads-this-group)
  (define-key gnus-group-mode-map 
    (kbd "G b u") 'nnmairix-update-database)
  (define-key gnus-group-mode-map 
    (kbd "G b s") 'nnmairix-search))

;; Summary mode
(defun nnmairix-summary-mode-hook ()
  (define-key gnus-summary-mode-map
    (kbd "S t") 'nnmairix-search-thread-this-article)
  (define-key gnus-summary-mode-map
    (kbd "S f") 'nnmairix-search-from-this-article)
  (define-key gnus-summary-mode-map
    (kbd "S g") 'nnmairix-create-search-group-from-message))

(add-hook 'gnus-group-mode-hook 'nnmairix-group-mode-hook)
(add-hook 'gnus-summary-mode-hook 'nnmairix-summary-mode-hook)


;; Customizable stuff

(defvar nnmairix-group-prefix "zz_mairix"
  "Prefix for mairix search groups on back end server. nnmairix will
create these groups automatically on the back end server for each
nnmairix search group. The name on the back end server will be this
prefix plus a random number. You can delete unused nnmairix groups on
the back end using nnmairix-purge-old-groups.")

(defvar nnmairix-mairix-output-buffer "*mairix output*"
  "Buffer used for mairix output.")

(defvar nnmairix-mairix-update-options '("-F" "-Q")
  "Options when calling mairix for updating the database. The default
is '-F' and '-Q' for making updates faster. You should call mairix
without these options from time to time (e.g. via cron job).")

(defvar nnmairix-mairix-synchronous-update nil
  "Set this to t if you want emacs to wait for mairix updating the
database. ")

(defvar nnmairix-delete-and-create-on-change '(nnimap nnmaildir)
  "This variable is a list of back ends where the search group should
be completely deleted and re-created when the query or thread
parameter changes. I know this is rather \"brute force\" and maybe
even dangerous (you have backups, right?), but it should be used at
least for nnimap since some IMAP servers are really not amused when
mailbox content changes behind their back. It usually also corrects
the problem of \"holes\" in the article numbers which often lead to a
wrong count of total articles shown by Gnus (it doesn't work with
nnml, though).")

;;; === Server variables

(defvoo nnmairix-backend  nil
  "Backend where mairix stores its searches.")

(defvoo nnmairix-backend-server nil
  "Name of the server where mairix stores its searches.")

(defvoo nnmairix-mairix-command "mairix"
  "Command to call mairix for this nnmairix server.")

(defvoo nnmairix-hidden-folders nil
  "Set this to t if the back end server uses hidden directories for
its maildir mail folders (e.g. the Dovecot IMAP server or mutt).")

(defvoo nnmairix-default-group nil
  "Default search group. This is the group which is used for all
temporary searches, e.g. nnmairix-search.")


;;; === Internal variables

;; Regexp for mairix groups on back end
(setq nnmairix-group-regexp (format "%s-\\(.*\\)-[0-9]+" nnmairix-group-prefix))

;; Back ends (hopefully...) supported by nnmairix. 
;; Other backends might or might not work.
(setq nnmairix-valid-backends '(nnimap nnml nnmaildir))

;; Last chosen server
(setq nnmairix-last-server nil)

;; Maximum article number for each group
(setq nnmairix-max-article-numbers nil)

;; Current server
(setq nnmairix-current-server nil)

;;; === Gnus backend functions
  
(nnoo-define-basics nnmairix)

(gnus-declare-backend "nnmairix" 'mail 'address)

(deffoo nnmairix-open-server (server &optional definitions)
  ;; just set server variables
  (setq nnmairix-current-server server)
  (nnoo-change-server 'nnmairix server definitions))

(deffoo nnmairix-request-group (group &optional server fast)
  ;; Call mairix and request group on back end server
  (when server (nnmairix-open-server server))
  (let* ((qualgroup (if server 
			(gnus-group-prefixed-name group (list 'nnmairix server)) 
		      group))
	 (query (gnus-group-get-parameter qualgroup 'query t))
	(folder (gnus-group-get-parameter qualgroup 'folder))
	(threads (gnus-group-get-parameter qualgroup 'threads))
	(corr (gnus-group-get-parameter qualgroup 'numcorr t))
	(commandsplit (split-string nnmairix-mairix-command))
	(args (cons (car commandsplit) '(nil t nil)))
	rval mfolder ret)
    ;; For maildir++ folders
    (setq mfolder (if (and nnmairix-hidden-folders
			   (not (string-match "^\\." folder)))
		      (concat "." folder)
		    folder))
    (if folder
	(if query
	    (progn 
	      (setq rval
		    (if fast 0
		      (save-excursion
			(set-buffer 
			 (get-buffer-create nnmairix-mairix-output-buffer))
			(erase-buffer)
			(when (> (length commandsplit) 1)
			  (setq args (append args (cdr commandsplit))))
			(when threads
			  (setq args (append args '("-t"))))
			(apply 'call-process 
			       (append args (list "-o" mfolder) query)))))
	      (if (zerop rval)
		  (progn
		    (nnmairix-call-backend 
		     "open-server" nnmairix-backend-server)
		    ;; If we're dealing with nnml, update active file for this group
		    (when (eq nnmairix-backend 'nnml)
		      (nnml-generate-nov-databases-directory 
		       (concat (file-name-as-directory 
				(expand-file-name nnml-directory)) folder)))
		    (nnmairix-call-backend 
		     "request-scan" folder nnmairix-backend-server)
		    (setq ret (nnmairix-call-backend 
			       "request-group" folder nnmairix-backend-server fast))
		    (if (and ret
			     (not fast))
			(save-excursion
			  (set-buffer nnmairix-mairix-output-buffer)
			  (goto-char (point-min))
			  (re-search-forward "^Matched.*messages")
			  (nnheader-message 7 (match-string 0))
			  (set-buffer nntp-server-buffer)
			  (goto-char (point-min))
			  (let ((status (read (current-buffer)))
				(total (read (current-buffer)))
				(low (read (current-buffer)))
				(high (read (current-buffer))))
			    (if (= status 211)
				(progn
				  ;; Article number correction
				  (if (and corr
					   (> (+ (car (cddr corr)) high) 0))
				      (progn
					(when (car corr) ;Group has changed
					  (setq corr 
						(list nil 
						      (car (cddr corr)) 
						      (+ (car (cddr corr)) high)))
					  (gnus-group-set-parameter 
					   qualgroup 'numcorr corr))
					(setq low (+ low (cadr corr))
					      high (+ high (cadr corr))))
				    (when (member nnmairix-backend 
						  nnmairix-delete-and-create-on-change)
				      (gnus-group-set-parameter 
				       qualgroup 'numcorr (list nil 0 high))))
				  (erase-buffer)
				  (insert (format "%d %d %d %d %s" status total low high group)))
			      (progn
				(nnheader-report 
				 'nnmairix "Error calling back end on group %s" folder)
				(setq ret nil)))))))
		(if (and (= rval 1) 
			 (save-excursion (set-buffer nnmairix-mairix-output-buffer)
					 (goto-char (point-min))
					 (looking-at "^Matched 0 messages")))
		    (save-excursion
		      (nnheader-message 5 "Mairix: No matches found.")
		      (set-buffer nntp-server-buffer)
		      (erase-buffer)
		      (insert (concat "211 0 1 0 " group))
		      (setq ret t))
		  (progn
		    (nnheader-report 'nnmairix "Error running marix. See buffer %s for details." nnmairix-mairix-output-buffer)
		    (setq ret nil)))))
	  (progn
	    ;; No query set - empty group
	    (save-excursion
	      (set-buffer nntp-server-buffer)
	      (erase-buffer)
	      (insert (concat "211 0 1 0 " group)) 
	      (setq ret t))))
    ;; Folder parameter not set
      (progn
	(nnheader-report 'nnmairix "Check folder parameter for group %s" group) 
	(setq ret nil)))
    ret))

(deffoo nnmairix-request-create-group (group &optional server args)
  (let ((qualgroup (if server (gnus-group-prefixed-name group (list 'nnmairix server)) 
		     group))
	(exist t)
	(count 0)
	groupname info)
    (when server (nnmairix-open-server server))
    (gnus-group-add-parameter qualgroup '(query . nil))
    (gnus-group-add-parameter qualgroup '(threads . nil))
    (while exist
      (setq count (1+ count))
      (setq groupname (format "%s-%s-%s" nnmairix-group-prefix group 
			      (number-to-string count)))
      (setq exist (nnmairix-call-backend 
		   "request-group" groupname nnmairix-backend-server)))
    (nnmairix-call-backend 
     "request-create-group" groupname nnmairix-backend-server)
    (gnus-group-add-parameter qualgroup '(folder . nil))
    (gnus-group-set-parameter qualgroup 'folder groupname))
  t)


(deffoo nnmairix-retrieve-headers (articles group &optional server fetch-old)
  (when server (nnmairix-open-server server))
  (let* ((folder (nnmairix-get-backend-folder group server))
	 (corr (nnmairix-get-numcorr group server))
	 (numcorr 0)
	 rval)
    (when (and corr
	       (not (zerop (cadr corr)))
	       (numberp (car articles)))
      (setq numcorr (cadr corr))
      (setq articles 
	    (mapcar 
	     (lambda (arg) (- arg numcorr)) 
	     articles)))
    (setq rval (nnmairix-call-backend 
		"retrieve-headers" articles folder nnmairix-backend-server fetch-old))
    (when (eq rval 'nov)
      (nnmairix-replace-group-and-numbers articles folder group numcorr)
      rval)))

(deffoo nnmairix-request-article (article &optional group server to-buffer)
  (when server (nnmairix-open-server server))
  (let ((folder (nnmairix-get-backend-folder group server))
	(corr (nnmairix-get-numcorr group server)))
    (when (and 
	   (numberp article)
	   corr
	   (not (zerop (cadr corr))))
      (setq article (- article (cadr corr))))
    (nnmairix-call-backend 
     "request-article" article folder nnmairix-backend-server to-buffer))
  t)

(deffoo nnmairix-close-group (group &optional server)
  ;; Should we do something here?
  nil)


(deffoo nnmairix-request-list (&optional server)
  (when server (nnmairix-open-server server))
  (if (nnmairix-call-backend "request-list" nnmairix-backend-server)
      (let (cpoint cur qualgroup folder)
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (goto-char (point-min))
	  (setq cpoint (point))
	  (while (re-search-forward nnmairix-group-regexp (point-max) t)
	    (setq cur (match-string 1)
		  qualgroup (gnus-group-prefixed-name cur 
						      (list 'nnmairix server)))
	    (if (and (gnus-group-entry qualgroup)
		     (string= (match-string 0) 
			      (gnus-group-get-parameter qualgroup 'folder)))
		(progn
		  (replace-match cur)
		  (delete-region cpoint (point-at-bol))
		  (forward-line)
		  (setq cpoint (point)))
	      (forward-line)))
	  (delete-region cpoint (point-max)))
	t)
    nil))

	         
(nnoo-define-skeleton nnmairix)


;;; === Interactive functions


(defun nnmairix-create-search-group (server group query threads)
  "Creates nnmairix search group GROUP on SERVER with QUERY. If
THREADS is t, include whole threads from found messages. If called
interactively, user will be asked for parameters."
  (interactive 
   (list 
    (gnus-server-to-method (car (nnmairix-get-server)))
    (read-string "Group name: ")
    (read-string "Query: ")
    (y-or-n-p "Include threads? ")))
  (when (and (stringp query) 
	     (string-match "[\\s-]" query))
    (setq query (split-string query)))
  (when (and server group query)
    (save-excursion
      (let ((groupname (gnus-group-prefixed-name group server))
	    info)
	(set-buffer gnus-group-buffer)
	(gnus-group-make-group group server)
	(gnus-group-set-parameter groupname 'query  query)
	(gnus-group-set-parameter groupname 'threads threads)
	(nnmairix-update-and-clear-marks groupname)))))

(defun nnmairix-create-search-group-from-message ()
  "Interactively creates a new search group with query based on the
current message."
  (interactive)
  (let ((char-header 
	 '((?f "from" "f" "From")
	   (?t "to" "t" "To")
	   (?c "cc" "c" "Cc")
	   (?a "to" "tc" "To or Cc")
	   (?s "subject" "s" "subject")
	   (?b nil "b" "body")
	   (?m "Message-ID" "m" "message ID")))
	 (server (nnmairix-backend-to-server gnus-current-select-method))
	 query achar header finished group threads cq)
    (when (or (not (gnus-buffer-live-p gnus-article-buffer))
	      (not (gnus-buffer-live-p gnus-summary-buffer)))
      (error "No article or summary buffer."))
    (when (not server)
      (error "No nnmairix server found for back end %s:%s" 
	     (symbol-name (car gnus-current-select-method))
	     (nth 1 gnus-current-select-method)))
    (while (not finished)
      (save-excursion
	(gnus-summary-toggle-header 1)
	(while (not achar)
	  (message "Query (f=From,t=To,c=Cc,a=To or Cc,s=subject,b=body,m=Msgid): ")
	  (setq achar (read-char))
	  (when (not (assoc achar char-header))
	    (setq achar nil)))
	(set-buffer gnus-article-buffer)
	(setq header nil)
	(when (setq cq (nth 1 (assoc achar char-header))) 
	  (setq header (gnus-fetch-field (nth 1 (assoc achar char-header)))))
	(if (> emacs-major-version 20)
	    (while (string-match "[^-.@& [:alnum:]]" header)
	      (setq header (replace-match "" t t header)))
	  (while (string-match "[[]{}:<>]" header)
	    (setq header (replace-match "" t t header))))
	(while (string-match "[-& ]" header)
	  (setq header (replace-match "," t t header)))
	(setq header (read-string 
		      (concat "Match " (nth 3 (assoc achar char-header)) " on: ") 
		      header))
	(push  (concat (nth 2 (assoc achar char-header)) ":" header) query)
	(setq finished (not (y-or-n-p "Add another search query? "))
	      achar nil)))
    (setq threads (y-or-n-p "Include whole threads? "))
    (setq group (read-string "Group name: "))
    (set-buffer gnus-summary-buffer)
    (message "Creating group %s on server %s with query %s." group 
	     (gnus-method-to-server server) (mapconcat 'identity query " "))
    (nnmairix-create-search-group server group query threads)))

(defun nnmairix-create-server-and-default-group ()
  "Interactively creates a new nnmairix server with default search
group. The function will ask the user for all necessary information."
  (interactive)
  (let* ((name (read-string "Name of the mairix server: "))
	(server (completing-read "Back end server (TAB for completion): " 
				 (nnmairix-get-valid-servers)))
	(mairix (read-string "Command to call mairix: " "mairix"))
	(defaultgroup (read-string "Default search group: "))
	(backend (substring server 0 (string-match ":" server)))
	(servername (substring server (match-end 0)))
	(hidden (and (string-match "^nn\\(imap\\|maildir\\)$" backend)
		     (y-or-n-p 
		      "Does the back end server work with maildir++ (i.e. hidden directories)? ")))
	create)

    (apply (intern (format "%s-%s" backend "open-server")) (list servername)) 

    (when (and hidden
	       (string-match "^\\." defaultgroup))
      (setq defaultgroup (substring defaultgroup 1)))
    ;; Create default search group
    (gnus-group-make-group 
     defaultgroup (list 'nnmairix name  (list 'nnmairix-backend (intern backend)) 
			(list 'nnmairix-backend-server servername) 
			(list 'nnmairix-mairix-command mairix)
			(list 'nnmairix-hidden-folders hidden) 
			(list 'nnmairix-default-group defaultgroup)))))


(defun nnmairix-group-change-query-this-group (&optional query)
  "Changes search query for GROUP by replacing existing search query
with QUERY for group under cursor."
  (interactive)
  (let* ((group (gnus-group-group-name))
	 (method (gnus-find-method-for-group group))
	 (oldquery (gnus-group-get-parameter group 'query t)))
    (if (eq (car method) 'nnmairix)
	(progn
	  (when (listp oldquery)
	    (setq oldquery (mapconcat 'identity oldquery " ")))
	  (setq query (or query 
			  (read-string "New query: " oldquery)))
	  (when (stringp query)
	    (setq query (split-string query)))
	  (when query
	    (gnus-group-set-parameter group 'query query)
	    (nnmairix-update-and-clear-marks group)))
      (error "This is no nnmairix group"))))
		  

(defun nnmairix-group-toggle-threads-this-group (&optional threads)
  "Toggle threads parameter for this group.
If THREADS is a positive number, set threads parameter to t.
If THREADS is a negative number, set it to nil."
  (interactive)
  (let* ((group (gnus-group-group-name))
	 (method (gnus-find-method-for-group group))
	 (getthreads (or threads 
			(not (gnus-group-get-parameter group 'threads)))))
    (if (eq (car method) 'nnmairix)
	(progn
	  (when (numberp getthreads)
	    (setq getthreads (> getthreads 0)))
	  (gnus-group-set-parameter group 'threads getthreads)
	  (if getthreads
	      (message "Threads activated for group %s" group)
	    (message "Threads deacitavted for group %s" group))
	  (nnmairix-update-and-clear-marks group))
      (error "This is no nnmairix group"))))
        

(defun nnmairix-search (query &optional server threads)
  "Sends QUERY to mairix on SERVER with default search group
and enters that group, showing the results.
If THREADS is t, enable threads. 
If THREADS is a negative number, disable threads.
Otherwise, leave threads parameter as it is."
  (interactive (list (read-string "Query: ")))
  (when (not server)
    (setq server (car (nnmairix-get-server))))
  (if (not server)
      (error "No opened nnmairix server found")
    (setq server (gnus-server-to-method server)))
  (nnmairix-open-server (nth 1 server))
  (let* ((qualgroup (gnus-group-prefixed-name nnmairix-default-group 
					      (list 'nnmairix (nth 1 server))))) 
    (set-buffer gnus-group-buffer)
    (when (stringp query) 
      (setq query (split-string query)))
    (gnus-group-set-parameter qualgroup 'query query)
    (if (symbolp threads) 
	(when (eq threads 't)
	  (gnus-group-set-parameter qualgroup 'threads t))
      (when (< threads 0)
	(gnus-group-set-parameter qualgroup 'threads nil)))
    (nnmairix-update-and-clear-marks qualgroup)
    (when (not (zerop (gnus-group-unread qualgroup)))
      (gnus-group-read-group nil t qualgroup))))

(defun nnmairix-search-thread-this-article ()
  "Searches thread for the current article shown in the article
buffer. This is effectively a shortcut for calling nnmairix-search
with m:msgid of the current article and enabled threads."
  (interactive)
  (let* ((server 
	  (nnmairix-backend-to-server gnus-current-select-method))
	 mid)
    (if server
	(if (gnus-buffer-live-p gnus-article-buffer)
	    (progn
	      (save-excursion
		(set-buffer gnus-article-buffer)
		(gnus-summary-toggle-header 1)
		(setq mid (message-fetch-field "Message-ID")))
	      (while (string-match "[<>]" mid)
		(setq mid (replace-match "" t t mid)))
	      (nnmairix-search (concat "m:" mid) server t))
	  (message "No article buffer."))
      (error "No nnmairix server found for back end %s:%s" 
	     (symbol-name (car gnus-current-select-method))
	     (nth 1 gnus-current-select-method)))))

(defun nnmairix-search-from-this-article ()
  "Searches messages from sender of the current article. This is
effectively a shortcut for calling nnmairix-search with f:current_from."
  (interactive)
  (let* ((server 
	  (nnmairix-backend-to-server gnus-current-select-method))
	 from)
    (if server    
	(if (gnus-buffer-live-p gnus-article-buffer)
	    (progn
	      (save-excursion
		(set-buffer gnus-article-buffer)
		(gnus-summary-toggle-header 1)
		(setq from (cadr (gnus-extract-address-components 
				  (gnus-fetch-field "From"))))
		(nnmairix-search (concat "f:" from) server -1)))
	  (message "No article buffer."))
      (error "No nnmairix server found for back end %s:%s" 
	     (symbol-name (car gnus-current-select-method))
	     (nth 1 gnus-current-select-method)))))


(defun nnmairix-purge-old-groups (&optional dontask server)
  "Deletes old mairix search groups on the back end which are no
longer used. You may want to call this from time to time if you are
creating and deleting lots of nnmairix groups. If DONTASK is t, do not
ask before deleting a group on the back end (highly discouraged! - this
is beta software after all...)."
  (interactive)
  (let ((server (or server
		    (gnus-server-to-method (car (nnmairix-get-server))))))
    (if (nnmairix-open-server (nth 1 server))
	(when (nnmairix-call-backend 
	       "request-list" nnmairix-backend-server)
	  (let (cur qualgroup folder)
	    (save-excursion
	      (set-buffer nntp-server-buffer)
	      (goto-char (point-min))
	      (while (re-search-forward nnmairix-group-regexp (point-max) t)
		(setq cur (match-string 0)
		      qualgroup (gnus-group-prefixed-name 
				 (match-string 1) server))
		(when (not (and (gnus-group-entry qualgroup)
				(string= cur 
					 (gnus-group-get-parameter 
					  qualgroup 'folder))))
		  (when (or dontask
			    (y-or-n-p 
			     (concat "Delete group " cur 
				     " on server " nnmairix-backend-server "? ")))
		    (nnmairix-call-backend 
		     "request-delete-group" cur t nnmairix-backend-server)))))))
      (message "Couldn't open server %s" (nth 1 server)))))


(defun nnmairix-update-database (&optional servers)
  "Calls mairix for updating the database for SERVERS. If SERVERS is
nil, do update for all nnmairix servers.  
Mairix will be called asynchronously unless
nnmairix-mairix-synchronous-update is t.
Mairix will be called with nnmairix-mairix-update-options."
  (interactive)
  (let ((servers (or servers
		     (nnmairix-get-nnmairix-servers)))
	args cur commandsplit)
    (while servers
      (setq cur (car (pop servers)))
      (nnmairix-open-server 
       (nth 1 (gnus-server-to-method cur)))
      (setq commandsplit (split-string nnmairix-mairix-command))
      (nnheader-message 7 "Updating mairix database for %s..." cur)
      (if nnmairix-mairix-synchronous-update
	  (progn
	    (setq args (append (list (car commandsplit) nil 
				     (get-buffer nnmairix-mairix-output-buffer) 
				     nil)))
	    (if (> (length commandsplit 1))
		(setq args (append args (cdr commandsplit) nnmairix-mairix-update-options))
	      (setq args (append args nnmairix-mairix-update-options)))
	    (apply 'call-process args)
	    (nnheader-message 7 "Updating mairix database for %s... done" cur))
	(progn
	  (setq args (append (list cur (get-buffer nnmairix-mairix-output-buffer) 
				   (car commandsplit))))
	  (if (> (length commandsplit) 1)
	      (setq args (append args (cdr commandsplit) nnmairix-mairix-update-options))
	    (setq args (append args nnmairix-mairix-update-options)))
	  (set-process-sentinel (apply 'start-process args)
				'nnmairix-sentinel-mairix-update-finished))))))


;;; ==== Helper functions


(defun nnmairix-get-server ()
  "If there exists just one nnmairix server, return its
value. Otherwise, ask user for server."
  (let ((openedserver (nnmairix-get-nnmairix-servers)))
    (when (not openedserver)
      (error "No opened nnmairix server found"))
    (if (> (length openedserver) 1)
	(progn
	  (while 
	      (equal '("") 
		  (setq nnmairix-last-server 
			(list (completing-read "Server: " openedserver nil 1 
					       (or nnmairix-last-server
						   "nnmairix:"))))))
	  nnmairix-last-server)
      (car openedserver))))

(defun nnmairix-get-nnmairix-servers ()
  "Return all opened nnmairix servers."
  (let ((alist gnus-opened-servers)
	server openedserver)
    (while alist
      (setq server (pop alist))
      (when (and server 
		 (eq (cadr server) 'ok)
		 (eq (caar server) 'nnmairix)
		 (not (member (car server) gnus-ephemeral-servers)))
	(setq server 
	      (concat (symbol-name (caar server)) ":" (nth 1 (car server))))
	(push (list server) openedserver)))
    openedserver))


;; Get list of valid backend servers for nnmairix groups
(defun nnmairix-get-valid-servers ()
  (let ((alist gnus-opened-servers)
	server openedserver)
    (while alist
      (setq server (pop alist))
      (when (and server 
		 (eq (cadr server) 'ok)
		 (member (caar server) nnmairix-valid-backends)
		 (not (member (car server) gnus-ephemeral-servers))
		 (not (nnmairix-backend-to-server (car server))))
	(setq server 
	      (concat (symbol-name (caar server)) ":" (nth 1 (car server))))
	(push (list server) openedserver))) 
  openedserver))

;; Call backend function
(defun nnmairix-call-backend (func &rest args)
  (apply (intern (format "%s-%s" (symbol-name nnmairix-backend) func)) args)) 

;; Get back end folder from nnmairix group
(defun nnmairix-get-backend-folder (group &optional server)
  (let* ((qualgroup (if server 
			(gnus-group-prefixed-name group (list 'nnmairix server)) 
		      group))
	 (folder (gnus-group-get-parameter qualgroup 'folder)))
    folder))

(defun nnmairix-get-numcorr (group &optional server)
  (let* ((qualgroup (if server 
			(gnus-group-prefixed-name group (list 'nnmairix server)) 
		      group))
	 (corr (gnus-group-get-parameter qualgroup 'numcorr t)))
    corr))


;; replace folder names in Xref header
;; and correct article numbers if necessary
(defun nnmairix-replace-group-and-numbers (articles backendgroup mairixgroup numc)
  (let ((buf (get-buffer-create " *nnmairix buffer*"))
	(corr (not (zerop numc)))
	(name (buffer-name nntp-server-buffer))
	header cur xref)
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (set-buffer nntp-server-buffer)
      (goto-char (point-min))
      (nnheader-message 7 "nnmairix: Rewriting headers...") 
      (mapcar 
       (function 
	(lambda (article)
	  (when (or (looking-at (number-to-string article))
		    (nnheader-find-nov-line article))
	    (setq cur (nnheader-parse-nov))
	    (when corr
		  (setq article (+ (mail-header-number cur) numc))
		  (mail-header-set-number cur article))
	    (setq xref (replace-regexp-in-string 
			(format "[ \t]%s:[0-9]+" backendgroup) 
			(format " %s:%d" mairixgroup article)
			(mail-header-xref cur) t t))
	    (mail-header-set-xref cur xref)
	    (set-buffer buf)			    
	    (nnheader-insert-nov cur)
	    (set-buffer nntp-server-buffer)
	    (when (not (eobp)) 
	      (forward-line 1)))))
       articles)
      (nnheader-message 7 "nnmairix: Rewriting headers... done") 
      (kill-buffer nntp-server-buffer)
      (set-buffer buf)
      (rename-buffer name)
      (setq nntp-server-buffer buf))))

(defun nnmairix-backend-to-server (server)
  "Return nnmairix server responsible for back end SERVER."
  (let ((allservers (nnmairix-get-nnmairix-servers))
	mairixserver found)
    (while (and allservers (not found))
	(setq mairixserver (gnus-server-to-method (car (pop allservers))))
	(setq found (and (eq (cadr (assoc 'nnmairix-backend mairixserver)) 
			     (car server))
			 (string= (cadr (assoc 'nnmairix-backend-server mairixserver)) 
				  (nth 1 server)))))
      (if found mairixserver nil)))

;; Update group and clear all marks
(defun nnmairix-update-and-clear-marks (group &optional method)
  (when method
    (setq group (gnus-group-prefixed-name group method)))
  (let ((method (or method
		    (gnus-find-method-for-group group)))
	(folder (gnus-group-get-parameter group 'folder))
	(corr (gnus-group-get-parameter group 'numcorr t))
	info)
    (if (eq (nth 0 method) 'nnmairix)
	(save-excursion
	  (set-buffer gnus-group-buffer)
	  (setq info (gnus-get-info group))
	  ;; Clear active and info
	  (gnus-set-active group nil)
	  (gnus-info-clear-data info)
	  ;; Delete and re-create group if needed
	  (when (member nnmairix-backend nnmairix-delete-and-create-on-change)
	    (if (string-match nnmairix-group-regexp folder)
		(progn
		  (nnmairix-call-backend "open-server" nnmairix-backend-server)
		  (nnmairix-call-backend "request-delete-group" folder t nnmairix-backend-server)
		  (nnmairix-call-backend "request-create-group" folder nnmairix-backend-server)
		  ;; set flag that group has changed for article number correction
		  (when corr
		    (setcar corr t)
		    (gnus-group-set-parameter group 'numcorr corr)))
	      (error "nnmairix-update-and-clear-marks - delete/create with non-mairix group!! - check folder parameter")))
	  (when (gnus-group-jump-to-group group)
	    (gnus-group-get-new-news-this-group)))
      (error "nnmairix-update-and-clear-marks - Called with non-nnmairix group"))))
  
;; Sentinel for mairix update process
(defun nnmairix-sentinel-mairix-update-finished (proc status)
  (if (equal status "finished\n")
      (nnheader-message 7 "Updating mairix database for %s... done" proc)
    (error "There was an error updating the mairix database for server %s. \
See %s for details" proc nnmairix-mairix-output-buffer)))

(provide 'nnmairix)

;; nnmairix.el ends here
