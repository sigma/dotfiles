;;; My configuration file for johnw/resolve's excellent emacs-wiki.el
;;; Sacha Chua <sacha@free.net.ph>

(require 'font-lock)
(global-font-lock-mode t)
(request 'emacs-wiki)

;; Can't do lazy font lock support - <lisp> tags won't get interpreted properly.
;(unless (listp font-lock-support-mode)
;  (setq font-lock-support-mode (list (cons t font-lock-support-mode))))
;(add-to-list 'font-lock-support-mode '(emacs-wiki-mode . nil))

;;;_+ Setting up details

;; We use Damien Elmes' excellent stylesheet, available at
;; http://www.repose.cx/core.css
(setq emacs-wiki-style-sheet "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\">")
(setq emacs-wiki-maintainer "mailto:Yann.Hodique@lifl.fr")
(setq emacs-wiki-publishing-directory "~/public_html")

;;;_+ Automatically publish files upon saving
(defun sacha/emacs-wiki-auto-publish ()
  (when (derived-mode-p 'emacs-wiki-mode)
    (unless emacs-wiki-publishing-p
      (let ((emacs-wiki-publishing-p t)
            (emacs-wiki-after-wiki-publish-hook nil))
	(emacs-wiki-publish-this-page)))))
(add-hook 'emacs-wiki-mode-hook
          (lambda () (add-hook 'after-save-hook
                               'sacha/emacs-wiki-auto-publish nil t)))

;;;_+ Misc
(setq emacs-wiki-file-ignore-regexp
  "index.html\\|\\`\\(\\.?#.*\\|.*,v\\|.*~\\|\\.\\.?\\)\\'")
(setq emacs-wiki-interwiki-names (quote (("CS21ASum03" . (lambda (tag)
                                                           (emacs-wiki-project-interwiki-link "CS21ASum03" tag)))
                                         ("CS21A_IF" . (lambda (tag) (emacs-wiki-project-interwiki-link "CS21A_IF" tag)))
                                         ("WikiPlanner" . (lambda (tag) (emacs-wiki-project-interwiki-link "WikiPlanner" tag)))
                                         ("WikiPedia" . "http://www.wikipedia.org/wiki/")
                                         ("MeatballWiki" . "http://www.usemod.com/cgi-bin/mb.pl?"))))
(setq emacs-wiki-charset-default "utf-8")

(setq emacs-wiki-table-attributes "border=\"0\" cellpadding=\"2\" cellspacing=0")

;;;_+ Custom tags: <answer>
(add-to-list 'emacs-wiki-markup-tags '("answer" t t nil emacs-wiki-answer-tag))
(defun emacs-wiki-answer-tag (beg end highlight-p &optional attrs)
  (interactive)
  (unless highlight-p
    (set (make-variable-buffer-local 'answer-id)
         (if answer-id (+ answer-id 1) 0))
    (insert "<div style=\"background: black\">")
    (when (< (point) end)
      (goto-char end))
    (insert "</div>")))

;;;_+ <grin>, <laugh>, <smile>

(add-to-list 'emacs-wiki-markup-tags '("grin" nil nil nil emacs-wiki-grin-tag))
(add-to-list 'emacs-wiki-markup-tags '("laugh" nil nil nil emacs-wiki-laugh-tag))
(add-to-list 'emacs-wiki-markup-tags '("smile" nil nil nil emacs-wiki-smile-tag))

(defun emacs-wiki-grin-tag (beg end) (insert "&lt;grin&gt;"))
(defun emacs-wiki-laugh-tag (beg end) (insert "&lt;laugh&gt;"))
(defun emacs-wiki-smile-tag (beg end) (insert "&lt;smile&gt;"))

;;;_+ Local file links should be transformed to relative file links if possible

(defvar sacha/emacs-wiki-use-absolute-url-flag nil
  "Non-nil means publish absolute URLs.")

;;;_+ <contents> should strip all the tags

(defun sacha/emacs-wiki-strip-tags (string)
  (while (string-match "<.*?>" string)
    (setq string (replace-match "" nil t string)))
  string)
(defadvice emacs-wiki-contents-tag (around sacha activate)
  (let* ((beg (ad-get-arg 0))
         (end (ad-get-arg 1))
         (attrs (ad-get-arg 2))
         (max-depth (let ((depth (cdr (assoc "depth" attrs))))
                      (or (and depth (string-to-int depth)) 3)))
         (index 1)
         base contents l)
    (save-excursion
      (catch 'done
	(while (re-search-forward "^\\(\\*+\\)\\s-+\\(.+\\)" nil t)
	  (setq l (length (match-string 1)))
	  (if (null base)
	      (setq base l)
	    (if (< l base)
		(throw 'done t)))
	  (when (<= l max-depth)
	    (setq contents (cons (cons l (match-string-no-properties 2))
				 contents))
	    (goto-char (match-beginning 2))
	    (emacs-wiki-insert-anchor (concat "sec" (int-to-string index)))
	    (setq index (1+ index))))))
    (setq index 1 contents (reverse contents))
    (let ((depth 1) (sub-open 0) (p (point)))
      (insert "<dl class=\"contents\">\n")
      (while contents
	(insert "<dt class=\"contents\">\n")
	(insert "<a href=\"#sec" (int-to-string index) "\">"
                (sacha/emacs-wiki-strip-tags (cdar contents))
                "</a>\n")
	(setq index (1+ index))
	(insert "</dt>\n")
	(setq depth (caar contents)
	      contents (cdr contents))
	(if contents
	    (cond
	     ((< (caar contents) depth)
	      (let ((idx (caar contents)))
		(while (< idx depth)
		  (insert "</dl>\n</dd>\n")
		  (setq sub-open (1- sub-open)
			idx (1+ idx)))))
	     ((> (caar contents) depth)	; can't jump more than one ahead
	      (insert "<dd>\n<dl class=\"contents\">\n")
	      (setq sub-open (1+ sub-open))))))
      (while (> sub-open 0)
	(insert "</dl>\n</dd>\n")
	(setq sub-open (1- sub-open)))
      (insert "</dl>\n")
      (put-text-property p (point) 'read-only t))))

;;;_+ I want #top links after each heading! =)
(defun sacha/emacs-wiki-markup-heading ()
  "Add #top links after each heading."
  (let ((len (1+ (length (match-string 1)))))
    (emacs-wiki-surround-text
     (format "<h%d> <span class=\"toplink\"><a href=\"#top\">top</a> - <a href=\"#feedback\">feedback</a></span> " len)
     (format "</h%d>" len)
     'end-of-line)
    ""))
(defalias 'emacs-wiki-markup-heading 'sacha/emacs-wiki-markup-heading)

;;;_+ Other windows

(defun sacha/emacs-wiki-visit-link-other-window ()
  "Visit the link at point, or insert a newline if none."
  (interactive)
  (if (emacs-wiki-link-at-point)
      (let ((str (match-string 0)))
        (other-window 1)
	(emacs-wiki-visit-link str))
    (error "There is no valid link at point")))

(defun sacha/htmlfontify-insert-region (buffer begin end)
  "Insert into BUFFER the htmlified text between BEGIN and END."
  (save-excursion
    (let* ((hfy-optimisations (cons 'skip-refontification hfy-optimisations))
	   (input-text (buffer-substring begin end))
	   (temp-file (make-temp-file "html-input"))
	   output-buffer)
      (with-temp-buffer
	(insert input-text)
	(setq buffer-file-name temp-file)
	(save-excursion (setq output-buffer (htmlfontify-buffer nil nil)))
	(set-buffer-modified-p nil))
      (unwind-protect
	  (let (b e yanked-output)
	    (set-buffer output-buffer)
	    (goto-char (point-min))
	    (search-forward "<pre>\n")
	    (setq b (line-beginning-position))
	    (goto-char (point-max))
	    (search-backward "</pre>")
	    (forward-line -1)
	    (setq e (line-beginning-position))
	    (setq yanked-output (buffer-substring-no-properties b e))
	    (set-buffer buffer)
	    (insert yanked-output))
	(set-buffer output-buffer)
	(set-buffer-modified-p nil)
	(delete-file temp-file)
	(kill-buffer output-buffer)))))

(defun sacha/emacs-wiki-example-tag (beg end attrs highlight-p)
  "Mark up text as an example with optional font-locking."
  (if highlight-p
      (progn
        (remove-text-properties
         beg end '(face nil font-lock-multiline nil
                        invisible nil intangible nil display nil
                        mouse-face nil keymap nil help-echo nil))
        (goto-char end))
    ;; I don't know what would happen if you don't have
    ;; htmlfontify. I guess if you are installing this you
    ;; should have it...
    (let ((end-marker (set-marker (make-marker) (1+ end))))
      (save-restriction
	(narrow-to-region beg end)
	(let* ((mode (cdr (assoc "mode" attrs)))
	       (start (progn (forward-line) (point)))
	       (stop (progn (goto-char end) (beginning-of-line) (point)))
	       (text (buffer-substring-no-properties start stop))
	       (buffer (current-buffer)))
	  (delete-region beg end)
	  (with-temp-buffer
	    (insert text)
	    (when (and mode (and (stringp mode) (functionp (intern mode))))
	      (funcall (intern mode))
	      (font-lock-fontify-buffer))
	    (sacha/htmlfontify-insert-region buffer (point-min) (point-max)))
	  (goto-char (point-min))
	  (insert "<pre class=\"example\">\n")
	  (goto-char (point-max))
	  (insert "</pre>\n")
	  (add-text-properties (point-min) (point-max)
			       '(rear-nonsticky (read-only) read-only t))))
      (goto-char end-marker))))

(unless (featurep 'xemacs)
  (add-to-list 'emacs-wiki-markup-tags '("example" t t t sacha/emacs-wiki-example-tag) t))

;;;_+ Do not treat = as special

(emacs-wiki-configure-highlighting 'emacs-wiki-highlight-markup
      (delete '("=[^\t =]" ?= emacs-wiki-highlight-verbatim) emacs-wiki-highlight-markup))

(defadvice emacs-wiki-highlight-verbatim-tag (around sacha activate)
  "Do not do verbatim at all.")

(defadvice emacs-wiki-markup-word (around sacha activate)
  "Do not treat = as special."
  (let* ((beg (match-beginning 2))
         (end (1- (match-end 2)))
         (leader (buffer-substring-no-properties beg end))
         open-tag close-tag mark-read-only loc multi-line)
    (cond
     ((string= leader "_")
      (setq open-tag "<u>" close-tag "</u>"))
     (t
      (setq multi-line t)
      (let ((l (length leader)))
        (cond
         ((= l 1) (setq open-tag "<em>" close-tag "</em>"))
         ((= l 2) (setq open-tag "<strong>" close-tag "</strong>"))
         ((= l 3) (setq open-tag "<strong><em>"
                        close-tag "</em></strong>"))))))
    (if (and (setq loc (search-forward leader nil t))
             (eq 0 (skip-syntax-forward "w" (1+ loc)))
             (or multi-line (= 1 (count-lines beg loc))))
        (progn
          (replace-match "")
          (insert close-tag)
          (save-excursion
            (goto-char beg)
            (delete-region beg end)
            (insert open-tag))
          (if mark-read-only
              (add-text-properties beg (point)
                                   '(rear-nonsticky (read-only) read-only
                                   t))))
      (backward-char))
    nil))

;;;_ + External viewers

(defun sacha/emacs-wiki-perform-operation-on-link-at-point (command)
  "Run a shell command on the link at point."
  (interactive
   (list
    (read-string
     "Command: "
     (when (emacs-wiki-link-at-point)
       (concat
        (shell-quote-argument
         (emacs-wiki-wiki-link-target (match-string-no-properties 0)))
        " &")))))
  (with-temp-buffer
    (cd planner-directory)
    (shell-command command)))

(defun sacha/emacs-wiki-get-base-path ()
  "Base path."
  (cond ((string-match "CS139" (buffer-name))
         "../school/2004-sem1/cs139.3")
        ((string-match "CS21" (buffer-name))
         "../school/2004-sem1/cs21a")
        ((string-match "CS110" (buffer-name))
         "../school/2004-sem1/cs110")))

(defun sacha/emacs-wiki-insert-document (document title)
  "Insert document links."
  (interactive "MDocument:
MTitle: ")
  (let ((base (sacha/emacs-wiki-get-base-path)))
    (insert
     (mapconcat
      (lambda (extension)
        (format
         "[[%s/%s.%s][%s (%s)]]"
         base
         document
         (downcase extension)
         title
         (upcase extension)))
      (list "PDF" "SXW" "DOC")
      " - "))))

(defun sacha/emacs-wiki-insert-presentation (document title)
  "Insert document links."
  (interactive "MDocument:
MTitle: ")
  (let ((base (sacha/emacs-wiki-get-base-path)))
    (insert
     (mapconcat
      (lambda (extension)
        (format
         "[[%s/%s.%s][%s (%s)]]"
         base
         document
         (downcase extension)
         title
         (upcase extension)))
      (list "PDF" "SXI" "PPT")
      " - ")
     (format
      " - [[%s/%s-handouts.pdf][%s (Handouts, PDF)]]"
      base
      document
      title))))

(define-key emacs-wiki-mode-map (kbd "C-c !")
  'sacha/emacs-wiki-perform-operation-on-link-at-point)

;; Manually refresh the alist whenever a file is saved.
(setq emacs-wiki-refresh-file-alist-p nil)

(provide 'emacs-wiki-config)

