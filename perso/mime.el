;;; mime.el --- Simple MIME Composer for Emacs

;; Copyright (C) 1993-1995  Masanobu UMEDA

;; Author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
;; Version: $Header: /cvsroot/OLD/dotemacs/dotemacs/perso/mime.el,v 1.1 2004/06/15 16:02:05 sigma Exp $
;; Keywords: mail, news, mime

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This is an Emacs minor mode for editing Internet multimedia
;; messages formatted in MIME (RFC1341 and RFC1342).  All messages in
;; this mode are composed in the tagged MIME format, that are
;; described in the following examples.  The messages composed in the
;; tagged MIME format are automatically translated into a MIME
;; compliant message when exiting the mode.

;; Mule (a multilingual extension to Emacs 18 and 19) has a capability
;; of handling multilingual text in limited ISO-2022 manner that is
;; based on early experiences in Japanese Internet community and
;; resulted in RFC1468 (ISO-2022-JP charset for MIME).  In order to
;; enable multilingual capability in single text message in MIME,
;; charset of multilingual text written in Mule is declared as either
;; `ISO-2022-JP-2' [RFC 1554] or `ISO-2022-INT-1'.  Mule is required
;; for reading the such messages.

;; This MIME composer can work with Mail mode, mh-e letter Mode, and
;; News mode.  First of all, you need the following autoload
;; definition to load mime-mode automatically:
;;
;; (autoload 'mime-mode "mime" "Minor mode for editing MIME message." t)
;;
;; In case of Mail mode (includes VM mode), you need the following
;; hook definition:
;;
;; (setq mail-mode-hook
;;       (list
;;        (function
;;         (lambda ()
;;           (mime-mode)))))
;;
;; In case of MH-E, you need the following hook definition:
;;
;; (setq mh-letter-mode-hook
;;       (list
;;        (function
;;         (lambda ()
;;           (mime-mode)
;;           (make-local-variable 'mail-header-separator)
;;           (setq mail-header-separator "--------")))))
;;
;; In case of News mode, you need the following hook definition:
;;
;; (setq news-reply-mode-hook
;;       (list
;;        (function
;;         (lambda ()
;;           (mime-mode)))))
;;
;; Followings are for message forwarding as content-type
;; "message/rfc822".
;;
;; (setq rmail-mode-hook
;;       (list
;;        (function
;;         (lambda ()
;;           ;; Forward mail using MIME.
;;           (require 'mime)
;;           (substitute-key-definition 'rmail-forward
;;                                      'mime-forward-from-rmail-using-mail
;;                                      (current-local-map))
;;           ))))
;;
;; (setq gnus-mail-forward-method 'mime-forward-from-gnus-using-mail)
;; (setq gnus-summary-mode-hook
;;       (list
;;        (function
;;         (lambda ()
;;           ;; Forward article using MIME.
;;           (require 'mime)
;;           ))))
;;
;; In case of Emacs 19, it is possible to emphasize the message tags
;; using font-lock mode as follows:
;;
;; (setq mime-mode-hook
;;       (list
;;        (function
;;         (lambda ()
;;           (font-lock-mode 1)
;;           (setq font-lock-keywords (list mime-tag-regexp))))))

;; The message tag looks like:
;;
;;	--[[TYPE/SUBTYPE;PARAMETERS][ENCODING]]
;;
;; The tagged MIME message examples:
;;
;; This is a conventional plain text.  It should be translated into
;; text/plain.
;; 
;;--[[text/plain]]
;; This is also a plain text.  But, it is explicitly specified as is.
;;
;;--[[text/plain; charset=ISO-2022-JP]]
;; これは charset を ISO-2022-JP に指定した日本語の plain テキストです.
;;
;;--[[text/richtext]]
;; <center>This is a richtext.</center>
;;
;;--[[image/gif][base64]]^M...image encoded in base64 comes here...
;;
;;--[[audio/basic][base64]]^M...audio encoded in base64 comes here...

;; LCD Archive Entry:
;; mime|Masanobu UMEDA|umerin@mse.kyutech.ac.jp|
;; Simple MIME Composer|
;; $Date: 2004/06/15 16:02:05 $|$Revision: 1.1 $|~/misc/mime.el.Z|

;;; Code:

(require 'sendmail)
(require 'mail-utils)
(require 'metamail)

(defvar mime-prefix "\C-c\C-x"
  "*Keymap prefix for MIME commands.")

(defvar mime-signature-file "~/.signature.rtf"
  "*Signature file to be included as a part of a multipart message.")

(defvar mime-ignore-preceding-spaces nil
  "*Ignore preceding white spaces if non-nil.")

(defvar mime-ignore-trailing-spaces nil
  "*Ignore trailing white spaces if non-nil.")

(defvar mime-ignore-same-text-tag t
  "*Ignore preceding text content-type tag that is same with new one.
If non-nil, the text tag is not inserted unless something different.")

(defvar mime-auto-fill-header t
  "*Fill header fields that contain encoded-words if non-nil.")

(defvar mime-auto-hide-body t
  "*Hide non-textual body encoded in base64 after insertion if non-nil.")

(defvar mime-header-charset-chooser
  (cond ((boundp 'NEMACS)
	 (function mime-header-charset-chooser-for-nemacs))
	((featurep 'mule)
	 (function mime-header-charset-chooser-for-mule))
	((string-match "^19\\." emacs-version)
	 (function mime-header-charset-chooser-for-emacs19))
	(t				;ASCII only emacs
	 (function mime-header-charset-chooser-for-emacs18)))
  "*Function to identify charset and encoding of a header in a give region.
The value is a form of (CHARSET . ENCODING), where ENCODING is either
'B' or 'Q'.  Nil means no encoding is necessary.")

(defvar mime-body-charset-chooser
  (cond ((boundp 'NEMACS)
	 (function mime-body-charset-chooser-for-nemacs))
	((featurep 'mule)
	 (function mime-body-charset-chooser-for-mule))
	((string-match "^19\\." emacs-version)
	 (function mime-body-charset-chooser-for-emacs19))
	(t				;ASCII only emacs
	 (function mime-body-charset-chooser-for-emacs18)))
  "*Function to identify charset and encoding of a text in a given region.
The value is a form of (CHARSET . ENCODING), where ENCODING must be a
full name, such as base64.")

(defvar mime-string-encoder
  (cond ((boundp 'NEMACS)
	 (function mime-string-encoder-for-nemacs))
	((featurep 'mule)
	 (function mime-string-encoder-for-mule))
	((string-match "^19\\." emacs-version)
	 (function mime-string-encoder-for-emacs19))
	(t				;ASCII only emacs
	 (function mime-string-encoder-for-emacs18)))
  "*Function to encode a string for given encoding method.
The method is a form of (CHARSET . ENCODING).")

(defvar mime-voice-recorder
  (function mime-voice-recorder-for-sun)
  "*Function to record a voice message and return a buffer that contains it.")

(defvar mime-mode-hook nil
  "*Hook called when enter MIME mode.")

(defvar mime-translate-hook nil
  "*Hook called before translating into a MIME compliant message.
To insert a signature file specified by mime-signature-file
(`.signature.rtf' by default) automatically, call the function
`mime-insert-signature' from this hook.")

(defvar mime-exit-hook nil
  "*Hook called when exit MIME mode.")

(defvar mime-content-types
  '(("text"
     ;; Charset parameter need not to be specified, since it is
     ;; defined automatically while translation.
     ("plain"
      ;;("charset" "" "ISO-2022-JP" "US-ASCII" "ISO-8859-1" "ISO-8859-8")
      )
     ("richtext"
      ;;("charset" "" "ISO-2022-JP" "US-ASCII" "ISO-8859-1" "ISO-8859-8")
      ))
    ("message"
     ("external-body"
      ("access-type"
       ("anon-ftp"
	("site" "ftp.kyutech.ac.jp" "nic.karrn.ad.jp")
	("directory")
	("name")
	("mode" "binary" "ascii"))
       ("ftp"
	("site")
	("directory")
	("name")
	("mode" "binary" "ascii"))
       ("tftp"
	("site")
	("name"))
       ("afs"
	("site")
	("name"))
       ("local-file"
	("site")
	("name"))
       ("mail-server"
	("server" "ftpmail@nic.karrn.ad.jp"))))
     ("rfc822"))
    ("application"
     ("octet-stream"
      ("name")
      ("type" "" "tar" "shar")
      ("conversions"))
     ("postscript"))
    ("image"
     ("gif")
     ("jpeg")
     ("x-xwd")
     ("x-xbm"))
    ("audio"
     ("basic"))
    ("video"
     ("mpeg")))
  "*Alist of content-type, subtype, parameters and its values.")

(defvar mime-file-types
  '(("\\.rtf$"	"text"	"richtext"	nil	nil)
    ("\\.ps$"	"application"	"postscript"	nil	"quoted-printable")
    ("\\.gif$"	"image"		"gif"	nil	"base64")
    ("\\.jpg$"	"image"		"jpeg"	nil	"base64")
    ("\\.xwd$"	"image"		"x-xwd"	nil	"base64")
    ("\\.xbm$"	"image"		"x-xbm"	nil	"base64")
    ("\\.au$"	"audio"		"basic"	nil	"base64")
    ("\\.mpg$"	"video"		"mpeg"	nil	"base64")
    (".*"	nil		nil	nil	nil))
  "*Alist of file name, types, parameters, and default encoding.
If encoding is nil, it is determined from its contents.")

(defvar mime-transfer-encoders
  '(("base64"			"mmencode")
    ("quoted-printable"		"mmencode" "-q")
    ("7bit"			nil)	;Default
    ("8bit"			nil)
    ("binary"			nil))
  "*Alist of a canonical encoding name, and encoder and optional arguments.
Content transfer encoder is either a command string or a function
symbol or a lambda list that takes two arguments.  If it is nil, no
conversion is necessary.")

(defvar mime-transfer-decoders
  '(("base64"			"mmencode" "-u")
    ("quoted-printable"		"mmencode" "-q" "-u")
    ("7bit"			nil)	;Default
    ("8bit"			nil)
    ("binary"			nil))
  "*Alist of a canonical encoding name, and decoder and optional arguments.
Content transfer decoder is either a command string or a function
symbol or a lambda list that takes two arguments.  If it is nil, no
conversion is necessary.")

(defvar mime-encoding-cnames
  '(("b"	"base64")
    ("q"	"quoted-printable"))
  "*Alist of a short encoding name used in headers and its canonical name.")

(defvar mime-message-handlers
  '(("text/plain"		t)
    ;;("text/richtext"		t)
    (t				mime-run-metamail))
  "*Alist of Content-type and its preview method.
The method can be a function symbol or a lambda list that takes a
region, nil or t.  nil means no handler is defined.  t means what you
see is what you get.  If content-type is t, it is a default.")

(defvar mime-max-header-length 76
  "*Any header should not exceed this value.")

(defvar mime-tspecials-regexp "[][()<>@,;:\\\"/?.= \t]"
  "*Specify MIME tspecials.
Tspecials means any character that matches with it in header must be quoted.")

(defvar mime-tag-regexp "^--[[][[]\\([^]]*\\)]\\([[]\\([^]]*\\)]\\|\\)]"
  "*Regexp of MIME tag in the form of [[CONTENT-TYPE][ENCODING]].")

(defvar mime-tag-format "--[[%s]]"
  "*Control-string making a MIME tag.")

(defvar mime-tag-format-with-encoding "--[[%s][%s]]"
  "*Control-string making a MIME tag with encoding.")

(defvar mime-multipart-boundary "Multipart"
  "*Boundary of a multipart message.")


(defconst mime-version-number "1.0"
  "MIME version number.")

(defvar mime-mode-flag nil)
(make-variable-buffer-local 'mime-mode-flag)

(or (assq 'mime-mode-flag minor-mode-alist)
    (setq minor-mode-alist
	  (cons (list 'mime-mode-flag " MIME") minor-mode-alist)))

(defun mime-define-keymap (keymap)
  "Add MIME commands to KEYMAP."
  (if (not (keymapp keymap))
      nil
    (define-key keymap "\C-t" 'mime-insert-text)
    (define-key keymap "\C-i" 'mime-insert-file)
    (define-key keymap "\C-e" 'mime-insert-external)
    (define-key keymap "\C-v" 'mime-insert-voice)
    (define-key keymap "\C-w" 'mime-insert-signature)
    (define-key keymap "\C-m" 'mime-insert-tag)
    (define-key keymap "\C-p" 'mime-preview-content)
    (define-key keymap "\C-z" 'mime-mode-exit)
    (define-key keymap "?" 'help-mime-mode)))

(defun mime-define-menu-for-emacs19 ()
  "Define menu for Emacs 19."
  (define-key (current-local-map) [menu-bar mime]
    (cons "MIME" (make-sparse-keymap "MIME")))
  (define-key (current-local-map) [menu-bar mime help]
    '("Describe MIME Mode" . help-mime-mode))
  (define-key (current-local-map) [menu-bar mime preview]
    '("Preview Content" . mime-preview-content))
  (define-key (current-local-map) [menu-bar mime tag]
    '("Insert New Tag" . mime-insert-tag))
  (define-key (current-local-map) [menu-bar mime signature]
    '("Insert Signature" . mime-insert-signature))
  (define-key (current-local-map) [menu-bar mime voice]
    '("Insert Voice" . mime-insert-voice))
  (define-key (current-local-map) [menu-bar mime external]
    '("Insert External" . mime-insert-external))
  (define-key (current-local-map) [menu-bar mime file]
    '("Insert File" . mime-insert-file))
  (define-key (current-local-map) [menu-bar mime text]
    '("Insert Text" . mime-insert-text)))

(defun mime-define-menu-for-xemacs ()
  "Define menu for XEmacs/Lucid Emacs"
  (let ((mime-menu-bar
	 '("MIME"
	   ["Insert Text" mime-insert-text t]
	   ["Insert File" mime-insert-file t]
	   ["Insert External" mime-insert-external t]
	   ["Insert Voice" mime-insert-voice t]
	   ["Insert Signature" mime-insert-signature t]
	   ["Insert New Tag" mime-insert-tag t]
	   "-----"
	   ["Preview Contents" mime-preview-content t]
	   "-----"
	   ["Describe MIME Mode" help-mime-mode t])))
    (set-buffer-menubar (copy-sequence current-menubar))
    (add-menu nil (car mime-menu-bar) (cdr mime-menu-bar) nil)
    ))

;;;###autoload
(defun mime-mode ()
  "MIME minor mode for editing the tagged MIME message.

In this mode, basically, the message is composed in the tagged MIME
format.  The message tag looks like:

	`--[[text/plain; charset=ISO-2022-JP][7bit]]'.

The tag specifies the MIME content type, subtype, optional parameters
and transfer encoding of the message following the tag.  Messages
without any tag are treated as `text/plain' by default.  Charset and
transfer encoding are automatically defined unless explicitly
specified.  Binary messages such as audio and image are usually hidden
using selective-display facility.  The messages in the tagged MIME
format are automatically translated into a MIME compliant message when
exiting this mode.

Available charsets depend on Emacs version being used.  The following
lists the available charsets of each emacs.

Emacs18:	US-ASCII is only available.
NEmacs:		US-ASCII and ISO-2022-JP are available.
Emacs19:	US-ASCII and ISO-8859-1 are available.
Mule:		US-ASCII, ISO-8859-* (except for ISO-8859-6),
		ISO-2022-JP, ISO-2022-JP-2 and ISO-2022-INT-1 are available.

ISO-2022-JP-2 and ISO-2022-INT-1 charsets used in Mule is expected to
be used to represent multilingual text in intermixed manner.  Any
languages that has no registered charset are represented as either
ISO-2022-JP-2 or ISO-2022-INT-1 in Mule.

Following commands are available in addition to major mode commands:
\\[mime-insert-text]	insert a text message.
\\[mime-insert-file]	insert a (binary) file.
\\[mime-insert-external]	insert a reference to external body.
\\[mime-insert-voice]	insert a voice message.
\\[mime-insert-signature]	insert a signature file at end.
\\[mime-insert-tag]	insert a new MIME tag.
\\[mime-preview-content]	preview an included content.
\\[mime-mode-exit]	exit and translate into a MIME compliant message.
\\[mime-mode-exit-and-run]		exit, translate and run the original command.
\\[help-mime-mode]	show this help.

Additional commands are available in some major modes:
C-c C-c		exit, translate and run the original command.
C-c C-s		exit, translate and run the original command.

The following is a message example written in the tagged MIME format.
TABs at the beginning of the line are not a part of the message:

	This is a conventional plain text.  It should be translated
	into text/plain.
	--[[text/plain]]
	This is also a plain text.  But, it is explicitly specified as
	is.
	--[[text/plain; charset=ISO-2022-JP]]
	これは charset を ISO-2022-JP に指定した日本語の plain テキス
	トです.
	--[[text/richtext]]
	<center>This is a richtext.</center>
	--[[image/gif][base64]]^M...image encoded in base64 here...
	--[[audio/basic][base64]]^M...audio encoded in base64 here...

User customizable variables (not documented all of them):
 mime-prefix
    Specifies a key prefix for MIME minor mode commands.

 mime-signature-file
    Specifies a signature file to be included as part of a multipart
    message.

 mime-ignore-preceding-spaces
    Preceding white spaces in a message body are ignored if non-nil.

 mime-ignore-trailing-spaces
    Trailing white spaces in a message body are ignored if non-nil.

 mime-auto-fill-header
    Fill header fields that contain encoded-words if non-nil.

 mime-auto-hide-body
    Hide a non-textual body message encoded in base64 after insertion
    if non-nil.

 mime-header-charset-chooser
    Specifies a function to identify charset and encoding of a text in
    a give region of header fields.  The value is a form of (CHARSET . 
    ENCODING), where ENCODING is either 'B' or 'Q'.  Nil means no
    encoding is necessary.

 mime-body-charset-chooser
    Specifies a function to identify charset and encoding of a text in
    a given region.  The value is a form of (CHARSET . ENCODING),
    where ENCODING must be a full name, such as base64.

 mime-string-encoder
    Specifies a function to encode a string for given encoding method.
    The method is a form of (CHARSET . ENCODING).

 mime-voice-recorder
    Specifies a function to record a voice message and return a buffer
    that contains it.  The function mime-voice-recorder-for-sun is for
    Sun SparcStations.

 mime-mode-hook
    Turning on MIME mode calls the value of mime-mode-hook, if it is
    non-nil.

 mime-translate-hook
    The value of mime-translate-hook is called just before translating
    the tagged MIME format into a MIME compliant message if it is
    non-nil.  If the hook call the function mime-insert-signature, the
    signature file will be inserted automatically.

 mime-exit-hook
    Turning off MIME mode calls the value of mime-exit-hook, if it is
    non-nil."
  (interactive)
  (if mime-mode-flag
      (error "You are already editing a MIME message.")
    (setq mime-mode-flag t)
    ;; Remember old key bindings.
    (make-local-variable 'mime-mode-old-local-map)
    (setq mime-mode-old-local-map (current-local-map))
    ;; Add MIME commands to current local map.
    (use-local-map (copy-keymap (current-local-map)))
    (if (not (lookup-key (current-local-map) mime-prefix))
	(define-key (current-local-map) mime-prefix (make-sparse-keymap)))
    (mime-define-keymap (lookup-key (current-local-map) mime-prefix))
    ;; Replace key definitions to avoid sending a message without
    ;; conversion into a MIME compliant message.
    (let ((keymap nil)
          (keymaps (accessible-keymaps (current-local-map))))
      (while keymaps
        (setq keymap (cdr (car keymaps)))
        (setq keymaps (cdr keymaps))
        (if (not (keymapp keymap))
            nil
          ;; Mail mode:
          (substitute-key-definition
           'mail-send 'mime-mode-exit-and-run keymap)
          (substitute-key-definition
           'mail-send-and-exit 'mime-mode-exit-and-run keymap)
          ;; mh-e letter mode:
          (substitute-key-definition
           'mh-send-letter 'mime-mode-exit-and-run keymap)
	  ;; Mail mode called from VM:
	  (substitute-key-definition
	   'vm-mail-send 'mime-mode-exit-and-run keymap)
	  (substitute-key-definition
	   'vm-mail-send-and-exit 'mime-mode-exit-and-run keymap)
          ;; News mode:
          (substitute-key-definition
           'news-inews 'mime-mode-exit-and-run keymap)
          )))
    ;; Define menu.  Menus for other emacs implementations are
    ;; welcome.
    (cond ((string-match "XEmacs\\|Lucid" emacs-version)
           (mime-define-menu-for-xemacs))
	  ((string-match "^19\\." emacs-version)
	   (mime-define-menu-for-emacs19)))
    ;; Remember old selective-display.
    (make-local-variable 'mime-mode-old-selective-display)
    (setq mime-mode-old-selective-display selective-display)
    (setq selective-display t)
    ;; I don't care about saving these.
    (setq paragraph-start
	  (concat mime-tag-regexp "\\|" paragraph-start))
    (setq paragraph-separate
	  (concat mime-tag-regexp "\\|" paragraph-separate))
    (run-hooks 'mime-mode-hook)
    (message
     (substitute-command-keys
      "Type \\[mime-mode-exit] to exit MIME mode, and type \\[help-mime-mode] to get help."))
    ))

;;;###autoload
(fset 'edit-mime 'mime-mode)		; for convenience

(defun mime-mode-exit (&optional nomime)
  "Translate the tagged MIME message into a MIME compliant message.
With no argument encode a message in the buffer into MIME, otherwise
just return to previous mode."
  (interactive "P")
  (if (not mime-mode-flag)
      (error "You aren't editing a MIME message.")
    (if (not nomime)
	(progn
	  (run-hooks 'mime-translate-hook)
	  (mime-translate-buffer)))
    ;; Restore previous state.
    (setq mime-mode-flag nil)
    (use-local-map mime-mode-old-local-map)
    (setq selective-display mime-mode-old-selective-display)
    (set-buffer-modified-p (buffer-modified-p))
    (run-hooks 'mime-exit-hook)
    (message "Exit MIME mode.")
    ))

(defun mime-mode-exit-and-run ()
  "Run a command bound to the same key on old keymap after the translation."
  (interactive)
  (let ((cmd (lookup-key mime-mode-old-local-map (this-command-keys))))
    (mime-mode-exit)
    (if cmd
	(call-interactively cmd))))

(defun help-mime-mode ()
  "Show help message about MIME mode."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "Edit MIME Mode:\n")
    (princ (documentation 'mime-mode))
    (print-help-return-message)))

(defun mime-insert-text ()
  "Insert a text message.
Charset is automatically obtained from the mime-body-charset-chooser."
  (interactive)
  (if (and (mime-insert-tag "text" nil nil)
	   (looking-at mime-tag-regexp))
      (progn
	;; Make a space between the following message.
	(insert "\n")
	(forward-char -1))))

(defun mime-insert-file (file)
  "Insert a message from a file."
  (interactive "fInsert file as MIME message: ")
  (let*  ((guess (mime-find-file-type file))
	  (pritype (nth 0 guess))
	  (subtype (nth 1 guess))
	  (parameters (nth 2 guess))
	  (default (nth 3 guess))	;Guess encoding from its file name.
	  (encoding
	   (if (not (interactive-p))
	       default
	     (completing-read
	      (concat "What transfer encoding"
		      (if default
			  (concat " (default "
				  (if (string-equal default "") "\"\"" default)
				  ")"
				  ))
		      ": ")
	      mime-transfer-encoders nil t nil))))
    (if (string-equal encoding "")
	(setq encoding default))
    (mime-insert-tag pritype subtype parameters)
    (mime-insert-binary-file file encoding)))

(defun mime-insert-external ()
  "Insert a reference to external body."
  (interactive)
  (mime-insert-tag "message" "external-body" nil ";\n\t")
  ;;(forward-char -1)
  ;;(insert "Content-Description: " (read-string "Content-Description: ") "\n")
  ;;(forward-line 1)
  (let* ((pritype (mime-prompt-for-type))
	 (subtype (mime-prompt-for-subtype pritype))
	 (parameters (mime-prompt-for-parameters pritype subtype ";\n\t")))
    (and pritype
	 subtype
	 (insert "Content-Type: "
		 pritype "/" subtype (or parameters "") "\n")))
  (if (and (not (eobp))
	   (not (looking-at mime-tag-regexp)))
      (insert (mime-make-text-tag) "\n")))

(defun mime-insert-voice ()
  "Insert a voice message."
  (interactive)
  (mime-insert-tag "audio" "basic" nil)
  (let ((buffer (funcall mime-voice-recorder)))
    (unwind-protect
	(mime-insert-binary-buffer buffer "base64")
      (kill-buffer buffer)
      )))

(defun mime-insert-signature ()
  "Insert a signature file specified by mime-signature-file."
  (interactive)
  (let ((file (expand-file-name mime-signature-file)))
    (if (file-exists-p file)
	(progn
	  (goto-char (point-max))
	  (if (not (bolp))		;Make sure the end of message.
	      (insert "\n"))
	  (mime-insert-file file)))))

(defun mime-preview-content ()
  "Preview an included message content around point through metamail."
  (interactive)
  (let ((tag nil)
	(handler nil)
	(beg nil)
	(end nil))
    (save-excursion
      (if (mime-goto-tag)
	  (progn
	    (setq tag (buffer-substring (match-beginning 0) (match-end 0)))
	    (setq handler (mime-find-handler
			   (mime-strip-parameters
			    (mime-get-contype tag))))
	    (setq beg (point))
	    (setq end (mime-content-end))
	    )))
    (if tag
	(cond ((eq handler t)		;As is.
	       (message "Nothing to do.  You are seeing as is."))
	      ((or (symbolp handler) (consp handler))
	       (funcall handler beg end))
	      (t
	       (ding) (message "No message handler defined.")))
      (ding) (message "No messages to preview around here.")
      )))


;; Insert a new tag around a point.

(defun mime-insert-tag (&optional pritype subtype parameters delimiter)
  "Insert new MIME tag and return a list of PRITYPE, SUBTYPE, and PARAMETERS.
If nothing is inserted, return nil."
  (interactive)
  (let ((oldtag nil)
	(newtag nil)
	(current (point)))
    (setq pritype
	  (or pritype
	      (mime-prompt-for-type)))
    (setq subtype
	  (or subtype
	      (mime-prompt-for-subtype pritype)))
    (setq parameters
	  (or parameters
	      (mime-prompt-for-parameters pritype subtype delimiter)))
    ;; Make a new MIME tag.
    (setq newtag (mime-make-tag pritype subtype parameters))
    ;; Find an current MIME tag.
    (setq oldtag
	  (save-excursion
	    (if (mime-goto-tag)
		(buffer-substring (match-beginning 0) (match-end 0))
	      ;; Assume content type is 'text/plan'.
	      (mime-make-tag "text" "plain")
	      )))
    ;; We are only interested in TEXT.
    (if (and oldtag
	     (not (mime-test-content-type (mime-get-contype oldtag) "text")))
	(setq oldtag nil))
    (beginning-of-line)
    (cond ((and oldtag			;Text
		(not (eobp))
		(save-excursion
		  (forward-line -1)
		  (looking-at mime-tag-regexp))
		(or mime-ignore-same-text-tag
		    (not (string-equal oldtag newtag))))
	   ;; If point is at the next of current tag, move to the
	   ;; beginning of the tag to disable insertion of extra tag.
	   (forward-line -1))
	  ((and oldtag			;Text
		(not (eobp))
		(not (looking-at mime-tag-regexp))
		(or mime-ignore-same-text-tag
		    (not (string-equal oldtag newtag))))
	   ;; Copy current tag to break a text into two.
	   (save-excursion
	     (insert oldtag "\n")))
	  ((and (null oldtag)		;Not text
		(not (looking-at mime-tag-regexp)))
	   ;; Adjust insertion point.  In the middle of text, it is
	   ;; okay to break the text into two.  However, it should not
	   ;; be broken into two, if otherwise.
	   (goto-char (mime-content-end))
	   (if (eolp)
	       (forward-line 1))
	   (if (not (bolp))
	       (insert "\n"))
	   ))
    ;; Make a new tag.
    (if (or (not oldtag)		;Not text
	    (or mime-ignore-same-text-tag
		(not (string-equal oldtag newtag))))
	(progn
	  ;; Mark the beginning of the tag for convenience.
	  (push-mark (point) 'nomsg)
	  (insert newtag "\n")
	  (list pritype subtype parameters) ;New tag is created.
	  )
      ;; Restore previous point.
      (goto-char current)
      nil				;Nothing is created.
      )
    ))

;; Insert the binary content after MIME tag.

(defun mime-insert-binary-file (file &optional encoding)
  "Insert binary FILE at point.
Optional argument ENCODING specifies an encoding method such as base64."
  (let ((tmpbuf (get-buffer-create " *MIME insert*")))
    (save-excursion
      (set-buffer tmpbuf)
      (erase-buffer)
      (let ((mc-flag nil)		;Mule
	    (emx-binary-mode t)	        ;Stop CRLF to LF conversion in OS/2
	    (file-coding-system-for-read
	     (if (featurep 'mule) *noconv*))
	    (kanji-flag nil))		;NEmacs
	(insert-file-contents file)))
    (prog1
	(mime-insert-binary-buffer tmpbuf encoding)
      (kill-buffer tmpbuf))))

(defun mime-insert-binary-buffer (buffer &optional encoding)
  "Insert binary BUFFER at point.
Optional argument ENCODING specifies an encoding method such as base64."
  (let* ((tagend (1- (point)))		;End of the tag
	 (hide-p (and mime-auto-hide-body
		      (stringp encoding)
		      (string-equal (downcase encoding) "base64"))))
    (save-restriction
      (narrow-to-region (1- (point)) (point))
      (let ((start (point))
	    (emx-binary-mode t))	;Stop LF to CRLF conversion in OS/2
	(insert-buffer-substring buffer)
	;; Encode binary message if necessary.
	(if encoding
	    (mime-encode-region encoding start (point-max))))
      (if hide-p
	  (progn
	    (mime-flag-region (point-min) (1- (point-max)) ?\^M)
	    (goto-char (point-max)))
	))
    ;; Define encoding even if it is 7bit.
    (if (stringp encoding)
	(save-excursion
	  (goto-char tagend)		;Make sure which line the tag is on.
	  (mime-define-encoding encoding)))
    ))

(defun mime-run-metamail (begin end)
  "Run metamail on a tagged MIME message from BEGIN to END."
  (let ((msgbuf (current-buffer)))
    (set-buffer (get-buffer-create "*Previewing MIME message*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert-buffer-substring msgbuf begin end)
    ;; Imitate mail mode buffer.
    (goto-char (point-min))
    (insert "Content-Description: Previewing current message.\n"
	    mail-header-separator "\n")
    ;; Then translate it into a MIME compliant message.
    (mime-translate-body)
    (goto-char (point-min))
    (if (search-forward (concat "\n" mail-header-separator "\n") nil t)
	(replace-match "\n\n"))
    ;; Preview the contents using metamail.
    ;; We are interested in its body.
    (metamail-interpret-body)
    (view-buffer (current-buffer))
    ))


;; Commands work on a current message flagment.

(defun mime-goto-tag ()
  "Search for the beginning of the tagged MIME message."
  (let ((current (point)))
    (if (looking-at mime-tag-regexp)
	t
      ;; At first, go to the end.
      (if (re-search-forward mime-tag-regexp nil t)
	  (progn
	    (goto-char (match-beginning 0)) ;For multiline tag
	    (forward-line -1)
	    (end-of-line))
	(goto-char (point-max)))
      ;; Then search for the beginning. 
      (re-search-backward mime-tag-regexp nil t)
      (beginning-of-line)
      (or (looking-at mime-tag-regexp)
	  ;; Restore previous point.
	  (progn
	    (goto-char current)
	    nil
	    ))
      )))

(defun mime-content-beginning ()
  "Return the point of the beginning of content."
  (save-excursion
    (let ((beg (save-excursion
		 (beginning-of-line) (point))))
      (if (mime-goto-tag)
	  (let ((top (point)))
	    (goto-char (match-end 0))
	    (if (and (= beg top)
		     (= (following-char) ?\^M))
		(point)
	      (forward-line 1)
	      (point)))
	;; Default text/plain tag.
	(goto-char (point-min))
	(re-search-forward
	 (concat "\n" (regexp-quote mail-header-separator)
		 (if mime-ignore-preceding-spaces
		     "[ \t\n]*\n" "\n")) nil 'move)
	(point))
      )))

(defun mime-content-end ()
  "Return the point of the end of content."
  (save-excursion
    (let ((beg (save-excursion
		 (beginning-of-line) (point))))
      (if (mime-goto-tag)
	  (let ((top (point)))
	    (goto-char (match-end 0))
	    (if (and (= beg top)	;Must be on the same line.
		     (= (following-char) ?\^M))
		(progn
		  (end-of-line)
		  (point))
	      ;; Move to the end of this text.
	      (if (re-search-forward mime-tag-regexp nil 'move)
		  ;; Don't forget a multiline tag.
		  (goto-char (match-beginning 0)))
	      (point)
	      ))
	;; Assume the message begins with text/plain.
	(goto-char (mime-content-beginning))
	(if (re-search-forward mime-tag-regexp nil 'move)
	    ;; Don't forget a multiline tag.
	    (goto-char (match-beginning 0)))
	(point))
      )))

(defun mime-define-charset (charset)
  "Set charset of current tag to CHARSET."
  (save-excursion
    (if (mime-goto-tag)
	(let ((tag (buffer-substring (match-beginning 0) (match-end 0))))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert
	   (mime-create-tag (mime-set-parameter
			     (mime-get-contype tag) "charset" charset)
			    (mime-get-encoding tag))))
      )))

(defun mime-define-encoding (encoding)
  "Set encoding of current tag to ENCODING."
  (save-excursion
    (if (mime-goto-tag)
	(let ((tag (buffer-substring (match-beginning 0) (match-end 0))))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert (mime-create-tag (mime-get-contype tag) encoding)))
      )))

(defun mime-choose-charset ()
  "Choose charset of a text following current point."
  (save-excursion
    (let* ((beg (point))
	   (end (mime-content-end)))
      (car (funcall mime-body-charset-chooser beg end)))))

(defun mime-choose-encoding ()
  "Choose encoding of a text following current point."
  (save-excursion
    (let* ((beg (point))
	   (end (mime-content-end)))
      (cdr (funcall mime-body-charset-chooser beg end)))))

(defun mime-make-text-tag (&optional subtype)
  "Make a tag for a text after current point.
Subtype of text type can be specified by an optional argument SUBTYPE.
Otherwise, it is obtained from mime-content-types."
  (let* ((pritype "text")
	 (subtype (or subtype
		      (car (car (cdr (assoc pritype mime-content-types)))))))
    ;; Charset should be defined later.
    (mime-make-tag pritype subtype)))


;; Tag handling functions

(defun mime-make-tag (pritype subtype &optional parameters encoding)
  "Make a tag of MIME message of PRITYPE, SUBTYPE and optional PARAMETERS."
  (mime-create-tag (concat (or pritype "") "/" (or subtype "")
			   (or parameters ""))
		   encoding))

(defun mime-create-tag (contype &optional encoding)
  "Make a tag with CONTENT-TYPE and optional ENCODING."
  (format (if encoding mime-tag-format-with-encoding mime-tag-format)
	  contype encoding))

(defun mime-get-contype (tag)
  "Return Content-Type (including parameters) of TAG."
  (and (stringp tag)
       (string-match mime-tag-regexp tag)
       (substring tag (match-beginning 1) (match-end 1))))

(defun mime-get-encoding (tag)
  "Return encoding of TAG."
  (and (stringp tag)
       (string-match mime-tag-regexp tag)
       (match-beginning 3)
       (not (= (match-beginning 3) (match-end 3)))
       (substring tag (match-beginning 3) (match-end 3))))

(defun mime-get-parameter (contype parameter)
  "For given CONTYPE return value for PARAMETER.
Nil if no such parameter."
  (if (string-match
       (concat
	";[ \t\n]*"
	(regexp-quote parameter)
	"[ \t\n]*=[ \t\n]*\\([^\" \t\n;]*\\|\"[^\"]*\"\\)\\([ \t\n]*;\\|$\\)")
       contype)
      (substring contype (match-beginning 1) (match-end 1))
    nil					;No such parameter
    ))

(defun mime-set-parameter (contype parameter value)
  "For given CONTYPE set PARAMETER to VALUE."
  (if (string-match
       (concat
	";[ \t\n]*\\("
	(regexp-quote parameter)
	"[ \t\n]*=[ \t\n]*\\([^\" \t\n;]*\\|\"[^\"]*\"\\)\\)[ \t\n]*\\(;\\|$\\)")
       contype)
      ;; Change value
      (concat (substring contype 0 (match-beginning 1))
	      parameter "=" value
	      (substring contype (match-end 1)))
    (concat contype "; " parameter "=" value)))

(defun mime-strip-parameters (contype)
  "Return primary content-type and subtype without parameters for CONTYPE."
  (if (string-match "^[ \t]*\\([^; \t\n]*\\)" contype)
      (substring contype (match-beginning 1) (match-end 1)) nil))

(defun mime-test-content-type (contype type &optional subtype)
  "Test if CONTYPE is a TYPE and an optional SUBTYPE."
  (and (stringp contype)
       (stringp type)
       (string-match
	(concat "^[ \t]*" (downcase type) "/" (downcase (or subtype "")))
	(downcase contype))))


;; Basic functions

(defun mime-find-encoder (encoding)
  "Return message transfer encoder for ENCODING."
  (and (stringp encoding)
       (let* ((encoding
	       (or (nth 1 (assoc (downcase encoding) mime-encoding-cnames))
		   encoding))
	      (encoder
	       (cdr (assoc (downcase encoding) mime-transfer-encoders))))
	 ;; (nil) means there is no encoder.
	 (if (and (consp encoder)
		  (null (car encoder)))
	     nil encoder
	     ))))

(defun mime-find-decoder (encoding)
  "Return message transfer decoder for ENCODING."
  (and (stringp encoding)
       (let* ((encoding
	       (or (nth 1 (assoc (downcase encoding) mime-encoding-cnames))
		   encoding))
	      (decoder
	       (cdr (assoc (downcase encoding) mime-transfer-decoders))))
	 ;; (nil) means there is no decoder.
	 (if (and (consp decoder)
		  (null (car decoder)))
	     nil decoder
	     ))))

(defun mime-find-file-type (file)
  "Guess Content-Type, subtype, and parameters from FILE."
  (let ((guess nil)
	(guesses mime-file-types))
    (while (and (not guess) guesses)
      (if (string-match (car (car guesses)) file)
	  (setq guess (cdr (car guesses))))
      (setq guesses (cdr guesses)))
    guess
    ))

(defun mime-find-handler (type)
  "Return message handler for Content-type TYPE."
  (and (stringp type)
       (let ((pair (assoc (downcase type) mime-message-handlers)))
	 (if pair
	     (nth 1 pair)
	   ;; Search default if no entry.
	   (nth 1 (assoc t mime-message-handlers))))))

(defun mime-prompt-for-type ()
  "Ask for Content-type."
  (let ((type ""))
    ;; Repeat until primary content type is specified.
    (while (string-equal type "")
      (setq type
	    (completing-read "What content type: "
			     mime-content-types
			     nil
			     'require-match ;Type must be specified.
			     nil
			     ))
      (if (string-equal type "")
	  (progn
	    (message "Content type is required.")
	    (beep)
	    (sit-for 1)
	    ))
      )
    type
    ))

(defun mime-prompt-for-subtype (pritype)
  "Ask for Content-type subtype of Content-Type PRITYPE."
  (let* ((default (car (car (cdr (assoc pritype mime-content-types)))))
	 (answer
	  (completing-read
	   (if default
	       (concat
		"What content subtype: (default " default ") ")
	     "What content subtype: ")
	   (cdr (assoc pritype mime-content-types))
	   nil
	   'require-match		;Subtype must be specified.
	   nil
	   )))
    (if (string-equal answer "") default answer)))

(defun mime-prompt-for-parameters (pritype subtype &optional delimiter)
  "Ask for Content-type parameters of Content-Type PRITYPE and SUBTYPE.
Optional DELIMITER specifies parameter delimiter (';' by default)."
  (let* ((delimiter (or delimiter "; "))
	 (parameters
	  (mapconcat
	   (function identity)
	   (delq nil
		 (mime-prompt-for-parameters-1
		  (cdr (assoc subtype
			      (cdr (assoc pritype mime-content-types))))))
	   delimiter
	   )))
    (if (and (stringp parameters)
	     (not (string-equal parameters "")))
	(concat delimiter parameters)
      ""				;"" if no parameters
      )))

(defun mime-prompt-for-parameters-1 (optlist)
  (apply (function append)
	 (mapcar (function mime-prompt-for-parameter) optlist)))

(defun mime-prompt-for-parameter (parameter)
  "Ask for PARAMETER.
Parameter must be '(PROMPT CHOICE1 (CHOISE2 ...))."
  (let* ((prompt (car parameter))
	 (choices (mapcar (function
			   (lambda (e)
			     (if (consp e) e (list e))))
			  (cdr parameter)))
	 (default (car (car choices)))
	 (answer nil))
    (if choices
	(progn
	  (setq answer
		(completing-read
		 (concat "What " prompt
			 ": (default "
			 (if (string-equal default "") "\"\"" default)
			 ") ")
		 choices nil nil ""))
	  ;; If nothing is selected, use default.
	  (if (string-equal answer "")
	      (setq answer default)))
      (setq answer
	    (read-string (concat "What " prompt ": "))))
    (cons (if (and answer
		   (not (string-equal answer "")))
	      (concat prompt "="
		      ;; Note: control characters ignored!
		      (if (string-match mime-tspecials-regexp answer)
			  (concat "\"" answer "\"") answer)))
	  (mime-prompt-for-parameters-1 (cdr (assoc answer (cdr parameter)))))
    ))

(defun mime-encode-string (encoding string)
  "Using ENCODING encode a STRING.
If the STRING is too long, the encoded string may be broken into
several lines."
  (save-excursion
    (set-buffer (get-buffer-create " *MIME encoding*"))
    (erase-buffer)
    (insert string)
    (mime-encode-region encoding (point-min) (point-max))
    (prog1
	(buffer-substring (point-min) (point-max))
      (kill-buffer (current-buffer)))))

(defun mime-decode-string (encoding string)
  "Using ENCODING decode a STRING."
  (save-excursion
    (set-buffer (get-buffer-create " *MIME decoding*"))
    (erase-buffer)
    (insert string)
    (mime-decode-region encoding (point-min) (point-max))
    (prog1
	(buffer-substring (point-min) (point-max))
      (kill-buffer (current-buffer)))))

(defun mime-encode-region (encoding begin end)
  "Encode current buffer using transfer ENCODING from BEGIN to END."
  (interactive
   (list (completing-read "Choose transfer encoding: "
			  mime-transfer-encoders nil 'req)
	 (region-beginning) (region-end)))
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (let ((encoder (mime-find-encoder encoding)))
	(cond ((and (consp encoder)
		    (stringp (car encoder)))
	       ;; BUG: kanji-flag must be localized.
	       (let ((selective-display nil) ;Disable ^M to nl translation.
		     (mc-flag nil)	;Mule
		     (kanji-flag nil))	;NEmacs
		 ;; Disable automatic conversion of coding system if Mule.
		 (if (featurep 'mule)
		     (define-program-coding-system nil (car encoder) *noconv*))
		 (apply (function call-process-region)
			(point-min) (point-max)
			(car encoder)
			t		;Delete the text
			t		;Output to current buffer
			nil		;No display
			(cdr encoder))))
	      (encoder
	       (funcall encoder (point-max) (point-min))))
	))))

(defun mime-decode-region (encoding begin end)
  "Decode current buffer using transfer ENCODING from BEGIN to END."
  (interactive
   (list (completing-read "Choose transfer encoding: "
			  mime-transfer-decoders nil 'req)
	 (region-beginning) (region-end)))
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (let ((decoder (mime-find-decoder encoding)))
	(cond ((and (consp decoder)
		    (stringp (car decoder)))
	       ;; BUG: kanji-flag must be localized.
	       (let ((selective-display nil) ;Disable ^M to nl translation.
		     (mc-flag nil)	;Mule
		     (kanji-flag nil))	;NEmacs
		 ;; Disable automatic conversion of coding system if Mule.
		 (if (featurep 'mule)
		     (define-program-coding-system nil (car decoder) *noconv*))
		 (apply (function call-process-region)
			(point-min) (point-max)
			(car decoder)
			t		;Delete the text
			t		;Output to current buffer
			nil		;No display
			(cdr decoder))))
	      (decoder
	       (funcall decoder (point-min) (point-max))))
	))))

(defun mime-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is `\\n' (newline character) then text is shown,
while if FLAG is `\\^M' (control-M) the text is hidden."
  (let ((buffer-read-only nil)		;Okay even if write protected.
	(modp (buffer-modified-p)))
    (unwind-protect
        (subst-char-in-region from to
			      (if (= flag ?\n) ?\^M ?\n)
			      flag t)
      (set-buffer-modified-p modp))))


;; Translate the tagged MIME messages into a MIME compliant message.

(defun mime-translate-buffer ()
  "Encode the tagged MIME message in current buffer in MIME compliant message."
  (interactive)
  (mime-translate-header)
  (mime-translate-body))

(defun mime-translate-header ()
  "Encode headers in MIME compliant message headers."
  (interactive)
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (search-forward (concat "\n" mail-header-separator "\n") nil t)
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      (while (re-search-forward "^[^ \t\n:]+:[ \t]" nil t)
	(let* ((beg (point))
	       (end (progn
		      (while (progn (forward-line 1) (looking-at "[ \t]")))
		      (1- (point)))))
	  (if (funcall mime-header-charset-chooser beg end)
	      (let* ((string (buffer-substring beg end))
		     (newstr (mime-translate-header-string string))
		     (fill-column mime-max-header-length)
		     (fill-prefix " "))
		(if (not (string-equal string newstr))
		    (save-excursion
		      (goto-char beg)
		      (delete-region beg end)
		      (insert newstr)
		      ;; Fill header fields.
		      (if mime-auto-fill-header
			  ;; The region must end with a newline to
			  ;; fill the region without inserting extra
			  ;; newline.
			  (progn
			    (fill-region-as-paragraph beg (1+ (point)))
			    ;; If word is long, the first line may
			    ;; contain only white spaces.  This causes
			    ;; troubles.
			    (goto-char beg)
			    (if (looking-at "[ \t]*\n[ \t]")
				(replace-match ""))
			    ))
		      ))))
	  ))
      )))

(defun mime-translate-header-string (string)
  "Encode STRING suitable for use in header."
  (save-excursion
    (let* ((poslist nil)		;Possible break points.
	   (last-encoding nil)		;last chunk encoding
	   (this-encoding nil)		;this chunk encoding
	   (start nil)			;Chunk start point
	   (chunk nil)
	   (saved nil)			;Saved last line.
	   (separator nil)
	   (result ""))
      (set-buffer (get-buffer-create " *MIME header*"))
      (erase-buffer)
      (insert string)
      ;; Replace white spaces for continuation line with a space.
      (goto-char (point-min))
      (while (re-search-forward "\n[ \t]*" nil t)
	(replace-match " "))
      ;; Encode string
      (goto-char (point-min))
      (while (not (eobp))
	(setq start (point))
	;; Set initial code.
	(setq last-encoding this-encoding) 
	(setq this-encoding
	      (funcall mime-header-charset-chooser
		       start
		       (save-excursion
			 (forward-char 1) (point))))
	(setq poslist (mime-search-for-code-boundary))
	(if (not this-encoding)
	    (setq chunk (buffer-substring start (point)))
	  ;; Break line if too long in terms of RFC1342.
	  (let ((repeat t))
	    (while repeat
	      (setq chunk
		    (mime-encode-header-string
		     this-encoding
		     (buffer-substring start (point))))
	      (if (<= (length chunk) mime-max-header-length)
		  (setq repeat nil)
		;; Must break since too long.
		(setq poslist (cdr poslist))
		(if poslist
		    ;; There is a break point candidate.
		    (progn
		      (goto-char (car poslist))
		      (skip-chars-backward " \t"))
		  ;; Still too long, so insert a newline.
		  (backward-char
		   (max 1
			(/ (* (- (point) start)
			      (- (length chunk) mime-max-header-length))
			   (- (length chunk)
			      (length (car this-encoding))
			      (length (cdr this-encoding)) 6))))
		  ))
	      ))
	  )
	;; Fold line if necessary.
	;; BUG: An extra space may be inserted after a white space
	;; followed by non-ascii characters.
	(cond ((string-equal result "")
	       (setq saved chunk)
	       (setq separator ""))
	      ((and (or last-encoding this-encoding)
		    (> (length (concat saved chunk))
		       mime-max-header-length))
	       (setq saved (concat chunk " "))
	       (setq separator "\n "))
	      ;; (not (equal last-encoding this-encoding))
	      ((and last-encoding (not this-encoding)
		    (> (length (concat saved " " chunk))
		       mime-max-header-length))
	       (setq saved (concat chunk " "))
	       (setq separator "\n "))
	      ;; (not (equal last-encoding this-encoding))
	      ((and last-encoding (not this-encoding)
		    (string-match "^[^ \t]" chunk))
	       (setq saved (concat saved " " chunk))
	       (setq separator " "))
	      (t
	       (setq saved (concat saved chunk))
	       (setq separator "")))
	(setq result (concat result separator chunk)))
      (kill-buffer (current-buffer))
      result
      )))

(defun mime-encode-header-string (method string)
  "For given encoding METHOD encode STRING for use in header."
  (let* ((charset (car method))
	 (encoding (cdr method))
	 (qp (string-equal (downcase encoding) "q"))) ;quoted-printable?
    (cond ((and (stringp charset)
		(stringp encoding))
	   (setq string
		 (if qp (concat string "\n") string))
	   (let ((encoded (funcall mime-string-encoder method string)))
	     (setq encoded
		   (substring encoded 0 (1- (length encoded))))
	     (if (string-equal string encoded)
		 string
	       (concat "=?" charset "?" encoding "?" encoded "?="))))
	  ;; Return string without any encoding.
	  (t string)
	  )))

(defun mime-search-for-code-boundary ()
  "Search for code boundary starting from current point and return points."
  (let ((repeat t)
	(lastwhite nil)
	(poslist nil)			;possible boundary points
	(encoding (and (not (eobp))
		       (funcall mime-header-charset-chooser
				(point)
				(save-excursion
				  (forward-char 1) (point))))))
    (while repeat
      (while (and (not (eobp))
		  (equal encoding
			 (funcall mime-header-charset-chooser
				  (point)
				  (save-excursion
				    (forward-char 1) (point)))))
	(setq lastwhite nil)
	(forward-char 1))
      ;; Remember the candidate.
      (if (not lastwhite)
	  (setq poslist (cons (point) poslist)))
      ;; Iterate if the next of the next whitespace is the same.
      (if (looking-at "[ \t]")
	  (skip-chars-forward " \t")
	(setq repeat nil))
      (setq lastwhite t)
      )
    ;; Goto the last point.
    (goto-char (car poslist))
    poslist
    ))

(defun mime-translate-body ()
  "Encode the tagged MIME body in current buffer in MIME compliant message."
  (interactive)
  (save-excursion
    (let ((boundary
	   (concat mime-multipart-boundary " " (current-time-string)))
	  (tag nil)			;MIME tag
	  (contype nil)			;Content-Type
	  (encoding nil)		;Content-Transfer-Encoding
	  (nparts 0))			;Number of body parts
      (save-restriction
	;; We are interested in message body.
	(let* ((beg
		(progn
		  (goto-char (point-min))
		  (re-search-forward
		   (concat "\n" (regexp-quote mail-header-separator)
			   (if mime-ignore-preceding-spaces
			       "[ \t\n]*\n" "\n")) nil 'move)
		  (point)))
	       (end
		(progn
		  (goto-char (point-max))
		  (and mime-ignore-trailing-spaces
		       (re-search-backward "[^ \t\n]\n" beg t)
		       (forward-char 1))
		  (point))))
	  (narrow-to-region beg end))
	;; Normalize the body part by inserting appropriate message
	;; tags for every message contents.
	(mime-normalize-body)
	;; Counting the number of Content-Type.
	(goto-char (point-min))
	(while (re-search-forward mime-tag-regexp nil t)
	  (setq nparts (1+ nparts)))
	;; Begin translation.
	(cond ((<= nparts 1)
	       ;; It's a singular message.
	       (goto-char (point-min))
	       (while (re-search-forward mime-tag-regexp nil t)
		 (setq tag
		       (buffer-substring (match-beginning 0) (match-end 0)))
		 (delete-region (match-beginning 0) (1+ (match-end 0)))
		 (setq contype (mime-get-contype tag))
		 (setq encoding (mime-get-encoding tag))
		 ))
	      (t
	       ;; It's a multipart message.
	       (goto-char (point-min))
	       (while (re-search-forward mime-tag-regexp nil t)
		 (setq tag
		       (buffer-substring (match-beginning 0) (match-end 0)))
		 (delete-region (match-beginning 0) (match-end 0))
		 (setq contype (mime-get-contype tag))
		 (setq encoding (mime-get-encoding tag))
		 (insert "--" boundary "\n")
		 (insert "Content-Type: " contype "\n")
		 (if encoding
		     (insert "Content-Transfer-Encoding: " encoding "\n"))
		 )
	       ;; Define Content-Type as "multipart/mixed".
	       (setq contype
		     (concat "multipart/mixed; boundary=\"" boundary "\""))
	       ;; Content-Transfer-Encoding must be "7bit".
	       ;; The following encoding can be `nil', but is
	       ;; specified as is since there is no way that a user
	       ;; specifies it.
	       (setq encoding "7bit")
	       ;; Insert the trailer.
	       (goto-char (point-max))
	       (if (not (= (preceding-char) ?\n))
		   ;; Boundary must start with a newline.
		   (insert "\n"))
	       (insert "--" boundary "--\n")))
	)
      ;; Make primary MIME headers.
      (or (mail-position-on-field "Mime-Version")
	  (insert mime-version-number))
      ;; Remove old Content-Type and other fields.
      (save-restriction
	(goto-char (point-min))
	(search-forward (concat "\n" mail-header-separator "\n") nil t)
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(mime-delete-field "Content-Type")
	(mime-delete-field "Content-Transfer-Encoding"))
      ;; Then, insert Content-Type and Content-Transfer-Encoding fields.
      (mail-position-on-field "Content-Type")
      (insert contype)
      (if encoding
	  (progn
	    (mail-position-on-field "Content-Transfer-Encoding")
	    (insert encoding)))
      )))

(defun mime-normalize-body ()
  "Normalize the body part by inserting appropriate message tags."
  ;; Insert the first MIME tags if necessary.
  (goto-char (point-min))
  (if (not (looking-at mime-tag-regexp))
      (insert (mime-make-text-tag) "\n"))
  ;; Check each tag, and add new tag or correct it if necessary.
  (goto-char (point-min))
  (while (re-search-forward mime-tag-regexp nil t)
    (let* ((tag (buffer-substring (match-beginning 0) (match-end 0)))
	   (contype (mime-get-contype tag))
	   (charset (mime-get-parameter contype "charset"))
	   (encoding (mime-get-encoding tag)))
      ;; Remove extra whitespaces after the tag.
      (if (looking-at "[ \t]+$")
	  (delete-region (match-beginning 0) (match-end 0)))
      (cond ((= (following-char) ?\^M)
	     ;; It must be image, audio or video.
	     (let ((beg (point))
		   (end (mime-content-end)))
	       ;; Insert explicit MIME tags after hidden messages.
	       (forward-line 1)
	       (if (and (not (eobp))
			(not (looking-at mime-tag-regexp)))
		   (progn
		     (insert (mime-make-text-tag) "\n")
		     (forward-line -1)	;Process it again as text.
		     ))
	       ;; Show a hidden message.  The point is not altered
	       ;; after the conversion.
	       (mime-flag-region beg end ?\n)))
	    ((mime-test-content-type contype "message")
	     ;; Content-type "message" should be sent as is.
	     (forward-line 1))
	    ((mime-test-content-type contype "text")
	     ;; Define charset for text if necessary.
	     (setq charset (or charset (mime-choose-charset)))
	     (mime-define-charset charset)
	     ;; Point is now on current tag.
	     ;; Define encoding and encode text if necessary.
	     (if (null encoding)	;Encoding is not specified.
		 (let* ((encoding (mime-choose-encoding))
			(beg (mime-content-beginning))
			(end (mime-content-end))
			(body (buffer-substring beg end))
			(encoded (funcall mime-string-encoder
					  (cons charset encoding) body)))
		   (if (not (string-equal body encoded))
		       (progn
			 (goto-char beg)
			 (delete-region beg end)
			 (insert encoded)
			 (goto-char beg)))
		   (mime-define-encoding encoding)))
	     (forward-line 1))
	    ((null encoding)		;Encoding is not specified.
	     ;; Application, image, audio, video, and any other
	     ;; unknown content-type without encoding should be
	     ;; encoded.
	     (let* ((encoding "base64")	;Encode in BASE64 by default.
		    (beg (mime-content-beginning))
		    (end (mime-content-end))
		    (body (buffer-substring beg end))
		    (encoded (funcall mime-string-encoder
				      (cons nil encoding) body)))
	       (if (not (string-equal body encoded))
		   (progn
		     (goto-char beg)
		     (delete-region beg end)
		     (insert encoded)
		     (goto-char beg)))
	       (mime-define-encoding encoding))
	     (forward-line 1))
	    )
      )))

(defun mime-delete-field (field)
  "Delete header FIELD."
  (let ((regexp (format "^%s:[ \t]*" field)))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (delete-region (match-beginning 0)
		     (progn (forward-line 1) (point)))
      )))


;;;
;;; Platform dependent functions
;;;

;; Emacs 18 implementations

(defun mime-header-charset-chooser-for-emacs18 (begin end)
  "Return a cons of charset and header encoding for a given header region.
Encoding name must be a short name, such as `B' or `Q'."
  nil					;No encoding is necessary.
  )

(defun mime-body-charset-chooser-for-emacs18 (begin end)
  "Return a cons of charset and encoding of a message in a given region.
Encoding name must be a canonical name, such as `base64'."
  '("US-ASCII" . nil)			;Default charset of MIME.
  )

(defun mime-string-encoder-for-emacs18 (method string)
  "For given METHOD that is a cons of charset and encoding, encode a STRING."
  (let ((charset (car method))
	(encoding (cdr method)))
    (cond ((stringp encoding)
	   (mime-encode-string encoding string))
	  ;; Return string without any encoding.
	  (t string)
	  )))


;; Emacs 19 implementations

(defun mime-header-charset-chooser-for-emacs19 (begin end)
  "Return a cons of charset and header encoding for a given header region.
Encoding name must be a short name, such as `B' or `Q'.
US-ASCII and ISO-8859-1 are supported on Emacs 19."
  (cond ((save-excursion
	   (goto-char begin)
	   (re-search-forward "[\200-\377]" end t))
	 '("ISO-8859-1" . "Q"))
	(t nil)				;No encoding is necessary.
	))

(defun mime-body-charset-chooser-for-emacs19 (begin end)
  "Return a cons of charset and encoding of a message in a given region.
Encoding name must be a canonical name, such as `base64'.
US-ASCII and ISO-8859-1 are supported on Emacs 19."
  (cond ((save-excursion
	   (goto-char begin)
	   (re-search-forward "[\200-\377]" end t))
	 '("ISO-8859-1" . "quoted-printable"))
	(t
	 '("US-ASCII" . nil))		;Default charset of MIME.
	))

(defun mime-string-encoder-for-emacs19 (method string)
  "For given METHOD that is a cons of charset and encoding, encode a STRING."
  (let ((charset (car method))
	(encoding (cdr method)))
    (cond ((stringp encoding)
	   (mime-encode-string encoding string))
	  ;; Return string without any encoding.
	  (t string)
	  )))


;; NEmacs implementations

(defun mime-header-charset-chooser-for-nemacs (begin end)
  "Return a cons of charset and header encoding for a given header region.
Encoding name must be a short name, such as `B' or `Q'.
US-ASCII and ISO-2022-JP are supported on NEmacs."
  (cond ((check-region-kanji-code begin end)
	 '("ISO-2022-JP" . "B"))
	(t nil)				;No encoding is necessary.
	))

(defun mime-body-charset-chooser-for-nemacs (begin end)
  "Return a cons of charset and encoding of a message in a given region.
Encoding name must be a canonical name, such as `base64'.
US-ASCII and ISO-2022-JP are supported on NEmacs."
  (cond ((check-region-kanji-code begin end)
	 ;; The following are safe encoding methods for use in
	 ;; USENET News systems that strip off all ESCs.
	 ;; '("ISO-2022-JP" . "quoted-printable")
	 ;; '("ISO-2022-JP" . "base64")
	 ;; The following expects transport systems are all MIME
	 ;; compliants.  For instance, ESCs are never stripped off.
	 '("ISO-2022-JP" . nil))
	(t
	 '("US-ASCII" . nil))		;Default charset of MIME.
	))

(defun mime-string-encoder-for-nemacs (method string)
  "For given METHOD that is a cons of charset and encoding, encode a STRING.
US-ASCII and ISO-2022-JP are supported on NEmacs."
  (let ((charset (car method))
	(encoding (cdr method)))
    (cond ((stringp encoding)
	   (mime-encode-string encoding
			       ;; Convert internal (EUC) to JIS code.
			       (convert-string-kanji-code string 3 2)
			       ))
	  ;; NEmacs can convert into ISO-2022-JP automatically,
	  ;; but can do it myself as follows:
	  ;;(t (convert-string-kanji-code string 3 2))

	  ;; Return string without any encoding.
	  (t string)
	  )))


;; Mule implementations
;; Thanks to contributions by wkenji@flab.fujitsu.co.jp (Kenji
;; WAKAMIYA) and handa@etl.go.jp (Kenichi Handa).

(defun mime-header-charset-chooser-for-mule (begin end)
  "Return cons of charset and header encoding for a given header
region.  Encoding name must be a short name, such as `B' or `Q'.
US-ASCII, ISO-8859-* (except for ISO-8859-6), ISO-2022-JP,
ISO-2022-JP-2 and ISO-2022-INT-1 are supported on Mule."
  (let ((lclist (find-charset-region begin end)))
    (cond ((null lclist) nil)		;US-ASCII requres no encoding.
	  ((memq lc-ltn1 lclist)
	   '("ISO-8859-1" . "Q"))
	  ((memq lc-ltn2 lclist)
	   '("ISO-8859-2" . "Q"))
	  ((memq lc-ltn3 lclist)
	   '("ISO-8859-3" . "Q"))
	  ((memq lc-ltn4 lclist)
	   '("ISO-8859-4" . "Q"))
	  ((memq lc-crl lclist)
	   '("ISO-8859-5" . "Q"))
	  ;;((memq lc-arb lclist)
	  ;; '("ISO-8859-6" . "Q"))
	  ((memq lc-grk lclist)
	   '("ISO-8859-7" . "Q"))
	  ((memq lc-hbw lclist)
	   '("ISO-8859-8" . "Q"))
	  ((memq lc-ltn5 lclist)
	   '("ISO-8859-9" . "Q"))
	  ((memq lc-jp lclist)
	   '("ISO-2022-JP" . "B"))
	  ;; Unknown charset.
	  ((boundp '*iso-2022-int-1*)
	   '("ISO-2022-INT-1" . "B"))
	  (t
	   '("ISO-2022-JP-2" . "B"))
	  )))

(defun mime-body-charset-chooser-for-mule (begin end)
  "Return a cons of charset and encoding of a message in a given
region.  Encoding name must be a canonical name, such as `base64'.
US-ASCII, ISO-8859-* (except for ISO-8859-6), ISO-2022-JP,
ISO-2022-JP-2 and ISO-2022-INT-1 are supported on Mule.  Either of
charset ISO-2022-JP-2 or ISO-2022-INT-1 is used for multilingual text
in Mule."
  (let ((lclist (find-charset-region begin end)))
    (cond ((null lclist)
	   '("US-ASCII" . nil))		;Default charset of MIME.
	  ;; Multilingual capability requred.
	  ((and (> (length lclist) 1)
		(boundp '*iso-2022-int-1*))
	   '("ISO-2022-INT-1" . nil))
	  ((> (length lclist) 1)
	   '("ISO-2022-JP-2" . nil))
	  ;; Simple charset.
	  ((memq lc-ltn1 lclist)
	   '("ISO-8859-1" . "quoted-printable"))
	  ((memq lc-ltn2 lclist)
	   '("ISO-8859-2" . "quoted-printable"))
	  ((memq lc-ltn3 lclist)
	   '("ISO-8859-3" . "quoted-printable"))
	  ((memq lc-ltn4 lclist)
	   '("ISO-8859-4" . "quoted-printable"))
	  ((memq lc-crl lclist)
	   '("ISO-8859-5" . "quoted-printable"))
	  ;;((memq lc-arb lclist)
	  ;; '("ISO-8859-6" . "quoted-printable"))
	  ((memq lc-grk lclist)
	   '("ISO-8859-7" . "quoted-printable"))
	  ((memq lc-hbw lclist)
	   '("ISO-8859-8" . "quoted-printable"))
	  ((memq lc-ltn5 lclist)
	   '("ISO-8859-9" . "quoted-printable"))
	  ((memq lc-jp lclist)
	   '("ISO-2022-JP" . nil))
	  ;; Unknown charset.
	  ((boundp '*iso-2022-int-1*)
	   '("ISO-2022-INT-1" . nil))
	  (t
	   '("ISO-2022-JP-2" . nil))
	  )))

(defun mime-string-encoder-for-mule (method string)
  "For given METHOD that is a cons of charset and encoding, encode a
STRING.  US-ASCII, ISO-8859-* (except for ISO-8859-6), ISO-2022-JP,
ISO-2022-JP-2 and ISO-2022-INT-1 are supported on Mule.  Either of
charset ISO-2022-JP-2 or ISO-2022-INT-1 is used for multilingual
text."
  (let* ((charset (car method))
	 (encoding (cdr method))
	 (coding-system
	  (cdr (assoc (and (stringp charset) (upcase charset))
		      '(("ISO-8859-1" . *ctext*)
			("ISO-8859-2" . *iso-8859-2*)
			("ISO-8859-3" . *iso-8859-3*)
			("ISO-8859-4" . *iso-8859-4*)
			("ISO-8859-5" . *iso-8859-5*)
			;;("ISO-8859-6" . *iso-8859-6*)
			("ISO-8859-7" . *iso-8859-7*)
			("ISO-8859-8" . *iso-8859-8*)
			("ISO-8859-9" . *iso-8859-9*)
			("ISO-2022-JP" . *junet*)
			("ISO-2022-JP-2" . *iso-2022-ss2-7*)
			("ISO-2022-INT-1" . *iso-2022-int-1*)
			)))))
    ;; In bilingual environment it may be unnecessary to convert the
    ;; coding system of the string unless transfer encoding is
    ;; required since such conversion may be performed by mule
    ;; automatically.
    (if (not (null coding-system))
	(setq string (code-convert-string string *internal* coding-system)))
    (if (stringp encoding)
	(setq string (mime-encode-string encoding string)))
    string
    ))


;; Sun implementations

(defun mime-voice-recorder-for-sun ()
  "Record voice in a buffer using Sun audio device, and return the buffer.
If the environment variable AUDIOHOST is defined, its value is used as
a recording host instead of local host."
  (let ((buffer (get-buffer-create " *MIME audio*"))
	(host (getenv "AUDIOHOST")))
    (message "Start the recording on %s.  Type C-g to finish the recording..."
	     (or host (system-name)))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (condition-case errorcode
	  (let ((selective-display nil) ;Disable ^M to nl translation.
		(mc-flag nil)		;Mule
		(kanji-flag nil))	;NEmacs
	    ;; If AUDIOHOST is defined, use the value as recording host.
	    (cond ((not (null host))
		   ;; Disable automatic conversion of coding system if Mule.
		   (if (featurep 'mule)
		       (define-program-coding-system nil "rsh" *noconv*))
		   (call-process "rsh"
				 nil
				 buffer
				 nil
				 host
				 "cat"
				 "/dev/audio"
				 ))
		  (t
		   ;; Disable automatic conversion of coding system if Mule.
		   (if (featurep 'mule)
		       (define-program-coding-system nil "cat" *noconv*))
		   (call-process "cat"
				 "/dev/audio"
				 buffer
				 nil
				 ))))
	(quit (message "Type C-g to finish recording... done.")
	      buffer			;Return the buffer
	      )))))


;;;
;;; Other useful commands.
;;;

;; Message forwarding commands as content-type "message/rfc822".

;;;###autoload
(defun mime-forward-from-rmail-using-mail ()
  "Forward current message in message/rfc822 content-type message from rmail.
The message will be appended if being composed."
  (interactive)
  ;;>> this gets set even if we abort. Can't do anything about it, though.
  (rmail-set-attribute "forwarded" t)
  (let ((initialized nil)
	(beginning nil)
	(forwarding-buffer (current-buffer))
	(subject (concat "["
			 (mail-strip-quoted-names (mail-fetch-field "From"))
			 ": " (or (mail-fetch-field "Subject") "") "]")))
    ;; If only one window, use it for the mail buffer.
    ;; Otherwise, use another window for the mail buffer
    ;; so that the Rmail buffer remains visible
    ;; and sending the mail will get back to it.
    (setq initialized
	  (if (one-window-p t)
	      (mail nil nil subject)
	    (mail-other-window nil nil subject)))
    (save-excursion
      (goto-char (point-max))
      (forward-line 1)
      (setq beginning (point))
      (mime-insert-tag "message" "rfc822")
      (insert-buffer forwarding-buffer))
    (if (not initialized)
	(goto-char beginning))
    ))

;;;###autoload
(defun mime-forward-from-gnus-using-mail ()
  "Forward current article in message/rfc822 content-type message from GNUS.
The message will be appended if being composed."
  (let ((initialized nil)
	(beginning nil)
	(forwarding-buffer (current-buffer))
	(subject
	 (concat "[" gnus-newsgroup-name "] "
		 ;;(mail-strip-quoted-names (gnus-fetch-field "From")) ": "
		 (or (gnus-fetch-field "Subject") ""))))
    ;; If only one window, use it for the mail buffer.
    ;; Otherwise, use another window for the mail buffer
    ;; so that the Rmail buffer remains visible
    ;; and sending the mail will get back to it.
    (setq initialized
	  (if (one-window-p t)
	      (mail nil nil subject)
	    (mail-other-window nil nil subject)))
    (save-excursion
      (goto-char (point-max))
      (setq beginning (point))
      (mime-insert-tag "message" "rfc822")
      (insert-buffer forwarding-buffer)
      ;; You have a chance to arrange the message.
      (run-hooks 'gnus-mail-forward-hook)
      )
    (if (not initialized)
	(goto-char beginning))
    ))

(provide 'mime)

;;; mime.el ends here
