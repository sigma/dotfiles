;;; pmwiki-mode.el --- edit PmWiki pages within Emacs
;;
;; Copyright (C) 2003  Christian Ridderström
;;
;; Author:  Christian Ridderström <christian.ridderstrom@home.se>
;; Maintainer: Christian Ridderström <christian.ridderstrom@home.se>
;; Release: $Name:  $
;; Revision: $Revision: 1.1 $
;; Keywords: hypermedia
;; URI: http://www.pmichaud.com/wiki/EmacsModes/Pmwiki-mode
;; 
;;
;; This file is not part of GNU Emacs.
;;
;;; License:
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; Background:
;;
;; `pmwiki-mode' was created because in order to easily edit wiki
;; pages at http://wiki.lyx.org (also see http://www.lyx.org). This
;; site the wiki engine PmWiki (see http://www.pmichaud.com/wiki).
;;
;; `pmwiki-mode' is based on the source of `simple-wiki-mode', i.e.
;; 	simple-emacswiki.el, simple-wiki-edit.el and simple-wiki.el.
;; `simple-wiki-mode' was written by Alex Schroeder <alex@gnu.org>
;; (see http://www.emacswiki.org/cgi-bin/wiki.pl/SimpleWikiEditMode).
;;
;;
;; Requirements:
;;
;; This file has been tested with Emacs 21.2.1 and requires these
;; additional packages:
;;	`http-post'	http://www.emacswiki.org/cgi-bin/wiki.pl/HttpPost
;;	`http-get'	http://www.emacswiki.org/cgi-bin/wiki.pl/HttpGet
;;
;; In addition, this package is useful:
;;	`longlines'	http://www.emacswiki.org/cgi-bin/wiki.pl/LongLines
;;
;; If you downloaded this file as a tar.gz-file, you should already
;; have these packages. Note that the version of `longlines' in the
;; archive has been patched --- this line has been added to the end:
;;	(provide 'longlines)
;; Similarly, a minor change has been done to the `http-post' and
;; `http-get' so that *HTTP...-buffers are not put at the front of the
;; list of recently selected buffers.
;;
;; Installation:
;;
;; 1. Assuming that this file is located in the directory
;;
;;	~/emacs/pmwiki-mode
;;
;;    add these lines to your .emacs-file:
;;
;;	(add-to-list 'load-path "~/emacs/pmwiki-mode")
;;	(require 'pmwiki-mode)
;;
;; 2. Optionally, use the command `byte-compile' to compile this file.
;; 3. Restart Emacs.
;;
;;
;; Usage:
;;
;; Two examples to get you started:
;; * Use the command `pmichaud-open' with the argument
;;	Main.WikiSandbox
;;   to download the page Main.WikiSandbox from the PmWiki wiki.
;;
;; * Use the command `pmwiki-open' to open a page by specifying
;;   its URI. For example, to open the page where this mode is
;;   described, give the following URI as argument to the command:
;;
;;	http://www.pmichaud.com/wiki/Cookbook/EmacsPmWikiMode
;;
;; Enjoy. /Christian
;;
;;
;; Configuration options:
;;
;; Note that the code below is meant to be inserted in .emacs (or an
;; equivalent file).
;; 
;; If you want to use any of the configuration examples below, first
;; insert this line of code:
;;
;;	(defvar pmwiki-mode-hooks)
;;
;; If your auto-fill-mode is usually on, you might want to turn it off:
;;
;;	(add-hook 'pmwiki-mode-hooks 'turn-off-auto-fill)
;;	
;;
;; Also consider using the package longlines.el, for details see
;;   http://www.emacswiki.org/cgi-bin/wiki.pl/LongLines 
;;
;; 	(add-hook 'pmwiki-mode-hooks
;;	  'pmwiki-delayed-hook-longlines-mode-on)
;;	(add-hook 'pmwiki-save-before-hooks 'longlines-mode-off)
;;
;; in which case, also customize `pmwiki-fill-column' back to 70.
;; Note that you may have to patch the file longlines.el by adding
;; 	(provide 'longlines)
;; to the end of it. As an alternative configuration, this code also
;; makes the `longlines-mode' show the hard newlines.
;;
;; 	(add-hook 'pmwiki-mode-hooks
;;	  'pmwiki-delayed-hook-longlines-mode-showing-hard-newlines)
;;	(add-hook 'pmwiki-save-before-hooks 'longlines-mode-off)
;;
;; Note: PmWiki supports using '\' as line continuation character,
;; i.e. if it is the the last character on the line the linebreak is
;; ignored. This is not supported.
;;
;; !!!Author name
;;
;; The name of the author who modified the page is set through the
;; variable `pmwiki-author'. To configure this, either customize the
;; variable or set it from the your initialization script like this:
;;
;;	(set 'pmwiki-author "chr")
;;
;; !!!File extension for PmWiki-files
;;
;; (add-to-list 'auto-mode-alist '("\\.pmwiki\\'" . pmwiki-mode))
;;

;;; Code:
;; Are we running XEmacs or Emacs?
(defvar pmwiki-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
  "Indicates if XEmacs (t) or Emacs (nil) is used")
(defvar pmwiki-running-emacs (not pmwiki-running-xemacs)
  "Indicates if Emacs (t) or XEmacs (nil) is used")

(require 'thingatpt)
(require 'http-get)
(require 'http-post)
;;
;; Fix required for XEmacs prior to 21.5

(unless (fboundp 'replace-regexp-in-string)
(defun replace-regexp-in-string (regexp rep string &optional
                                 fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING.
Return a new string containing the replacements.
Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.
REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.
To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"
  ;; To avoid excessive consing from multiple matches in long strings,
  ;; don't just call `replace-match' continually.  Walk down the
  ;; string looking for matches of REGEXP and building up a (reversed)
  ;; list MATCHES.  This comprises segments of STRING which weren't
  ;; matched interspersed with replacements for segments that were.
  ;; [For a `large' number of replacments it's more efficient to
  ;; operate in a temporary buffer; we can't tell from the function's
  ;; args whether to choose the buffer-based implementation, though it
  ;; might be reasonable to do so for long enough STRING.]
  (let ((l (length string))
        (start (or start 0))
        matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
        (setq mb (match-beginning 0)
              me (match-end 0))
        ;; If we matched the empty string, make sure we advance by one char
        (when (= me mb) (setq me (min l (1+ mb))))
        ;; Generate a replacement for the matched substring.
        ;; Operate only on the substring to minimize string consing.
        ;; Set up match data for the substring for replacement;
        ;; presumably this is likely to be faster than munging the
        ;; match data directly in Lisp.
        (string-match regexp (setq str (substring string mb me)))
        (setq matches
              (cons (replace-match (if (stringp rep)
                                       rep
                                     (funcall rep (match-string 0 str)))
                                   fixedcase literal str subexp)
                    (cons (substring string start mb) ; unmatched prefix
                          matches)))
        (setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))
)

;; Fix required for XEmacs since `string-bytes' doesn't exist
(unless (fboundp 'string-bytes)
  (defun string-bytes (string)
    "Fake replacement for Emacs `string-bytes'. This function fails for
multibyte characters."
    (length string)))

;; ----------------------------------------------------------------------
;;
;;	Preliminaries
;;
;; Define variables with various patterns
;;
(defvar pmwiki-groupname-pattern "[A-Z][A-Za-z0-9]+"
  "Pattern of a valid group name (for PmWiki)")

(defvar pmwiki-wikiword-pattern
  "[A-Z][A-Za-z0-9]*\\(?:[A-Z][a-z0-9]\\|[a-z0-9][A-Z]\\)[A-Za-z0-9]*"
  "Pattern that defines a WikiWord in a PmWiki-page.")

(defvar pmwiki-pagetitle-pattern
  "[A-Z][A-Za-z0-9]*\\(?:-[A-Za-z0-9]+\\)*"
  "Pattern of a valid page name (for PmWiki)")

(defvar pmwiki-HTTP-pattern
  "^[hH][tT][tT][pP][sS]?:"
  "Pattern matching 'http:' (case insensitive) at the beginning of a string.")

;; ...(<GroupName>)[/.](<PageTitle>)(?|$)
(defvar pmwiki-URI-pattern1
  (concat "\\(" pmwiki-HTTP-pattern ".+\\)/"    ; Match 1
	  "\\(" pmwiki-groupname-pattern "\\)/" ; Match 2
	  "\\(" pmwiki-pagetitle-pattern "\\)" 	; Match 3
	  "\\(?:\\?\\|$\\)" )
  "This pattern is used on `pmwiki-page-uri' to extract various parts.
The parts matches \"<BaseURI>/<group>/<page>?\". where
 1 - <BaseURI>, 2 - <group> and 3 - <page>.
The parts of the pattern are derived from `pmwiki-groupname-pattern'
and `pmwiki-pagetitle-pattern'.")
	     
;; ...(<GroupName>)[/.](<PageTitle>)(?|$)
(defvar pmwiki-URI-pattern2
  (concat "\\(" pmwiki-HTTP-pattern ".+\\)\\?pagename=" ; Match 1
	  "\\(" pmwiki-groupname-pattern "\\)"	; Match 2
	  "[./]"
	  "\\(" pmwiki-pagetitle-pattern "\\)" 	; Match 3
	  "\\(?:&\\|$\\)" )
  "This alternative pattern is used on `pmwiki-page-uri' to extract 
various parts.
The parts matches \"<BaseURI>?pagename=<group>/<page>?\". where
 1 - <BaseURI>, 2 - <group> and 3 - <page>.
The parts of the pattern are derived from `pmwiki-groupname-pattern'
and `pmwiki-pagetitle-pattern'.")


(defvar pmwiki-fragment-pattern
  "#[A-Za-z][-.:\\w]*"
  "Pattern that defines an appended #-target to a page.")

;; Link patterns:
;; 	\b(<GroupName>[/.])?<FreeLink><Fragment>
;;	<FreeLink><Fragment>?
;;	\b<GroupName>[/.]<WikiWord><Fragment>?
;;	\[\[#([A-Za-z][-.:\w]*?\]\]		; que?
;;	\b<WikiWord><Fragment>?

;; Pattern of WikiWord-link: [<GroupName>{.|/}]<WikiWord>
(defvar pmwiki-link-pattern
  (concat 
   "\\<"
   "\\(?:\\(" pmwiki-groupname-pattern "\\)[./]\\)?" 
   "\\(" pmwiki-wikiword-pattern "\\)" )
  "The pattern used for finding WikiWords.")

;; Pattern of a free link (from .php-file)
;; "{{(?>"
;;    "([A-Za-z][A-Za-z0-9]*"
;;	"(?:"
;;	    "(?:[\\s_]*|-)"
;;	    "[A-Za-z0-9]+"
;;      ")*"
;;    ")"
;;	"(?:\\|((?:(?:[\\s_]*|-)[A-Za-z0-9])*))?"
;; ")}}"
;;    "((?:-?[A-Za-z0-9]+)*)";
;;
;; Note: The free link name will be transformed as follows into a page name:
;;	[_\\s]+  ->  '' (i.e. nothing, unless $PageNameSpace='_' for instance)
(defvar pmwiki-free-link-pattern
  (concat 
   "{{\\(\\([A-Za-z][A-Za-z0-9]*"
   "\\(?:\\(?:[	 _-]*\\|-\\)[A-Za-z0-9]+\\)*\\)"
   "\\(?:|\\(\\(?:\\(?:[ 	_-]*\\|-\\)[A-Za-z0-9]\\)*\\)\\)?\\)}}"
   "\\(\\(?:-?[A-Za-z0-9]+\\)*\\)")
  "Pattern of a {{free link}} in PmWiki-page")

;;
;; 

;;
;; Other patterns
;;
(defvar pmwiki-style-pattern
  "%%\\|%[A-Za-z][-,=#\\w\\s]*%"
  "Pattern for PmWiki style tags.")

(defvar pmwiki-password-URI-pattern
  "http://\\(.*?:.*?\\)@\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?/\\(.*/\\)?\\([^:]*\\)"
  "Pattern matching a URI like this:
	http://user:password@www.pmwiki.org/Main/WikiSandbox")

;; ----------------------------------------------------------------------
;;
;; Define variables with some default URI's
;;

(defvar pmichaud-sandbox-uri "http://www.pmichaud.com/wiki/Main/WikiSandbox"
  "URI to sandbox at http://www.pmichaud.com")

(defvar pmwiki-default-uri pmichaud-sandbox-uri
  "Default URI used by some interactive functions")

(defvar pmwiki-open-uri-default pmichaud-sandbox-uri
  "Default URI used by `pmwiki-open-uri'.")

(defvar lyx-wiki-base-uri "http://wiki.lyx.org/pmwiki.php"
  "Base URI of the Lyx Wiki site")

(defvar lyx-homepage-uri (concat lyx-wiki-base-uri "/Main/HomePage")
  "URI for the home page of the LyX wiki")

;; ----------------------------------------------------------------------
;;
;; Define general variables 
;;
;; `pmwiki-page-uri' becomes local to a buffer in `pmwiki-mode'
(defvar pmwiki-page-uri nil
  "The URI of the page corresponding to the current buffer, thus defining
the base URI of the wiki engine as well as group and page name.")

(defvar pmwiki-encoded-password nil
  "The password when authorization is required")

;;
;; There are too many "URI's" now... basically we have
;; 'base URI' -> the PmWiki-script, e.g. http://wiki.lyx.org/pmwiki.php
;; 'post URI' -> to where the form with a page is posted, same as 'base URI'
;; 'page URI' -> the page, e.g. http://wiki.lyx.org/pmwiki.php/Main/WikiWord
;; 'source URI' -> source of PmWiki-page, e.g.
;;		http://wiki.lyx.org/pmwiki.php/Main/WikiWord?action=source
;;
;; I'm thinking of storing the location data as a list, location-list
;; This requires conversion methods:
;;	data    -> pageURI	=> (pmwiki-loc 'URI  listData)
;;	        -> baseURI	=> (pmwiki-loc 'base listData)
;;	pageURI -> data		=> (pmwiki-uri-to-loc-list URI)
;;	srcURI  -> data		=> (pmwiki-uri-to-loc-list URI)
;;(defvar pmwiki-location-list nil
;;  "List with three strings, (<baseURI> <group> <page>), specifying
;;the base URI of the wiki engine as well as a group name and a page name.")
;;

(defvar pmwiki-save-function 'pmwiki-default-save-function
  "Specifies the function used to save a PmWiki-buffer.")

(defcustom pmwiki-fill-column 70 "Fill column width for `pmwiki-mode'.")
(defcustom pmwiki-mode-hooks nil "")
(defcustom pmwiki-save-before-hooks nil "")

(defcustom pmwiki-author "" "\"Name\" that shows up as author of the
changes when a page is saved from `pmwiki-mode'.")

;; ----------------------------------------------------------------------
;;
;;	Auxiliary helper function
;;
(defun pmwiki-default-string (s s0)
  "Return S if not empty, otherwise return S0"
  (if (string= s "") s0 s))

(defun pmwiki-URIp (name)
  "Returns t if NAME matches a URI pattern 
If NAME starts with 'http:', it must match `pmwiki-URI-pattern1' or
`pmwiki-URI-pattern2', otherwise an error is thrown."
  (let ((case-fold-search nil))
    (if (string-match pmwiki-HTTP-pattern name)
	(if (string-match pmwiki-URI-pattern1 name) t
	  (if (string-match pmwiki-URI-pattern2 name) t
	    (error
	     "pmwiki-uri-type: %s does not match a proper URI pattern" name)))
      nil)))



;; ----------------------------------------------------------------------
;;
;;	Main code
;;
;; Define the `pmwiki-mode'. 
;;
;; Have to modify patterns for faces. List of faces:
;; builtin, comment, constant, doc, function, keyword, string, type, variable
;; warning
;;	[=...=]			- string
;;	[[#<target>]]		- constant
;;	[[<uri> <text>]]	- type
;;	{{link}}		- keyword
;;	Wiki-links		- keyword
;;  ;;	 <monospace>		- comment  (not used anymore)
;;	@@<monospace>@@		- comment
;;	''<emp>''		- type
;;	'''<strong>'''		- function
;;	'''''<extra>'''''	- warning
;;	*...			- builtin
;;	#...			- doc
;;	!...			- function
;;	:..: : <indented par.>	- doc
;;
;; Maybe remove sgml-tag-alist?

;; Special faces
(defface pmwiki-question-face
  '((t (:bold t)))
  "Used to indicate a question in a PmWiki page.")

(defface pmwiki-answer-face
  '((t (:bold t :italic t)))
  "Used to indicate an answer in a PmWiki page.")

(define-derived-mode pmwiki-mode text-mode "PmWiki"
  "Mode to edit PmWiki pages. Typically you start by using the
function `pmwiki-open' to open a URI, e.g.
	http://www.pmichaud.com/wiki/Main/HomePage

Alternatively, the same page can be opened with `pmichaud-open' by
giving 'Main.HomePage' or 'Main/HomePage' as argument. Similarly, a
page at the LyX wiki can be opened using `lyx-open'.

Once a page has been opened, you can edit it and then save it using
`pmwiki-save' or `pmwiki-save-as'. The latter function allows you to
save the page with a new name, and even on a different wiki engine by
giving an URI as argument.

To view the wiki page that you are editing in a browser, first save it
and then use the command `pmwiki-browse'.

While editing the page, you can use `pmwiki-next' and `pmwiki-prev' to
quickly go to the next and previous links. And when the point is on a
link, you can use `pmwiki-follow' to goto the page that the link
points to, i.e. that page is opened in a new buffer.

Interactive functions:
`pmwiki-open'    - Open wiki page specified by group/name or URI
`pmwiki-next'    - Goto the next WikiWord or {{free link}}.
`pmwiki-prev'    - Goto the previous WikiWord or {{free link}}.
`pmwiki-follow'  - Open (goto) page pointed to by link at point.
`pmwiki-save'    - Save current buffer to the wiki engine.
`pmwiki-save-as' - Save buffer to new page specified by group/name or URI.
`pmwiki-reload'  - Reload the page.
`pmwiki-browse'  - Show current buffer in browser.
`pmwiki-search'  - Search the wiki site for a text.
`pmwiki-map-site'  - Generate a map of the site (all groups and pages).
`pmwiki-delete'	 - Delete a page
`pmwiki-rename'	 - Save a page under a different name and delete the original

`pmichaud-open'  - Open page from the Pmichaud wiki.
`lyx-open'       - Open a page from the LyX wiki.
`lyx-save-as'    - Save current buffer to page at the LyX wiki.

\\{pmwiki-mode-map}"
  (when pmwiki-running-emacs
    (font-lock-add-keywords
     nil
     '(
;;       ("^[ \t].+?$" . font-lock-comment-face)		;  <monospace>
       ("\\[=[^=]*=\\]" . font-lock-function-name-face)	; [= ... =]
       ("@@[^ \t@][^@]*[^ \t@]@@" . font-lock-comment-face) ; @@a .. b@@
       ("\\(^[*#]+\\)[^*#]" 1 font-lock-constant-face)	; {*|#}+ ...
       ("^!+[^!].*" . font-lock-constant-face)		; !+ ...
       ("\\[\\[<<\\]\\]" . font-lock-comment-face)	; [[<<]]
       ("\\[\\[#[^]]+\\]\\]" . font-lock-constant-face)	; [[#<target>]]
       ("\\\\$" . font-lock-keyword-face)		;     \
       ("\\\[\[[^#].*\]\]" . font-lock-type-face)	; [[ ... ]]
       ("{{[^}]*}}\\(\\(?:-?[A-Za-z0-9]+\\)*\\)" . font-lock-keyword-face)
       ("^:+ +:" . font-lock-doc-face)			; :..: : 
       ("^:[ \t]*[^ \t:]+[ \t]*+:" . font-lock-string-face) ; : term : 
       ("%[^ \t%][^%]*[^ \t%]%" . font-lock-warning-face) ; %color = red%
       ("^Q:" . 'pmwiki-question-face)			; Q:
       ("^A:" . 'pmwiki-answer-face)			; Q:
       )))

  (when pmwiki-running-emacs
    (font-lock-add-keywords
     nil
     (list (cons (symbol-value 'pmwiki-link-pattern) 'font-lock-keyword-face))
     ))

  (when pmwiki-running-emacs (font-lock-mode 1))
  (goto-address)
  (set (make-local-variable 'sgml-tag-alist)
       `(("b") ("code") ("em") ("i") ("strong") ("nowiki")
	 ("pre" \n) ("tt") ("u")))
  (set (make-local-variable 'skeleton-transformation) 'identity)
  (set (make-local-variable 'pmwiki-search-mode-status) nil)
  (setq indent-tabs-mode t)
  (run-hooks 'pmwiki-mode-hooks))

(define-key pmwiki-mode-map (kbd "C-c C-p") 'pmwiki-prev)
(define-key pmwiki-mode-map (kbd "C-c C-n") 'pmwiki-next)
(define-key pmwiki-mode-map (kbd "C-c C-f") 'pmwiki-follow)
(define-key pmwiki-mode-map (kbd "C-c C-g") 'pmwiki-follow)
(define-key pmwiki-mode-map (kbd "C-c C-o") 'pmwiki-open)
(define-key pmwiki-mode-map (kbd "C-c C-c") 'pmwiki-save)
(define-key pmwiki-mode-map (kbd "C-c C-w") 'pmwiki-save-as)
(define-key pmwiki-mode-map (kbd "C-c C-b") 'pmwiki-browse)
(define-key pmwiki-mode-map (kbd "C-c C-m") 'pmwiki-map-site)
(define-key pmwiki-mode-map (kbd "C-c C-s") 'pmwiki-search)
(define-key pmwiki-mode-map (kbd "C-c C-r") 'pmwiki-reload)
(define-key pmwiki-mode-map (kbd "C-c m")   'pmwiki-minor-mode)


;; ----------------------------------------------------------------------
;;
;; `pmwiki-edit' - main function used to load a PmWiki-page into a buffer
;;
;;
;;;###autoload
(defun pmwiki-edit (link &optional base-uri goto-end http-ver content-type)
  "Download the wiki page specified by LINK and BASE-URI.
If BASE-URI is not given, it's taken from `pmwiki-page-uri'.
Then change the buffer's mode to `pmwiki-mode'.
If GOTO-END is nil, wait until the process is done and move point to
the beginning of the buffer."
  (unless content-type (setq content-type 'iso-8859-1))
  (let ((page-uri (pmwiki-create-page-uri link base-uri))
	(proc nil)
	(headers '(("Connection" "close")))
	(userAndPassword nil))
	(when (string-match pmwiki-password-URI-pattern page-uri)
	  (setq userAndPassword (base64-encode-string
				 (match-string 1 page-uri)))
	  (setq page-uri (replace-regexp-in-string
			  "http://.*?:.*?@" "http://" page-uri))
	  (add-to-list 'headers (list "Authorization"
				      (concat "Basic " userAndPassword))))
	(set 'proc
	     (http-get (concat page-uri "&action=source") ; Source URI
		       headers		; headers
		       nil		; ?
		       http-ver		; nil -> 1.0
		       (pmwiki-link-to-name link) ; output buffer
		       content-type)	) ; typically nil
	(pmwiki-mode)
	(set-fill-column pmwiki-fill-column)
	(set (make-local-variable 'pmwiki-page-uri) page-uri)
	(set (make-local-variable 'pmwiki-encoded-password) userAndPassword)
	(unless goto-end
	  (while (string-equal (process-status proc) "open") (sleep-for 0.050))
	  (goto-char 1))
	(not-modified)
	proc))


(defun pmwiki-parse-uri (P uri)
  "Helper function to `pmwiki-loc'. P must be a number:
	1  -> <BaseURI>
	2  -> <group>
	3  -> <page>"
  (let ((case-fold-search nil))
    (if (string-match pmwiki-URI-pattern1 uri)
	(match-string P uri)
      (if (string-match pmwiki-URI-pattern2 uri)
	  (match-string P uri)
	(error "pmwiki-parse-uri: Cannot determine part %d in %s" P uri)))))
  

(defun pmwiki-loc (P &optional uri)
  "Extract BaseURI, GroupName or PageName from URI.
If URI is not given, `pmwiki-page-uri' is used instead.
`pmwiki-URI-pattern1' is used to match to different parts of URI:
	URI = \"<BaseURI>/<group>/<page>?...\"
where part, P, determines what is returned according to this list:
	'base	-> <BaseURI>
	'group	-> <group>
	'page	-> <page>
	'link	-> (<group> . <page>).
	'name   -> \"<group>.<page>\"
"
  (unless uri (set 'uri pmwiki-page-uri))
  (unless uri (error "No URI given, set `pmwiki-page-uri' first!"))
  (unless (symbolp P) (error "Invalid type of argument"))
  (let ((s (symbol-name P)))
    (if (string-equal s "base") (pmwiki-parse-uri 1 uri)
      (if (string-equal s "group") (pmwiki-parse-uri 2 uri)
	(if (string-equal s "page") (pmwiki-parse-uri 3 uri)
	  (if (string-equal s "link")
	      (cons (pmwiki-parse-uri 2 uri) (pmwiki-parse-uri 3 uri))
	    (if (string-equal s "name")
		(pmwiki-link-to-name (pmwiki-loc 'link uri))
	      (error "Invalid argument 'P' of `pmwiki-loc'!"))
	    ))))))


(defun pmwiki-create-page-uri (link &optional base-uri)
  "Create URI for a page specified by LINK,  with WikiEngine at BASE-URI.
The parameter link LINK is (cons <group-name> <page-name>).
The parameter BASE-URI is an optional URI to the WikiEngine and
taken from `pmwiki-page-uri' if BASE-URI is not given."
  (unless base-uri (set 'base-uri (pmwiki-loc 'base)))
  (unless base-uri
    (error "pmwiki-create-page-uri: No suitable BASE-URI available"))
  (unless link (error "pmwiki-create-page-uri: No LINK given!"))
  (unless (string-match pmwiki-HTTP-pattern base-uri)
    (error "pmwiki-create-page-uri: BASE-URI doesn't start with 'http:'"))
  (if (string-match "\\([^?]*\\)\\(\\?.*\\|/?\\)$"
		    base-uri) ; Remove last '/'
;;      (concat (match-string 1 base-uri) "/" (car link) "/" (cdr link))
      (concat (match-string 1 base-uri) "?pagename=" (car link) "." (cdr link))
    (error "Unable to remove last '/' from BASE-URI")))


(defun pmwiki-next ()
  "Goto the next WikiWord or {{free link}}."
  (interactive)
  (let ((case-fold-search nil))
    (re-search-forward
     (concat "\\(" pmwiki-link-pattern "\\|" pmwiki-free-link-pattern "\\)")
     nil t)))

(defun pmwiki-prev ()
  "Goto the previous WikiWord or {{free link}}."
  (interactive)
  (let ((case-fold-search nil))
    (re-search-backward
     (concat "\\(" pmwiki-link-pattern "\\|" pmwiki-free-link-pattern "\\)")
     nil t)))


(defun pmwiki-follow ()
  "Goto (open) the page pointed to by the PmWiki-link at point."
  (interactive)
  (pmwiki-edit (pmwiki-link-at-point)))


(defun pmwiki-page-name-at-point ()
  "If point is on a page link, return page name. Otherwise, return nil."
  (if (thing-at-point-looking-at
       (concat "\\<\\(" pmwiki-wikiword-pattern "\\)"))
      (match-string 1)
    nil))


(defun pmwiki-page-link-at-point ()
  "If point is on a page link, return link with default group and page name.
Otherwise, return nil."
  (let ((page (pmwiki-page-name-at-point)))
    (if page
	(if pmwiki-search-mode-status 
	    (cons (pmwiki-search-mode-group-at-point) page)
	  (cons (pmwiki-loc 'group) page))
      nil)))

(defun pmwiki-link-at-point ()
  "Return PmWiki-link in `cons' (\"<group>\", \"<page>\") at point 
of the current buffer. If no link is found at the current point, nil is
returned."
  (let ((case-fold-search nil)
	(found-full-link (thing-at-point-looking-at
			  "\\<[A-Za-z0-9]+[./][A-Za-z0-9]+\\>")))
    (if found-full-link
	(let ((maybe-link (match-string 0)))
	  (message (concat "maybe-link: " maybe-link))
	  (if (string-match (concat "\\<" "\\(" pmwiki-groupname-pattern "\\)"
				    "[./]\\(" pmwiki-wikiword-pattern "\\)")
			    maybe-link)
	      (cons (match-string 1 maybe-link) (match-string 2 maybe-link))
	    (pmwiki-page-link-at-point)))
      (pmwiki-page-link-at-point))))


(defun pmwiki-name-to-link (name)
  "Convert a string with a name, e.g. \"Group.Name\" to a link, i.e.
	( \"Group\" . \"Name\" )
The conversion allows names in these formats:
	<Group>.<Name>	-> ( <Group> <Name> )
	<Group>/<Name>	-> ( <Group> <Name> )
	<Name>		-> ( <current group> <Name> )
where <current group> is the group that the current buffer belongs to."
  (when (string-match
	 (concat "^\\(?:\\(" pmwiki-groupname-pattern "\\)[./]\\)?"
		 "\\(" pmwiki-pagetitle-pattern "\\)$")
	 name)
    (let ((group (match-string 1 name))
	  (page (match-string 2 name)))
      (if group (cons group page)
	(cons (pmwiki-loc 'group) page)))))
		

(defun pmwiki-link-to-name (&optional link)
  "Convert a LINK to a string with its name. If LINK is not given, the
data is taken from `pmwiki-page-uri'."
  (unless link (set 'link (pmwiki-loc 'link)))
  (concat (car link) "." (cdr link)))


(defun pmwiki-show-link (link)
  "Show a PmWiki LINK, i.e. a pair with a group and page name.
This function is mainly used for testing."
  (message (if link (pmwiki-link-to-name link) "Not a link!")))


(defun pmwiki-buffer-link (&optional buf)
  "Return link for buffer BUF. If BUF is not given, the current buffer
is assumed"
  (unless buf (set 'buf (buffer-name)))
  (save-excursion
    (set-buffer buf)
    (pmwiki-loc 'link)))


(defun pmwiki-buffer-uri (&optional buf)
  "Return URI of page in buffer BUF. If BUF is not given, the 
current buffer is assumed."
  (save-excursion
    (if buf (set-buffer buf))
    (pmwiki-create-page-uri (pmwiki-buffer-link buf))))


(defun pmwiki-browse (&optional buf)
  "Open the buffer BUF in a browser. If BUF is not given,
the current buffer is used."
  (interactive)
  (browse-url (pmwiki-buffer-uri buf)))


(defun pmwiki-browse-link-manual (link &optional base-uri)
  "Open page in browser URI specified by LINK, with WikiEngine at BASE-URI.
The parameter link LINK is (cons <group-name> <page-name>).
The parameter BASE-URI is an optional URI to the WikiEngine and
is taken from `pmwiki-page-uri' if BASE-URI is not given."
  (browse-url (pmwiki-create-page-uri link base-uri)))


(defun pmwiki-open (name)
  "Open a page specified by NAME from the current wiki engine, or
open page specified by a URI."
  (interactive "sName or URI (ex: Main.HomePage): ")
  (if (pmwiki-URIp name)
      (pmwiki-edit (pmwiki-loc 'link name) (pmwiki-loc 'base name))
    (when (string-equal name "") (set 'name "Main/HomePage"))
    (unless pmwiki-page-uri
      (error "pmwiki-open: `pmwiki-page-uri' is empty!"))
    (pmwiki-edit (pmwiki-name-to-link name))))


(defun pmwiki-reload ()
  "Reload the current page from the wiki engine."
  (interactive)
  (unless pmwiki-page-uri (error "pmwiki-reload: `pmwiki-page-uri' is empty!"))
  (let ((old-point (point))
	(proc nil))
    (if pmwiki-search-mode-status
	(pmwiki-search pmwiki-search-text)
      (set 'proc (pmwiki-open (pmwiki-loc 'name))))
    (while (string-equal (process-status proc) "open") 
      (sleep-for 0 50))
    (goto-char old-point)))


(defun pmwiki-save ()
  "Save the current buffer to the wiki."
  (interactive)
  (unless pmwiki-save-function (error "No save function specified"))
  (when pmwiki-search-mode-status (error "Not available for search buffers"))
  (run-hooks 'pmwiki-save-before-hooks)
  (save-excursion (funcall pmwiki-save-function))
  (not-modified))


(defun pmwiki-save-to-uri (page-uri)
  "Save current buffer to a PAGE-URI."
  (set 'pmwiki-page-uri page-uri)
  (rename-buffer (pmwiki-loc 'name))
  (pmwiki-save))


(defun pmwiki-save-as (name)
  "Save current buffer to a specific page on a wiki. This also
changes `pmwiki-page-uri', so future saves also goes to this page.
The argument NAME can be either a name (= <group>.<page>) or an URI.
An URI must begin with 'http'."
  (interactive "sSave page as NAME or URI (ex: Main.WikiSandbox): ")
  (if (string-match "^https?:" name)
      (pmwiki-save-to-uri name)
    (pmwiki-save-to-uri (pmwiki-create-page-uri (pmwiki-name-to-link name)))))
      

(defun lyx-save-as (name)
  "Save buffer to a certain <group>.<page> (=NAME) at the LyX wiki"
  (interactive "sName (ex: Main.WikiSandbox): ")
  (let ((page-uri (pmwiki-create-page-uri (pmwiki-name-to-link name)
					  lyx-wiki-base-uri)))
    (message (concat "Saving to " page-uri))
    (pmwiki-save-to-uri page-uri)))
	

(defun pmwiki-delete (name)
  "Delete page from the current wiki engine, specified by NAME which
can be empty (current buffer), a page name or a URI."
  (interactive "sDelete page (or empty for current page): ")
  (unless (string-equal name "") (pmwiki-open name))
  (erase-buffer)
  (insert "delete")
  (pmwiki-save)
  (kill-buffer nil)
  )

(defun pmwiki-rename (new-name)
  "Save current buffer under a new name and delete the original page."
  (interactive "sNew page name: ")
  (let ((old-page (pmwiki-loc 'name)))
    (pmwiki-save-as new-name)
    (pmwiki-delete old-page)))

;;
;; Functions useful for configuration.
;;
(defun pmwiki-delayed-hook-longlines-mode-on ()
  "Hook that activates `longlines-mode' when Emacs has been idle for 1 s."
  (require 'longlines)
  (run-with-idle-timer
   1 nil
   (lambda (buf)
     (save-excursion
       (set-buffer buf)
       (message (concat "Enabling longlines-mode for " (buffer-name)))
       (longlines-mode-on)))
   (current-buffer)))

(defun pmwiki-delayed-hook-longlines-mode-showing-hard-newlines ()
  "Hook that activates `longlines-mode' when Emacs has been idle for 1 s,
and in addition runs it `longlines-show-hard-newlines'."
  (require 'longlines)
  (run-with-idle-timer
   1 nil
   (lambda (buf)
     (save-excursion
       (set-buffer buf)
       (message (concat "Enabling longlines-mode for " (buffer-name)))
       (longlines-mode-on)
       (longlines-show-hard-newlines)))
   (current-buffer)))


;;; ************************************************************************
;;;
;;; Based on simple-emacswiki.el
;;;

(defun pmwiki-default-save-function ()
  "Save the current page to a PmWiki wiki."
  (let ((cur-buffer (buffer-name))
	(page-uri pmwiki-page-uri)
	(in-minor-mode pmwiki-minor-mode)
	(diffclass (if pmwiki-minor-mode "minor" ""))
	(pagename (pmwiki-loc 'name))
	(password pmwiki-encoded-password)
	(headers nil))
    (if password
	(setq headers
	      (list (list "Authorization" (concat "Basic " password)))))
    (message (concat "Any output is in "
		   (buffer-name 
		    (process-buffer
		     (http-post
		      (pmwiki-loc 'base) ;; Get destination
		      (list (cons "pagename" pagename)
			    (cons "action" "post")
			    (cons "diffclass" diffclass)
			    (cons "author" pmwiki-author)
			    (cons "text" (buffer-string)))
		      'iso-8859-1
		      headers)))))
    (switch-to-buffer cur-buffer)
    (pmwiki-mode)
    (set (make-local-variable 'pmwiki-page-uri) page-uri)
    (set (make-local-variable 'pmwiki-encoded-password) password)
    (when in-minor-mode (pmwiki-minor-mode))))


(defun pmichaud-open (name)
  "Browse the PmWiki wiki. Ex: 'Main.WikiSandbox' will open a page from
	http://www.pmichaud.com/wiki/Main/WikiSandbox"
  (interactive "sName (default Main.WikiSandbox): ")
  (if (pmwiki-URIp name)
      (pmwiki-edit (pmwiki-loc 'link name) (pmwiki-loc 'base name))
    (pmwiki-edit (pmwiki-name-to-link
		  (pmwiki-default-string name "Main.WikiSandbox"))
		 (pmwiki-loc 'base pmichaud-sandbox-uri))))


(defun lyx-open (name)
  "Open page NAME from the LyX wiki (or a page specified by an URI).
Ex: 'Main.WikiSandbox' will open the page
   http://wiki.lyx.org/pmwiki.php/Main/WikiSandbox
Note when NAME is an URI, the page doesn't have to belong to the LyX wiki."
  (interactive "sName (default Main.HomePage): ")
  (if (pmwiki-URIp name)
      (pmwiki-edit (pmwiki-loc 'link name) (pmwiki-loc 'base name))
    (pmwiki-edit (pmwiki-name-to-link
		  (pmwiki-default-string name "Main.HomePage"))
		 (pmwiki-loc 'base lyx-homepage-uri))))


(define-minor-mode pmwiki-minor-mode
  "Toggle `pmwiki' minor changes mode.
With no argument, this command toggles the mode. 
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.
The default is for this mode to be activated!

When this minor mode is active, saved changes will be tagged with a
`pmwiki-diffclass` of \"minor\"."
  t " minor")


(define-derived-mode pmwiki-search-mode pmwiki-mode "PmWiki-Search"
  "Mode for the search result. Uses a different font-lock scheme (colors) and
the buffer cannot be saved.
The variable `pmwiki-search-mode-status' is set to 't in this mode.
\\{pmwiki-search-mode-map}"
  (set 'pmwiki-search-mode-status t)
  (when pmwiki-running-emacs
    (font-lock-add-keywords
     nil
     (list (cons
	    (concat "^	\\(" (symbol-value 'pmwiki-pagetitle-pattern) "\\)")
	    'font-lock-keyword-face)
	   (cons
	    (concat "^\\(" (symbol-value 'pmwiki-groupname-pattern) "\\)")
	    'font-lock-constant-face))
     'set))
  )

(defun pmwiki-map-site ()
  "Generate \"site-map\" with all groups and pages.
The function uses `pmwiki-search'."
  (interactive)
  (pmwiki-search "")
)
  

(defun pmwiki-search (text &optional link base-uri goto-end http-ver content-type)
  "Search the wiki site of the current buffer for TEXT.

Optionally, LINK specifies the 'current' page and BASE-URI the site
address. See `http-get' for more information on the other arguments."

  (interactive "sSearch for: ")
  (let ((page-uri
	 (pmwiki-create-page-uri (or link (pmwiki-loc 'link)) base-uri))
	(content-type (or content-type 'iso-8859-1))
	(proc nil))
    (message (if (string-equal text "") "Retrieving site map"
	       (format "Searching site for '%s'" text)))
    (set 'proc
	 (http-get (concat page-uri "&action=search&text=" text)
		   '(("Connection" "close"))		; headers
		   nil					; ?
		   http-ver				; nil -> 1.0
		   "search-result (PmWiki)"		; output buffer
		   content-type))			; typically iso-8859-1
    (while (string-equal (process-status proc) "open") (sleep-for 0.050))
    (message "Parsing HTML result of search...")
    (pmwiki-search-parse-buffer)
    (pmwiki-search-mode)
    (set-fill-column pmwiki-fill-column)
    (set (make-local-variable 'pmwiki-page-uri) page-uri)
    (set (make-local-variable 'pmwiki-search-text) text)
    (unless goto-end
      (while (string-equal (process-status proc) "open") (sleep-for 0.050))
      (goto-char 1))
    proc))
  

(defun pmwiki-search-parse-buffer ()
  "Parse the resulting html of search to prodcue pmwiki markup."
  (goto-char 0)
  (delete-non-matching-lines "<dl>.+</dl>")
  (goto-char 0)
  (while (re-search-forward "\\(<a href=\"[^\"]+\">\\)\\|\\(</a>\\)" nil t)
    (replace-match "" nil t))
  (goto-char 0)
  (while (re-search-forward "\\( *<p /> *\\)\\|\\(^ +$\\)" nil t)
    (replace-match "" nil t))
  (goto-char 0)
  (while (re-search-forward "\\(<dl>\\)\\|\\(</dl>\\)" nil t)
    (replace-match "" nil t))
  (goto-char 0)
  (while (re-search-forward "\\(</dt>\\)\\|\\(</dd>\\)" nil t)
    (replace-match "" nil t))
  (goto-char 0)
  (while (search-forward "<dt>" nil t)
    (replace-match "\n\n" nil t))
  (goto-char 0)
  (while (search-forward "<dd>" nil t)
    (replace-match "\n	" nil t))
)

(defun pmwiki-search-mode-group-at-point()
  "Used to find the name of the group corresponding to the current point in
a buffer with search results. It searches backwards for

	GroupName /

and returns 'GroupName' (or an error).
"
  (let ((old-point (point-marker))
	(found (re-search-backward
		(concat "^\\(" pmwiki-groupname-pattern "\\) /")  nil t)))
    (goto-char old-point)
    (unless found (error "No group name found!"))
    (match-string 1)))

(provide 'pmwiki-mode)
;;; pmwiki-mode.el ends here
