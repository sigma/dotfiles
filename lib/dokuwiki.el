;;; dokuwiki.el --- Edit dokuwiki pages through XML-RPC

;; Copyright (C) 2008  Free Software Foundation, Inc.

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords:

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(require 'xml-rpc)

(defcustom dokuwiki-wikis
  '(("SigmaWiki" "http://www.hodique.info/lib/exe/xmlrpc.php" utf-8))
  "Alist mapping wiki names to URLs."
  :type '(repeat (list (string :tag "Wiki")
                       (string :tag "URL")
                       (symbol :tag "Coding System")))
  :group 'dokuwiki)

(defvar dokuwiki-wiki nil
  "The current wiki. Must match a key from `dokuwiki-wikis'.")

(defvar dokuwiki-pages-hash (make-hash-table :test 'equal)
  "The wiki-name / pages pairs.")

(defun dokuwiki-read-wiki ()
  "Read an wikiname of `dokuwiki-wikis' with completion."
  (completing-read "Wiki: " dokuwiki-wikis nil t dokuwiki-wiki))

(defun dokuwiki-read-pagename (wiki &optional reload)
  "Read a pagename of WIKI with completion."
  (completing-read "Pagename: " (dokuwiki-make-completion-table wiki reload)))

(defun dokuwiki-read-wiki-and-pagename (&optional reload)
  "Read an wikiname and a pagename of `dokuwiki-wikis' with completion."
  (interactive "P")
  (let ((wiki (dokuwiki-read-wiki)))
    (list wiki (dokuwiki-read-pagename wiki reload))))

(defun dokuwiki-make-completion-table (wiki &optional reload)
  "Create pagename completion table for WIKI. if available, return precomputed one."
  (or (and (not reload) (gethash wiki dokuwiki-pages-hash))
      (dokuwiki-compute-pagename-completion-table wiki reload)))

(defun dokuwiki-get-wiki (name)
  (cadr (assoc wiki dokuwiki-wikis)))

(defun dokuwiki-compute-pagename-completion-table (&optional wiki-arg force)
  "Really fetch the list of pagenames from WIKI.
This command is used to reflect new pages to
`dokuwiki-pages-hash'."
  (let* ((wiki (or wiki-arg (dokuwiki-read-wiki)))
         (url (dokuwiki-get-wiki wiki))
         (coding (caddr (assoc wiki dokuwiki-wikis)))
         table)
    (dokuwiki-populate-pages-hash wiki force)))

(defun dokuwiki-puthash (wiki pages hash)
  (puthash wiki pages hash))

(defun dokuwiki-gethash (wiki hash)
  (gethash wiki hash))

(defun dokuwiki-ignore (&rest args)
  (print (format "%s" args)))

(defun dokuwiki-populate-pages-hash (wiki force &optional callback)
  (or (and (not force)
           (dokuwiki-gethash wiki dokuwiki-pages-hash))
      (let ((url (dokuwiki-get-wiki wiki)))
        (if (not callback)
            (prog1
                (setq table (xml-rpc-method-call url 'wiki.getAllPages))
              (dokuwiki-puthash wiki table dokuwiki-pages-hash))
          (xml-rpc-method-call-async ;; `(lambda (pages)
           ;;    (dokuwiki-puthash ,wiki pages dokuwiki-pages-hash)
           ;;    (funcall ,callback))
           #'dokuwiki-ignore
           url
           'wiki.getAllPages)))))

(defun dokuwiki-insert-page (page)
  (insert page)
  (newline))

(defun dokuwiki-index-pages (wiki)
  (interactive (list (dokuwiki-read-wiki)))
  ;; (dokuwiki-populate-pages-hash wiki t
  ;;                               `(lambda ()
  ;;                                  (let ((buf (get-buffer-create " *Dokuwiki Index*")))
  ;;                                    (with-current-buffer buf
  ;;                                      (mapcar 'dokuwiki-insert-page (dokuwiki-gethash ,wiki dokuwiki-pages-hash))
  ;;                                      (pop-to-buffer buf)))))

  )

(provide 'dokuwiki)
;;; dokuwiki.el ends here
