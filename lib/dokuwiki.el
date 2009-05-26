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

(require 'eieio)
(require 'xml-rpc)
;; (xml-rpc-method-call-async callbackb url 'wiki.getAllPages)
;; (xml-rpc-method-call url 'wiki.getAllPages)

(defcustom dokuwiki-wikis
  '(("SigmaWiki" "http://www.hodique.info/lib/exe/xmlrpc.php" utf-8)
    ("TestWiki" "http://localhost/~yann/wiki/lib/exe/xmlrpc.php" utf-8))
  "Alist mapping wiki names to URLs."
  :type '(repeat (list (string :tag "Wiki")
                       (string :tag "URL")
                       (symbol :tag "Coding System")))
  :group 'dokuwiki)

(defvar dokuwiki-current-wiki nil
  "The current wiki. Must match a key from `dokuwiki-wikis'.")
(make-variable-buffer-local 'dokuwiki-current-wiki)

(defclass dokuwiki-wiki ()
  ((name :initarg :name
         :type string
         :documentation "Short name for this wiki")
   (url :initarg :url
        :type string
        :documentation "Base url")
   (coding-system :initarg :coding-system
                  :type symbol
                  :documentation "Coding system")
   (pages :initarg :pages
          :type dokuwiki-page-store
          :documentation "List of pages")))

(defmethod dokuwiki-refresh-pages ((wiki dokuwiki-wiki))
  (let ((store (oref wiki :pages)))
    (dokuwiki-clear-pages store)
    (dokuwiki-populate-pages store 
                             (xml-rpc-method-call (oref wiki :url) 'wiki.getAllPages))))

(defmethod dokuwiki-org-index ((wiki dokuwiki-wiki))
  (let ((buffer
         (get-buffer-create (format "*%s Index*" (oref wiki :name)))))
    (with-current-buffer buffer
      (dokuwiki-children-as-org (oref (oref wiki :pages) :fake-root))
      (org-mode)
      (toggle-read-only 1))
    (pop-to-buffer buffer)))

(defclass dokuwiki-wiki-provider (eieio-singleton)
  ((wikis :initform nil
          :type list
          :documentation "Defined wikis"
          :protection :private)))

(defmethod dokuwiki-get-wiki ((provider dokuwiki-wiki-provider) name)
  (let ((known (assoc name (oref provider wikis)))
        (new (assoc name dokuwiki-wikis)))
    (cond (known
           (cadr known))
          (new
           (let* ((wiki (make-instance 'dokuwiki-wiki 
                                       :name (nth 0 new)
                                       :url (nth 1 new)
                                       :coding-system (nth 2 new)
                                       :pages (make-instance 'dokuwiki-page-store))))
             (oset provider wikis (cons (list name wiki) (oref provider wikis)))
             wiki))
          (t
           nil))))

(defclass dokuwiki-page ()
  ((id :initarg :id
       :initform ""
       :type string
       :documentation "Page Id")
   (perms :initarg :perms
          :type string
          :documentation "Page permissions")
   (size :initarg :size
         :type number
         :documentation "Page size")
   (modified :initarg :modified
             :type string
             :documentation "Page last modification date")
   (children :initarg :children
             :initform nil
             :type list
             :documentation "Page children in wiki hierarchy")))

(defmethod dokuwiki-mappage ((page dokuwiki-page) func)
  (mapcar (lambda (p)
            (funcall func (car p) (cdr p)))
          (oref page :children)))

(defmethod dokuwiki-children-index ((page dokuwiki-page))
  (dokuwiki-mappage page
                    (lambda (n p)
                      (list n (oref p :id) (dokuwiki-children-index p)))))

(defmethod dokuwiki-children-as-org ((page dokuwiki-page) &optional level)
  (unless level
    (setq level 1))
  (let ((prefix (make-string level ?*)))
    (dokuwiki-mappage page
                      (lambda (n p)
                        (insert prefix " " n "\n")
                        (dokuwiki-children-as-org p (1+ level))))))

(defclass dokuwiki-page-store ()
  ((fake-root :initarg :fake-root
              :initform (lambda () (make-instance 'dokuwiki-page))
              :type dokuwiki-page
              :documentation "Fake root for page hierarchy to make it a tree")
   (date :initarg :date
         :type string
         :documentation "Date of latest update"
         :protection :private)))

(defmethod dokuwiki-pages-index ((store dokuwiki-page-store))
  (dokuwiki-children-index (oref store :fake-root)))


(defmethod dokuwiki-record-page ((store dokuwiki-page-store) page)
  (let* ((id (cdr (assoc "id" page)))
         (perms (cdr (assoc "perms" page)))
         (size (cdr (assoc "size" page)))
         (modified (cdr (assoc "lastModified" page)))
         (cpts (nreverse (split-string id ":")))
         (file (car cpts))
         (dirs (nreverse (cdr cpts)))
         (p (make-instance 'dokuwiki-page
                           :id id :perms perms
                           :size size :modified modified))
         (node (oref store :fake-root)))
    (dolist (d dirs)
      (let ((loc (assoc d (oref node :children))))
        (if loc ;;node already exist
            (setq node (cdr loc))
          (let ((dir (make-instance 'dokuwiki-page)))
            (oset node :children 
                  (push (cons d dir) (oref node :children)))
            (setq node dir)))))
    (oset node :children (push (cons file p) (oref node :children)))))

(defmethod dokuwiki-clear-pages ((store dokuwiki-page-store))
  (oset (oref store :fake-root) :children nil))

(defmethod dokuwiki-populate-pages ((store dokuwiki-page-store) pages)
  (mapc (lambda (page) 
          (dokuwiki-record-page store page)) 
        pages))

(defun dokuwiki-read-wiki ()
  "Read a wikiname of `dokuwiki-wikis' with completion. Returns a wiki object"
  (let* ((name (completing-read "Wiki: " dokuwiki-wikis nil t 
                                (and dokuwiki-current-wiki 
                                     (oref dokuwiki-current-wiki :name)))))
    (dokuwiki-get-wiki (dokuwiki-wiki-provider nil) name)))

;; (defmethod dokuwiki-index-pages ((wiki dokuwiki-wiki))
;;   (interactive (list (dokuwiki-read-wiki)))
;;   (dokuwiki-refresh-pages wiki)
;;   (let ((titles
;;          (mapcar (lambda (page) (cdr (assoc "id" page))) 
;;                   (dokuwiki-get-pages wiki)))
;;         (buffer
;;          (get-buffer-create (format "*%s Index" (oref wiki name)))))
;;     (with-current-buffer buffer
;;       (dokuwiki-render-titles-as-org titles))
;;     (pop-to-buffer buffer)))

;; (defun dokuwiki-render-titles-as-org (titles)
;;   (let ((tree nil))
;;     (mapc (lambda (title)
;;             (let ((cpts (split-string title ":")))
;;               ))
;;           titles)))
(provide 'dokuwiki)
;;; dokuwiki.el ends here
