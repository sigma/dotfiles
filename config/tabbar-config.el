;;; tabbar-config.el --- Configuration for tabbar

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Yann Hodique <Yann.Hodique@lifl.fr>
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

;;; Tabbar

(when (request 'tabbar)
  (tabbar-mode)
  (global-set-key '[(shift right)] 'tabbar-forward-tab)
  (global-set-key '[(shift left)]  'tabbar-backward-tab)
  (global-set-key '[(shift up)]    'tabbar-forward-group)
  (global-set-key '[(shift down)]  'tabbar-backward-group))

(defun yh/tabbar-inhibit-function ()
  (or (window-dedicated-p (selected-window))
      (member (buffer-name)
              '("*Group*" "*Calendar*"))
      (string-match "\\*Summary" (buffer-name))))

(setq tabbar-inhibit-functions '(tabbar-default-inhibit-function yh/tabbar-inhibit-function))

(defun yh/tabbar-buffer-groups (buffer)
  "Return the list of group names BUFFER belongs to.
Return only one group for each buffer."
  (with-current-buffer (get-buffer buffer)
    (cond
     ((memq major-mode
            '(emacs-lisp-mode inferior-emacs-lisp-mode lisp-mode lisp-interaction-mode scheme-mode sawfish-mode))
      '("Lisp"))
     ((memq major-mode
            '(circe-server-mode circe-channel-mode circe-query-mode))
      '("IRC"))
     ((or (get-buffer-process (current-buffer))
          (memq major-mode
                '(comint-mode compilation-mode)))
      '("Process"))
     ((string-match "\\*\\(svn-\\|cvs\\|x?tla-\\|baz\\)" (buffer-name))
      '("VC"))
     ((eq major-mode 'fundamental-mode)
      '("Common"))
     ((eq major-mode 'dired-mode)
      '("Dired"))
     ((memq major-mode
            '(help-mode apropos-mode Info-mode Man-mode))
      '("Help"))
     ((memq major-mode
            '(rmail-mode
              rmail-edit-mode vm-summary-mode vm-mode mail-mode
              mh-letter-mode mh-show-mode mh-folder-mode
              gnus-summary-mode message-mode gnus-group-mode
              gnus-article-mode score-mode gnus-browse-killed-mode))
      '("Mail"))
     (t
      (list
       (if (and (stringp mode-name) (string-match "[^ ]" mode-name))
           mode-name
         (symbol-name major-mode))))
     )))

(setq tabbar-buffer-groups-function 'yh/tabbar-buffer-groups)

(defun yh/tabbar-buffer-tab-label (tab)
  "Return the label to display TAB.
Must be a valid `header-line-format' template element."
  (if tabbar-buffer-group-mode
      (format "[%s]" (tabbar-tab-tabset tab))
    (format " %s " (tabbar-tab-value tab))))

(setq tabbar-tab-label-function 'yh/tabbar-buffer-tab-label)

;; This  functions overloads the function defined in tabbar.el
(defun tabbar-line ()
  "Return the header line templates that represent the tab bar.
Call `tabbar-current-tabset-function' to obtain the current tab set to
display.  Then call `tabbar-line-element' on each tab in current tab
set's view to build a list of template elements for
`header-line-format'."
  (if (run-hook-with-args-until-success 'tabbar-inhibit-functions)
      (setq header-line-format nil)
    (let ((result
           (let ((tabset (tabbar-current-tabset t))
                 (padcolor (face-background 'tabbar-default-face)))
             (when tabset
               (list
                (format "[%s]" tabset)
                tabbar-separator-value
                (or
                 ;; If a cached template exists, use it.
                 (tabbar-template tabset)
                 ;; Otherwise use a refeshed value.
                 (tabbar-set-template tabset
                                      (mapcar 'tabbar-line-element
                                              (tabbar-view tabset))))
                (propertize "%-" 'face (list :background padcolor
                                             :foreground padcolor)))))))
      (yh/tabbar-ensure-visible)
      result)))

(defadvice tabbar-add-tab (after ad-tabbar-add-tab-after (tabset object &optional append) act)
  "Force the tabs to remain sorted"
  (set tabset (sort (symbol-value tabset) #'(lambda (a b) (string< (car a) (car b))))))

(defun yh/tabbar-scroll-left (arg)
  (interactive "p")
  (tabbar-scroll (tabbar-current-tabset) (- arg)))

(defun yh/tabbar-scroll-right (arg)
  (interactive "p")
  (tabbar-scroll (tabbar-current-tabset) arg))

(global-set-key '[(meta shift left)] 'yh/tabbar-scroll-right)
(global-set-key '[(meta shift right)]  'yh/tabbar-scroll-left)

(defun yh/tabbar-visible-p ()
  (let* ((tabs (tabbar-view (tabbar-current-tabset)))
         (pos (position-if (lambda (e) (equal (buffer-name (current-buffer)) (car e))) tabs)))
    (when pos
      (< (apply '+ (mapcar #'(lambda (e) (length (car e))) (subseq tabs 0 (- pos 1))))
         (window-width)))))

(defsubst yh/tabbar-current-index ()
  (position-if (lambda (e) (equal (buffer-name (current-buffer)) (car e)))
               (tabbar-view (tabbar-current-tabset))))

(defun yh/tabbar-ensure-visible ()
  (let ((width (window-width))
        (tabset (tabbar-current-tabset)))
    (while (not (yh/tabbar-current-index))
      (tabbar-scroll tabset -1))
    (while (>= (apply '+ (mapcar #'(lambda (e) (length (car e)))
                                 (subseq (tabbar-view tabset)
                                         0
                                         (yh/tabbar-current-index))))
               width)
      (tabbar-scroll tabset 1))))

(provide 'tabbar-config)
;;; tabbar-config.el ends here
