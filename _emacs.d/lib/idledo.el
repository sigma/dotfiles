;; -*- auto-recompile: t -*-
;;; idledo.el --- do stuff when emacs is idle..
;; Time-stamp: <2001-08-09 15:11:50 deego>
;; Copyright (C) Deepak Goel 2001
;; Emacs Lisp Archive entry
;; Filename: idledo.el
;; Package: idledo
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 0.1.2
;; Author's homepage: http://www.glue.umd.edu/~deego
;; For latest version:

(defvar idledo-home-page  "http://www.glue.umd.edu/~deego")

;; Requires: timerfunctions.el
;; See also: Jari's tinyload.el (implements an idle-loading of files..)

 
;; This file is NOT (yet) part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 

;; See also:


;; Quick start:
(defvar idledo-quick-start
  "Drop idledo.el and timerfunctions.el somewhere in your load-path. In your
.emacs, type \(require 'idledo\) and \(require 'timerfunctions\) Create
idledo-list, either by hand, or by using one of the many functions
provided.  Then, write \(idledo-start\), and idledo will do the tasks
mentioned in the idledo-list whenever emacs is idle."
)

(defun idledo-quick-start ()
  "Provides electric help for function `idledo-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert idledo-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar idledo-introduction
  "idledo not only makes emacs say 'i do' to you, it also
does stuff for you when emacs is idle. idledo==>idle-do.

The actions can be simple one-time actions or repetitive.  You can
include as many actions as you want.  Thus, if you leave emacs running
for sometime, take a trip and come back, you can now start my gnus or
eshell or w3 instantly.. When you are using gnus, you can check mail
periodically..  

Even though it seems to do all i can fancy needing, idledo will
nonetheless someday be interfaced with a prioritizer, which will include
all sorts of enhanced capabilites, like enhanced repetitive actions, weighting
of actions etc.

See also M-x idledo-commentary
"
)

(defun idledo-introduction ()
  "Provides electric help for function `idledo-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert idledo-introduction) nil) "*doc*"))

;;; Commentary:
(defvar idledo-commentary
  "First type M-x idledo-introduction.  
Also see M-x idledo-quick-start

You give idledo a list of commands, and it will eval them when emacs is
idleand you are away..  Thus, if you take a hike and come back, your
w3, gnus, eshell should all start instantly..  Your gnus-news should
be checked periodically for you.. and *Group* buffer updated..

If emacs is idle *not* because you are away, but because you are
deeply absorbed using info, you probably don't want idledo springing into
action and loading eshell for you.. So, idledo tries to alert you before
loading anything, and gives you enough time to cancel any action
before it is taken..

As an example, here is my idledo-specs from my .emacs:
\(add-hook 'gnus-started-hook 'gf-periodically-check-mail-my\)
\(require 'idledo\)
\(idledo-require-now 'mailabbrev\)
\(idledo-require 'esh-mode\)
\(idledo-require 'em-alias'em-banner 'em-basic 'em-dis
 'em-glob 'em-hist\)
\(idledo-require 'em-hist\)
\(idledo-require 'em-ls\)
\(idledo-require 'em-prompt\)
\(idledo-require 'em-script\)
\(idledo-require 'em-term\)
\(idledo-require 'em-xtra\)
\(idledo-require 'em-unix\)
\(idledo-require 'etags\)
\(idledo-require 'ange-ftp\)
\(idledo-require 'pcmpl-auto\)
\(idledo-require 'pcomplete\)
\(idledo-require 'eshell-auto\)
\(idledo-require 'gnus\)
\(idledo-require 'gnus-msg\)
\(idledo-require 'faith\)
\(idledo-add-action '\(message \"IDLEDO sample...\"\)\)


\(idledo-start\)

(defun gf-periodically-check-mail-my ()
  ;; this let so that no copies made when new mail checked..
  (let ((ebackup-max-copies 0))
    (require 'idledo)
    (idledo-add-periodic-action-to-beginning-crude
     ;; the function below is defined to essentially call
     ;; gnus-group-get-new-news
     '(gf-get-new-news-once-my))))


"
)

(defun idledo-commentary ()
  "Provides electric help for function `idledo-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert idledo-commentary) nil) "*doc*"))

;;; History:

;;; New features:
(defvar idledo-new-features
  "0.1 new features:
* Now called idledo, to avoid a name-conflict with another package.
* Macros like ido-add-require now called idledo-require.
"
)

(defun idledo-new-features ()
  "Provides electric help for function `idledo-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert idledo-new-features) nil) "*doc*"))

(defvar idledo-version "0.1.2")

(defvar idle-todo 
"TODO: 
* Ideally, one should be able to cancel the timer if idledo-list
   becomes nil.

* Write a prioritizer, and interface the same with idledo. The priotizer
  should. among other things like weights and \(arbitrarily specified\)
  repetitivity, try to support different idle times for different
  tasks..
" )


;;==========================================
;;; Code:



(defvar idledo-list nil
  "A list of actions to perform..")

(defvar idledo-active-p nil
  "If t, no more idledo's can be initiated.. 
The aim is to only have one idledo active at a time.

Why?  I don't know.  You can easily setq this to nil, and start yet
another idledo-start if you want.
")

(defvar idledo-interval 60
  "The interval between several tasks.. 
An additional small interval will be allowed to enable the user to
cancel the action. " )

(defvar idledo-small-interval  6
  "Before beginning any action, idledo will flash a warning, and will
wait for these many seconds.. if you do something in this time, the
action will be cancelled. ")


(defun idledo-start ()
  "This starts idledo. See also idledo-active-p."
  (interactive)
  (if (not idledo-active-p)
      (progn
	(setq idledo-active-p t)
	(tf-run-with-idle-timer idledo-interval t idledo-interval t nil 
				'idledo-one-action))
    (error "Idledo is already active")))
    
(defvar idledo-done-interval 1
  "Idledo will wait for this much time before flashing a 'done-action'
message "
)


(defvar idledo-action-imminent-string 
  "IDLEDO IMMINENT UNLESS KEYPRESS!! ---> ")

(defun idledo-one-action ()
  "Internal.
Does one instance of processing of action. "
  (when (not (null idledo-list))
    (message 
     (concat idledo-action-imminent-string
	     (idledo-shorten (format "%S" (car idledo-list)))))
    (if (sit-for 2)
	(progn
	  (message 
	   (concat "IDLEDO doing action.."
		   (idledo-shorten (format "%S" (car idledo-list)))))
	  (let ((carval (car idledo-list)))
	    (setq idledo-list (cdr idledo-list))
	    (idledo-ignore-errors (eval  carval)))
	  (sit-for idledo-done-interval)
	  (message "%S more idledo(s) remainig.. "
		   (length idledo-list)))
      (message 
       (concat "IDLEDO's action canceled.."
	       (idledo-shorten (format "%S" (car idledo-list)))))
      )))
  
  
(defun idledo-add-periodic-action-crude (action)
  "Is a crude mechanism for adding action to the idledo-list and make it
repetitive.  ACTION is a list which will be evaled to perform an
eval. 
Note that the ACTION this way is added to the END of idledo-list.
And ACTION is added to list no matter what (even if there is a similar
action already waiting in the list).
"
  (setq 
   idledo-list
   (append 
    idledo-list
    (list
     `(progn
	,action
	(idledo-add-periodic-action-crude
	 ,action))))))

(defun idledo-add-periodic-action-to-beginning-crude (action)
  "Is a crude mechanism for adding action to the idledo-list and make it
periodic.  ACTION is a list which will be evaled to perform an
eval. 
Note that the ACTION this way is added to the BEGINNING and subsequent
calls are also added to the beginning of the list.
And ACTION is added to list no matter what (even if there is a similar
action already waiting in the list).
"
  (idledo-add-action-forced
   `(progn
      (quote ,action)
      (idledo-add-periodic-action-to-beginning-crude
       (quote ,action)))))


   

;;;###autoload
(defun idledo-add-to-end-of-list (list-var element)
  "Like add-to-list, but adds at the end, if added at all."
  (if (member element (symbol-value list-var))
      (symbol-value list-var)
    (set list-var (append  (symbol-value list-var) (list element)))))

(defun idledo-add-action (action)
  "Action is a symbol."
  (add-to-list 'idledo-list action))

(defun idledo-add-action-forced (action)
  (setq idledo-list (cons action idledo-list)))

(defun idledo-add-action-at-end (&rest actions)
  (mapcar 
   (lambda (action)
     (idledo-add-to-end-of-list 'idledo-list action))
   actions))

(defmacro idledo-require (&rest features)
  (cons 'progn
	(mapcar 
	 (lambda (arg)
	   `(idledo-add-action-at-end '(require ,arg)))
	 features)))

(defmacro idledo-require-now (feature)
  `(idledo-add-action '(require ,feature)))

(defun idledo-add-action-at-end-forced (action)
  (setq idledo-list (append idledo-list (list action))))

(defun idledo-initialize (initial-list)
  (setq idledo-list initial-list))

(defun idledo-remove-action (action)
  (idledo-remove-from-list 'idledo-list action))

(defun idledo-remove-from-list (listname elt)
  "INTERNAL"
  (set listname (idledo-list-without-element 
		 (eval listname)
		 elt)))

(defun idledo-list-without-element (list elt)
  "INTERNAL"
  (if (null list)
      list
    (if (equal (car list) elt)
	(idledo-list-without-element (cdr list) elt)
      (cons
       (car list)
       (idledo-list-without-element
	(cdr list) elt)))))



(defun idledo-shorten (string)
  "Internal, return a shortened version with no newlines.
Internal, returns a shortened version of STRING with no newlines."
  (let
      ((string-no-enter
	(with-temp-buffer
	  (insert string)
	  (goto-char (point-min))
	  (while (search-forward "\n" nil t)
	    (replace-match " " nil t))
	  (buffer-substring (point-min) (point-max)))))
    (if (> (length string-no-enter) 55)
	(substring string-no-enter 0 55)
		   string-no-enter)))


(defmacro idledo-ignore-errors (&rest body)
  "Like ignore-errors, but tells the error..
Improved for me by Kalle on 7/3/01:
 * used backquote: something i should have done long ago.
 * removed the progn: condition-case automatically has one..
 * made sure that the return is nil.. just as it is in ignore-errors. "
  (let ((err (gensym)))
    `(condition-case ,err (progn ,@body)
       (error
	(message "IGNORED ERROR: %s" (error-message-string ,err))
	(sit-for 1)
	nil))))



(provide 'idledo)

;;; idledo.el ends here
