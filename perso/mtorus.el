;;; mtorus.el --- navigation with marks on a ring of rings (torus)
;; $Id: mtorus.el,v 1.1 2004/04/19 17:53:59 sigma Exp $
;; Copyright (C) 2003 by Stefan Kamphausen
;; Author: Stefan Kamphausen <mail@skamphausen.de>
;; Created: Winter 2002
;; Keywords: bookmarks, navigation, tools, extensions, user

;; This file is not part of XEmacs.

(defconst  mtorus-version "2.0"
  "Version number of MTorus.")

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.


;;; Commentary:
;; FIXMEs
;;  - generic interface for special lists, because currently the
;;    (only) special list (buffer list) behaviour is hard coded in a
;;    way that's probably not sensible for lists like recent-files or
;;    the like
;;  - handling of invalid markers
;;    -> (don't display, quietly discard?)
;;    -> the correct while-loop which deletes invalid in
;;    mtorus-jump-current-marker or mtorus-current-buffer
;;    -> what happens when a buffer is reverted (or killed and
;;    reopened)?
;; MTorus on the Web:
;; Main page:
;; http://www.skamphausen.de/software/skamacs/mtorus.html
;; German intro:
;; http://www.skamphausen.de/xemacs/lisp/mtorus.html
;;
;;; Usage:
;;  ======
;; MTorus lets you work with several groups of buffers, each group
;; being a separate ring.  This is all for easier navigation through
;; buffers.  I've been using some buffer cycling functions on (shift
;; left and shift right) for quite a long time now and I find myself
;; cycling through larger and larger lists every day.  Starting one
;; instance of (X)Emacs for each editing context isn't the way and I
;; found no way of doing what I want with frames.  An `editing context'
;; means a logical grouping of buffers.  This could be a group for
;; quick edit of the emacs configuration files while you're actually
;; working on some (Ruby/Perl/whatever-) program or it could be all
;; the headers of your C project while all the .c-files make up
;; another group.  Whatever you can think of.  You could even make
;; different parts of your buffers (point positions) show up in
;; different groups like when one set of functions spread throughout
;; one (or more) files is responsible for one specific task or like
;; working on a chapter in a LaTeX-document while defining some macros
;; at the top of the file.  There is always a default group which
;; contains all the open buffers and can not be altered.
;;
;; Like so:
;; +- Ring: C-code -------------------------------------------------+
;; |                                                                |
;; | +- marker ---+  +- marker ---+  +- marker ---+                 |
;; | | main.cc/224|  | main.cc/567|  | main.hh/312|                 |
;; | +------------+  +------------+  +------------+                 |
;; |                                                                |
;; | +- marker ---+  +- marker ----+                                |
;; | | *Occur*/84 |  | README/1388 |                                |
;; | +------------+  +-------------+                                |
;; |                                                                |
;; +----------------------------------------------------------------+
;;
;; +- Ring: quick --------------------------------------------------+
;; |                                                                |
;; |                                                                |
;; | +- marker ---+  +- marker -----+                               |
;; | | .emacs/224 |  | *scratch*/33 |                               |
;; | +------------+  +--------------+                               |
;; |                                                                |
;; +----------------------------------------------------------------+
;;
;; +- Ring: doc ----------------------------------------------------+
;; |                                                                |
;; |                                                                |
;; | +- marker ---+  +- marker ---+                                 |
;; | | *info*/999 |  | Man: ls/1  |                                 |
;; | +------------+  +------------+                                 |
;; |                                                                |
;; +----------------------------------------------------------------+
;;
;; Choosing another entry in such a group is done by actually cycling
;; through the group (like with e.g. the kill-ring) and since the
;; entries are made of a buffer together with a position (and that is a
;; marker) each of the groups is comparable to the mark-ring.  And
;; since we are cycling through a collection of those rings we are
;; using a Torus here.  Hence the name: "mtorus": mark-torus.
;; (A "group" will usually be referred to as a "ring" from now.)

;;; History
;; Just as many other people I have once (co-)written a buffer cycling
;; mechanism, that was later expanded to using a skipping
;; predicate.  At some point it wasn't enough anymore and I still had
;; the question nagging: what could I possible do with Shift Up and
;; Down? Then I discovered that I often used several buffers in one
;; context (and they were not neighbors on the buffer-list).  This made
;; me think of groups of buffers and led to a first implementation
;; under the name "session-stack.el".  Then it occurred to me that I
;; wanted to navigate different positions in the same buffer with it
;; and expanded the code.  Sometimes I wondered that those hotspots
;; seemed to move around until I read a chapter about markers in the
;; elisp info file and suddenly I understood that the points I stored
;; were not moving together with the text.  During that rewrite I
;; renamed the whole thing to mtorus reflecting the topology of the
;; main data structure.  All this happened during autumn and winter of
;; 2002.  In early 2003 I came across swbuff.el which had a very nice
;; popup window feature that I definetely admired and wanted to
;; have.  It became clear that a full rewrite had to be done once again
;; because the code (which was then just released to the public one
;; week ago) had become quite a mess.  The time of this writing is
;; right during that rewrite.  Why I tell this all here? Hm, probably
;; mainly for myself to remember when I'm grey and old (and still be
;; using XEmacs ;-)

;;; Code:
(eval-when-compile
  (require 'cl)
  (require 'timer)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable User Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup mtorus nil
  "An interface to navigating groups of buffers.
This code should work fast so intuition is a very important
matter.  Some of the customizable variables are switches to tune the
behavior of the functions to your habits.  Hopefully it is possible to
find good settings for many people."
  :tag "MTorus"
  :link '(url-link :tag "Home Page"
                   "http://www.skamphausen.de/software/skamacs/")
  :link '(emacs-commentary-link
          :tag "Commentary in mtorus.el" "mtorus.el")
  :prefix "mtorus-"
  :group 'environment
  :group 'extensions
  :group 'convenience)


(defcustom mtorus-init-hook nil
  "Hook run after the torus is initialized."
  :type 'hook
  :group 'mtorus)

(defcustom mtorus-init-rings-emtpy nil
  "*Whether to create a new ring with a marker at point."
  :type 'boolean
  :group 'mtorus)

(defcustom mtorus-notify-method 't
  "*Controls how the status is displayed to the user.
If set to 't' mtorus uses a popup window and the echo erea.

If set to 'popup' only the popup window will be used

If set to 'echo'  only the echo area will be used.

Set this to 'nil' to avoid notifying at all."
  :type '(choice (const t) (const nil) (const popup) (const echo))
  :group 'mtorus)

(defcustom mtorus-buffer-skip-p
  'mtorus-default-buffer-skip-p
  "Predicate to use to skip buffers when cycling the real buffer list.
This has nothing to do with the cycling inside a normal ring.
A good example would be to use the result of
  (string-match \"^[ \\*]+\" (buffer-name buffer))
which skips the buffers with a star or a space at the beginning of
their buffer names.
The default predicate `mtorus-default-buffer-skip-p'  skips
buffers whose names begin with a space."
  :type 'function
  :group 'mtorus)

(defcustom mtorus-save-on-exit nil
  "*Whether to save the current torus to the current dir on exit.
This is an ALPHA feature."
  :type 'boolean
  :group 'mtorus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; How to display information
(defgroup mtorus-notify nil
  "Controls the display of information in mtorus."
  :tag "MTorus Notify"
  :prefix "mtorus-notify"
  :group 'mtorus)

(defcustom mtorus-notify-popup-clear-timeout 4
  "*Time in seconds before the pop up window is removed."
  :type 'number
  :group 'mtorus-pop-up)

(defcustom mtorus-notify-popup-separator " - "
  "String appearing between two entries in pop up window."
  :type 'string
  :group 'mtorus-pop-up)


(defvar mtorus-notify-popup-buffer-name " *mtorus*"
  "Name of the temporary buffer to display the torus.")

(defvar mtorus-notify-popup-timer nil
  "The timer used to remove the popup window.")



(defun mtorus-install-suggested-bindings ()
  "This sets the key-bindings that I consider useful.
The bindings don't not fulfill the requirements of good key-defining
but I like it and I provide it only as a convenience function.

Special care for CUA users is taken."
  (interactive)
  (message "installed")
  (cond
   ((featurep 'cua) ;; FIXME how to detect?
    (global-set-key '[(f10)] 'mtorus-next-marker)
    (global-set-key '[(f9)]  'mtorus-prev-marker)
    (global-set-key '[(shift f10)]    'mtorus-next-ring)
    (global-set-key '[(shift f9)]  'mtorus-prev-ring))
   (t
    (global-set-key '[(shift right)] 'mtorus-next-marker)
    (global-set-key '[(shift left)]  'mtorus-prev-marker)
    (global-set-key '[(shift up)]    'mtorus-next-ring)
    (global-set-key '[(shift down)]  'mtorus-prev-ring)))
   
   ;; ring handling: f11
   (global-set-key '[(f11)]
     'mtorus-new-ring)
   (global-set-key '[(shift f11)]
     'mtorus-delete-ring)
   (global-set-key '[(control f11)]
    'mtorus-notify)
   ;; marker handling: f12
   (global-set-key '[(f12)]
     'mtorus-new-marker)
   (global-set-key '[(shift f12)]
     'mtorus-delete-current-marker)
   (global-set-key '[(control f12)]
     'mtorus-update-current-marker))

;;;;;;;;;;;;;;;;;;;
;; MTorus internals

;; Default Settings
(defun mtorus-default-buffer-skip-p (buffer)
  "The default predicate used for `mtorus-buffer-skip-p'
is to skip only the special buffers whose name begins with a space."
  (string-match "[ ]+" (buffer-name buffer)))

(defcustom mtorus-buffer-list-name "*buffer-list*"
  "The name of the ring that always contains all open buffers.
Cycling within this ring is different from cycling the others since it
always uses the real buffer list.  It skips all buffers that
`mtorus-buffer-skip-p' returns t for and is not editable."
  :type 'string
  :group 'mtorus)

;; Variables
(defvar mtorus-torus nil
  "Alist containing the rings of markers.
The main data structure of MTorus:
 \(\(\"ring1\" \(\marker1
            marker2
            marker3
            marker4))
 \(\"ring2\" \(\marker5
            marker6
            marker7)))

Positions are stored as markers so that they keep in place when
altering the contents of the buffer.")

(defun mtorus-message (msg)
  (message msg))
(defalias 'mtorus-make-extent 'make-overlay)
(defalias 'mtorus-set-extent-property 'overlay-put)
(defalias 'mtorus-delete-extent 'delete-overlay)

(defun mtorus-set-extent-face (extent face)
  (mtorus-set-extent-property extent 'face face))

;; from matlab.el:
(cond ((fboundp 'point-at-bol)
       (defalias 'mtorus-point-at-bol 'point-at-bol)
       (defalias 'mtorus-point-at-eol 'point-at-eol))
      ((fboundp 'line-beginning-position)
       (defalias 'mtorus-point-at-bol 'line-beginning-position)
       (defalias 'mtorus-point-at-eol 'line-end-position))
      (t
       (defmacro mtorus-point-at-bol ()
     (save-excursion (beginning-of-line) (point)))
       (defmacro mtorus-point-at-eol ()
     (save-excursion (end-of-line) (point)))))

;; Testing function
;(defun tt ()
;  ""
;  (interactive)
;  (setq debug-on-error t)
;  (mtorus-init)
;  (mtorus-new-ring "ring1")
;  (forward-line)
;  (mtorus-new-marker)
;  (describe-variable 'mtorus-torus)
;  )


;; Commands

;; all other functions depend on this being run first, so not further
;; autoloads
;;###autoload
(defun mtorus-init ()
  "This inits the torus in `mtorus-torus' and creates the special ring
  `mtorus-buffer-list-name' (see there)."
  (interactive)
  (setq mtorus-torus nil)
  (mtorus-new-ring mtorus-buffer-list-name)
  (mtorus-maybe-install-kill-hook)
  (run-hooks 'mtorus-init-hook))


;; Rings
(defun mtorus-new-ring (ring-name)
  "Create a ring with name RING-NAME (asked from user).
If `mtorus-init-rings-emtpy' is non nil a marker at the current point
is created and pushed on the list, otherwise the ring stays empty for
the moment.  Makes the new ring the current ring.

It won't create a ring with a name that already exists."
  (interactive "sRing name: ")
  (if (mtorus-ringp ring-name)
      (mtorus-message
       (format "A ring with name \"%s\" already exists."
               ring-name))
    (let* ((content (mtorus-initial-ring-contents ring-name))
          (ring (cons ring-name (list content))))
      (setq mtorus-torus
            (append mtorus-torus
                    (list ring))))
    (mtorus-switch-to-ring ring-name t)))

(defun mtorus-delete-ring (&optional ring-name)
  "Delete the ring with name RING-NAME.
If none is given it is asked from the user."
  (interactive)
  (let ((rname (or ring-name (mtorus-ask-ring))))
    (if (not (mtorus-special-ringp rname))
        (if (y-or-n-p (format "delete ring \"%s\"? " rname))
            (setq mtorus-torus
                  (delete* rname mtorus-torus :key 'car
                           :test 'equal)))
      (mtorus-message "can't delete special rings"))))

(defun mtorus-rename-ring (&optional ring-name new-name)
  "Rename RING-NAME to NEW-NAME asking if omitted."
  (interactive)
  (let* ((rname (or ring-name (mtorus-ask-ring)))
        (nname (or new-name (read-string
                             (format "rename \"%s\" to: " rname)))))
    (if (not (mtorus-special-ringp rname))
        (setcar (assoc rname mtorus-torus) nname)
      (mtorus-message "can't rename special rings"))))
      
(defun mtorus-switch-to-ring (ring-name &optional quiet)
  "Make RING-NAME the current ring."
  (while (not (mtorus-current-ringp ring-name))
    (mtorus-rotate-rings 1))
  (unless quiet (mtorus-notify)))

(defun mtorus-next-ring ()
  "Make the next ring on the torus the current ring."
  (interactive)
  (mtorus-switch-to-ring (mtorus-nth-ring-name 1)))

(defun mtorus-prev-ring ()
  "Make the next ring on the torus the current ring."
  (interactive)
  (mtorus-switch-to-ring
   (mtorus-nth-ring-name (1- (length mtorus-torus)))))


;; Marker
(defun mtorus-modify-torus (ring-name func)
  (if (not (mtorus-special-ringp ring-name))
      (let ((new-ring
             (funcall func (copy-list
                            (mtorus-ring-by-name ring-name)))))
        (setq mtorus-torus
              (mapcar #'(lambda (item)
                          (if (string= (first item)
                                       ring-name)
                              new-ring
                            item))
                      mtorus-torus))
        )
    (mtorus-message "can't edit special lists")))


(defun mtorus-rename-current-ring (&optional ring-name new-name)
  "Rename RING-NAME to NEW-NAME asking if omitted."
  (interactive)
  (let* ((rname (or ring-name (mtorus-ask-ring)))
        (nname
         (or new-name
             (read-string (format "rename \"%s\" to: " rname)))))
    (mtorus-modify-torus
     ring-name #'(lambda (ring)
                   (list nname (second ring))))))

(defun mtorus-new-marker ()
  "Create a new marker at the current position.
It is added to the current ring and made the current entry in that
ring."
  (interactive)
  (mtorus-modify-torus
   (mtorus-current-ring-name)
    #'(lambda (ring)
         (list (first ring)
               (cons (point-marker)
                     (second ring))))))


(defun mtorus-delete-current-marker ()
  "Delete the current marker from the current ring."
  (interactive)
  (mtorus-modify-torus
   (mtorus-current-ring-name)
    #'(lambda (ring)
        ;; hopefully this is good enough w/ garbage collection?
        (set-marker (first (second ring)) nil)
        (list (first ring)
              (cdr (second ring))))))

(defun mtorus-update-current-marker ()
  "Make the current marker point to the current position."
  (interactive)
  (mtorus-modify-torus
   (mtorus-current-ring-name)
    #'(lambda (ring)
        (set-marker (first (second ring))
                    (point))
        ring)))

(defun mtorus-next-marker ()
  "Switch to the next marker in the current ring.
Handles special rings different like cycling the buffer list when the
current's ring name is equal to `mtorus-buffer-list-name'."
  (interactive)
  (if (mtorus-special-ringp (mtorus-current-ring-name))
      (mtorus-blist-next)
    (mtorus-rotate-entries 0 1))
  (mtorus-jump-current-marker))

(defun mtorus-prev-marker ()
  (interactive)
  (if (mtorus-special-ringp (mtorus-current-ring-name))
      (mtorus-blist-prev)
    (mtorus-rotate-entries 0 -1))
  (mtorus-jump-current-marker))

(defun mtorus-jump-current-marker ()
  "Move point to the point and buffer defined by current marker."
  (interactive)
  (let ((buf (mtorus-current-buffer))
        (pos (mtorus-current-pos)))
    (if (buffer-live-p buf)
        (progn
          (switch-to-buffer buf)
          (goto-char pos)
          (mtorus-notify))
      (mtorus-message (format "no such buffer %s"
                              (buffer-name buf)))
      (mtorus-delete-current-marker))))
;(defun mtorus-delete-marker ()
;  "Delete the current marker from the current ring."
;  (interactive)
;  (if (not (mtorus-special-ringp
;            (mtorus-current-ring-name)))
;      (let ((ring (mtorus-current-ring)))
;        (set-marker (mtorus-current-marker) nil)
;        (setf (second ring)
;              (cdr (second (mtorus-current-ring)))))
;    (mtorus-message "can't delete from special rings")))

;(defun mtorus-update-current-marker ()
;  "Make the current marker point to the current position.
;Actually the current marker is deleted and a new one is created at the
;current position."
;  (interactive)
;  (and (mtorus-delete-marker)
;       (mtorus-new-marker)))

;; FIXME: can the actual rotation be done by one backend function for
;; this and for mtorus-rotate-rings?
(defun mtorus-rotate-entries (nth-ring amount)
  "Rotate the NTH-RING AMOUNT times."
  (let* ((ring (mtorus-ring-by-name
                (mtorus-nth-ring-name nth-ring)))
         (rlist (second ring)))
    (if (> amount 0)
        (while (> amount 0)
          (setq rlist
                (append (cdr rlist)
                        (list (car rlist))))
          (setq amount (1- amount)))
      (while (< amount 0)
        (setq rlist
              (append (last rlist)
                      (butlast rlist)))
        (setq amount (1+ amount))))
    (setf (second ring) rlist)))
    
;; Special Ring: The Buffer List
(defun mtorus-blist-next ()
  "Cycle the real buffer list.
This can be used separately and is always used when navigating through
the default ring.  It skips buffers for that
`mtorus-buffer-skip-p' returns t."
  (interactive)
  (let ((blist (mtorus-buffer-list)))
    (when (> (length blist) 1)
      (bury-buffer)
      (while (funcall mtorus-buffer-skip-p (current-buffer))
        (bury-buffer)))))

(defun mtorus-blist-prev ()
  "Cycle the real buffer list.
This can be used separately and is always used when navigating through
the default ring.  It skips buffers for that
`mtorus-buffer-skip-p' returns t."
  (interactive)
  (let ((blist (mtorus-buffer-list)))
    (if blist (switch-to-buffer (car (reverse blist))))))

(defun mtorus-buffer-list ()
  "Return a filtered buffer list according to `mtorus-buffer-skip-p'."
  (mtorus-grep (buffer-list)
               (lambda (buf)
                 (not (funcall mtorus-buffer-skip-p buf)))))

(defun mtorus-grep (l predicate)
  "Helper function for a grep over a list.
Apply predicate PREDICATE to all elements of list L and return a list
consisting of all elements PREDICATE returned t for."
  (defun helper (ret-list rest)
    (if (null rest)
        (reverse ret-list)
      (progn
        (if (funcall predicate (car rest))
            (setq ret-list (cons (car rest) ret-list)))
        (helper ret-list (cdr rest)))))
  (helper '() l))

;; GUI
;; Code and idea highly inspired by swbuff.el by D. Ponce. Thanks!
(defvar mtorus-notify-status-freeze nil
  "A temporary freezed description of the current torus.
Avoids rearrangement of the popup window every time the main data
structure `mtorus-torus' is changed.  It's structure is similar to
mtorus but allows for better access during the creation of the notify
window.")

(defun mtorus-notify ()
  "Notify the user of the current status of the torus.
This might just use the echo area or popup it's own window according
to the settings of `mtorus-notify-method'."
  (interactive)
  (when (not mtorus-notify-status-freeze)
    (setq mtorus-notify-status-freeze
          (mtorus-freeze-torus)))
  (cond ;; FIXME: other methods
   ((eq mtorus-notify-method t)
    (mtorus-notify-popup))
   ((eq mtorus-notify-method 'popup)
    (mtorus-notify-popup))
   ((eq mtorus-notify-method 'echo))
   (t))
  (mtorus-highlight-line))

(defun mtorus-freeze-torus ()
  "Create a freezed status snapshot of `mtorus-torus'.
This changes the torus by creating the current entry strings for all
entries to avoid repeated calls."
  (mapcar
   #'(lambda (ring)
       (let ((cringn (car ring)))
         (cons
          cringn
          (list
           (if (mtorus-special-ringp cringn)
               (cond
                ((string-equal cringn mtorus-buffer-list-name)
                 (mapcar #'(lambda (buf)
                             (with-current-buffer buf
                               (mtorus-entry-to-string
								;; FIXME: good enough for GC?
                                (point-marker))))
                         (mtorus-buffer-list))))
             (mapcar #'(lambda (marker)
                         (mtorus-entry-to-string marker))
                     (second ring)))))))
       mtorus-torus))


(defun mtorus-notify-popup ()
  "Open a navigation window at the bottom of the current window.
The upper line shows all rings and the lower line all the current
entries in that ring.  The window will vanish on the next action taken
or if `mtorus-notify-popup-clear-timeout' seconds go by."
  (let ((currentry-s (mtorus-current-entry-string))
        (window-min-height 4))
    (with-current-buffer
        (get-buffer-create mtorus-notify-popup-buffer-name)
      (let ((w (or (get-buffer-window mtorus-notify-popup-buffer-name)
                   (split-window-vertically -4))))
        (set-window-buffer w (current-buffer))
        ;; insert the torus description
        (erase-buffer)
        (mtorus-insert-status currentry-s)
        (add-hook 'pre-command-hook
                  'mtorus-notify-maybe-cleanup)
        (if (timerp mtorus-notify-popup-timer)
            (cancel-timer mtorus-notify-popup-timer))
        (setq mtorus-notify-popup-timer
              (run-with-timer
               mtorus-notify-popup-clear-timeout nil
               #'mtorus-notify-maybe-cleanup))))))

;; (defface mtorus-highlight-face
;;   '((((class color) (background light))
;;      (:background "khaki"))
;;     (((class color) (background dark))
;;      (:background "sea green"))
;;     (((class grayscale monochrome)
;;       (background light))
;;      (:background "black"))
;;     (((class grayscale monochrome)
;;       (background dark))
;;      (:background "white")))
;;   "Face for the highlighting of the line jumped to."
;;   :group 'mtorus)
;; (setq frame-background-mode 'light)

(defalias 'mtorus-highlight-face 'highlight)

(defface mtorus-notify-highlight-face
  '((((class color) (background light))
     (:foreground "red"))
    (((class color) (background dark))
     (:foreground "green"))
    (((class grayscale monochrome)
      (background light))
     (:foreground "black"))
    (((class grayscale monochrome)
      (background dark))
     (:background "white")))
  "Face for the highlighting the current entry in the notify window."
  :group 'mtorus)
  

(defun mtorus-insert-status (currentry-s)
  "Insert a description of the torus in the current buffer.
This is used inside the notify popup window and displays all ring
names plus the contents of the current ring."
  (let ((rings (mapcar 'car mtorus-notify-status-freeze))
        (cringn (mtorus-current-ring-name))
        entry start)
    (while rings
      (setq entry (car rings)
            rings (cdr rings)
            start (point))
      (insert entry)
      (when (string-equal entry cringn)
        (set-text-properties
         start (point) '(face mtorus-notify-highlight-face))
        (save-excursion
          (insert "\n")
          (mtorus-insert-ring-description currentry-s
           (cadr (assoc entry mtorus-notify-status-freeze)))))
        (insert " ")
      )))

(defun mtorus-insert-ring-description (curr entries)
  "Insert descriptions of ENTRIES in the current buffer.
This is used inside mtorus-insert-status to insert the description of
all the current entries."
  ;; (message (format "%s" entries))
  (let* (entry start)
   (while entries
     (setq entry (car entries)
           entries (cdr entries)
           start (point))
     (insert entry)
     (when (string-equal entry curr)
       (set-text-properties
        start (point) '(face mtorus-notify-highlight-face)))
     (insert "  "))))
   
   
;; Used to prevent discarding the notify window on some mouse event.
(defalias 'mtorus-ignore 'ignore)

(defun mtorus-notify-maybe-cleanup ()
  "Function to sit on `pre-command-hook' and track successive calls
to the cycling commands."
  (if (memq this-command '(mtorus-next-ring
                           mtorus-prev-ring
                           mtorus-next-marker
                           mtorus-prev-marker
                           mtorus-ignore))
      nil
    (mtorus-notify-cleanup)
    (if (timerp mtorus-notify-popup-timer)
        (cancel-timer mtorus-notify-popup-timer))
    (setq mtorus-notify-status-freeze nil)
    (setq mtorus-notify-popup-timer nil)
    (remove-hook 'pre-command-hook
                 'mtorus-notify-maybe-cleanup)))

(defun mtorus-notify-cleanup ()
  "Discard the notify window."
  (let ((w (get-buffer-window mtorus-notify-popup-buffer-name))
        (b (get-buffer mtorus-notify-popup-buffer-name)))
    (and w (delete-window w))
    (and b (kill-buffer b))))

;; FIXME: cleanly merge this with the notify code?
(defun mtorus-highlight-line ()
  "Show the line you jumped to by highlighting it."
  (setq mtorus-highlight-extent
        (mtorus-make-extent (mtorus-point-at-bol)
                            (mtorus-point-at-eol)))
  (mtorus-set-extent-face mtorus-highlight-extent
                          'mtorus-highlight-face)
  (add-hook 'pre-command-hook
            'mtorus-unhighlight-line))

(defun mtorus-unhighlight-line ()
  "Remove highlighting of the current line if any."
  (if mtorus-highlight-extent
      (progn
        (mtorus-delete-extent mtorus-highlight-extent)
        (setq mtorus-highlight-extent nil)
        (remove-hook 'pre-command-hook
                     'mtorus-unhighlight-current-line))))


;; Backend Level Functions
(defun mtorus-initial-ring-contents (ring-name)
  "Return a list to initialize RING-NAME with.
Takes care of special ring names"
  (list 
   (cond
    ((mtorus-special-ringp ring-name)
     (concat "special: "
             ring-name))
    ((not mtorus-init-rings-emtpy)
     (point-marker))
    (t
     ()))))

(defun mtorus-ask-ring ()
  "Ask the user to choose a ring (with completion)."
  (let ((default (mtorus-current-ring-name)))
    (completing-read
     (format "choose a ring (%s): " default)
     mtorus-torus
     nil t nil nil
     default)))

(defun mtorus-rotate-rings (amount)
  "Rotate the rings on the torus AMOUNT times."
  (if (> amount 0)
      (while (> amount 0)
        (setq mtorus-torus
              (append (cdr mtorus-torus)
                      (list (car mtorus-torus))))
        (setq amount (1- amount)))
    (while (< amount 0)
      (setq mtorus-torus
            (append (last mtorus-torus)
                    (butlast (car mtorus-torus))))
      (setq amount (1+ amount)))))


;; Ring Accessors
(defun mtorus-current-ring-name ()
  "Return a string containing the name of the current ring."
  (mtorus-nth-ring-name 0))

(defun mtorus-nth-ring-name (nth)
  "Return a string containing the name of the NTH ring on the torus.
For the current entry NTH is 0 and for the last NTH is length -1."
  (car (elt mtorus-torus (% nth (length mtorus-torus)))))

(defun mtorus-ring-names ()
  "Return a list of all available ring names as strings."
  (mapcar 'car mtorus-torus))

(defun mtorus-current-ring ()
  "Return the current ring as a list.
The current ring is always the CDR of the 0th in the list."
  (first mtorus-torus))

(defun mtorus-ring-by-name (ring-name)
  "Return the ASSOC of NAME in `mtorus-torus'."
  (assoc ring-name mtorus-torus))

(defun mtorus-add-to-ring (ring-name marker)
  "Put MARKER on the ring named RING-NAME."
  (let ((ring (mtorus-ring-by-name ring-name)))
    (setf (second ring)
          (cons marker
                (second ring)))))

;; Entry Accessors
(defun mtorus-current-entry-string ()
  "Return a string containing a description of the current entry."
  (mtorus-entry-to-string (mtorus-current-marker)))

(defun mtorus-entry-to-string (marker)
  "Return a stringified description of MARKER."
  (if (buffer-live-p (marker-buffer marker))
    (format "%s@%d"
            (buffer-name (marker-buffer marker))
            (marker-position marker))
    "*del*"))

(defun mtorus-current-marker ()
  "Return the current marker in the current ring."
  (if (mtorus-special-ringp (mtorus-current-ring-name))
	  ;; FIXME: good enough for GC?
      (point-marker)
    (first (second (first mtorus-torus)))))

(defun mtorus-current-pos ()
  "Return the current entry's position in the buffer."
  (marker-position (mtorus-current-marker)))

(defun mtorus-current-buffer ()
  "Return the current entry's buffer."
  (marker-buffer (mtorus-current-marker)))

;; Predicates
(defun mtorus-current-ringp (ring-name)
  "Return non-nil if RING-NAME is the current ring.
That is if it is first on the torus."
 (string-equal (mtorus-current-ring-name) ring-name))

(defun mtorus-special-ringp (ring-name)
  "Return non-nil if the ring-name looks like a special ring.
By convention special ring names begin with a '*'."
  (char-equal ?* (elt ring-name 0)))

(defun mtorus-ringp (ring-name)
  "Return non-nil if ring-name is found on `mtorus-torus'."
  (and (stringp ring-name)
       (assoc ring-name mtorus-torus)))

(defun mtorus-maybe-install-kill-hook ()
  "Install some functions on the `kill-emacs-hook' according to custom
  settings and assuring no duplicates."
  (when (and mtorus-save-on-exit
             (not (memq 'mtorus-quit kill-emacs-hook)))
    (add-hook 'kill-emacs-hook
              'mtorus-quit)))

(provide 'mtorus)

;;; newtorus.el ends here