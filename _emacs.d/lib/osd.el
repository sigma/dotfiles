;;; osd.el --- minor mode for displaying osd messages

;; Copyright (C) 2003  Free Software Foundation, Inc.

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

(defgroup osd nil
  "Osd manipulations")

(defvar osd-buffer nil
  "Buffer in which osd-process infos are displayed"
)

(defcustom osd-proc-name "osd-process"
  "Name of the external process"
  :type 'string
  :group 'osd
)

(defcustom osd-prog "osd_cat"
  "Name of the used executable"
  :type 'string
  :group 'osd
)

(defcustom osd-args '("--delay=3" "--age=3" "--pos=bottom")
  "Arguments for osd-prog"
  :type '(repeat string)
  :group 'osd
)

(defun osd-init () 
  "Inits the osd process if not existing"
  (interactive)
  (let ((proc (get-process osd-proc-name)))
    (if (not proc)
	(eval (append '(start-process osd-proc-name osd-buffer osd-prog) osd-args))
	)
    )
)

(defun osd-close ()
  "Closes the osd process, sending EOF to it"
  (interactive)
  (if (get-process osd-proc-name)
      (process-send-eof osd-proc-name))
  (if (get-process osd-proc-name)
      (delete-process osd-proc-name))
)

(defun osd-display (arg)
  "Displays a message using osd"
  (interactive)
  (if (and (stringp arg)
	   (get-process osd-proc-name))
      (process-send-string osd-proc-name (concat arg "\n")))
  )

(provide 'osd)
;;; osd.el ends here
