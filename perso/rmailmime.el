;;; rmailmime.el --- Rmail: MIME message reading.

;; Copyright (C) 1993-1995 Masanobu UMEDA

;; Author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
;; Version: $Header: /cvsroot/OLD/dotemacs/dotemacs/perso/rmailmime.el,v 1.1 2004/06/15 16:02:05 sigma Exp $
;; Keywords: mail, mime

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

;; LCD Archive Entry:
;; rmailmime|Masanobu UMEDA|umerin@mse.kyutech.ac.jp|
;; Rmail: MIME message support.|
;; $Date: 2004/06/15 16:02:05 $|$Revision: 1.1 $|~/misc/rmailmime.el.Z|

;; Usage examples:
;; First of all, define the following autoload entries:
;;
;; (autoload 'rmail-show-mime		"rmailmime" "Show MIME messages."  t)
;; (autoload 'rmail-convert-mime-header	"rmailmime" "Convert MIME header." nil)
;;
;; To convert MIME headers into readable form automatically in Rmail,
;; set the variable rmail-message-filter to the function
;; rmail-convert-mime-header as follows:
;; 
;; (setq rmail-message-filter 'rmail-convert-mime-header)
;;
;; In VM, use vm-message-filter instead of rmail-message-filter (The
;; variable vm-message-filter may not be defined in older versions of
;; VM released before rmailmime.el is developed).
;;
;; (setq vm-message-filter 'rmail-convert-mime-header)
;;
;; To show MIME messages using metamail program in Rmail, bind the
;; command rmail-show-mime to some key in rmail mode.  The following
;; example binds it to the key `!':
;; 
;; (setq rmail-mode-hook
;;       (list
;;        (function
;;         (lambda ()
;;           (local-set-key "!" 'rmail-show-mime)
;;           ))))
;;
;; In VM, the following definitions may be requried:
;; 
;; (setq vm-preview-lines nil)
;; (setq vm-invisible-header-regexp nil)
;; (setq vm-visible-headers
;;       (append vm-visible-headers
;;               '("Mime-Version:"
;;                 "Content-Type:"
;;                 "Content-Transfer-Encoding:")))

;;; Code:

(require 'metamail)

;;;###autoload
(defun rmail-show-mime (&optional viewmode do-header)
  "Show a MIME message in current buffer using a View mode.
Optional 1st argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
If an optional 2nd argument DO-HEADER is non-nil, interpret a header part,
too.  Otherwise, a body part is only interpreted.
The contents of current buffer are not changed at all."
  (interactive "p")
  (let ((curbuf (current-buffer)))
    (set-buffer (get-buffer-create "*metamail*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert-buffer curbuf)
    (view-buffer (current-buffer))
    (if do-header
	(metamail-interpret-header))
    (let ((metamail-switches		;Suppress header fields in a body.
	   (append metamail-switches '("-q"))))
      (metamail-interpret-body viewmode))
    ;;(goto-char (point-min))
    ))

;;;###autoload
(defun rmail-convert-mime-header ()
  "Convert MIME header fields of current message into a readable form.
It is expected to be used as rmail-message-filter in Rmail and
vm-message-filter in VM.  Original header is preserved in Rmail."
  (interactive)
  (save-excursion
    ;; Convert only when it has Mime-Version header field.
    (if (save-restriction
	  (narrow-to-region (point-min)
			    (progn
			      (goto-char (point-min))
			      (search-forward "\n\n" nil t)
			      (point)))
	  (mail-fetch-field "Mime-Version"))
	(metamail-interpret-header))))

(provide 'rmailmime)

;;; rmailmime.el ends here
