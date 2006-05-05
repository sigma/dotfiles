;;; ecb-config.el ---

;; Copyright (C) 2006  Free Software Foundation, Inc.

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

(require 'ecb-util)
(require 'ecb-layout)
(require 'ecb-common-browser)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(defconst ecb-reftex-buffer-name "*toc*")
(defvar ecb-reftex-last-file nil)

(defun ecb-reftex-sync ()
  (interactive)

  (ecb-do-if-buffer-visible-in-ecb-frame 'ecb-reftex-buffer-name

    (let ((filename (buffer-file-name (current-buffer))))

      (if (and filename (file-readable-p filename))

          ;; synchronizing for real filesource-buffers

          ;; Let us be smart: We synchronize only if sourcebuffer has changed
          (when (not (ecb-string= (ecb-fix-filename filename)
                                  (ecb-fix-filename
                                   ecb-examples-bufferinfo-last-file)))
            ;; set new last-file-name so we can check next time if changed
            (setq ecb-examples-bufferinfo-last-file filename)
            ;; we display the file-infos for current source-file
            (reftex-toc))


        ;; what should we do for non file buffers like help-buffers etc...
        (setq ecb-examples-bufferinfo-last-file nil)))))

(defun ecb-maximize-reftex-window ()
  (interactive)
  (ecb-maximize-ecb-buffer ecb-reftex-buffer-name t))

(defun ecb-goto-reftex-window ()
  "Make the bufferinfo-window the current window."
  (interactive)
  (ecb-goto-ecb-window ecb-reftex-buffer-name))

(defecb-window-dedicator ecb-set-reftex-buffer
    ecb-reftex-buffer-name
  "Set the buffer in the current window to the bufferinfo-buffer and make this
window dedicated for this buffer."
  (switch-to-buffer (get-buffer-create ecb-reftex-buffer-name))
  (setq buffer-read-only t))

(ecb-layout-define "reftex-layout1" left
    ;; dedicating the bufferinfo window to the bufferinfo-buffer
  (ecb-set-reftex-buffer)

  ;; selecting the edit-window
  (select-window (next-window)))

(defvar ecb-reftex-preact-layout nil)
(defvar ecb-reftex-preact-windows-height nil)
(defun ecb-reftex-preactivation-state(action)
  (cond ((equal action 'save)
         (setq ecb-reftex-preact-layout
               ecb-layout-name
               ecb-reftex-preact-windows-height
               ecb-windows-height))
        ((equal action 'restore)
         (setq ecb-layout-name
               ecb-reftex-preact-layout
               ecb-windows-height
               ecb-reftex-preact-windows-height))))

(defun ecb-reftex-activate ()
  "Activate the new layout \"example-layout1\".
Add `ecb-reftex-bufferinfo-sync' to `ecb-current-buffer-sync-hook', set
`ecb-compile-window-height' to 5 and `ecb-windows-height' to 6. The
preactivation-state is saved and will be restored by
`ecb-reftex-deactivate'."
  (interactive)

  (assert (featurep 'ecb) nil
          "ECB must be loaded!")
  (assert ecb-minor-mode nil
          "ECB must be activated!")
  (assert (equal (selected-frame) ecb-frame) nil
          "The ECB-frame must be selected!")
  (assert (not (ecb-string= ecb-layout-name "reftex-layout1")) nil
          "The reftex-layout1 is already active!")

  ;; activating the synchronization of the bufferinfo-window
  (add-hook 'ecb-current-buffer-sync-hook
            'ecb-reftex-sync)

  ;; saving the state
  (ecb-reftex-preactivation-state 'save)

  ;; switch to our prefered layout
  (ecb-layout-switch "reftex-layout1"))

(defun ecb-reftex-deactivate ()
  "Deactivate the new layout \"example-layout1\".
Remove `ecb-reftex-bufferinfo-sync' from `ecb-current-buffer-sync-hook' and
restore the state as before activation."
  (interactive)

  (assert (featurep 'ecb) nil
          "ECB must be loaded!")
  (assert ecb-minor-mode nil
          "ECB must be activated!")
  (assert (equal (selected-frame) ecb-frame) nil
          "The ECB-frame must be selected!")
  (assert (ecb-string= ecb-layout-name "reftex-layout1") nil
          "The reftex-layout1 is not active!")

  (remove-hook 'ecb-current-buffer-sync-hook
               'ecb-reftex-sync)
  (ecb-reftex-preactivation-state 'restore)
  (ecb-layout-switch ecb-layout-name))

(provide 'ecb-config)
;;; ecb-config.el ends here
