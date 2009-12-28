;;; command-config.el --- code affecting M-x related stuff

;; Copyright (C) 2009  Free Software Foundation, Inc.

;; Author: Yann Hodique <yhodique@vmware.com>
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

;; This file contains modifications to the regular M-x behavior

;;; Code:

(eval-after-load 'smex
  '(progn
     (smex-initialize)
     (global-set-key (kbd "M-x") 'smex)
     (global-set-key (kbd "M-X") 'smex-major-mode-commands)
     (global-set-key (kbd "C-c x") 'smex-update-and-run)
     ;; This is your old M-x.
     (global-set-key (kbd "A-x") 'execute-extended-command)))

(eval-after-load 'ecb
  '(smex-update))

(provide 'command-config)
;;; command-config.el ends here
