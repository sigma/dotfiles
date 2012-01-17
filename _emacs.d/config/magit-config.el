;;; magit-config.el ---

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

;;

;;; Code:

(when (request 'magit)
  (request 'magit-svn)
  (request 'magit-topgit)
  (request 'magit-gh-pulls)

  (when (fboundp 'magit-load-config-extensions)
    (add-hook 'magit-mode-hook 'magit-load-config-extensions)))

(provide 'magit-config)
;;; magit-config.el ends here
