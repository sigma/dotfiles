;;; lispy-config.el ---

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

;;; Lispy : client for telnet-based chat server

(autoload 'lispy "lispy-session" "" t nil)
(eval-after-load "lispy"
  '(progn
     (request 'lispy-commands)
     (request 'lispy-history)
     (request 'lispy-font-lock)
     (request 'lispy-occur)
     (request 'lispy-h4x0r)
     (request 'lispy-osd)
     (request 'lispy-autoreconnect)
     (request 'lispy-limit)
     ))


(provide 'lispy-config)
;;; lispy-config.el ends here
