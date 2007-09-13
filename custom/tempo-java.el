;;; tempo-java.el ---

;; Copyright (C) 2007  Free Software Foundation, Inc.

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

(tempo-define-snippet "java-class"
  '("class " (p "Class: " class) " {\n\n"
    > "public " (s class) "(" p ") {\n" > p n
    "}" > n n "}" > n)
  "jclass"
  "Insert a Java class skeleton"
  nil)

(tempo-define-snippet "java-get-set"
  '("private " (p "Type: " type) " _" (p "Name: " var) ";\n\n"
    > "public " (s type) " get" (upcase-initials (tempo-lookup-named 'var))
    "() {\n"
    > "return _" (s var)  ";\n" "}" > n n
    > "public set" (upcase-initials (tempo-lookup-named 'var))
    "(" (s type) " value) {\n"
    > "_" (s var) " = value;\n" "}" > n)
  "getset"
  "Insert a getter/setter couple"
  nil)

(provide 'tempo-java)
;;; tempo-java.el ends here
