;;; tempo-c++.el --- abbrevs for c/c++ programming

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Yann Hodique <Yann.Hodique@lifl.fr>
;; Keywords: c, languages

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
;; This is a way to hook tempo into cc-mode
;;

;;; Code:

(defvar c-tempo-tags nil
  "Tempo tags for C mode")

(defvar c++-tempo-tags nil
  "Tempo tags for C++ mode")

(defvar c-tempo-keys-alist nil
  "")

;;; C-Mode Templates and C++-Mode Templates (uses C-Mode Templates also)
(require 'tempo)
(require 'mycompletion)

;;; Preprocessor Templates (appended to c-tempo-tags)

(yh/tempo-define-template "c-include"
                          "#i"
                          'c-tempo-keys-alist
                          '("#include <" r ".h>" > n)
                          "#include"
                          "Insert a #include <> statement"
                          'c-tempo-tags)

(yh/tempo-define-template "c-ifdef"
                          "#f"
                          'c-tempo-keys-alist
                          '("#ifdef " (p "ifdef-clause: " clause) > n> p n
                            "#else /* !(" (s clause) ") */" n> p n
                            "#endif /* " (s clause)" */" n>)
                          "#ifdef"
                          "Insert a #ifdef #else #endif statement"
                          'c-tempo-tags)

(yh/tempo-define-template "c-ifndef"
                          "#n"
                          'c-tempo-keys-alist
                          '("#ifndef " (p "ifndef-clause: " clause) > n
                            "#define " (s clause) n> n ~ n n
                            "#endif /* " (s clause)" */" n>)
                          "#ifndef"
                          "Insert a #ifndef #define #endif statement"
                          'c-tempo-tags)

;;; C-Mode Templates

(yh/tempo-define-template "c-if"
                          "i"
                          'c-tempo-keys-alist
                          '(> "if(" (p "if-clause: " clause) ")"
                              " {" > n> r> n
                              "}" > n>)
                          "if"
                          "Insert a C if statement"
                          'c-tempo-tags)

(yh/tempo-define-template "c-else"
                          "e"
                          'c-tempo-keys-alist
                          '(> "else {" > n> r> n
                              "} /* end of else */" > n>)
                          "else"
                          "Insert a C else statement"
                          'c-tempo-tags)

(yh/tempo-define-template "c-if-else"
                          "I"
                          'c-tempo-keys-alist
                          '(> "if(" (p "if-clause: " clause) ") {" > n> r> ~ n
                              "}" > n>
                              > "else {" > n> n>
                              "}" > n>)
                          "ifelse"
                          "Insert a C if else statement"
                          'c-tempo-tags)

(yh/tempo-define-template "c-while"
                          "w"
                          'c-tempo-keys-alist
                          '(> "while(" (p "while-clause: " clause) ") {" > n> r> n
                              "}" > n>)
                          "while"
                          "Insert a C while statement"
                          'c-tempo-tags)

(yh/tempo-define-template "c-for"
                          "f"
                          'c-tempo-keys-alist
                          '(> "for(" (p "for-clause: " clause) ") {" > n> r> n
                              "}" > n>)
                          "for"
                          "Insert a C for statement"
                          'c-tempo-tags)

(yh/tempo-define-template "c-for-i"
                          "F"
                          'c-tempo-keys-alist
                          '(> "for(" (p "variable: " var) " = 0; " (s var)
                              " < "(p "upper bound: " ub)"; " (s var) "++) {" > n> r> n
                              "}" > n>)
                          "fori"
                          "Insert a C for loop: for(x = 0; x < ..; x++)"
                          'c-tempo-tags)

(yh/tempo-define-template "c-main"
                          "M"
                          'c-tempo-keys-alist
                          '(> "int main(int argc, char *argv[]) {" > n> r> n
                              "return 0;" > n>
                              "}" > n>)
                          "main"
                          "Insert a C main statement"
                          'c-tempo-tags)

(yh/tempo-define-template "c-if-malloc"
                          "m"
                          'c-tempo-keys-alist
                          '(> "if((" (p "variable: " var) " = ("
                              (p "type: " type) " *) malloc(" (p "size: " size) " * sizeof(" (s type)
                              "))) == (" (s type) " *) NULL) {" > n> r> n
                              "}" > n>)
                          "ifmalloc"
                          "Insert a C if(malloc...) statement"
                          'c-tempo-tags)

(yh/tempo-define-template "c-switch"
                          "s"
                          'c-tempo-keys-alist
                          '(> "switch(" (p "switch-condition: " clause) ") {" > n
                              "case " (p "first value: ") ":" > n> p n
                              "break;" > n> p n
                              "default:" > n> p n
                              "break;" > n
                              "}" > n>)
                          "switch"
                          "Insert a C switch statement"
                          'c-tempo-tags)

(yh/tempo-define-template "c-case"
                          "c"
                          'c-tempo-keys-alist
                          '(n "case " (p "value: ") ":" > n> p n
                              "break;" > n> p)
                          "case"
                          "Insert a C case statement"
                          'c-tempo-tags)


;;;C++-Mode Templates

;; Warning : make use of hacked tempo for 'z flag and '~ position

(yh/tempo-define-template "c++-class"
                          "C"
                          'c-tempo-keys-alist
                          '("class " (z "classname: "
                                        (capitalize (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
                                        class) " {" n "public:" n>

                                        (s class) "(" ~ "); //the default constructor()" n>
                                        n > "~" (s class) "(); //the destructor" n n>

                                        (s class)
                                        "(const " (s class) "& ref); //the copy constructor" n>

                                        (s class)
                                        "& operator=(const " (s class) "& ref); //the assignment operator" n>

                                        p n
                                        "protected:" n> p n
                                        "private:" n> p n
                                        "};\t// end of class " (s class) n>)
                          "class"
                          "Insert a class skeleton"
                          'c++-tempo-tags)

(yh/tempo-define-template "c++-for-it"
                          "t"
                          'c-tempo-keys-alist
                          '(> "for(" (p "type: " type) "::iterator "
                              (p "iterator: " iter) " = "
                              (p "name: " name) ".begin(); " (s iter)
                              " != " (s name) ".end(); ++" (s iter) ") {" > n> r> n
                              "}" > n>)
                          "forit"
                          "Insert a C++ for loop: for(container::iterator it = a.begin(); it != a.end(); ++it)"
                          'c-tempo-tags)

(yh/tempo-define-template "c++-for-cit"
                          "T"
                          'c-tempo-keys-alist
                          '(> "for(" (p "type: " type) "::const_iterator "
                              (p "iterator: " iter) " = "
                              (p "name: " name) ".begin(); " (s iter)
                              " != " (s name) ".end(); ++" (s iter) ") {" > n> r> n
                              "}" > n>)
                          "forcit"
                          "Insert a C++ for loop: for(container::const_iterator it = a.begin(); it != a.end(); ++it)"
                          'c-tempo-tags)

(yh/tempo-define-template "c++-qt--for-it"
                          "q"
                          'c-tempo-keys-alist
                          '(> "for(" (p "type: " type) "::Iterator "
                              (p "iterator: " iter) " = "
                              (p "name: " name) ".begin(); " (s iter)
                              " != " (s name) ".end(); ++" (s iter) ") {" > n> r> n
                              "}" > n>)
                          "qforit"
                          "Insert a C++ for loop: for(container::Iterator it = a.begin(); it != a.end(); ++it)"
                          'c-tempo-tags)

(yh/tempo-define-template "c-for-it"
                          "Q"
                          'c-tempo-keys-alist
                          '(> "for(" (p "type: " type) "::ConstIterator "
                              (p "iterator: " iter) " = "
                              (p "name: " name) ".begin(); " (s iter)
                              " != " (s name) ".end(); ++" (s iter) ") {" > n> r> n
                              "}" > n>)
                          "qforcit"
                          "Insert a C++ for loop: for(container::ConstIterator it = a.begin(); it != a.end(); ++it)"
                          'c-tempo-tags)

(add-hook 'c-mode-common-hook
          (lambda ()
            (unless (eq major-mode 'php-mode)
              (tempo-use-tag-list 'c-tempo-tags)
              (tempo-use-tag-list 'c++-tempo-tags)
              (yh/tempo-build-local-map c-tempo-keys-alist))))

(provide 'tempo-c++)
;;; tempo-c++.el ends here
