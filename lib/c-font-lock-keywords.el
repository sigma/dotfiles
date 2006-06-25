;;;
;;; --- C/C++ KEYWORDS ---
;;;
;;; Written by: Chris Holt <xris@migraine.stanford.edu>
;;;

(require 'font-lock)

;; Setup my two new font-lock faces
(defvar font-lock-special-comment-face 'font-lock-special-comment-face
  "Don't even think of using this.")
(defface font-lock-special-comment-face
  '((((class color) (background dark)) (:foreground "gray80"))
    (((class color) (background light)) (:foreground "blue4"))
    (((class grayscale) (background light))
     (:foreground "DimGray" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :bold t :italic t))
    (t (:bold t)))
  "Font Lock mode face used to highlight special comments."
  :group 'font-lock-faces)

(defvar font-lock-special-keyword-face 'font-lock-special-keyword-face
  "Don't even think of using this.")
(defface font-lock-special-keyword-face
  '((((class color) (background dark)) (:foreground "cyan"))
    (((class color) (background light)) (:foreground "red4"))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (t (:bold t)))
  "Font Lock mode face used to highlight special keywords."
  :group 'font-lock-faces)

;;; This wasn't defined in font-lock.el
(defvar font-lock-warning-face 'font-lock-warning-face
  "Don't even think of using this.")
;;; This face is defined in xemacs-21.0 (though unused), but it is *not*
;;; defined in xemacs-20.4. D'oh. So put it in.
(defface font-lock-warning-face
  '((((class color) (background light)) (:foreground "Red" :bold t))
    (((class color) (background dark)) (:foreground "Pink" :bold t))
    (t (:inverse-video t :bold t)))
  "Font Lock mode face used to highlight warnings."
  :group 'font-lock-faces)


;(setq font-lock-face-list (append font-lock-face-list
;                                  '(font-lock-special-comment-face
;                                    font-lock-special-keyword-face)))

;;; CC-mode keywords
;;;
;;; Okay. This is an overkill c/c++ fontification regexp list. It handles
;;; templates, multiple variable declarations, struct/class function
;;; declarations, and more. What it does not handle is specified below or in
;;; the comments for the particular regexps. I try to make things fast by
;;; anchoring expression to the beginning of a line or starting with an
;;; uncommon character wherever possible, and I heavily use the recent
;;; extensions to the regexp syntax. Plus, all regexps are one-liners.
;;; Despite all that, however, I'm sure these regexps require some decent
;;; computing power. But doesn't everyone use lazy-shot by now? :-)
;;;
;;; Some general comments on the style I support:
;;;
;;; 0. The code makes heavy use of the up-list function, so make sure that
;;;    all blocks are balanced. Throwing in some #ifdefs with hanging braces
;;;    may compile correctly, but it won't highlight correctly.
;;;
;;; 1. Always use braces for if|while|for statements, or things may not
;;;    work. They probably will, though...
;;;
;;; 2. "template <blah>" should be on its own line, although you can have as
;;;    many "template <blah>"s on one line as you want.
;;;
;;; 3. Multi-line template declarations are not guaranteed to work. If
;;;    you're trying to define a type or typedef, it probably won't. If
;;;    you're doing a template <class A, class B, ...> thing, that'll work.
;;;
;;; 4. Templates cannot have whitespace between the name and the "<". For
;;;    example, "pair<T>" works; "pair <T>" doesn't.
;;;
;;; 5. Nested templates must have a space after the first "<". For example,
;;;    "pair<T, T> works; "pair< T, pair<T, T> >" works;
;;;    "pair<T, pair<T, T> >" doesn't.
;;;
;;; 6. You cannot have multiple nested template variable decls on the same
;;;    line. However, you _can_ have a m.n.t. return type, function name,
;;;    and first argument in a function decl (all on one line).
;;;
;;; 7. There is a specific allowed sequence for type qualifirers. See the
;;;    regexp below for exactly what it is.
;;;
;;; 8. The *'s and &'s in types must bind tightly to each other, and also to
;;;    either the type name or the variable, but not both (or neither). For
;;;    example, "foo* bar" works; "foo *bar" works; "foo*bar" doesn't;
;;;    "foo * bar" doesn't. Use the latter two for math expressions.
;;;
;;; 9. There is a maximum of two "::"s per name. How many more than that do
;;;    you need?
;;;
;;; 10. Only the first three variables declared of the same base type in
;;;     a statement are guaranteed to be highlighted correctly.
;;;
;;; 11. The outer parens in a cast must bind tightly to the type they
;;;     contain. For example, "(int*)" works; "( int* )" doesn't. Same thing
;;;     with function arguments. No whitespace between the outer parens are
;;;     supported.
;;;
;;; 12. Only top-level function decls and those declared within a
;;;     class|struct|extern are allowed. To support this, the closest open
;;;     brace is found, and the text before it is examined.
;;;     So, if the braces aren't correctly balanced in the code (because of
;;;     #ifdefs or whatever), it won't work.
;;;
;;; 13. Don't use "(un)signed" to mean "(un)signed int". To see why, look
;;;     at:
;;;
;;;     unsigned foo = 10?
;;;
;;;     Is foo a type or a variable? Either one can be legal, unless ? is a
;;;     semicolon. Only in that case can I determine that foo is a variable.
;;;     The default is to treat foo like a type, which won't always be
;;;     correct.
;;;
;;; 14. Function pointer types such as "foo (*)(int bar)" are not supported.
;;;     Use a typedef, or declare the variable "foo (*fp)(int bar)".
;;;
;;; 15. There are two regexps for special comments. These can be highlighted
;;;     differently from normal comments (font-lock-special-comment-face).
;;;
;;; 16. There is a new face for special keywords
;;;     (font-lock-special-keyword-face).
;;;
;;; Good luck...
(let*
    ((common-keywords
;      ("break" "do" "else" "for" "if" "switch" "while" "friend" "case"
;       "asm" "sizeof" "this" "virtual" "using" "namespace" "new" "delete"
;       "protected" "private" "public" "typedef" "malloc" "free" "template"
;       "typename" "inline" "class" "struct" "enum" "union" "typeid"
;       "static" "extern" "auto" "register" "explicit" "mutable")
      (concat "asm\\|do\\|virtual\\|typeid\\|case\\|"
              "class\\|struct\\|enum\\|union\\|explicit\\|mutable\\|"
              "static\\|extern\\|auto\\|register\\|new\\|delete\\|"
              "using\\|namespace\\|type\\(name\\|def\\)\\|"
              "else\\|for\\|if\\|template\\|friend\\|inline\\|"
              "p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\|malloc\\|free\\|"
              "s\\(izeof\\|witch\\)\\|this\\|while"))
     (special-keywords
      (concat "DEBUG_PRINT\\|DebugIsEnabled\\|DEBUGGING\\|DEBUG\\|"
              "DEBUG_ENTER\\|GET_DEBUG_FLAGS\\|SET_DEBUG_FLAGS\\|"
              "DEBUG_EXIT\\|DEBUG_INIT\\|FLDEBUG\\|ASSERT_ALWAYS\\|"
              "ASSERT\\|assert"))
     (warning-keywords
      (concat "Warn\\|Die\\|Fatal\\|FATAL\\|Warning\\|exit\\|return\\|break"
              "\\|continue\\|throw\\|try\\|catch"))

     ;;
     ;; First define a bunch of building-block regexps
     ;;

     (ws* "[ \t]*")
     (ws+ "[ \t]+")
     (ws+-or-$ (concat "\\(?:" ws+ "\\|$\\)"))
     (template-ext "\\(?:<\\(?:>\\|[^ >\n][^>\n]*>\\| .* >\\)\\)")
     (name-prefix (concat "\\(?:\\<\\sw+" template-ext "?::\\)"))
     (name-suffix (concat "\\<\\sw+" template-ext "?"))
     (name (concat "\\(?:" name-prefix "\\{0,2\\}\\|::\\)" name-suffix))
     (array-ext-no-parens "\\(?:\\[[^,=;\n.-]*\\]\\)")
     (array-ext (concat "\\(" array-ext-no-parens "\\)"))

     (type-name name)
     ;; Doesn't everybody use the same sequence for qualifiers? I hope so.
     (type-quals
      (concat
       "\\(?:\\(?:\\(?:static\\|extern\\|auto\\|register\\|inline\\|friend\\)"
       ws+ "\\)?"
       "\\(?:\\(?:volatile\\|const\\|mutable\\)" ws+ "\\)?"
       "\\(?:\\(?:struct\\|class\\|enum\\|union\\|typename"
       "\\|signed\\|unsigned\\)" ws+ "\\)?\\)"))
     (type-extra
      (concat "\\(?:" ws+
              "\\(?:long\\|int\\|char\\|short\\|double\\)\\>\\)?"))
     ;; This huge expression assumes that types are defined as "foo *var" or
     ;; "foo* var" but NOT "foo * var" or "foo*var". In general, use
     ;; asymmetric spacing for types, symmetric spacing for math exps. Note
     ;; that this regexp expects to end in whitespace or [*&].
     (type
      (concat "\\(" type-quals type-name type-extra "\\(?:"
  "\\(?:[*&]+\\(?:" ws* "\\(?:volatile\\|const\\)\\>\\)?" ws+-or-$ "\\)"
  "\\|"
  "\\(?:" ws+ "[*&]+\\(?:" ws* "\\(?:volatile\\|const\\)\\>" ws+-or-$ "\\)?\\)"
  "\\|\\(?:" ws+-or-$ "\\)\\)\\)"
              ))
     ;; For casts and places where whitespace isn't expected after the type
     ;; (usually because some other non word character is next). Also allows
     ;; an array extension.
     (type-cast
      (concat "\\(?:" type-quals type-name type-extra ws* "[*&]*"
              "\\(?:" ws* "\\(?:volatile\\|const\\)\\>\\)?" ws*
              array-ext-no-parens "?\\)"
              ))

     (var-name
      (concat "\\(" name "\\)"))
     (var-normal
      (concat var-name array-ext "?"))
     (var-fcn-ptr
      (concat "(\\(" name-prefix "\\{0,2\\}[*]\\)" ws*
              var-name ")(\\([^)\n]*\\))"))
     (var-decl
      (concat type "\\(?:" var-normal "\\|" var-fcn-ptr "\\)"))
     (var-multi
      (concat "\\(?:," ws* "\\([*]*\\)" ws* var-name array-ext "?\\)"))

     ;; Need a special suffix because of the destructor '~'.
     (fcn-normal-suffix
      (concat "\\(?:~?\\<\\sw+\\>\\)"))
     (fcn-opr-suffix
      (concat "\\(?:\\<operator\\(?:\\(?:..?" ws* "\\)\\|\\(?:"
              ws+ type-cast "\\(?:\\[\\]\\)?\\)\\)\\)"))
     (fcn-name
      (concat "\\(\\(?:\\(?:" name-prefix "\\{0,2\\}\\)\\|::\\)\\(?:"
              fcn-opr-suffix "\\|" fcn-normal-suffix "\\)\\(?:"
              ws* template-ext "\\)?\\)")))

  ;;
  ;; Helper function that takes the end of a (possible) type match and
  ;; returns the face the type should be highlighted in.
  ;;
  (defsubst compute-type-face (end type fcn-name)
    (save-excursion
      (save-match-data
        (if (ignore-errors (up-list -1) t)
            ;; Better be in a "()" list
            (if (looking-at "(")
                (progn
                  ;; Okay. Find out if we're in an if|while|for header.
                  (skip-chars-backward "^{:\n")
                  (if (search-backward-regexp ")[ \t]*:" (point-at-bol) t)
                      ; false alarm
                      ; X: Why did I put this here? I know there was a good
                      ; reason at some point. Sigh. I hate when I do that.
                      (get-face 'default)
                    (skip-chars-backward "^{;\n")
                    (if (looking-at "^[ \t]+\\(?:if\\>\\|while\\>\\|for\\>\\)")
                        ;; We are. So either what's next is a variable or an
                        ;; assignment. If it's the former, we're a type. If
                        ;; it's the latter, we're already a variable so no
                        ;; color.
                        (progn
                          (goto-char end)
                          (if (looking-at "[ \t]*=")
                              (get-face 'default)
                            font-lock-type-face))
                      ;; We're not. So go up again and see what scope we're in.
                      (if (ignore-errors (up-list -1) t)
                          (progn
                            ;; Find out what the initial keyword for the
                            ;; block is.
                            (search-backward-regexp
                             (concat "^[ \t]*\\(?:\\(class\\|struct"
                                     "\\|namespace\\|extern\\)\\>"
                                     "\\|\\(?:virtual[ \t]+\\)?"
                                     "\\(?:" type "\\)?"
                                     fcn-name "[ \t]*(\\)")
                             (point-min) t)
                            ;; If it's class|struct|namespace|extern, we're
                            ;; a type. Else, we're nada.
                            (if (looking-at
                                 (concat "^[ \t]*\\(class\\|struct"
                                         "\\|namespace\\|extern\\)\\>"))
                                font-lock-type-face
                              (get-face 'default)))
                        ;; We're at the top level already, so assume we're
                        ;; part of a variable decl or something.
                        font-lock-type-face))))
              ;; We're not in a "()" list, but we are in a block of some
              ;; kind. Check for enums.
              (search-backward-regexp "^[ \t]*\\sw+" (point-min) t)
              (if (looking-at "[ \t]*enum\\>")
                  font-lock-variable-name-face
                ;; We could be part of a var-multi, so check that one out.
                (goto-char end)
                (if (and (looking-at "[,;]")
                         (looking-at "[^\n>]*;"))
                    font-lock-variable-name-face
                  (get-face 'default))))
          ;; Top level, so we are nothing special.
          (get-face 'default)))))

  ;; Now to set the keywords proper
 (setq c-font-lock-keywords-3
  (list
   ;;
   ;; Old Stuff
   ;;

   ;; Fontify filenames in #include <...> preprocessor directives as
   ;; strings. --- NOTE: This is a little different from the original ---
   '("^#[ \t]*include[ \t]+<\\([^>\"\n]+\\)>" 1 font-lock-string-face)
   ;;
   ;; Fontify function macro names.
   '("^#[ \T]*define[ \t]+\\(\\(\\sw+\\)(\\)" 2 font-lock-function-name-face)
   ;;
   ;; Fontify symbol names in #if ... defined preprocessor directives.
   '("^#[ \t]*if\\>"
     ("\\<\\(defined\\)\\>[ \t]*(\\(\\sw+\\)?" nil nil
      (1 font-lock-preprocessor-face) (2 font-lock-reference-face nil t)))
   ;;
   ;; Fontify #ifndef as symbol names, and the preprocessor directive names.
   '("^\\(#[ \t]*ifndef\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-preprocessor-face) (2 font-lock-reference-face nil t))
   ;;
   ;; Fontify #ifdef as symbol names, and the preprocessor directive names.
   '("^\\(#[ \t]*ifdef\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-preprocessor-face) (2 font-lock-reference-face nil t))
   ;;
   ;; Fontify #if|elif as symbol names, and the rest of the line.
   (list "^\\(#[ \t]*\\(?:if\\|elif\\)\\)\\>[ \t]*\\(.*\\)?"
         '(1 font-lock-preprocessor-face)
         '(2 font-lock-reference-face keep t))
   '("^\\(#[ \t]*\\(?:define\\|undef\\)\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-preprocessor-face) (2 font-lock-variable-name-face nil t))
   ;;
   ;; Fontify otherwise as symbol names, and the preprocessor directive names.
   '("^\\(#[ \t]*[a-z]+\\)\\>[ \t]*\\(.*\\)?"
     (1 font-lock-preprocessor-face) (2 font-lock-special-keyword-face nil t))
   ;;
   ;; These next two are different from the old keywords, but still pretty
   ;; basic.
   ;;
   ;; Special comments
   '("\\(--- .*? ---\\)" 1 font-lock-special-comment-face t)
   (list (concat "\\(XXX:\\(?:" ws* "<\\(" name "\\)>\\)?.*\\)")
         '(1 font-lock-special-comment-face t)
         '(2 font-lock-doc-string-face t t))
   ;;
   ;; Keywords
   (cons (concat "\\<\\(" common-keywords "\\)\\>") 'font-lock-keyword-face)
   (cons (concat "\\<\\(" special-keywords "\\)\\>")
         'font-lock-special-keyword-face)
   (cons (concat "\\<\\(" warning-keywords "\\)\\>")
         'font-lock-warning-face)
   ;;
   ;; Fontify case/goto keywords and targets, and case default/goto tags.
   ;; These have to go after fontifying the keywords or "public:", etc. will
   ;; get tagged.
   '("\\<\\(case\\|goto\\)\\>[ \t]*\\([^ \t\n:;]+\\)?"
     (1 font-lock-warning-face keep) (2 font-lock-reference-face nil t))
   '("^[ \t]*\\(\\sw+\\)[ \t]*:[^:]" 1 font-lock-reference-face)

   ;;
   ;; New Stuff
   ;;

   ;; First, types.
   ;;
   ;; New
   ;;
   ;; NOTE: There are a lot of "keep"s around here. They are needed because
   ;; keywords like typename are already colored as such, but it can also be
   ;; a valid type-qual.
   (list (concat "\\<new" ws+ "\\(?:(\\(" ws* name ws* "\\))" ws* "\\)?\\("
                 type-cast "\\)")
         '(1 font-lock-reference-face nil t)
         '(2 font-lock-type-face keep))
   ;;
   ;; Catch bare "::operator" words (for global new|delete)
   (list (concat "\\W\\(::operator\\)" ws+) 1 font-lock-keyword-face)
   ;;
   ;; Sizeof|typeid
   (list (concat "\\(?:sizeof\\|typeid\\)(\\(" type-cast "\\))")
         1 font-lock-type-face 'keep)
   ;;
   ;; Typedefs
   (list (concat "^" ws* "\\<typedef" ws+ "\\(?:"
                 "\\(\\(?:\\(?:const\\|volatile\\)" ws+ "\\)?"
                 "\\(?:\\(?:typename\\)" ws+ "\\)?"
                 "\\(?:struct\\|class\\|enum\\|union\\)" ws+ "\\){"
                 "\\|\\(?:" var-decl "\\(" ws* "{\\)?\\)\\)")
         '(1 font-lock-type-face keep t)
         '(2 font-lock-type-face keep t)
         ;; If a { is on the line, then assume we're starting a multi-line
         ;; struct|class|union|enum typedef, so what would be the variable
         ;; name is really just the type.
         '(3 (if (match-beginning 8) font-lock-type-face
               font-lock-variable-name-face) nil t)
         '(4 font-lock-type-face nil t)
         '(5 font-lock-type-face nil t)
         '(6 (if (match-beginning 8) font-lock-type-face
               font-lock-variable-name-face) nil t)
         '(7 font-lock-type-face nil t))
   ;;
   ;; Struct/Class/Union/Enum declarations (beginning)
   (list (concat "\\<\\(?:struct\\|class\\|enum\\|union\\)" ws+
                 "\\(" name "\\)?" ws* "\\([{;:,=>]\\|$\\)")
         '(1 (if (string-match "[,=>]" (match-string 2))
                 (if (ignore-errors (up-list -1) t)
                     (if (looking-at "(")
                         font-lock-type-face
                       font-lock-doc-string-face)
                   font-lock-doc-string-face)
               (save-excursion
                 (save-match-data
                   (if (search-backward-regexp "\\<friend\\>"
                                               (point-at-bol) t)
                       font-lock-type-face
                     font-lock-reference-face)))) nil t))
   ;;
   ;; Assume that close brace with a name after it is the end of a
   ;; struct|class|enum decl.
   (list (concat "}" ws* "\\([*]*\\)" ws* var-name array-ext "?" ws* "[:;]")
         '(1 font-lock-type-face nil t)
;        '(2 font-lock-variable-name-face)
         '(2 (save-excursion
               (save-match-data
                 ; Check if the block we just left starts with a typedef. If
                 ; it does, then the var-name is really a type.
                 (goto-char (match-beginning 0))
                 (if (ignore-errors (up-list -1) t)
                     (progn
                       (skip-chars-backward " \t\n")
                       (beginning-of-line)
                       (if (looking-at "^[ \t]*typedef\\>")
                           font-lock-reference-face
                         font-lock-variable-name-face))
                   ;; This should never happen, but I don't want to bomb out
                   ;; even if it does. So make a guess...
                   font-lock-reference-face))))
         '(3 font-lock-type-face nil t))
   ;;
   ;; Parent decls
   (list (concat "\\<\\(?:virtual" ws+ "\\)?"
                 "\\(?:public\\|protected\\|private\\)" ws+ "\\(" name "\\)")
         1 font-lock-type-face 'keep)

   ;; Now move onto function stuff.
   ;;
   ;; Type by itself
   (list (concat "^" ws* type ws* "$") 1 font-lock-type-face 'keep)
   ;;
   ;; Function declaration (beginning)
   ;; The function name overrides because of "operator new" and
   ;; "operator delete", as well as the operator casts.
   (list (concat "^\\(" ws+ "\\)?\\(?:virtual" ws+ "\\)?" "\\(?:" type "\\)?"
                 fcn-name ws* "(\\($\\|)\\|\\(" type-cast
                 "\\|\\.\\.\\.\\)[,=)/]\\|\\(" var-decl "\\)\\)")
         ;; If you define a function like "operator int()", the word
         ;; "operator" will match as a type but it should be a function.
         '(2 (save-match-data
               (if (and (match-string 2)
                        (string-match "^operator\\>" (match-string 2)))
                   font-lock-function-name-face
                 font-lock-type-face)) keep t)
         ;; Boy, functions are a pain. Many things can masquerade as
         ;; functions. The first if statement gets the current context
         ;; surrounding the function name. If we're in a string or comment,
         ;; use those faces. The next if statement checks if the decl is
         ;; flushed. If it is, assume it's a function. The next if handles
         ;; statements like:
         ;;
         ;;     foo bar(new baz);
         ;;
         ;; Which is really a variable definition. The next if statement
         ;; allows nested extern function decls. The next if statement makes
         ;; sure we don't accidentally turn for|while|if into a function.
         ;; The next two if statements handle special-keywords and
         ;; catch|return separately, because they use different faces. The
         ;; next three if statements ensure that the only nested function
         ;; decls (except for externs, of course) are within a class,
         ;; struct, namespace, or extern "C" scope, *or* have an argument
         ;; (in which case it's a variable decl in disguise). The final if
         ;; statement handles cases like:
         ;;
         ;;     type|return object_with_constructor|function_call (arg);
         ;;
         ;; If it's a return, blank it, because it could be a function call.
         ;; If it's a type, then we're defining a variable. Whew. I hope
         ;; that's all.
         ;;
         ;; Also, squirrel away the chosen face, because the next real match
         ;; can use it.
         `(3 (save-excursion
               (save-match-data
                 (let ((context (save-match-data (buffer-syntactic-context))))
                   (if (not context)
                       (if (not (match-string 1))
                           (setq the-face font-lock-function-name-face)
                         (if (and (match-string 7)
                                  (string-match
                                   "\\<new\\>\\|\\<delete\\>"
                                   (match-string 7)))
                             (setq the-face font-lock-variable-name-face)
                           (if (and (match-string 2)
                                    (string-match "\\<extern\\>"
                                                  (match-string 2)))
                               (setq the-face font-lock-function-name-face)
                             (if (string-match
                                  "^\\<\\(?:for\\|while\\|if\\)\\>"
                                  (match-string 3))
                                 (setq the-face font-lock-keyword-face)
                               (if (string-match
                                    (concat "\\<\\(?:"
                                            ,special-keywords "\\)\\>")
                                    (match-string 3))
                                   (setq the-face
                                         font-lock-special-keyword-face)
                                 (if (string-match
                                      "^\\<\\(?:catch\\|return\\)\\>"
                                      (match-string 3))
                                     (setq the-face font-lock-warning-face)
                                   (goto-char (match-beginning 3))
                                   (if (ignore-errors (up-list -1) t)
                                       (progn
                                         (save-match-data
                                           (search-backward-regexp
                                            (concat
                                             "^[ \t]*\\(?:\\(class\\|struct"
                                             "\\|namespace\\|extern\\)\\>"
                                             "\\|\\(?:virtual[ \t]+\\)?"
                                             "\\(?:" ,type "\\)?"
                                             ,fcn-name "[ \t]*(\\)")
                                            (point-min) t))
                                         (if (looking-at
                                              (concat
                                               "^[ \t]*\\(class\\|struct\\|"
                                               "namespace\\|extern\\)\\>"))
                                             (setq the-face
                                           font-lock-function-name-face)
                                           (if (and (match-string 2)
                                                    (not (string-match
                                                          "^return"
                                                          (match-string 2))))
                                               (setq the-face
                                             font-lock-variable-name-face)
                                             (setq the-face
                                                   (get-face 'default))))
                                         )
                                     (setq the-face
                                           font-lock-function-name-face))))))))
                     (case context
                       (string        font-lock-string-face)
                       (comment       font-lock-comment-face)
                       (block-comment font-lock-comment-face))
                     )))) t)
         ;;
         ;; Okay. If we decided this thing is a function (or a return
         ;; statement that looks like this:
         ;;
         ;;    return (cast) ret_val;
         ;;
         ;; then color this thing like a type. Otherwise blank it.
         '(5 (if (eq the-face font-lock-function-name-face)
                 font-lock-type-face
               (save-match-data
                 ; Highlight all catch types appropriately.
                 (if (and (eq the-face font-lock-warning-face)
                          (string-match "\\<catch\\>" (match-string 3))
                          (match-string 5))
                     (save-excursion
                       (save-match-data
                         (goto-char (match-end 5))
                         (if (looking-at ")")
                             font-lock-type-face
                           (get-face 'default))))
                   (get-face 'default)))) keep t)
         '(7 (save-match-data
               (if (and (match-string 7)
                        (string-match "\\<new\\>\\|\\<delete\\>"
                                      (match-string 7)))
                   font-lock-keyword-face
                 font-lock-type-face)) keep t)
         '(8 (save-match-data
               (if (and (match-string 7)
                        (string-match "\\<delete\\>" (match-string 7)))
                   (get-face 'default)
                 font-lock-variable-name-face)) nil t)
         '(9 font-lock-type-face nil t)
         '(10 font-lock-type-face nil t)
         '(11 font-lock-variable-name-face nil t)
         '(12 font-lock-type-face nil t)
         ;;
         ;; Use an anchored match to find any type-only args on the command
         ;; line. Start after the first argument and go for it. [ This
         ;; anchored stuff is great ]
         (list
          (concat "," ws* "\\(" type-cast "\\)")
          '(goto-char (or (match-beginning 4)
                          (match-beginning 5)
                          (match-beginning 6)))
          nil
          `(1 (compute-type-face (match-end 1) ,type ,fcn-name) keep)))
   ;;
   ;; Check for const after argument list
   (list (concat ")" ws* "\\(const\\>\\)") ;ws* "\\(?:[;:{,)]\\|$\\)")
         1 font-lock-type-face)
   ;;
   ;; Casts
   ;; No whitespace between the type and the outer parens. This must be done
   ;; after function decls or something like "operator<<(foo bar)" will get
   ;; "(foo bar)" tagged as a cast.
   (list (concat "[^a-zA-Z0-9_](\\(" type-cast "\\))" ws* "[(_a-zA-Z0-9&]")
         '(1 font-lock-type-face keep))
   ;;
   ;; The four funky c++ casts + auto_ptr
   (list
    (concat "\\<\\(\\(?:\\(?:const\\|dynamic\\|reinterpret\\|static\\)"
            "_cast<\\)\\|auto_ptr<\\)\\(" ws* type-cast ws* "\\)\\(>\\)")
    '(1 font-lock-keyword-face) '(2 font-lock-type-face keep)
    '(3 font-lock-keyword-face))
   ;;
   ;; Friend decls
   (list (concat "^" ws* "friend" ws+ type)
         1 font-lock-type-face)
   ;;
   ;; Namespace decls
   (list (concat "^" ws* "\\(?:using" ws+ "\\)?namespace" ws+
                 "\\(" name "\\)")
         1 font-lock-reference-face)
   ;;
   ;; Type-only args.
   ;; Check if the line starts with a valid type-like arg, color it, and
   ;; then search for others on the same line.
   (list (concat "^" ws+ "\\(" type-cast "\\)" ws* "\\([,=)/]\\)")
         `(1 (save-excursion
               (save-match-data
                 (goto-char (match-beginning 1))
                 (compute-type-face (match-end 1) ,type ,fcn-name))) keep)
         (list
          (concat "," ws* "\\(" type-cast "\\)")
          '(goto-char (match-beginning 2)) nil
          `(1 (compute-type-face (match-end 1) ,type ,fcn-name) keep)))
   ;;
   ;; Do the same thing if the line starts with a var-decl. Don't color the
   ;; decl, though; we'll do that below.
   (list (concat "^" ws+ var-decl ws* "\\([,)=]\\)")
         (list
          (concat "," ws* "\\(" type-cast "\\)")
          '(goto-char (match-beginning 7)) nil
          `(1 (compute-type-face (match-end 1) ,type ,fcn-name) keep)))
   ;;
   ;; Or an enum
   (list (concat "^" ws* "enum\\>[^{]*\\({\\)" ws*)
         (list
          (concat "[{,]" ws* "\\(" type-cast "\\)")
          '(goto-char (match-beginning 1)) nil
          `(1 (compute-type-face (match-end 1) ,type ,fcn-name) keep)))
   ;;
   ;; Variable declarations (cont'd)
   ;; This has to come before the intial var dec, or lines like:
   ;;           foo bar, foo bar
   ;; get "foo bar, foo" tagged as a var-decl (initial), because of
   ;; var-multi.
   (list (concat "," ws* var-decl)
         '(1 font-lock-type-face keep)
         ;; delete|return|typename statements can masquerade as var decls,
         ;; so watch out. If we are in one, don't highlight it. For asm
         ;; statements, format it as a type.
         '(2 (save-match-data
               (if (string-match
                    "^delete\\>\\|^return\\>\\|typename[ \t]*$"
                    (match-string 1))
                   (get-face 'default)
                 (if (string-match "^asm\\>" (match-string 1))
                     font-lock-type-face
                   font-lock-variable-name-face))) nil t)
         '(3 font-lock-type-face nil t)
         '(4 font-lock-type-face nil t)
         '(5 font-lock-variable-name-face nil t)
         '(6 font-lock-type-face nil t))
   ;;
   ;; Variable declarations (initial)
   ;; Only two extra variables are allowed to tag along ("int i, j, k").
   (list (concat "^" ws* "\\(?:template" ws* "<\\)?" var-decl
                 var-multi "?" var-multi "?")
         ;; delete|return|typename statements can masquerade as var decls,
         ;; so watch out. If we are in one, don't highlight it. For asm
         ;; statements, format it as a type.
         '(1 (save-match-data
               (if (and (match-string 1)
                        (string-match
                         "^\\(?:delete\\>\\|return\\>\\|typename[ \t]*$\\)"
                         (match-string 1)))
                   (get-face 'default)
                 font-lock-type-face)) keep t)
         '(2 (save-match-data
               (let ((ms1 (match-string 1)))
                 (if (string-match
                      (concat "^\\(?:delete\\>\\|return\\>"
                              "\\|throw\\>\\|else\\>\\|inline\\>"
                              "\\|typedef\\>\\|typename[ \t]*$\\)") ms1)
                     (if (string-match "\\<throw\\>" ms1)
                         font-lock-reference-face
                       (get-face 'default))
                   (if (string-match "^asm\\>" ms1)
                       font-lock-type-face
                     font-lock-variable-name-face)))) nil t)
         '(3 (save-match-data
               (if (and (match-string 1)
                        (string-match
                         "^\\(?:delete\\>\\|return\\>\\|typename[ \t]*$\\)"
                         (match-string 1)))
                   (get-face 'default)
                 font-lock-type-face)) nil t)
         '(4 font-lock-type-face nil t)
         '(5 font-lock-variable-name-face nil t)
         '(6 font-lock-type-face nil t)
         '(7 font-lock-type-face nil t)
         '(8 font-lock-variable-name-face nil t)
         '(9 font-lock-type-face nil t)
         '(10 font-lock-type-face nil t)
         '(11 font-lock-variable-name-face nil t)
         '(12 font-lock-type-face nil t))
   ;; Miscellaneous
   ;;
   ;; Catch extern "?" decls
   (list (concat "^" ws* "\\(extern\\)" ws* "\"") 1 font-lock-keyword-face)
   ;;
   ;; Tag "..." as a type (to match fcn-decl-start)
   '("\\.\\.\\." . font-lock-type-face)
   ;;
   ;; Catch the "[]" at the end of delete[]
   '("\\<delete\\(\\[\\]\\)" 1 font-lock-keyword-face)))
 ;;
 ;; I don't know why I have to set *-3, but it wouldn't work otherwise.
 (setq c++-font-lock-keywords-3 c-font-lock-keywords-3)
)
(setq c-font-lock-keywords c-font-lock-keywords-3)
(setq c++-font-lock-keywords c++-font-lock-keywords-3)

(provide 'c-font-lock-keywords)
