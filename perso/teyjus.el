;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Teyjus Lambda Prolog mode to go with COMINT package by Kamal Aboul-Hosn
;;;
;;; This package handles the following features:
;;;   - Syntax highlighting for .mod and .sig files
;;;   - Running an inferior Teyjus process within Emacs
;;;
;;; Commands have been added to make easier the use of Teyjus:
;;;
;;;  Command                Shortcut     Function
;;;  ----------             --------     --------
;;;  M-x teyjus                          Starts teyjus in Emacs
;;;  M-x teyjus-path                     Manually add a search path
;;;  M-x teyjus-compile     C-c c        Compile a module
;;;  M-x teyjus-query       C-c q        Load and query a specific module
;;;  M-x teyjus-query-top                Query Teyjus at the top level
;;;  M-x teyjus-run         C-c r        Compile, load, and query the current buffer's module
;;;  M-x teyjus-next-error  C-c n        Find the next compilation error in the source file
;;;  M-x teyjus-prev-error  C-c p        Find the previous compilation error in the source file
;;;  M-x teyjus-goal-file                Query a module with the goals in the specified file
;;;
;;; These commands are also available via the Teyjus menu
;;;
;;; Problems/Things to be fixed:
;;;   - teyjus-goal-file does not print the goals in the Teyjus buffer
;;;   - teyjus-next-error/teyjus-prev-error can have odd behavior in certain cases
;;;   - Tendency to access unmapped memory and bail out
;;;
;;;
;;;
;;; To use the Teyjus Emacs interface, add these lines to your .emacs file:
;;; (setq auto-mode-alist (cons '("\\.mod\\'" . teyjus-edit-mode) auto-mode-alist))
;;; (setq auto-mode-alist (cons '("\\.sig\\'" . teyjus-edit-mode) auto-mode-alist))
;;; (setq explicit-lprolog-file-name "TEYJUS EXECUTABLE")
;;; (autoload 'teyjus "teyjus"  "Run an inferior Teyjus process." t)
;;; (autoload 'teyjus-edit-mode "teyjus"  "Syntax Highlighting, etc. for Lambda Prolog" t)
;;;
;;; where "TEYJUS EXECUTABLE" is the command to run teyjus
;;;
;;; Make sure that you have the comint package installed as well.
;;;
;;;
;;; Any comments, questions, bugs, or suggestions can be directed to Kamal at kxa153@psu.edu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'easymenu)
(require 'comint)
(provide 'teyjus)
(provide 'teyjus-mode)

(setq auto-mode-alist (cons '("\\.mod\\'" . teyjus-edit-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sig\\'" . teyjus-edit-mode) auto-mode-alist))

(defvar lprolog-prompt-pattern "^| \\?\\- ")
(defvar explicit-lprolog-file-name nil)
(defvar teyjus-mode-map nil)
(defvar teyjus-edit-mode-map nil)

(defun full-copy-sparse-keymap (km)
  "Recursively copy the sparse keymap KM"
  (cond ((consp km)
	 (cons (full-copy-sparse-keymap (car km))
	       (full-copy-sparse-keymap (cdr km))))
	(t km)))

(cond ((not teyjus-mode-map)
       (setq teyjus-mode-map (full-copy-sparse-keymap comint-mode-map))
       (define-key teyjus-mode-map "\C-cc" 'teyjus-compile)
       (define-key teyjus-mode-map "\C-cq" 'teyjus-query)
       (define-key teyjus-mode-map "\C-cp" 'teyjus-prev-error)
       (define-key teyjus-mode-map "\C-cn" 'teyjus-next-error)
))

(cond ((not teyjus-edit-mode-map)
       (setq teyjus-edit-mode-map (full-copy-sparse-keymap text-mode-map))
       (define-key teyjus-edit-mode-map "\C-cr" 'teyjus-run)
       (define-key teyjus-edit-mode-map "\C-cc" 'teyjus-compile)
       (define-key teyjus-edit-mode-map "\C-cq" 'teyjus-query)
       (define-key teyjus-edit-mode-map "\C-cp" 'teyjus-prev-error)
       (define-key teyjus-edit-mode-map "\C-cn" 'teyjus-next-error)))

(defvar teyjus-mode-hook '()
  "*Hook for customizing teyjus mode")

;; Define the menu for teyjus
(defvar teyjus-menu
  '("Teyjus"

    ["Start Teyjus" teyjus t]
    ["Add search path" teyjus-path t]
    ["----" nil t]
    ["Compile file"       teyjus-compile  t]
    ["Query file"                  teyjus-query t]
    ["Query toplevel"                  teyjus-query-top t]
    ["Compile and query buffer"    teyjus-run t]
    ["------" nil t]
    ["Goto next error"             teyjus-next-error   t]
    ["Goto previous error"          teyjus-prev-error  t]
    ["-----" nil t]
    ["Query goals in file"            teyjus-goal-file t]))




(defun teyjus-mode ()
  "Major mode for interacting with an inferior teyjus process.
Return after the end of the process' output sends the text from the 
    end of process to the end of the current line.
Return before end of process output copies rest of line to end (skipping
    the prompt) and sends it.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."

  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (setq comint-prompt-regexp lprolog-prompt-pattern)
  (setq major-mode 'teyjus-mode)
  (setq mode-name "Teyjus")
  (easy-menu-define teyjus-mode-menu-symbol
		    teyjus-mode-map "Teyjus menu" teyjus-menu)
  (use-local-map teyjus-mode-map)
  (set-syntax-table teyjus-mode-syntax-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(iteyjus-font-lock-keywords t nil))
  (easy-menu-add teyjus-menu)
  (turn-on-font-lock-if-enabled)
  (run-hooks 'teyjus-mode-hook)
)


(defun teyjus-edit-mode ()
  "Mode for editing Lambda Prolog Files"
  (interactive)
  (kill-all-local-variables)
  (setq default-case-fold-search nil)
  (setq imenu-case-fold-search nil)
  (setq font-lock-keywords-case-fold-search nil)
  (setq major-mode 'teyjus-edit-mode)
  (setq mode-name "Teyjus")
  (easy-menu-define teyjus-edit-mode-menu-symbol
		    teyjus-edit-mode-map "Teyjus menu" teyjus-menu)
  (use-local-map teyjus-edit-mode-map)
  (set-syntax-table teyjus-mode-syntax-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(teyjus-font-lock-keywords nil nil))
  (turn-on-font-lock)
  (easy-menu-add teyjus-menu)
  (run-hooks 'teyjus-mode-hook)
)

(defun teyjus ()
  "Run an inferior Prolog, with I/O through buffer *teyjus*.
If buffer exists but prolog process is not running, make new prolog.
If buffer exists and prolog process is running, just switch to buffer *teyjus*.
Program used comes from variable explicit-prolog-file-name,
 or (if that is nil) from the PROLOG environment variable.
If a file ~/.emacs_prolog exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in teyjus-mode, giving commands for sending input
and controlling the subjobs of the prolog.  See teyjus-mode.
See also variable lprolog-prompt-pattern.

\(Type \\[describe-mode] in the prolog buffer for a list of commands.)"
  (interactive)
  (cond ((not (comint-check-proc "*teyjus*"))
	 (let* ((prog (or explicit-lprolog-file-name
			  (getenv "PROLOG")
			  "prolog"))
		(name (file-name-nondirectory prog))
		(startfile (concat "~/.emacs_" name)))
	   (set-buffer (apply 'make-comint "teyjus" prog
			      (if (file-exists-p startfile) startfile)))
	   (teyjus-mode)

)))
  (switch-to-buffer "*teyjus*"))

;; Variables for error lines
(defvar linenumber 0)
(defvar filename nil)
(defvar teyjus-last-error 1)
(defvar teyjus-top-error 1)

;; Regular Expression for errors/Warnings
(defvar teyjus-error-regexp
  "^\\(.+\\):\\([0-9]+\\)\\.\\([0-9]+\\).+\\(Error\\|Warning\\|parse error\\)")


;; Function to find the next error in the *teyjus* buffer,
;; split the window, and goto the line in the program with
;; the error.
(defun teyjus-next-error ()
  (interactive)
  (set-buffer "*teyjus*")
  (let ((found t))
  (condition-case ()
	(progn
	  ;; Goto the previous error
	  (goto-char teyjus-last-error)
	  (end-of-line)
	  ;; Find the next error in *teyjus*
	  (re-search-forward teyjus-error-regexp))
      (error (setq found nil)))
  (setq teyjus-last-error (point))
  ;; If there was another error, find it in the file
    (if found
	(progn
	  (delete-other-windows)
	   (setq filename (buffer-substring (match-beginning 1) (match-end 1)))
	  (setq linenumber (string-to-int (buffer-substring (match-beginning 2) (match-end 2))))
	  (switch-to-buffer (find-file-noselect filename))
	  (switch-to-buffer "*teyjus*")
	  (split-window)
	  (set-buffer (switch-to-buffer (find-file-noselect filename)))
	  (goto-line linenumber))
      (message "No more errors!"))))


;; Function to find the previous error in the *teyjus* buffer,
;; split the window, and goto the line in the program with
;; the error.
(defun teyjus-prev-error ()
  (interactive)
  (set-buffer "*teyjus*")
  (let ((found t))
  (condition-case ()
      (progn
	;; Goto the previous error
	  (goto-char teyjus-last-error)
	  (beginning-of-line)
	  (re-search-backward teyjus-error-regexp))
      (error (setq found nil)))
  (setq teyjus-last-error (point))
  ;; If there was another error, find it in the file
    (if (and found (> (point) teyjus-top-error))
	(progn
	  (delete-other-windows)
	  (setq filename (buffer-substring (match-beginning 1) (match-end 1)))
	  (setq linenumber (string-to-int (buffer-substring (match-beginning 2) (match-end 2))))
	  (switch-to-buffer (find-file-noselect filename))
	  (goto-line linenumber)
	  (switch-to-buffer "*teyjus*")
	  (split-window)
	  (switch-to-buffer (find-file-noselect filename)))
      (message "No more errors!"))))

(defvar filepath nil)

;; Function to compile a \Prolog Module into Teyjus
(defun teyjus-compile (string)
  "Compile a module with the inferior shell running teyjus"
  (interactive "FCompile module: ")
  (teyjus)
  ;; We could be in an query state, so send the stop command just in case.
  (send-string "*teyjus*" "stop.\n")
  (goto-char (point-max))
  ;; Reset error points
  (setq teyjus-last-error (point))
  (setq teyjus-top-error (point))
  (setq string (expand-file-name string))
  (setq filepath (file-name-directory string))
  (save-some-buffers)  
  ;; Add the directory of the file to Teyjus' search path
  (send-string "*teyjus*"
	       (concat "#search " filepath
		       ".\n"))

  ;; Compile the bloody thing
  (send-string "*teyjus*"
	       (concat "#compile " string
		       ".\n")))

(defun teyjus-query-top nil
  (interactive)
  (teyjus)
  (send-string "*teyjus*" "stop.\n")
  (send-string "*teyjus*" "#query.\n"))
  


;; Function to query a module.  It loads the module,
;; then queries it.  The search path for the module must already be in the system.
(defun teyjus-query (file)
  "Query a module with the inferior shell running teyjus"
  (interactive "sModule name: ")
  (teyjus)
  ;; We could be in an query state, so send the stop command just in case.
  (send-string "*teyjus*" "stop.\n")
  (goto-char (point-max))
  ;; Reset error points
  (setq teyjus-last-error (point))
  (setq teyjus-top-error (point))
  ;; Load directory of file into search path
  (send-string "*teyjus*"
	       (concat "#load " file
		       ".\n"))
  ;; Query the module
  (send-string "*teyjus*"
	       (concat "#query " file
		       ".\n")))

;; Function to add a directory to the search path
(defun teyjus-path (file)
  "Add a search page to teyjus"
  (interactive "FDirectory: ")
  (teyjus)
  (send-string "*teyjus*" "stop.\n")
  (setq file (expand-file-name file))
  (send-string "*teyjus*" (concat "#search " file ".\n")))

(defvar file nil)
(defvar filepat nil)
(defvar module nil)

;; Function to compile, load, and query the current buffer in Teyjus
(defun teyjus-run ()
  "Compile and query the current buffer in Teyjus"
  (interactive)
  (setq file (buffer-file-name (current-buffer)))
  ;; Get the file information
  (setq filepat (file-name-directory file))
  (setq module (file-name-nondirectory file))
  (setq module (substring module 0 -4))
  (teyjus)
  (goto-char (point-max))
  ;; Reset error points
  (setq teyjus-last-error (point))
  (setq teyjus-top-error (point))
  (save-some-buffers)
  ;; We could be in an query state, so send the stop command just in case.
  (send-string "*teyjus*" "stop.\n")
  ;; Add the directory of the of current buffer's file to the search path
  (send-string "*teyjus*"
	       (concat "#search " filepat
		       ".\n"))
  ;; Compile the module
  (send-string "*teyjus*"
	       (concat "#compile " file
	       ".\n"))
  ;; Load the module
  (send-string "*teyjus*"
	       (concat "#load " module
		       ".\n"))
  ;; Query the module
  (send-string "*teyjus*"
	       (concat "#query " module
		       ".\n")))

(defun teyjus-goal-file (module file)
  "Query a module with specific goals from a file the inferior shell running Teyjus"
  (interactive "sModule name: \nFGoals file:")
  (teyjus)
  (send-string "*teyjus*" "stop.\n")
  (save-some-buffers)
  (goto-char (point-max))
  (setq teyjus-last-error (point))
  (setq teyjus-top-error (point))
  ;; Load the module
  (send-string "*teyjus*"
	       (concat "#load " module
		       ".\n"))
  (send-string "*teyjus*"
	       (concat "#query " module
		       ".\n"))
  (setq string (expand-file-name file))
  (find-file file)
  (switch-to-buffer "*teyjus*")
  ;; Open and access the files of goals
  (set-buffer (file-name-nondirectory file))
  (defvar currentpoint 1)
  (defvar linebeg 1)
  (defvar lineend 1)
  (defvar mygoal nil)
  (goto-char currentpoint)
  ;; Continue to query goals as long as they exist
  (while (not (eobp))
    (progn

      (beginning-of-line)
      (setq currentpoint (point))
      (setq linebeg (point))
      (end-of-line)
      (setq lineend (point))
      (setq mygoal (buffer-substring linebeg lineend))
      (save-excursion
      (set-buffer "*teyjus*")
      ;; Send the goal to teyjus
      (insert mygoal)
      (send-string "*teyjus*" (concat mygoal "\n"))
      ;; Wait for the output
      (accept-process-output)
      (goto-char (point-max))
      (backward-char)

      ;; Determine if the goal succeeded or not, if so we have to send an "n"
      ;; because we only want one answer
      (if (char-equal ?\? (preceding-char))
	  (send-string "*teyjus*" "n\n")))
      (set-buffer (file-name-nondirectory file))
      (goto-char currentpoint)
      (forward-line))))


(defvar teyjus-mode-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?/ "w" table)
    (modify-syntax-entry ?* "w" table)
    (modify-syntax-entry ?+ "w" table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?= "w" table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?< "w" table)
    (modify-syntax-entry ?> "w" table)
    (modify-syntax-entry ?\' "\"" table)
    (setq teyjus-mode-syntax-table table)
    table))

(defvar iteyjus-font-lock-keywords
  '(("#\\(search\\|quit\\|query\\|help\\|load\\|compile\\)\\(\.\\| \\)" . font-lock-builtins-face)
    ("Teyjus>" . font-lock-header-face)
    (".+\\?\\-" . font-lock-definitions-face)
    ("\\?\\-" . (0 font-lock-header-face t))))

(defvar teyjus-font-lock-keywords
  ;; Logical and mathematical connectives
  '(("\\( +\\)\\(->\\|:-\\|=>\\|=\\|::\\|\+\\|\*\\|&\\|^\\|\\|<\\|>\\|>=\\|=<\\|-\\)[,. ]" . (2 font-lock-connectors-face keep t))
    ;; Cut
    ("!" . font-lock-builtins-face)
    ;; Headers
    ("\\(^\\| \\)\\(module\\|sig\\) " . font-lock-moduledefs-face)
    ;; Variable
    ("\\([^a-z_]\\)\\([A-Z][^]) \.|,\n\t]*\\)" . (2 font-lock-variable-face keep t))
    ;; Builtin keywords next to each other
    ("\\([^a-zA-Z\"_]\\)\\(abs\\|sqrt\\|sin\\|cos\\|arctan\\|ln\\|floor\||ceil\\|truncate\\|rabs\\|size\\|chr\\|string_to_int\\|substring\\|int_to_string\\|real_to_string\\|std_in\\|std_out\\|std_err\\|is\\|open_in\\|open_out\\|open_string\\|open_append\\|close_in\\|close_out\\|term_to_string\\|string_to_term\\|input\\|output\\|input_line\\|lookahead\\|eof\\|flush\\|print\\|read\\|printterm\\|readterm\\|nil\\|not\\|true\\|fail\\|pi\\|sigma\\)\\( \\)\\(abs\\|sqrt\\|sin\\|cos\\|arctan\\|ln\\|floor\||ceil\\|truncate\\|rabs\\|size\\|chr\\|string_to_int\\|substring\\|int_to_string\\|real_to_string\\|std_in\\|std_out\\|std_err\\|is\\|open_in\\|open_out\\|open_string\\|open_append\\|close_in\\|close_out\\|term_to_string\\|string_to_term\\|input\\|output\\|input_line\\|lookahead\\|eof\\|flush\\|print\\|read\\|printterm\\|readterm\\|nil\\|not\\|true\\|fail\\|pi\\|sigma\\)\\([])( ^,.;[]\\)" . (4 font-lock-builtins-face keep t))
    ;; Single builtin keywords
    ("\\([^a-zA-Z\"_]\\)\\(abs\\|sqrt\\|sin\\|cos\\|arctan\\|ln\\|floor\||ceil\\|truncate\\|rabs\\|size\\|chr\\|string_to_int\\|substring\\|int_to_string\\|real_to_string\\|std_in\\|std_out\\|std_err\\|is\\|open_in\\|open_out\\|open_string\\|open_append\\|close_in\\|close_out\\|term_to_string\\|string_to_term\\|input\\|output\\|input_line\\|lookahead\\|eof\\|flush\\|print\\|read\\|printterm\\|readterm\\|nil\\|not\\|true\\|fail\\|pi\\|sigma\\)\\([])( ^,.;[]\\)" . (2 font-lock-builtins-face keep t))
    ;; Preamble statements
    ("\\(^\\| \\)\\(accumulate\\|import\\|accum_sig\\)\\( \\)" . (2 font-lock-header-face keep t))
    ;; Type/Kind declarations
    ("\\(^\\| \\)\\(type\\|kind\\|infix\\|infixl\\|infixr\\|prefix\\|prefixr\\|postfix\\|postfixl\\|local\\|exportdef\\|useonly\\)\\( \\)" . (2 font-lock-definitions-face keep t))
    ;; Types
    ("\\([ (]\\)\\(int\\|real\\|string\\|list\\|out_stream\\|in_stream\\)\\([ ).]\\)" . (2 font-lock-types-face keep t))
    ("\\([^A-Za-z_]\\)\\(o\\)\\([^A-Za-z_]\\)" . (2 font-lock-types-face keep t))
    ;; Connectives that do not need spaces around them
    ("[;|,]" . (0 font-lock-connectors-face keep t))
    ("\\([^\"]\\)\\(\\\\\\)\\([^a-zA-Z\"\\\\]\\)" . (2 font-lock-connectors-face t t))
))

(defvar font-lock-moduledefs-face (defface font-lock-moduledefs-face
  '((((class grayscale) (background light)) (:foreground "DimGray" t))
    (((class grayscale) (background dark)) (:foreground "LightGray" t))
    (((class color) (background light)) (:foreground "Purple2"))
    (((class color) (background dark)) (:foreground "CornflowerBlue"))
    (t (t))) "Used for module definitions"))

(defvar font-lock-variable-face (defface font-lock-variable-face
  '((((class grayscale) (background light)) (:foreground "DimGray" t))
    (((class grayscale) (background dark)) (:foreground "LightGray" t))
    (((class color) (background light)) (:foreground "RoyalBlue3"))
    (((class color) (background dark)) (:foreground "#FFFFFF"))
    (t (t))) "Used for variables starting with capital letters"))

(defvar font-lock-connectors-face (defface font-lock-connectors-face
  '((((class grayscale) (background light)) (:foreground "DimGray" t))
    (((class grayscale) (background dark)) (:foreground "LightGray" t))
    (((class color) (background light)) (:foreground "DarkGoldenrod4"))
    (((class color) (background dark)) (:foreground "SpringGreen2"))
    (t (t))) "Used for mathematical/logical connectives"))

(defvar font-lock-builtins-face (defface font-lock-builtins-face
  '((((class grayscale) (background light)) (:foreground "DimGray" t))
    (((class grayscale) (background dark)) (:foreground "LightGray" t))
    (((class color) (background light)) (:foreground "SpringGreen4"))
    (((class color) (background dark)) (:foreground "Turquoise2"))
    (t (t))) "Used for builtin predicates"))

(defvar font-lock-header-face (defface font-lock-header-face
  '((((class grayscale) (background light)) (:foreground "DimGray" t))
    (((class grayscale) (background dark)) (:foreground "LightGray" t))
    (((class color) (background light)) (:foreground "Magenta4"))
    (((class color) (background dark)) (:foreground "MediumOrchid1"))
    (t (t))) "Used for sig and module clauses"))

(defvar font-lock-definitions-face (defface font-lock-definitions-face
  '((((class grayscale) (background light)) (:foreground "DimGray" t))
    (((class grayscale) (background dark)) (:foreground "LightGray" t))
    (((class color) (background light)) (:foreground "DarkCyan"))
    (((class color) (background dark)) (:foreground "LightBlue"))
    (t (t))) "Used for type and kind definitions"))

(defvar font-lock-types-face (defface font-lock-types-face
  '((((class grayscale) (background light)) (:foreground "DimGray" t))
    (((class grayscale) (background dark)) (:foreground "LightGray" t))
    (((class color) (background light)) (:foreground "Orange3"))
    (((class color) (background dark)) (:foreground "Orange"))
    (t (t))) "Used for builtin types"))
