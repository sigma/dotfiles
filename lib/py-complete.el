;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some rather useful completion and help stuff for python in emacs
;;
;; Comments and suggestions to jensen@slog.dk
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; usage:
;; 1. place py-complete.el somewhere in your load-path
;; 2. insert the following into your .emacs:
;;    (autoload 'py-complete-init "py-complete")
;;    (add-hook 'python-mode-hook 'py-complete-init)
;;
;; - Several uses are supported:
;;
;;   1. hippie-expand support via try-complete-py-complete-symbol,
;;   just like the lisp-version, nice for the [S-tab] hungry.
;;
;;   2. minibuffer-complete, pressing [M-return] brings up a
;;   minibuffer completion of the expression before point, usefull if
;;   you want to get an overview of your options.
;;
;;  3. pressing [f1] brings up help on a python symbol before point
;;  (just what help(thingy) gives
;;
;;  4. pressing "(" tries to parse the preceeding tokens as a funtion
;;  or method, and retrieve the signature via the inspect module, and
;;  messages it to you
;;
;;   5. pressing "," shows last signature.
;;
;;      (4+5) is great for calling functions like:
;;
;;         string.join
;;
;;      where I can never remember whether it's the list or seperator first
;;
;;   6. tries to work for both py-execute-buffer and
;;   py-execute-import-or-reload oriented work-styles. Feel free to
;;   suggest improvements, or to just improve the code yourself.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2003-12-29: replaced "py-mode-hook" with "python-mode-hook"
;; 2003-11-19: fixed long remaining bug when completing input without
;;             completions
;; 2003-10-16: Added usage info 
;; 2003-10-14: fixed some minor bugs, now seems to work!
;; 2003-10-09: rewrite of entire python-process-communication 
;; 2003-10-05: implemented separate completion, instead of rlcompleter

;;;
;;; Customizations
;;;
(defgroup py-complete nil "python completion"
  :group 'python)
(defcustom py-complete-exec-timeout 2
  "how long to wait for output from the python interpreter when doing
py-complete-exec. If this timeout is exceeded signal an error"
  :type 'integer
  :group 'py-complete)
(defcustom py-complete-use-filename t
  "use the filename of current buffer as prefix when trying completes
outside the interactive python buffer

Usefull if you import files into the interactive python buffer,
instead of evaluating them directly into the buffer"
  :type '(choice (const nil   :tag "No")
                 (const t  :tag "Yes"))
  :group 'py-complete)
(defcustom py-complete-eat-output t
  "remove the output it reads from the interactive
python buffer when py-complete'ing"
  :type '(choice (const nil   :tag "No")
                 (const t  :tag "Yes"))
  :group 'py-complete)

;;;
;;; communication with the python process
;;;
(defun py-complete-ensure-shell nil
  "make sure an interactive python shell is available"
  (if (not (get-process py-which-bufname))
      (py-shell))
  (get-process py-which-bufname))
(defun py-complete-last-output nil
  "retrieve output from last python command and delete it from the
visible buffer"
  (save-excursion
    (re-search-backward ">>> ")
    (let ((out-end (match-beginning 0)))
      (re-search-backward ">>> ")
      (let ((out-begin (match-end 0))
            (cmd-begin (match-beginning 0)))
        (let ((ret (buffer-substring-no-properties out-begin out-end)))
          (if py-complete-eat-output
              (delete-region cmd-begin out-end))
          ret)))))
(defun py-complete-exec (string)
  "execute STRING from the python-shell, return output made, removing
it from the python-shell buffer

Remember to end STRING with a newline
"
  (let ((proc (py-complete-ensure-shell)))
    (save-excursion
      (set-buffer (process-buffer proc))
      (let ((bound (marker-position (process-mark proc))))
        (process-send-string proc string)
        (message "py-complete: waiting for python prompt")
        (let (out done)
          (while (not done)
            (if (not (accept-process-output proc 2))
                (error "py-complete: python seems to hang"))
            (save-excursion
              (goto-char bound)
              (if (re-search-forward ">>> " nil t)
                  (set 'done t)))))
        (py-complete-last-output)))))
(defun py-complete-parse-list (string)
  "Simplest possible parsing of python list-of-strings, usefull in
py-complete-completions"
  (if string
      (let ((l nil) (idx 0))
        (while (string-match "'\\([^']+\\)'" string idx)
          (setq l (cons (match-string-no-properties 1 string) l))
          (setq idx (+ (match-end 1) 1)))
        (nreverse l))))

;;;
;;; core function: py-complete-completions
;;;
(defun py-complete-completions (string &optional filename)
  "list possible completions for STRING"
  (let ((output   
         (py-complete-exec 
          (format "__py_complete_string('''%s''', %s, globals(), locals())\n" 
                  string
                  (if filename
                      (format "'''%s'''" filename)
                    "None")))))
    (if (not (string-match "Traceback (most recent call last):" output))
        (py-complete-parse-list output))))


;;;
;;; finding what to complete
;;;

;; where to stop when looking for a completion cadidate
(setq py-complete-completion-re 
      (let ((v " \t\n\['\",()"))
        (format "[%s]\\([^%s]+\\)\\=" v v)))
(defun py-complete-completion-candidate nil
  "return a candidate before point for calling py-complete-completions
on, or nil"
  (interactive)
  (save-excursion
    (and (re-search-backward py-complete-completion-re nil t)
         (let ((match (match-string-no-properties 1)))
           match))))

;;;
;;; hippie expand integration
;;;
(set 'py-complete-try-complete-symbol-list nil)
(defun py-complete-try-complete-symbol (old)
  "Try to complete a python symbol (for use with hippie-expand)

The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (condition-case var
      (progn
        (if (not old)
            (progn
              (if (py-complete-completion-candidate)
                  (progn
                    (require 'hippie-exp)
                    (he-init-string  (match-beginning 1) (point))
                    (set 'py-complete-try-complete-symbol-list
                         (py-complete-completions 
                          (match-string-no-properties 1)
                          (and py-complete-use-filename buffer-file-name)))))))
        (py-complete-try-complete-symbol-next))
      (error (progn (message "py-complete unable to hippie expand") nil))))
(defun py-complete-try-complete-symbol-next nil
  "get next completion (for use with hippie-expand)"
  (interactive)
  (let ((str (car py-complete-try-complete-symbol-list)))
    (if str
        (progn
          (he-substitute-string str nil)
          (set 'py-complete-try-complete-symbol-list
               (cdr py-complete-try-complete-symbol-list))
          t)
      (he-reset-string)
      nil)))
(defun py-complete-bind-hippie-expand nil
  "make nessesary magic for buffer-locally extending hippie-expand"
  (interactive)
  (make-variable-buffer-local 'hippie-expand-try-functions-list)
  (set 
   'hippie-expand-try-functions-list
   (cons 
    'py-complete-try-complete-symbol
    hippie-expand-try-functions-list)))

;;;
;;; Guessing the right "thing" from the cursor position, use thing-at-point.
;;;  - could be refined, when the need arises
;;; 
(defvar py-complete-python-identifier-re "\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)"
  "match a python identifier")
(defvar py-complete-python-dotexpr-re 
  (format "\\b%s\\(?:\\.%s\\)*" 
          py-complete-python-identifier-re 
          py-complete-python-identifier-re)
  "match a python expressions consisting of x.y.z")
(defun py-complete-python-dotexpr-begin nil
  (interactive)
  (re-search-backward "[^a-zA-Z_0-9\\.]")
  (forward-char))
(defun py-complete-python-dotexpr-end nil
  (interactive)
  (re-search-forward "[a-zA-Z_0-9\\.]*"))
(put 'python-dotexpr 'beginning-op 'py-complete-python-dotexpr-begin)
(put 'python-dotexpr 'end-op 'py-complete-python-dotexpr-end)

;;;
;;; help
;;;
(defun py-complete-help (string)
  "get help on a python expression"
  (let ((help-string (py-complete-exec (format "print help(%s)\n" string))))
    (if (and help-string (> (length help-string) 300))
        (with-output-to-temp-buffer "*Python Help*"
          (print help-string))
      (py-complete-show help-string))))
(defun py-complete-help-thing-at-point nil
  (interactive)
  (require 'thingatpt)
  (let ((sym (thing-at-point 'python-dotexpr)))
    (if sym
        (py-complete-help sym))))
(defun py-complete-help-expr nil
  (interactive)
  (require 'thingatpt)
  (let ((dotexpr (read-string "help on: " (thing-at-point 'python-dotexpr))))
    (if dotexpr
        (py-complete-help dotexpr))))

;;;
;;; signature
;;;
(set 'py-complete-current-signature nil)
(defun py-complete-signature (function)
  "get signature of a python function or method"
  (interactive)
  (set 'py-complete-current-signature
       (py-complete-exec
        (format "print __py_signature(%s)\n" function))))
(defun py-complete-signature-show nil
  (interactive)
  (require 'thingatpt) 
  (let ((sym (thing-at-point 'python-dotexpr)))
    (if sym
        (progn 
          (py-complete-show (py-complete-signature sym))))))
(defun py-complete-signature-expr nil
  (interactive)
  (require 'thingatpt)
  (let ((dotexpr (read-string "signature on: "
                              (thing-at-point 'python-dotexpr))))
    (if dotexpr
        (py-complete-show
         (py-complete-signature dotexpr)))))

;;;
;;; minibuffer completions
;;;
(defun py-complete-minibuffer-complete nil
  "try to complete stuff at point using the minibuffer"
  (interactive)
  (if (py-complete-completion-candidate)
      (let* ((val (match-string-no-properties 1))
            (beg (match-beginning 1))
            (end (match-end 1))
            (completes (py-complete-completions val))
            (ret (completing-read 
                  "complete: " ; prompt
                  (mapcar (lambda (item) `(,item 0)) completes) ; collection
                  nil ; predicate
                  nil ; require match
                  val ; prefix
                  nil ; history
                  nil ; default
                  t ; inherit-input-method
                  )))
          (if ret
              (progn
                (delete-region beg end)
                (insert ret))))))

;;;
;;; hooking
;;;
(defun py-complete-show (string)
  (display-message-or-buffer string "*PythonHelp*"))
;; keys
(defun py-complete-electric-lparen nil
  "electricly insert '(', and try to get a signature for the stuff to the left"
  (interactive)
  (py-complete-signature-show)
  (self-insert-command 1))

(defun py-complete-electric-comma nil
  "electricly insert ',', and redisplay latest signature"
  (interactive)
  (self-insert-command 1)
  (if py-complete-current-signature
      (py-complete-show (format "%s" py-complete-current-signature))))
(defun py-complete-setup-keys nil
  "setup some standard keys for py-complete"
  (define-key py-mode-map "(" 'py-complete-electric-lparen)
  (define-key py-shell-map "(" 'py-complete-electric-lparen)
  (define-key py-mode-map "," 'py-complete-electric-comma)
  (define-key py-shell-map "," 'py-complete-electric-comma)
  (define-key py-mode-map [C-return] 'py-complete-minibuffer-complete)
  (define-key py-shell-map [C-return] 'py-complete-minibuffer-complete)
  (define-key py-mode-map [f1] 'py-complete-help-thing-at-point)
  (define-key py-shell-map [f1] 'py-complete-help-thing-at-point)
  (define-key py-mode-map [f2] 'py-complete-signature-expr)
  (define-key py-shell-map [f2] 'py-complete-signature-expr))
;; the old py-shell-mode doesn't run py-shell-hook on initialization,
;; so we do it ourselves. The definition of py-shell below is copied
;; from python-mode
(defun py-complete-hack-shell nil
  (condition-case var
      (symbol-value 'py-shell-hook)
    (error
     ;; hack it so we get a hook at py-shell invocation
     ;; all this does, is run py-shell-hook when a py-shell is started
     (message "py-shell-hook not defined... hacking")
     (require 'python-mode)
     (add-hook 'py-shell-hook 'py-complete-init)
     (defun py-shell (&optional argprompt)
       "Start an interactive Python interpreter in another window.
This is like Shell mode, except that Python is running in the window
instead of a shell.  See the `Interactive Shell' and `Shell Mode'
sections of the Emacs manual for details, especially for the key
bindings active in the `*Python*' buffer.

With optional universal-argument, the user is prompted for the
flags to pass to the Python interpreter.  This has no effect when this
command is used to switch to an existing process, only when a new
process is started.  If you use this, you will probably want to ensure
that the current arguments are retained (they will be included in the
prompt).  This argument is ignored when this function is called
programmatically, or when running in Emacs 19.34 or older.

Note: You can toggle between using the CPython interpreter and the
JPython interpreter by hitting \\[py-toggle-shells].  This toggles
buffer local variables which control whether all your subshell
interactions happen to the `*JPython*' or `*Python*' buffers (the
latter is the name used for the CPython buffer).

Warning: Don't use an interactive Python if you change sys.ps1 or
sys.ps2 from their default values, or if you're running code that
prints `>>> ' or `... ' at the start of a line.  `python-mode' can't
distinguish your output from Python's output, and assumes that `>>> '
at the start of a line is a prompt from Python.  Similarly, the Emacs
Shell mode code assumes that both `>>> ' and `... ' at the start of a
line are Python prompts.  Bad things can happen if you fool either
mode.

Warning:  If you do any editing *in* the process buffer *while* the
buffer is accepting output from Python, do NOT attempt to `undo' the
changes.  Some of the output (nowhere near the parts you changed!) may
be lost if you do.  This appears to be an Emacs bug, an unfortunate
interaction between undo and process filters; the same problem exists in
non-Python process buffers using the default (Emacs-supplied) process
filter."
       (interactive "P")
       (require 'python-mode)
       ;; Set the default shell if not already set
       (when (null py-which-shell)
         (py-toggle-shells py-default-interpreter))
       (let ((args py-which-args))
         (when (and argprompt
                    (interactive-p)
                    (fboundp 'split-string))
           ;; TBD: Perhaps force "-i" in the final list?
           (setq args (split-string
                       (read-string (concat py-which-bufname
                                            " arguments: ")
                                    (concat
                                     (mapconcat 'identity py-which-args " ") " ")
                                    ))))
         (switch-to-buffer-other-window
          (apply 'make-comint py-which-bufname py-which-shell nil args))
         (make-local-variable 'comint-prompt-regexp)
         (setq comint-prompt-regexp "^>>> \\|^[.][.][.] \\|^(pdb) ")
         (add-hook 'comint-output-filter-functions
                   'py-comint-output-filter-function)
         ;; pdbtrack
         (add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)
         (setq py-pdbtrack-do-tracking-p t)
         (set-syntax-table py-mode-syntax-table)
         (use-local-map py-shell-map)
         ;; hook to run when starting the shell
         (condition-case var
             (if py-shell-hook-has-run
                 (message "py-shell-hook already run"))
           (error 
            (run-hooks 'py-shell-hook)
            (make-local-variable 'py-shell-hook-has-run)
            (set 'py-shell-hook-has-run t))))))))
;; set up py-complete
(defun py-complete-init nil
  (py-complete-hack-shell)
  (py-complete-define-python)
  (py-complete-bind-hippie-expand)
  (py-complete-setup-keys))
;; make sure the init stuff is run
(add-hook 'python-mode-hook 'py-complete-init)

;;;
;;; python code
;;;
(defun py-complete-define-python nil
  (interactive)
  (let ((actually-do-it 
         (condition-case var
             py-complete-define-python-run
           (error t))))
    (if actually-do-it
        (let ((file (make-temp-file "py-complete")))
          (message "defining py-complete helper function")
          (condition-case var
              (progn
                (write-region "
def __py_complete_sort(x,y):
    if len(x) < len(y):
      return -1
    elif len(y) < len(x):
      return 1
    else:
      if x < y:
        return -1
      elif y < x:
        return 1
      else:
        return 0
def __py_completions(str, domains):
    r = []
    for list in domains:
        if not list is None:
            for e in list:
                if e.startswith(str) and not e in r:
                    r.append(e)
    r.sort(__py_complete_sort)
    return r

def __py_complete_string(str, file, globals, locals):
    from os.path import basename, dirname
    from keyword import kwlist
    r = []

    # find elements in module namespace
    if file and file.endswith('.py'):
        try:
            module = basename(file)[:-3]
            if module == '__init__':
                module = basename(dirname(file))
            last_dot = str.rfind('.')
            if last_dot == -1:
                prefix = ''
                end = str
                domains = [ dir(eval(module, globals, locals)) ]
            else:
                prefix = '%s.' % str[:last_dot]
                end = str[last_dot+1:]
                domains = [ dir(eval(prefix[:-1], globals, locals)) ]
            r += [ '%s%s' % (prefix, c)
                   for c in __py_completions(end, domains) ]
        except:
            import traceback
            traceback.print_exc()
            pass

    # find elements in interpreters namespace
    try:
        last_dot = str.rfind('.')
        if last_dot == -1:
            prefix = ''
            end = str
            domains = [ locals, globals, kwlist ]
        else:
            prefix = '%s.' % str[:last_dot]
            end = str[last_dot+1:]
            domains = [ dir(eval(prefix[:-1], globals, locals)) ]
        r += [ '%s%s' % (prefix, c)
               for c in __py_completions(end, domains) ]
    except:
        # no luck, that is OK
        import traceback
        traceback.print_exc() 
        pass
        
    return r


def __py_signature(f):
  import inspect
  if inspect.ismethod(f): f = f.im_func
  if not inspect.isfunction(f): return None
  (args, varargs, varkw, defaults) = inspect.getargspec(f)
  return('%s: %s'
         % (f.__name__, inspect.formatargspec(args,varargs,varkw,defaults)))
print 'py_complete loaded'
"
                              nil
                              file)
                (process-send-string 
                 (get-process py-which-bufname)
                 (format "execfile('''%s''')\n" file))
                ;; wait until python _probably_ opened the file, since it
                ;; would be missing otherwise.
                (sleep-for 1)
                (delete-file file))
            (error (delete-file file)))))))

(provide 'py-complete)
