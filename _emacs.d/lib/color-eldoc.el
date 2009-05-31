;;; color-eldoc.el -- Eldoc add-on to colorize the current argument

;; Version: 1.1.2
;; Time-stamp: <16/05/2008 12:15:11 Yann Hodique>
;; at <http://www.dd.chalmers.se/~bojohan/emacs/lisp/>

;;; Commentary:
;; (require 'color-eldoc)
;; Works best with a `eldoc-argument-case' value of `upcase' (the
;; default.)

;; TODO:
;; * Deal with &key args (in CVS Emacs) and other stuff

;;; Code:

(require 'eldoc)

(eval-when-compile
  (require 'cl) ; incf
  (defvar sym)  ; `sym' is a dynamically bound variable containing
		; the current function or variable
  )

;; Color the N'th arg, and any corresponding &foo keyword
(defun color-eldoc-arg (n name doc type)
  (let* ((args (split-string doc)))

    ;; Change (FOO BODY ...) to (FOO BODY...)
    (let ((temp (last args 2)))
      (when (and (not (eq 'setq sym)) (string= "..." (cadr temp)))
	(setcar temp (concat (car temp) "..."))
	(setcdr temp nil)))
    (when (eq sym 'setq-default)
      (setq args (mapcar 'copy-sequence '("SYM" "VAL" "SYM" "VAL" "..."))))

    (cond ((and (eq type 'fn)
		(eq (car-safe (symbol-function sym)) 'autoload)))
	  ((zerop n)
	   (color-eldoc-add-face name (if (eq 'fn type)
					  font-lock-function-name-face
					font-lock-variable-name-face)))
	  ;; "SYM VAL SYM VAL ..."
	  ((and (member sym '(setq setq-default))
		(> n 4))
	   (color-eldoc-add-face (nth 4 args) ; ...
				 font-lock-variable-name-face)
	   (color-eldoc-add-face (nth (if (= (logand n 1) 1) 2 3) args)	; oddp
				 font-lock-variable-name-face))

	  ;; Normal case
	  (args
	   (let ((m n)
		 (arg args)
		 (nargs (loop for s in args count (/= ?& (aref s 0))))
		 ;;(nargs (count-if-not (lambda (s) (eq ?& (aref s 0))) args))
		 ampersand rest)

	     (while (and (> (decf m) 0)
			 (setq arg (or (cond ((eq ?& (aref (car arg) 0))
					      (setq ampersand arg)
					      (cddr arg))
					     (t (cdr arg)))
				       arg))))
	     (when (eq ?& (aref (car arg) 0))
	       (shiftf ampersand arg (cdr arg)))

	     ;; [DOCSTRING] is optional docstring. Maybe skip to next
	     ;; argument.
	     (and (string-match "\\[DOC" (car arg))
		  (not (or (color-eldoc-inside-string-p)
			   (and (memq (char-syntax (char-before))
				      '(?\  ?>))
				(memq (char-syntax (char-after))
				      '(?\  ?\" ?>)))
			   (color-eldoc-inside-string-p (1- (point)))))
		  (cdr arg)
		  (pop arg))

	     (when ampersand
	       (color-eldoc-add-face (car ampersand) font-lock-type-face))
	     (let ((warning
		    ;; Wrong number of args?
		    (and (> n nargs)
			 (not (and ampersand
				   (string= "&rest" (downcase (car ampersand)))))
			 (not (string-match "\\.\\.\\.\\'" (car arg))))))
	       (color-eldoc-add-face
		(car arg)
		(cond (warning font-lock-warning-face)
		      ((string-match "\\[?DOC" (car arg)) font-lock-doc-face)
		      (t font-lock-variable-name-face))
		(and (string-match "\\`\\[" (car arg)) 1)
		(and (string-match "\\]\\'" (car arg)) -1)))))

	  (t (color-eldoc-add-face name font-lock-warning-face)))

    (concat name
	    (and name ": ")
	    (and (eq 'fn type) "(")
	    (mapconcat 'identity  args " ")
	    (and (eq 'fn type) ")"))))

(defun color-eldoc-add-face (string face &optional start end)
  "Add face to the STRING from START to END.
  If START or END is negative, it counts from the end."
  (add-text-properties (cond ((null start) 0)
			     ((wholenump start) start)
			     (t (+ (length string) start)))
		       (cond ((null end) (length string))
			     ((wholenump end) end)
			     (t (+ (length string) end)))
		       (list 'face face)
		       string))

(defun color-eldoc-inside-string-p (&optional pos)
  (let ((parse (save-excursion
		 (if (fboundp 'syntax-ppss)
		     (syntax-ppss pos)
		   (when pos (goto-char pos))
		   (parse-partial-sexp
		    (save-excursion (beginning-of-defun) (point))
		    (point))))))
    ;; Return string start position
    (and (nth 3 parse) (nth 8 parse))))

;; Count args, return current arg's number
(defun color-eldoc-current-arg ()
  (let ((opoint (point))
	(arg -1))
    (save-excursion
      (eldoc-beginning-of-sexp)
      (condition-case nil
	  (while (< (point) opoint)
	    (forward-sexp)
	    (incf arg))
	(scan-error (incf arg))))
    (max arg 0)))

;; Return the colorized message
(defun color-eldoc-message (msg type)
  (when msg
    ;;(string-match (concat (regexp-quote sym) ": ") msg)
    (string-match "\\(?:\\(\\S-*\\): \\)?\\(.*\\)" msg) ;
    (let* ((name (match-string 1 msg))
	   (doc  (match-string 2 msg))
	   (n (if (eq 'var type) ; (not (eq ?\( (aref doc 0)))
		  0
		(setq doc (substring doc 1 -1))
		(color-eldoc-current-arg))))
      (color-eldoc-arg n name doc type))))


(when (boundp 'eldoc-documentation-function)
  (defun color-eldoc-documentation-function ()
    ;; Taken from the body of eldoc-print-current-symbol-info
    (let* ((current-symbol (eldoc-current-symbol))
	   (current-fnsym  (eldoc-fnsym-in-current-sexp))
	   (fnsym (lambda ()
		    (let ((sym current-fnsym))
		      (color-eldoc-message
		       (eldoc-get-fnsym-args-string current-fnsym)
		       'fn))))
	   (var (lambda ()
		  (let ((sym current-symbol))
		    (color-eldoc-message
		     (eldoc-get-var-docstring current-symbol)
		     'var))))
	   (doc (cond
		 ((eq current-symbol current-fnsym)
		  (or (funcall fnsym) (funcall var)))
		 (t
		  (or (funcall var) (funcall fnsym))))))
      doc))

  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (set (make-local-variable 'eldoc-documentation-function)
		   'color-eldoc-documentation-function)))
  )

(unless (boundp 'eldoc-documentation-function)
  (defadvice eldoc-get-fnsym-args-string (after color-eldoc activate)
    ;; eldoc-get-fnsym-args-string: (SYM)
    (setq ad-return-value (color-eldoc-message ad-return-value 'fn)))

  (defadvice eldoc-get-var-docstring (after color-eldoc activate)
    ;; eldoc-get-var-docstring: (SYM)
    (setq ad-return-value (and (not (color-eldoc-inside-string-p))
			       (color-eldoc-message ad-return-value 'var))))
  )

  ;; (defadvice eldoc-fnsym-in-current-sexp (after color-eldoc activate)
  ;;   (when (color-eldoc-inside-string-p) (setq ad-return-value nil)))

;; Fix the "code-in-strings" bug
(defadvice eldoc-beginning-of-sexp (around color-eldoc activate)
  (let ((start (color-eldoc-inside-string-p)))
    (when start (goto-char start)))
  ad-do-it)

(eldoc-add-command-completions "newline")

(provide 'color-eldoc)

;;; color-eldoc.el ends here
