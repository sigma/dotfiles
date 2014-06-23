;;; lib-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "autoloads+" "autoloads+.el" (20399 25031 0
;;;;;;  0))
;;; Generated autoloads from autoloads+.el

(autoload 'create-file-autoloads "autoloads+" "\
Update the autoloads for FILE in `generated-autoload-file'
\(which FILE might bind in its local variables).
If SAVE-AFTER is non-nil (which is always, when called interactively),
save the buffer too.

Return FILE if there was no autoload cookie in it, else nil.

\(fn FILE &optional SAVE-AFTER)" t nil)

(autoload 'create-directory-autoloads "autoloads+" "\
Update loaddefs.el with all the current autoloads from DIRS, and no old ones.
This uses `update-file-autoloads' (which see) do its work.
In an interactive call, you must give one argument, the name
of a single directory.  In a call from Lisp, you can supply multiple
directories as separate arguments, but this usage is discouraged.

The function does NOT recursively descend into subdirectories of the
directory or directories specified.

\(fn DIR)" t nil)

;;;***

;;;### (autoloads nil "bst" "bst.el" (20399 25031 0 0))
;;; Generated autoloads from bst.el

(autoload 'bst-mode "bst" "\
Setup for BibTeX style file editing.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "map-lines" "map-lines.el" (20399 25032 0 0))
;;; Generated autoloads from map-lines.el

(autoload 'map-lines "map-lines" "\
Map COMMAND over lines matching REGEX.

\(fn COMMAND-C REGEX)" t nil)

;;;***

;;;### (autoloads nil "moy-bbdb" "moy-bbdb.el" (20399 25032 0 0))
;;; Generated autoloads from moy-bbdb.el

(autoload 'bbdb/send-ignore-most-messages-hook "moy-bbdb" "\
For use as the value of `bbdb/send-auto-create-p'.
This will automatically create BBDB entries for messages which match
the bbdb/send-ignore-most-messages-alist (which see) and *no* others.

\(fn &optional INVERT-SENSE)" nil nil)

(autoload 'bbdb/send-ignore-some-messages-hook "moy-bbdb" "\
For use as a `bbdb/send-auto-create-hook'.
This will automatically create BBDB entries for messages which do *not*
match the `bbdb/send-ignore-some-messages-alist' (which see).

\(fn)" nil nil)

(autoload 'bbdb/send-auto-notes-hook "moy-bbdb" "\
For use as a `bbdb/send-notice-hook'.  This might automatically add
some text to  the notes field of the BBDB  record corresponding to the
current record  based on the header  of the current  message.  See the
documentation  for   the  variables  `bbdb/send-auto-notes-alist'  and
`bbdb/send-auto-notes-ignore'.

\(fn RECORD)" nil nil)

(autoload 'bbdb/send-hook "moy-bbdb" "\
Parse headers of outgoing message, insert the addresses of the
  recipients one by one into BBDB if they do not exist already

\(fn)" t nil)

;;;***

;;;### (autoloads nil "page-break" "page-break.el" (20399 25032 0
;;;;;;  0))
;;; Generated autoloads from page-break.el

(autoload 'page-break-mode "page-break" "\
Toggle Page Break mode.

In Page Break mode, page breaks (^L characters) are displayed as a
horizontal line of `page-break-string-char' characters.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "pod" "pod.el" (20399 25032 0 0))
;;; Generated autoloads from pod.el

(autoload 'pod-quick-start "pod" "\
Provides electric help regarding `pod-quick-start'.

\(fn)" t nil)

(autoload 'pod-introduction "pod" "\
Provides electric help regarding `pod-introduction'.

\(fn)" t nil)

(autoload 'pod-commentary "pod" "\
Provides electric help regarding `pod-commentary'.

\(fn)" t nil)

(autoload 'pod-new-features "pod" "\
Provides electric help regarding `pod-new-features'.

\(fn)" t nil)

(autoload 'pod-todo "pod" "\
Provides electric help regarding `pod-todo'.

\(fn)" t nil)

(autoload 'pod-reset-results "pod" "\


\(fn)" t nil)

(autoload 'pod-deactivate-advice "pod" "\


\(fn)" t nil)

(autoload 'pod-activate-advice "pod" "\


\(fn)" t nil)

(autoload 'pod-load "pod" "\
Kinda like load..

\(fn FILE &rest ARGS)" nil nil)

(autoload 'pod-load-file "pod" "\


\(fn FILE)" t nil)

(autoload 'pod-last-sexp-runtime "pod" "\


\(fn &optional PT)" t nil)

;;;***

;;;### (autoloads nil "tabbar" "tabbar.el" (20399 25032 0 0))
;;; Generated autoloads from tabbar.el

(autoload 'tabbar-backward "tabbar" "\
Select the previous available tab.
Depend on the setting of the option `tabbar-cycling-scope'.

\(fn)" t nil)

(autoload 'tabbar-forward "tabbar" "\
Select the next available tab.
Depend on the setting of the option `tabbar-cycling-scope'.

\(fn)" t nil)

(autoload 'tabbar-backward-group "tabbar" "\
Go to selected tab in the previous available group.

\(fn)" t nil)

(autoload 'tabbar-forward-group "tabbar" "\
Go to selected tab in the next available group.

\(fn)" t nil)

(autoload 'tabbar-backward-tab "tabbar" "\
Select the previous visible tab.

\(fn)" t nil)

(autoload 'tabbar-forward-tab "tabbar" "\
Select the next visible tab.

\(fn)" t nil)

(defvar tabbar-mode nil "\
Non-nil if Tabbar mode is enabled.
See the command `tabbar-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `tabbar-mode'.")

(custom-autoload 'tabbar-mode "tabbar" nil)

(autoload 'tabbar-mode "tabbar" "\
Toggle display of a tab bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\(fn &optional ARG)" t nil)

(autoload 'tabbar-local-mode "tabbar" "\
Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When on and tab bar global mode is on, if a buffer local value of
`header-line-format' exists, it is saved, then the local header line
is killed to show the tab bar.  When off, the saved local value of the
header line is restored, hiding the tab bar.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "zap-char" "zap-char.el" (20399 25032 0 0))
;;; Generated autoloads from zap-char.el

(autoload 'zap-from-char "zap-char" "\
Kill from ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.

\(fn ARG CHAR)" t nil)

(autoload 'zap-following-char "zap-char" "\
Kill following ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.

\(fn ARG CHAR)" t nil)

(autoload 'zap-upto-char "zap-char" "\
Kill up to ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.

\(fn ARG CHAR)" t nil)

(autoload 'zap-to-char "zap-char" "\
Kill to ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.

\(fn ARG CHAR)" t nil)

;;;***

;;;### (autoloads nil nil ("cmake-mode.el" "color-eldoc.el" "cython-mode.el"
;;;;;;  "decompile.el" "flashcard.el" "ipython.el" "ll-debug.el"
;;;;;;  "osd.el" "powerline.el" "py-complete.el" "sure-tags.el" "tree-widget.el"
;;;;;;  "virtualenv.el" "visible-mark-mode.el") (21149 24450 13574
;;;;;;  0))

;;;***

(provide 'lib-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lib-loaddefs.el ends here
