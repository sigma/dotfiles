;;;; auto-save.el - safer autosaving with support for ange-ftp and /tmp

(defconst auto-save-version (substring "$Revision: 1.1 $" 11 -2)
  "$Id: auto-save.el,v 1.1 2004/04/19 17:54:02 sigma Exp $")

;;;; Copyright (C) 1992 by Sebastian Kremer <sk@thp.uni-koeln.de>

;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 1, or (at your option)
;;;; any later version.
;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; LISPDIR ENTRY for the Elisp Archive ===============================
;;;;    LCD Archive Entry:
;;;;    auto-save|Sebastian Kremer|sk@thp.uni-koeln.de|
;;;;    Safer autosaving with support for ange-ftp and /tmp|
;;;;    01-May-1992|1.19|~/misc/auto-save.el.Z|

;;;; OVERVIEW ==========================================================

;;;; Combines autosaving for ange-ftp (to a local or remote directory)
;;;; with the ability to do autosaves to a fixed directory on a local
;;;; disk, in case NFS is slow.  The auto-save file used for
;;;;     /usr/foo/bar/baz.txt
;;;; will be
;;;;     AUTOSAVE/#\!usr\!foo\!bar\!baz.txt#
;;;; assuming AUTOSAVE is the non-nil value of the variable
;;;; `auto-save-directory'.

;;;; Takes care that autosave files for non-file-buffers (e.g. *mail*)
;;;; from two simultaneous Emacses don't collide.

;;;; SECURITY NOTE: make your auto-save-directory unreadable/unwriteable for
;;;; anyone but yourself, since it might contain stuff from
;;;; unreadable/unwriteable directories (the auto-save files are all in one
;;;; great big auto-save-directory, so the original directory permissions are
;;;; ignored).

;;;; Autosaves even if the current directory is not writable.

;;;; Can limit autosave names to 14 characters using a hash function,
;;;; see `auto-save-hash-p'.

;;;; See `auto-save-directory' and `make-auto-save-file-name' and
;;;; references therein for complete documentation.

;;;; Meta-x recover-all-files will effectively do recover-file on all
;;;; files whose autosave file is newer (one of the benefits of having
;;;; all autosave files in the same place).

;;;; INSTALLATION ======================================================

;;;; Put this file into your load-path and the following in your ~/.emacs:

;;;; If you want to autosave in the fixed directory /tmp/USER-autosave/
;;;; (setq auto-save-directory
;;;;       (concat "/tmp/" (user-login-name) "-autosave/"))

;;;; If you don't want to save in /tmp (e.g., because it is swap
;;;; mounted) but rather in ~/autosave/
;;;;   (setq auto-save-directory (expand-file-name "~/autosave/"))

;;;; If you want to save each file in its own directory (the default)
;;;;   (setq auto-save-directory nil)
;;;; You still can take advantage of autosaving ange-ftp remote files
;;;; in a fixed local directory, `auto-save-directory-fallback' will
;;;; be used.

;;;; If you want to use 14 character hashed autosave filenames
;;;;   (setq auto-save-hash-p t)

;;;; Finally, put this line after the others in your ~/.emacs:
;;;;   (require 'auto-save)


;;;; ACKNOWLEDGEMENT ===================================================

;;;; This code is loosely derived from autosave-in-tmp.el by Jamie
;;;; Zawinski <jwz@lucid.com> (the version I had was last modified 22
;;;; dec 90 jwz) and code submitted to ange-ftp-lovers on Sun, 5 Apr
;;;; 92 23:20:47 EDT by drw@BOURBAKI.MIT.EDU (Dale R. Worley).
;;;; auto-save.el tries to cover the functionality of those two
;;;; packages.

;;;; Valuable comments and help from Dale Worley, Andy Norman, Jamie
;;;; Zawinski and Sandy Rutherford are gratefully acknowledged.

;;;; CUSTOMIZATION =====================================================

(defvar auto-save-directory nil

  ;;; Don't make this user-variable-p, it should be set in .emacs and
  ;;; left at that.  In particular, it should remain constant across
  ;;; several Emacs session to make recover-all-files work.

  "If non-nil, fixed directory for autosaving: all autosave files go
there.  If this directory does not yet exist at load time, it is
created and its mode is set to 0700 so that nobody else can read your
autosave files.

If nil, each autosave files goes into the same directory as its
corresponding visited file.

A non-nil `auto-save-directory' could be on a local disk such as in
/tmp, then auto-saves will always be fast, even if NFS or the
automounter is slow.  In the usual case of /tmp being locally mounted,
note that if you run emacs on two different machines, they will not
see each other's auto-save files.

The value \(expand-file-name \"~/autosave/\"\) might be better if /tmp
is mounted from swap (possible in SunOS, type `df /tmp' to find out)
and thus vanishes after a reboot, or if your system is particularly
thorough when cleaning up /tmp, clearing even non-empty subdirectories.

It should never be an ange-ftp remote filename because that would
defeat `ange-ftp-auto-save-remotely'.

Unless you set `auto-save-hash-p', you shouldn't set this to a
directory in a filesystem that does not support long filenames, since
a file named

    /home/sk/lib/emacs/lisp/auto-save.el

will have a longish filename like

    AUTO-SAVE-DIRECTORY/#\\!home\\!sk\\!lib\\!emacs\\!lisp\\!auto-save.el#

as autosave file.

See also variables `auto-save-directory-fallback',
`ange-ftp-auto-save' and `ange-ftp-auto-save-remotely'.")

(defvar auto-save-hash-p nil
  "If non-nil, hashed autosave names of length 14 are used.
This is to avoid autosave filenames longer than 14 characters.
The directory used is `auto-save-hash-directory' regardless of
`auto-save-directory'.
Hashing defeats `recover-all-files', you have to recover files
individually by doing `recover-file'.")

;;; This defvar is in ange-ftp.el now, but for older versions it
;;; doesn't hurt to give it here as well so that loading auto-save.el
;;; does not abort.
(defvar ange-ftp-auto-save nil
  "If non-nil, allows remote ange-ftp files to be auto-saved.")

(defvar ange-ftp-auto-save-remotely nil
  "*If non-nil, causes the auto-save file for an ange-ftp file to be written in
the remote directory containing the file, rather than in a local directory.

For remote files, this being true overrides a non-nil
`auto-save-directory'.  Local files are unaffected.

If you want to use this feature, you probably only want to set this
true in a few buffers, rather than globally.  You might want to give
each buffer its own value using `make-variable-buffer-local'.

See also variable `ange-ftp-auto-save'.")

(defvar auto-save-offer-delete nil
  "*If non-nil, `recover-all-files' offers to delete autosave files
that are out of date or were dismissed for recovering.
Special value 'always deletes those files silently.")

;;;; end of customization

 
;;; Preparations to be done at load time

(defvar auto-save-directory-fallback (expand-file-name "~/autosave/")
  ;; not user-variable-p, see above
  "Directory used for local autosaving of remote files if
both `auto-save-directory' and `ange-ftp-auto-save-remotely' are nil.
Also used if a working directory to be used for autosaving is not writable.
This *must* always be the name of directory that exists or can be
created by you, never nil.")

(defvar auto-save-hash-directory
  (expand-file-name "hash/" (or auto-save-directory
    auto-save-directory-fallback))
  "If non-nil, directory used for hashed autosave filenames.")

(defun auto-save-check-directory (var)
  (let ((dir (symbol-value var)))
    (if (null dir)
 nil
      ;; Expand and store back into the variable
      (set var (setq dir (expand-file-name dir)))
      ;; Make sure directory exists
      (if (file-directory-p dir)
   nil
 ;; Else we create and chmod 0700 the directory
 (setq dir (directory-file-name dir)) ; some systems need this
 (if (fboundp 'make-directory) ; V19 or tree dired
     (make-directory dir)
   (call-process "mkdir" nil nil nil dir))
 (set-file-modes dir (* 7 8 8))))))

(mapcar (function auto-save-check-directory)
 '(auto-save-directory auto-save-directory-fallback))

(and auto-save-hash-p
     (auto-save-check-directory 'auto-save-hash-directory))

 
;;; Computing an autosave name for a file and vice versa

(defun make-auto-save-file-name ();; redefines files.el
  ;; auto-save-file-name-p need not be redefined.

  "Return file name to use for auto-saves of current buffer.
Does not consider `auto-save-visited-file-name'; that is checked
before calling this function.

Offers to autosave all files in the same `auto-save-directory'.  All
autosave files can then be recovered at once with function
`recover-all-files'.

Takes care to make autosave files for files accessed through ange-ftp
be local files if variable `ange-ftp-auto-save-remotely' is nil.

Takes care of slashes in buffer names to prevent autosave errors.

Takes care that autosave files for buffers not visiting any file (such
as `*mail*') from two simultaneous Emacses don't collide by prepending
the Emacs pid.

Uses 14 character autosave names if `auto-save-hash-p' is true.

Autosaves even if the current directory is not writable, using
directory `auto-save-directory-fallback'.

You can redefine this for customization (he he :-).
See also function `auto-save-file-name-p'."

  ;; We have to be very careful about not signalling an error in this
  ;; function since files.el does not provide for this (e.g. find-file
  ;; would fail for each new file).

  (condition-case error-data
      (let* ((file-name (or (and (boundp 'buffer-file-truename) ; From jwz,
     buffer-file-truename) ; for Emacs 19?
       buffer-file-name))
      ;; So autosavename looks like #%...#, roughly as with the
      ;; old make-auto-save-file-name function.  The
      ;; make-temp-name inserts the pid of this Emacs: this
      ;; avoids autosaving from two Emacses into the same file.
      ;; It cannot be recovered automatically then because in
      ;; the next Emacs session (the one after the crash) the
      ;; pid will be different, but file-less buffers like
      ;; *mail* must be recovered manually anyway.
      (name-prefix (if file-name nil (make-temp-name "#%")))
      (save-name (or file-name
       ;; Prevent autosave errors.  Buffername
       ;; (to become non-dir part of filename) will
       ;; be unslashified twice.  Don't care.
       (auto-save-unslashify-name (buffer-name))))
      (remote-p (and (stringp file-name)
       (fboundp 'ange-ftp-ftp-path)
       (ange-ftp-ftp-path file-name))))
 ;; Return the appropriate autosave file name:
 (expand-file-name;; a buffername needs this, a filename not
  (if remote-p
      (if ange-ftp-auto-save-remotely
   (auto-save-name-in-same-directory save-name)
        ;; We have to use the `fixed-directory' now since the
        ;; `same-directory' would be remote.
        ;; It will use the fallback if needed.
        (auto-save-name-in-fixed-directory save-name))
    ;; Else it is a local file (or a buffer without a file, hence
    ;; the name-prefix).
    ;; Hashed files always go into the special hash dir, never
    ;; in the same directory, to make recognizing reliable.
    (if (or auto-save-directory auto-save-hash-p)
        (auto-save-name-in-fixed-directory save-name name-prefix)
      (auto-save-name-in-same-directory save-name name-prefix)))))

    ;; If any error occurs in the above code, return what the old
    ;; version of this function would have done.  It is not ok to
    ;; return nil, e.g., when after-find-file tests
    ;; file-newer-than-file-p, nil would bomb.

    (error (progn
      (message "make-auto-save-file-name %s" error-data)
      (sit-for 2)
      (if buffer-file-name
   (concat (file-name-directory buffer-file-name)
    "#"
    (file-name-nondirectory buffer-file-name)
    "#")
        (expand-file-name (concat "#%" (buffer-name) "#")))))))

(defun auto-save-original-name (savename)
  "Reverse of `make-auto-save-file-name'.
Returns nil if SAVENAME was not associated with a file (e.g., it came
>from an autosaved `*mail*' buffer) or does not appear to be an
autosave file at all.
Hashed files are not understood, see `auto-save-hash-p'."
  (let ((basename (file-name-nondirectory savename))
 (savedir (file-name-directory savename)))
    (cond ((or (not (auto-save-file-name-p basename))
        (string-match "^#%" basename))
    nil)
   ;; now we know it looks like #...# thus substring is safe to use
   ((or (equal savedir auto-save-directory) ; 2nd arg may be nil
        (equal savedir auto-save-directory-fallback))
    ;; it is of the `-fixed-directory' type
    (auto-save-slashify-name (substring basename 1 -1)))
   (t
    ;; else it is of `-same-directory' type
    (concat savedir (substring basename 1 -1))))))

(defun auto-save-name-in-fixed-directory (filename &optional prefix)
  ;; Unslashify and enclose the whole FILENAME in `#' to make an auto
  ;; save file in the auto-save-directory, or if that is nil, in
  ;; auto-save-directory-fallback (which must be the name of an
  ;; existing directory).  If the results would be too long for 14
  ;; character filenames, and `auto-save-hash-p' is set, hash FILENAME
  ;; into a shorter name.
  ;; Optional PREFIX is string to use instead of "#" to prefix name.
  (let ((base-name (concat (or prefix "#")
      (auto-save-unslashify-name filename)
      "#")))
    (if (and auto-save-hash-p
      auto-save-hash-directory
      (> (length base-name) 14))
 (expand-file-name (auto-save-cyclic-hash-14 filename)
     auto-save-hash-directory)
      (expand-file-name base-name
   (or auto-save-directory
       auto-save-directory-fallback)))))

(defun auto-save-name-in-same-directory (filename &optional prefix)
  ;; Enclose the non-directory part of FILENAME in `#' to make an auto
  ;; save file in the same directory as FILENAME.  But if this
  ;; directory is not writable, use auto-save-directory-fallback.
  ;; FILENAME is assumed to be in non-directory form (no trailing slash).
  ;; It may be a name without a directory part (pesumably it really
  ;; comes from a buffer name then), the fallback is used then.
  ;; Optional PREFIX is string to use instead of "#" to prefix name.
  (let ((directory (file-name-directory filename)))
    (or (null directory)
 (file-writable-p directory)
 (setq directory auto-save-directory-fallback))
    (concat directory   ; (concat nil) is ""
     (or prefix "#")
     (file-name-nondirectory filename)
     "#")))

(defun auto-save-unslashify-name (s)
  ;;  "Quote any slashes in string S by replacing them with the two
  ;;characters `\\!'.
  ;;Also, replace any backslash by double backslash, to make it one-to-one."
  (let ((limit 0))
    (while (string-match "[/\\]" s limit)
      (setq s (concat (substring s 0 (match-beginning 0))
        (if (string= (substring s
           (match-beginning 0)
           (match-end 0))
       "/")
     "\\!"
   "\\\\")
        (substring s (match-end 0))))
      (setq limit (1+ (match-end 0)))))
  s)

(defun auto-save-slashify-name (s)
  ;;"Reverse of `auto-save-unslashify-name'."
  (let (pos)
    (while (setq pos (string-match "\\\\[\\!]" s pos))
      (setq s (concat (substring s 0 pos)
        (if (eq ?! (aref s (1+ pos))) "/" "\\")
        (substring s (+ pos 2)))
     pos (1+ pos))))
  s)

 
;;; Hashing for autosave names

;;; Hashing function contributed by Andy Norman <ange@hplb.hpl.hp.com>
;;; based upon C code from pot@fly.cnuce.cnr.IT (Francesco Potorti`).

(defun auto-save-cyclic-hash-14 (s)
  ;;   "Hash string S into a string of length 14.
  ;; A 7-bytes cyclic code for burst correction is calculated on a
  ;; byte-by-byte basis. The polynomial used is D^7 + D^6 + D^3 +1.
  ;; The resulting string consists of hexadecimal digits [0-9a-f].
  ;; In particular, it contains no slash, so it can be used as autosave name."
  (let ((crc (make-string 8 0))
 result)
    (mapcar
     (function
      (lambda (new)
 (setq new (+ new (aref crc 7)))
 (aset crc 7 (aref crc 6))
 (aset crc 6 (+ (aref crc 5) new))
 (aset crc 5 (aref crc 4))
 (aset crc 4 (aref crc 3))
 (aset crc 3 (+ (aref crc 2) new))
 (aset crc 2 (aref crc 1))
 (aset crc 1 (aref crc 0))
 (aset crc 0 new)))
     s)
    (setq result (format "%02x%02x%02x%02x%02x%02x%02x"
    (aref crc 0)
    (aref crc 1)
    (aref crc 2)
    (aref crc 3)
    (aref crc 4)
    (aref crc 5)
    (aref crc 6)
    (aref crc 7)))
    result))

;; This leaves two characters that could be used to wrap it in `#' or
;; make two filenames from it: one for autosaving, and another for a
;; file containing the name of the autosaved filed, to make hashing
;; reversible.
(defun auto-save-cyclic-hash-12 (s)
  "Outputs the 12-characters ascii hex representation of a 6-bytes
cyclic code for burst correction calculated on STRING on a
byte-by-byte basis. The used polynomial is D^6 + D^5 + D^4 + D^3 +1."
  (let ((crc (make-string 7 0)))
    (mapcar
     (function
      (lambda (new)
        (setq new (+ new (aref crc 6)))
        (aset crc 6 (aref crc 5))
        (aset crc 5 (+ (aref crc 4) new))
        (aset crc 4 (+ (aref crc 3) new))
        (aset crc 3 (+ (aref crc 2) new))
        (aset crc 2 (aref crc 1))
        (aset crc 1 (aref crc 0))
        (aset crc 0 new)))
     s)
    (format "%02x%02x%02x%02x%02x%02x"
            (aref crc 0)
            (aref crc 1)
            (aref crc 2)
            (aref crc 3)
            (aref crc 4)
            (aref crc 5))))
 

;;; Recovering files

(defun recover-all-files (&optional silent)
  "Do recover-file for all autosave files which are current.
Only works if you have a non-nil `auto-save-directory'.

Optional prefix argument SILENT means to be silent about non-current
autosave files.  This is useful if invoked automatically at Emacs
startup.

If `auto-save-offer-delete' is t, this function will offer to delete
old or rejected autosave files.

Hashed files (see `auto-save-hash-p') are not understood, use
`recover-file' to recover them individually."
  (interactive "P")
  (let ((savefiles (directory-files auto-save-directory t "^#"))
 afile    ; the autosave file
 file    ; its original file
 (total 0)   ; # of files offered to recover
 (count 0))   ; # of files actually recovered
    (or (equal auto-save-directory auto-save-directory-fallback)
 (setq savefiles
       (append savefiles
        (directory-files auto-save-directory-fallback t "^#"))))
    (while savefiles
      (setq afile (car savefiles)
     file (auto-save-original-name afile)
     savefiles (cdr savefiles))
      (if file
   (cond ((/= (nth 2 (file-attributes afile)) (user-uid))
   ;; Roland McGrath says: I made this change to support
   ;; a common autosave directory for everybody on the
   ;; machine (I have a script run by cron to tell people
   ;; about their dead autosaves).
   (or silent
       (message "Autosave file \"%s\" is not yours." afile)))
  ((not (file-newer-than-file-p afile file))
   (if silent
       nil
     (and auto-save-offer-delete
   (or (eq 'always auto-save-offer-delete)
       (yes-or-no-p
        (format "Delete old autosave file for `%s'? "
         file)))
   (delete-file afile))))
  (t
   (setq total (1+ total))
   (with-output-to-temp-buffer "*Directory*"
     (call-process "ls" nil standard-output nil
     "-l" afile file))
   (if (yes-or-no-p (format "Recover %s from autosave file? "
       file))
       (let* ((obuf (current-buffer))
       (buf (set-buffer (find-file-noselect file t)))
       (buffer-read-only nil))
         (erase-buffer)
         (insert-file-contents afile nil)
         (after-find-file nil)
         (setq buffer-auto-save-file-name nil)
         (setq count (1+ count))
         (message "\
Auto-save off in buffer \"%s\" till you do M-x auto-save-mode."
    (buffer-name))
         (set-buffer obuf)
         (sit-for 1))
     ;; If not used for recovering, offer to delete
     ;; autosave file
     (and auto-save-offer-delete
   (or (eq 'always auto-save-offer-delete)
       (yes-or-no-p
        (format "Delete autosave file for `%s'? " file)))
   (delete-file afile)))))))
    (if (zerop total)
 (or silent (message "Nothing to recover."))
      (message "%d/%d file%s recovered." count total (if (= count 1) "" "s"))))
  (if (get-buffer "*Directory*") (kill-buffer "*Directory*")))

(provide 'auto-save)
