;;;;    Replace Recent Character - Emacs lisp macro
;;;;    Version 0.2 from 2004-07-27 21:20 CEST
;;;;    Copyright (C) 2004  Jakub Travnik
;;;;     <j.travnik _at_ sh _dot_ cvut _dot_ cz>
;;;;     or <jtra _at_ seznam _dot_ cz>
;;;;    See also http://www.emacswiki.org/cgi-bin/wiki/ReplaceRecentCharacter
;;;;
;;;;    This program is free software; you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation; either version 2 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program; if not, write to the Free Software
;;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;;###autoload
(defun replace-recent-character ()
  "Replace-recent-character is interactive function for quick corrections of
recenlty typed text. It first prompts for character to search backwards. If
such character is found, following options are shown:
1, repeat the character to search in previous text.
2, M-r for delete of the found character.
3, C-t for trasposition of the found and the following character.
4, TAB for promt for character to insert after the found character.
5, ESC for no operation.
6, Any other insertable character will replace found character."
  (interactive)
  (let* ((repev (read-char "Replace back character:" t))
	 (repstr (string repev)))
    (labels ((check () (if (fboundp 'flyspell-word) (flyspell-word)))
	     (rec ()
		  (save-excursion
		    (let ((point (search-backward repstr (point-at-bol -3) t)))
		      (if point
			  (let (repwithev
				(ov (make-overlay point (1+ point))))
			    (overlay-put ov 'face isearch)
			    (overlay-put ov 'priority 1)
			    (unwind-protect
				(setq repwithev
				      (read-char "Replace with (repeat  previous, M-r  delete, C-t  transpose, TAB  insert):" t))
			      (delete-overlay ov))
			    (cond ((equal repwithev (event-convert-list '(meta ?r)))
				   (delete-char 1)
				   (check)
				   (message (format "Character \"%s\" deleted." repstr)))
				  ((equal repwithev (event-convert-list '(control ?t)))
				   (forward-char)
				   (transpose-chars 1)
				   (check)
				   (message "Transposed."))
				  ((equal repwithev ?\t)
				   (forward-char)
				   (insert-char (read-char "Character to insert after match:" t) 1 t)
				   (check)
				   (message "Insert."))
				  ((equal repwithev ?\e)
				   (message "Replace aborted."))
				  ((equal repwithev repev)
				   (rec))
				  (t
				   (delete-char 1)
				   (insert-char repwithev 1 t)
				   (check)
				   (message
				    (format "Replace \"%s\" -> \"%s\" done." repstr (string repwithev))))))
			(message (format "\"%s\" is not recent." repstr)))))))
      (rec))))

(provide 'rrc)