;; -*-Emacs-Lisp-*- 
;; gnus.el version 0.2
;; NBC - chuche@soda.teaser.fr
;; http://www.teaser.fr/~nchuche/gnus/
;; 
;; insertion d'une signature aléatoire.
;; 
;; fortement inspiré du code de Jack Vinson <jvinson@cheux.ecs.umass.edu> 
;; trouvé dans la FAQ gnus.
;;
;; ATTENTION, ce programme ne marchera pas avec un emacs < 20.3 à cause 
;; de la fonction 'assoc-default'.
;; Si vous voulez faire des signatures aléatoires avec votre vieil emacs,
;; utilisez le programme sig.oldemacs.el trouvable au même endroit.
;;
;; mode d'emploi :
;;
;; 1 - Soit vous l'ajoutez directement à la fin de votre .gnus
;;     soit vous le chargez depuis votre .gnus grace à une instruction du type :
;;        (load "~/.elisp/gnus/sig.el")
;;     Si vous avez récupéré mon .gnus, vous n'avez rien à faire sauf à aller 
;;     dedans et à remplacer la ligne
;;          (setq nbc-sig nil)
;;         par 
;;          (setq nbc-sig t)
;;     est déjà chargé tout à la fin.
;; 2 - Vous créez un répertoire .sig directement dans votre homedir
;; 3 - Vous créez les répertoires pour les différents groupes avec les 
;;     signatures que vous voulez et remplissez la alist nommée nbc-sig-dir.
;; 4 - et enfin, vous mettez les signatures que vous voulez dans les répertoires 
;;     adéquats dans des fichiers commençant comme la variable nbc-signature-base.
;;     exemple, chez moi, mon répertoire .sig contient :
;;       [nc@soda nc]$ cd ; tree .sig
;;       .sig
;;       |-- fmbd
;;       |   |-- sig1
;;       |   `-- sig2
;;       |-- mail
;;       |   |-- sig1
;;       |   `-- sig2
;;       |-- sig1
;;       |-- sig2
;;       `-- sig3


;; *********************
;; les variables à modifier
;; *********************

;; la alist (liste associée) mettant en correspondance une regexp et le répertoire
;; ou sont stockés les signatures des groupes vérifiant la regexp
(defvar nbc-sig-group
      '(("dino" . "fmbd")		; les groupes vérifiant la regexp dino auront
					; une signature dans .sig/fmbd/
	("nnml" . "mail")		; les groupes vérifiant la regexp nnml auront 
					; une signature dans .sig/mail
	;; ne pas enlever, répertoire par défaut des signatures
	(".*" . "")))			; les autres auront une signature dans .sig

;; le répertoire ou sont placés vos signatures
(defvar nbc-signature-dir   "~/.sig/")
;; le début du nom des fichiers de signatures
(defvar nbc-signature-base  "sig")

;; *********************
;; le corps de sig.el que vous n'avez, normalement, pas à modifier
;; *********************

;; la définition du test de 'assoc-default'
(defvar nbc-assoc-default-test 'string-match)
;; on définit la variable nbc-sig-dir sinon le defadvice hurle à la mort
(defvar nbc-sig-dir "")

;; la fonction qui est ajoutée a la fonction message-insert-signature
(defadvice message-insert-signature (before random-mail-sig-ag act comp)
  "Change the value of message-signature-file each time
`message-insert-signature' is called."
  ;; le chemin complet du répertoire des signatures pour le groupe actuel
  (let* ((group (or gnus-newsgroup-name ""))
	(nbc-sig-dir (concat nbc-signature-dir
			     (assoc-default group nbc-sig-group nbc-assoc-default-test)
			     "/")))
    (let* ((files (file-name-all-completions
		  nbc-signature-base (expand-file-name nbc-sig-dir))))
      (if files (let ((file (nth (random (length files)) files)))
		  (setq message-signature-file (concat nbc-sig-dir file))
		  )))))