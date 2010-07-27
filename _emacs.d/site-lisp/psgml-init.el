(setq load-path (append load-path '("/usr/share/emacs/site-lisp/psgml")))

(autoload 'sgml-mode "psgml" "Major mode for editing SGML." t)
(if (not (getenv "SGML_CATALOG_FILES"))
   (defvar sgml-catalog-files '("CATALOG" "/usr/lib/sgml/CATALOG" "/usr/lib/sgml-tools/dtd/catalog"))
  "*List of catalog entry files.
The files are in the format defined in the SGML Open Draft Technical
Resolution on Entity Management.")
(put 'sgml-catalog-files 'sgml-type 'list)

