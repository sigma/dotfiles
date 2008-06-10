import os

elisp = Builder(action = '$EMACS $EFLAGS -f batch-byte-compile $SOURCE',
                suffix = '.elc',
                src_suffix = '.el')

loaddefs = Builder(action = "$EMACS $EFLAGS -eval '(progn (load-library \"autoloads+\") (let ((generated-autoload-file \"$SOURCE\")) (create-directory-autoloads \".\")))'",
                   src_suffix = '.el',
                   suffix = '.elc')

env = Environment(BUILDERS = {'Elisp' : elisp, 'Loaddefs' : loaddefs}, EMACS = 'emacs-cvs', EFLAGS = '-L custom -L lib -batch -l ~/.emacs-local', ENV = os.environ)

SConscript(['custom/SConscript', 'config/SConscript', 'lib/SConscript'], exports='env')
