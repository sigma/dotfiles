EMACS = 'emacs-cvs'

elisp = Builder(action = EMACS + ' $EFLAGS $SOURCE',
                suffix = '.elc',
                src_suffix = '.el')

env = Environment(BUILDERS = {'Elisp' : elisp}, EFLAGS = '-batch -f batch-byte-compile')

SConscript(['config/SConscript', 'perso/SConscript'], exports='env')
