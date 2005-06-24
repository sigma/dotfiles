import os

EMACS = 'emacs-cvs'

elisp = Builder(action = EMACS + ' $EFLAGS $SOURCE',
                suffix = '.elc',
                src_suffix = '.el')

env = Environment(BUILDERS = {'Elisp' : elisp}, EFLAGS = '-L perso -batch -f batch-byte-compile', ENV = os.environ)

SConscript(['config/SConscript', 'perso/SConscript'], exports='env')
