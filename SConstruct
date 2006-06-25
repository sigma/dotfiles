import os

EMACS = 'emacs-cvs'

elisp = Builder(action = EMACS + ' $EFLAGS $SOURCE',
                suffix = '.elc',
                src_suffix = '.el')

env = Environment(BUILDERS = {'Elisp' : elisp}, EFLAGS = '-L custom -batch -f batch-byte-compile', ENV = os.environ)

SConscript(['custom/SConscript', 'config/SConscript', 'lib/SConscript'], exports='env')
