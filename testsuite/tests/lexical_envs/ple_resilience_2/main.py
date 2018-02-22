from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('foo', 'example')

if u.diagnostics:
    for d in u.diagnostics:
        print('{}'.format(d))
    sys.exit(1)

u.populate_lexical_env()

print('main.py: Done.')
