from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('foo.txt', 'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.populate_lexical_env()

print('u.root.p_er(None) = {}'.format(u.root.p_er(None)))

try:
    res_int = u.root.p_er(42)
except TypeError as exc:
    res_int = '<TypeError: {}>'.format(exc)
print('u.root.p_er(42) = {}'.format(res_int))

print('main.py: Done.')
