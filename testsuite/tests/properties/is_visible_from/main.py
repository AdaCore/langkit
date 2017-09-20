from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', 'a b c')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.populate_lexical_env()

for arg1, arg2 in [
    (True, True),
    (True, False),
    (False, True),
    (False, False)
]:
    try:
        result = u.root.p_prop(arg1, arg2)
    except libfoolang.PropertyError as exc:
        result = '<PropertyError: {}>'.format(exc)
    print('u.root.p_test({}, {}) = {}'.format(arg1, arg2, result))

print('main.py: Done.')
