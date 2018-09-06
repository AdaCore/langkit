from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', 'def a = 1; def b (x) = 2;')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

r = u.root
for d in r:
    print('{}.decl_kind = {}'.format(d, d.p_decl_kind))

for value in (None, 1, 'blah', 'func'):
    try:
        result = r.p_identity(value)
    except Exception as exc:
        result = '<{}: {}>'.format(type(exc).__name__, exc)
    else:
        result = repr(result)
    print('{}.p_identity({}) = {}'.format(r, repr(value), result))

print('main.py: Done.')
