from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', 'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print('root.is_synthetic = {}'.format(u.root.is_synthetic))

n = u.root.p_get
print(n)
for prop in ['is_synthetic', 'text', 'sloc_range']:
    try:
        result = repr(getattr(n, prop))
    except Exception as exc:
        result = '<{}: {}>'.format(type(exc).__name__, exc)
    print('{} = {}'.format(prop, result))

print('main.py: Done.')
