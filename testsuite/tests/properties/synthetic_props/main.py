from __future__ import absolute_import, division, print_function

import sys

import libfoolang
from libfoolang import _py2to3


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print('root.is_synthetic = {}'.format(u.root.is_synthetic))

n = u.root.p_get
print(n)
for prop in ['is_synthetic', 'text', 'sloc_range']:
    try:
        value = getattr(n, prop)
    except Exception as exc:
        result = '<{}: {}>'.format(type(exc).__name__, exc)
    else:
        result = (_py2to3.text_repr(value)
                  if isinstance(value, _py2to3.text_type) else
                  repr(value))
    print('{} = {}'.format(prop, result))

print('main.py: Done.')
