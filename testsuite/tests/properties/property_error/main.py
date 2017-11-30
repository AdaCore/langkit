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

n = u.root

for p in ('p_raise_msg', 'p_raise_no_msg'):
    print('Evaluating {}...'.format(p))
    try:
        _ = getattr(u.root, p)
    except libfoolang.PropertyError as exc:
        print('  -> {}'.format(exc))
    else:
        print('No exception raised...')

print('main.py: Done.')
