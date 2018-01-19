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
for prop in ('p_run_bare', 'p_run_entity'):
    print('Running {}...'.format(prop))
    try:
        getattr(n, prop)
    except libfoolang.PropertyError as exc:
        print('    Got a PropertyError: {}'.format(exc))
    else:
        print('    Got no PropertyError')

print('main.py: Done.')
