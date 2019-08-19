from __future__ import absolute_import, division, print_function

import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', 'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

try:
    u.root.p_solve
except libfoolang.PropertyError as exc:
    print('Got a property error: {}'.format(exc))
else:
    print('ERROR: got no error')

print('main.py: Done.')
