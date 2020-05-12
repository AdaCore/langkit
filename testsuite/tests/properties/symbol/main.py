import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

try:
    result = u.root.p_prop(None)
except libfoolang.PropertyError as exc:
    result = '<{}: {}>'.format(type(exc).__name__, exc)

print('p_prop(None) = {}'.format(result))

print('main.py: Done.')
