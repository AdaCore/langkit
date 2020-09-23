import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for n in (None, u.root):
    try:
        result = libfoolang._py2to3.text_repr(u.root.p_prop(n))
    except libfoolang.PropertyError as exc:
        result = '<{}: {}>'.format(type(exc).__name__, exc)
    print('p_prop({}) = {}'.format(n, result))

print('main.py: Done.')
