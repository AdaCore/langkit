import sys

import libfoolang


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
        result = repr(value)
    print('{} = {}'.format(prop, result))

print('main.py: Done.')
