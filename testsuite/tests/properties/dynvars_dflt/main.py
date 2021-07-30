import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print('u.root.p_prop(False) = {}'.format(u.root.p_prop(False)))
print('u.root.p_prop(True) = {}'.format(u.root.p_prop(True)))
print('u.root.p_prop() = {}'.format(u.root.p_prop()))
print('u.root.p_prop2() = {}'.format(u.root.p_prop2))

print('main.py: Done.')
