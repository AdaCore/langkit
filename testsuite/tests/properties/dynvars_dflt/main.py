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

print('u.root.p_prop(False) = {}'.format(u.root.p_prop(False)))
print('u.root.p_prop(True) = {}'.format(u.root.p_prop(True)))
print('u.root.p_prop() = {}'.format(u.root.p_prop()))

print('main.py: Done.')
