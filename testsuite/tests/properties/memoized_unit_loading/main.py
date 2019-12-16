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

for i in range(3):
    print('root.p_mmz_prop() = {}'.format(u.root.p_mmz_prop))

print('main.py: Done.')
