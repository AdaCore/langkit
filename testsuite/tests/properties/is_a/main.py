from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', 'def example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

n = u.root[0]
print('.is_null_a_def = {}'.format(n.p_is_null_a_def))

print('main.py: Done.')
