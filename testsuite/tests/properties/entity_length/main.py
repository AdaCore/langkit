from __future__ import absolute_import, division, print_function

import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', 'a b c')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


print('Count:', u.root.p_count)

print('main.py: Done.')
