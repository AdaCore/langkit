from __future__ import absolute_import, division, print_function

import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b'(main 1, 2, 3)')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.root.p_prop.dump()
print('main.py: Done.')
