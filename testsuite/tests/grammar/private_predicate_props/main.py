from __future__ import absolute_import, division, print_function

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b'class')
for d in u.diagnostics:
    print(d)

print('main.py: Done.')
