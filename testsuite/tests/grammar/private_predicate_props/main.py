from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', 'class')
for d in u.diagnostics:
    print(d)

print('main.py: Done.')
