from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('foo.txt', 'example+ null')
u.root.dump()
print('main.py: Done.')
