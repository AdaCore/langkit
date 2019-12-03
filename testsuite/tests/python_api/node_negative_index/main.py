from __future__ import absolute_import, division, print_function

import libfoolang


ctx = libfoolang.AnalysisContext()
unit = ctx.get_from_buffer('foo.txt', b'a b c')
for i in range(-4, 5):
    try:
        child = unit.root[i]
    except IndexError:
        child = '<IndexError>'
    print('i={}: {}'.format(i, child))
