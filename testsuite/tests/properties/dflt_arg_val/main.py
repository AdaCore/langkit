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

n = u.root
for args in [(), (True, ), (False, )]:
    print('n.p_prop1({}) = {}'.format(
        ', '.join(str(a) for a in args),
        n.p_prop1(*args),
    ))


n = u.root
for args in [(), (None, ), (n, )]:
    print('n.p_prop2({}) = {}'.format(
        ', '.join(str(a) for a in args),
        n.p_prop2(*args),
    ))

print('main.py: Done.')
