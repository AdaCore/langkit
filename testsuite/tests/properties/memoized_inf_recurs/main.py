from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import itertools
import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', 'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.populate_lexical_env()
n = u.root

for i in range(4):
    prop_name = 'cond{}'.format(i)

    print('== {} =='.format(prop_name))
    prop = getattr(n, 'p_{}'.format(prop_name))
    for args in itertools.product(*([[True, False]] * i)):
        result = prop(*args) if args else prop
        print('  .{}({}) = {}'.format(prop_name, args, result))


print('== cond_node ==')
print('.cond_node(False) = {}'.format(n.p_cond_node(False)))
print('.cond_node(True) = {}'.format(n.p_cond_node(True)))

print('main.py: Done.')
