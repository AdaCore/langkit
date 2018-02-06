from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', 'def a, def b, b, a')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


def name_repr(n):
    return '{} ({})'.format(n, n.text)


for n in u.root:
    if n.is_a(libfoolang.Name):
        assert n.p_resolve
        print('{} -> {}'.format(name_repr(n),
                                name_repr(n.p_definition.f_name)))

print('main.py: Done.')
