from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', 'a(c) b(a c) +c(a)')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.populate_lexical_env()


a, b, c = u.root.children

for n1, n2 in [(a, a), (a, b)]:
    print('{} .test_env ({}) = {}'.format(n1, n2, n1.p_test_env(n2)))
    print('{} .test_struct ({}) = {}'.format(n1, n2, n1.p_test_struct(n2)))
    print('{} .test_array ({}) = {}'.format(n1, n2, n1.p_test_array(n2)))

print('main.py: Done.')
