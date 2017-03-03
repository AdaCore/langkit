from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('foo.txt', 'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.populate_lexical_env()

res_none = u.root.p_var(None)
res_field = u.root.p_var(u.root.f_var_field)
print('u.root.p_var(None) = {}'.format(res_none))
print('u.root.p_var(root.f_var_field): result has type {}'.format(
    type(res_field)
))
print('main.py: Done.')
