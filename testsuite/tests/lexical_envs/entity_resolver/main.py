from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_file('main.txt')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.populate_lexical_env()
foo = u.root[0]

for ref in foo.f_refs:
    print('{} resolves to {}'.format(ref, ref.p_resolve.el))

print('main.py: Done.')
