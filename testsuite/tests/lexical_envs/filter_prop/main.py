from __future__ import absolute_import, division, print_function

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
for ref in u.root.findall(libfoolang.Ref):
    print('{} resolves to {}'.format(ref, ref.p_resolve))

print('main.py: Done.')
