from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', '((1, 2), 3)')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.populate_lexical_env()

print('.test_main:', u.root.p_test_main)
print('.property_on_entity:', u.root.p_property_on_entity)
print('main.py: Done.')
