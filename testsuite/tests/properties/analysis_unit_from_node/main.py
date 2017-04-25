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
for node in u.root.findall(lambda _: True):
    print('{}.root = {}'.format(node, node.p_root_node))

print('main.py: Done.')
