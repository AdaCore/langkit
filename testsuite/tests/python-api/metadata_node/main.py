from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import sys

import libfoolang


def show(node):
    print('>>', node)
    print('  ', node.metadata)
    print('  ', node.metadata.f_node)


u = libfoolang.AnalysisContext().get_from_buffer('main.txt', 'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

show(u.root)
show(u.root.p_get)

print('main.py: Done.')
