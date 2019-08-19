from __future__ import absolute_import, division, print_function
import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', """
Root(
    A( a() )
    B( a() b() )
    C( a() b() c() )
)
""")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


def repr_node(n):
    return n.f_id.text if n else 'None'


for l in u.root.f_nodes:
    print('== {} =='.format(repr_node(l)))
    for child in l.f_nodes:
        print('  {} <= {} => {}'.format(repr_node(child.previous_sibling),
                                        repr_node(child),
                                        repr_node(child.next_sibling)))
    print('')


print('main.py: Done.')
