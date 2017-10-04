from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()


def parse_unit(filename, src_buffer):
    u = ctx.get_from_buffer(filename, src_buffer)
    if u.diagnostics:
        for d in u.diagnostics:
            print('{}: {}'.format(filename, d))
        sys.exit(1)
    u.populate_lexical_env()
    return u


u1 = parse_unit('src1.txt', 'example\nexample')
u2 = parse_unit('src2.txt', 'example')

root = u1.root
u1_ex1, u1_ex2 = u1.root
u2_ex = u2.root


def fmt_node(n):
    return ('None' if n is None else
            '<{} {}:{}>'.format(n.kind_name, n.unit.filename,
                                n.sloc_range.start.line))


for node, from_node in [
    (u1_ex1, u1_ex1),
    (u1_ex1, u1_ex2),
    (u1_ex2, u1_ex1),

    (None, u1_ex1),
    (u1_ex1, None),
    (None, None),

    (u1_ex1, u2_ex),
    (u2_ex, u1_ex1),
]:
    print('can_reach({}, {}) = {}'.format(fmt_node(node), fmt_node(from_node),
                                          root.p_can_reach(node, from_node)))

print('main.py: Done.')
