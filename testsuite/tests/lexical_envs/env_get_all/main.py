from __future__ import absolute_import, division, print_function

import sys

import libfoolang


print('main.py: Running...')


def test_case(content):
    ctx = libfoolang.AnalysisContext()
    u = ctx.get_from_buffer('input', content)
    if u.diagnostics:
        for d in u.diagnostics:
            print(d)
        sys.exit(1)

    u.populate_lexical_env()
    print(' '.join(str(x.p_get_sym) for x in u.root.p_env_get_all))


test_case("""
def key_a node_a
def key_b node_b
def key_c node_c
def key_d node_d
def key_e node_e
""")

test_case("""
def key_d node_d
def key_b node_b
def key_a node_a
def key_c node_c
def key_e node_e
""")

test_case("""
def key_x node_a
def key_x node_b
def key_y node_a
def key_y node_b
""")

test_case("""
def key_x node_b
def key_x node_a
def key_y node_b
def key_y node_a
""")

print('main.py: Done.')
