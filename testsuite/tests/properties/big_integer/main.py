from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', """
def a0 = 2
def a = 10000000000000000000000000000000
def b = a + 1
def c = b - 1

def d = a = b
def e = a = c
def f = a < b
def g = a < c
""")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for decl in u.root:
    expr = decl.f_expr_tree
    big_int = expr.p_evaluate
    try:
        small_int = expr.p_evaluate_as_int
    except libfoolang.PropertyError:
        small_int = '<too big>'
    print('{} evaluates to {} ({})'.format(decl.f_name.text, big_int,
                                           small_int))

print('main.py: Done.')
