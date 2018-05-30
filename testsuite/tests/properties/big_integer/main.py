from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', """
def a = 10000000000000000000000000000000
def b = a + 1
def c = b - 1

def d = a = b
def e = a = c
""")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for decl in u.root:
    print('{} evaluates to {}'.format(decl.f_name.text,
                                      decl.f_expr_tree.p_evaluate))

print('main.py: Done.')
