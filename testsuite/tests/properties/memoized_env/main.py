from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import gc
import sys

import libfoolang


def load_unit(filename, content):
    unit = ctx.get_from_buffer(filename, content)
    if unit.diagnostics:
        for d in unit.diagnostics:
            print(d)
        sys.exit(1)
    unit.populate_lexical_env()
    return unit


ctx = libfoolang.AnalysisContext()
ctx.discard_errors_in_populate_lexical_env(False)
unit_a = load_unit('a.txt', 'a (b)')
unit_b = load_unit('b.txt', 'b (a)')

# Make unit_a memoize a lexical environment that belong to unit_b
print('unit_a references: {}'.format(
    [n.p_referenced for n in unit_a.root.f_content]
))

print('Deallocate unit_b')
ctx.remove('b.txt')
del unit_b
gc.collect()

# And then unit_a: we expect the destroy mechanism not to try to dec-ref the
# memoization slot, which point to a lexical environment that has been
# deallocated.
print('Deallocate unit_a')
ctx.remove('a.txt')
del unit_a
gc.collect()

print('main.py: Done.')
