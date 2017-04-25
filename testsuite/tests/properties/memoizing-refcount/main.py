from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import ctypes
import gc
import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', 'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.populate_lexical_env()


def c_address(py_array):
    return ctypes.addressof(py_array._c_value.contents)


result_1 = u.root.p_foo
result_2 = u.root.p_foo
print('result = {}'.format(result_1))
print('Both C arrays are equal? {}'.format(
    c_address(result_1) == c_address(result_2)
))

# Perform a full collection, to make sure the context outlives everything else
del result_1, result_2
del u
gc.collect()

print('main.py: Done.')
