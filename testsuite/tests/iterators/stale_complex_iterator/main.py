import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b"""example""")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

iter_of_iter = u.root.p_test_struct_iterator

print('Parsing new unit')
u = ctx.get_from_buffer('main2.txt', b"""example""")

print('Trying to iterate...')
try:
    print(next(iter_of_iter))
except libfoolang.StaleReferenceError:
    print('Got a StaleReferenceError')

print('main.py: Done.')
