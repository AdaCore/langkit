import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b"""example""")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

itr = u.root.p_entities_iterator

print('main.py: Iterating once: {}'.format(str(next(itr))))

print('main.py: Parsing new unit')
u = ctx.get_from_buffer('main2.txt', b"""example""")

try:
    print('main.py: Iterating once more: ', end='')
    print(next(itr))
except libfoolang.StaleReferenceError:
    print('<StaleReferenceError>')

print('main.py: Done.')
