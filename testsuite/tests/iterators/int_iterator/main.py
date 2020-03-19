import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b"""example""")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print(u.root.p_values_array)
it = u.root.p_values_iterator
for x in it:
    print(x)

print('main.py: Done.')
