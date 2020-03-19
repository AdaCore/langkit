import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b"""example""")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


print("Array result:")
for x in u.root.p_entities_array:
    print("  {}".format(x))

print("Iterator result:")
for x in u.root.p_entities_iterator:
    print("  {}".format(x))

print('main.py: Done.')
