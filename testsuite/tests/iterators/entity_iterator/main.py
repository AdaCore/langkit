import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b"""example""")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

arr = u.root.p_entities_array
itr = u.root.p_entities_iterator

# print array elements
print("Array result:")
for x in arr:
    print("  " + str(x))

print("Iterator result:")
for x in itr:
    print("  " + str(x))

print('main.py: Done.')
