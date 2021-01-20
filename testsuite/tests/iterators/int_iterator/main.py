import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b"example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print("Iterating through iterator...")
print(u.root.p_values_array)
it = u.root.p_values_iterator
for x in it:
    print(x)
print("")

print("Identity on a null iterator...")
print("-> {}".format(u.root.p_iterator_identity(None)))
print("")

print("Identity on a stale iterator...")
it = u.root.p_values_iterator
u.reparse(buffer=b" example ")
try:
    u.root.p_iterator_identity(it)
    print("... got no exception")
except libfoolang.StaleReferenceError:
    print("... got a stale reference error")
print("")

print('main.py: Done.')
