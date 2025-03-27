from itertools import product
import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print("main.py: Testing inequality between nodes.")

exs = u.root.findall(libfoolang.Example)

for a, b in product(exs, exs):
    print("{} != {} ? {}".format(a, b, a.p_not_eq(b)))

print("main.py: Testing inequality between integers")
for a, b in [(2, 4), (3, 3)]:
    print("{} != {} ? {}".format(a, b, u.root.p_integers_neq(a, b)))


print("main.py: Done.")
