import itertools
import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

n = u.root

for i in range(1, 4):
    prop_name = "cond{}".format(i)

    print("== {} ==".format(prop_name))
    prop = getattr(n, "p_{}".format(prop_name))
    for args in itertools.product(*([[True, False]] * i)):
        result = prop(*args) if args else prop
        print("  .{}({}) = {}".format(prop_name, args, result))


print("== cond_node ==")
print(".cond_node(False) = {}".format(n.p_cond_node(False)))
print(".cond_node(True) = {}".format(n.p_cond_node(True)))

print("main.py: Done.")
