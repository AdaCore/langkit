import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"a b c")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for node in [u.root, u.root[0]]:
    print("{}.unit = {}".format(node, node.p_node_unit))

print("main.py: Done.")
