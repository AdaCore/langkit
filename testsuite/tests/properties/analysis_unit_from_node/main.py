import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_file("main.txt")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for node in u.root.findall(lambda _: True):
    print("{}.root = {}".format(node, node.p_root_node))

print("main.py: Done.")
