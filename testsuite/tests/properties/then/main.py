import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example\nexample")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

e1 = u.root[0]
e2 = u.root[1]

images = {None: "None", u.root: "root", e1: "e1", e2: "e2"}

for prop in ["p_node_then", "p_node_then_with_default"]:
    for n in [None, e2]:
        result = getattr(e1, prop)(n)
        print(f"{prop}({images[n]}) =", images[result])

print("main.py: Done.")
