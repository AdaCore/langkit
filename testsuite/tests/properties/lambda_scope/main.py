import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example\nexample")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print("p_prop(root) =", u.root.p_prop(u.root))

print("main.py: Done.")
