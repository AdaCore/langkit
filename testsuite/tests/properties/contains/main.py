import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"1 2 3 4")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print("root.p_prop = {}".format(u.root.p_prop))

print("main.py: Done.")
