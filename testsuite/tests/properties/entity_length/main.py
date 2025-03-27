import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"a b c")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


print("Count: {}".format(u.root.p_count))

print("main.py: Done.")
