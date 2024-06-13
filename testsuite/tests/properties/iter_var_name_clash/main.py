import sys

import libfoolang


print("main.py: Running...")
print("")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


print("p_prop =", u.root[0].p_prop)

print("main.py: Done.")
