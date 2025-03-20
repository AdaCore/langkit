import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", "example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print("root.p_mmz_prop(42) = {}".format(u.root.p_mmz_prop(42)))

try:
    result = u.root.p_mmz_prop(0)
except libfoolang.PropertyError:
    result = "PropertyError"
print("root.p_mmz_prop(0)  = {}".format(result))

print("main.py: Done.")
