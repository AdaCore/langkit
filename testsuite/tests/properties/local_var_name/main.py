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
ints = [1, 2]
result = n.p_prop(ints)
print(f"p_prop({ints}) = {result}")

print("main.py: Done.")
