import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", buffer=b"example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

result = u.root.p_prop
print(f"p_prop = {result}")

print("main.py: Done.")
