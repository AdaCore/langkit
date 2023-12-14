import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"+ -")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for n in u.root:
    print(f"{n} -> {n.p_to_int}")

print('main.py: Done.')
