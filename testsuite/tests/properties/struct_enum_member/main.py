import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"def a = 1; def b (x) = 2;")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

r = u.root
for d in r:
    print("{}.with_kind = {}".format(d, d.p_with_kind))

print("main.py: Done.")
