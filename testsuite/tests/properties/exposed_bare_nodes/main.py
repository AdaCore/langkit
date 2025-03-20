import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

foo = u.root
print("{}.p_compute = {}".format(foo, foo.p_compute(foo)))

print("main.py: Done.")
