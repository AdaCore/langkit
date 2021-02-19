import sys

import libfoolang


print("main.py: Starting...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("foo.txt", buffer="example example example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)
    u.populate_lexical_env()

n1, n2, n3 = u.root
print(n1.p_rebind(n2, n3))

print("main.py: Done.")
