import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_file("main.txt")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

foo = u.root[0]

for ref in foo.f_refs:
    print("{} resolves to {}".format(ref, ref.p_resolve))

print("main.py: Done.")
