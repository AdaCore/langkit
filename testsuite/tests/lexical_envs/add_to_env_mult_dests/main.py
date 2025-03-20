import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_file("input")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.populate_lexical_env()
for i in u.root.findall(libfoolang.Id):
    print("Scope for {}: {}".format(i, i.p_get_scope))

print("main.py: Done.")
