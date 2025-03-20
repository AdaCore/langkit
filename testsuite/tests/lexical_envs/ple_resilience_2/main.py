import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("foo", b"example")

if u.diagnostics:
    for d in u.diagnostics:
        print("{}".format(d))
    sys.exit(1)

u.populate_lexical_env()

print("main.py: Done.")
