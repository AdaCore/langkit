import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

try:
    u.root.p_solve
except libfoolang.PropertyError as exc:
    print("Got a {}: {}".format(type(exc).__name__, exc))
else:
    print("ERROR: got no error")

print("main.py: Done.")
