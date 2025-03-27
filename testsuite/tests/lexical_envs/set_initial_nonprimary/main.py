import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
ctx.discard_errors_in_populate_lexical_env(False)
u = ctx.get_from_buffer("main.txt", "example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print("Running PLE...")
try:
    u.populate_lexical_env()
except libfoolang.PropertyError as exc:
    print("   PropertyError: {}".format(exc))
else:
    print("   No exception...")
    sys.exit(1)

print("main.py: Done.")
