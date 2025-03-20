import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

n = u.root[0]

for prop in sorted(f for f in dir(n) if f.startswith("p_test_")):
    try:
        result = getattr(n, prop)
    except libfoolang.PropertyError as exc:
        result = "<PropertyError: {}>".format(exc)
    except Exception as exc:
        print("ERROR ({}): {}: {}".format(prop, type(exc).__name__, exc))
        sys.exit(1)
    print("{}.{} = {}".format(n, prop, result))

print("main.py: Done.")
