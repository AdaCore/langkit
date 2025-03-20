import re
import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

n = u.root

for p in ("p_raise_msg", "p_raise_concatenated_msg", "p_raise_no_msg"):
    print("Evaluating {}...".format(p))
    try:
        _ = getattr(u.root, p)
    except libfoolang.PropertyError as exc:
        msg = re.sub(r"\d+", "[line-number]", str(exc))
        print("  -> {}: {}".format(type(exc).__name__, msg))
    else:
        print("No exception raised...")

print("main.py: Done.")
