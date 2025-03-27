import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"(main 1, 2, 3)")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for prop in ("prop", "prop2"):
    for with_null in (False, True):
        print(f"== {prop}({with_null}) ==")
        try:
            node = getattr(u.root, f"p_{prop}")(with_null)
        except libfoolang.PropertyError as exc:
            print(f"PropertyError: {exc}")
        else:
            node.dump()
        print("")

print("main.py: Done.")
