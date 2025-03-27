import sys

import libfoolang


print("main.py: Running...")


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"{ d1 }")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for r in u.root.f_decls:
    print(f"{r} resolves to:")
    try:
        result = r.p_lookup("foobar")
    except libfoolang.PropertyError as exc:
        result = f"<{type(exc).__name__}: {exc}>"
    print(f"  {result}")

print("main.py: Done.")
