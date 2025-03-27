import sys

import libfoolang


print("main.py: Running...")

for prop in ("p_decl", "p_decl_wrapper"):
    ctx = libfoolang.AnalysisContext()
    u = ctx.get_from_buffer("main.txt", b"a(c) b(a c) c(a)")
    if u.diagnostics:
        for d in u.diagnostics:
            print(d)
        sys.exit(1)

    ref = u.root[1].f_items[0]
    print("{}.{} = {}".format(ref, prop, getattr(ref, prop)))

print("main.py: Done.")
