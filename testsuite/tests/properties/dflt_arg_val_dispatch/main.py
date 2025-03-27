import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"var a def b")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for decl in u.root:
    for args in [(), (True,), (False,)]:
        print(
            "{}.p_prop({}) = {}".format(
                type(decl).__name__,
                ", ".join(str(a) for a in args),
                decl.p_prop(*args),
            )
        )

print("main.py: Done.")
