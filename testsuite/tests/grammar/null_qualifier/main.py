import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer(
    "main.txt",
    """
    var v1;
    null var v2;
    def f1();
""",
)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.root.dump()

print("main.py: Done.")
