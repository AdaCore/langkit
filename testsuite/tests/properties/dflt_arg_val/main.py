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

for args in [(), (True,), (False,)]:
    print(
        "n.p_prop1({}) = {}".format(
            ", ".join(str(a) for a in args),
            n.p_prop1(*args),
        )
    )

for args in [(), (libfoolang.Color.red,), (libfoolang.Color.green,)]:
    print(
        "n.p_prop2({}) = {}".format(
            ", ".join(str(a) for a in args),
            n.p_prop2(*args),
        )
    )

for args in [(), (None,), (n,)]:
    print(
        "n.p_prop3({}) = {}".format(
            ", ".join(str(a) for a in args),
            n.p_prop3(*args),
        )
    )

print("main.py: Done.")
