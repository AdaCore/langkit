import sys

import libfoolang


print("main.py: Running...")


def load_unit(name, content):
    u = ctx.get_from_buffer(name, content)
    if u.diagnostics:
        for d in u.diagnostics:
            print(d)
        sys.exit(1)
    return u


ctx = libfoolang.AnalysisContext()
u1 = load_unit("u1", b"example example")
u2 = load_unit("u2", b"example")

u1_n1, u1_n2 = u1.root
u2_n = u2.root[0]

for lhs, rhs in [
    (u1_n1, u1_n2),
    (u1_n2, u1_n1),
    (u1_n1, u1_n1),
    (u1_n1, u2_n),
    (u1_n1, None),
]:
    for prop in ("p_before", "p_before_or_equal"):
        try:
            result = getattr(lhs, prop)(rhs)
        except libfoolang.PropertyError:
            result = "<PropertyError>"
        print("{}.{}({}) = {}".format(lhs, prop, rhs, result))

print("main.py: Done.")
