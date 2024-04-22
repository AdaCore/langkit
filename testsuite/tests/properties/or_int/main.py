import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"a b c")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


def repr_node(n):
    return "<None>" if n is None else n.text


a, b, c = u.root
for n1, n2, n3 in [
    (a, b, c),

    (None, b, c),
    (a, None, c),
    (a, b, None),

    (a, None, None),
    (None, b, None),
    (None, None, c),

    (None, None, None),
]:
    print(
        f"compute({repr_node(n1)}, {repr_node(n2)}, {repr_node(n3)}) =",
        repr_node(u.root.p_compute(n1, n2, n3)),
    )

print("main.py: Done.")
