import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer(
    "main.txt",
    b"""
def a
    (1 + (2 + c))
def b
    ((10 + (20 + a)) + 30)
def c
    100
""",
)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for label, nodes in [
    ("Atoms", u.root.p_find_atoms),
    ("Atoms or exprs", u.root.p_find_atoms_or_exprs),
    ("Exprs", u.root.p_find_atoms_or_exprs),
    (
        "Refs",
        sum([list(def_node.f_expr.p_find_refs) for def_node in u.root], []),
    ),
]:
    print("{}:".format(label))
    for n in nodes:
        print("   {}".format(n))

print("main.py: Done.")
