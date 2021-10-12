import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

n = u.root[0]
print("parents() = {}".format(n.parents()))
print("parents(with_self=True) = {}".format(n.parents(with_self=True)))
print("parents(with_self=False) = {}".format(n.parents(with_self=False)))
print("")

for p in (
    "p_node_parents",
    "p_node_parents_without_self",
    "p_entity_parents",
    "p_entity_parents_without_self"
):
    print("{} = {}".format(p, getattr(n, p)))
print("")

for p in ("p_given_node_parents", "p_given_entity_parents"):
    prop = getattr(n, p)
    try:
        result = prop(None)
    except libfoolang.PropertyError as exc:
        result = f"<PropertyError: {exc}>"
    print("{}() = {}".format(p, result))
print("")

print("main.py: Done.")
