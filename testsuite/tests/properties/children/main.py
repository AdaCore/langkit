import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"()\n(example)\n(example example)")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

seqs = list(u.root)

for p in ("children", "p_node_children", "p_entity_children"):
    for s in seqs:
        result = getattr(s, p)
        print(f"{s}.{p}() = {result}")
    print("")

for p in ("p_given_node_children", "p_given_entity_children"):
    prop = getattr(u.root, p)
    try:
        result = prop(None)
    except libfoolang.PropertyError as exc:
        result = f"<PropertyError: {exc}>"
    print("{}() = {}".format(p, result))
print("")

print("main.py: Done.")
