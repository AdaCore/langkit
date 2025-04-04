import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_file("foo.txt")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

expr = u.root
iden = expr.f_left
for node, prop in [
    (expr, "p_null_unit"),
    (expr, "p_null_node"),
    (expr, "p_deref_null_unit"),
    (expr, "p_deref_null_node"),
    (expr, "p_null_node_unit"),
    (expr, "p_cast_null_node"),
    (expr, "p_match_null_node"),
    (iden, "p_deref_env_element"),
    (iden, "p_match_env_element"),
]:
    try:
        result = getattr(node, prop)
    except libfoolang.PropertyError:
        result = "PropertyError"
    print("{}.{}: {}".format(node, prop, result))

print("main.py: Done.")
