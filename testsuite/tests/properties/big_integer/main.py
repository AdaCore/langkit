import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_file("main.txt")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print(f".p_check_big_literal = {u.root.p_check_big_literal}")

for decl in u.root:
    expr = decl.f_expr_tree
    big_int = expr.p_evaluate
    try:
        small_int = expr.p_evaluate_as_int
    except libfoolang.PropertyError:
        small_int = "<too big>"
    print(f"{decl.f_name.text} evaluates to {big_int} ({small_int})")


print(".p_identity(1234) = {}".format(u.root.p_identity(1234)))

print("main.py: Done.")
