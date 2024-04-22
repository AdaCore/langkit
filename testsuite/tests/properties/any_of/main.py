import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

e_list = u.root
e1 = e_list[0]
e2 = e_list[1]

for prop, *args in [
    ("p_in_1", 1),
    ("p_in_1", 2),
    ("p_in_1_3_4", 1),
    ("p_in_1_3_4", 2),
    ("p_in_nodes", None, None),
    ("p_in_nodes", e_list, e1),
    ("p_in_nodes", e_list, e2),
]:
    result = getattr(e1, prop)(*args)
    print("{}({}) = {}".format(
        prop, ", ".join(str(arg) for arg in args), result
    ))

print("main.py: Done.")
