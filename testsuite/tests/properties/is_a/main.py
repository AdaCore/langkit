import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"def example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

n = u.root[0]
print(".is_null_a_def = {}".format(n.p_is_null_a_def))
print(".is_null_a_def_or_example = {}".format(n.p_is_null_a_def_or_example))

print("main.py: Done.")
