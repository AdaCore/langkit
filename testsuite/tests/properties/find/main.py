import sys

import libfoolang


print("main.py: Running...")
print("")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for values in [[], [0], [1, 10, 5, 11]]:
    print(f"p_find_above({values}, 6) = {u.root.p_find_above(values, 6)}")

print("main.py: Done.")
