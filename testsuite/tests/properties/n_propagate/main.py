import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", buffer="1 + 2")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


plus = u.root
lit_left = plus[0]
lit_right = plus[1]

for lhs, rhs in [
    (lit_left, lit_right),
    (plus, lit_right),
    (None, lit_right),
    (lit_left, plus),
    (lit_left, None),
]:
    for dyn in [False, True]:
        try:
            result = str(plus.p_resolve(lhs, rhs, dyn))
        except libfoolang.PropertyError as exc:
            result = f"<PropertyError: {exc}>"

        print(
            f"p_resolve({lhs}, {rhs}, use_dynamic_combiner={dyn}) = {result}"
        )

print("main.py: Done.")
