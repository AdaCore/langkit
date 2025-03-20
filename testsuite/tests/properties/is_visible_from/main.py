import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"a b c")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for arg1, arg2 in [(True, True), (True, False), (False, True), (False, False)]:
    try:
        result = u.root.p_prop(arg1, arg2)
    except libfoolang.PropertyError as exc:
        result = "<PropertyError: {}>".format(exc)
    print("u.root.p_test({}, {}) = {}".format(arg1, arg2, result))

print("main.py: Done.")
