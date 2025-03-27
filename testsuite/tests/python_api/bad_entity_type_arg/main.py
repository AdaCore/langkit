import sys

import libfoolang


print("main.py: Running...")
print("")

src_buffer = b"()" b"(1)" b"(a)" b"(1 a 2)" b"(a 1 b)"
ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

try:
    print(u.root.p_foo(u.root))
except TypeError as exc:
    print(f"TypeError: {exc}")

print("")
print("main.py: Done.")
