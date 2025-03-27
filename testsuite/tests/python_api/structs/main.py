import sys

import libfoolang


print("main.py: Running...")


def load_unit(name, buffer):
    u = ctx.get_from_buffer(name, buffer)
    if u.diagnostics:
        for d in u.diagnostics:
            print(d)
        sys.exit(1)
    return u


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example")
for d in u.diagnostics:
    print(d)
    sys.exit(1)

s = libfoolang.MyStruct(10, 32)
print(f"sum({s}):", u.root.p_sum(s))
print("create(100, 200):", u.root.p_create(100, 200))

print("main.py: Done.")
