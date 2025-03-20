import sys

import libfoolang


def load_unit(name, buffer):
    u = ctx.get_from_buffer(name, buffer)
    if u.diagnostics:
        for d in u.diagnostics:
            print(d)
        sys.exit(1)
    return u


print("main.py: Running...")
ctx = libfoolang.AnalysisContext()
u = load_unit("main1.txt", b"example")
print(u.root[0].p_build_1("foo"))
print(u.root[0].p_build_2("foo", 1))
print("main.py: Done")
