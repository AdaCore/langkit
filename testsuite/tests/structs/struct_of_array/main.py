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
u1 = load_unit("main1.txt", b"example")
print(u1.root[0].p_get)
print("main.py: Done")
