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
s = libfoolang.KV(key="foo", value=10)
print(s)
print(u.root[0].p_increment(s))
print("main.py: Done")
