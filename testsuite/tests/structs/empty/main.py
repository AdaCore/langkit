import sys

import libfoolang


print('main.py: Running...')


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

print(u.root.p_identity(libfoolang.MyStruct()))

print('main.py: Done.')
