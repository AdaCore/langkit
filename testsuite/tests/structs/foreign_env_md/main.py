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
ctx.discard_errors_in_populate_lexical_env(False)

u1 = load_unit("main1.txt", b"a")
u2 = load_unit("main2.txt", b"b+a")

u1.populate_lexical_env()
try:
    u2.populate_lexical_env()
except libfoolang.PropertyError:
    print("main.py: got a property error")
else:
    print("main.py: got no exception")
print("main.py: Done.")
