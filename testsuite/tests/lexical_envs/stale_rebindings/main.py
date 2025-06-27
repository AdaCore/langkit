import sys

import libfoolang


print("main.py: Starting...")

ctx = libfoolang.AnalysisContext()
n = None


def load(filename, buffer):
    unit = ctx.get_from_buffer(filename, buffer=buffer)
    if unit.diagnostics:
        for d in unit.diagnostics:
            print(d)
        sys.exit(1)
    unit.populate_lexical_env()
    return unit


def try_print_n(label):
    print("{}:".format(label))
    try:
        msg = str(n)
    except libfoolang.StaleReferenceError as exc:
        msg = "got a StaleReferenceError: {}".format(exc)
    print("   {}".format(msg))


u1 = load("foo1.txt", b"example # u1 version 1")
u2 = load("foo2.txt", b"example example # u2 version 1")
u3 = load("foo3.txt", b"example # u3 version 1")

n = u1.root[0].p_rebind(from_node=u2.root[0], to_node=u2.root[1])
try_print_n("original")

u3 = load("foo3.txt", "example # u3 version 2")
try_print_n("After U3 reload")

u2 = load("foo2.txt", "example example # u2 version 2")
try_print_n("After U2 reload")

u1 = load("foo1.txt", "example # u1 version 2")
try_print_n("After U1 reload")

print("main.py: Done.")
