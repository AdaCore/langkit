import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"n")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


def repr_node(n):
    return "<None>" if n is None else n.text


n = u.root[0]
print("in_bind:", repr_node(n.p_in_bind(n)))
print("in_call_arg:", repr_node(n.p_in_call_arg(n)))
print("in_if:", repr_node(n.p_in_if(False, n, n)))
print("in_or_int:", repr_node(n.p_in_or_int(n, n)))
print("in_prop_result:", repr_node(n.p_in_prop_result(n)))

print("main.py: Done.")
