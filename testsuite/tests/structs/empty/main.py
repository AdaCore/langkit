import sys

import libfoolang


print("main.py: Running...")
ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example")
n = u.root
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print("p_get_empty =", n.p_get_empty)
print("p_get_wrapper =", n.p_get_wrapper)

empty = libfoolang.EmptyStruct()
print("p_id_empty =", n.p_id_empty(empty))

wrapper = libfoolang.WrapperStruct(10, libfoolang.EmptyStruct(), 20)
print("p_id_wrapper =", n.p_id_wrapper(wrapper))

print("main.py: Done")
