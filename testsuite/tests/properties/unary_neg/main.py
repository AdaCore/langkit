import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

n = u.root

print("== negate_int ==")
print("")
for value in (0, 1, 10, -1, -10):
    print(f"{value} -> {n.p_negate_int(value)}")
print("")

print("== minus_one_int ==")
print("")
print(n.p_minus_one_int)
print("")

print("== negate_bigint ==")
print("")
for value in (0, 1, 10, 10**30, -1, -10, -(10**30)):
    print(f"{value} -> {n.p_negate_bigint(value)}")
print("")

print("== minus_one_bigint ==")
print("")
print(n.p_minus_one_bigint)
print("")

print("main.py: Done.")
