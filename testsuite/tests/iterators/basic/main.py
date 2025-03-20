import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


def print_title(label):
    print(label)
    print("#" * len(label))
    print("")


print_title("Int iterator")

print("Base array:")
for x in u.root.p_int_array:
    print(f"  {x}")

print("Iteration:")
for x in u.root.p_int_iterator:
    print(f"  {x}")
print("")

print("Identity on a null iterator...")
print("-> {}".format(u.root.p_int_iterator_identity(None)))
print("")

print("Identity on a stale iterator...")
it = u.root.p_int_iterator
u.reparse(buffer=b" example ")
try:
    u.root.p_int_iterator_identity(it)
    print("... got no exception")
except libfoolang.StaleReferenceError:
    print("... got a stale reference error")
print("")

print_title("Bigint iterator")

print("Base array:")
for x in u.root.p_bigint_array:
    print(f"  {x}")

print("Iteration:")
for x in u.root.p_bigint_iterator:
    print(f"  {x}")
print("")

print_title("Entity iterator")

print("Base array:")
for x in u.root.p_entities_array:
    print(f"  {x}")

print("Iteration:")
for x in u.root.p_entities_iterator:
    print(f"  {x}")
print("")

print("main.py: Done.")
