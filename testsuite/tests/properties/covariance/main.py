import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", "example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

roots = [u.root, u.root[0]]
examples = [u.root[0], u.root[0]]

print("array:")
for prop, arg in [
    ("p_test_array_root", roots),
    ("p_test_array_example", examples),
]:
    print(f"  {prop}:", getattr(u.root, prop)(arg))
print()

print("iterator:")
for prop, arg in [
    ("p_test_iterator_root", roots),
    ("p_test_iterator_example", examples),
]:
    print(f"  {prop}:", list(getattr(u.root, prop)(arg)))
print()

print("set:")
for prop, arg in [
    ("p_test_set_root", roots),
    ("p_test_set_example", examples),
]:
    print(f"  {prop}:", getattr(u.root, prop)(arg))
print()

print("main.py: Done.")
