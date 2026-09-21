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

for prop_root, prop_example in [
    ("p_test_array_root", "p_test_array_example"),
]:
    print(f"{prop_root}:", getattr(u.root, prop_root)(roots))
    print(f"{prop_example}:", getattr(u.root, prop_example)(examples))

print("main.py: Done.")
