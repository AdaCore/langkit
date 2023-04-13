import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
ctx.discard_errors_in_populate_lexical_env(False)
u = ctx.get_from_buffer("main.txt", "example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print(f"p_get_all_foo = {u.root.p_get_all_foo}")
print("main.py: Done.")
