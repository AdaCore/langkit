import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
unit = ctx.get_from_buffer(
    "main.txt",
    b"""
    def foo
    def bar
    def baz
    """,
)
if unit.diagnostics:
    for d in unit.diagnostics:
        print(d)
    sys.exit(1)

for name in ("foo", "bar", "baz", "unknown"):
    print(f"Looking for {name}:")
    for n in unit.root.p_lookup(name):
        print(f"  {n.text}")
    print("end")

print("main.py: Done.")
