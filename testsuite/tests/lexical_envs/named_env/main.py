import sys

import libfoolang


print("main.py: Running...")
print()

ctx = libfoolang.AnalysisContext()


u = ctx.get_from_file("foo.txt")
if u.diagnostics:
    for d in u.diagnostics:
        print("Error: {}".format(d))
    sys.exit(1)


def dump_env(node):
    print(f"Env for {node}:")
    for n in node.p_dump_env:
        print(f"  {n.text}")
    print()


dump_env(u.root)
for example in u.root:
    dump_env(example)


print("main.py: Done.")
