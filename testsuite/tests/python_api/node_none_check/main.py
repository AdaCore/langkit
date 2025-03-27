import libfoolang


ctx = libfoolang.AnalysisContext()
unit = ctx.get_from_buffer("foo.txt", b"example example")

for node in [unit.root, unit.root[0]]:
    print("bool({}) = {}".format(node, bool(node)))

print("main.py: Done.")
