import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"foo")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


print("p_root1 = {}".format(u.root.p_root1))
print("p_root2('3', '4') = {}".format(repr(u.root.p_root2("3", "4"))))
print("p_root3 = {}".format(repr(u.root.p_root3)))

print("main.py: Done.")
