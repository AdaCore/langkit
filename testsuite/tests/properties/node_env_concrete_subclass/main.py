import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"def a; var d")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


def_node = u.root.f_decls[0]
var_node = u.root.f_decls[1]
print('def_node lookup "a" {}'.format(def_node.p_lookup("a")))
print('var node lookup "d" {}'.format(var_node.p_lookup("d")))

print("main.py: Done.")
