import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_file("main.txt")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

decl_list = u.root
foo = decl_list[0]
bar = decl_list[1]

baz = decl_list.p_get_with_md("baz", foo, bar)
print(baz.f_name.text + " has the following metadata:")
print("  - foo_node: " + baz.p_get_foo_metadata.f_name.text)
print("  - bar_node: " + baz.p_get_bar_metadata.f_name.text)

print("main.py: Done.")
