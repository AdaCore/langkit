import sys

import libfoolang


print("main.py: Running...")
print("")

src_buffer = (
    b"()"
    b"(1)"
    b"(a)"
    b"(1 a 2)"
    b"(a 1 b)"
)
ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", src_buffer)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


def format_nodes(nodes):
    return " ".join(n.text for n in nodes) or "<empty>"


for node in u.root:
    print("#", node.text)

    keep_list = node.p_keep_list
    keep_array = node.p_keep_array(list(node.f_items))

    print(f"  keep_list:  {format_nodes(keep_list)}")
    print(f"  keep_array: {format_nodes(keep_array)}")
    print("")

print("main.py: Done.")
