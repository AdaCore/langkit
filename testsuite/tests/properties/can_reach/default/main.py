import os.path
import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()


def parse_unit(filename, src_buffer):
    u = ctx.get_from_buffer(filename, src_buffer)
    if u.diagnostics:
        for d in u.diagnostics:
            print("{}: {}".format(filename, d))
        sys.exit(1)
    return u


u1 = parse_unit("src1.txt", b"example\nexample")
u2 = parse_unit("src2.txt", b"example")

root = u1.root
u1_ex1, u1_ex2 = u1.root
u2_ex = u2.root


def fmt_node(n):
    return (
        "None"
        if n is None
        else "<{} {}:{}>".format(
            n.kind_name,
            os.path.basename(n.unit.filename),
            n.sloc_range.start.line,
        )
    )


for node, from_node in [
    (u1_ex1, u1_ex1),
    (u1_ex1, u1_ex2),
    (u1_ex2, u1_ex1),
    (u1_ex1, None),
    (u1_ex1, u2_ex),
    (u2_ex, u1_ex1),
]:
    print(
        f"can_reach({fmt_node(node)}, {fmt_node(from_node)})"
        f" = {node.p_can_reach_wrapper(from_node)}"
    )

print("main.py: Done.")
