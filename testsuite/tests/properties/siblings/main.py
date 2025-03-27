print("main.py: Running...")


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer(
    "main.txt",
    """
Root(
    A( a() )
    B( a() b() )
    C( a() b() c() )
)
""",
)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


def repr_node(n):
    return n.f_id.text if n else "None"


print(
    "  {} <= {} => {}".format(
        repr_node(u.root.previous_sibling),
        repr_node(u.root),
        repr_node(u.root.next_sibling),
    )
)
print("")


for l in u.root.f_nodes:
    print("== {} ==".format(repr_node(l)))
    for child in l.f_nodes:
        print(
            "  {} <= {} => {}".format(
                repr_node(child.previous_sibling),
                repr_node(child),
                repr_node(child.next_sibling),
            )
        )
    print("")


print("main.py: Done.")
