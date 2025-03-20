import os.path
import sys

import libfoolang


print("main.py: Running...")


def load_unit(filename, content):
    unit = ctx.get_from_buffer(filename, content)
    if unit.diagnostics:
        for d in unit.diagnostics:
            print(d)
        sys.exit(1)
    unit.populate_lexical_env()
    return unit


def repr_node(node):
    return "{} from {}".format(node, os.path.basename(node.unit.filename))


ctx = libfoolang.AnalysisContext()
ctx.discard_errors_in_populate_lexical_env(False)
unit_a = load_unit("a.txt", b"example")
unit_b = load_unit("b.txt", b"example")

for unit in [unit_a, unit_b]:
    print(
        "{} -> {}".format(
            os.path.basename(unit.filename),
            repr_node(unit.root.p_unit_root_node(unit)),
        )
    )

print("main.py: Done.")
