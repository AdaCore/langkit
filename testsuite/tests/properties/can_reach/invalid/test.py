"""
Check that invalid definitions for "can_reach" are properly rejected.
"""

from langkit.dsl import ASTNode, T, UserField
from langkit.expressions import Entity, Property

from utils import emit_and_print_errors


def run(label, can_reach_fn):
    print(f"== {label} ==")

    class FooNode(ASTNode):
        can_reach = can_reach_fn()

    class Example(FooNode):
        token_node = True

    emit_and_print_errors(lkt_file="expected_concrete_syntax.lkt")


run("not a property", lambda: UserField(type=T.Bool, public=False))
run("bad return type", lambda: Property(lambda n=T.FooNode: n))
run("no argument", lambda: Property(lambda: True))
run("too many arguments", lambda: Property(lambda _=T.FooNode, b=T.Bool: b))
run("bad arg type", lambda: Property(lambda b=T.Bool: b))
run("use entity info",
    lambda: Property(lambda n=T.FooNode: n.as_bare_entity == Entity))

print("Done")
