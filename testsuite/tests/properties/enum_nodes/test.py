"""
Check that referencing enum nodes in expressions works as expected.
"""

from langkit.dsl import ASTNode
from langkit.expressions import Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class OpKind(FooNode):
    enum_node = True
    alternatives = ["foo", "bar"]

    @langkit_property(public=True)
    def to_int():
        return Self.match(
            lambda _=OpKind.alt_foo: 10,
            lambda _=OpKind.alt_bar: 20,
        )


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print('Done')
