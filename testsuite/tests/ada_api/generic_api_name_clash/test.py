"""
Test that creating an array of "TypeRef" nodes in a property generates valid
(compilable) Ada code. It used to trigger name clashes in the Ada library.
"""

from langkit.dsl import ASTNode
from langkit.expressions import Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class TypeRef(FooNode):
    token_node = True

    @langkit_property(public=True)
    def p1():
        return Self.p2.map(lambda n: n.as_bare_entity)

    @langkit_property()
    def p2():
        return Self.singleton


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
