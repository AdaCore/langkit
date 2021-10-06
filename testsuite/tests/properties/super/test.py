"""
Check that the ".super()" DSL construct works as expected.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, abstract
from langkit.expressions import Entity, Self, String, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(public=True)
    def root1():
        return [1]

    @langkit_property(public=True)
    def root2(a=T.String, b=T.String):
        return a.concat(String(" + ")).concat(b)

    @langkit_property(public=True)
    def root3():
        return Entity.info.rebindings.then(lambda _: True)


@abstract
class Expr(FooNode):
    @langkit_property()
    def root1():
        return Self.super().concat([2])


class Name(Expr):
    token_node = True

    @langkit_property()
    def root1():
        return Self.super().concat([3])

    @langkit_property()
    def root2(a=T.String, b=T.String):
        return String("<").concat(
            Self.super(String("[").concat(a).concat(String("]")),
                       b=String("{").concat(b).concat(String("}")))
        ).concat(String(">"))

    @langkit_property()
    def root3():
        return Entity.super()


build_and_run(lkt_file="expected_concrete_syntax.lkt", py_script="main.py")
print("Done")
