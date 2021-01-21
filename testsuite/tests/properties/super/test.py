"""
Check that the "Super" DSL construct works as expected.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, abstract
from langkit.expressions import String, Super, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(public=True)
    def root1():
        return [1]

    @langkit_property(public=True)
    def root2(a=T.String, b=T.String):
        return a.concat(String(" + ")).concat(b)


@abstract
class Expr(FooNode):
    @langkit_property()
    def root1():
        return Super().concat([2])


class Name(Expr):
    token_node = True

    @langkit_property()
    def root1():
        return Super().concat([3])

    @langkit_property()
    def root2(a=T.String, b=T.String):
        return String("<").concat(
            Super(String("[").concat(a).concat(String("]")),
                  b=String("{").concat(b).concat(String("}")))
        ).concat(String(">"))


build_and_run(lkt_file="expected_concrete_syntax.lkt", py_script="main.py")
print("Done")
