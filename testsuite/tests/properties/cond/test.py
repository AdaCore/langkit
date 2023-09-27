"""
Test the handling of analysis units in the properties DSL.
"""

from langkit.dsl import ASTNode, Bool
from langkit.expressions import Cond, No, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property()
    def identity(b=Bool):
        return b

    @langkit_property(public=True)
    def cond1(b=Bool):
        return Cond(Self.identity(b), 1,
                    2)

    @langkit_property(public=True)
    def cond2(b1=Bool, b2=Bool):
        return Cond(Self.identity(b1), 1,
                    Self.identity(b2), 2,
                    3)

    @langkit_property(public=True)
    def cond3(b1=Bool, b2=Bool, b3=Bool):
        return Cond(Self.identity(b1), 1,
                    Self.identity(b2), 2,
                    Self.identity(b3), 3,
                    3)

    @langkit_property(public=True)
    def cond_node(b=Bool):
        return Cond(Self.identity(b), Self,
                    No(FooNode)).as_bare_entity


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
