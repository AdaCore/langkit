"""
Check that the case insenvitity feature works as expected.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, Field
from langkit.expressions import Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Decl(FooNode):
    name = Field(type=T.Identifier)
    value = Field(type=T.Number)


class Number(FooNode):
    token_node = True


class Identifier(FooNode):
    token_node = True

    @langkit_property(public=True)
    def matches(s=T.Symbol):
        return s == Self.symbol


build_and_run(lkt_file='expected_concrete_syntax.lkt',
              py_script='main.py',
              case_insensitive=True)
print('Done')
