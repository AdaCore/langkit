"""
Test getting the analysis unit of a node in the properties DSL.
"""

from langkit.dsl import ASTNode, Field, abstract
from langkit.expressions import (
    Property, Self
)

from utils import build_and_run


class FooNode(ASTNode):
    root_node = Property(Self.unit.root.as_bare_entity, public=True)


@abstract
class Expression(FooNode):
    pass


class Literal(Expression):
    token_node = True


class Plus(Expression):
    left = Field()
    right = Field()


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
