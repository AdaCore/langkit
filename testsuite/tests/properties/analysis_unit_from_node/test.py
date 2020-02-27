"""
Test getting the analysis unit of a node in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

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


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
