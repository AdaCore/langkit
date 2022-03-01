"""
Test the handling of analysis units in the properties DSL.
"""

from langkit.dsl import ASTNode, AnalysisUnit, Field, Int, T, abstract
from langkit.expressions import (AbstractProperty, ExternalProperty, Property,
                                 Self, langkit_property)

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(public=True)
    def eval_unit(u=AnalysisUnit):
        return u.root.cast(T.Expression).result

    @langkit_property(public=True)
    def id_unit(u=AnalysisUnit):
        return u


@abstract
class Expression(FooNode):
    result = AbstractProperty(type=Int, public=True)


class Literal(Expression):
    token_node = True

    result = ExternalProperty(uses_entity_info=False, uses_envs=False)


class Name(Expression):
    token_node = True

    designated_unit = ExternalProperty(
        type=AnalysisUnit, uses_entity_info=False, uses_envs=True
    )
    result = Property(Self.designated_unit.root.cast(Expression).result)


class Plus(Expression):
    left = Field()
    right = Field()

    result = Property(Self.left.result + Self.right.result)


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              types_from_lkt=True)
print('Done')
