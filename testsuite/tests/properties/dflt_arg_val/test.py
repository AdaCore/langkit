from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Bool, Enum, EnumValue, T
from langkit.expressions import No, langkit_property
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Color(Enum):
    Red = EnumValue()
    Green = EnumValue()
    Blue = EnumValue()


class Example(FooNode):

    # Test default argument for a simple type: boolean
    @langkit_property(public=True)
    def prop1(arg=(Bool, True)):
        return arg

    # Test default argument for a simple type: enum
    @langkit_property(public=True)
    def prop2(arg=(Color, Color.Red)):
        return arg

    # Test default argument for public entities
    @langkit_property(public=True)
    def prop3(arg=(T.FooNode.entity, No(T.FooNode.entity))):
        return arg


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=Example('example'),
)
build_and_run(grammar, 'main.py')
print('')
print('Done')
