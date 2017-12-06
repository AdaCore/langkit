from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.expressions import Property, Self
from langkit.parsers import Grammar

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class BarCode(FooNode):
    a = Field()
    prop_1 = Property(Self.a.prop_2)


class BarNode(FooNode):
    prop_2 = Property(Self.parent.cast(BarCode).prop_1)


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=BarCode('example', grammar.rule_2),
    rule_2=BarNode('example'),
)
emit_and_print_errors(grammar)
print('')
print('Done')
