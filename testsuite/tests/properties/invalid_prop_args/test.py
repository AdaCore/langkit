from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, Int
from langkit.expressions import Property, langkit_property
from langkit.parsers import Grammar

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class BarCode(FooNode):
    a = Field()

    @langkit_property(return_type=Int)
    def base_prop(x=Int):
        return x


class BarNode(BarCode):
    base_prop = Property(lambda: 12)


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=BarCode('example', grammar.rule_2),
    rule_2=BarNode('example', grammar.rule_2),
)
emit_and_print_errors(grammar)
print('')
print('Done')
