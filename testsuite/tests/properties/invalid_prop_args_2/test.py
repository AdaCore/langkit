from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, LongType, SymbolType
from langkit.expressions import Property, langkit_property, AbstractKind
from langkit.parsers import Grammar

from os import path
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(path.abspath(__file__))


class FooNode(ASTNode):
    pass


class BarCode(FooNode):
    a = Field()

    @langkit_property(kind=AbstractKind.abstract, return_type=LongType)
    def base_prop(x=LongType):
        pass


class BarNode(BarCode):
    base_prop = Property(lambda x=SymbolType: 12)


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=BarCode('example', grammar.rule_2),
    rule_2=BarNode('example', grammar.rule_2),
)
emit_and_print_errors(grammar)
print('')
print('Done')
