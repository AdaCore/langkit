from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, Row

from os import path
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(path.abspath(__file__))


class FooNode(ASTNode):
    pass


class BarCode(FooNode):
    a = Field()
    prop_1 = Property(Self.a.prop_2)


class BarNode(FooNode):
    prop_2 = Property(Self.parent.cast(BarCode).prop_1)


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=Row('example', grammar.rule_2) ^ BarCode,
    rule_2=Row('example') ^ BarNode,
)
emit_and_print_errors(grammar)
print('')
print('Done')
