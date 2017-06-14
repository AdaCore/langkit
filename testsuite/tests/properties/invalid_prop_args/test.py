from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, LongType
from langkit.expressions import Property, langkit_property, AbstractKind
from langkit.parsers import Grammar, Row

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
    base_prop = Property(lambda: 12)


def lang_def():
    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(
        main_rule=Row('example', foo_grammar.rule_2) ^ BarCode,
        rule_2=Row('example', foo_grammar.rule_2) ^ BarNode,
    )
    return foo_grammar

emit_and_print_errors(lang_def)
print('')
print('Done')
