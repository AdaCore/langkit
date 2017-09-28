from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, T
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, Row

from os import path
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(path.abspath(__file__))


class FooNode(ASTNode):
    pass


class ExampleNode(FooNode):
    prop = Property(Self.cast(T.ExampleNode).as_bare_entity, public=True)


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=Row('example') ^ ExampleNode
)
emit_and_print_errors(grammar)
print('Done')
