from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode
from langkit.expressions import Property, Self
from langkit.parsers import Grammar

from os import path
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(path.abspath(__file__))


class FooNode(ASTNode):
    pass


class Example(FooNode):
    array_of_nodes = Property(Self.singleton, public=True)
    array_of_array = Property(Self.singleton.singleton, public=True)


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=Example('example'),
)
emit_and_print_errors(grammar)

print('')
print('Done')
