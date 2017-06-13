from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, root_grammar_class
from langkit.expressions import Let, No, Property, Self
from langkit.parsers import Grammar

from os import path
from utils import build_and_run


Diagnostics.set_lang_source_dir(path.abspath(__file__))


@root_grammar_class
class FooNode(ASTNode):
    pass


class BarNode(FooNode):
    prop = Property(Let(lambda _=Self.parent: No(FooNode)), public=True)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(main_rule=BarNode('example'))
build_and_run(foo_grammar, 'main.py')
print('Done')
