"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property
from langkit.parsers import Grammar, List, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):

    @langkit_property(public=True)
    def can_reach(n=T.FooNode.entity, from_node=T.FooNode.entity):
        return n.el.can_reach(from_node.el)


class Example(FooNode):
    pass


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(Example(Tok(Token.Example))),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
