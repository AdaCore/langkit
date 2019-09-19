"""
Test the "unique" array operation in the DSL (valid use).
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True

    @langkit_property(public=True, return_type=T.AnalysisUnit.array)
    def test(a=T.AnalysisUnit.array):
        return a.unique


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(main_rule=Example('example'))
build_and_run(foo_grammar, 'main.py')
print('Done')
