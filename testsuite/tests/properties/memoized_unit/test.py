from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True, memoized=True)
    def unit_root_node(unit=T.AnalysisUnit):
        return unit.root.as_bare_entity


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(main_rule=Example('example'))
build_and_run(foo_grammar, 'main.py')
print('Done')
