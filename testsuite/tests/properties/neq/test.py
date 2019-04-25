"""
Test that the overloaded "!=" operator on abstract expressions works as
expected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Bool, Int, T
from langkit.expressions import Self, langkit_property
from langkit.parsers import Grammar, List

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(public=True, return_type=Bool)
    def integers_neq(a=Int, b=Int):
        return a != b


class Example(FooNode):
    token_node = True

    @langkit_property(public=True, return_type=Bool)
    def not_eq(other=T.Example):
        return Self != other


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(Example("example")),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
