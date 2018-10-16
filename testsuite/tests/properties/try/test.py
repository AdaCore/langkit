from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import PropertyError, langkit_property, Self, Try
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(return_type=T.Bool)
    def failing_property():
        return PropertyError(T.Bool)

    @langkit_property(public=True, return_type=T.Bool)
    def failsafe_property():
        return Try(Self.failing_property, False)

    @langkit_property(public=True, return_type=T.Bool)
    def failsafe_property_2():
        return Try(Self.failing_property)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Example('example'),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
