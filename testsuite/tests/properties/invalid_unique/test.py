"""
Test the "unique" array operation in the DSL (invalid usages).
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import Self, langkit_property
from langkit.parsers import Grammar

from utils import emit_and_print_errors


def test_invalid(label, expr):
    print('== {} =='.format(label))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        token_node = True

        @langkit_property(public=True, return_type=T.Bool)
        def test():
            return expr.unique

    g = Grammar('main_rule')
    g.add_rules(main_rule=Example('example'))
    emit_and_print_errors(g)


test_invalid('Not an array', Self)
test_invalid('Not hashable array element', Self.token_start.singleton)
print('Done')
