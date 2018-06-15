"""
Test that string arguments in public properties are rejected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import DynamicVariable, langkit_property
from langkit.parsers import Grammar

from utils import emit_and_print_errors


some_name = DynamicVariable('some_name', T.StringType)


class FooNode(ASTNode):
    pass


class Example(FooNode):
    @langkit_property(public=True)
    def identity1(s=T.StringType):
        return s

    @langkit_property(public=True, dynamic_vars=[some_name])
    def identity2():
        return some_name


g = Grammar('main_rule')
g.add_rules(main_rule=Example('example'))
emit_and_print_errors(g)
print('Done')
