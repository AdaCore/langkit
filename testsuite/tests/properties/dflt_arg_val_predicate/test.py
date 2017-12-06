"""
Test that Prediate works well with default argument values.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, BoolType, LogicVarType, T, UserField
from langkit.expressions import (And, Predicate, Self, Var, langkit_property,
                                 ignore)
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    var1 = UserField(LogicVarType, public=False)
    var2 = UserField(LogicVarType, public=False)

    @langkit_property()
    def predicate(n=T.FooNode.entity,
                  flag1=(BoolType, False),
                  flag2=(BoolType, True)):
        return And(flag1, n.is_null, flag2)

    @langkit_property(public=True)
    def prop():
        ignore(Var(Predicate(T.Example.predicate, Self.var1, Self.var2)))
        return Self.as_bare_entity


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Example('example'),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
