"""
Test that Prediate works well with default argument values.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, BoolType, LogicVarType, T, UserField
from langkit.expressions import Predicate, Self, Var, ignore, langkit_property
from langkit.parsers import Grammar

from utils import emit_and_print_errors


def run(name, *pred_args):
    Diagnostics.set_lang_source_dir(os.path.abspath(__file__))

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):

        var1 = UserField(LogicVarType, public=False)
        var2 = UserField(LogicVarType, public=False)

        @langkit_property(warn_on_unused=False)
        def pred1(n=T.FooNode.entity):
            return n.is_null

        @langkit_property(warn_on_unused=False)
        def pred2(n=T.FooNode.entity, b=(BoolType, False)):
            return n.is_null & b

        @langkit_property(public=True)
        def prop():
            ignore(Var(Predicate(*pred_args)))
            return Self.as_bare_entity

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=Example('example'),
    )
    emit_and_print_errors(grammar)
    print('')


run('No logic var', T.Example.pred1)
run('One missing logic var', T.Example.pred1, Self.var1)
run('Bad argument type', T.Example.pred1, Self.var1, True)
run('Too many arguments', T.Example.pred1, Self.var1, Self.var2, True)
run('Ok (no default var)', T.Example.pred1, Self.var1, Self.var2)

run('Ok (one unpassed default var)', T.Example.pred2, Self.var1, Self.var2)
run('Ok (one passed default var)', T.Example.pred2, Self.var1, Self.var2, True)

print('Done')
